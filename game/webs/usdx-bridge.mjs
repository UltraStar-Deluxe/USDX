#!/usr/bin/env node
import fs from 'node:fs';
import net from 'node:net';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';

const protocol = 1;
const fsp = fs.promises;
const bridgeDir = path.dirname(fileURLToPath(import.meta.url));
const gameDir = path.resolve(bridgeDir, '..');
const repoRoot = path.resolve(gameDir, '..');

function getArg(name, fallback) {
  const prefix = `--${name}=`;
  const found = process.argv.find((arg) => arg.startsWith(prefix));
  if (found) return found.slice(prefix.length);
  const index = process.argv.indexOf(`--${name}`);
  if (index >= 0 && process.argv[index + 1]) return process.argv[index + 1];
  return process.env[`USDX_${name.toUpperCase().replaceAll('-', '_')}`] || fallback;
}

function getBoolArg(name, fallback) {
  const value = String(getArg(name, fallback ? 'true' : 'false')).toLowerCase();
  return !['0', 'false', 'no', 'off'].includes(value);
}

function hostMonoUs() {
  return Number(process.hrtime.bigint() / 1000n);
}

function wsUrl(base) {
  if (base.startsWith('ws://') || base.startsWith('wss://')) return base;
  const url = new URL(base);
  url.protocol = url.protocol === 'https:' ? 'wss:' : 'ws:';
  url.pathname = '/ws/host';
  return url.toString();
}

const server = getArg('server', 'ws://127.0.0.1:8080/ws/host');
let maxPlayers = Number(getArg('max-players', '6'));
const reconnectDelayMs = Number(getArg('reconnect-delay-ms', '2000'));
const ipcHost = getArg('ipc-host', '127.0.0.1');
const ipcPort = Number(getArg('ipc-port', '8765'));
const p2pEnabled = getBoolArg('p2p', true);
const p2pIceServersArg = getArg('ice-servers', 'stun:stun.l.google.com:19302');
const mockSong = getBoolArg('mock-song', true);
const autoAck = getBoolArg('auto-ack', true);
const autoAssign = getBoolArg('auto-assign', true);
const songScan = getBoolArg('song-scan', true);
const songConfig = getArg('song-config', path.join(gameDir, 'config.ini'));
const songRootArg = getArg('song-root', '');
const playlistDirArg = getArg('playlist-dir', '');
const maxIniPlayerDelays = Number(getArg('max-player-delays', '12'));

let ws = null;
let room = null;
let seq = 1;
let songSeq = 0;
let lastSongClockMessage = null;
let songScanStarted = false;
let scannedSongState = null;
let scannedLibrary = [];
let currentPlaylistIndex = -1;
let reconnectTimer = null;
let songScanProgress = {
  status: 'idle',
  indexedSongs: 0,
  scannedFiles: 0,
  scannedDirs: 0,
  roots: []
};
const players = new Map();
const pitchStats = new Map();
const ipcClients = new Set();
const songPreviewPaths = new Map();
const activePreviewStreams = new Map();
const maxActivePreviewStreams = Number(getArg('max-active-previews', '6'));
const previewBackpressureHighBytes = Number(getArg('preview-backpressure-high-bytes', String(512 * 1024)));
const previewBackpressureLowBytes = Number(getArg('preview-backpressure-low-bytes', String(128 * 1024)));
let playerDelayWriteQueue = Promise.resolve();
const p2pSessions = new Map();
let nodeDataChannelPromise = null;

function send(message) {
  if (!ws || ws.readyState !== WebSocket.OPEN) return false;
  ws.send(JSON.stringify({ protocol, ...message }));
  return true;
}

function sendPreviewMessage(message) {
  if (!send(message)) return false;
  return !ws || Number(ws.bufferedAmount || 0) < previewBackpressureHighBytes;
}

function log(message, fields = {}) {
  const suffix = Object.keys(fields).length ? ` ${JSON.stringify(fields)}` : '';
  console.log(`[usdx-bridge] ${message}${suffix}`);
}

function safeJsonParse(text) {
  try {
    return JSON.parse(text);
  } catch {
    return null;
  }
}

function parseIceServersForNodeDataChannel(value) {
  const raw = String(value || '').trim();
  if (!raw) return [];
  const parsed = safeJsonParse(raw);
  const entries = Array.isArray(parsed) ? parsed : (parsed && typeof parsed === 'object' ? [parsed] : null);
  if (!entries) return raw.split(',').map((url) => url.trim()).filter(Boolean);

  const urls = [];
  for (const entry of entries) {
    if (typeof entry === 'string') {
      urls.push(entry);
      continue;
    }
    if (!entry || typeof entry !== 'object') continue;
    const entryUrls = Array.isArray(entry.urls) ? entry.urls : [entry.urls];
    for (const url of entryUrls) {
      if (!url) continue;
      const text = String(url);
      if (/^turns?:/i.test(text) && entry.username && entry.credential && !text.includes('@')) {
        urls.push(text.replace(/^(turns?:)/i, `$1${entry.username}:${entry.credential}@`));
      } else {
        urls.push(text);
      }
    }
  }
  return urls;
}

const p2pIceServers = parseIceServersForNodeDataChannel(p2pIceServersArg);

async function loadNodeDataChannel() {
  if (!p2pEnabled) return null;
  if (!nodeDataChannelPromise) {
    nodeDataChannelPromise = import('node-datachannel')
      .then((module) => module.default || module)
      .catch((error) => {
        log('p2p unavailable', { error: error.message });
        return null;
      });
  }
  return nodeDataChannelPromise;
}

function compactPitchFrameSongTimeUs(batch, frame) {
  if (!Array.isArray(frame)) return Number(frame?.songTimeUs);
  const base = Number(batch.baseSongTimeUs);
  const delta = Number(frame[0]);
  if (!Number.isFinite(delta)) return NaN;
  return Number.isFinite(base) ? base + delta : delta;
}

function compactPitchFrameVoiced(frame) {
  return Array.isArray(frame) ? Boolean(frame[4]) : Boolean(frame?.voiced);
}

function compactPitchFrameHz(frame) {
  if (!Array.isArray(frame)) return frame?.f0Hz ?? null;
  const cents = Number(frame[1]);
  if (!Number.isFinite(cents) || cents <= 0) return null;
  return 440 * (2 ** ((cents - 6900) / 1200));
}

function sendToIpc(message) {
  const line = `${JSON.stringify({ protocol, ...message })}\n`;
  for (const client of ipcClients) {
    if (!client.destroyed) client.write(line);
  }
}

function hasIpcClient() {
  return ipcClients.size > 0;
}

function closeHostConnection(reason = 'ipc_disconnected') {
  if (ws && ws.readyState === WebSocket.OPEN) {
    log('closing host connection', { reason });
    try { ws.close(); } catch {}
  }
  room = null;
  players.clear();
  pitchStats.clear();
  closeAllP2pSessions();
}

function p2pDescriptionType(type) {
  const value = String(type || '').toLowerCase();
  if (value === 'offer') return 'Offer';
  if (value === 'answer') return 'Answer';
  return type || 'Unspec';
}

function closeP2pSession(playerId, reason = 'closed') {
  const session = p2pSessions.get(playerId);
  if (!session) return;
  p2pSessions.delete(playerId);
  try { session.dc?.close?.(); } catch {}
  try { session.pc?.close?.(); } catch {}
  log('p2p closed', { playerId, reason });
}

function closeAllP2pSessions(reason = 'closed') {
  for (const playerId of [...p2pSessions.keys()]) closeP2pSession(playerId, reason);
}

function sendP2p(playerId, message) {
  const dc = p2pSessions.get(playerId)?.dc;
  if (!dc) return false;
  try {
    dc.sendMessage(JSON.stringify({ protocol, roomId: room?.roomId || null, playerId, ...message }));
    return true;
  } catch {
    return false;
  }
}

async function handleP2pClientMessage(playerId, message) {
  const player = players.get(playerId);
  if (!player) return;
  const msg = {
    ...message,
    protocol,
    roomId: room?.roomId || null,
    playerId,
    slot: player.slot || null,
    role: player.role || null
  };

  if (msg.type === 'clock.sync.ping') {
    const hostReceiveUs = hostMonoUs();
    sendP2p(playerId, {
      type: 'clock.sync.pong',
      clientSendUs: msg.clientSendUs,
      hostReceiveUs,
      hostSendUs: hostMonoUs()
    });
    return;
  }

  if (msg.type === 'control.command' && player.role !== 'controller') {
    sendP2p(playerId, {
      type: 'ack',
      commandId: msg.commandId,
      accepted: false,
      reason: 'not_controller',
      gameStateSeq: seq
    });
    return;
  }

  if (['control.command', 'pitch.batch'].includes(msg.type)) {
    await handleMessage(msg);
  }
}

function attachP2pDataChannel(playerId, attempt, dc) {
  const session = p2pSessions.get(playerId);
  if (!session || session.attempt !== attempt) return;
  session.dc = dc;
  dc.onOpen?.(() => {
    log('p2p open', { playerId, attempt });
    send({ type: 'p2p.state', playerId, attempt, state: 'open' });
  });
  dc.onMessage?.((data) => {
    const text = Buffer.isBuffer(data) ? data.toString('utf8') : String(data);
    try {
      handleP2pClientMessage(playerId, JSON.parse(text)).catch((error) => {
        log('p2p message failed', { playerId, error: error.message });
      });
    } catch (error) {
      log('p2p bad message', { playerId, error: error.message });
    }
  });
  dc.onClosed?.(() => closeP2pSession(playerId, 'datachannel_closed'));
  dc.onError?.((error) => log('p2p datachannel error', { playerId, error: String(error?.message || error || 'error') }));
}

async function handleP2pOffer(msg) {
  const playerId = String(msg.playerId || '');
  const player = players.get(playerId);
  if (!player) return;

  const rtc = await loadNodeDataChannel();
  if (!rtc) {
    send({ type: 'p2p.error', playerId, attempt: msg.attempt, code: 'bridge.webrtc_unavailable' });
    return;
  }

  closeP2pSession(playerId, 'new_offer');
  const attempt = Number(msg.attempt || 0);
  const pc = new rtc.PeerConnection(`usdx-${playerId}`, { iceServers: p2pIceServers });
  p2pSessions.set(playerId, { pc, dc: null, attempt });
  pc.onLocalDescription((sdp, type) => {
    send({
      type: 'p2p.answer',
      playerId,
      attempt,
      description: {
        type: String(type || 'answer').toLowerCase(),
        sdp
      }
    });
  });
  pc.onLocalCandidate((candidate, mid) => {
    send({
      type: 'p2p.ice',
      playerId,
      attempt,
      candidate: {
        candidate,
        sdpMid: mid || '0'
      }
    });
  });
  pc.onDataChannel((dc) => attachP2pDataChannel(playerId, attempt, dc));

  const description = msg.description || { type: msg.descriptionType || msg.sdpType || 'offer', sdp: msg.sdp };
  if (!description.sdp) {
    send({ type: 'p2p.error', playerId, attempt, code: 'offer.bad_description' });
    closeP2pSession(playerId, 'bad_offer');
    return;
  }
  try {
    pc.setRemoteDescription(description.sdp, p2pDescriptionType(description.type));
  } catch (error) {
    log('p2p offer failed', { playerId, error: error.message });
    send({ type: 'p2p.error', playerId, attempt, code: 'offer.failed', reason: error.message });
    closeP2pSession(playerId, 'offer_failed');
  }
}

function handleP2pIce(msg) {
  const playerId = String(msg.playerId || '');
  const session = p2pSessions.get(playerId);
  if (!session) return;
  if (Number(msg.attempt || 0) && Number(msg.attempt) !== session.attempt) return;
  const candidate = msg.candidate && typeof msg.candidate === 'object'
    ? msg.candidate
    : { candidate: msg.candidate, sdpMid: msg.sdpMid || msg.mid };
  if (!candidate.candidate) return;
  try {
    session.pc.addRemoteCandidate(candidate.candidate, candidate.sdpMid || '0');
  } catch (error) {
    log('p2p candidate failed', { playerId, error: error.message });
  }
}

function scheduleConnect() {
  if (reconnectTimer) return;
  if (!hasIpcClient()) {
    log('connect deferred until USDX IPC is available');
    return;
  }
  reconnectTimer = setTimeout(() => {
    reconnectTimer = null;
    connect();
  }, reconnectDelayMs);
  reconnectTimer.unref?.();
}

function normalizeHostPath(filePath) {
  const value = String(filePath || '');
  if (!value) return '';
  if (process.platform !== 'win32') {
    const match = value.match(/^([A-Za-z]):[\\/](.*)$/);
    if (match) return `/mnt/${match[1].toLowerCase()}/${match[2].replaceAll('\\', '/')}`;
  }
  return value;
}

function resolveHostPath(filePath, baseDir = process.cwd()) {
  const normalized = normalizeHostPath(filePath);
  if (!normalized) return '';
  if (path.isAbsolute(normalized)) return path.normalize(normalized);
  return path.resolve(baseDir, normalized);
}

function contentTypeForAudio(filePath) {
  const ext = path.extname(filePath).toLowerCase();
  if (ext === '.m4a' || ext === '.mp4') return 'audio/mp4';
  if (ext === '.mp3' || ext === '.mp2' || ext === '.mp1' || ext === '.mpeg' || ext === '.mpg') return 'audio/mpeg';
  if (ext === '.ogg' || ext === '.oga') return 'audio/ogg';
  if (ext === '.wav') return 'audio/wav';
  if (ext === '.flac') return 'audio/flac';
  return 'audio/mpeg';
}

function bufferAscii(buffer, start, length) {
  return buffer.subarray(start, start + length).toString('ascii');
}

function looksLikeMpegAudio(buffer) {
  for (let index = 0; index < buffer.length - 1; index += 1) {
    if (buffer[index] === 0xff && (buffer[index + 1] & 0xe0) === 0xe0) return true;
  }
  return false;
}

function sniffContentTypeForAudio(filePath) {
  let fd = null;
  try {
    fd = fs.openSync(filePath, 'r');
    const buffer = Buffer.alloc(4096);
    const bytesRead = fs.readSync(fd, buffer, 0, buffer.length, 0);
    const bytes = buffer.subarray(0, bytesRead);
    if (bytes.length >= 10 && bufferAscii(bytes, 0, 3) === 'ID3') return 'audio/mpeg';
    if (looksLikeMpegAudio(bytes)) return 'audio/mpeg';
    if (bytes.length >= 12 && bufferAscii(bytes, 4, 4) === 'ftyp') return 'audio/mp4';
    if (bytes.length >= 4 && bufferAscii(bytes, 0, 4) === 'OggS') return 'audio/ogg';
    if (bytes.length >= 4 && bufferAscii(bytes, 0, 4) === 'fLaC') return 'audio/flac';
    if (bytes.length >= 12 && bufferAscii(bytes, 0, 4) === 'RIFF' && bufferAscii(bytes, 8, 4) === 'WAVE') return 'audio/wav';
  } catch {
    // Fall back to extension-based MIME below.
  } finally {
    if (fd != null) {
      try { fs.closeSync(fd); } catch {}
    }
  }
  return contentTypeForAudio(filePath);
}

function normalizeAudioExtensionHint(formatHint = '') {
  const hint = String(formatHint || '').toLowerCase().replace(/^\./, '').replace(/[^a-z0-9]/g, '');
  return hint;
}

function audioPathWithExtension(filePath, formatHint = '') {
  const ext = normalizeAudioExtensionHint(formatHint);
  if (!filePath || !ext) return filePath;
  const candidate = path.join(path.dirname(filePath), `${path.basename(filePath, path.extname(filePath))}.${ext}`);
  return fs.existsSync(candidate) ? candidate : filePath;
}

function sanitizePlaylistName(value) {
  return String(value || 'Unnamed')
    .replace(/[\u0000-\u001f\u007f]/g, '')
    .replace(/[<>:"/\\|?*]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim()
    .slice(0, 48) || 'Unnamed';
}

function playlistDirectory() {
  if (playlistDirArg) return resolveHostPath(playlistDirArg, repoRoot);
  return path.join(gameDir, 'playlists');
}

function songLine(song) {
  const artist = String(song?.artist || '').trim();
  const title = String(song?.title || '').trim();
  if (!artist || !title) return '';
  return `${artist} : ${title}`;
}

function songKey(artist, title) {
  return `${String(artist || '').trim().toLowerCase()}\u0000${String(title || '').trim().toLowerCase()}`;
}

function libraryByArtistTitle() {
  const map = new Map();
  for (const song of scannedLibrary) {
    const key = songKey(song.artist, song.title);
    if (key !== '\u0000' && !map.has(key)) map.set(key, song);
  }
  return map;
}

function uniqueLibrarySongByArtistTitle(artist, title) {
  const key = songKey(artist, title);
  if (key === '\u0000') return null;
  const matches = scannedLibrary.filter((song) => songKey(song.artist, song.title) === key);
  return matches.length === 1 ? matches[0] : null;
}

function playlistPathForName(name) {
  const dir = playlistDirectory();
  let base = sanitizePlaylistName(name);
  let candidate = path.join(dir, `${base}.upl`);
  let suffix = 2;
  while (fs.existsSync(candidate)) {
    candidate = path.join(dir, `${base}${suffix}.upl`);
    suffix += 1;
  }
  return candidate;
}

async function readPlaylistFile(filePath, index = -1, songMap = libraryByArtistTitle()) {
  const text = await fsp.readFile(filePath, 'utf8');
  const lines = text.split(/\r?\n/);
  let name = path.basename(filePath, path.extname(filePath));
  let fixedOrder = false;
  let inSongs = false;
  const items = [];
  for (const rawLine of lines) {
    const line = rawLine.trim();
    if (!line) continue;
    if (line.toLowerCase().startsWith('#name:')) {
      name = line.slice(line.indexOf(':') + 1).trim() || name;
      continue;
    }
    if (line.toLowerCase().startsWith('#fixedorder:')) {
      fixedOrder = /:\s*on/i.test(line);
      continue;
    }
    if (line.toLowerCase() === '#songs:') {
      inSongs = true;
      continue;
    }
    if (!inSongs || line.startsWith('#')) continue;
    const separator = line.indexOf(' : ');
    const artist = separator >= 0 ? line.slice(0, separator).trim() : '';
    const title = separator >= 0 ? line.slice(separator + 3).trim() : line;
    const song = songMap.get(songKey(artist, title));
    items.push({
      itemIndex: items.length,
      songId: Number.isInteger(Number(song?.songId)) ? Number(song.songId) : -1,
      artist,
      title,
      previewStart: Number(song?.previewStart || 0),
      previewAvailable: Boolean(song?.previewAvailable),
      audioPath: song?.audioPath || ''
    });
  }
  return {
    index,
    name,
    filename: path.basename(filePath),
    filePath,
    fixedOrder,
    size: items.length,
    current: index === currentPlaylistIndex,
    items
  };
}

async function loadPlaylists() {
  const dir = playlistDirectory();
  await fsp.mkdir(dir, { recursive: true });
  const names = await fsp.readdir(dir);
  const songMap = libraryByArtistTitle();
  const playlists = [];
  for (const filename of names.filter((name) => name.toLowerCase().endsWith('.upl')).sort((a, b) => a.localeCompare(b))) {
    try {
      const playlist = await readPlaylistFile(path.join(dir, filename), playlists.length, songMap);
      playlists.push(playlist);
    } catch (error) {
      log('playlist read failed', { filename, error: error.message });
    }
  }
  if (currentPlaylistIndex >= playlists.length) currentPlaylistIndex = playlists.length ? 0 : -1;
  return playlists.map((playlist, index) => ({ ...playlist, index, current: index === currentPlaylistIndex }));
}

async function writePlaylistFile(playlist) {
  const filePath = playlist.filePath || playlistPathForName(playlist.name);
  await fsp.mkdir(path.dirname(filePath), { recursive: true });
  const lines = [
    '######################################',
    '#Ultrastar Deluxe Playlist Format v1.0',
    `#Playlist ${playlist.name} with ${playlist.items.length} Songs.`,
    '######################################',
    `#Name: ${playlist.name}`,
    `#FixedOrder: ${playlist.fixedOrder ? 'On' : 'Off'}`,
    '#Songs:',
    ...playlist.items.map((item) => `${item.artist} : ${item.title}`)
  ];
  await fsp.writeFile(filePath, `${lines.join('\n')}\n`, 'utf8');
}

function sendCommandAck(msg, accepted, reason = null) {
  send({
    type: 'ack',
    playerId: msg.playerId,
    commandId: msg.commandId,
    accepted,
    reason,
    gameStateSeq: seq
  });
}

async function handleBridgePlaylistCommand(msg) {
  const command = String(msg.command || '');
  if (![
    'playlist.load',
    'playlist.create',
    'playlist.delete',
    'playlist.addSongs',
    'playlist.removeSong',
    'playlist.removeItem',
    'playlist.moveItemUp',
    'playlist.moveItemDown'
  ].includes(command)) {
    return false;
  }

  try {
    const args = msg.args || {};
    let playlists = await loadPlaylists();
    const playlistIndex = Number(args.playlistIndex ?? args.index ?? -1);
    if (command === 'playlist.create') {
      const name = sanitizePlaylistName(args.text || args.name || 'Unnamed');
      const playlist = { name, fixedOrder: false, items: [], filePath: playlistPathForName(name) };
      await writePlaylistFile(playlist);
      playlists = await loadPlaylists();
      currentPlaylistIndex = playlists.findIndex((item) => item.filename === path.basename(playlist.filePath));
    } else if (command === 'playlist.load') {
      if (!Number.isInteger(playlistIndex) || playlistIndex < 0 || playlistIndex >= playlists.length) throw new Error('bad_playlist');
      currentPlaylistIndex = playlistIndex;
    } else if (command === 'playlist.delete') {
      if (!Number.isInteger(playlistIndex) || playlistIndex < 0 || playlistIndex >= playlists.length) throw new Error('bad_playlist');
      await fsp.unlink(playlists[playlistIndex].filePath);
      if (currentPlaylistIndex === playlistIndex) currentPlaylistIndex = -1;
      else if (currentPlaylistIndex > playlistIndex) currentPlaylistIndex -= 1;
    } else {
      if (!Number.isInteger(playlistIndex) || playlistIndex < 0 || playlistIndex >= playlists.length) throw new Error('bad_playlist');
      const playlist = playlists[playlistIndex];
      if (command === 'playlist.addSongs') {
        const existing = new Set(playlist.items.map((item) => `${item.artist} : ${item.title}`));
        for (const rawSongId of Array.isArray(args.songIds) ? args.songIds : [args.songId]) {
          const songId = Number(rawSongId);
          const song = scannedLibrary.find((item) => Number(item.songId) === songId);
          const line = songLine(song);
          if (line && !existing.has(line)) {
            playlist.items.push({
              itemIndex: playlist.items.length,
              songId,
              artist: song.artist,
              title: song.title,
              previewStart: Number(song.previewStart || 0),
              previewAvailable: Boolean(song.previewAvailable),
              audioPath: song.audioPath || ''
            });
            existing.add(line);
          }
        }
      } else if (command === 'playlist.removeSong') {
        const songId = Number(args.songId);
        const index = playlist.items.findIndex((item) => Number(item.songId) === songId);
        if (index >= 0) playlist.items.splice(index, 1);
      } else if (command === 'playlist.removeItem') {
        const itemIndex = Number(args.itemIndex);
        if (!Number.isInteger(itemIndex) || itemIndex < 0 || itemIndex >= playlist.items.length) throw new Error('bad_item');
        playlist.items.splice(itemIndex, 1);
      } else if (command === 'playlist.moveItemUp' || command === 'playlist.moveItemDown') {
        const itemIndex = Number(args.itemIndex);
        const nextIndex = command === 'playlist.moveItemUp' ? itemIndex - 1 : itemIndex + 1;
        if (!Number.isInteger(itemIndex) || itemIndex < 0 || itemIndex >= playlist.items.length || nextIndex < 0 || nextIndex >= playlist.items.length) throw new Error('bad_item');
        const [item] = playlist.items.splice(itemIndex, 1);
        playlist.items.splice(nextIndex, 0, item);
      }
      playlist.items = playlist.items.map((item, itemIndex) => ({ ...item, itemIndex }));
      await writePlaylistFile(playlist);
      currentPlaylistIndex = playlistIndex;
    }
    await publishScannedSongState(scannedLibrary, { broadcast: !hasIpcClient() });
    sendCommandAck(msg, true);
    log('playlist command handled', { command, playlistIndex, playerId: msg.playerId });
  } catch (error) {
    sendCommandAck(msg, false, error.message || 'playlist_error');
    log('playlist command failed', { command, error: error.message });
  }
  return true;
}

function parseIniSections(text) {
  const result = {};
  let section = '';
  for (const rawLine of String(text || '').split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line || line.startsWith(';') || line.startsWith('#')) continue;
    const sectionMatch = line.match(/^\[([^\]]+)\]$/);
    if (sectionMatch) {
      section = sectionMatch[1].trim();
      result[section] ||= {};
      continue;
    }
    const equals = line.indexOf('=');
    if (equals < 0 || !section) continue;
    const key = line.slice(0, equals).trim();
    const value = line.slice(equals + 1).trim();
    result[section] ||= {};
    result[section][key] = value;
  }
  return result;
}

function clampDelayMs(value) {
  const number = Number(value);
  if (!Number.isFinite(number)) return 0;
  return Math.max(-500, Math.min(500, Math.round(number / 10) * 10));
}

function micDelayFromSettings(settings, fallback = 0) {
  if (!settings || typeof settings !== 'object') return clampDelayMs(fallback);
  if (Object.prototype.hasOwnProperty.call(settings, 'micDelayMs')) return clampDelayMs(settings.micDelayMs);
  if (Object.prototype.hasOwnProperty.call(settings, 'playerDelayMs')) return clampDelayMs(settings.playerDelayMs);
  return clampDelayMs(fallback);
}

function replaceIniValue(text, sectionName, keyName, value) {
  const newline = String(text || '').includes('\r\n') ? '\r\n' : '\n';
  const lines = String(text || '') ? String(text || '').split(/\r?\n/) : [];
  const wantedSection = sectionName.toLowerCase();
  const wantedKey = keyName.toLowerCase();
  let sectionStart = -1;
  let sectionEnd = lines.length;

  for (let index = 0; index < lines.length; index += 1) {
    const match = lines[index].trim().match(/^\[([^\]]+)\]$/);
    if (!match) continue;
    if (sectionStart >= 0) {
      sectionEnd = index;
      break;
    }
    if (match[1].trim().toLowerCase() === wantedSection) {
      sectionStart = index;
    }
  }

  if (sectionStart < 0) {
    if (lines.length && lines.at(-1) !== '') lines.push('');
    lines.push(`[${sectionName}]`, `${keyName}=${value}`);
    return lines.join(newline);
  }

  for (let index = sectionStart + 1; index < sectionEnd; index += 1) {
    const line = lines[index];
    const equals = line.indexOf('=');
    if (equals < 0) continue;
    const key = line.slice(0, equals).trim();
    if (key.toLowerCase() === wantedKey) {
      lines[index] = `${keyName}=${value}`;
      return lines.join(newline);
    }
  }

  lines.splice(sectionEnd, 0, `${keyName}=${value}`);
  return lines.join(newline);
}

async function writePlayerDelayConfig(reason = 'update') {
  const configPath = resolveHostPath(songConfig);
  const delaySlots = Number.isFinite(maxIniPlayerDelays)
    ? Math.max(1, Math.min(64, Math.round(maxIniPlayerDelays)))
    : 12;
  const delays = Array.from({ length: delaySlots }, () => 0);
  for (const player of players.values()) {
    const slot = Number(player.slot);
    if (Number.isInteger(slot) && slot >= 1 && slot <= delays.length) {
      delays[slot - 1] = clampDelayMs(player.micDelayMs);
    }
  }

  let text = '';
  try {
    text = await fsp.readFile(configPath, 'utf8');
  } catch (error) {
    if (error.code !== 'ENOENT') throw error;
  }

  let next = text;
  delays.forEach((delay, index) => {
    next = replaceIniValue(next, 'PlayerDelay', `P${index + 1}`, String(delay));
  });
  if (next !== text) {
    await fsp.mkdir(path.dirname(configPath), { recursive: true });
    await fsp.writeFile(configPath, next, 'utf8');
    log('player delay config updated', { config: configPath, reason, delays });
  }
}

function schedulePlayerDelayConfigWrite(reason) {
  playerDelayWriteQueue = playerDelayWriteQueue
    .catch(() => {})
    .then(() => writePlayerDelayConfig(reason))
    .catch((error) => {
      log('player delay config update failed', { error: error.message });
    });
}

function songRootsFromConfig() {
  const roots = [];
  function addRoot(root) {
    if (root) roots.push(path.normalize(root));
  }

  if (songRootArg) {
    for (const part of songRootArg.split(path.delimiter)) {
      const root = resolveHostPath(part);
      addRoot(root);
    }
  }

  const configPath = resolveHostPath(songConfig);
  try {
    const config = fs.readFileSync(configPath, 'utf8');
    const ini = parseIniSections(config);
    const directories = ini.Directories || ini.directories || {};
    for (const [key, value] of Object.entries(directories)) {
      if (/^SongDir/i.test(key) && String(value || '').trim()) {
        addRoot(resolveHostPath(value, path.dirname(configPath)));
      }
    }
  } catch (error) {
    log('song config unavailable', { config: configPath, error: error.message });
  }

  // Match USDX defaults beyond config.ini: shared/user "songs" folders.
  // In dev setups this can be repo-local, game-local, or drive-root \songs.
  for (const fallback of [
    path.resolve(repoRoot, 'songs'),
    path.resolve(gameDir, 'songs'),
    path.resolve(path.parse(repoRoot).root, 'songs'),
    path.resolve(path.parse(configPath).root, 'songs')
  ]) {
    addRoot(fallback);
  }
  const wslDrive = repoRoot.match(/^\/mnt\/([A-Za-z])\//);
  if (wslDrive) addRoot(`/mnt/${wslDrive[1].toLowerCase()}/songs`);
  const msysDrive = repoRoot.match(/^\/([A-Za-z])\//);
  if (msysDrive) {
    addRoot(`/${msysDrive[1].toLowerCase()}/songs`);
    addRoot(`/mnt/${msysDrive[1].toLowerCase()}/songs`);
  }

  return [...new Set(roots.filter(Boolean))];
}

function songMetaFromTxt(text, txtPath) {
  const meta = {
    artist: '',
    title: '',
    year: 0,
    audio: '',
    previewStart: 0,
    duet: false
  };
  for (const rawLine of String(text || '').split(/\r?\n/)) {
    if (!rawLine.startsWith('#')) continue;
    const colon = rawLine.indexOf(':');
    if (colon < 0) continue;
    const key = rawLine.slice(1, colon).trim().toUpperCase();
    const value = rawLine.slice(colon + 1).trim();
    if (key === 'ARTIST') meta.artist = value;
    else if (key === 'TITLE') meta.title = value;
    else if (key === 'YEAR') meta.year = Number.parseInt(value, 10) || 0;
    else if (key === 'MP3' || key === 'AUDIO') meta.audio = value;
    else if (key === 'PREVIEWSTART' || key === 'PREVIEW') meta.previewStart = Number(value.replace(',', '.')) || 0;
    else if (key === 'DUETSINGERP1' || key === 'DUETSINGERP2') meta.duet = true;
  }
  if (!meta.title) meta.title = path.basename(txtPath, path.extname(txtPath)).replaceAll('_', ' ');
  return meta;
}

function candidateAudioPath(txtPath, audioName) {
  const dir = path.dirname(txtPath);
  const extensions = ['.mp3', '.mp4', '.m4a', '.mp2', '.mp1', '.mpeg', '.mpg', '.ogg', '.oga', '.wav', '.flac'];
  if (audioName) {
    const requested = resolveHostPath(audioName, dir);
    if (fs.existsSync(requested)) return requested;
    const requestedStem = path.join(path.dirname(requested), path.basename(requested, path.extname(requested)));
    for (const ext of extensions) {
      const candidate = `${requestedStem}${ext}`;
      if (fs.existsSync(candidate)) return candidate;
    }
  }
  const stem = path.join(dir, path.basename(txtPath, path.extname(txtPath)));
  for (const ext of extensions) {
    const candidate = `${stem}${ext}`;
    if (fs.existsSync(candidate)) return candidate;
  }
  return '';
}

async function collectSongTxtFiles(root, progress) {
  const files = [];
  const visited = new Set();
  async function walk(dir) {
    let realDir = dir;
    try {
      realDir = await fsp.realpath(dir);
    } catch {}
    if (visited.has(realDir)) return;
    visited.add(realDir);
    progress.scannedDirs += 1;
    if (progress.scannedDirs % 100 === 0) sendSongLibraryProgress(progress);
    let entries;
    try {
      entries = await fsp.readdir(dir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      let isDirectory = entry.isDirectory();
      let isFile = entry.isFile();
      if (entry.isSymbolicLink()) {
        try {
          const stat = await fsp.stat(fullPath);
          isDirectory = stat.isDirectory();
          isFile = stat.isFile();
        } catch {
          isDirectory = false;
          isFile = false;
        }
      }
      if (isDirectory) {
        await walk(fullPath);
      } else if (isFile && entry.name.toLowerCase().endsWith('.txt')) {
        files.push(fullPath);
        progress.scannedFiles += 1;
        if (progress.scannedFiles % 200 === 0) sendSongLibraryProgress(progress);
      }
    }
  }
  await walk(root);
  return files;
}

function sendSongLibraryProgress(progress = songScanProgress) {
  songScanProgress = {
    ...songScanProgress,
    ...progress,
    updatedAt: new Date().toISOString()
  };
  send({
    type: 'song.library.progress',
    status: songScanProgress.status,
    indexedSongs: songScanProgress.indexedSongs,
    scannedFiles: songScanProgress.scannedFiles,
    scannedDirs: songScanProgress.scannedDirs,
    roots: songScanProgress.roots,
    message: songScanProgress.message || ''
  });
}

async function publishScannedSongState(library, options = {}) {
  const broadcast = options.broadcast ?? !hasIpcClient();
  const playlists = await loadPlaylists();
  const selectedPlaylist = currentPlaylistIndex >= 0 ? playlists[currentPlaylistIndex] : null;
  scannedSongState = {
    type: 'song.select.state',
    source: 'bridge-scan',
    selectedSongId: library[0]?.songId ?? -1,
    visibleSongs: library.length,
    totalSongs: library.length,
    searchActive: false,
    playlistActive: false,
    currentSong: library[0] || null,
    results: library.slice(0, 30),
    library,
    playlists: playlists.map((playlist) => ({
      index: playlist.index,
      name: playlist.name,
      size: playlist.size,
      current: playlist.current,
      items: playlist.items || []
    })),
    playlistItems: selectedPlaylist?.items || [],
    curPlaylist: selectedPlaylist ? selectedPlaylist.index : -1,
    libraryProgress: songScanProgress
  };
  if (broadcast) send(sanitizeSongStateForServer(scannedSongState));
}

async function scanSongLibrary() {
  if (!songScan) return;
  if (songScanStarted) {
    sendSongLibraryProgress();
    if (!hasIpcClient() && scannedSongState) send(sanitizeSongStateForServer(scannedSongState));
    return;
  }
  songScanStarted = true;
  const roots = songRootsFromConfig().filter((root) => {
    try {
      return fs.existsSync(root) && fs.statSync(root).isDirectory();
    } catch {
      return false;
    }
  });
  sendSongLibraryProgress({ status: roots.length ? 'scanning' : 'no_roots', roots, message: roots.length ? 'Scanning song folders' : 'No song folders found' });
  if (!roots.length) return;

  const progress = { ...songScanProgress, status: 'scanning', roots, indexedSongs: 0, scannedFiles: 0, scannedDirs: 0 };
  const txtFiles = [];
  for (const root of roots) {
    txtFiles.push(...await collectSongTxtFiles(root, progress));
    sendSongLibraryProgress(progress);
  }
  txtFiles.sort((a, b) => a.localeCompare(b));

  const library = [];
  for (const txtPath of txtFiles) {
    try {
      const text = await fsp.readFile(txtPath, 'utf8');
      const meta = songMetaFromTxt(text, txtPath);
      const audioPath = candidateAudioPath(txtPath, meta.audio);
      const songId = library.length;
      const song = {
        songId,
        artist: meta.artist,
        title: meta.title,
        year: meta.year,
        visible: true,
        category: false,
        duet: meta.duet,
        previewStart: meta.previewStart,
        previewAvailable: Boolean(audioPath && fs.existsSync(audioPath)),
        audioPath
      };
      library.push(song);
      rememberSongPreview(song);
      progress.indexedSongs = library.length;
      if (library.length % 250 === 0) sendSongLibraryProgress(progress);
    } catch (error) {
      log('song parse failed', { file: txtPath, error: error.message });
    }
  }
  sendSongLibraryProgress({ ...progress, status: 'ready', indexedSongs: library.length, message: `Indexed ${library.length} songs` });
  scannedLibrary = library;
  await publishScannedSongState(library, { broadcast: !hasIpcClient() });
  log('song scan complete', { songs: library.length, roots: roots.length });
}

function rememberSongPreview(song) {
  if (!song || typeof song !== 'object') return;
  const songId = Number(song.songId);
  if (!Number.isInteger(songId) || !song.audioPath) return;
  songPreviewPaths.set(songId, {
    filePath: normalizeHostPath(song.audioPath),
    title: song.title || '',
    artist: song.artist || '',
    previewStart: Number(song.previewStart || 0)
  });
}

function sanitizeSongStateForServer(msg) {
  const copy = JSON.parse(JSON.stringify(msg));
  const songs = [];
  if (copy.currentSong) songs.push(copy.currentSong);
  if (Array.isArray(copy.results)) songs.push(...copy.results);
  if (Array.isArray(copy.library)) songs.push(...copy.library);
  if (Array.isArray(copy.playlists)) {
    for (const playlist of copy.playlists) {
      if (Array.isArray(playlist.items)) songs.push(...playlist.items);
    }
  }
  if (Array.isArray(copy.playlistItems)) songs.push(...copy.playlistItems);
  for (const song of songs) {
    rememberSongPreview(song);
    if (song.audioPath) song.audioFileName = path.basename(song.audioPath);
    delete song.audioPath;
  }
  return copy;
}

function streamPreviewToServer(requestId, songId, startSec = 0, purpose = 'preview', rangeStart = null, rangeEnd = null, formatHint = '') {
  const info = songPreviewPaths.get(Number(songId));
  if (!info || !info.filePath) {
    log('preview.error', { requestId, songId, purpose, reason: 'preview_not_found' });
    send({ type: 'song.preview.error', requestId, reason: 'preview_not_found' });
    return;
  }
  if (activePreviewStreams.size >= maxActivePreviewStreams) {
    log('preview.error', { requestId, songId, purpose, reason: 'too_many_active_previews', active: activePreviewStreams.size });
    send({ type: 'song.preview.error', requestId, reason: 'too_many_active_previews' });
    return;
  }

  const requestPurpose = String(purpose || 'song-preview');
  const requestedFilePath = audioPathWithExtension(info.filePath, formatHint);
  const contentType = sniffContentTypeForAudio(requestedFilePath);
  const seekSec = 0;
  let contentLength = null;
  let fileSize = null;
  try {
    fileSize = fs.statSync(requestedFilePath).size;
    contentLength = fileSize;
  } catch {
    fileSize = null;
    contentLength = null;
  }
  let streamStart = 0;
  let streamEnd = null;
  const requestedRangeStart = Number(rangeStart);
  const requestedRangeEnd = Number(rangeEnd);
  const hasRange = Number.isSafeInteger(requestedRangeStart) && requestedRangeStart >= 0 && Number.isFinite(fileSize) && fileSize > 0;
  if (hasRange) {
    streamStart = Math.min(requestedRangeStart, Math.max(0, fileSize - 1));
    streamEnd = Number.isSafeInteger(requestedRangeEnd) && requestedRangeEnd >= streamStart
      ? Math.min(requestedRangeEnd, fileSize - 1)
      : fileSize - 1;
    contentLength = Math.max(0, streamEnd - streamStart + 1);
  }
  const streamOptions = { highWaterMark: 64 * 1024 };
  if (hasRange) {
    streamOptions.start = streamStart;
    streamOptions.end = streamEnd;
  }
  const stream = fs.createReadStream(requestedFilePath, streamOptions);
  let bytesSent = 0;
  let resumeTimer = null;
  const clearResumeTimer = () => {
    if (resumeTimer) clearInterval(resumeTimer);
    resumeTimer = null;
  };
  const cleanupStream = () => {
    clearResumeTimer();
    activePreviewStreams.delete(requestId);
  };
  const waitForPreviewDrain = () => {
    if (resumeTimer) return;
    stream.pause();
    resumeTimer = setInterval(() => {
      if (!ws || ws.readyState !== WebSocket.OPEN) {
        stream.destroy();
        return;
      }
      if (Number(ws.bufferedAmount || 0) <= previewBackpressureLowBytes) {
        clearResumeTimer();
        stream.resume();
      }
    }, 25);
    resumeTimer.unref?.();
  };
  activePreviewStreams.set(requestId, {
    destroy() {
      stream.destroy();
    }
  });
  log('preview.start', {
    requestId,
    songId: Number(songId),
    purpose: requestPurpose,
    startSec: seekSec,
    rangeStart: hasRange ? streamStart : null,
    rangeEnd: hasRange ? streamEnd : null,
    transcoded: false,
    contentType,
    contentLength,
    file: path.basename(requestedFilePath),
    formatHint: normalizeAudioExtensionHint(formatHint)
  });
  send({
    type: 'song.preview.meta',
    requestId,
    songId: Number(songId),
    statusCode: hasRange ? 206 : 200,
    contentType,
    contentLength,
    contentRange: hasRange ? `bytes ${streamStart}-${streamEnd}/${fileSize}` : null,
    fileName: path.basename(requestedFilePath),
    title: info.title,
    artist: info.artist,
    previewStart: info.previewStart,
    startSec: seekSec,
    purpose: requestPurpose,
    transcoded: false
  });
  stream.on('data', (chunk) => {
    bytesSent += chunk.length;
    const canContinue = sendPreviewMessage({ type: 'song.preview.chunk', requestId, data: chunk.toString('base64') });
    if (!canContinue) waitForPreviewDrain();
  });
  stream.on('end', () => {
    cleanupStream();
    log('preview.end', { requestId, songId: Number(songId), purpose: requestPurpose, bytes: bytesSent });
    send({ type: 'song.preview.end', requestId });
  });
  stream.on('error', (error) => {
    cleanupStream();
    log('preview.error', { requestId, songId: Number(songId), purpose: requestPurpose, reason: error.message, bytes: bytesSent });
    send({ type: 'song.preview.error', requestId, reason: error.message });
  });
  stream.on('close', () => {
    cleanupStream();
  });
}

function handleIpcMessage(msg) {
  if (!msg || typeof msg.type !== 'string') return;
  if (msg.protocol && msg.protocol !== protocol) {
    sendToIpc({ type: 'error', code: 'protocol.unsupported', source: 'bridge.ipc' });
    return;
  }

  if (msg.type === 'bridge.ping') {
    sendToIpc({ type: 'bridge.pong', hostMonoUs: hostMonoUs(), roomId: room?.roomId || null, displayCode: room?.displayCode || null });
    return;
  }

  const receivedHostUs = hostMonoUs();
  if (msg.type === 'game.state') {
    seq = Math.max(seq, Number(msg.seq || 0) + 1);
  } else if (msg.type === 'song.select.state') {
    msg.hostStateUs = receivedHostUs;
  } else if (msg.type === 'song.started' || msg.type === 'song.resumed') {
    songSeq = Number(msg.songSeq || songSeq || 0);
    const mediaStartUs = Number(msg.mediaStartUs || 0);
    msg.hostStateUs = receivedHostUs;
    if (!Number.isFinite(Number(msg.hostSongStartUs))) {
      msg.hostSongStartUs = receivedHostUs - Math.max(0, mediaStartUs);
    }
    if (!Number.isInteger(Number(msg.songId))) {
      const song = uniqueLibrarySongByArtistTitle(msg.artist, msg.title);
      if (song && Number.isInteger(Number(song.songId))) {
        msg.songId = Number(song.songId);
      }
    }
    lastSongClockMessage = { ...msg };
  } else if (msg.type === 'score.snapshot' || msg.type === 'song.position') {
    const mediaStartUs = Number(msg.mediaStartUs || 0);
    msg.hostStateUs = receivedHostUs;
    if (Number.isFinite(mediaStartUs) && mediaStartUs >= 0 && !Number.isFinite(Number(msg.hostSongStartUs))) {
      msg.hostSongStartUs = receivedHostUs - mediaStartUs;
    }
    lastSongClockMessage = { ...msg };
  } else if (msg.type === 'song.paused' || msg.type === 'song.ended') {
    const mediaStartUs = Number(msg.mediaStartUs || 0);
    msg.hostStateUs = receivedHostUs;
    if (Number.isFinite(mediaStartUs) && mediaStartUs >= 0 && !Number.isFinite(Number(msg.hostSongStartUs))) {
      msg.hostSongStartUs = receivedHostUs - mediaStartUs;
    }
    if (msg.type === 'song.ended') lastSongClockMessage = null;
    else lastSongClockMessage = { ...msg };
  }

  const allowed = new Set([
    'game.state',
    'song.starting',
    'song.started',
    'song.paused',
    'song.position',
    'song.lyrics',
    'song.resumed',
    'song.ended',
    'score.snapshot',
    'song.select.state',
    'playlist.state',
    'playlist.automation.state',
    'playlist.automation.event',
    'player.assigned',
    'ack',
    'clock.sync.pong',
    'song.state.request',
    'error'
  ]);
  if (!allowed.has(msg.type)) {
    sendToIpc({ type: 'error', code: 'bridge.ipc_ignored', ignoredType: msg.type });
    return;
  }

  send(msg.type === 'song.select.state' ? sanitizeSongStateForServer(msg) : msg);
}

function startIpcServer() {
  if (!Number.isFinite(ipcPort) || ipcPort <= 0) {
    log('ipc disabled');
    return;
  }

  const server = net.createServer((socket) => {
    socket.setEncoding('utf8');
    ipcClients.add(socket);
    log('ipc client connected', { remote: `${socket.remoteAddress}:${socket.remotePort}` });
    if (!ws || ws.readyState === WebSocket.CLOSED || ws.readyState === WebSocket.CLOSING) {
      scheduleConnect();
    }
    socket.write(`${JSON.stringify({
      protocol,
      type: 'bridge.ready',
      roomId: room?.roomId || null,
      displayCode: room?.displayCode || null,
      hostMonoUs: hostMonoUs()
    })}\n`);

    let buffer = '';
    socket.on('data', (chunk) => {
      buffer += chunk;
      let newline = buffer.indexOf('\n');
      while (newline >= 0) {
        const line = buffer.slice(0, newline).trim();
        buffer = buffer.slice(newline + 1);
        newline = buffer.indexOf('\n');
        if (!line) continue;
        try {
          handleIpcMessage(JSON.parse(line));
        } catch (error) {
          socket.write(`${JSON.stringify({ protocol, type: 'error', code: 'json.invalid', reason: error.message })}\n`);
        }
      }
    });
    socket.on('close', () => {
      ipcClients.delete(socket);
      log('ipc client disconnected');
      if (!hasIpcClient()) closeHostConnection('ipc_disconnected');
    });
    socket.on('error', (error) => {
      ipcClients.delete(socket);
      log('ipc client error', { error: error.message });
      if (!hasIpcClient()) closeHostConnection('ipc_error');
    });
  });

  server.on('error', (error) => {
    log('ipc listen failed', { ipcHost, ipcPort, error: error.message });
  });
  server.listen(ipcPort, ipcHost, () => {
    log('ipc listening', { ipcHost, ipcPort });
  });
}

function gameState(state = 'lobby') {
  return {
    type: 'game.state',
    seq: seq++,
    state,
    songSeq,
    maxPlayers,
    players: [...players.values()].map((player) => ({
      slot: player.slot,
      playerId: player.playerId,
      name: player.name,
      connected: player.connected !== false,
      singing: player.role === 'singer' || player.role === 'controller',
      role: player.role,
      micDelayMs: player.micDelayMs || 0
    })),
    controllerPlayerId: [...players.values()].find((player) => player.role === 'controller')?.playerId || null,
    playlistSize: 0
  };
}

function startMockSong() {
  songSeq += 1;
  const hostSongStartUs = hostMonoUs() + 2_000_000;
  send({
    type: 'song.started',
    songSeq,
    hostSongStartUs,
    mediaStartUs: 0,
    title: 'Bridge mock song',
    artist: 'USDX Remote'
  });
  send(gameState('singing'));
  log('mock song clock started', { songSeq, startsInMs: 2000 });
}

async function handleMessage(msg) {
  if (msg.type === 'p2p.offer') {
    await handleP2pOffer(msg);
    return;
  }

  if (msg.type === 'p2p.ice') {
    handleP2pIce(msg);
    return;
  }

  if (msg.type === 'p2p.close') {
    closeP2pSession(String(msg.playerId || ''), 'client_closed');
    return;
  }

  if (msg.type === 'room.created') {
    if (!room || room.roomId !== msg.roomId) {
      players.clear();
      pitchStats.clear();
      closeAllP2pSessions('new_room');
    }
    room = msg;
    log('room created', { code: msg.displayCode, roomId: msg.roomId });
    console.log(`Join at https://usdx.at`);
    console.log(`Code: ${msg.displayCode}`);
    sendToIpc({ type: 'room.created', roomId: msg.roomId, displayCode: msg.displayCode, expiresAt: msg.expiresAt });
    send(gameState('lobby'));
    schedulePlayerDelayConfigWrite('room.created');
    scanSongLibrary().catch((error) => {
      sendSongLibraryProgress({ status: 'error', message: error.message });
      log('song scan failed', { error: error.message });
    });
    return;
  }

  if (msg.type === 'room.resumed') {
    room = msg;
    log('room resumed', { code: msg.displayCode, roomId: msg.roomId });
    console.log(`Join at https://usdx.at`);
    console.log(`Code: ${msg.displayCode}`);
    sendToIpc({ type: 'room.resumed', roomId: msg.roomId, displayCode: msg.displayCode, expiresAt: msg.expiresAt });
    send(gameState(songSeq ? 'singing' : 'lobby'));
    schedulePlayerDelayConfigWrite('room.resumed');
    scanSongLibrary().catch((error) => {
      sendSongLibraryProgress({ status: 'error', message: error.message });
      log('song scan failed', { error: error.message });
    });
    return;
  }

  if (msg.type === 'game.state') {
    if (Number.isFinite(Number(msg.maxPlayers))) {
      maxPlayers = Math.max(1, Math.min(12, Number(msg.maxPlayers)));
    }
    sendToIpc({ ...msg, protocol });
    return;
  }

  if (msg.type === 'player.joined') {
    players.set(msg.playerId, {
      playerId: msg.playerId,
      name: msg.name,
      slot: msg.slot,
      role: msg.role,
      micDelayMs: clampDelayMs(msg.micDelayMs),
      connected: true
    });
    log('player joined', { name: msg.name, slot: msg.slot, role: msg.role });
    sendToIpc({
      type: 'player.joined',
      roomId: room?.roomId,
      playerId: msg.playerId,
      name: msg.name,
      slot: msg.slot,
      role: msg.role,
      micDelayMs: clampDelayMs(msg.micDelayMs)
    });
    if (autoAssign) {
      send({
        type: 'player.assigned',
        playerId: msg.playerId,
        slot: msg.slot,
        role: msg.role,
        micDelayMs: clampDelayMs(msg.micDelayMs)
      });
    }
    send(gameState('lobby'));
    schedulePlayerDelayConfigWrite('player.joined');
    if (mockSong && players.size === 1 && songSeq === 0) startMockSong();
    return;
  }

  if (msg.type === 'player.assigned') {
    const player = players.get(msg.playerId);
    if (player) {
      player.slot = msg.slot ?? player.slot;
      player.role = msg.role || player.role;
      if (Object.prototype.hasOwnProperty.call(msg, 'micDelayMs')) {
        player.micDelayMs = clampDelayMs(msg.micDelayMs);
      }
      player.connected = true;
    }
    log('player assigned', { playerId: msg.playerId, slot: msg.slot, role: msg.role });
    sendToIpc({
      type: 'player.assigned',
      roomId: room?.roomId,
      playerId: msg.playerId,
      name: player?.name || '',
      slot: msg.slot,
      role: msg.role,
      micDelayMs: player?.micDelayMs || 0
    });
    send(gameState(songSeq ? 'singing' : 'lobby'));
    schedulePlayerDelayConfigWrite('player.assigned');
    return;
  }

  if (msg.type === 'player.left') {
    closeP2pSession(msg.playerId, 'player_left');
    players.delete(msg.playerId);
    log('player left', { playerId: msg.playerId });
    sendToIpc({ type: 'player.left', roomId: room?.roomId, playerId: msg.playerId, reason: msg.reason || 'disconnect' });
    send(gameState(songSeq ? 'singing' : 'lobby'));
    schedulePlayerDelayConfigWrite('player.left');
    return;
  }

  if (msg.type === 'player.renamed') {
    const player = players.get(msg.playerId);
    if (player) player.name = msg.name || player.name;
    log('player renamed', { playerId: msg.playerId, name: msg.name });
    sendToIpc({ type: 'player.renamed', roomId: room?.roomId, playerId: msg.playerId, name: msg.name });
    send(gameState(songSeq ? 'singing' : 'lobby'));
    return;
  }

  if (msg.type === 'player.disconnected') {
    const player = players.get(msg.playerId);
    if (player) player.connected = false;
    closeP2pSession(msg.playerId, 'player_disconnected');
    log('player disconnected', { playerId: msg.playerId });
    sendToIpc({ type: 'player.disconnected', roomId: room?.roomId, playerId: msg.playerId, reason: msg.reason || 'disconnect' });
    send(gameState(songSeq ? 'singing' : 'lobby'));
    return;
  }

  if (msg.type === 'playlist.automation.state' || msg.type === 'playlist.automation.event') {
    log('playlist automation', {
      type: msg.type,
      enabled: msg.enabled,
      phase: msg.automationPhase,
      remaining: msg.countdownRemaining
    });
    sendToIpc({ ...msg, protocol });
    return;
  }

  if (msg.type === 'song.preview.request') {
    streamPreviewToServer(
      String(msg.requestId || ''),
      Number(msg.songId),
      Number(msg.startSec || 0),
      msg.purpose,
      msg.rangeStart,
      msg.rangeEnd,
      msg.formatHint
    );
    return;
  }

  if (msg.type === 'song.preview.cancel') {
    const source = activePreviewStreams.get(String(msg.requestId || ''));
    if (source) {
      source.destroy();
      activePreviewStreams.delete(String(msg.requestId || ''));
      log('preview.cancelled', { requestId: String(msg.requestId || '') });
    }
    return;
  }

  if (msg.type === 'song.state.request') {
    if (lastSongClockMessage) send(lastSongClockMessage);
    sendSongLibraryProgress();
    if (!hasIpcClient() && scannedSongState) send(sanitizeSongStateForServer(scannedSongState));
    return;
  }

  if (msg.type === 'settings.update') {
    const player = players.get(msg.playerId);
    if (player) {
      player.micDelayMs = micDelayFromSettings(msg.settings, player.micDelayMs);
      log('player settings updated', { playerId: msg.playerId, slot: player.slot, micDelayMs: player.micDelayMs });
      sendToIpc({
        type: 'settings.update',
        roomId: room?.roomId,
        playerId: msg.playerId,
        slot: player.slot,
        role: player.role,
        settings: {
          ...(msg.settings && typeof msg.settings === 'object' ? msg.settings : {}),
          micDelayMs: player.micDelayMs,
          playerDelayMs: player.micDelayMs
        }
      });
      send(gameState(songSeq ? 'singing' : 'lobby'));
      schedulePlayerDelayConfigWrite('settings.update');
    }
    return;
  }

  if (msg.type === 'control.command') {
    if (await handleBridgePlaylistCommand(msg)) return;
    log('control', { playerId: msg.playerId, command: msg.command, commandId: msg.commandId, ipcClients: ipcClients.size });
    sendToIpc({
      type: 'control.command',
      roomId: room?.roomId,
      playerId: msg.playerId,
      slot: players.get(msg.playerId)?.slot || null,
      role: players.get(msg.playerId)?.role || null,
      commandId: msg.commandId,
      command: msg.command,
      args: msg.args || {},
      clientIssuedUs: msg.clientIssuedUs
    });
    if (autoAck) {
      send({
        type: 'ack',
        playerId: msg.playerId,
        commandId: msg.commandId,
        accepted: true,
        reason: null,
        gameStateSeq: seq
      });
    }
    return;
  }

  if (msg.type === 'pitch.batch') {
    const frames = Array.isArray(msg.frames) ? msg.frames : [];
    const voiced = frames.filter((frame) => compactPitchFrameVoiced(frame)).length;
    const last = frames.at(-1);
    const stats = pitchStats.get(msg.playerId) || { batches: 0, frames: 0, voiced: 0, lastHz: null, lastSongTimeMs: null };
    stats.batches += 1;
    stats.frames += frames.length;
    stats.voiced += voiced;
    stats.songSeq = msg.songSeq;
    stats.lastHz = compactPitchFrameHz(last) ?? stats.lastHz;
    const lastSongTimeUs = compactPitchFrameSongTimeUs(msg, last);
    stats.lastSongTimeMs = Number.isFinite(lastSongTimeUs) ? Math.round(lastSongTimeUs / 1000) : stats.lastSongTimeMs;
    pitchStats.set(msg.playerId, stats);
    sendToIpc({
      type: 'pitch.batch',
      roomId: room?.roomId,
      playerId: msg.playerId,
      slot: players.get(msg.playerId)?.slot || null,
      role: players.get(msg.playerId)?.role || null,
      songSeq: msg.songSeq,
      compact: msg.compact,
      batchSeq: msg.batchSeq,
      baseSongTimeUs: msg.baseSongTimeUs,
      frameDurUs: msg.frameDurUs,
      frames
    });
    return;
  }

  if (msg.type === 'clock.sync.ping') {
    const hostReceiveUs = hostMonoUs();
    send({
      type: 'clock.sync.pong',
      playerId: msg.playerId,
      clientSendUs: msg.clientSendUs,
      hostReceiveUs,
      hostSendUs: hostMonoUs()
    });
    return;
  }

  if (msg.type === 'error') {
    log('server error', { code: msg.code, reason: msg.reason });
    if (msg.code === 'host.resume_failed') {
      room = null;
      players.clear();
      pitchStats.clear();
    }
    sendToIpc({ type: 'error', code: msg.code, reason: msg.reason, source: 'server' });
  }
}

function connect() {
  if (!hasIpcClient()) {
    log('connect skipped; no USDX IPC client');
    return;
  }
  const target = wsUrl(server);
  log('connecting', { target });
  ws = new WebSocket(target);
  ws.addEventListener('open', () => {
    log('connected');
    send({
      type: 'host.hello',
      gameVersion: 'usdx-dev',
      bridgeVersion: '0.1.0',
      capabilities: ['remotePitch', 'remoteControl', 'clockSync', ...(p2pEnabled ? ['p2pDataChannel'] : [])],
      maxPlayers,
      roomId: room?.roomId,
      hostToken: room?.hostToken
    });
  });
  ws.addEventListener('message', (event) => {
    try {
      handleMessage(JSON.parse(event.data)).catch((error) => {
        log('message handler failed', { error: error.message });
      });
    } catch (error) {
      log('bad message', { error: error.message });
    }
  });
  ws.addEventListener('close', () => {
    log('disconnected');
    sendToIpc({ type: 'bridge.disconnected' });
    if (hasIpcClient()) scheduleConnect();
  });
  ws.addEventListener('error', (event) => {
    log('socket error', { message: event.message || 'websocket error' });
  });
}

setInterval(() => {
  if (room && hasIpcClient()) {
    send({ type: 'host.heartbeat' });
  } else if (room && !hasIpcClient()) {
    closeHostConnection('ipc_unavailable');
  }
}, 1_000).unref();

setInterval(() => {
  for (const [playerId, stats] of pitchStats) {
    if (stats.batches > 0) {
      log('pitch.summary', { playerId, ...stats });
      pitchStats.set(playerId, { batches: 0, frames: 0, voiced: 0, songSeq: stats.songSeq, lastHz: stats.lastHz, lastSongTimeMs: stats.lastSongTimeMs });
    }
  }
}, 1000).unref();

startIpcServer();
scheduleConnect();
