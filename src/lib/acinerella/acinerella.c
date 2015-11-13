/*
    This file is part of Acinerella.

    Acinerella is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Acinerella is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Acinerella.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdlib.h>
#include <stdbool.h>
#include "acinerella.h"
#include <libavformat/avformat.h>
#include <libavformat/avio.h>
#include <libavcodec/avcodec.h>
#include <libavutil/avutil.h>
#include <libswscale/swscale.h>
#include <string.h>

#define AUDIO_BUFFER_BASE_SIZE AVCODEC_MAX_AUDIO_FRAME_SIZE


//This struct represents one Acinerella video object.
//It contains data needed by FFMpeg.

#define AC_BUFSIZE 1024*64

struct _ac_data {
  ac_instance instance;
  
  AVFormatContext *pFormatCtx;
  
  void *sender;
  ac_openclose_callback open_proc;
  ac_read_callback read_proc; 
  ac_seek_callback seek_proc; 
  ac_openclose_callback close_proc; 
  
  ByteIOContext io;
  void* buffer; 
};

typedef struct _ac_data ac_data;
typedef ac_data* lp_ac_data;

struct _ac_decoder_data {
  ac_decoder decoder;
  int sought;
  double last_timecode;
};

typedef struct _ac_decoder_data ac_decoder_data;
typedef ac_decoder_data* lp_ac_decoder_data;

struct _ac_video_decoder {
  ac_decoder decoder;
  int sought;
  double last_timecode;
  AVCodec *pCodec;
  AVCodecContext *pCodecCtx;
  AVFrame *pFrame;
  AVFrame *pFrameRGB; 
  struct SwsContext *pSwsCtx;  
};

typedef struct _ac_video_decoder ac_video_decoder;
typedef ac_video_decoder* lp_ac_video_decoder;

struct _ac_audio_decoder {
  ac_decoder decoder;
  int sought;
  double last_timecode;
  char *tmp_buf;
  int max_buffer_size;
  AVCodec *pCodec;
  AVCodecContext *pCodecCtx;
};

typedef struct _ac_audio_decoder ac_audio_decoder;
typedef ac_audio_decoder* lp_ac_audio_decoder;

struct _ac_package_data {
  ac_package package;
  AVPacket ffpackage;
  int pts;
};

typedef struct _ac_package_data ac_package_data;
typedef ac_package_data* lp_ac_package_data;

//
//--- Initialization and Stream opening---
//

void init_info(lp_ac_file_info info)
{
  info->title[0] = 0;
  info->author[0] = 0;
  info->copyright[0] = 0;
  info->comment[0] = 0;
  info->album[0] = 0;
  info->year = -1;
  info->track = -1;
  info->genre[0] = 0;
  info->duration = -1;
  info->bitrate = -1;
}

int av_initialized = 0;
void ac_init_ffmpeg()
{
  if(!av_initialized)
  {
    avcodec_register_all();
    av_register_all();
    av_initialized = 1;
  }
}

lp_ac_instance CALL_CONVT ac_init(void) { 
  ac_init_ffmpeg();
  
  //Allocate a new instance of the videoplayer data and return it
  lp_ac_data ptmp;  
  ptmp = (lp_ac_data)av_malloc(sizeof(ac_data));
  
  //Initialize the created structure
  memset(ptmp, 0, sizeof(ac_data));
  
  ptmp->instance.opened = 0;
  ptmp->instance.stream_count = 0;
  ptmp->instance.output_format = AC_OUTPUT_BGR24;
  init_info(&(ptmp->instance.info));
  return (lp_ac_instance)ptmp;  
}

void CALL_CONVT ac_free(lp_ac_instance pacInstance) {
  //Close the decoder. If it is already closed, this won't be a problem as ac_close checks the streams state
  ac_close(pacInstance);
  
  if (pacInstance != NULL) {
    av_free((lp_ac_data)pacInstance);
  }
}

static int io_read(void *opaque, uint8_t *buf, int buf_size)
{
  if (((lp_ac_data)(opaque))->read_proc != NULL) {
    return ((lp_ac_data)(opaque))->read_proc(
      ((lp_ac_data)(opaque))->sender, buf, buf_size);
  }
  
  return -1;  
}

static int64_t io_seek(void *opaque, int64_t pos, int whence)
{
  if (((lp_ac_data)(opaque))->seek_proc != NULL) {
    if ((whence >= 0) && (whence <= 2)) {
      return ((lp_ac_data)(opaque))->seek_proc(
        ((lp_ac_data)(opaque))->sender, pos, whence);
    }
  }

  return -1;
}

uint64_t global_video_pkt_pts = AV_NOPTS_VALUE;

int ac_get_buffer(struct AVCodecContext *c, AVFrame *pic) {
  int ret = avcodec_default_get_buffer(c, pic);
  uint64_t *pts = av_malloc(sizeof(uint64_t));
  *pts = global_video_pkt_pts;
  pic->opaque = pts;
  return ret;
}

void ac_release_buffer(struct AVCodecContext *c, AVFrame *pic){
  if (pic) av_freep(&pic->opaque);
  avcodec_default_release_buffer(c, pic);
}

lp_ac_proberesult CALL_CONVT ac_probe_input_buffer(
  char* buf,
  int bufsize,
  char* filename,
  int* score_max) 
{
  AVProbeData pd;
  AVInputFormat *fmt = NULL;
  
  //Initialize FFMpeg libraries
  ac_init_ffmpeg();
  
  //Set the filename
  pd.filename = "";
  if (filename) {
    pd.filename = filename;
  }  
  
  //The given buffer has to be copied to a new one, which is aligned and padded
  char *aligned_buf = av_malloc(bufsize + AVPROBE_PADDING_SIZE);
  memset(aligned_buf, 0, bufsize + AVPROBE_PADDING_SIZE);
  memcpy(aligned_buf, buf, bufsize);
  
  //Set the probe data buffer
  pd.buf = aligned_buf;
  pd.buf_size = bufsize;
  
  //Test it
  fmt = av_probe_input_format2(&pd, 1, score_max);
  
  //Free the temporary buffer
  av_free(aligned_buf);
  
  return (lp_ac_proberesult)fmt;
} 

#define PROBE_BUF_MIN 2048
#define PROBE_BUF_MAX (1<<20)

AVInputFormat* ac_probe_input_stream(
  void* sender,
  ac_read_callback read_proc, 
  char* filename,
  void* *buf,
  int* buf_read)
{
  //Initialize the result variables
  AVInputFormat* fmt = NULL;
  *buf_read = 0;
  *buf = NULL;
  int last_iteration = 0;
  int probe_size = 0;
  
  for (probe_size = PROBE_BUF_MIN;
       (probe_size <= PROBE_BUF_MAX) && !fmt && !last_iteration;
       probe_size<<=1) {    
    int score = AVPROBE_SCORE_MAX / 4;
        
    //Allocate some memory for the current probe buffer
    void* tmp_buf = av_malloc(probe_size); //Unaligned memory would also be ok here
	memset(tmp_buf, 0, probe_size); 
        
    //Copy the old data to the new buffer
    if (*buf) {
      memcpy(tmp_buf, *buf, *buf_read);      
      //Free the old data memory
      av_free(*buf);      
    }    

    //Read the new data 
    void* write_ptr = tmp_buf + *buf_read;
    int read_size = probe_size - *buf_read;
    int size;
    if (size = read_proc(sender, write_ptr, read_size) < read_size) {
      last_iteration = 1;
      probe_size = *buf_read + size;
    }
    
    //Probe it
    fmt = (AVInputFormat*)ac_probe_input_buffer(tmp_buf, probe_size, filename, &score);

    //Set the new buffer
    *buf = tmp_buf;
    *buf_read = probe_size;
  }
  
  //Return the result
  return fmt;
}

int CALL_CONVT ac_open(
  lp_ac_instance pacInstance,
  void *sender, 
  ac_openclose_callback open_proc,
  ac_read_callback read_proc, 
  ac_seek_callback seek_proc,
  ac_openclose_callback close_proc,
  lp_ac_proberesult proberesult)
{ 
  pacInstance->opened = 0;
    
  //Store the given parameters in the ac Instance
  ((lp_ac_data)pacInstance)->sender = sender;
  ((lp_ac_data)pacInstance)->open_proc = open_proc;  
  ((lp_ac_data)pacInstance)->read_proc = read_proc;
  ((lp_ac_data)pacInstance)->seek_proc = seek_proc;
  ((lp_ac_data)pacInstance)->close_proc = close_proc;   

  //Call the file open proc
  if (open_proc != NULL) {
    open_proc(sender);
  }    
 
  AVInputFormat* fmt = NULL;
  int probe_size = 0;
  
  //Probe the input format, if no probe result is specified
  if(proberesult == NULL)
  {
    fmt = ac_probe_input_stream(sender, read_proc, "",
    (void*)&((lp_ac_data)pacInstance)->buffer, &probe_size);         
  }
  else
  {
    fmt = (AVInputFormat*)proberesult;
  }
  
  if (!fmt) return -1;

  if (!seek_proc) {
    init_put_byte(
      &(((lp_ac_data)pacInstance)->io),
      ((lp_ac_data)pacInstance)->buffer,
      probe_size, 0, pacInstance, io_read, 0, NULL);
    ((lp_ac_data)pacInstance)->io.is_streamed = 1;
    
    //Feed the probed bytes into the IO-Context 
    ((lp_ac_data)pacInstance)->io.buf_end =
      ((lp_ac_data)pacInstance)->buffer + probe_size;
    ((lp_ac_data)pacInstance)->io.pos = probe_size;    
  } else {
    //If the stream is seekable, seek back to the beginning of the stream and
    //let FFMpeg start from the beginning
    av_free(((lp_ac_data)pacInstance)->buffer);    

    seek_proc(sender, 0, SEEK_SET);

    //Reserve AC_BUFSIZE Bytes of memory
    ((lp_ac_data)pacInstance)->buffer = av_malloc(AC_BUFSIZE);       

    init_put_byte(
      &(((lp_ac_data)pacInstance)->io),
      ((lp_ac_data)pacInstance)->buffer,
      AC_BUFSIZE, 0, pacInstance, io_read, 0, io_seek);  
  }
  
  //Open the given input stream (the io structure) with the given format of the stream
  //(fmt) and write the pointer to the new format context to the pFormatCtx variable
  if (av_open_input_stream(
	&(((lp_ac_data)pacInstance)->pFormatCtx),
    &(((lp_ac_data)pacInstance)->io), "", fmt, NULL) < 0)
  {
    return -1;
  }   

  //Retrieve stream information
  AVFormatContext *ctx = ((lp_ac_data)pacInstance)->pFormatCtx;  
  if(av_find_stream_info(ctx) >= 0) {    
    strcpy(pacInstance->info.title, ctx->title);
    strcpy(pacInstance->info.author, ctx->author);
    strcpy(pacInstance->info.copyright, ctx->copyright);
    strcpy(pacInstance->info.comment, ctx->comment);
    strcpy(pacInstance->info.album, ctx->album);
    strcpy(pacInstance->info.genre, ctx->genre);    

    pacInstance->info.year = ctx->year;
    pacInstance->info.track = ctx->track;
    pacInstance->info.bitrate = ctx->bit_rate;     
   
    pacInstance->info.duration = ctx->duration * 1000 / AV_TIME_BASE;      
  } else {
    return -1;
  }

  //Set some information in the instance variable 
  pacInstance->stream_count = ((lp_ac_data)pacInstance)->pFormatCtx->nb_streams;
  pacInstance->opened = pacInstance->stream_count > 0;  

  return 0;
}

void CALL_CONVT ac_close(lp_ac_instance pacInstance) {
  if (pacInstance->opened) {    
    //Close the opened file
    if (((lp_ac_data)(pacInstance))->close_proc != NULL) {
      ((lp_ac_data)(pacInstance))->close_proc(((lp_ac_data)(pacInstance))->sender);
    }
   
    av_close_input_stream(((lp_ac_data)(pacInstance))->pFormatCtx);
    pacInstance->opened = 0;

    //If the seek proc has not been specified, the input buffer is not automatically
    //freed, as ffmpeg didn't get the original pointer to the buffer
    if (!((lp_ac_data)(pacInstance))->seek_proc &&
		((lp_ac_data)(pacInstance))->buffer) {
      av_free(((lp_ac_data)(pacInstance))->buffer);
    }

  }
}

void CALL_CONVT ac_get_stream_info(lp_ac_instance pacInstance, int nb, lp_ac_stream_info info) {
  if (!(pacInstance->opened)) { 
    return;
  }
  
  switch (((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->codec->codec_type) {
    case CODEC_TYPE_VIDEO:
      //Set stream type to "VIDEO"
      info->stream_type = AC_STREAM_TYPE_VIDEO;
      
      //Store more information about the video stream
      info->additional_info.video_info.frame_width = 
        ((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->codec->width;
      info->additional_info.video_info.frame_height = 
        ((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->codec->height;
	
	  double pixel_aspect_num = ((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->codec->sample_aspect_ratio.num;
	  double pixel_aspect_den = ((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->codec->sample_aspect_ratio.den;
		
	  //Sometime "pixel aspect" may be zero or have other invalid values. Correct this.
	  if (pixel_aspect_num <= 0.0 || pixel_aspect_den <= 0.0)
        info->additional_info.video_info.pixel_aspect = 1.0;
      else
	    info->additional_info.video_info.pixel_aspect = pixel_aspect_num / pixel_aspect_den;  
      
      info->additional_info.video_info.frames_per_second =
        (double)((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->r_frame_rate.num /
        (double)((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->r_frame_rate.den;
    break;
    case CODEC_TYPE_AUDIO:
      //Set stream type to "AUDIO"
      info->stream_type = AC_STREAM_TYPE_AUDIO;
      
      //Store more information about the video stream
      info->additional_info.audio_info.samples_per_second = 
        ((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->codec->sample_rate;        
      info->additional_info.audio_info.channel_count = 
        ((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->codec->channels;
      
      // Set bit depth      
      switch (((lp_ac_data)pacInstance)->pFormatCtx->streams[nb]->codec->sample_fmt) {
        //8-Bit
        case SAMPLE_FMT_U8:
          info->additional_info.audio_info.bit_depth = 
            8;                
        break;
        
        //16-Bit
        case SAMPLE_FMT_S16:
          info->additional_info.audio_info.bit_depth = 
              16;                            
        break;
        
/*        //24-Bit (removed in the newest ffmpeg version)
        case SAMPLE_FMT_S24:
          info->additional_info.audio_info.bit_depth = 
              24;                                          
        break; */
        
        //32-Bit
        case SAMPLE_FMT_S32: case SAMPLE_FMT_FLT:
          info->additional_info.audio_info.bit_depth = 
              32;                                          
        break;       
         
        //Unknown format, return zero
        default:
          info->additional_info.audio_info.bit_depth = 
            0;        
      }
        
    break;
    default:
      info->stream_type = AC_STREAM_TYPE_UNKNOWN;
  }
}

//
//---Package management---
//

lp_ac_package CALL_CONVT ac_read_package(lp_ac_instance pacInstance) {
  //Try to read package
  AVPacket Package;  
  if (av_read_frame(((lp_ac_data)(pacInstance))->pFormatCtx, &Package) >= 0) {
    //Reserve memory
    lp_ac_package_data pTmp = (lp_ac_package_data)(av_malloc(sizeof(ac_package_data)));
	memset(pTmp, 0, sizeof(ac_package_data));
    
    //Set package data
    pTmp->package.stream_index = Package.stream_index;
    pTmp->ffpackage = Package;
    if (Package.dts != AV_NOPTS_VALUE) {
      pTmp->pts = Package.dts;
    }
    
    return (lp_ac_package)(pTmp);
  } else {
    return NULL;
  }
}

//Frees the currently loaded package
void CALL_CONVT ac_free_package(lp_ac_package pPackage) {
  //Free the packet
  if (pPackage != NULL) {        
    AVPacket* pkt = &((lp_ac_package_data)pPackage)->ffpackage;
    if (pkt) {
      if (pkt->destruct) pkt->destruct(pkt);
      pkt->data = NULL; pkt->size = 0;
    }     
    av_free((lp_ac_package_data)pPackage);
  }
}

//
//--- Decoder management ---
//

enum PixelFormat convert_pix_format(ac_output_format fmt) {
  switch (fmt) {
    case AC_OUTPUT_RGB24: return PIX_FMT_RGB24;
    case AC_OUTPUT_BGR24: return PIX_FMT_BGR24;
    case AC_OUTPUT_RGBA32: return PIX_FMT_RGB32;
    case AC_OUTPUT_BGRA32: return PIX_FMT_BGR32;        
  }
  return PIX_FMT_RGB24;
}

//Init a video decoder
void* ac_create_video_decoder(lp_ac_instance pacInstance, lp_ac_stream_info info, int nb) {
  //Allocate memory for a new decoder instance
  lp_ac_video_decoder pDecoder;  
  pDecoder = (lp_ac_video_decoder)(av_malloc(sizeof(ac_video_decoder)));
  memset(pDecoder, 0, sizeof(ac_video_decoder));
  
  //Set a few properties
  pDecoder->decoder.pacInstance = pacInstance;
  pDecoder->decoder.type = AC_DECODER_TYPE_VIDEO;
  pDecoder->decoder.stream_index = nb;
  pDecoder->pCodecCtx = ((lp_ac_data)(pacInstance))->pFormatCtx->streams[nb]->codec;
  pDecoder->decoder.stream_info = *info;  
  
  //Find correspondenting codec
  if (!(pDecoder->pCodec = avcodec_find_decoder(pDecoder->pCodecCtx->codec_id))) {
    return NULL; //Codec could not have been found
  }
  
  //Open codec
  if (avcodec_open(pDecoder->pCodecCtx, pDecoder->pCodec) < 0) {
    return NULL; //Codec could not have been opened
  }
  
  //Reserve frame variables
  pDecoder->pFrame = avcodec_alloc_frame();
  pDecoder->pFrameRGB = avcodec_alloc_frame();
  
  pDecoder->pSwsCtx = NULL;
  
  //Reserve buffer memory
  pDecoder->decoder.buffer_size = avpicture_get_size(convert_pix_format(pacInstance->output_format), 
    pDecoder->pCodecCtx->width, pDecoder->pCodecCtx->height);
  pDecoder->decoder.pBuffer = (uint8_t*)av_malloc(pDecoder->decoder.buffer_size);

  //Link decoder to buffer
  avpicture_fill(
    (AVPicture*)(pDecoder->pFrameRGB), 
    pDecoder->decoder.pBuffer, convert_pix_format(pacInstance->output_format),
    pDecoder->pCodecCtx->width, pDecoder->pCodecCtx->height);
    
  return (void*)pDecoder;
}

//Init a audio decoder
void* ac_create_audio_decoder(lp_ac_instance pacInstance, lp_ac_stream_info info, int nb) {
  //Allocate memory for a new decoder instance
  lp_ac_audio_decoder pDecoder;
  pDecoder = (lp_ac_audio_decoder)(av_malloc(sizeof(ac_audio_decoder)));
  memset(pDecoder, 0, sizeof(ac_audio_decoder));
  
  //Set a few properties
  pDecoder->decoder.pacInstance = pacInstance;
  pDecoder->decoder.type = AC_DECODER_TYPE_AUDIO;
  pDecoder->decoder.stream_index = nb;
  pDecoder->decoder.stream_info = *info;
  
  //Temporary store codec context pointer
  AVCodecContext *pCodecCtx = ((lp_ac_data)(pacInstance))->pFormatCtx->streams[nb]->codec;
  pDecoder->pCodecCtx = pCodecCtx;  
  
  //Find correspondenting codec
  if (!(pDecoder->pCodec = avcodec_find_decoder(pCodecCtx->codec_id))) {
    return NULL;
  }
  
  //Open codec
  if (avcodec_open(pCodecCtx, pDecoder->pCodec) < 0) {
    return NULL;
  }

  //Initialize the buffers
  pDecoder->decoder.pBuffer = NULL; //av_malloc(AUDIO_BUFFER_BASE_SIZE);
  pDecoder->decoder.buffer_size = 0;
  pDecoder->max_buffer_size = 0;
  
  //Reserve the temporary buffer which contains AVCODEC_MAX_AUDIO_FRAME_SIZE bytes
  pDecoder->tmp_buf = av_malloc(AVCODEC_MAX_AUDIO_FRAME_SIZE);
  
  return (void*)pDecoder;
}

lp_ac_decoder CALL_CONVT ac_create_decoder(lp_ac_instance pacInstance, int nb) {
  //Get information about the chosen data stream and create an decoder that can
  //handle this kind of stream.
  ac_stream_info info;
  ac_get_stream_info(pacInstance, nb, &info);
  
  lp_ac_decoder result;
  
  if (info.stream_type == AC_STREAM_TYPE_VIDEO) {
    result = ac_create_video_decoder(pacInstance, &info, nb);
  } 
  else if (info.stream_type == AC_STREAM_TYPE_AUDIO) {
    result = ac_create_audio_decoder(pacInstance, &info, nb);  
  }
  
  ((lp_ac_decoder_data)result)->last_timecode = 0;
  ((lp_ac_decoder_data)result)->sought = 1;
  result->video_clock = 0;
  
  return result;
}

double ac_sync_video(lp_ac_package pPackage, lp_ac_decoder pDec, AVFrame *src_frame, double pts){
  double frame_delay;
  
  if(pts != 0){
    pDec->video_clock = pts;
  } else {
    pts = pDec->video_clock;
  }
  
  frame_delay = av_q2d(((lp_ac_data)pDec->pacInstance)->pFormatCtx->streams[pPackage->stream_index]->time_base);
  frame_delay += src_frame->repeat_pict * (frame_delay * 0.5);
  pDec->video_clock += frame_delay;
  return pts;
}

int ac_decode_video_package(lp_ac_package pPackage, lp_ac_video_decoder pDecoder, lp_ac_decoder pDec) {
  int finished;
  double pts;

  avcodec_decode_video2(
    pDecoder->pCodecCtx, pDecoder->pFrame, &finished, 
    &(((lp_ac_package_data)pPackage)->ffpackage));  

  if (finished) {
	pts=0;
    global_video_pkt_pts = ((lp_ac_package_data)pPackage)->ffpackage.pts;
	
    if(((lp_ac_package_data)pPackage)->ffpackage.dts == AV_NOPTS_VALUE &&
	  *(uint64_t*)pDecoder->pFrame->opaque != AV_NOPTS_VALUE ){
	  pts = *(uint64_t*)pDecoder->pFrame->opaque;
    } else if(((lp_ac_package_data)pPackage)->ffpackage.dts != AV_NOPTS_VALUE){
      pts = ((lp_ac_package_data)pPackage)->ffpackage.dts;
    } else {
	  pts = 0;
    }
	
	if(((lp_ac_data)pDec->pacInstance)->pFormatCtx->streams[pPackage->stream_index]->start_time != AV_NOPTS_VALUE){
      pts -= ((lp_ac_data)pDec->pacInstance)->pFormatCtx->streams[pPackage->stream_index]->start_time;
	}

    pts *= av_q2d(((lp_ac_data)pDec->pacInstance)->pFormatCtx->streams[pPackage->stream_index]->time_base);
	
    pts = ac_sync_video(pPackage, pDec, pDecoder->pFrame, pts);
	pDec->timecode = pts;

    pDecoder->pSwsCtx = sws_getCachedContext(pDecoder->pSwsCtx,
        pDecoder->pCodecCtx->width, pDecoder->pCodecCtx->height, pDecoder->pCodecCtx->pix_fmt,
        pDecoder->pCodecCtx->width, pDecoder->pCodecCtx->height, convert_pix_format(pDecoder->decoder.pacInstance->output_format),
                                  SWS_FAST_BILINEAR, NULL, NULL, NULL);
                                  
      sws_scale(
        pDecoder->pSwsCtx,
        (const uint8_t* const*)(pDecoder->pFrame->data),
        pDecoder->pFrame->linesize,
        0, //?
        pDecoder->pCodecCtx->height, 
        pDecoder->pFrameRGB->data, 
        pDecoder->pFrameRGB->linesize);
    return 1;
  }
  return 0;
}

int ac_decode_audio_package(lp_ac_package pPackage, lp_ac_audio_decoder pDecoder) {
  //Variables describing the destination buffer
  int dest_buffer_pos = 0;
  
  //Make a copy of the package read by avformat, so that we can move the data pointers around
  AVPacket pkt_tmp = ((lp_ac_package_data)pPackage)->ffpackage;
    
  //Initialize the buffer size
  pDecoder->decoder.buffer_size = 0;   
  
  while (pkt_tmp.size > 0) {  
    //Set the size of bytes that can be written to the current size of the destination buffer
    int size = AVCODEC_MAX_AUDIO_FRAME_SIZE;
    
    //Decode a piece of the audio buffer. len1 contains the count of bytes read from the soure buffer.
    int len1 = avcodec_decode_audio3(
      pDecoder->pCodecCtx, (int16_t*)(pDecoder->tmp_buf),
      &size, &pkt_tmp
    );
    
    //If an error occured, skip the frame
    if (len1 < 0){ 
      return 0;    
    }
	    
    //Increment the source buffer pointers     
    pkt_tmp.size -= len1;
    pkt_tmp.data += len1;
    
	if (size > 0){
      //Reserve enough memory for coping the result data
	  if (dest_buffer_pos + size > pDecoder->max_buffer_size) {
	    pDecoder->decoder.pBuffer = av_realloc(pDecoder->decoder.pBuffer, dest_buffer_pos + size);
		pDecoder->max_buffer_size = dest_buffer_pos + size;
	  }
	  memcpy(pDecoder->decoder.pBuffer + dest_buffer_pos, pDecoder->tmp_buf, size);
	  
      //Increment the destination buffer pointers, copy the result to the output buffer
      dest_buffer_pos += size;
      pDecoder->decoder.buffer_size += size;
    }	  
  }
  
  return 1;
}

int CALL_CONVT ac_decode_package(lp_ac_package pPackage, lp_ac_decoder pDecoder) {
  if (pDecoder->type == AC_DECODER_TYPE_AUDIO) {
      
    double timebase = 
    av_q2d(((lp_ac_data)pDecoder->pacInstance)->pFormatCtx->streams[pPackage->stream_index]->time_base);
  
	  //Create a valid timecode
	  if (((lp_ac_package_data)pPackage)->pts > 0) {
		lp_ac_decoder_data dec_dat = (lp_ac_decoder_data)pDecoder;    

		dec_dat->last_timecode = pDecoder->timecode;
		pDecoder->timecode = ((lp_ac_package_data)pPackage)->pts * timebase;
		
		double delta = pDecoder->timecode - dec_dat->last_timecode;
		double max_delta, min_delta;
		
		if (dec_dat->sought > 0) {
		  max_delta = 120.0;
		  min_delta = -120.0;
		  --dec_dat->sought;
		} else {
		  max_delta = 4.0;
		  min_delta = 0.0;
		}
		  
		if ((delta < min_delta) || (delta > max_delta)) {
		  pDecoder->timecode = dec_dat->last_timecode;
		  if (dec_dat->sought > 0) {
			++dec_dat->sought;
		  }
		}
	  }
    return ac_decode_audio_package(pPackage, (lp_ac_audio_decoder)pDecoder);
  } else if (pDecoder->type == AC_DECODER_TYPE_VIDEO) {
    return ac_decode_video_package(pPackage, (lp_ac_video_decoder)pDecoder, pDecoder);
  }
  return 0;
}

int CALL_CONVT ac_drop_decode_package(lp_ac_package pPackage, lp_ac_decoder pDecoder) {
  if (pDecoder->type == AC_DECODER_TYPE_VIDEO) {
    return ac_drop_decode_video_package(pPackage, (lp_ac_video_decoder)pDecoder, pDecoder);
  }
  return 0;
}

int ac_drop_decode_video_package(lp_ac_package pPackage, lp_ac_video_decoder pDecoder, lp_ac_decoder pDec) {
  int finished;
  double pts;

  avcodec_decode_video2(
    pDecoder->pCodecCtx, pDecoder->pFrame, &finished, 
    &(((lp_ac_package_data)pPackage)->ffpackage));  

  if (finished) {
	pts=0;
    global_video_pkt_pts = ((lp_ac_package_data)pPackage)->ffpackage.pts;
	
    if(((lp_ac_package_data)pPackage)->ffpackage.dts == AV_NOPTS_VALUE &&
	  *(uint64_t*)pDecoder->pFrame->opaque != AV_NOPTS_VALUE ){
	  pts = *(uint64_t*)pDecoder->pFrame->opaque;
    } else if(((lp_ac_package_data)pPackage)->ffpackage.dts != AV_NOPTS_VALUE){
      pts = ((lp_ac_package_data)pPackage)->ffpackage.dts;
    } else {
	  pts = 0;
    }
	
	if(((lp_ac_data)pDec->pacInstance)->pFormatCtx->streams[pPackage->stream_index]->start_time != AV_NOPTS_VALUE){
      pts -= ((lp_ac_data)pDec->pacInstance)->pFormatCtx->streams[pPackage->stream_index]->start_time;
	}

    pts *= av_q2d(((lp_ac_data)pDec->pacInstance)->pFormatCtx->streams[pPackage->stream_index]->time_base);
	
    pts = ac_sync_video(pPackage, pDec, pDecoder->pFrame, pts);
	pDec->timecode = pts;

    return 1;
  }
  return 0;
}

//Seek function
int CALL_CONVT ac_seek(lp_ac_decoder pDecoder, int dir, int64_t target_pos) {
  AVRational timebase = 
    ((lp_ac_data)pDecoder->pacInstance)->pFormatCtx->streams[pDecoder->stream_index]->time_base;
  
  int flags = dir < 0 ? AVSEEK_FLAG_BACKWARD : 0;    
  
  int64_t pos = av_rescale(target_pos, AV_TIME_BASE, 1000);
  
  ((lp_ac_decoder_data)pDecoder)->sought = 100;
  pDecoder->timecode = target_pos / 1000;
  //pDecoder->timecode = 0;
  if (av_seek_frame(((lp_ac_data)pDecoder->pacInstance)->pFormatCtx, pDecoder->stream_index, 
      av_rescale_q(pos, AV_TIME_BASE_Q, timebase), flags) >= 0) {
	avcodec_flush_buffers(((lp_ac_video_decoder)pDecoder)->pCodecCtx);
    return 1;
  }
  
  return 0;  
}

//Free video decoder
void ac_free_video_decoder(lp_ac_video_decoder pDecoder) {  
  av_free(pDecoder->pFrame);
  av_free(pDecoder->pFrameRGB);    
  if (pDecoder->pSwsCtx != NULL) {
    sws_freeContext(pDecoder->pSwsCtx);
  }
  avcodec_close(pDecoder->pCodecCtx);
  
  //Free reserved memory for the buffer
  av_free(pDecoder->decoder.pBuffer);
  
  //Free reserved memory for decoder record
  av_free(pDecoder);
}

//Free video decoder
void ac_free_audio_decoder(lp_ac_audio_decoder pDecoder) {
//  av_free(pDecoder->decoder.pBuffer);
  avcodec_close(pDecoder->pCodecCtx);
  
  //Free reserved memory for the buffer
  av_free(pDecoder->decoder.pBuffer);
  
  //Free the memory reserved for the temporary audio buffer
  av_free(pDecoder->tmp_buf);

  //Free reserved memory for decoder record
  av_free(pDecoder);
}

void CALL_CONVT ac_free_decoder(lp_ac_decoder pDecoder) {
  if (pDecoder->type == AC_DECODER_TYPE_VIDEO) {
    ac_free_video_decoder((lp_ac_video_decoder)pDecoder);
  }
  else if (pDecoder->type == AC_DECODER_TYPE_AUDIO) {
    ac_free_audio_decoder((lp_ac_audio_decoder)pDecoder);  
  }  
}