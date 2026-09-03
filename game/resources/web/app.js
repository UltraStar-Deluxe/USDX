const DEBOUNCE_MS = 100;

const COLUMNS = [
    { key: "artist", label: "Artist", numeric: false },
    { key: "title", label: "Title", numeric: false },
    { key: "edition", label: "Edition", numeric: false },
    { key: "genre", label: "Genre", numeric: false },
    { key: "year", label: "Year", numeric: true }
];

const td = value => {
    const el = document.createElement("td");
    el.textContent = value;
    return el;
};

const buildRow = song => {
    const tr = document.createElement("tr");
    tr.append(...COLUMNS.map(({ key }) => td(song[key])));
    return tr;
};

const normalize = value => String(value ?? "").toLocaleLowerCase();

const withSearchIndex = song => ({
    ...song,
    _search: normalize(COLUMNS.map(({ key }) => song[key]).join(" ")),
    _sort: Object.fromEntries(COLUMNS.map(({ key, numeric }) =>
        [key, numeric ? (parseInt(song[key], 10) || 0) : normalize(song[key])]))
});

const matchesQuery = query => song =>
    normalize(query) === "" || song._search.includes(normalize(query));

const compareBy = (key, dir) => (a, b) => {
    const { numeric } = COLUMNS.find(c => c.key === key);
    const av = a._sort[key];
    const bv = b._sort[key];
    const result = numeric ? av - bv : String(av).localeCompare(String(bv));
    return result * dir;
};

const updateCount = visible =>
    document.getElementById("songCount").textContent = `${visible} of ${songs.length} songs`;

const updateSortIndicators = (sortKey, sortDir) =>
    document.querySelectorAll("#songTable th").forEach(th => {
        const active = th.dataset.key === sortKey;
        th.classList.toggle("sort-asc", active && sortDir === 1);
        th.classList.toggle("sort-desc", active && sortDir === -1);
        th.setAttribute("aria-sort", !active ? "none" : sortDir === 1 ? "ascending" : "descending");
    });

const renderRows = sorted => {
    const tbody = document.querySelector("#songTable tbody");
    tbody.replaceChildren(...sorted.map(buildRow));
    return Array.from(tbody.rows);
};

const applyFilter = (rows, query) =>
    rows.forEach(row => {
        row.hidden = !matchesQuery(query)(row._song);
    });

let songs = [];
let sortState = { key: "artist", dir: 1 };

const sortRows = (rows, key, dir) =>
    [...rows].sort((ra, rb) => compareBy(key, dir)(ra._song, rb._song));

const onSort = key => {
    sortState = {
        key,
        dir: sortState.key === key ? -sortState.dir : 1
    };
    const tbody = document.querySelector("#songTable tbody");
    tbody.append(...sortRows(Array.from(tbody.rows), sortState.key, sortState.dir));
    updateSortIndicators(sortState.key, sortState.dir);
};

const init = async () => {
    const fail = () =>
        document.getElementById("songCount").textContent = "Failed to load songs.";

    try {
        const data = await (await fetch("/songs.json")).json();
        songs = data.songs.map(withSearchIndex);

        const rows = renderRows(songs);
        rows.forEach((row, i) => { row._song = songs[i]; });

        const sorted = sortRows(rows, sortState.key, sortState.dir);
        document.querySelector("#songTable tbody").append(...sorted);
        updateSortIndicators(sortState.key, sortState.dir);
        updateCount(songs.length);

        let debounceTimer;
        document.getElementById("search").addEventListener("input", e => {
            clearTimeout(debounceTimer);
            debounceTimer = setTimeout(() => {
                const currentRows = Array.from(document.querySelector("#songTable tbody").rows);
                applyFilter(currentRows, e.target.value);
                updateCount(currentRows.filter(r => !r.hidden).length);
            }, DEBOUNCE_MS);
        });

        document.querySelectorAll("#songTable th").forEach(th =>
            th.addEventListener("click", () => onSort(th.dataset.key)));
    } catch {
        fail();
    }
};

init();
