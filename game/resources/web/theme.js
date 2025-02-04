const LIGHT_THEME = "light", DARK_THEME = "dark";

function isValidTheme(theme) {
    return theme === LIGHT_THEME || theme === DARK_THEME
}

function getTheme() {
    let theme = localStorage.getItem("theme");
    if (isValidTheme(theme)) {
        return theme;
    }

    if (window.matchMedia("(prefers-color-scheme: dark)")) {
        return DARK_THEME;
    }

    return LIGHT_THEME;
}

function isLightTheme() {
    return getTheme() === LIGHT_THEME
}

function setTheme(theme) {
    if (!isValidTheme(theme)) return;

    document.documentElement.setAttribute("data-theme", theme);
    localStorage.setItem("theme", theme);
}

function toggleTheme() {
    if (isLightTheme()) {
        setTheme(DARK_THEME);
    } else {
        setTheme(LIGHT_THEME);
    }
}

// initialize theme
setTheme(getTheme());

function updateToggle(toggle) {
    toggle.checked = isLightTheme()
}

window.onload = function() {
    let toggle = document.querySelector("#themeToggle");
    
    toggle.addEventListener("change", () => {
        toggleTheme();
        updateToggle(toggle);
    });

    // initialize toggle
    updateToggle(toggle);
}
