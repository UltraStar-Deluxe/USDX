const LIGHT_THEME = "light", DARK_THEME = "dark";

const isValidTheme = theme => theme === LIGHT_THEME || theme === DARK_THEME;

const systemTheme = () =>
    window.matchMedia("(prefers-color-scheme: dark)").matches ? DARK_THEME : LIGHT_THEME;

const storedTheme = () => localStorage.getItem("theme");

const getTheme = () => {
    const stored = storedTheme();
    return isValidTheme(stored) ? stored : systemTheme();
};

const isLightTheme = () => getTheme() === LIGHT_THEME;

const applyTheme = theme => {
    document.documentElement.setAttribute("data-theme", theme);
    localStorage.setItem("theme", theme);
    return theme;
};

const setTheme = theme => isValidTheme(theme) ? applyTheme(theme) : theme;

const toggleTheme = () => setTheme(isLightTheme() ? DARK_THEME : LIGHT_THEME);

const syncToggle = toggle => { toggle.checked = isLightTheme(); };

// initialize theme
setTheme(getTheme());

window.onload = () => {
    const toggle = document.querySelector("#themeToggle");
    toggle.addEventListener("change", () => { toggleTheme(); syncToggle(toggle); });
    syncToggle(toggle);
};
