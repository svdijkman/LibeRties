(function() {
  var STORAGE_KEY = "libertiesDarkTheme";

  function isDarkPreferred() {
    try {
      return localStorage.getItem(STORAGE_KEY) !== "0";
    } catch (e) {
      return true;
    }
  }

  function applyTheme(dark) {
    if (dark) {
      document.body.classList.add("theme-dark");
    } else {
      document.body.classList.remove("theme-dark");
    }
    var label = document.getElementById("theme_label");
    var toggle = document.getElementById("theme_toggle");
    if (label) {
      label.textContent = dark ? "Dark" : "Light";
    }
    if (toggle) {
      toggle.checked = !!dark;
    }
    try {
      localStorage.setItem(STORAGE_KEY, dark ? "1" : "0");
    } catch (e) {}
  }

  function initThemeToggle() {
    var toggle = document.getElementById("theme_toggle");
    if (!toggle || toggle.dataset.bound === "1") {
      return;
    }
    toggle.dataset.bound = "1";
    applyTheme(isDarkPreferred());
    toggle.addEventListener("change", function() {
      applyTheme(toggle.checked);
    });
  }

  $(document).on("shiny:connected", initThemeToggle);
  $(function() {
    applyTheme(isDarkPreferred());
    initThemeToggle();
  });
})();
