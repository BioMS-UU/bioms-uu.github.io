// Print support for the book; the layout itself lives in print.css.
//
// On every page:
//   - an iframe/video without a .print-only partner gets a .print-fallback box
//     with its web address, so it does not silently vanish from paper;
//   - tab panes learn their tab name, so a printed tabset shows all tabs;
//   - the end-of-chapter footnotes are named after their chapter;
//   - dark mode is switched off while printing, and back on afterwards.
//
// On the print page (print.qmd, which has <div id="print-book">):
//   - every chapter in the sidebar is fetched as rendered HTML and appended in
//     sidebar order, so the page can never fall behind the chapters and needs
//     no list to maintain. Add a chapter to _quarto.yml and it is in here.
//   - the reader picks which sections go on paper (GROUPS below); every
//     chapter is loaded once, and the tick boxes only show or hide them.
//   - NEVER lists pages that are not offered for paper at all.
(function () {
  "use strict";

  var NEVER = ["print.html", "slides.html", "planning.html", "spiekbriefjes.html"];

  // The tick boxes on the print page, in this order. A chapter belongs to the
  // first group whose `files` (file name, regex) or `parts` (sidebar part
  // name) matches it. A chapter that matches no group is always printed
  // (e.g. the dankwoord). `on` is the default.
  var GROUPS = [
    { key: "info",       label: "Cursusinformatie",                parts: ["Cursusinformatie"],     on: true },
    { key: "modellen",   label: "Biologische modellen",            parts: ["Biologische modellen"], on: true },
    { key: "statistiek", label: "Statistiek",                      parts: ["Statistiek"],           on: true },
    { key: "basis",      label: "Basisprincipes van R en Basiswiskunde",
                                                                   parts: ["Basisprincipes van R", "Basiswiskunde"], on: false },
    { key: "extra",      label: "Antwoorden en verdieping",        files: /^(antwoorden_.*|verdieping)\.html$/, on: false }
  ];

  function groupOf(ch) {
    for (var i = 0; i < GROUPS.length; i++) {
      var g = GROUPS[i];
      if (g.files && g.files.test(ch.file)) return g.key;
      if (g.parts && g.parts.indexOf(ch.part) !== -1) return g.key;
    }
    return "";
  }

  function chapterLabel(scope) {
    var num = scope.querySelector("h1 .chapter-number");
    var title = scope.querySelector("h1 .chapter-title") || scope.querySelector("h1.title");
    if (num && /^\d+$/.test(num.textContent.trim())) return "Hoofdstuk " + num.textContent.trim();
    if (num) return "Bijlage " + num.textContent.trim();
    return title ? title.textContent.replace(/ /g, " ").trim() : "";
  }

  function youtubeLink(src) {
    var m = src.match(/youtube(?:-nocookie)?\.com\/embed\/([\w-]+)/);
    return m ? "https://youtu.be/" + m[1] : src;
  }

  // -- stand-ins for paper ---------------------------------------------------
  function prepareForPaper(root) {
    // Quarto rewrites links to full addresses at load time, so CSS alone cannot
    // tell a link within the book from an external one. Mark ours: on paper
    // only external addresses are spelled out.
    root.querySelectorAll("a[href]").forEach(function (a) {
      if (a.href && new URL(a.href, location.href).origin === location.origin) a.setAttribute("data-internal", "");
    });
    root.querySelectorAll("iframe").forEach(function (frame) {
      if (frame.closest(".screen-only, .print-only")) return;
      var target = frame.closest(".quarto-video") || frame;
      if (target.nextElementSibling && target.nextElementSibling.classList.contains("print-fallback")) return;
      var src = frame.src || frame.getAttribute("src") || "";
      var isVideo = /youtu|vimeo/.test(src);
      var box = document.createElement("div");
      box.className = "print-fallback";
      var label = document.createElement("em");
      label.textContent = (isVideo ? "Video" : "Interactief element") +
        (frame.title && !/^(simulat|video)/i.test(frame.title) ? " (" + frame.title + ")" : "") +
        " — bekijk het op de website:";
      var url = document.createElement("div");
      url.className = "print-fallback-url";
      url.textContent = isVideo ? youtubeLink(src) : src;
      box.appendChild(label);
      box.appendChild(url);
      target.insertAdjacentElement("afterend", box);
    });

    root.querySelectorAll(".panel-tabset").forEach(function (set) {
      var tabs = set.querySelectorAll(".nav-tabs .nav-link");
      var panes = set.querySelectorAll(".tab-content > .tab-pane");
      panes.forEach(function (pane, i) {
        if (!tabs[i] || pane.querySelector(":scope > .print-tab-title")) return;
        var t = document.createElement("div");
        t.className = "print-tab-title";
        t.innerHTML = tabs[i].innerHTML;  // innerHTML, so a formula in a tab name is typeset too
        pane.insertBefore(t, pane.firstChild);
      });
    });
  }

  // -- dark mode off for printing --------------------------------------------
  var wasDark = false;
  window.addEventListener("beforeprint", function () {
    wasDark = document.body.classList.contains("quarto-dark");
    if (wasDark && window.quartoToggleColorScheme) window.quartoToggleColorScheme();
  });
  window.addEventListener("afterprint", function () {
    if (wasDark && window.quartoToggleColorScheme) window.quartoToggleColorScheme();
    wasDark = false;
    if (new URLSearchParams(location.search).get("layout") !== "2col") {
      document.documentElement.classList.remove("print-1col", "print-2col");
    }
  });

  // -- the print page ----------------------------------------------------------
  function absolutise(el, base) {
    ["src", "href", "poster", "data-src"].forEach(function (attr) {
      el.querySelectorAll("[" + attr + "]").forEach(function (node) {
        var v = node.getAttribute(attr);
        if (!v || v.charAt(0) === "#" || /^(data|mailto|javascript):/.test(v)) return;
        var abs = new URL(v, base);
        node.setAttribute(attr, abs.href);
        if (attr === "href" && abs.origin === location.origin) node.setAttribute("data-internal", "");
      });
    });
    el.querySelectorAll("[srcset]").forEach(function (node) {
      node.setAttribute("srcset", node.getAttribute("srcset").split(",").map(function (part) {
        var bits = part.trim().split(/\s+/);
        bits[0] = new URL(bits[0], base).href;
        return bits.join(" ");
      }).join(", "));
    });
  }

  function sidebarChapters() {
    var seen = {};
    var out = [];
    document.querySelectorAll("#quarto-sidebar a.sidebar-link[href]").forEach(function (a) {
      if (a.hasAttribute("data-bs-toggle")) return;
      var url = new URL(a.getAttribute("href"), location.href);
      url.hash = "";
      if (!/\.html$/.test(url.pathname) && !/\/$/.test(url.pathname)) return;
      var file = url.pathname.split("/").pop() || "index.html";
      if (NEVER.indexOf(file) !== -1 || seen[url.href]) return;
      seen[url.href] = true;
      var section = a.closest(".sidebar-item-section");
      var partEl = section && section.querySelector(":scope > .sidebar-item-container .menu-text");
      var ch = { url: url.href, file: file, part: partEl ? partEl.textContent.trim() : "" };
      ch.group = groupOf(ch);
      out.push(ch);
    });
    return out;
  }

  function loadChapter(ch) {
    return fetch(ch.url).then(function (r) {
      if (!r.ok) throw new Error(r.status + " " + ch.url);
      return r.text();
    }).then(function (html) {
      var doc = new DOMParser().parseFromString(html, "text/html");
      var main = doc.querySelector("main#quarto-document-content") || doc.querySelector("main");
      main.querySelectorAll("script, nav.page-navigation, .quarto-title-meta").forEach(function (n) { n.remove(); });
      // The live versions are never shown on this page; leaving them in would
      // load every simulation and video in the book in the background.
      main.querySelectorAll(".screen-only").forEach(function (n) { n.remove(); });
      absolutise(main, ch.url);
      // Stand-ins first, then drop the embeds, all before the chapter joins
      // this page (an iframe starts loading the moment it is inserted).
      prepareForPaper(main);
      main.querySelectorAll("iframe, video").forEach(function (n) { (n.closest(".quarto-video") || n).remove(); });
      var header = main.querySelector("#title-block-header");
      if (header) { header.removeAttribute("id"); header.classList.add("print-chapter-header"); }
      var notes = main.querySelector(".footnotes-end-of-document");
      if (notes) notes.setAttribute("data-for", chapterLabel(main));
      var wrap = document.createElement("section");
      wrap.className = "print-chapter";
      Array.prototype.slice.call(main.childNodes).forEach(function (n) {
        wrap.appendChild(document.importNode(n, true));
      });
      return wrap;
    });
  }

  function imagesLoaded(root) {
    return Promise.all(Array.prototype.map.call(root.querySelectorAll("img"), function (img) {
      if (img.complete) return null;
      return new Promise(function (res) { img.onload = img.onerror = res; });
    }));
  }

  // MathJax 4's typesetPromise() can stay pending long after the formulas are
  // drawn (it also waits for its accessibility work), so instead of waiting
  // for it, watch for the moment no formula is left untypeset. Give up after
  // two minutes rather than keep the buttons disabled forever.
  function mathTypeset(book) {
    if (!window.MathJax || !window.MathJax.startup) return Promise.resolve();
    window.MathJax.startup.promise.then(function () { window.MathJax.typesetPromise([book]); });
    var started = Date.now();
    return new Promise(function (resolve) {
      (function check() {
        var left = book.querySelectorAll("span.math:not(:has(mjx-container))").length;
        if (left === 0 || Date.now() - started > 120000) resolve();
        else setTimeout(check, 400);
      })();
    });
  }

  // -- choosing sections ------------------------------------------------------
  var ready = "";

  function buildOptions(book) {
    var box = document.getElementById("print-options");
    if (!box) return;
    var params = new URLSearchParams(location.search);
    var preset = params.get("parts");  // print.html?parts=info,modellen,basis
    GROUPS.forEach(function (g) {
      var label = document.createElement("label");
      var input = document.createElement("input");
      input.type = "checkbox";
      input.value = g.key;
      input.checked = preset ? preset.split(",").indexOf(g.key) !== -1 : g.on;
      input.addEventListener("change", function () { applySelection(book); });
      label.appendChild(input);
      label.appendChild(document.createTextNode(" " + g.label));
      box.appendChild(label);
    });
  }

  // Hide the chapters of unticked groups, re-mark the first chapter that is
  // left (no page break before it), and report what is selected.
  function applySelection(book) {
    var on = {};
    document.querySelectorAll("#print-options input").forEach(function (i) { on[i.value] = i.checked; });
    var first = true;
    var count = 0;
    book.querySelectorAll(":scope > .print-chapter").forEach(function (el) {
      var g = el.getAttribute("data-group");
      var show = !g || on[g] !== false;
      el.classList.toggle("print-excluded", !show);
      el.classList.toggle("print-first", show && first);
      if (show) { first = false; count++; }
    });
    var status = document.getElementById("print-status");
    if (status && ready) status.textContent = ready + " (" + count + " hoofdstukken geselecteerd)";
  }

  function buildPrintBook(book) {
    document.body.classList.add("print-page");
    buildOptions(book);
    var status = document.getElementById("print-status");
    var buttons = document.querySelectorAll(".print-choices button");
    var chapters = sidebarChapters();
    var done = 0;
    function say(t) { if (status) status.textContent = t; }
    say("Boek wordt samengesteld… 0/" + chapters.length);

    Promise.all(chapters.map(function (ch) {
      return loadChapter(ch).then(function (el) {
        done++;
        say("Boek wordt samengesteld… " + done + "/" + chapters.length);
        return el;
      }, function (err) {
        console.warn("Print book: could not load", err);
        return null;
      });
    })).then(function (sections) {
      var lastPart = null;
      sections.forEach(function (el, i) {
        if (!el) return;
        var part = chapters[i].part;
        if (part && part !== lastPart) {
          var p = document.createElement("div");
          p.className = "print-part";
          p.textContent = part;
          el.insertBefore(p, el.firstChild);
        }
        lastPart = part;
        el.setAttribute("data-group", chapters[i].group);
        book.appendChild(el);
      });
      say("Formules worden gezet…");
      return mathTypeset(book).then(function () { return imagesLoaded(book); });
    }).then(function () {
      var missing = chapters.length - book.querySelectorAll(".print-chapter").length;
      ready = missing ? "Klaar, maar " + missing + " hoofdstuk(ken) konden niet geladen worden." : "Klaar om te printen.";
      say(ready);
      applySelection(book);
      buttons.forEach(function (b) { b.disabled = false; });
    });
  }

  window.printBook = function (mode) {
    var root = document.documentElement;
    root.classList.remove("print-1col", "print-2col");
    root.classList.add(mode);
    window.print();
  };

  function init() {
    var main = document.querySelector("main");
    if (main) {
      prepareForPaper(main);
      var notes = main.querySelector(".footnotes-end-of-document");
      if (notes && !document.getElementById("print-book")) notes.setAttribute("data-for", chapterLabel(main));
    }
    var book = document.getElementById("print-book");
    if (book) {
      // print.html?layout=2col preselects the two-column layout (handy for a
      // direct link, or for printing to PDF from the command line).
      if (new URLSearchParams(location.search).get("layout") === "2col") {
        document.documentElement.classList.add("print-2col");
      }
      buildPrintBook(book);
    }
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", init);
  else init();
})();
