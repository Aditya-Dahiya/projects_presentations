<<<<<<< HEAD:docs/data_vizs/ind_airports_voronoi.html
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en"><head>

<meta charset="utf-8">
<meta name="generator" content="quarto-1.4.549">

<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes">

<meta name="author" content="Aditya Dahiya">
<meta name="dcterms.date" content="2024-05-27">

<title>Presentations, Projects &amp; Data Viz - Navigating India: Closest International Airports</title>
<style>
code{white-space: pre-wrap;}
span.smallcaps{font-variant: small-caps;}
div.columns{display: flex; gap: min(4vw, 1.5em);}
div.column{flex: auto; overflow-x: auto;}
div.hanging-indent{margin-left: 1.5em; text-indent: -1.5em;}
ul.task-list{list-style: none;}
ul.task-list li input[type="checkbox"] {
  width: 0.8em;
  margin: 0 0.8em 0.2em -1em; /* quarto-specific, see https://github.com/quarto-dev/quarto-cli/issues/4556 */ 
  vertical-align: middle;
}
/* CSS for syntax highlighting */
pre > code.sourceCode { white-space: pre; position: relative; }
pre > code.sourceCode > span { line-height: 1.25; }
pre > code.sourceCode > span:empty { height: 1.2em; }
.sourceCode { overflow: visible; }
code.sourceCode > span { color: inherit; text-decoration: inherit; }
div.sourceCode { margin: 1em 0; }
pre.sourceCode { margin: 0; }
@media screen {
div.sourceCode { overflow: auto; }
}
@media print {
pre > code.sourceCode { white-space: pre-wrap; }
pre > code.sourceCode > span { text-indent: -5em; padding-left: 5em; }
}
pre.numberSource code
  { counter-reset: source-line 0; }
pre.numberSource code > span
  { position: relative; left: -4em; counter-increment: source-line; }
pre.numberSource code > span > a:first-child::before
  { content: counter(source-line);
    position: relative; left: -1em; text-align: right; vertical-align: baseline;
    border: none; display: inline-block;
    -webkit-touch-callout: none; -webkit-user-select: none;
    -khtml-user-select: none; -moz-user-select: none;
    -ms-user-select: none; user-select: none;
    padding: 0 4px; width: 4em;
  }
pre.numberSource { margin-left: 3em;  padding-left: 4px; }
div.sourceCode
  {   }
@media screen {
pre > code.sourceCode > span > a:first-child::before { text-decoration: underline; }
}
</style>


<script src="../site_libs/quarto-nav/quarto-nav.js"></script>
<script src="../site_libs/quarto-nav/headroom.min.js"></script>
<script src="../site_libs/clipboard/clipboard.min.js"></script>
<script src="../site_libs/quarto-search/autocomplete.umd.js"></script>
<script src="../site_libs/quarto-search/fuse.min.js"></script>
<script src="../site_libs/quarto-search/quarto-search.js"></script>
<meta name="quarto:offset" content="../">
<script src="../site_libs/quarto-html/quarto.js"></script>
<script src="../site_libs/quarto-html/popper.min.js"></script>
<script src="../site_libs/quarto-html/tippy.umd.min.js"></script>
<script src="../site_libs/quarto-html/anchor.min.js"></script>
<link href="../site_libs/quarto-html/tippy.css" rel="stylesheet">
<link href="../site_libs/quarto-html/quarto-syntax-highlighting.css" rel="stylesheet" id="quarto-text-highlighting-styles">
<script src="../site_libs/bootstrap/bootstrap.min.js"></script>
<link href="../site_libs/bootstrap/bootstrap-icons.css" rel="stylesheet">
<link href="../site_libs/bootstrap/bootstrap.min.css" rel="stylesheet" id="quarto-bootstrap" data-mode="light">
<link href="../site_libs/quarto-contrib/social-share-0.1.0/social-share.css" rel="stylesheet">
<link href="../site_libs/quarto-contrib/social-share-0.1.0/all.css" rel="stylesheet">
<script id="quarto-search-options" type="application/json">{
  "location": "navbar",
  "copy-button": false,
  "collapse-after": 3,
  "panel-placement": "end",
  "type": "overlay",
  "limit": 50,
  "keyboard-shortcut": [
    "f",
    "/",
    "s"
  ],
  "show-item-context": false,
  "language": {
    "search-no-results-text": "No results",
    "search-matching-documents-text": "matching documents",
    "search-copy-link-title": "Copy link to search",
    "search-hide-matches-text": "Hide additional matches",
    "search-more-match-text": "more match in this document",
    "search-more-matches-text": "more matches in this document",
    "search-clear-button-title": "Clear",
    "search-text-placeholder": "",
    "search-detached-cancel-button-title": "Cancel",
    "search-submit-button-title": "Submit",
    "search-label": "Search"
  }
}</script>
<link href="../site_libs/htmltools-fill-0.5.8.1/fill.css" rel="stylesheet">
<script src="../site_libs/htmlwidgets-1.6.4/htmlwidgets.js"></script>
<script src="../site_libs/d3-bundle-5.16.0/d3-bundle.min.js"></script>
<script src="../site_libs/d3-lasso-0.0.5/d3-lasso.min.js"></script>
<script src="../site_libs/save-svg-as-png-1.4.17/save-svg-as-png.min.js"></script>
<script src="../site_libs/flatbush-4.4.0/flatbush.min.js"></script>
<link href="../site_libs/ggiraphjs-0.8.10/ggiraphjs.min.css" rel="stylesheet">
<script src="../site_libs/ggiraphjs-0.8.10/ggiraphjs.min.js"></script>
<script src="../site_libs/girafe-binding-0.8.10/girafe.js"></script>


<link rel="stylesheet" href="../styles.css">
</head>

<body class="nav-fixed">

<div id="quarto-search-results"></div>
  <header id="quarto-header" class="headroom fixed-top">
    <nav class="navbar navbar-expand-lg " data-bs-theme="dark">
      <div class="navbar-container container-fluid">
      <div class="navbar-brand-container mx-auto">
    <a href="../index.html" class="navbar-brand navbar-brand-logo">
    <img src="../docs/aditya_dahiya_er_sheet.jpg" alt="Presentations, Data Viz &amp; Mor" class="navbar-logo">
    </a>
    <a class="navbar-brand" href="../index.html">
    <span class="navbar-title">Presentations, Projects &amp; Data Viz</span>
    </a>
  </div>
            <div id="quarto-search" class="" title="Search"></div>
          <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarCollapse" aria-controls="navbarCollapse" aria-expanded="false" aria-label="Toggle navigation" onclick="if (window.quartoToggleHeadroom) { window.quartoToggleHeadroom(); }">
  <span class="navbar-toggler-icon"></span>
</button>
          <div class="collapse navbar-collapse" id="navbarCollapse">
            <ul class="navbar-nav navbar-nav-scroll me-auto">
  <li class="nav-item">
    <a class="nav-link" href="../presentations.html"> 
<span class="menu-text">Presentations</span></a>
  </li>  
  <li class="nav-item">
    <a class="nav-link" href="../projects.html"> 
<span class="menu-text">Projects</span></a>
  </li>  
  <li class="nav-item">
    <a class="nav-link" href="../data_vizs.html"> 
<span class="menu-text">Data Visualizations</span></a>
  </li>  
  <li class="nav-item">
    <a class="nav-link" href="../about.html"> 
<span class="menu-text">About Me</span></a>
  </li>  
</ul>
          </div> <!-- /navcollapse -->
          <div class="quarto-navbar-tools tools-wide">
    <a href="../index.xml" title="" class="quarto-navigation-tool px-1" aria-label=""><i class="bi bi-rss"></i></a>
    <div class="dropdown">
      <a href="" title="" id="quarto-navigation-tool-dropdown-0" class="quarto-navigation-tool dropdown-toggle px-1" data-bs-toggle="dropdown" aria-expanded="false" aria-label=""><i class="bi bi-github"></i></a>
      <ul class="dropdown-menu dropdown-menu-end" aria-labelledby="quarto-navigation-tool-dropdown-0">
          <li>
            <a class="dropdown-item quarto-navbar-tools-item" href="https://github.com/Aditya-Dahiya/sessions_presentations">
            Source Code
            </a>
          </li>
          <li>
            <a class="dropdown-item quarto-navbar-tools-item" href="https://github.com/Aditya-Dahiya/sessions_presentations/issues">
            Suggestions / Feedback
            </a>
          </li>
      </ul>
    </div>
  <a href="" class="quarto-reader-toggle quarto-navigation-tool px-1" onclick="window.quartoToggleReader(); return false;" title="Toggle reader mode">
  <div class="quarto-reader-toggle-btn">
  <i class="bi"></i>
  </div>
</a>
</div>
      </div> <!-- /container-fluid -->
    </nav>
</header>
<!-- content -->
<div id="quarto-content" class="quarto-container page-columns page-rows-contents page-layout-article page-navbar">
<!-- sidebar -->
<!-- margin-sidebar -->
    <div id="quarto-margin-sidebar" class="sidebar margin-sidebar">
        <nav id="TOC" role="doc-toc" class="toc-active">
    <h2 id="toc-title">On this page</h2>
   
  <ul>
  <li><a href="#navigating-india-closest-international-airports" id="toc-navigating-india-closest-international-airports" class="nav-link active" data-scroll-target="#navigating-india-closest-international-airports">Navigating India: Closest International Airports</a>
  <ul class="collapse">
  <li><a href="#an-interactive-version" id="toc-an-interactive-version" class="nav-link" data-scroll-target="#an-interactive-version">An interactive version</a></li>
  <li><a href="#how-i-made-this-graphic" id="toc-how-i-made-this-graphic" class="nav-link" data-scroll-target="#how-i-made-this-graphic">How I made this graphic?</a></li>
  </ul></li>
  </ul>
<div class="toc-actions"><ul><li><a href="https://github.com/Aditya-Dahiya/sessions_presentations/edit/main/data_vizs/ind_airports_voronoi.qmd" class="toc-action"><i class="bi bi-github"></i>Edit this page</a></li><li><a href="https://github.com/Aditya-Dahiya/sessions_presentations/issues/new" class="toc-action"><i class="bi empty"></i>Report an issue</a></li></ul></div></nav>
    </div>
<!-- main -->
<main class="content" id="quarto-document-content">

<header id="title-block-header" class="quarto-title-block default">
<div class="quarto-title">
<h1 class="title">Navigating India: Closest International Airports</h1>
<p class="subtitle lead">A Voronoi Tesselation to produce regions nearest to each international airport in India</p>
  <div class="quarto-categories">
    <div class="quarto-category">A4 Size Viz</div>
    <div class="quarto-category">India</div>
    <div class="quarto-category">Maps</div>
    <div class="quarto-category">Interactive</div>
  </div>
  </div>



<div class="quarto-title-meta">

    <div>
    <div class="quarto-title-meta-heading">Author</div>
    <div class="quarto-title-meta-contents">
             <p>Aditya Dahiya </p>
          </div>
  </div>
    
    <div>
    <div class="quarto-title-meta-heading">Published</div>
    <div class="quarto-title-meta-contents">
      <p class="date">May 27, 2024</p>
    </div>
  </div>
  
    
  </div>
  


</header>


<section id="navigating-india-closest-international-airports" class="level1">
<h1>Navigating India: Closest International Airports</h1>
<p>The map of India illustrates the locations of international airports and the regions nearest to each, highlighted with distinct colors using Voronoi tessellation. The size of the dots represents the annual passenger traffic for each airport in 2023, with larger dots indicating busier airports. The geographical boundaries of India are sourced from the Survey of India. Airport locations are obtained from the OpenFlights database, accessed through the {airportr} package in R. Passenger traffic data is retrieved from Wikipedia using the {rvest} package in R. For more details, refer to <a href="https://surveyofindia.gov.in">Survey of India</a>, <a href="https://openflights.org/data.html">OpenFlights</a>, and <a href="https://en.wikipedia.org">Wikipedia</a>.</p>
<div class="quarto-figure quarto-figure-center">
<figure class="figure">
<p><img src="a4_ind_airports_voronoi.png" class="img-fluid figure-img"></p>
<figcaption>This map of India shows the locations of international airports and the areas closest to each one, highlighted in different colors. The size of each airport’s dot represents how many passengers used it in 2023, with larger dots indicating busier airports.</figcaption>
</figure>
</div>
<section id="an-interactive-version" class="level3">
<h3 class="anchored" data-anchor-id="an-interactive-version">An interactive version</h3>
<div class="cell">
<div id="fig-interact-airports" class="cell-output-display quarto-figure quarto-figure-center quarto-float anchored">
<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-interact-airports-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
=======
---
title: "Navigating India: Closest International Airports"
author: "Aditya Dahiya"
date: "2024-05-27"
subtitle: "A Voronoi Tesselation to produce regions nearest to each international airport in India"
categories:
  - "A4 Size Viz"
  - "India"
  - "Maps"
  - "Interactive"
image: "thumbnails/ind_airports_voronoi.png"
format:
  html:
    code-fold: true
editor_options: 
  chunk_output_type: console
execute: 
  error: false
  message: false
  warning: false
  eval: false
  cache: true
filters:
  - social-share
share:
  permalink: "https://aditya-dahiya.github.io/session_presentations/data_vizs.html"
  twitter: true
  linkedin: true
  email: true
  mastodon: true
bibliography: references.bib
---


# Navigating India: Closest International Airports

The map of India illustrates the locations of international airports and the regions nearest to each, highlighted with distinct colors using Voronoi tessellation. The size of the dots represents the annual passenger traffic for each airport in 2023, with larger dots indicating busier airports. The geographical boundaries of India are sourced from the Survey of India. Airport locations are obtained from the OpenFlights database, accessed through the {airportr} package in R. Passenger traffic data is retrieved from Wikipedia using the {rvest} package in R. For more details, refer to [Survey of India](https://surveyofindia.gov.in), [OpenFlights](https://openflights.org/data.html), and [Wikipedia](https://en.wikipedia.org).

![This map of India shows the locations of international airports and the areas closest to each one, highlighted in different colors. The size of each airport's dot represents how many passengers used it in 2023, with larger dots indicating busier airports.](a4_ind_airports_voronoi.png)

### An interactive version


::: {.cell}
::: {#fig-interact-airports .cell-output-display}

```{=html}
>>>>>>> parent of bcb2dda (update website to remove all interactive versions):data_vizs/ind_airports_voronoi.knit.md
<div class="girafe html-widget html-fill-item" id="htmlwidget-1db363d34ad0c8903d0c" style="width:100%;height:975px;"></div>
```


<<<<<<< HEAD:docs/data_vizs/ind_airports_voronoi.html
</section>
</section>

</main> <!-- /main -->
<script id="quarto-html-after-body" type="application/javascript">
window.document.addEventListener("DOMContentLoaded", function (event) {
  const toggleBodyColorMode = (bsSheetEl) => {
    const mode = bsSheetEl.getAttribute("data-mode");
    const bodyEl = window.document.querySelector("body");
    if (mode === "dark") {
      bodyEl.classList.add("quarto-dark");
      bodyEl.classList.remove("quarto-light");
    } else {
      bodyEl.classList.add("quarto-light");
      bodyEl.classList.remove("quarto-dark");
    }
  }
  const toggleBodyColorPrimary = () => {
    const bsSheetEl = window.document.querySelector("link#quarto-bootstrap");
    if (bsSheetEl) {
      toggleBodyColorMode(bsSheetEl);
    }
  }
  toggleBodyColorPrimary();  
  const icon = "";
  const anchorJS = new window.AnchorJS();
  anchorJS.options = {
    placement: 'right',
    icon: icon
  };
  anchorJS.add('.anchored');
  const isCodeAnnotation = (el) => {
    for (const clz of el.classList) {
      if (clz.startsWith('code-annotation-')) {                     
        return true;
      }
    }
    return false;
  }
  const clipboard = new window.ClipboardJS('.code-copy-button', {
    text: function(trigger) {
      const codeEl = trigger.previousElementSibling.cloneNode(true);
      for (const childEl of codeEl.children) {
        if (isCodeAnnotation(childEl)) {
          childEl.remove();
        }
      }
      return codeEl.innerText;
    }
  });
  clipboard.on('success', function(e) {
    // button target
    const button = e.trigger;
    // don't keep focus
    button.blur();
    // flash "checked"
    button.classList.add('code-copy-button-checked');
    var currentTitle = button.getAttribute("title");
    button.setAttribute("title", "Copied!");
    let tooltip;
    if (window.bootstrap) {
      button.setAttribute("data-bs-toggle", "tooltip");
      button.setAttribute("data-bs-placement", "left");
      button.setAttribute("data-bs-title", "Copied!");
      tooltip = new bootstrap.Tooltip(button, 
        { trigger: "manual", 
          customClass: "code-copy-button-tooltip",
          offset: [0, -8]});
      tooltip.show();    
    }
    setTimeout(function() {
      if (tooltip) {
        tooltip.hide();
        button.removeAttribute("data-bs-title");
        button.removeAttribute("data-bs-toggle");
        button.removeAttribute("data-bs-placement");
      }
      button.setAttribute("title", currentTitle);
      button.classList.remove('code-copy-button-checked');
    }, 1000);
    // clear code selection
    e.clearSelection();
  });
  function tippyHover(el, contentFn, onTriggerFn, onUntriggerFn) {
    const config = {
      allowHTML: true,
      maxWidth: 500,
      delay: 100,
      arrow: false,
      appendTo: function(el) {
          return el.parentElement;
      },
      interactive: true,
      interactiveBorder: 10,
      theme: 'quarto',
      placement: 'bottom-start',
    };
    if (contentFn) {
      config.content = contentFn;
    }
    if (onTriggerFn) {
      config.onTrigger = onTriggerFn;
    }
    if (onUntriggerFn) {
      config.onUntrigger = onUntriggerFn;
    }
    window.tippy(el, config); 
  }
  const noterefs = window.document.querySelectorAll('a[role="doc-noteref"]');
  for (var i=0; i<noterefs.length; i++) {
    const ref = noterefs[i];
    tippyHover(ref, function() {
      // use id or data attribute instead here
      let href = ref.getAttribute('data-footnote-href') || ref.getAttribute('href');
      try { href = new URL(href).hash; } catch {}
      const id = href.replace(/^#\/?/, "");
      const note = window.document.getElementById(id);
      return note.innerHTML;
    });
  }
  const xrefs = window.document.querySelectorAll('a.quarto-xref');
  const processXRef = (id, note) => {
    // Strip column container classes
    const stripColumnClz = (el) => {
      el.classList.remove("page-full", "page-columns");
      if (el.children) {
        for (const child of el.children) {
          stripColumnClz(child);
        }
      }
    }
    stripColumnClz(note)
    if (id === null || id.startsWith('sec-')) {
      // Special case sections, only their first couple elements
      const container = document.createElement("div");
      if (note.children && note.children.length > 2) {
        container.appendChild(note.children[0].cloneNode(true));
        for (let i = 1; i < note.children.length; i++) {
          const child = note.children[i];
          if (child.tagName === "P" && child.innerText === "") {
            continue;
          } else {
            container.appendChild(child.cloneNode(true));
            break;
          }
        }
        if (window.Quarto?.typesetMath) {
          window.Quarto.typesetMath(container);
        }
        return container.innerHTML
      } else {
        if (window.Quarto?.typesetMath) {
          window.Quarto.typesetMath(note);
        }
        return note.innerHTML;
      }
    } else {
      // Remove any anchor links if they are present
      const anchorLink = note.querySelector('a.anchorjs-link');
      if (anchorLink) {
        anchorLink.remove();
      }
      if (window.Quarto?.typesetMath) {
        window.Quarto.typesetMath(note);
      }
      // TODO in 1.5, we should make sure this works without a callout special case
      if (note.classList.contains("callout")) {
        return note.outerHTML;
      } else {
        return note.innerHTML;
      }
    }
  }
  for (var i=0; i<xrefs.length; i++) {
    const xref = xrefs[i];
    tippyHover(xref, undefined, function(instance) {
      instance.disable();
      let url = xref.getAttribute('href');
      let hash = undefined; 
      if (url.startsWith('#')) {
        hash = url;
      } else {
        try { hash = new URL(url).hash; } catch {}
      }
      if (hash) {
        const id = hash.replace(/^#\/?/, "");
        const note = window.document.getElementById(id);
        if (note !== null) {
          try {
            const html = processXRef(id, note.cloneNode(true));
            instance.setContent(html);
          } finally {
            instance.enable();
            instance.show();
          }
        } else {
          // See if we can fetch this
          fetch(url.split('#')[0])
          .then(res => res.text())
          .then(html => {
            const parser = new DOMParser();
            const htmlDoc = parser.parseFromString(html, "text/html");
            const note = htmlDoc.getElementById(id);
            if (note !== null) {
              const html = processXRef(id, note);
              instance.setContent(html);
            } 
          }).finally(() => {
            instance.enable();
            instance.show();
          });
        }
      } else {
        // See if we can fetch a full url (with no hash to target)
        // This is a special case and we should probably do some content thinning / targeting
        fetch(url)
        .then(res => res.text())
        .then(html => {
          const parser = new DOMParser();
          const htmlDoc = parser.parseFromString(html, "text/html");
          const note = htmlDoc.querySelector('main.content');
          if (note !== null) {
            // This should only happen for chapter cross references
            // (since there is no id in the URL)
            // remove the first header
            if (note.children.length > 0 && note.children[0].tagName === "HEADER") {
              note.children[0].remove();
            }
            const html = processXRef(null, note);
            instance.setContent(html);
          } 
        }).finally(() => {
          instance.enable();
          instance.show();
        });
      }
    }, function(instance) {
    });
  }
      let selectedAnnoteEl;
      const selectorForAnnotation = ( cell, annotation) => {
        let cellAttr = 'data-code-cell="' + cell + '"';
        let lineAttr = 'data-code-annotation="' +  annotation + '"';
        const selector = 'span[' + cellAttr + '][' + lineAttr + ']';
        return selector;
      }
      const selectCodeLines = (annoteEl) => {
        const doc = window.document;
        const targetCell = annoteEl.getAttribute("data-target-cell");
        const targetAnnotation = annoteEl.getAttribute("data-target-annotation");
        const annoteSpan = window.document.querySelector(selectorForAnnotation(targetCell, targetAnnotation));
        const lines = annoteSpan.getAttribute("data-code-lines").split(",");
        const lineIds = lines.map((line) => {
          return targetCell + "-" + line;
        })
        let top = null;
        let height = null;
        let parent = null;
        if (lineIds.length > 0) {
            //compute the position of the single el (top and bottom and make a div)
            const el = window.document.getElementById(lineIds[0]);
            top = el.offsetTop;
            height = el.offsetHeight;
            parent = el.parentElement.parentElement;
          if (lineIds.length > 1) {
            const lastEl = window.document.getElementById(lineIds[lineIds.length - 1]);
            const bottom = lastEl.offsetTop + lastEl.offsetHeight;
            height = bottom - top;
          }
          if (top !== null && height !== null && parent !== null) {
            // cook up a div (if necessary) and position it 
            let div = window.document.getElementById("code-annotation-line-highlight");
            if (div === null) {
              div = window.document.createElement("div");
              div.setAttribute("id", "code-annotation-line-highlight");
              div.style.position = 'absolute';
              parent.appendChild(div);
            }
            div.style.top = top - 2 + "px";
            div.style.height = height + 4 + "px";
            div.style.left = 0;
            let gutterDiv = window.document.getElementById("code-annotation-line-highlight-gutter");
            if (gutterDiv === null) {
              gutterDiv = window.document.createElement("div");
              gutterDiv.setAttribute("id", "code-annotation-line-highlight-gutter");
              gutterDiv.style.position = 'absolute';
              const codeCell = window.document.getElementById(targetCell);
              const gutter = codeCell.querySelector('.code-annotation-gutter');
              gutter.appendChild(gutterDiv);
            }
            gutterDiv.style.top = top - 2 + "px";
            gutterDiv.style.height = height + 4 + "px";
          }
          selectedAnnoteEl = annoteEl;
        }
      };
      const unselectCodeLines = () => {
        const elementsIds = ["code-annotation-line-highlight", "code-annotation-line-highlight-gutter"];
        elementsIds.forEach((elId) => {
          const div = window.document.getElementById(elId);
          if (div) {
            div.remove();
          }
        });
        selectedAnnoteEl = undefined;
      };
        // Handle positioning of the toggle
    window.addEventListener(
      "resize",
      throttle(() => {
        elRect = undefined;
        if (selectedAnnoteEl) {
          selectCodeLines(selectedAnnoteEl);
        }
      }, 10)
    );
    function throttle(fn, ms) {
    let throttle = false;
    let timer;
      return (...args) => {
        if(!throttle) { // first call gets through
            fn.apply(this, args);
            throttle = true;
        } else { // all the others get throttled
            if(timer) clearTimeout(timer); // cancel #2
            timer = setTimeout(() => {
              fn.apply(this, args);
              timer = throttle = false;
            }, ms);
        }
      };
    }
      // Attach click handler to the DT
      const annoteDls = window.document.querySelectorAll('dt[data-target-cell]');
      for (const annoteDlNode of annoteDls) {
        annoteDlNode.addEventListener('click', (event) => {
          const clickedEl = event.target;
          if (clickedEl !== selectedAnnoteEl) {
            unselectCodeLines();
            const activeEl = window.document.querySelector('dt[data-target-cell].code-annotation-active');
            if (activeEl) {
              activeEl.classList.remove('code-annotation-active');
            }
            selectCodeLines(clickedEl);
            clickedEl.classList.add('code-annotation-active');
          } else {
            // Unselect the line
            unselectCodeLines();
            clickedEl.classList.remove('code-annotation-active');
          }
        });
      }
  const findCites = (el) => {
    const parentEl = el.parentElement;
    if (parentEl) {
      const cites = parentEl.dataset.cites;
      if (cites) {
        return {
          el,
          cites: cites.split(' ')
        };
      } else {
        return findCites(el.parentElement)
      }
    } else {
      return undefined;
    }
  };
  var bibliorefs = window.document.querySelectorAll('a[role="doc-biblioref"]');
  for (var i=0; i<bibliorefs.length; i++) {
    const ref = bibliorefs[i];
    const citeInfo = findCites(ref);
    if (citeInfo) {
      tippyHover(citeInfo.el, function() {
        var popup = window.document.createElement('div');
        citeInfo.cites.forEach(function(cite) {
          var citeDiv = window.document.createElement('div');
          citeDiv.classList.add('hanging-indent');
          citeDiv.classList.add('csl-entry');
          var biblioDiv = window.document.getElementById('ref-' + cite);
          if (biblioDiv) {
            citeDiv.innerHTML = biblioDiv.innerHTML;
          }
          popup.appendChild(citeDiv);
        });
        return popup.innerHTML;
      });
    }
  }
});
</script>
</div> <!-- /content -->
<footer class="footer"><div class="nav-footer"><div class="nav-footer-center"><div class="toc-actions d-sm-block d-md-none"><ul><li><a href="https://github.com/Aditya-Dahiya/sessions_presentations/edit/main/data_vizs/ind_airports_voronoi.qmd" class="toc-action"><i class="bi bi-github"></i>Edit this page</a></li><li><a href="https://github.com/Aditya-Dahiya/sessions_presentations/issues/new" class="toc-action"><i class="bi empty"></i>Report an issue</a></li></ul></div></div></div></footer><div class="page-columns page-rows-contents page-layout-article"><div class="social-share"><a href="https://twitter.com/share?url=https://aditya-dahiya.github.io/session_presentations/data_vizs.html&amp;text=Navigating India: Closest International Airports" target="_blank" class="twitter"><i class="fab fa-twitter fa-fw fa-lg"></i></a><a href="https://www.linkedin.com/shareArticle?url=https://aditya-dahiya.github.io/session_presentations/data_vizs.html&amp;title=Navigating India: Closest International Airports" target="_blank" class="linkedin"><i class="fa-brands fa-linkedin-in fa-fw fa-lg"></i></a>  <a href="mailto:?subject=Navigating India: Closest International Airports&amp;body=Check out this link:https://aditya-dahiya.github.io/session_presentations/data_vizs.html" target="_blank" class="email"><i class="fa-solid fa-envelope fa-fw fa-lg"></i></a><a href="javascript:void(0);" onclick="var mastodon_instance=prompt('Mastodon Instance / Server Name?'); if(typeof mastodon_instance==='string' &amp;&amp; mastodon_instance.length){this.href='https://'+mastodon_instance+'/share?text=Navigating India: Closest International Airports https://aditya-dahiya.github.io/session_presentations/data_vizs.html'}else{return false;}" target="_blank" class="mastodon"><i class="fa-brands fa-mastodon fa-fw fa-lg"></i></a></div></div>
=======
Sources of Data: The map of India is based on official data from the Survey of India. International airports are plotted using data from the OpenFlights database, accessed via the {airportr} package in R. Voronoi tessellation is applied to depict areas closest to each airport, with the method highlighting regions by nearest airport proximity. Airport dot sizes, representing annual passenger traffic for 2023, are sourced from Wikipedia through the {rvest} package in R.
:::
:::
>>>>>>> parent of bcb2dda (update website to remove all interactive versions):data_vizs/ind_airports_voronoi.knit.md


### How I made this graphic?

Getting the data


::: {.cell}

```{.r .cell-code}
# Data Import and Wrangling Tools
library(tidyverse)            # All things tidy

# Final plot tools
library(scales)               # Nice Scales for ggplot2
library(fontawesome)          # Icons display in ggplot2
library(ggtext)               # Markdown text support for ggplot2
library(showtext)             # Display fonts in ggplot2
# library(colorspace)         # To lighten and darken colours

# Maps related packages
library(sf)                   # Geomteric objects manipulation

# Get country map of India
# Credits: Survey of India; and Anuj Tiwari @
# https://github.com/AnujTiwari/India-State-and-Country-Shapefile-Updated-Jan-2020
india_map <- read_sf(here::here("data", "india_map", 
                                "India_Country_Boundary.shp")) |> 
  # During interations, using lower resolution for quick plotting
  # st_simplify(dTolerance = 1000) |> 
  st_transform(crs = 4326)


# Get State Map of India
india_state_map <- read_sf(here::here(
  "data", "india_map", "India_State_Boundary.shp"
)) |> 
  # During interations, using lower resolution for quick plotting
  # st_simplify(dTolerance = 1000) |> 
  st_transform(crs = 4326)

# Get locations of Airports in India
ind_airports <- airportr::airports |> 
  janitor::clean_names() |> 
  rename(lat = latitude, lon = longitude, airport = name) |> 
  filter(country == "India") |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  # Remove Air Force run airports
  filter(!str_detect(airport, "Air Force")) |> 
  # Only international airports
  filter(str_detect(airport, "International"))

# Check the data: it works!
ggplot() +
  geom_sf(data = india_map) +
  geom_sf(data = ind_airports)

# Get passenger traffic data in International airports of India
# Load necessary libraries
library(rvest)
# Wikipedia URL for India's busiest airports
url <- "https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_India"
# Read the HTML content from the webpage
webpage <- read_html(url)
# Extract the table containing the airport data
airport_table <- webpage |> 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') |> 
  html_table()
# Convert the list to a data frame
airport_df <- as.data.frame(airport_table[[1]])
# Clean the data frame
airport_data <- airport_df |> 
  select(name_of_airport = Name, 
         iata = `IATA Code`, 
         passengers_2023 = `Passengers  2022–23`) |> 
  mutate(
    passengers_2023 = as.numeric(gsub(",", "", passengers_2023))
  )
rm(airport_df, webpage, airport_table, url)
```
:::


Visualization Parameters


::: {.cell}

```{.r .cell-code}
# Font for titles
font_add_google("Merienda",
  family = "title_font"
) 

# Font for the caption
font_add_google("Saira Extra Condensed",
  family = "caption_font"
) 

# Font for plot text
font_add_google("Fjalla One",
  family = "body_font"
) 

showtext_auto()

# Background Colour
bg_col <- "#fffee6"
text_col <- "#400600"
text_hil <- "#630b01"

# Base Text Size
bts <- 80

plot_title <- "India's Airport Proximity Map"

plot_subtitle <- str_wrap("Locations of India's international airports and the areas closest to each one. The size of each airport's dot represents passenger traffic in 2023 - larger dots mean busier airports. The number in brackets are the passengers handled by the airport in the year 2023.", 72)
str_view(plot_subtitle)

# Caption stuff for the plot
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf")
)
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>&#xf09b;</span> <span style='color: {text_col}'>aditya-dahiya  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>&#xe61b;</span> <span style='color: {text_col}'>@adityadahiyaias </span>")
plot_caption <- paste0(
  "**Data:** Survey of India; Open Flights Database  |  ",
  "**Code:** ", social_caption_1, 
  " |  **Graphics:** ", social_caption_2
  )

data_source <- str_wrap("Sources of Data: The map of India is based on official data from the Survey of India. International airports are plotted using data from the OpenFlights database, accessed via the {airportr} package in R. Voronoi tessellation is applied to depict areas closest to each airport, with the method highlighting regions by nearest airport proximity. Airport dot sizes, representing annual passenger traffic for 2023, are sourced from Wikipedia through the {rvest} package in R.", 80)

data_source |> str_view()
```
:::


Data Wrangling


::: {.cell}

```{.r .cell-code}
# Credits: https://stackoverflow.com/questions/76856625/perimeter-of-voronoi-cells

voronoi_lines <- ind_airports |> 
  st_union() |> 
  st_voronoi() |>
  st_collection_extract("POLYGON") |> 
  st_sf(geometry = _) |> 
  st_crop(st_bbox(india_map)) |> 
  st_intersection(india_map) 

# Set seed for reproducability  
set.seed(42)
voronoi_lines <- voronoi_lines |> 
  mutate(fill_var = sample(
  x = letters[1:20],
  size = nrow(voronoi_lines),
  replace = T
  )
)
```
:::


Visualization


::: {.cell}

```{.r .cell-code}
g_base <- ggplot() +
  geom_sf(
    data = india_map,
    colour = text_col,
    fill = "transparent",
    linewidth = 1
  ) +
  geom_sf(data = voronoi_lines,
          mapping = aes(fill = fill_var),
          alpha = 0.5,
          colour = bg_col) +
  geom_sf(
    data = ind_airports |> left_join(airport_data),
    mapping = aes(size = passengers_2023),
    colour = text_col,
    alpha = 0.75
  ) +
  geom_sf(
    data = india_state_map,
    colour = "grey50",
    fill = "transparent"
  ) +
  
  # Using ggrepel to label the airports with geom_sf_text
  # Technique Credits:https://github.com/slowkow/ggrepel/issues/111
  ggrepel::geom_text_repel(
    data = ind_airports |> left_join(airport_data),
    mapping = aes(
      geometry = geometry,
      label = paste0(
        airport, 
        "\n", city, " (",
        number(passengers_2023, big.mark = ","), 
        ")"
      )
    ),
    stat = "sf_coordinates",
    min.segment.length = 0,
    family = "caption_font",
    size = bts / 5,
    lineheight = 0.3,
    force = 10,
    force_pull = 0.5
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  paletteer::scale_fill_paletteer_d("palettesForR::Windows") +
  
  # Scales and Coordinates
  coord_sf(clip = "off") +
  scale_size(range = c(2, 10)) +
  
  # Themes
  ggthemes::theme_map(
    base_size = bts,
    base_family = "body_font"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 2.8 * bts,
      colour = text_hil,
      margin = margin(10,0,10,0, "mm"),
      family = "title_font"
    ),
    plot.subtitle = element_text(
      colour = text_hil,
      margin = margin(0,0,0,0, "mm"),
      lineheight = 0.35,
      size = 1.2 * bts
    ),
    plot.caption = element_textbox(
      colour = text_hil,
      family = "caption_font",
      hjust = 0.5
    )
  ) 
```
:::


Add annotations and insets


::: {.cell}

```{.r .cell-code}
# QR Code for the plot
url_graphics <- paste0(
  "https://aditya-dahiya.github.io/projects_presentations/projects/",
  # The file name of the current .qmd file
  "ind_airports_voronoi",
  ".qmd"
)
# remotes::install_github('coolbutuseless/ggqr')
# library(ggqr)
plot_qr <- ggplot(
  data = NULL, 
  aes(x = 0, y = 0, label = url_graphics)
  ) + 
  ggqr::geom_qr(
    colour = text_hil, 
    fill = bg_col,
    size = 2
    ) +
  # labs(caption = "Scan for the Interactive Version") +
  coord_fixed() +
  theme_void() +
  labs(caption = "Interactive Version") +
  theme(plot.background = element_rect(
    fill = NA, 
    colour = NA
    ),
    plot.caption = element_text(
      hjust = 0.5,
      margin = margin(0,0,0,0, "mm"),
      family = "caption_font",
      size = bts/1.5,
      colour = text_hil
    )
  )

inset1 <- ggplot() +
  annotate(
    geom = "text",
    x = 0, y = 0,
    label = data_source,
    lineheight = 0.3,
    family = "caption_font",
    hjust = 1,
    size = bts / 4,
    colour = text_col
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    panel.background = element_rect(
      fill = "transparent",
      colour = "transparent"
    )
  )

library(patchwork)
g <- g_base +
  inset_element(
    p = plot_qr,
    left = 0.82, right = 0.96,
    top = 0.91, bottom = 0.78,
    align_to = "full"
  ) +
  inset_element(
    p = inset1,
    left = 0.55, right = 1.25,
    top = 0.8, bottom = 0.55,
    align_to = "full"
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = "transparent",
        colour = "transparent"
      ),
      panel.background = element_rect(
        fill = "transparent",
        colour = "transparent"
      )
    )
  )

ggsave(
  filename = here::here("data_vizs", "a4_ind_airports_voronoi.png"),
  plot = g,
  height = 297 * 2,
  width = 210 * 2,
  units = "mm",
  bg = bg_col
)
```
:::


Save graphic and a thumbnail


::: {.cell}

```{.r .cell-code}
ggsave(
  filename = here::here("data_vizs", "a4_ind_airports_voronoi.png"),
  plot = g,
  height = 297 * 2,
  width = 210 * 2,
  units = "mm",
  bg = bg_col
)


library(magick)
# Saving a thumbnail for the webpage
image_read(here::here("data_vizs", 
                      "a4_ind_airports_voronoi.png")) |> 
  image_resize(geometry = "400") |> 
  image_write(here::here("data_vizs", "thumbnails", 
                         "ind_airports_voronoi.png"))
```
:::
