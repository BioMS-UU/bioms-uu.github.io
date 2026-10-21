/* Ruimtelijke-groei-explainer (Biologische Modellen en Statistiek, hoofdstuk 13).
 * Gebruik in Quarto (één regel, op de plek waar de app moet komen):
 *   <script src="Applets/ruimtelijk_explainer.js"></script>                        -> hele app met tabbladen
 *   <script src="Applets/ruimtelijk_explainer.js" data-tab="evenwicht"></script>   -> alleen dat onderdeel
 *   <script src="Applets/ruimtelijk_explainer.js" data-tab="evenwicht" data-nav="1"></script> -> tabbladen, start op dat tabblad
 * data-tab: overzicht | netwerk | hokjes | quiz
 * data-theme="dark|light" forceert een thema; anders volgt de app het boek (ook de donkere modus).
 * (Alternatief: <div data-ruim-app data-tab="..."></div> plus het script één keer op de pagina.)
 */
(function(){
"use strict";
const CS=document.currentScript;
function inlineHost(cs){ if(!cs||!cs.dataset||cs.__appDone||!cs.parentNode) return null;
  const single=!!cs.dataset.tab;
  // a plain <script> with no data-tab becomes the full app with tabs, unless the page already uses <div data-ruim-app>
  if(!single&&document.querySelector('[data-ruim-app]')) return null;
  cs.__appDone=true;
  const d=document.createElement('div'); d.setAttribute('data-ruim-app','');
  if(cs.dataset.tab) d.dataset.tab=cs.dataset.tab;
  if(single&&cs.dataset.nav!=='1') d.dataset.hidenav='1'; if(cs.dataset.theme) d.dataset.theme=cs.dataset.theme;
  cs.parentNode.insertBefore(d,cs); return d; }
if(window.RuimApp){ const h=inlineHost(CS); if(h) window.RuimApp.mount(h); window.RuimApp.init(); return; }
const STYLE="\n\n:host{\n  --bg: var(--bs-body-bg, #ffffff);\n  --fg: var(--bs-body-color, #1d2327);\n  --line: var(--bs-border-color, #d9e0dc);\n  --card: color-mix(in srgb, var(--fg) 4%, var(--bg));\n  --muted: color-mix(in srgb, var(--fg) 62%, var(--bg));\n  --grid: color-mix(in srgb, var(--fg) 9%, var(--bg));\n  --axis: color-mix(in srgb, var(--fg) 50%, var(--bg));\n  --accent:#D55E00; --accent-soft:#D55E0022; --accent2:#0072B2; --blue:#0072B2; --warn:#555b61;\n  --font: var(--bs-body-font-family, system-ui,-apple-system,\"Segoe UI\",Roboto,\"Helvetica Neue\",sans-serif);\n  --mono: var(--bs-font-monospace, ui-monospace,SFMono-Regular,Menlo,Consolas,monospace);\n  display:block; color:var(--fg); font-family:var(--font); font-size:.95em; line-height:1.5; margin:1rem 0;\n}\n:host([data-dark]){\n  --bg: var(--bs-body-bg, #16191b);\n  --fg: var(--bs-body-color, #e6e9e7);\n  --line: var(--bs-border-color, #30383c);\n  --accent:#F08A3E; --accent-soft:#F08A3E2B; --accent2:#56B4E9; --blue:#56B4E9; --warn:#c3cad0; --muted: color-mix(in srgb, var(--fg) 82%, var(--bg)); --axis: color-mix(in srgb, var(--fg) 62%, var(--bg));\n}\n.app{color:var(--fg)}\n.ovb{display:inline-block;border-top:1.5px solid currentColor;line-height:1.05;padding-top:1px}\n*{box-sizing:border-box}\nh2{font-size:1.15em;margin:.2em 0 .4em;border:0;padding:0}\nh3{font-size:1em;margin:1em 0 .3em}\np{margin:.35rem 0 .6rem}\nb{font-weight:600}\n.muted{color:var(--muted)}\n.mono{font-family:var(--mono)}\n.eqn,.formula{font-family:\"Latin Modern Math\",\"STIX Two Math\",\"Cambria Math\",\"Times New Roman\",Georgia,serif;font-size:1.06em;white-space:nowrap}\n.mi{font-style:italic}\n.mu{font-style:normal}\n/* tabs */\nnav.tabs{display:flex;gap:4px;flex-wrap:wrap;border-bottom:1px solid var(--line);margin-bottom:12px}\n.tab{appearance:none;border:0;background:none;color:var(--muted);font:inherit;font-size:.92rem;padding:7px 11px;border-radius:8px 8px 0 0;cursor:pointer;border-bottom:2px solid transparent;margin-bottom:-1px}\n.tab:hover{color:var(--fg)}\n.tab[aria-selected=\"true\"]{color:var(--accent);border-bottom-color:var(--accent);font-weight:600}\n/* layout */\n.grid2{display:grid;grid-template-columns:1fr 1fr;gap:14px}\n@media (max-width:680px){.grid2{grid-template-columns:1fr}}\n.card{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:10px 12px}\ncanvas{display:block;width:100%;border-radius:8px}\n.sq{display:block;width:100%;max-width:460px;aspect-ratio:1/1;margin:0 auto;image-rendering:pixelated}\n.plot-half{aspect-ratio:16/7}\n.plot{aspect-ratio:16/10}\n.plot-wide{aspect-ratio:16/8}\n@media (max-width:680px){.plot-wide{aspect-ratio:16/10}}\n.controls{position:relative;display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:6px 16px;margin:8px 0}\n.ctl label{display:flex;justify-content:space-between;font-size:.88rem;color:var(--muted)}\n.ctl label span{color:var(--fg);font-family:var(--mono);font-size:.85rem}\n.ctl label span.lbl{color:var(--muted);font-family:var(--font);font-size:.88rem}\ninput[type=range]{width:100%;accent-color:var(--accent)}\n.btns{display:flex;gap:6px;flex-wrap:wrap;align-items:center;margin:6px 0}\nbutton.b{appearance:none;font:inherit;font-size:.88rem;border:1px solid var(--line);background:var(--card);color:var(--fg);padding:5px 12px;border-radius:7px;cursor:pointer}\nbutton.b:hover{border-color:var(--accent)}\nbutton.b:disabled{opacity:.5;cursor:default}\nbutton.b.primary{background:var(--accent);border-color:var(--accent);color:#fff}\n.chk{font-size:.88rem;color:var(--muted);display:inline-flex;gap:5px;align-items:center;margin-right:10px}\n.chk input{accent-color:var(--accent)}\n.seg{display:inline-flex;border:1px solid var(--line);border-radius:8px;overflow:hidden;flex-wrap:wrap}\n.seg button{appearance:none;font:inherit;font-size:.86rem;border:0;background:none;color:var(--muted);padding:5px 10px;cursor:pointer}\n.seg button[aria-pressed=\"true\"]{background:var(--accent);color:#fff}\n.callout{border-left:3px solid var(--accent);background:var(--accent-soft);padding:7px 11px;border-radius:0 8px 8px 0;margin:8px 0}\n.callout.note{border-left-color:var(--accent2);background:color-mix(in srgb,var(--accent2) 10%,transparent);font-size:.92em}\n.callout.warn{border-left-color:var(--accent2);background:color-mix(in srgb,var(--accent2) 13%,transparent)}\n.formula{font-size:1.06em;display:inline-block;margin:2px 0}\n/* overview */\n.cards{display:flex;flex-wrap:wrap;gap:10px;margin-top:8px}\n.ov{display:flex;flex-direction:column;gap:4px;flex:1 1 230px;min-width:0}\n.ov .num{font-size:.75rem;color:var(--muted);font-family:var(--mono)}\n.ov h4{margin:0;font-size:.95rem}\n.ov p{margin:0;font-size:.88rem;flex:1}\n.ov button{align-self:flex-start;margin-top:6px}\ntable.cmp{border-collapse:collapse;width:100%;font-size:.88rem;margin:6px 0}\ntable.cmp td,table.cmp th{border-bottom:1px solid var(--line);padding:5px 6px;text-align:left;vertical-align:top}\ntable.cmp th{color:var(--muted);font-weight:600}\ndetails{margin-top:10px}\nsummary{cursor:pointer;color:var(--accent);font-weight:600}\n/* flows */\n.flows{display:grid;grid-template-columns:auto 1fr auto;gap:4px 8px;align-items:center;font-size:.86rem;margin-top:8px}\n.bar{height:10px;border-radius:5px;background:var(--line);overflow:hidden}\n.bar i{display:block;height:100%;width:0;border-radius:5px;transition:width .08s linear}\n.legend{display:flex;gap:12px;font-size:.8rem;color:var(--muted);margin-top:4px;flex-wrap:wrap}\n.dot{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:4px;vertical-align:-1px}\n/* units */\n.eq{display:flex;align-items:flex-start;justify-content:center;gap:10px;flex-wrap:wrap;margin:14px 0 6px}\n.term{display:flex;flex-direction:column;align-items:center;gap:6px;min-width:90px}\n.term .sym{height:2.6em;display:flex;align-items:center;font-size:1.45rem;font-family:\"Cambria Math\",\"STIX Two Math\",Georgia,serif;padding:4px 12px;border-radius:8px;border:2px solid transparent;transition:all .35s}\n.term.hl .sym{border-color:var(--accent);background:var(--accent-soft)}\n.term .unit{min-height:46px;font-size:.92rem;color:var(--accent2);opacity:0;transform:translateY(-6px);transition:all .45s;text-align:center}\n.term .unit.show{opacity:1;transform:none}\n.op{font-size:1.45rem;height:2.6em;display:flex;align-items:center;font-family:Georgia,serif}\n.frac{display:inline-flex;flex-direction:column;align-items:center;vertical-align:middle;line-height:1.15;margin:0 2px}\n.frac>span:first-child{border-bottom:1.5px solid currentColor;padding:0 3px}\n.frac>span:last-child{padding:0 3px}\n.derive{min-height:120px;display:flex;flex-direction:column;align-items:center;gap:8px;margin:6px 0}\n.derive .ln{opacity:0;animation:fadein .5s forwards;font-size:1.05rem;display:flex;align-items:center;gap:8px;flex-wrap:wrap;justify-content:center}\n@keyframes fadein{from{opacity:0;transform:translateY(6px)}to{opacity:1;transform:none}}\n.cancel{position:relative;display:inline-block}\n.cancel::after{content:\"\";position:absolute;left:-2px;top:50%;height:2px;background:var(--warn);width:0;animation:strike .5s forwards;animation-delay:inherit;transform:rotate(-12deg)}\n@keyframes strike{to{width:calc(100% + 4px)}}\n.stepper{display:flex;justify-content:center;align-items:center;gap:10px;margin-top:6px}\n.dots{display:flex;gap:5px}\n.dots i{width:8px;height:8px;border-radius:50%;background:var(--line);cursor:pointer}\n.dots i.on{background:var(--accent)}\n.steptext{min-height:4.5em;text-align:center;max-width:620px;margin:6px auto}\n.term .unit.same{background:var(--accent-soft);border:1.5px solid var(--accent);border-radius:8px;padding:2px 8px;color:var(--fg);animation:same .9s ease-out}\n@keyframes same{0%{transform:scale(1)}40%{transform:scale(1.12)}100%{transform:scale(1)}}\n/* quiz */\n.q{margin:10px 0;padding:10px 12px}\n.q .opts{display:flex;flex-wrap:wrap;gap:6px;margin:6px 0}\n.q .opts button{appearance:none;font:inherit;font-size:.92rem;border:1px solid var(--line);background:var(--bg);color:var(--fg);padding:5px 12px;border-radius:7px;cursor:pointer;min-height:38px}\n.q .opts button.right{border-color:var(--accent);background:var(--accent-soft)}\n.q .opts button.wrong{border-color:var(--warn);opacity:.7}\n.q .fb{font-size:.88rem;display:none}\n.q .fb.show{display:block}\n.score{font-size:.9rem;color:var(--muted)}\n/* animated marks */\n.qhead{display:flex;gap:10px;justify-content:space-between;align-items:flex-start}\n.qmark{position:relative;width:34px;height:34px;flex:none}\n.mark{width:100%;height:100%;overflow:visible}\n.mark circle,.mark path{fill:none;stroke-linecap:round;stroke-linejoin:round}\n.mark circle{stroke-width:2.6;stroke-dasharray:88;stroke-dashoffset:88;animation:draw .45s ease-out forwards;transform:rotate(-90deg);transform-origin:center}\n.mark path{stroke-width:3.4;stroke-dasharray:30;stroke-dashoffset:30;animation:draw .35s .4s ease-out forwards}\n.mark.ok circle,.mark.ok path{stroke:var(--accent)}\n.mark.no circle,.mark.no path{stroke:var(--warn)}\n.mark.ok{animation:pop .35s .7s ease-out}\n@keyframes draw{to{stroke-dashoffset:0}}\n@keyframes pop{50%{transform:scale(1.22)}}\n.burst{position:absolute;left:50%;top:50%;width:0;height:0;pointer-events:none}\n.burst i{position:absolute;width:6px;height:6px;margin:-3px;border-radius:50%;background:var(--accent);opacity:0;animation:burst .7s .55s ease-out forwards}\n.burst i:nth-child(odd){background:var(--accent2)}\n@keyframes burst{0%{opacity:1;transform:rotate(var(--a)) translateY(0)}100%{opacity:0;transform:rotate(var(--a)) translateY(calc(-1 * var(--d,26px)))}}\n.shake{animation:shake .4s}\n@keyframes shake{20%,60%{transform:translateX(-5px)}40%,80%{transform:translateX(5px)}}\n.q .fb.ok{color:var(--fg)}\n.q .fb.no{color:var(--warn)}\n.q .opts button.right{font-weight:600}\n.slidein{animation:fadein .35s}\n.progress{height:6px;background:var(--line);border-radius:3px;overflow:hidden;margin:6px 0 10px}\n.progress i{display:block;height:100%;width:0;background:var(--accent);transition:width .4s}\n.endcard{text-align:center;padding:24px 12px}\n.bigmark{position:relative;width:84px;height:84px;margin:0 auto 6px;--d:70px}\n.bigmark .burst i{width:9px;height:9px;margin:-4.5px}\n@media (prefers-reduced-motion:reduce){.mark,.mark *,.burst i,.shake{animation-duration:.01s!important;animation-delay:0s!important}}\n.ctl.locked{opacity:.5}\n.ctl.locked input{cursor:not-allowed}\n.toast{position:absolute;left:50%;bottom:calc(100% + 10px);transform:translate(-50%,6px);z-index:5;background:var(--fg);color:var(--bg);padding:9px 13px;border-radius:9px;font-size:.88rem;line-height:1.4;width:max-content;max-width:min(540px,94%);box-shadow:0 6px 20px rgba(0,0,0,.25);opacity:0;pointer-events:none;transition:opacity .2s,transform .2s}\n.toast::after{content:\"\";position:absolute;left:50%;top:100%;margin-left:-7px;border:7px solid transparent;border-top-color:var(--fg)}\n.toast.show{opacity:1;transform:translate(-50%,0)}\n.toast b{font-weight:700}\n.ptitle{font-weight:600;font-size:.92rem;margin:0 0 4px}\n/* quiz scoreboard */\n.qboard{display:flex;flex-wrap:wrap;gap:5px;align-items:center;margin:-2px 0 10px}\n.qdot{appearance:none;background:none;font:inherit;padding:0;cursor:pointer;width:24px;height:24px;border-radius:50%;display:inline-flex;align-items:center;justify-content:center;font-size:.72rem;border:1.5px solid var(--line);color:var(--muted);transition:transform .15s}\n.qdot:not(:disabled):hover{transform:scale(1.12)}\n.qdot:disabled{cursor:default;opacity:.55}\n.qdot.here{box-shadow:0 0 0 2px var(--bg),0 0 0 3.5px var(--fg)}\n.qdot.fin{border-color:transparent;font-size:.85rem}\n.qdot.ok{background:var(--accent);border-color:var(--accent);color:#fff;font-weight:700}\n.qdot.retry{border-color:var(--accent);color:var(--accent);font-weight:700}\n.qdot.cur{border-color:var(--fg);color:var(--fg);font-weight:700}\n.qreset{margin-left:auto;appearance:none;border:0;background:none;color:var(--muted);font:inherit;font-size:.8rem;cursor:pointer;text-decoration:underline}\n.tabcount{font-size:.75em;font-weight:600;background:var(--accent-soft);color:var(--accent);border-radius:9px;padding:1px 6px;margin-left:2px}\n[hidden]{display:none!important}\n";
const HTML="\n<nav class=\"tabs\" role=\"tablist\">\n  <button class=\"tab\" data-tab=\"overzicht\">In \u00e9\u00e9n oogopslag<\/button>\n  <button class=\"tab\" data-tab=\"netwerk\">Wie ziet wie?<\/button>\n  <button class=\"tab\" data-tab=\"hokjes\">Groeien in lege hokjes<\/button>\n  <button class=\"tab\" data-tab=\"quiz\">Quiz \u2713<\/button>\n<\/nav>\n\n<section class=\"panel\" id=\"tab-overzicht\">\n  <h2>Hoofdstuk 13 in \u00e9\u00e9n oogopslag<\/h2>\n  <div class=\"grid2\">\n    <div>\n      <p>Tot nu toe beschreven we <b>populaties<\/b>: \u00e9\u00e9n getal N voor de hele groep, in een goed gemengde wereld. In een <b>individual-based model<\/b> (IBM) beschrijven we <b>individuen<\/b>, elk met een eigen positie. Interacties zijn dan <b>lokaal<\/b>: een cel ziet alleen zijn buren.<\/p>\n      <p class=\"muted\" style=\"font-size:.88rem\">Rechts groeit een bacteriekolonie op een agarplaat. Elke cel kan een dochter zetten in een leeg buurhokje. Af en toe ontstaat een mutant (blauw). Alleen mutanten aan de rand kunnen uitgroeien tot een \"taartpunt\". Dat kun je met een ODE niet zien.<\/p>\n      <table class=\"cmp\">\n        <tr><th><\/th><th>ODE<\/th><th>IBM<\/th><\/tr>\n        <tr><td>Omschrijvingsniveau<\/td><td>populatie<\/td><td>individu<\/td><\/tr>\n        <tr><td>Ruimte<\/td><td>goed gemengd<\/td><td>posities, lokale interacties<\/td><\/tr>\n        <tr><td>Toeval<\/td><td>nee (deterministisch)<\/td><td>ja (kansen per individu)<\/td><\/tr>\n        <tr><td>Draagvermogen<\/td><td>moet je erin stoppen (K)<\/td><td>kan vanzelf ontstaan<\/td><\/tr>\n      <\/table>\n    <\/div>\n    <div class=\"card\"><canvas id=\"hero\" class=\"sq\"><\/canvas>\n      <div class=\"muted\" id=\"hero-lbl\" style=\"font-size:.85rem;text-align:center;margin-top:4px\"><\/div><\/div>\n  <\/div>\n\n  <div class=\"cards\">\n    <div class=\"card ov\"><span class=\"num\">13.2<\/span><h4>Wat is een IBM?<\/h4>\n      <p>Een individual-based model (ook <i>agent-based model<\/i>) beschrijft organismen als losse individuen met eigen eigenschappen. Het <b>omschrijvingsniveau<\/b> bepaalt welke <b>modeluitkomsten<\/b> je kunt krijgen.<\/p><\/div>\n    <div class=\"card ov\"><span class=\"num\">13.3<\/span><h4>Individuen en hun posities<\/h4>\n      <p>Op een grid heeft elke cel een plek. Een cel kan zich alleen voortplanten naar een leeg buurhokje. Daardoor groeit een kolonie aan de <b>rand<\/b>, niet in het midden.<\/p>\n      <button class=\"b\" data-go=\"netwerk\">Wie ziet wie? \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">13.4<\/span><h4>Geboorte in lege hokjes<\/h4>\n      <p>De geboortekans in de code hangt niet af van de dichtheid. Toch ontstaat er een <b>draagvermogen<\/b>: hoe voller het grid, hoe minder lege hokjes, hoe kleiner de <i>effectieve<\/i> geboortekans.<\/p>\n      <button class=\"b\" data-go=\"hokjes\">Probeer het \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">13.4<\/span><h4>Stochasticiteit<\/h4>\n      <p>Geboorte en sterfte zijn kansen, dus elke simulatie loopt anders. Een kleine populatie kan door pech <b>uitsterven<\/b>, zelfs als b &gt; d. In een ODE met b &gt; d kan dat niet.<\/p>\n      <button class=\"b\" data-go=\"hokjes\">Zie het gebeuren \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">13.5<\/span><h4>Lokaal samenwerken<\/h4>\n      <p>Cellen helpen hun buren, bijvoorbeeld door aminozuren uit te wisselen. De geboortekans wordt dan b + c \u00d7 (aantal helpende buren). Het voordeel is <b>lokaal<\/b>: alleen de 8 buren profiteren.<\/p><\/div>\n    <div class=\"card ov\"><span class=\"num\">13.6<\/span><h4>De parasiet<\/h4>\n      <p>Een <i>cheater<\/i> krijgt wel hulp, maar geeft zelf niets. Zonder mengen ontstaat een cyclus van verdringen en heroveren. Met mengen nemen de cheaters het over. Of daarna alles uitsterft, hangt af van de parameters.<\/p><\/div>\n    <div class=\"card ov\"><span class=\"num\">13.7<\/span><h4>Eindeloze mogelijkheden<\/h4>\n      <p>Hokjes kunnen ook voedsel, antibiotica of feromonen bevatten, en individuen kunnen vrij bewegen. Maar: een model moet zo simpel mogelijk zijn, en niet simpeler.<\/p><\/div>\n    <div class=\"card ov\"><span class=\"num\">\u2713<\/span><h4>Test jezelf<\/h4>\n      <p>Acht korte vragen over individuen, ruimte en toeval. Bij een fout antwoord mag je het opnieuw proberen.<\/p>\n      <button class=\"b primary\" data-go=\"quiz\">Start de quiz \u2192<\/button><\/div>\n  <\/div>\n\n  <details>\n    <summary>Terminologie<\/summary>\n    <table class=\"cmp\">\n      <tr><th>Nederlands<\/th><th>Engels<\/th><th>Betekenis<\/th><\/tr>\n      <tr><td>Individual-based model<\/td><td>Individual-based model<\/td><td>Model dat de eigenschappen en het gedrag van individuen beschrijft.<\/td><\/tr>\n      <tr><td>Omschrijvingsniveau<\/td><td>Level of description<\/td><td>Wat het model beschrijft: de populatie (ODE) of het individu (IBM).<\/td><\/tr>\n      <tr><td>Modeluitkomsten<\/td><td>Model outcomes<\/td><td>Welke uitkomsten mogelijk zijn, hangt af van het omschrijvingsniveau.<\/td><\/tr>\n      <tr><td>Stochasticiteit<\/td><td>Stochasticity<\/td><td>Toeval in processen, waardoor uitkomsten vari\u00ebren, tot uitsterven aan toe.<\/td><\/tr>\n      <tr><td>Lokale interactie<\/td><td>Local interaction<\/td><td>Interactie die alleen op korte afstand plaatsvindt.<\/td><\/tr>\n    <\/table>\n  <\/details>\n<\/section>\n\n<section class=\"panel\" id=\"tab-netwerk\" hidden>\n  <h2>Wie ziet wie?<\/h2>\n  <p>Elk model maakt een aanname over <b>wie met wie kan interacteren<\/b>. Hieronder staan dezelfde individuen twee keer. Een lijn betekent: deze twee kunnen elkaar tegenkomen, helpen, beconcurreren of opeten. <b>Klik op een individu<\/b> om te zien met wie het kan interacteren.<\/p>\n  <div class=\"grid2\">\n    <div class=\"card\">\n      <div class=\"ptitle\">ODE: goed gemengd<\/div>\n      <canvas id=\"nw-ode\" class=\"sq\" style=\"cursor:pointer\"><\/canvas>\n      <div class=\"muted\" style=\"font-size:.82rem;margin-top:4px\">Iedereen is met iedereen verbonden: elk individu kan elk ander tegenkomen.<\/div>\n    <\/div>\n    <div class=\"card\">\n      <div class=\"ptitle\">IBM op een grid: lokaal<\/div>\n      <canvas id=\"nw-grid\" class=\"sq\" style=\"cursor:pointer\"><\/canvas>\n      <div class=\"muted\" style=\"font-size:.82rem;margin-top:4px\">Alleen buren zijn met elkaar verbonden. De positie bepaalt met wie je te maken hebt.<\/div>\n    <\/div>\n  <\/div>\n  <div class=\"btns\" style=\"gap:14px\">\n    <span class=\"muted\" style=\"font-size:.88rem\">Aantal individuen:<\/span><div class=\"seg\" id=\"nw-n\"><\/div>\n    <span class=\"muted\" style=\"font-size:.88rem\">Buren op het grid:<\/span><div class=\"seg\" id=\"nw-k\"><\/div>\n  <\/div>\n  <div class=\"callout\" id=\"nw-text\"><\/div>\n<\/section>\n\n<section class=\"panel\" id=\"tab-hokjes\" hidden>\n  <h2>Groeien in lege hokjes<\/h2>\n  <p>Elke tijdstap probeert elke cel zich voort te planten met kans b. De dochter komt in een willekeurig gekozen buurhokje (boven, onder, links of rechts), maar <b>alleen als dat hokje leeg is<\/b>. Daarna sterft elke cel met kans d. De kleur wordt doorgegeven aan de dochter, maar heeft verder geen invloed.<\/p>\n  <div class=\"btns\"><span class=\"muted\" style=\"font-size:.88rem\">Voorbeelden uit het boek:<\/span><div class=\"seg\" id=\"ih-presets\"><\/div><\/div>\n  <div class=\"grid2\">\n    <div class=\"card\">\n      <div class=\"ptitle\">Het grid (32 \u00d7 32)<\/div>\n      <canvas id=\"ih-grid\" class=\"sq\"><\/canvas>\n    <\/div>\n    <div class=\"card\">\n      <div class=\"ptitle\">Aantal cellen N door de tijd<\/div>\n      <canvas id=\"ih-n\" class=\"plot-half\"><\/canvas>\n      <div class=\"ptitle\" style=\"margin-top:8px\">Effectieve geboortekans per cel<\/div>\n      <canvas id=\"ih-b\" class=\"plot-half\"><\/canvas>\n      <div class=\"muted\" style=\"font-size:.82rem;margin-top:4px\">Oranje: welk deel van de cellen er deze stap \u00e9cht een dochter bij kreeg. Grijs gestippeld: de geboortekans b uit de regels. Blauw gestippeld: de sterftekans d.<\/div>\n    <\/div>\n  <\/div>\n  <div class=\"controls\" id=\"ih-ctl\">\n    <div class=\"toast\" id=\"ih-toast\" role=\"status\" aria-live=\"polite\"><\/div>\n    <div class=\"ctl\"><label><span class=\"lbl\">geboortekans b<\/span> <span id=\"ih-b-v\"><\/span><\/label><input type=\"range\" id=\"ih-sb\" min=\"0.05\" max=\"1\" step=\"0.01\" value=\"0.6\"><\/div>\n    <div class=\"ctl\"><label><span class=\"lbl\">sterftekans d<\/span> <span id=\"ih-d-v\"><\/span><\/label><input type=\"range\" id=\"ih-sd\" min=\"0.01\" max=\"0.5\" step=\"0.01\" value=\"0.1\"><\/div>\n  <\/div>\n  <div class=\"btns\">\n    <button class=\"b primary\" id=\"ih-play\">\u25b6 Start<\/button>\n    <button class=\"b\" id=\"ih-again\">\u21ba Nieuwe herhaling<\/button>\n    <span class=\"muted\" style=\"font-size:.82rem\">Eerdere herhalingen blijven grijs zichtbaar.<\/span>\n  <\/div>\n  <div class=\"callout\" id=\"ih-text\"><\/div>\n<\/section>\n\n<section class=\"panel\" id=\"tab-quiz\" hidden>\n  <h2>Quiz: ruimtelijke groei<\/h2>\n  <div style=\"display:flex;justify-content:space-between;font-size:.88rem\" class=\"muted\"><span id=\"qz-count\"><\/span><span id=\"qz-score\"><\/span><\/div>\n  <div class=\"progress\"><i id=\"qz-prog\"><\/i><\/div>\n  <div id=\"qz-box\"><\/div>\n  <div class=\"btns\" style=\"justify-content:flex-end\"><button class=\"b primary\" id=\"qz-next\" hidden>Volgende vraag \u2192<\/button><\/div>\n<\/section>\n\n";
function mount(host){
if(host.__appMounted) return; host.__appMounted=true;
const root=host.attachShadow({mode:'open'});
root.innerHTML='<style>'+STYLE+'</style><div class="app">'+HTML+'</div>';
const $=s=>root.querySelector(s);
const urlP=new URLSearchParams(location.search), useUrl=host.hasAttribute('data-url-params');
const params={get:k=>host.dataset[k]!=null?host.dataset[k]:(useUrl?urlP.get(k):null)};
let forcedTheme=params.get('theme');
window.addEventListener('message',e=>{ if(e.data&&e.data.type==='theme'){ forcedTheme=e.data.value; applyTheme(); }});

/* ---------- helpers ---------- */
let FONT='12px system-ui,sans-serif';
let cssCache=null;
const probe=document.createElement('span'); probe.style.display='none'; root.appendChild(probe);
function col(n){ if(!cssCache){ cssCache={}; FONT='12px '+getComputedStyle(host).fontFamily; }
  if(!(n in cssCache)){ probe.style.color='var(--'+n+')'; cssCache[n]=getComputedStyle(probe).color; } return cssCache[n]; }
function detectDark(){
  if(forcedTheme) return forcedTheme==='dark';
  const b=document.body; if(b.classList.contains('quarto-dark')) return true; if(b.classList.contains('quarto-light')) return false;
  for(let el=host.parentElement; el; el=el.parentElement){ const m=(getComputedStyle(el).backgroundColor.match(/[\d.]+/g)||[]).map(Number);
    if(m.length>=3&&(m.length<4||m[3]>0.5)) return (0.299*m[0]+0.587*m[1]+0.114*m[2])<128; }
  return matchMedia('(prefers-color-scheme: dark)').matches; }
/* Quarto wisselt van thema via body.quarto-dark en een andere stylesheet, zonder data-bs-theme op <html>; de --bs-body-*
   variabelen blijven dan die van het lichte thema. Daarom lezen we de echte tekst- en achtergrondkleur van de pagina. */
function applyTheme(){ const dark=detectDark(); if(dark) host.setAttribute('data-dark',''); else host.removeAttribute('data-dark');
  const cs=getComputedStyle(document.body), opaque=c=>c&&c!=='transparent'&&!/rgba\([^)]*,\s*0\s*\)$/.test(c);
  host.style.setProperty('--fg',opaque(cs.color)?cs.color:(dark?'#e6e9e7':'#1d2327'));
  host.style.setProperty('--bg',opaque(cs.backgroundColor)?cs.backgroundColor:(dark?'#16191b':'#ffffff'));
  host.style.setProperty('--line',dark?'color-mix(in srgb, var(--fg) 20%, var(--bg))':'color-mix(in srgb, var(--fg) 14%, var(--bg))');
  cssCache=null; }
applyTheme();
new MutationObserver(applyTheme).observe(document.body,{attributes:true,attributeFilter:['class','data-bs-theme','style']});
matchMedia('(prefers-color-scheme: dark)').addEventListener('change',applyTheme);
function fitCanvas(c){
  const r=c.getBoundingClientRect(), dpr=window.devicePixelRatio||1;
  const w=Math.max(1,Math.round(r.width)), h=Math.max(1,Math.round(r.height));
  if(c.width!==Math.round(w*dpr)||c.height!==Math.round(h*dpr)){ c.width=Math.round(w*dpr); c.height=Math.round(h*dpr); }
  const ctx=c.getContext('2d'); ctx.setTransform(dpr,0,0,dpr,0,0); return {ctx,w,h};
}
function niceStep(span,n){ const s0=span/n, mag=Math.pow(10,Math.floor(Math.log10(s0))), e=s0/mag;
  return (e>=7.5?10:e>=3.5?5:e>=1.5?2:1)*mag; }
function niceTicks(a,b,n){ const st=niceStep(b-a,n), t=[]; for(let v=Math.ceil(a/st-1e-9)*st; v<=b+1e-9; v+=st) t.push(+v.toFixed(10)); return t; }
function niceCeil(v){ if(v<=0) return 1; const st=niceStep(v,5); return Math.ceil(v/st-1e-9)*st; }
function fmt(v,dig=3){ if(!isFinite(v)) return '∞'; const a=Math.abs(v); if(a===0) return '0';
  if(a>=1e5||a<1e-3) return v.toExponential(1).replace('e+','e'); return String(+v.toPrecision(dig)); }
function frame(c,o){
  const {ctx,w,h}=fitCanvas(c);
  const p=Object.assign({l:50,r:12,t:12,b:38},o.pad||{});
  const pw=Math.max(10,w-p.l-p.r), ph=Math.max(10,h-p.t-p.b);
  const X=x=>p.l+(x-o.xmin)/(o.xmax-o.xmin)*pw, Y=y=>p.t+ph-(y-o.ymin)/(o.ymax-o.ymin)*ph;
  ctx.clearRect(0,0,w,h); ctx.font=FONT; ctx.lineWidth=1;
  ctx.strokeStyle=col('grid'); ctx.fillStyle=col('muted');
  ctx.textAlign='center'; ctx.textBaseline='top';
  for(const t of niceTicks(o.xmin,o.xmax,o.nx||6)){ const x=X(t); ctx.beginPath(); ctx.moveTo(x,p.t); ctx.lineTo(x,p.t+ph); ctx.stroke(); ctx.fillText(fmt(t),x,p.t+ph+5); }
  ctx.textAlign='right'; ctx.textBaseline='middle';
  for(const t of niceTicks(o.ymin,o.ymax,o.ny||5)){ const y=Y(t); ctx.beginPath(); ctx.moveTo(p.l,y); ctx.lineTo(p.l+pw,y); ctx.stroke(); ctx.fillText(fmt(t),p.l-6,y); }
  ctx.strokeStyle=col('axis'); ctx.lineWidth=1.2; ctx.beginPath();
  ctx.moveTo(p.l,p.t); ctx.lineTo(p.l,p.t+ph); ctx.lineTo(p.l+pw,p.t+ph); ctx.stroke();
  if(o.ymin<0&&o.ymax>0){ ctx.beginPath(); ctx.moveTo(p.l,Y(0)); ctx.lineTo(p.l+pw,Y(0)); ctx.stroke(); }
  ctx.fillStyle=col('muted'); ctx.textAlign='center'; ctx.textBaseline='bottom';
  if(o.xlab) drawMath(ctx,o.xlab,p.l+pw/2,h-2);
  if(o.ylab){ ctx.save(); ctx.translate(12,p.t+ph/2); ctx.rotate(-Math.PI/2); ctx.textBaseline='middle'; drawMath(ctx,o.ylab,0,0); ctx.restore(); }
  ctx.save(); ctx.beginPath(); ctx.rect(p.l,p.t,pw,ph); ctx.clip();
  return {ctx,w,h,X,Y,p,pw,ph,o,
    inv:(px,py)=>[o.xmin+(px-p.l)/pw*(o.xmax-o.xmin), o.ymin+(p.t+ph-py)/ph*(o.ymax-o.ymin)],
    inside:(px,py)=>px>=p.l&&px<=p.l+pw&&py>=p.t&&py<=p.t+ph};
}
function poly(f,pts,color,width=2,dash=null,alpha=1){
  const c=f.ctx; c.save(); c.globalAlpha=alpha; c.strokeStyle=color; c.lineWidth=width; c.lineJoin='round'; if(dash) c.setLineDash(dash);
  c.beginPath(); let started=false;
  for(const [x,y] of pts){ if(!isFinite(y)){ started=false; continue; } const px=f.X(x), py=f.Y(y); if(!started){ c.moveTo(px,py); started=true; } else c.lineTo(px,py); }
  c.stroke(); c.restore();
}
function disc(c,x,y,r,fill,stroke,lw=1.5){ c.beginPath(); c.arc(x,y,r,0,2*Math.PI); if(fill){ c.fillStyle=fill; c.fill(); } if(stroke){ c.strokeStyle=stroke; c.lineWidth=lw; c.stroke(); } }
function localXY(e,c){ const r=c.getBoundingClientRect(); return [e.clientX-r.left,e.clientY-r.top]; }
const logisticExact=(t,x0,r)=>1/(1+(1/x0-1)*Math.exp(-r*t));
function euler(r,dt,x0,T){ const pts=[[0,x0]]; let x=x0, t=0, broke=false; const n=Math.round(T/dt);
  for(let i=0;i<n;i++){ x=x+dt*r*x*(1-x); t=+(t+dt).toFixed(10); pts.push([t,x]); if(!isFinite(x)||x<-0.6||x>3){ broke=true; break; } }
  return {pts,broke}; }


/* ---------- math typesetting: variables & parameters italic, the "d" of d/dt upright ---------- */
function mathRuns(s){
  const runs=[], push=(t,k)=>{ if(!t) return; const l=runs[runs.length-1]; if(l&&l.k===k) l.t+=t; else runs.push({t,k}); };
  const re=/[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u024F\u0370-\u03FF]+/g; let last=0, mt;
  while((mt=re.exec(s))){ push(s.slice(last,mt.index),'n'); let w=mt[0]; const i=mt.index; last=i+w.length;
    const prev=s[i-1]||'', nxt=s.slice(last,last+2);
    if(w.length===2&&w[0]==='d'&&(nxt==='/d'||prev==='/')){ push('d','u'); push(w[1],'i'); continue; }
    if(w==='L'&&s.slice(i-3,i)==='mg/'){ push(w,'n'); continue; }
    if(prev==='\''||prev==='’'){ push(w,'n'); continue; }   // Dutch elisions: zo'n, 't
    for(const part of w.split(/(Δ)/)){ if(!part) continue;
      if(part==='Δ') push('Δ','n');
      else if(part.length===1||/^[a-zα-ω][A-Z]{1,2}$/.test(part)||(w.includes('Δ')&&part.length<=2)) push(part,'i');
      else push(part,'n'); }
  }
  push(s.slice(last),'n'); return runs;
}
const MATH_SKIP='script,style,nav,canvas,svg,input,textarea,.frac,.mi,.mu,.nomath,.tab,.num';
function mathify(node){
  if(!node) return;
  if(node.nodeType===3){ const par=node.parentElement; if(!par||par.closest(MATH_SKIP)) return;
    const runs=mathRuns(node.nodeValue); if(!runs.some(r=>r.k!=='n')) return;
    const fr=document.createDocumentFragment();
    for(const r of runs){ if(r.k==='n') fr.appendChild(document.createTextNode(r.t)); else { const e=document.createElement(r.k==='i'?'i':'span'); e.className=r.k==='i'?'mi':'mu'; e.textContent=r.t; fr.appendChild(e); } }
    node.replaceWith(fr); return; }
  if(node.nodeType!==1&&node.nodeType!==11) return;
  if(node.nodeType===1&&node.closest(MATH_SKIP)) return;
  const tw=document.createTreeWalker(node,4), list=[]; while(tw.nextNode()) list.push(tw.currentNode);
  list.forEach(mathify);
}
function drawMath(ctx,str,x,y){
  const runs=mathRuns(String(str)), base=ctx.font, ital='italic '+base.replace(/^italic\s+/,'');
  const ws=runs.map(r=>{ ctx.font=r.k==='i'?ital:base; return ctx.measureText(r.t).width; });
  const tot=ws.reduce((a,b)=>a+b,0), al=ctx.textAlign;
  let cx=al==='center'?x-tot/2:(al==='right'||al==='end')?x-tot:x;
  ctx.textAlign='left';
  runs.forEach((r,k)=>{ ctx.font=r.k==='i'?ital:base; ctx.fillText(r.t,cx,y); cx+=ws[k]; });
  ctx.font=base; ctx.textAlign=al;
}

/* ---------- iframe height ---------- */
function postHeight(){ if(useUrl&&window.parent!==window){ window.parent.postMessage({type:'bioms-iframe-height',height:document.documentElement.scrollHeight},'*'); } }
new ResizeObserver(()=>{ postHeight(); }).observe(host);

/* =========================================================
   QUIZ HELPERS (animated tick / cross)
   ========================================================= */
const MARK_OK='<svg class="mark ok" viewBox="0 0 32 32" aria-label="goed"><circle cx="16" cy="16" r="14"/><path d="M9.5 16.5l4.5 4.5 9-9.5"/></svg>';
const MARK_NO='<svg class="mark no" viewBox="0 0 32 32" aria-label="fout"><circle cx="16" cy="16" r="14"/><path d="M11.5 11.5l9 9M20.5 11.5l-9 9"/></svg>';
function burst(el){ const b=document.createElement('span'); b.className='burst';
  for(let i=0;i<12;i++){ const s=document.createElement('i'); s.style.setProperty('--a',(i*30)+'deg'); b.appendChild(s); }
  el.appendChild(b); setTimeout(()=>b.remove(),1400); }
/* answers are remembered in this browser (localStorage), so a refresh keeps the student's progress */
const store={ get(k){ try{ return JSON.parse(localStorage.getItem(k)); }catch(e){ return null; } },
  set(k,v){ try{ localStorage.setItem(k,JSON.stringify(v)); }catch(e){} },
  del(k){ try{ localStorage.removeItem(k); }catch(e){} } };
function qhash(list){ const s=list.map(q=>q.q+'|'+q.o.join('|')+'|'+q.a).join('§'); let h=5381; for(let k=0;k<s.length;k++) h=((h*33)^s.charCodeAt(k))>>>0; return h.toString(36); }
const newState=()=>({wrong:[],solved:false,first:true});
/* st: saved state of this question (mutated in place); onChange: called after every click so it can be saved */
function renderQuestion(it,num,onSolved,st,onChange){
  st=st||newState();
  const d=document.createElement('div'); d.className='card q';
  d.innerHTML=`<div class="qhead"><div>${num!=null?num+'. ':''}${it.q}</div><div class="qmark"></div></div><div class="opts"></div><div class="fb"></div>`;
  const ob=d.querySelector('.opts'), fb=d.querySelector('.fb'), mk=d.querySelector('.qmark');
  const btns=[];
  function markRight(b,anim){ b.classList.add('right'); mk.innerHTML=MARK_OK; if(anim) burst(mk);
    fb.innerHTML=(st.first?'<b>Goed!</b> ':'<b>Goed zo, nu wel.</b> ')+it.e; fb.className='fb show ok';
    btns.forEach(x=>{ if(x!==b) x.style.opacity=.45; }); }
  function markWrong(b,anim){ b.classList.add('wrong'); if(anim){ b.classList.remove('shake'); void b.offsetWidth; b.classList.add('shake'); }
    mk.innerHTML=MARK_NO; fb.innerHTML='Nog niet goed. '+(it.h||'Probeer het nog eens!'); fb.className='fb show no'; }
  it.o.forEach((o,oi)=>{ const b=document.createElement('button'); b.innerHTML=o; btns.push(b); b.onclick=()=>{
    if(st.solved||b.classList.contains('wrong')) return;
    if(oi===it.a){ st.solved=true; markRight(b,true); onChange&&onChange(st); onSolved&&onSolved(st.first); }
    else { st.first=false; st.wrong.push(oi); markWrong(b,true); onChange&&onChange(st); }
  }; ob.appendChild(b); });
  /* restore a previous visit without animations */
  st.wrong.forEach(oi=>{ if(btns[oi]) markWrong(btns[oi],false); });
  if(st.solved) markRight(btns[it.a],false);
  return d;
}

/* =========================================================
   IN ÉÉN OOGOPSLAG: colony on agar with mutant sectors (13.3)
   ========================================================= */
const NB4=[[1,0],[-1,0],[0,1],[0,-1]];
function drawGrid(c,G,W,H,colors){
  const {ctx,w,h}=fitCanvas(c); const s=Math.min(w,h)/W, ox=(w-s*W)/2, oy=(h-s*H)/2;
  ctx.clearRect(0,0,w,h); ctx.fillStyle=col('card'); ctx.fillRect(ox,oy,s*W,s*H);
  for(let y=0;y<H;y++) for(let x=0;x<W;x++){ const v=G[y*W+x]; if(!v) continue; ctx.fillStyle=colors[v]; ctx.fillRect(ox+x*s,oy+y*s,Math.ceil(s),Math.ceil(s)); }
  ctx.strokeStyle=col('line'); ctx.lineWidth=1; ctx.strokeRect(ox+.5,oy+.5,s*W-1,s*H-1);
}
const hero=(()=>{
  const c=$('#hero'), lbl=$('#hero-lbl'), W=81, H=81;
  let G, acc=0, hold=0, steps=0;
  function reset(){ G=new Uint8Array(W*H); G[40*W+40]=1; steps=0; hold=0; }
  function step(){
    const add=[];
    for(let i=0;i<W*H;i++){ const v=G[i]; if(!v) continue; const x=i%W, y=(i/W)|0;
      for(const [dx,dy] of NB4){ const nx=x+dx, ny=y+dy; if(nx<0||ny<0||nx>=W||ny>=H) continue; const j=ny*W+nx;
        if(!G[j]&&Math.random()<0.5) add.push([j, v===1&&steps>4&&Math.random()<0.002?2:v]); } }
    for(const [j,v] of add) if(!G[j]) G[j]=v;
    steps++;
    let edge=false; for(let k=0;k<W;k++){ if(G[k]||G[(H-1)*W+k]||G[k*W]||G[k*W+W-1]){ edge=true; break; } }
    if(edge) hold=3;
  }
  reset();
  function frameFn(dt){
    if(hold>0){ hold-=dt; if(hold<=0) reset(); }
    else { acc+=dt; while(acc>0.06&&hold<=0){ acc-=0.06; step(); } }
    drawGrid(c,G,W,H,[null,col('accent'),col('accent2')]);
    let mut=0, n=0; for(const v of G){ if(v) n++; if(v===2) mut++; }
    lbl.textContent=`stap ${steps} · ${n} cellen, waarvan ${mut} mutant`;
  }
  return {frame:frameFn};
})();

/* =========================================================
   WIE ZIET WIE? interaction networks: well-mixed vs grid
   ========================================================= */
const netwerk=(()=>{
  const cL=$('#nw-ode'), cR=$('#nw-grid'), segN=$('#nw-n'), segK=$('#nw-k');
  const SIZES=[3,5,8], KS=[{k:4,l:'4 (boven, onder, links, rechts)'},{k:8,l:'8 (ook diagonaal)'}];
  const D4=[[1,0],[-1,0],[0,1],[0,-1]], D8=[...D4,[1,1],[1,-1],[-1,1],[-1,-1]];
  let n=5, nb=4, sel=12, auto=true, tA=0, ph=0, posL=[], posR=[];
  SIZES.forEach(v=>{ const b=document.createElement('button'); b.textContent=v*v; b.dataset.v=v; b.onclick=()=>{ n=v; sel=Math.floor(n*n/2); auto=true; tA=0; mark(); }; segN.appendChild(b); });
  KS.forEach(o=>{ const b=document.createElement('button'); b.textContent=o.l; b.dataset.v=o.k; b.onclick=()=>{ nb=o.k; mark(); }; segK.appendChild(b); });
  function mark(){ [...segN.children].forEach(b=>b.setAttribute('aria-pressed',+b.dataset.v===n)); [...segK.children].forEach(b=>b.setAttribute('aria-pressed',+b.dataset.v===nb)); }
  mark();
  const nbrs=i=>{ const x=i%n, y=(i/n)|0, out=[]; for(const [dx,dy] of (nb===4?D4:D8)){ const a=x+dx, b=y+dy; if(a>=0&&b>=0&&a<n&&b<n) out.push(b*n+a); } return out; };
  function pick(e,c,pos){ const [px,py]=localXY(e,c); let best=-1, bd=1e9; pos.forEach(([x,y],i)=>{ const d=(x-px)**2+(y-py)**2; if(d<bd){ bd=d; best=i; } }); if(best>=0&&bd<900){ sel=best; auto=false; } }
  cL.addEventListener('click',e=>pick(e,cL,posL)); cR.addEventListener('click',e=>pick(e,cR,posR));
  function nodes(ctx,pos,partners,r){
    pos.forEach(([x,y],i)=>{ const isS=i===sel, isP=partners.has(i);
      disc(ctx,x,y,isS?r*1.35:r,isS?col('accent'):isP?col('accent2'):col('card'),isS||isP?col('bg'):col('axis'),isS||isP?1.5:1.2); });
    const [x,y]=pos[sel]; ctx.save(); ctx.globalAlpha=0.5*(1-(ph%1)); disc(ctx,x,y,r*1.35+10*(ph%1),null,col('accent'),2); ctx.restore();
  }
  function frameFn(dt){
    ph+=dt*0.8; const N=n*n;
    if(auto){ tA+=dt; if(tA>2.4){ tA=0; let s2; do{ s2=Math.floor(Math.random()*N); }while(s2===sel&&N>1); sel=s2; } }
    if(sel>=N) sel=0;
    const r=Math.max(3.5,Math.min(11,90/n));
    // left: everyone connected
    { const {ctx,w,h}=fitCanvas(cL); ctx.clearRect(0,0,w,h); const cx=w/2, cy=h/2, R=Math.min(w,h)*0.42;
      posL=[]; for(let i=0;i<N;i++){ const a=-Math.PI/2+2*Math.PI*i/N; posL.push([cx+R*Math.cos(a),cy+R*Math.sin(a)]); }
      ctx.lineWidth=0.8; ctx.strokeStyle=col('muted'); ctx.globalAlpha=N>40?0.08:0.18; ctx.beginPath();
      for(let i=0;i<N;i++) for(let j=i+1;j<N;j++){ if(i===sel||j===sel) continue; ctx.moveTo(...posL[i]); ctx.lineTo(...posL[j]); } ctx.stroke();
      ctx.globalAlpha=0.9; ctx.strokeStyle=col('accent2'); ctx.lineWidth=1.6; ctx.beginPath();
      for(let j=0;j<N;j++) if(j!==sel){ ctx.moveTo(...posL[sel]); ctx.lineTo(...posL[j]); } ctx.stroke(); ctx.globalAlpha=1;
      const all=new Set([...Array(N).keys()].filter(j=>j!==sel)); nodes(ctx,posL,all,r); }
    // right: grid neighbours
    const part=new Set(nbrs(sel));
    { const {ctx,w,h}=fitCanvas(cR); ctx.clearRect(0,0,w,h); const m=Math.min(w,h), step=m*0.84/(n-1||1), ox=(w-step*(n-1))/2, oy=(h-step*(n-1))/2;
      posR=[]; for(let i=0;i<N;i++) posR.push([ox+(i%n)*step, oy+((i/n)|0)*step]);
      ctx.lineWidth=1; ctx.strokeStyle=col('muted'); ctx.globalAlpha=0.35; ctx.beginPath();
      for(let i=0;i<N;i++) for(const j of nbrs(i)) if(j>i&&i!==sel&&j!==sel){ ctx.moveTo(...posR[i]); ctx.lineTo(...posR[j]); } ctx.stroke();
      ctx.globalAlpha=0.9; ctx.strokeStyle=col('accent2'); ctx.lineWidth=2; ctx.beginPath();
      for(const j of part){ ctx.moveTo(...posR[sel]); ctx.lineTo(...posR[j]); } ctx.stroke(); ctx.globalAlpha=1;
      nodes(ctx,posR,part,r); }
    const eGrid=nb===4?2*n*(n-1):2*n*(n-1)+2*(n-1)*(n-1), eOde=N*(N-1)/2, edge=part.size<nb;
    const txt=`Het oranje individu kan in de <b>ODE</b> met alle <b>${N-1}</b> anderen interacteren. Op het <b>grid</b> zijn dat er maar <b>${part.size}</b>${edge?' (aan de rand heb je minder buren)':''}. `+
      `In totaal zijn er ${eOde} mogelijke contacten in de ODE, tegen ${eGrid} op het grid. `+
      (n===SIZES[0]?'Kies meer individuen: in de ODE groeit het aantal partners mee met N, op het grid blijft het hooguit '+nb+'.':
       'Daarom telt in een ODE alleen de totale dichtheid N, en op een grid de <b>lokale</b> dichtheid: wie er naast je zit. Zo kan een cel aan de rand van een kolonie nog groeien, terwijl een cel in het midden vastzit.');
    const el=$('#nw-text'); if(el.__last!==txt){ el.innerHTML=txt; el.__last=txt; }
  }
  return {frame:frameFn};
})();

/* =========================================================
   GROEIEN IN LEGE HOKJES (13.4)
   ========================================================= */
const hokjes=(()=>{
  const W=32, H=32, TMAX=300, SPS=18;
  const gc=$('#ih-grid'), nc=$('#ih-n'), bc=$('#ih-b'), sb=$('#ih-sb'), sd=$('#ih-sd'), btn=$('#ih-play'), toast=$('#ih-toast'), pre=$('#ih-presets');
  const PRESETS=[{l:'a: b = 0.6, d = 0.1',b:0.6,d:0.1},{l:'b: b = 0.3, d = 0.1',b:0.3,d:0.1},{l:'c: b = 0.12, d = 0.1',b:0.12,d:0.1}];
  let G, b, d, t, Ns, Bs, old=[], playing=false, acc=0, toastT=null;
  PRESETS.forEach((p,k)=>{ const e=document.createElement('button'); e.innerHTML=p.l.replace(/^(\w):/,'<span class="nomath">$1:</span>'); e.onclick=()=>{ if(playing){ lockMsg(); return; } sb.value=p.b; sd.value=p.d; old=[]; reset(); }; pre.appendChild(e); });
  function markPreset(){ [...pre.children].forEach((e,k)=>e.setAttribute('aria-pressed',Math.abs(PRESETS[k].b-b)<1e-9&&Math.abs(PRESETS[k].d-d)<1e-9)); }
  function readP(){ b=+sb.value; d=+sd.value; $('#ih-b-v').textContent=b.toFixed(2)+' per stap'; $('#ih-d-v').textContent=d.toFixed(2)+' per stap'; markPreset(); }
  function setPlaying(v){ playing=v; btn.textContent=v?'⏸ Pauze':'▶ Start'; [sb,sd].forEach(x=>x.closest('.ctl').classList.toggle('locked',v)); }
  function reset(){ readP(); G=new Uint8Array(W*H); G[6*W+6]=1; G[6*W+7]=2; t=0; Ns=[[0,2]]; Bs=[]; setPlaying(false); }
  function lockMsg(){ toast.innerHTML='🔒 <b>De regels liggen vast tijdens een simulatie.</b> Pauzeer eerst: met een nieuwe waarde begint een nieuwe simulatie.';
    toast.classList.add('show'); clearTimeout(toastT); toastT=setTimeout(()=>toast.classList.remove('show'),4000); }
  [sb,sd].forEach(x=>{
    const block=e=>{ if(!playing) return; e.preventDefault(); e.stopPropagation(); lockMsg(); };
    x.addEventListener('pointerdown',block,true); x.addEventListener('touchstart',block,{capture:true,passive:false}); x.addEventListener('keydown',block,true);
    x.addEventListener('input',()=>{ if(playing){ sb.value=b; sd.value=d; lockMsg(); return; } old=[]; reset(); });
  });
  btn.onclick=()=>{ if(t>=TMAX||count()===0) { keepOld(); reset(); } setPlaying(!playing); };
  $('#ih-again').onclick=()=>{ keepOld(); reset(); setPlaying(true); };
  function keepOld(){ if(Ns.length>1){ old.push(Ns); if(old.length>6) old.shift(); } }
  function count(){ let n=0; for(const v of G) if(v) n++; return n; }
  function step(){
    const cells=[]; for(let i=0;i<W*H;i++) if(G[i]) cells.push(i);
    for(let k=cells.length-1;k>0;k--){ const j=Math.floor(Math.random()*(k+1)); [cells[k],cells[j]]=[cells[j],cells[k]]; }
    let births=0;
    for(const i of cells){ if(Math.random()>=b) continue; const [dx,dy]=NB4[Math.floor(Math.random()*4)];
      const x=i%W+dx, y=((i/W)|0)+dy; if(x<0||y<0||x>=W||y>=H) continue; const j=y*W+x;
      if(!G[j]){ G[j]=G[i]; births++; } }
    for(const i of cells) if(Math.random()<d) G[i]=0;
    t++; const n=count(); Ns.push([t,n]); Bs.push([t,cells.length?births/cells.length:NaN]);
    if(t>=TMAX||n===0) setPlaying(false);
  }
  reset();
  function frameFn(dt){
    if(playing){ acc+=dt; while(acc>1/SPS&&playing){ acc-=1/SPS; step(); } } else acc=0;
    drawGrid(gc,G,W,H,[null,col('accent'),col('accent2')]);
    const N=count();
    let ym=10; for(const o of [Ns,...old]) for(const q of o) ym=Math.max(ym,q[1]);
    const f=frame(nc,{xmin:0,xmax:TMAX,ymin:0,ymax:Math.min(W*H,niceCeil(ym*1.15)),xlab:'tijdstap',ylab:'N',ny:4,pad:{b:34}});
    for(const o of old) poly(f,o,col('muted'),1.4,null,.45);
    poly(f,Ns,col('fg'),2.2); f.ctx.restore();
    const g=frame(bc,{xmin:0,xmax:TMAX,ymin:0,ymax:1,xlab:'tijdstap',ylab:'per cel',ny:4,pad:{b:34}});
    poly(g,[[0,b],[TMAX,b]],col('muted'),1.6,[6,4]);
    poly(g,[[0,d],[TMAX,d]],col('accent2'),1.6,[2,3]);
    g.ctx.fillStyle=col('muted'); g.ctx.textAlign='right'; g.ctx.textBaseline='bottom'; drawMath(g.ctx,'b',g.X(TMAX)-4,g.Y(b)-2);
    g.ctx.fillStyle=col('accent2'); g.ctx.textBaseline=d>b-0.08?'top':'bottom'; drawMath(g.ctx,'d',g.X(TMAX)-4,g.Y(d)+(d>b-0.08?2:-2));
    const sm=[]; for(let k=0;k<Bs.length;k++){ let s=0,c=0; for(let j=Math.max(0,k-4);j<=k;j++){ if(isFinite(Bs[j][1])){ s+=Bs[j][1]; c++; } } sm.push([Bs[k][0],c?s/c:NaN]); }
    poly(g,sm,col('accent'),2.2); g.ctx.restore();
    const full=Math.round(N/(W*H)*100), last=sm.length?sm[sm.length-1][1]:NaN;
    let txt;
    if(N===0) txt=`<b>Uitgestorven na ${t} stappen!</b> `+(b>d?'Door pech stierven de laatste cellen, ook al is b > d. In een ODE met b > d kan dat niet. '+(b<0.2?'Klik een paar keer op <b>Nieuwe herhaling</b>: soms overleeft de populatie wél.':'Probeer een paar herhalingen met voorbeeld c.'):'Omdat d ≥ b sterven er gemiddeld meer cellen dan er bijkomen.');
    else if(t===0) txt='Druk op <b>Start</b>. We beginnen met twee cellen: een oranje en een blauwe.';
    else { txt=`Stap ${t}: N = ${N}, het grid is <b>${full}%</b> vol. De effectieve geboortekans is nu ongeveer <b>${isFinite(last)?last.toFixed(2):'–'}</b> per cel, terwijl b = ${b.toFixed(2)}. `;
      const ratio=isFinite(last)?last/b:1;
      if(ratio>0.7) txt+='Bijna elke poging lukt nog: de buurhokjes zijn grotendeels leeg.';
      else if(full<50) txt+=`Het grid is pas ${full}% vol, maar de effectieve geboortekans is al veel lager dan b. Cellen zitten in een kluitje naast hun eigen dochters, dus de <b>lokale</b> dichtheid is hoog: alleen cellen aan de rand van een kluitje kunnen nog groeien.`;
      else txt+='Hoe voller het grid, hoe vaker een dochter op een bezet hokje zou landen. Zo ontstaat een draagvermogen, zonder dat er een K in de regels staat.';
      if(t>60&&ratio<0.7) txt+=' Kijk ook waar de oranje lijn naartoe gaat: als N niet meer verandert, zijn er per stap evenveel geboortes als sterfgevallen, dus is de effectieve geboortekans gelijk aan d.';
      if(t>=TMAX) txt+=' Klik op <b>Nieuwe herhaling</b>: door toeval verloopt elke simulatie anders.'; }
    const el=$('#ih-text'); if(el.__last!==txt){ el.innerHTML=txt; el.__last=txt; }
  }
  return {frame:frameFn};
})();

/* =========================================================
   QUIZ
   ========================================================= */
function gridSVG(){
  const n=7, s=26, occ=new Set(['2,2','2,3','2,4','3,2','3,3','3,4','4,2','4,3','4,4','1,3']);
  const lab={'1,3':'A','3,3':'B','3,4':'C'};
  let r='';
  for(let y=0;y<n;y++) for(let x=0;x<n;x++){ const k=y+','+x, on=occ.has(k);
    r+=`<rect x="${x*s+1}" y="${y*s+1}" width="${s-2}" height="${s-2}" rx="3" style="fill:${on?'var(--accent)':'transparent'};stroke:var(--line)"/>`;
    if(lab[k]) r+=`<text x="${x*s+s/2}" y="${y*s+s/2+5}" text-anchor="middle" style="font-size:14px;font-weight:700;fill:#fff">${lab[k]}</text>`; }
  return `<svg viewBox="0 0 ${n*s} ${n*s}" width="${n*s}" style="display:block;margin:8px 0" role="img" aria-label="grid met cellen A, B en C">${r}</svg>`;
}
const quiz=(()=>{
  const L=x=>`<span class="nomath">${x}</span>`;
  const BASE=[
    {q:'Wat is het <b>omschrijvingsniveau</b> van een individual-based model?',
     o:['Het individu','De populatie','De soort','Het ecosysteem'], a:0,
     h:'Wat beschrijft een IBM, en wat beschrijft een ODE?',
     e:'Een IBM beschrijft losse individuen met eigen eigenschappen. Een ODE beschrijft de populatie als geheel, met één getal N.'},
    {q:'Welke uitkomst kan een ODE-model voor N <b>niet</b> laten zien?',
     o:['Dat cellen aan de rand sneller groeien','Dat de populatie een draagvermogen bereikt','Dat de populatie eerst exponentieel groeit','Dat de populatie naar een evenwicht gaat'], a:0,
     h:'Wat weet een ODE over individuen en hun posities?',
     e:'Een ODE kent alleen de populatiegrootte N, geen posities. Dat cellen aan de rand van een kolonie sneller groeien dan cellen in het midden, kan alleen een model met ruimtelijke structuur laten zien. Het omschrijvingsniveau bepaalt welke modeluitkomsten mogelijk zijn.'},
    {q:'In het lege-hokjes-model hangt de geboortekans b niet af van de dichtheid. Waarom bereikt de populatie toch een draagvermogen?',
     o:['Omdat een dochter alleen in een leeg buurhokje terecht kan','Omdat de sterftekans d stijgt naarmate het grid voller raakt','Omdat het draagvermogen K in de regels van het model staat','Omdat cellen met dezelfde kleur elkaars groei remmen'], a:0,
     h:'Wat gebeurt er als het gekozen buurhokje al bezet is?',
     e:'Hoe voller het grid, hoe vaker het gekozen buurhokje al bezet is en hoe minder geboortes lukken. De effectieve geboortekans daalt dus naarmate het grid voller raakt. Zo ontstaat een draagvermogen zonder dat je het in de regels hoeft te stoppen.'},
    {q:'Elke cel probeert zich met dezelfde kans b voort te planten naar een willekeurig buurhokje (boven, onder, links, rechts). Welke cel krijgt deze stap het waarschijnlijkst een dochter?'+gridSVG(),
     o:['Cel '+L('A'),'Cel '+L('B'),'Cel '+L('C'),'Ze hebben allemaal evenveel kans'], a:0,
     h:'Tel bij elke cel hoeveel van de vier buurhokjes leeg zijn.',
     e:`Cel ${L('A')} heeft 3 lege buurhokjes, cel ${L('C')} heeft er 1 en cel ${L('B')} geen enkele. De regels zijn voor iedereen gelijk, maar de positie bepaalt de effectieve geboortekans. Daarom groeit een kolonie aan de rand.`},
    {q:'We starten met 2 cellen, met b = 0.12 en d = 0.1. Wat kan er in het IBM gebeuren dat in een ODE met b &gt; d onmogelijk is?',
     o:['De populatie sterft door pech uit','De populatie groeit onbeperkt','De populatie bereikt een draagvermogen','De populatie blijft precies gelijk'], a:0,
     h:'Wat betekent het dat geboorte en sterfte kansen zijn?',
     e:'Door stochasticiteit kunnen de eerste cellen sterven voordat ze zich voortplanten. In een ODE met b > d groeit elke populatie groter dan 0.'},
    {q:'Is ruimte altijd de beperkende factor voor groei?',
     o:['Alleen in sommige biologische scenario\'s','Ja, ruimte is in elk biologisch scenario beperkend','Nee, in geen enkel biologisch scenario','Alleen in een ODE-model, niet in een IBM'], a:0,
     h:'Denk aan een boom en aan een koe.',
     e:'Het hangt af van de biologie. Voor planten die licht nodig hebben is ruimte wel beperkend, voor grazers waarschijnlijk niet: die kunnen dicht op elkaar staan zolang er genoeg voedsel is.'},
    {q:'Wat is een <i>cheater</i> (het blauwe type) in het samenwerkingsmodel?',
     o:['Een individu dat hulp krijgt maar niet geeft','Een individu dat hulp geeft maar niet krijgt','Een individu dat hulp geeft én krijgt','Een individu dat geen hulp geeft of krijgt'], a:0,
     h:'Kijk naar de formule: van wie krijgt blauw hulp, en wie helpt blauw?',
     e:'Blauw profiteert van rode én oranje buren, maar geeft zelf geen hulp. In het Engels heten zulke types <i>cheaters</i> of <i>defectors</i>.'},
    {q:'Wat gebeurt er met de samenwerkers en de cheaters als alle individuen na elke stap willekeurig worden <b>gemengd</b>?',
     o:['De cheaters nemen het over','De samenwerkers nemen het over','Er ontstaat een cyclus tussen beide','Er verandert niets aan de uitkomst'], a:0,
     h:'Zonder ruimtelijke structuur kunnen samenwerkers niet meer bij elkaar blijven.',
     e:'Door het mengen krijgen cheaters overal hulp en kunnen samenwerkers geen eigen plekjes meer vormen. Of daarna alles uitsterft, hangt af van de precieze parameters. Zonder mengen ontstaat juist een cyclus: cheaters verdringen samenwerkers, sterven lokaal uit, en samenwerkers heroveren de ruimte.'}
  ];
  const box=$('#qz-box'), prog=$('#qz-prog'), cnt=$('#qz-count'), sc=$('#qz-score'), next=$('#qz-next');
  let Q=[], i=0, score=0, S=[]; const KEY='bioms-quiz-'+qhash(BASE);
  const shuffle=a=>{ a=a.slice(); for(let k=a.length-1;k>0;k--){ const j=Math.floor(Math.random()*(k+1)); [a[k],a[j]]=[a[j],a[k]]; } return a; };
  function prep(it,idx){ idx=idx||shuffle(it.o.map((_,k)=>k)); return {...it,o:idx.map(k=>it.o[k]),a:idx.indexOf(it.a),perm:idx}; }
  function save(){ store.set(KEY,{i,score,perm:Q.map(q=>q.perm),st:S}); }
  function start(fresh){ const sv=fresh?null:store.get(KEY);
    if(sv&&Array.isArray(sv.perm)&&sv.perm.length===BASE.length&&Array.isArray(sv.st)){ Q=BASE.map((it,k)=>prep(it,sv.perm[k])); i=sv.i; score=sv.score; S=sv.st; }
    else { Q=BASE.map(it=>prep(it)); i=0; score=0; S=BASE.map(newState); save(); }
    show(); }
  /* scoreboard: one dot per question */
  const board=document.createElement('div'); board.className='qboard'; prog.parentNode.after(board);
  function drawBoard(){
    let front=S.findIndex(st=>!st.solved); if(front<0) front=S.length;   // first question not yet solved
    board.innerHTML=S.map((st,k)=>{ const c=(st.solved?(st.first?'ok':'retry'):(k===i?'cur':''))+(k===i?' here':'');
      const tip=`Vraag ${k+1}${st.solved?(st.first?': in één keer goed':': goed na een nieuwe poging'):''}`;
      return `<button type="button" class="qdot ${c}" data-k="${k}" title="${tip}" aria-label="${tip}" ${k>front?'disabled':''}>${st.solved?(st.first?'✓':'↻'):k+1}</button>`; }).join('')+
      (front===S.length?`<button type="button" class="qdot fin${i>=S.length?' here':''}" data-k="${S.length}" title="Resultaat" aria-label="Resultaat">🏁</button>`:'')+
      `<button class="qreset" type="button">↺ Begin opnieuw</button>`;
    board.querySelectorAll('.qdot').forEach(b=>b.onclick=()=>{ if(b.disabled) return; i=+b.dataset.k; save(); show(); });
    const tb=$('.tab[data-tab="quiz"]'), nd=S.filter(st=>st.solved).length; if(tb) tb.innerHTML='Quiz ✓'+(nd?` <span class="tabcount">${nd}/${S.length}</span>`:'');
    board.querySelector('.qreset').onclick=()=>{ if(confirm('Weet je zeker dat je opnieuw wilt beginnen? Je antwoorden worden gewist.')) start(true); }; }
  function show(){
    box.innerHTML=''; next.hidden=true; drawBoard();
    if(i>=Q.length) return end();
    cnt.textContent=`Vraag ${i+1} van ${Q.length}`; sc.textContent=`${score} in één keer goed`;
    prog.style.width=(i/Q.length*100)+'%';
    const solvedUI=()=>{ prog.style.width=((i+1)/Q.length*100)+'%'; next.textContent=i===Q.length-1?'Bekijk resultaat →':'Volgende vraag →'; next.hidden=false; };
    const el=renderQuestion(Q[i],null,first=>{ if(first) score++; sc.textContent=`${score} in één keer goed`; solvedUI(); save(); drawBoard(); }, S[i], ()=>save());
    if(S[i].solved) solvedUI();
    el.classList.add('slidein'); box.appendChild(el); postHeight();
  }
  function end(){
    prog.style.width='100%'; cnt.textContent='Klaar!';
    const frac=score/Q.length;
    const msg=frac===1?'Perfect! Je snapt hoe individuen, ruimte en toeval samenwerken.':frac>=.75?'Heel goed! Kijk nog even naar de vragen die je niet in één keer goed had.':frac>=.5?'Op de goede weg. Speel nog eens met de lege hokjes.':'Lees het hoofdstuk nog eens door en speel met de simulatie. Daarna gaat het vast beter!';
    box.innerHTML=`<div class="card endcard slidein"><div class="bigmark">${frac>=.5?MARK_OK:MARK_NO}</div><h3>${score} van ${Q.length} in één keer goed</h3><p>${msg}</p><button class="b primary" id="qz-again">↺ Opnieuw</button></div>`;
    if(frac>=.5) burst(box.querySelector('.bigmark'));
    $('#qz-again').onclick=()=>start(true); postHeight();
  }
  next.onclick=()=>{ i++; save(); show(); };
  start();
  return {hasProgress:()=>S.some(st=>st.solved||st.wrong.length)};
})();

/* ---------- italicize math in all (dynamic) text ---------- */
const MROOT=root.querySelector('.app');
mathify(MROOT);
const mobs=new MutationObserver(ms=>{ for(const m of ms){ if(m.type==='characterData') mathify(m.target); else m.addedNodes.forEach(mathify); } mobs.takeRecords(); });
mobs.observe(MROOT,{childList:true,subtree:true,characterData:true});

/* ---------- tabs & loop ---------- */
const modules={overzicht:hero,netwerk:netwerk,hokjes:hokjes,quiz:quiz};
let current='overzicht';
const TABKEY='bioms-tab-ruim';   // the last opened tab is remembered in this browser
function showTab(id){ if(!modules[id]) id='overzicht'; current=id;
  root.querySelectorAll('.tab').forEach(b=>b.setAttribute('aria-selected',b.dataset.tab===id));
  root.querySelectorAll('.panel').forEach(p=>p.hidden=p.id!=='tab-'+id);
  if(params.get('hidenav')!=='1') store.set(TABKEY,id);
  postHeight(); }
root.querySelectorAll('.tab').forEach(b=>b.onclick=()=>showTab(b.dataset.tab));
root.querySelectorAll('[data-go]').forEach(b=>b.onclick=()=>{ showTab(b.dataset.go); if(host.getBoundingClientRect().top<0) host.scrollIntoView({behavior:'smooth'}); });
if(params.get('hidenav')==='1') $('nav.tabs').hidden=true;
showTab(params.get('tab')||store.get(TABKEY)||'overzicht');
let last=performance.now();
let visible=true; new IntersectionObserver(es=>{ visible=es[0].isIntersecting; },{rootMargin:'100px'}).observe(host);
function loop(now){ const dt=Math.min(0.05,(now-last)/1000); last=now; const m=modules[current]; if(visible&&m&&m.frame&&!document.hidden) m.frame(dt); requestAnimationFrame(loop); }
requestAnimationFrame(loop);
}
function init(){ document.querySelectorAll('[data-ruim-app]').forEach(mount); }
window.RuimApp={mount,init};
const H0=inlineHost(CS); if(H0) mount(H0);
if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();