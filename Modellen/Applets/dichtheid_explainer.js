/* ODE-explainer (Biologische Modellen en Statistiek, hoofdstuk 12).
 * Gebruik in Quarto (één regel, op de plek waar de app moet komen):
 *   <script src="Applets/dichtheid_explainer.js"></script>                        -> hele app met tabbladen
 *   <script src="Applets/dichtheid_explainer.js" data-tab="evenwicht"></script>   -> alleen dat onderdeel
 *   <script src="Applets/dichtheid_explainer.js" data-tab="evenwicht" data-nav="1"></script> -> tabbladen, start op dat tabblad
 * data-tab: overzicht | snelheid | quiz
 * data-theme="dark|light" forceert een thema; anders volgt de app het boek (ook de donkere modus).
 * (Alternatief: <div data-dicht-app data-tab="..."></div> plus het script één keer op de pagina.)
 */
(function(){
"use strict";
const CS=document.currentScript;
function inlineHost(cs){ if(!cs||!cs.dataset||cs.__appDone||!cs.parentNode) return null;
  const single=!!cs.dataset.tab;
  // a plain <script> with no data-tab becomes the full app with tabs, unless the page already uses <div data-dicht-app>
  if(!single&&document.querySelector('[data-dicht-app]')) return null;
  cs.__appDone=true;
  const d=document.createElement('div'); d.setAttribute('data-dicht-app','');
  d.dataset.tab=cs.dataset.tab||'overzicht';
  if(single&&cs.dataset.nav!=='1') d.dataset.hidenav='1'; if(cs.dataset.theme) d.dataset.theme=cs.dataset.theme;
  cs.parentNode.insertBefore(d,cs); return d; }
if(window.DichtApp){ const h=inlineHost(CS); if(h) window.DichtApp.mount(h); window.DichtApp.init(); return; }
const STYLE="\n\n:host{\n  --bg: var(--bs-body-bg, #ffffff);\n  --fg: var(--bs-body-color, #1d2327);\n  --line: var(--bs-border-color, #d9e0dc);\n  --card: color-mix(in srgb, var(--fg) 4%, var(--bg));\n  --muted: color-mix(in srgb, var(--fg) 62%, var(--bg));\n  --grid: color-mix(in srgb, var(--fg) 9%, var(--bg));\n  --axis: color-mix(in srgb, var(--fg) 50%, var(--bg));\n  --accent:#D55E00; --accent-soft:#D55E0022; --accent2:#0072B2; --blue:#0072B2; --warn:#555b61;\n  --font: var(--bs-body-font-family, system-ui,-apple-system,\"Segoe UI\",Roboto,\"Helvetica Neue\",sans-serif);\n  --mono: var(--bs-font-monospace, ui-monospace,SFMono-Regular,Menlo,Consolas,monospace);\n  display:block; color:var(--fg); font-family:var(--font); font-size:.95em; line-height:1.5; margin:1rem 0;\n}\n:host([data-dark]){\n  --bg: var(--bs-body-bg, #16191b);\n  --fg: var(--bs-body-color, #e6e9e7);\n  --line: var(--bs-border-color, #30383c);\n  --accent:#F08A3E; --accent-soft:#F08A3E2B; --accent2:#56B4E9; --blue:#56B4E9; --warn:#aab2b8;\n}\n.app{color:var(--fg)}\n.ovb{display:inline-block;border-top:1.5px solid currentColor;line-height:1.05;padding-top:1px}\n*{box-sizing:border-box}\nh2{font-size:1.15em;margin:.2em 0 .4em;border:0;padding:0}\nh3{font-size:1em;margin:1em 0 .3em}\np{margin:.35rem 0 .6rem}\nb{font-weight:600}\n.muted{color:var(--muted)}\n.mono{font-family:var(--mono)}\n.eqn,.formula{font-family:\"Latin Modern Math\",\"STIX Two Math\",\"Cambria Math\",\"Times New Roman\",Georgia,serif;font-size:1.06em;white-space:nowrap}\n.mi{font-style:italic}\n.mu{font-style:normal}\n/* tabs */\nnav.tabs{display:flex;gap:4px;flex-wrap:wrap;border-bottom:1px solid var(--line);margin-bottom:12px}\n.tab{appearance:none;border:0;background:none;color:var(--muted);font:inherit;font-size:.92rem;padding:7px 11px;border-radius:8px 8px 0 0;cursor:pointer;border-bottom:2px solid transparent;margin-bottom:-1px}\n.tab:hover{color:var(--fg)}\n.tab[aria-selected=\"true\"]{color:var(--accent);border-bottom-color:var(--accent);font-weight:600}\n/* layout */\n.grid2{display:grid;grid-template-columns:1fr 1fr;gap:14px}\n@media (max-width:680px){.grid2{grid-template-columns:1fr}}\n.card{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:10px 12px}\ncanvas{display:block;width:100%;border-radius:8px}\n.plot{aspect-ratio:16/10}\n.plot-wide{aspect-ratio:16/8}\n@media (max-width:680px){.plot-wide{aspect-ratio:16/10}}\n.controls{position:relative;display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:6px 16px;margin:8px 0}\n.ctl label{display:flex;justify-content:space-between;font-size:.88rem;color:var(--muted)}\n.ctl label span{color:var(--fg);font-family:var(--mono);font-size:.85rem}\n.ctl label span.lbl{color:var(--muted);font-family:var(--font);font-size:.88rem}\ninput[type=range]{width:100%;accent-color:var(--accent)}\n.btns{display:flex;gap:6px;flex-wrap:wrap;align-items:center;margin:6px 0}\nbutton.b{appearance:none;font:inherit;font-size:.88rem;border:1px solid var(--line);background:var(--card);color:var(--fg);padding:5px 12px;border-radius:7px;cursor:pointer}\nbutton.b:hover{border-color:var(--accent)}\nbutton.b:disabled{opacity:.5;cursor:default}\nbutton.b.primary{background:var(--accent);border-color:var(--accent);color:#fff}\n.chk{font-size:.88rem;color:var(--muted);display:inline-flex;gap:5px;align-items:center;margin-right:10px}\n.chk input{accent-color:var(--accent)}\n.seg{display:inline-flex;border:1px solid var(--line);border-radius:8px;overflow:hidden;flex-wrap:wrap}\n.seg button{appearance:none;font:inherit;font-size:.86rem;border:0;background:none;color:var(--muted);padding:5px 10px;cursor:pointer}\n.seg button[aria-pressed=\"true\"]{background:var(--accent);color:#fff}\n.callout{border-left:3px solid var(--accent);background:var(--accent-soft);padding:7px 11px;border-radius:0 8px 8px 0;margin:8px 0}\n.callout.warn{border-left-color:var(--accent2);background:color-mix(in srgb,var(--accent2) 13%,transparent)}\n.formula{font-size:1.06em;display:inline-block;margin:2px 0}\n/* overview */\n.cards{display:grid;grid-template-columns:repeat(auto-fill,minmax(230px,1fr));gap:10px;margin-top:8px}\n.ov{display:flex;flex-direction:column;gap:4px}\n.ov .num{font-size:.75rem;color:var(--muted);font-family:var(--mono)}\n.ov h4{margin:0;font-size:.95rem}\n.ov p{margin:0;font-size:.88rem;flex:1}\n.ov button{align-self:flex-start;margin-top:6px}\ntable.cmp{border-collapse:collapse;width:100%;font-size:.88rem;margin:6px 0}\ntable.cmp td,table.cmp th{border-bottom:1px solid var(--line);padding:5px 6px;text-align:left;vertical-align:top}\ntable.cmp th{color:var(--muted);font-weight:600}\ndetails{margin-top:10px}\nsummary{cursor:pointer;color:var(--accent);font-weight:600}\n/* flows */\n.flows{display:grid;grid-template-columns:auto 1fr auto;gap:4px 8px;align-items:center;font-size:.86rem;margin-top:8px}\n.bar{height:10px;border-radius:5px;background:var(--line);overflow:hidden}\n.bar i{display:block;height:100%;width:0;border-radius:5px;transition:width .08s linear}\n.legend{display:flex;gap:12px;font-size:.8rem;color:var(--muted);margin-top:4px;flex-wrap:wrap}\n.dot{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:4px;vertical-align:-1px}\n/* units */\n.eq{display:flex;align-items:flex-start;justify-content:center;gap:10px;flex-wrap:wrap;margin:14px 0 6px}\n.term{display:flex;flex-direction:column;align-items:center;gap:6px;min-width:90px}\n.term .sym{height:2.6em;display:flex;align-items:center;font-size:1.45rem;font-family:\"Cambria Math\",\"STIX Two Math\",Georgia,serif;padding:4px 12px;border-radius:8px;border:2px solid transparent;transition:all .35s}\n.term.hl .sym{border-color:var(--accent);background:var(--accent-soft)}\n.term .unit{min-height:46px;font-size:.92rem;color:var(--accent2);opacity:0;transform:translateY(-6px);transition:all .45s;text-align:center}\n.term .unit.show{opacity:1;transform:none}\n.op{font-size:1.45rem;height:2.6em;display:flex;align-items:center;font-family:Georgia,serif}\n.frac{display:inline-flex;flex-direction:column;align-items:center;vertical-align:middle;line-height:1.15;margin:0 2px}\n.frac>span:first-child{border-bottom:1.5px solid currentColor;padding:0 3px}\n.frac>span:last-child{padding:0 3px}\n.derive{min-height:120px;display:flex;flex-direction:column;align-items:center;gap:8px;margin:6px 0}\n.derive .ln{opacity:0;animation:fadein .5s forwards;font-size:1.05rem;display:flex;align-items:center;gap:8px;flex-wrap:wrap;justify-content:center}\n@keyframes fadein{from{opacity:0;transform:translateY(6px)}to{opacity:1;transform:none}}\n.cancel{position:relative;display:inline-block}\n.cancel::after{content:\"\";position:absolute;left:-2px;top:50%;height:2px;background:var(--warn);width:0;animation:strike .5s forwards;animation-delay:inherit;transform:rotate(-12deg)}\n@keyframes strike{to{width:calc(100% + 4px)}}\n.stepper{display:flex;justify-content:center;align-items:center;gap:10px;margin-top:6px}\n.dots{display:flex;gap:5px}\n.dots i{width:8px;height:8px;border-radius:50%;background:var(--line);cursor:pointer}\n.dots i.on{background:var(--accent)}\n.steptext{min-height:4.5em;text-align:center;max-width:620px;margin:6px auto}\n.term .unit.same{background:var(--accent-soft);border:1.5px solid var(--accent);border-radius:8px;padding:2px 8px;color:var(--fg);animation:same .9s ease-out}\n@keyframes same{0%{transform:scale(1)}40%{transform:scale(1.12)}100%{transform:scale(1)}}\n/* quiz */\n.q{margin:10px 0;padding:10px 12px}\n.q .opts{display:flex;flex-wrap:wrap;gap:6px;margin:6px 0}\n.q .opts button{appearance:none;font:inherit;font-size:.92rem;border:1px solid var(--line);background:var(--bg);color:var(--fg);padding:5px 12px;border-radius:7px;cursor:pointer;min-height:38px}\n.q .opts button.right{border-color:var(--accent);background:var(--accent-soft)}\n.q .opts button.wrong{border-color:var(--warn);opacity:.7}\n.q .fb{font-size:.88rem;display:none}\n.q .fb.show{display:block}\n.score{font-size:.9rem;color:var(--muted)}\n/* animated marks */\n.qhead{display:flex;gap:10px;justify-content:space-between;align-items:flex-start}\n.qmark{position:relative;width:34px;height:34px;flex:none}\n.mark{width:100%;height:100%;overflow:visible}\n.mark circle,.mark path{fill:none;stroke-linecap:round;stroke-linejoin:round}\n.mark circle{stroke-width:2.6;stroke-dasharray:88;stroke-dashoffset:88;animation:draw .45s ease-out forwards;transform:rotate(-90deg);transform-origin:center}\n.mark path{stroke-width:3.4;stroke-dasharray:30;stroke-dashoffset:30;animation:draw .35s .4s ease-out forwards}\n.mark.ok circle,.mark.ok path{stroke:var(--accent)}\n.mark.no circle,.mark.no path{stroke:var(--warn)}\n.mark.ok{animation:pop .35s .7s ease-out}\n@keyframes draw{to{stroke-dashoffset:0}}\n@keyframes pop{50%{transform:scale(1.22)}}\n.burst{position:absolute;left:50%;top:50%;width:0;height:0;pointer-events:none}\n.burst i{position:absolute;width:6px;height:6px;margin:-3px;border-radius:50%;background:var(--accent);opacity:0;animation:burst .7s .55s ease-out forwards}\n.burst i:nth-child(odd){background:var(--accent2)}\n@keyframes burst{0%{opacity:1;transform:rotate(var(--a)) translateY(0)}100%{opacity:0;transform:rotate(var(--a)) translateY(calc(-1 * var(--d,26px)))}}\n.shake{animation:shake .4s}\n@keyframes shake{20%,60%{transform:translateX(-5px)}40%,80%{transform:translateX(5px)}}\n.q .fb.ok{color:var(--fg)}\n.q .fb.no{color:var(--warn)}\n.q .opts button.right{font-weight:600}\n.slidein{animation:fadein .35s}\n.progress{height:6px;background:var(--line);border-radius:3px;overflow:hidden;margin:6px 0 10px}\n.progress i{display:block;height:100%;width:0;background:var(--accent);transition:width .4s}\n.endcard{text-align:center;padding:24px 12px}\n.bigmark{position:relative;width:84px;height:84px;margin:0 auto 6px;--d:70px}\n.bigmark .burst i{width:9px;height:9px;margin:-4.5px}\n@media (prefers-reduced-motion:reduce){.mark,.mark *,.burst i,.shake{animation-duration:.01s!important;animation-delay:0s!important}}\n.ctl.locked{opacity:.5}\n.ctl.locked input{cursor:not-allowed}\n.toast{position:absolute;left:50%;bottom:calc(100% + 10px);transform:translate(-50%,6px);z-index:5;background:var(--fg);color:var(--bg);padding:9px 13px;border-radius:9px;font-size:.88rem;line-height:1.4;width:max-content;max-width:min(540px,94%);box-shadow:0 6px 20px rgba(0,0,0,.25);opacity:0;pointer-events:none;transition:opacity .2s,transform .2s}\n.toast::after{content:\"\";position:absolute;left:50%;top:100%;margin-left:-7px;border:7px solid transparent;border-top-color:var(--fg)}\n.toast.show{opacity:1;transform:translate(-50%,0)}\n.toast b{font-weight:700}\n.ptitle{font-weight:600;font-size:.92rem;margin:0 0 4px}\n[hidden]{display:none!important}\n";
const HTML="\n<nav class=\"tabs\" role=\"tablist\">\n  <button class=\"tab\" data-tab=\"overzicht\">In \u00e9\u00e9n oogopslag<\/button>\n  <button class=\"tab\" data-tab=\"snelheid\">Snelheidsgrafiek<\/button>\n  <button class=\"tab\" data-tab=\"quiz\">Quiz \u2713<\/button>\n<\/nav>\n\n<section class=\"panel\" id=\"tab-overzicht\">\n  <h2>Hoofdstuk 12 in \u00e9\u00e9n oogopslag<\/h2>\n  <div class=\"grid2\">\n    <div>\n      <p>In plaats van \u00e9\u00e9n netto groeisnelheid r splitsen we groei op in <b>geboorte<\/b> (b) en <b>sterfte<\/b> (d), allebei <i>per individu<\/i>. Bij <b>dichtheidsafhankelijkheid<\/b> hangen die snelheden af van hoeveel individuen er al zijn.<\/p>\n      <p class=\"muted\" style=\"font-size:.88rem\">In de animatie rechts daalt de geboorte per individu naarmate N groter wordt, terwijl de sterfte per individu gelijk blijft. Zolang de geboorte boven de sterfte ligt, groeit de populatie. Waar de twee lijnen elkaar snijden, ligt het evenwicht.<\/p>\n      <table class=\"cmp\">\n        <tr><th>Model<\/th><th>ODE<\/th><\/tr>\n        <tr><td>Exponentieel<\/td><td class=\"eqn\">dN/dt = (b \u2212 d)N<\/td><\/tr>\n        <tr><td>Logistisch<\/td><td class=\"eqn\">dN/dt = rN(1 \u2212 N/K)<\/td><\/tr>\n        <tr><td>Dichtheidsafh. geboorte<\/td><td class=\"eqn\">dN/dt = b(1 \u2212 N/K)N \u2212 dN<\/td><\/tr>\n        <tr><td>Dichtheidsafh. sterfte<\/td><td class=\"eqn\">dN/dt = bN \u2212 d(1 + N/K)N<\/td><\/tr>\n      <\/table>\n    <\/div>\n    <div class=\"card\"><canvas id=\"hero\" class=\"plot\"><\/canvas>\n      <div class=\"muted\" id=\"hero-lbl\" style=\"font-size:.85rem;text-align:center;margin-top:4px\"><\/div><\/div>\n  <\/div>\n\n  <div class=\"cards\">\n    <div class=\"card ov\"><span class=\"num\">12.2<\/span><h4>Geboorte en sterfte<\/h4>\n      <p>We splitsen r = b \u2212 d. Een negatieve r is lastig te interpreteren, maar b en d zijn snelheden <i>per individu<\/i> die je kunt meten. Bij b &gt; d groeit de populatie, bij b &lt; d neemt ze af.<\/p>\n      <button class=\"b\" data-go=\"snelheid\">Probeer het \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">12.3<\/span><h4>Dichtheidsafhankelijke geboorte<\/h4>\n      <p>De geboorte b wordt b(1 \u2212 N/K). Daarmee is b de <b>maximale<\/b> geboortesnelheid, die alleen bij een lage dichtheid wordt gehaald.<\/p>\n      <button class=\"b\" data-go=\"snelheid\">Bekijk de grafiek \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">12.4<\/span><h4>De snelheidsgrafiek<\/h4>\n      <p>Zet dN/dt uit tegen N. Boven de N-as groeit de populatie, eronder krimpt ze. Waar de grafiek de N-as snijdt, liggen de <b>evenwichten<\/b>.<\/p>\n      <button class=\"b\" data-go=\"snelheid\">Naar de snelheidsgrafiek \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">12.4<\/span><h4>Het faseportret<\/h4>\n      <p>De N-as met pijlen is de <b>toestandslijn<\/b>. Een \u25cf is een stabiel evenwicht (een attractor): het systeem gaat ernaartoe. Een \u25cb is instabiel: het systeem gaat ervan weg.<\/p>\n      <button class=\"b\" data-go=\"snelheid\">Bekijk het faseportret \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">12.5<\/span><h4>Dichtheidsafhankelijke sterfte<\/h4>\n      <p>De sterfte d wordt d(1 + N/K): hoe drukker, hoe meer sterfte per individu.<\/p>\n      <button class=\"b\" data-go=\"snelheid\">Probeer het \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">12.6<\/span><h4>Andere dichtheidseffecten<\/h4>\n      <p>Geboorte \u00e9n sterfte kunnen tegelijk van de dichtheid afhangen (met K<sub class=\"nomath\">b<\/sub> en K<sub class=\"nomath\">d<\/sub>). Ook andere soorten tellen mee, zoals vossen die konijnen eten: \u2212cNV. Dat is <b>massawerking<\/b>: evenredig met beide dichtheden.<\/p><\/div>\n    <div class=\"card ov\"><span class=\"num\">\u2713<\/span><h4>Test jezelf<\/h4>\n      <p>Acht korte vragen over dichtheidsafhankelijkheid en de snelheidsgrafiek. Bij een fout antwoord mag je het opnieuw proberen.<\/p>\n      <button class=\"b primary\" data-go=\"quiz\">Start de quiz \u2192<\/button><\/div>\n  <\/div>\n\n  <details>\n    <summary>Terminologie<\/summary>\n    <table class=\"cmp\">\n      <tr><th>Nederlands<\/th><th>Engels<\/th><th>Betekenis<\/th><\/tr>\n      <tr><td>Intrinsieke groeisnelheid<\/td><td>Intrinsic growth rate<\/td><td>Netto groei r. Hoe groot geboorte en sterfte apart zijn, blijft vaak onbekend.<\/td><\/tr>\n      <tr><td>Dichtheidsafhankelijk<\/td><td>Density dependent<\/td><td>Een proces versnelt of vertraagt met de dichtheid.<\/td><\/tr>\n      <tr><td>Toestandsruimte (of -lijn)<\/td><td>State space (or line)<\/td><td>Alle mogelijke toestanden van het systeem.<\/td><\/tr>\n      <tr><td>Faseportret<\/td><td>Phase portrait<\/td><td>Beeld van de evenwichten en hun stabiliteit.<\/td><\/tr>\n      <tr><td>Evenwicht<\/td><td>Steady state<\/td><td>Er is geen verandering in de tijd.<\/td><\/tr>\n      <tr><td>Stabiel evenwicht<\/td><td>Stable steady state<\/td><td>Het systeem beweegt ernaartoe.<\/td><\/tr>\n      <tr><td>Instabiel evenwicht<\/td><td>Unstable steady state<\/td><td>Het systeem beweegt ervan weg.<\/td><\/tr>\n      <tr><td>Attractor<\/td><td>Attractor<\/td><td>De toestand waar het systeem op lange termijn naartoe gaat.<\/td><\/tr>\n      <tr><td>Massawerking<\/td><td>Mass action<\/td><td>Een interactie is evenredig met de dichtheden.<\/td><\/tr>\n    <\/table>\n  <\/details>\n<\/section>\n\n<section class=\"panel\" id=\"tab-snelheid\" hidden>\n  <h2>De snelheidsgrafiek<\/h2>\n  <p>Kies een model. Links zie je hoe snel N verandert bij elke waarde van N. Rechts zie je hoe populaties met verschillende beginwaarden zich door de tijd gedragen.<\/p>\n  <div class=\"btns\"><div class=\"seg\" id=\"sg-models\"><\/div><\/div>\n  <p style=\"text-align:center;margin:.4rem 0\"><span class=\"formula\" id=\"sg-eq\"><\/span><\/p>\n  <div class=\"grid2\">\n    <div class=\"card\">\n      <div class=\"ptitle\">Snelheidsgrafiek (dN/dt tegen N)<\/div>\n      <canvas id=\"sg-phase\" class=\"plot\"><\/canvas>\n      <div class=\"legend\" id=\"sg-legend\"><\/div>\n      <div class=\"muted\" style=\"font-size:.82rem;margin-top:4px\">Boven de N-as groeit N (pijl naar rechts), eronder krimpt N. De N-as met de pijlen is de toestandslijn. Samen met \u25cf (stabiel) en \u25cb (instabiel) vormt die het faseportret. Klik in de grafiek om een populatie toe te voegen.<\/div>\n    <\/div>\n    <div class=\"card\">\n      <div class=\"ptitle\">Oplossingen (= grafiek door de tijd)<\/div>\n      <canvas id=\"sg-time\" class=\"plot\"><\/canvas>\n      <div class=\"muted\" style=\"font-size:.82rem;margin-top:4px\">Dezelfde populaties als links, nu uitgezet tegen de tijd.<\/div>\n    <\/div>\n  <\/div>\n  <div class=\"controls\" id=\"sg-ctl\"><\/div>\n  <div class=\"btns\">\n    <button class=\"b\" id=\"sg-reset\">\u21ba Opnieuw<\/button>\n    <label class=\"chk\" id=\"sg-split-l\"><input type=\"checkbox\" id=\"sg-split\"> toon geboorte- en sterfteterm apart<\/label>\n    <button class=\"b primary\" id=\"sg-reveal\" hidden>Toon evenwichten<\/button>\n  <\/div>\n  <div class=\"callout\" id=\"sg-text\"><\/div>\n<\/section>\n\n<section class=\"panel\" id=\"tab-quiz\" hidden>\n  <h2>Quiz: dichtheidsafhankelijkheid<\/h2>\n  <div style=\"display:flex;justify-content:space-between;font-size:.88rem\" class=\"muted\"><span id=\"qz-count\"><\/span><span id=\"qz-score\"><\/span><\/div>\n  <div class=\"progress\"><i id=\"qz-prog\"><\/i><\/div>\n  <div id=\"qz-box\"><\/div>\n  <div class=\"btns\" style=\"justify-content:flex-end\"><button class=\"b primary\" id=\"qz-next\" hidden>Volgende vraag \u2192<\/button><\/div>\n<\/section>\n\n";
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
function applyTheme(){ if(detectDark()) host.setAttribute('data-dark',''); else host.removeAttribute('data-dark'); cssCache=null; }
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
    if(w[0]==='Δ'){ push('Δ','n'); w=w.slice(1); if(!w) continue; }
    if(w==='L'&&s.slice(i-3,i)==='mg/'){ push(w,'n'); continue; }
    if(w.length===1||/^[a-zα-ω][A-Z]$/.test(w)) push(w,'i'); else push(w,'n');
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
function renderQuestion(it,num,onSolved){
  const d=document.createElement('div'); d.className='card q';
  d.innerHTML=`<div class="qhead"><div>${num!=null?num+'. ':''}${it.q}</div><div class="qmark"></div></div><div class="opts"></div><div class="fb"></div>`;
  const ob=d.querySelector('.opts'), fb=d.querySelector('.fb'), mk=d.querySelector('.qmark');
  let first=true, solved=false;
  it.o.forEach((o,oi)=>{ const b=document.createElement('button'); b.innerHTML=o; b.onclick=()=>{
    if(solved||b.classList.contains('wrong')) return;
    if(oi===it.a){ solved=true; b.classList.add('right'); mk.innerHTML=MARK_OK; burst(mk);
      fb.innerHTML=(first?'<b>Goed!</b> ':'<b>Goed zo, nu wel.</b> ')+it.e; fb.className='fb show ok';
      ob.querySelectorAll('button').forEach(x=>{ if(x!==b) x.style.opacity=.45; });
      onSolved&&onSolved(first); }
    else { first=false; b.classList.add('wrong'); b.classList.remove('shake'); void b.offsetWidth; b.classList.add('shake');
      mk.innerHTML=MARK_NO; fb.innerHTML='Nog niet goed. '+(it.h||'Probeer het nog eens!'); fb.className='fb show no'; }
  }; ob.appendChild(b); });
  return d;
}

/* =========================================================
   IN ÉÉN OOGOPSLAG: per-capita rates animation
   ========================================================= */
const hero=(()=>{
  const c=$('#hero'), lbl=$('#hero-lbl');
  const b=1, d=0.3, K=100, NE=K*(1-d/b), N0=4, XM=130;
  let N=N0, hold=0;
  function frameFn(dt){
    if(hold>0){ hold-=dt; if(hold<=0) N=N0; }
    else { const h=dt*1.4/10; for(let i=0;i<10;i++) N+=(b*(1-N/K)-d)*N*h; if(Math.abs(N-NE)<0.5){ N=NE; hold=2.5; } }
    const f=frame(c,{xmin:0,xmax:XM,ymin:-0.35,ymax:1.15,xlab:'dichtheid N',ylab:'snelheid per individu (per jaar)',pad:{l:46}});
    const cx=f.ctx, bn=b*(1-N/K);
    const bl=[]; for(let x=0;x<=XM;x+=2) bl.push([x,b*(1-x/K)]);
    poly(f,bl,col('accent'),2.8);
    poly(f,[[0,d],[XM,d]],col('accent2'),2.8);
    // current density
    cx.save(); cx.setLineDash([4,4]); cx.strokeStyle=col('muted'); cx.beginPath(); cx.moveTo(f.X(N),f.Y(-0.35)); cx.lineTo(f.X(N),f.Y(1.15)); cx.stroke(); cx.restore();
    if(Math.abs(bn-d)>0.02){ cx.save(); cx.strokeStyle=col('fg'); cx.globalAlpha=.55; cx.lineWidth=5; cx.beginPath(); cx.moveTo(f.X(N),f.Y(bn)); cx.lineTo(f.X(N),f.Y(d)); cx.stroke(); cx.restore(); }
    disc(cx,f.X(N),f.Y(bn),5,col('accent'),col('bg'),1.5);
    disc(cx,f.X(N),f.Y(d),5,col('accent2'),col('bg'),1.5);
    disc(cx,f.X(NE),f.Y(d),hold>0?7:4,null,col('fg'),2);
    cx.font='600 '+FONT; cx.textBaseline='bottom';
    cx.fillStyle=col('accent'); cx.textAlign='left'; drawMath(cx,'geboorte b(1 − N/K)',f.X(3),f.Y(1.02));
    cx.fillStyle=col('accent2'); cx.textAlign='right'; drawMath(cx,'sterfte d',f.X(XM)-4,f.Y(d)-4);
    cx.restore();
    lbl.innerHTML= hold>0
      ? `N = ${fmt(NE)}: geboorte = sterfte per individu → <b>evenwicht</b>`
      : `N = ${N.toFixed(0)}: geboorte (${bn.toFixed(2)}) &gt; sterfte (${d.toFixed(2)}) per individu → N <b>groeit</b>`;
  }
  return {frame:frameFn};
})();

/* =========================================================
   SNELHEIDSGRAFIEK
   ========================================================= */
const snel=(()=>{
  const MODELS={
    expo:{name:'Exponentieel', eq:'dN/dt = (b − d)N',
      p:[{k:'b',l:'geboorte b',min:0,max:1,step:0.05,val:0.5,u:'per jaar'},{k:'d',l:'sterfte d',min:0,max:1,step:0.05,val:0.3,u:'per jaar'}],
      B:(x,p)=>p.b*x, D:(x,p)=>p.d*x, xmax:p=>100, T:p=>Math.min(40,4/Math.max(Math.abs(p.b-p.d),0.05)),
      starts:xm=>[3,15,40],
      note:p=>p.b>p.d?'Omdat b > d groeit elke populatie groter dan 0 onbeperkt. Er is geen draagvermogen.':'Omdat b < d sterft elke populatie uit.'},
    logistisch:{name:'Logistisch', eq:'dN/dt = rN(1 − N/K)',
      p:[{k:'r',l:'groeisnelheid r',min:0.1,max:2,step:0.05,val:0.8,u:'per jaar'},{k:'K',l:'draagvermogen K',min:20,max:200,step:10,val:100,u:'individuen'}],
      f:(x,p)=>p.r*x*(1-x/p.K), xmax:p=>1.5*p.K, T:p=>9/p.r,
      starts:xm=>[0.02*xm,0.35*xm,0.9*xm],
      note:p=>'Het stabiele evenwicht is het draagvermogen K. De groei is het snelst bij N = K/2, de top van de bergparabool.'},
    geboorte:{name:'Dichtheidsafh. geboorte', eq:'dN/dt = b(1 − N/K)N − dN',
      p:[{k:'b',l:'max. geboorte b',min:0.1,max:2,step:0.05,val:1,u:'per jaar'},{k:'d',l:'sterfte d',min:0,max:1.5,step:0.05,val:0.3,u:'per jaar'},{k:'K',l:'K',min:20,max:200,step:10,val:100,u:'individuen'}],
      B:(x,p)=>p.b*(1-x/p.K)*x, D:(x,p)=>p.d*x, xmax:p=>1.5*p.K, T:p=>Math.min(60,8/Math.max(Math.abs(p.b-p.d),0.08)),
      starts:xm=>[0.02*xm,0.35*xm,0.9*xm],
      note:p=>p.b>p.d?'Bij een lage dichtheid is de geboorte b groter dan de sterfte d, dus de populatie groeit. Hoe drukker, hoe lager de geboorte per individu, totdat geboorte en sterfte even groot zijn.':'Zelfs bij een lage dichtheid is de geboorte b niet groter dan de sterfte d, dus de populatie sterft uit.'},
    sterfte:{name:'Dichtheidsafh. sterfte', eq:'dN/dt = bN − d(1 + N/K)N', hide:true,
      p:[{k:'b',l:'geboorte b',min:0.1,max:2,step:0.05,val:0.8,u:'per jaar'},{k:'d',l:'min. sterfte d',min:0.05,max:1,step:0.05,val:0.3,u:'per jaar'},{k:'K',l:'K',min:20,max:200,step:10,val:100,u:'individuen'}],
      B:(x,p)=>p.b*x, D:(x,p)=>p.d*(1+x/p.K)*x,
      xmax:p=>{ const e=p.b>p.d?p.K*(p.b/p.d-1):0; return niceCeil(Math.max(1.5*e,0.5*p.K,20)); },
      T:p=>Math.min(60,8/Math.max(Math.abs(p.b-p.d),0.08)),
      starts:xm=>[0.02*xm,0.3*xm,0.9*xm],
      note:p=>p.b>p.d?'Hoe drukker, hoe hoger de sterfte per individu, totdat die gelijk is aan de geboorte.':'De sterfte is altijd minstens zo groot als de geboorte, dus de populatie sterft uit.'}
  };
  const seg=$('#sg-models'), ph=$('#sg-phase'), tc=$('#sg-time'), ctl=$('#sg-ctl'), cbS=$('#sg-split'), rev=$('#sg-reveal');
  let key='geboorte', P={}, sliders=[], balls=[], t=0, Tend=10, lastF=null, revealed=true;
  const fOf=M=>M.f||((x,p)=>M.B(x,p)-M.D(x,p));
  Object.entries(MODELS).forEach(([k,M])=>{ const b=document.createElement('button'); b.textContent=M.name; b.dataset.k=k; b.onclick=()=>setModel(k); seg.appendChild(b); });
  function legend(){ const M=MODELS[key], split=cbS.checked&&M.B;
    const ln=dash=>`<svg width="24" height="8" style="vertical-align:middle;margin-right:4px"><line x1="0" y1="4" x2="24" y2="4" style="stroke:currentColor;stroke-width:1.8;stroke-dasharray:${dash}"/></svg>`;
    $('#sg-legend').innerHTML=`<span><i class="dot" style="background:var(--accent)"></i>dN/dt (netto)</span>`+
      (split?`<span>${ln('6 4')}geboorteterm</span><span>${ln('2 3')}sterfteterm</span>`:'')+
      `<span><i class="dot" style="background:var(--accent2)"></i>populaties</span>`; }
  function setModel(k){ key=k; const M=MODELS[k];
    [...seg.children].forEach(b=>b.setAttribute('aria-pressed',b.dataset.k===k));
    $('#sg-eq').textContent=M.eq;
    ctl.innerHTML='';
    sliders=M.p.map(d=>{ const w=document.createElement('div'); w.className='ctl';
      w.innerHTML=`<label><span class="lbl">${d.l}</span> <span class="val"></span></label><input type="range" min="${d.min}" max="${d.max}" step="${d.step}" value="${d.val}">`;
      ctl.appendChild(w); const inp=w.querySelector('input'); inp.addEventListener('input',()=>{ readP(); reset(); });
      return {d,inp,out:w.querySelector('.val')}; });
    cbS.disabled=!M.B; if(!M.B) cbS.checked=false;
    $('#sg-split-l').style.opacity=M.B?1:.5;
    $('#sg-split-l').title=M.B?'':'In het logistische model is r de netto groei: geboorte en sterfte zitten er niet apart in.';
    revealed=!M.hide; rev.hidden=revealed;
    readP(); reset(); legend(); }
  function readP(){ P={}; for(const s of sliders){ P[s.d.k]=+s.inp.value; s.out.textContent=s.inp.value+' '+s.d.u; } }
  function reset(){ const M=MODELS[key], xm=M.xmax(P); Tend=M.T(P); t=0;
    balls=M.starts(xm).filter(x=>x<=xm).map(x=>({x,hist:[[0,x]]})); }
  function addBall(x){ if(t>=Tend){ t=0; balls=[]; } balls.push({x,hist:[[t,x]]}); if(balls.length>8) balls.shift(); }
  function equil(f,xm){
    const out=[{x:0}], n=600; let px=xm*1e-6, pv=f(px);
    for(let i=1;i<=n;i++){ const x=xm*i/n, v=f(x);
      if(v===0){ out.push({x}); }
      else if(pv!==0&&pv*v<0){ let a=px, b=x; for(let k=0;k<60;k++){ const m=(a+b)/2; if(f(a)*f(m)<=0) b=m; else a=m; } out.push({x:(a+b)/2}); }
      px=x; pv=v; }
    const h=Math.max(1e-7,xm*1e-5);
    for(const e of out) e.stable = e.x===0 ? f(h)<0 : (f(e.x+h)-f(e.x-h))<0;
    return out; }
  cbS.addEventListener('change',legend);
  $('#sg-reset').onclick=reset;
  rev.onclick=()=>{ revealed=true; rev.hidden=true; };
  ph.addEventListener('click',e=>{ if(!lastF) return; const [px,py]=localXY(e,ph); const [x]=lastF.inv(px,py); const xm=MODELS[key].xmax(P); if(x>=0&&x<=xm) addBall(x); });
  setModel('geboorte');

  function frameFn(dt){
    const M=MODELS[key], fm=fOf(M), f=x=>fm(x,P), xm=M.xmax(P), split=cbS.checked&&!!M.B;
    if(t<Tend){ const ds=dt*Tend/7, n=Math.max(1,Math.ceil(ds/(Tend/2000))), h=ds/n;
      for(const b of balls){ for(let i=0;i<n;i++){ b.x+=f(b.x)*h; if(b.x>xm*50) break; } }
      t+=ds; for(const b of balls) b.hist.push([t,b.x]); }
    const flat=Math.abs(f(xm*0.5))<1e-12&&Math.abs(f(xm))<1e-12;
    const eqs=flat?[]:equil(f,xm);
    // phase plot
    let lo=0, hi=0;
    for(let i=0;i<=100;i++){ const x=xm*i/100, v=f(x); lo=Math.min(lo,v); hi=Math.max(hi,v);
      if(split){ const bb=M.B(x,P), dd=M.D(x,P); lo=Math.min(lo,bb,dd); hi=Math.max(hi,bb,dd); } }
    const ext=Math.max(Math.abs(lo),Math.abs(hi),1e-6)*1.15;
    const F=frame(ph,{xmin:0,xmax:xm,ymin:Math.min(-ext*0.35,lo*1.15),ymax:Math.max(ext*0.35,hi*1.15),xlab:'N (individuen)',ylab:'dN/dt'}); lastF=F;
    const cx=F.ctx, y0=F.Y(0);
    cx.fillStyle=col('muted'); cx.textAlign='left';
    cx.textBaseline='top'; cx.fillText('groei ↑',F.p.l+6,F.p.t+4);
    cx.textBaseline='bottom'; cx.fillText('afname ↓',F.p.l+6,F.p.t+F.ph-4);
    if(split){ const bp=[], dp=[]; for(let i=0;i<=200;i++){ const x=xm*i/200; bp.push([x,M.B(x,P)]); dp.push([x,M.D(x,P)]); }
      poly(F,bp,col('fg'),1.8,[6,4],.8); poly(F,dp,col('fg'),1.8,[2,3],.8); }
    const curve=[]; for(let i=0;i<=200;i++){ const x=xm*i/200; curve.push([x,f(x)]); } poly(F,curve,col('accent'),2.8);
    cx.save(); cx.strokeStyle=col('axis'); cx.lineWidth=1.5; cx.beginPath(); cx.moveTo(F.p.l,y0); cx.lineTo(F.p.l+F.pw,y0); cx.stroke(); cx.restore();
    for(let i=1;i<12;i++){ const x=xm*i/12, v=f(x); if(Math.abs(v)<ext*0.02) continue; const px=F.X(x), dir=v>0?1:-1;
      cx.fillStyle=col('muted'); cx.beginPath(); cx.moveTo(px+dir*6,y0); cx.lineTo(px-dir*4,y0-5); cx.lineTo(px-dir*4,y0+5); cx.closePath(); cx.fill(); }
    if(revealed) for(const e of eqs){ if(e.x<0||e.x>xm) continue; disc(cx,F.X(e.x),y0,6.5,e.stable?col('fg'):col('bg'),col('fg'),2); }
    for(const b of balls){ if(b.x<0||b.x>xm) continue; const px=F.X(b.x), py=F.Y(f(b.x));
      cx.save(); cx.strokeStyle=col('accent2'); cx.globalAlpha=.6; cx.setLineDash([2,3]); cx.beginPath(); cx.moveTo(px,y0); cx.lineTo(px,py); cx.stroke(); cx.restore();
      disc(cx,px,y0,5,col('accent2'),col('bg'),1.5); }
    cx.restore();
    // time plot
    const G=frame(tc,{xmin:0,xmax:Tend,ymin:0,ymax:xm,xlab:'tijd (jaar)',ylab:'N (individuen)'}); const gx=G.ctx;
    if(revealed) for(const e of eqs){ if(e.x<=0||e.x>xm) continue; const y=G.Y(e.x);
      gx.save(); gx.setLineDash([6,4]); gx.strokeStyle=col('muted'); gx.beginPath(); gx.moveTo(G.p.l,y); gx.lineTo(G.p.l+G.pw,y); gx.stroke(); gx.restore();
      const lab=' = '+fmt(e.x), xr=G.p.l+G.pw-4, yb=y-3; gx.fillStyle=col('muted'); gx.textAlign='right'; gx.textBaseline='bottom'; gx.fillText(lab,xr,yb);
      const wl=gx.measureText(lab).width, f0=gx.font; gx.font='italic '+f0; const wv=gx.measureText('N').width; gx.fillText('N',xr-wl,yb); gx.font=f0;
      gx.strokeStyle=col('muted'); gx.lineWidth=1.2; gx.beginPath(); gx.moveTo(xr-wl-wv,yb-12); gx.lineTo(xr-wl,yb-12); gx.stroke(); }
    for(const b of balls){ poly(G,b.hist,col('accent2'),2.2,null,.9); const l=b.hist[b.hist.length-1]; if(l[1]<=xm&&l[1]>=0) disc(gx,G.X(l[0]),G.Y(l[1]),4,col('accent2')); }
    gx.restore();
    // explanation
    let txt;
    if(!revealed) txt='Bereken eerst zelf de evenwichten van dit model en schets de snelheidsgrafiek (Oefening 12.2). Controleer daarna je antwoord met de knop <b>Toon evenwichten</b>.';
    else if(flat) txt='Bij b = d is dN/dt = 0 voor elke N. Geboorte en sterfte heffen elkaar precies op, dus de populatie blijft gelijk.';
    else { const OV='<span class="ovb">N</span>';
      txt='<b>Evenwichten</b> (waar de grafiek de N-as snijdt): '+eqs.map(e=>`${OV} = ${fmt(e.x)} <span class="muted">(${e.stable?'stabiel ●':'instabiel ○'})</span>`).join(' en ')+'. '+M.note(P);
      if(split) txt+=' De netto groei is de geboorteterm min de sterfteterm. Waar de twee stippellijnen elkaar snijden, is dN/dt = 0.'; }
    const el=$('#sg-text'); if(el.__last!==txt){ el.innerHTML=txt; el.__last=txt; }
  }
  return {frame:frameFn};
})();

/* =========================================================
   QUIZ
   ========================================================= */
function sgSVG(f,xm,ticks){
  const W=300,H=150,l=40,r=14,t=12,b=28; let lo=0,hi=0;
  for(let i=0;i<=100;i++){ const v=f(xm*i/100); lo=Math.min(lo,v); hi=Math.max(hi,v); }
  const ext=Math.max(Math.abs(lo),Math.abs(hi))*1.2, ymin=Math.min(-ext*0.4,lo*1.2), ymax=hi*1.2;
  const X=x=>l+x/xm*(W-l-r), Y=y=>t+(ymax-y)/(ymax-ymin)*(H-t-b);
  let d=''; for(let i=0;i<=120;i++){ const x=xm*i/120; d+=(i?'L':'M')+X(x).toFixed(1)+' '+Y(f(x)).toFixed(1); }
  const tk=ticks.map(tv=>{ const [v,lab]=Array.isArray(tv)?tv:[tv,tv]; return `<line x1="${X(v)}" y1="${Y(0)-3}" x2="${X(v)}" y2="${Y(0)+3}" style="stroke:currentColor"/><text x="${X(v)}" y="${Y(0)+15}" text-anchor="middle" style="font-size:11px;fill:currentColor;font-style:${isNaN(lab)?'italic':'normal'}">${lab}</text>`; }).join('');
  return `<svg viewBox="0 0 ${W} ${H}" width="${W}" style="max-width:100%;display:block;margin:8px 0;color:var(--muted)" role="img" aria-label="snelheidsgrafiek">
    <line x1="${l}" y1="${t}" x2="${l}" y2="${H-b}" style="stroke:currentColor"/>
    <line x1="${l}" y1="${Y(0)}" x2="${W-r}" y2="${Y(0)}" style="stroke:currentColor"/>
    ${tk}<path d="${d}" style="fill:none;stroke:var(--accent);stroke-width:2.6"/>
    <text x="${l-6}" y="${t+10}" text-anchor="end" style="font-size:11px;fill:currentColor">d<tspan font-style="italic">N</tspan>/d<tspan font-style="italic">t</tspan></text>
    <text x="${W-r}" y="${Y(0)-6}" text-anchor="end" font-style="italic" style="font-size:12px;fill:currentColor">N</text></svg>`;
}
const quiz=(()=>{
  const G=sgSVG(x=>0.05*x*(1-x/60),90,[0,30,60,90]);
  const BASE=[
    {q:'Wat betekent <b>dichtheidsafhankelijk</b>?',
     o:['De snelheid van een proces hangt af van hoeveel individuen er zijn','De populatie groeit altijd exponentieel','Het aantal individuen per vierkante meter blijft constant','Geboorte en sterfte zijn altijd even groot'], a:0,
     h:'Denk aan de geboorte per individu in een druk gebied.',
     e:'Een proces zoals geboorte of sterfte versnelt of vertraagt naarmate de dichtheid N toeneemt.'},
    {q:'Waarom splitsen we de groeisnelheid r op in geboorte b en sterfte d?',
     o:['Omdat r altijd groter is dan 1','Een negatieve r is lastig te interpreteren, maar b en d zijn snelheden per individu die je kunt meten','Zodat het model geen evenwicht meer heeft','Omdat b en d dezelfde eenheid hebben als N'], a:1,
     h:'Wat zou een "negatieve groeisnelheid" biologisch betekenen?',
     e:'Met r = b − d kun je geboorte en sterfte apart meten en apart dichtheidsafhankelijk maken.'},
    {q:'In dN/dt = (b − d)N is b = 0.2 en d = 0.5 per jaar. Wat gebeurt er met de populatie?',
     o:['Ze groeit exponentieel','Ze neemt af en sterft uit','Ze blijft gelijk','Ze groeit tot een draagvermogen'], a:1,
     h:'Is b − d positief of negatief?',
     e:'b − d = −0.3 per jaar is negatief, dus de populatie neemt exponentieel af.'},
    {q:'In dN/dt = b(1 − N/K)N − dN: wat gebeurt er met de geboorte <i>per individu</i>, b(1 − N/K), als N toeneemt?',
     o:['Die neemt af','Die neemt toe','Die blijft gelijk aan b','Die wordt negatief zodra N > 0'], a:0,
     h:'Vul N = 0 en N = K in.',
     e:'Bij N = 0 is die gelijk aan b, de maximale geboortesnelheid. Bij N = K is die 0. Hoe drukker, hoe minder geboorte per individu.'},
    {q:'In welke ODE is de <b>sterfte</b> dichtheidsafhankelijk?',
     o:['dN/dt = (b − d)N','dN/dt = b(1 − N/K)N − dN','dN/dt = bN − d(1 + N/K)N','dN/dt = rN'], a:2,
     h:'Zoek de sterfteterm waarin N niet alleen als laatste factor voorkomt.',
     e:'De sterfte per individu, d(1 + N/K), wordt groter naarmate N toeneemt.'},
    {q:'Hieronder staat een snelheidsgrafiek. Bij welke N ligt het <b>stabiele</b> evenwicht?'+G,
     o:['N = 0','N = 30','N = 60','N = 90'], a:2,
     h:'Kijk links en rechts van elk snijpunt met de N-as: groeit of krimpt N daar?',
     e:'Links van 60 is dN/dt > 0, dus N groeit naar 60 toe. Rechts van 60 is dN/dt < 0, dus N krimpt naar 60 toe. Bij 30 is de groei het snelst, maar dat is geen evenwicht.'},
    {q:'Dezelfde snelheidsgrafiek. Een populatie begint bij N = 80. Wat gebeurt er?'+G,
     o:['Ze groeit verder','Ze neemt af tot N = 60','Ze sterft uit','Ze blijft op N = 80'], a:1,
     h:'Ligt de grafiek bij N = 80 boven of onder de N-as?',
     e:'Bij N = 80 is dN/dt negatief, dus N neemt af. Dat gaat door totdat N bij het stabiele evenwicht 60 is.'},
    {q:'Hieronder staat de snelheidsgrafiek van het logistische model dN/dt = rN(1 − N/K). Bij welke N is de groeisnelheid dN/dt het grootst?'+sgSVG(x=>0.03*x*(1-x/100),130,[[0,'0'],[100,'K']]),
     o:['Bij N = 0','Bij N = K/2','Bij N = K','Bij N groter dan K'], a:1,
     h:'Waar ligt de top van een bergparabool die de as snijdt bij 0 en K?',
     e:'De bergparabool snijdt de N-as bij 0 en K, dus de top ligt precies halverwege, bij N = K/2.'},
  ];
  const box=$('#qz-box'), prog=$('#qz-prog'), cnt=$('#qz-count'), sc=$('#qz-score'), next=$('#qz-next');
  let Q=[], i=0, score=0;
  const shuffle=a=>{ a=a.slice(); for(let k=a.length-1;k>0;k--){ const j=Math.floor(Math.random()*(k+1)); [a[k],a[j]]=[a[j],a[k]]; } return a; };
  function prep(it){ const idx=shuffle(it.o.map((_,k)=>k)); return {...it,o:idx.map(k=>it.o[k]),a:idx.indexOf(it.a)}; }
  function start(){ Q=BASE.map(prep); i=0; score=0; show(); }
  function show(){
    box.innerHTML=''; next.hidden=true;
    if(i>=Q.length) return end();
    cnt.textContent=`Vraag ${i+1} van ${Q.length}`; sc.textContent=`${score} in één keer goed`;
    prog.style.width=(i/Q.length*100)+'%';
    const el=renderQuestion(Q[i],null,first=>{ if(first) score++; sc.textContent=`${score} in één keer goed`;
      prog.style.width=((i+1)/Q.length*100)+'%'; next.textContent=i===Q.length-1?'Bekijk resultaat →':'Volgende vraag →'; next.hidden=false; });
    el.classList.add('slidein'); box.appendChild(el); postHeight();
  }
  function end(){
    prog.style.width='100%'; cnt.textContent='Klaar!';
    const frac=score/Q.length;
    const msg=frac===1?'Perfect! Je kunt dichtheidseffecten en snelheidsgrafieken lezen.':frac>=.75?'Heel goed! Kijk nog even naar de vragen die je niet in één keer goed had.':frac>=.5?'Op de goede weg. Speel nog eens met de snelheidsgrafiek.':'Lees het hoofdstuk nog eens door en speel met de snelheidsgrafiek. Daarna gaat het vast beter!';
    box.innerHTML=`<div class="card endcard slidein"><div class="bigmark">${frac>=.5?MARK_OK:MARK_NO}</div><h3>${score} van ${Q.length} in één keer goed</h3><p>${msg}</p><button class="b primary" id="qz-again">↺ Opnieuw</button></div>`;
    if(frac>=.5) burst(box.querySelector('.bigmark'));
    $('#qz-again').onclick=start; postHeight();
  }
  next.onclick=()=>{ i++; show(); };
  start();
  return {};
})();

/* ---------- italicize math in all (dynamic) text ---------- */
const MROOT=root.querySelector('.app');
mathify(MROOT);
const mobs=new MutationObserver(ms=>{ for(const m of ms){ if(m.type==='characterData') mathify(m.target); else m.addedNodes.forEach(mathify); } mobs.takeRecords(); });
mobs.observe(MROOT,{childList:true,subtree:true,characterData:true});

/* ---------- tabs & loop ---------- */
const modules={overzicht:hero,snelheid:snel,quiz:quiz};
let current='overzicht';
function showTab(id){ if(!modules[id]) id='overzicht'; current=id;
  root.querySelectorAll('.tab').forEach(b=>b.setAttribute('aria-selected',b.dataset.tab===id));
  root.querySelectorAll('.panel').forEach(p=>p.hidden=p.id!=='tab-'+id);
  postHeight(); }
root.querySelectorAll('.tab').forEach(b=>b.onclick=()=>showTab(b.dataset.tab));
root.querySelectorAll('[data-go]').forEach(b=>b.onclick=()=>{ showTab(b.dataset.go); if(host.getBoundingClientRect().top<0) host.scrollIntoView({behavior:'smooth'}); });
if(params.get('hidenav')==='1') $('nav.tabs').hidden=true;
showTab(params.get('tab')||'overzicht');
let last=performance.now();
let visible=true; new IntersectionObserver(es=>{ visible=es[0].isIntersecting; },{rootMargin:'100px'}).observe(host);
function loop(now){ const dt=Math.min(0.05,(now-last)/1000); last=now; const m=modules[current]; if(visible&&m&&m.frame&&!document.hidden) m.frame(dt); requestAnimationFrame(loop); }
requestAnimationFrame(loop);
}
function init(){ document.querySelectorAll('[data-dicht-app]').forEach(mount); }
window.DichtApp={mount,init};
const H0=inlineHost(CS); if(H0) mount(H0);
if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();