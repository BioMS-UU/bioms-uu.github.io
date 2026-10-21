/* Zwermen-en-foerageren-explainer (Biologische Modellen en Statistiek, hoofdstuk 16).
 * Gebruik in Quarto (één regel, op de plek waar de app moet komen):
 *   <script src="Applets/zwermen_explainer.js"></script>                        -> hele app met tabbladen
 *   <script src="Applets/zwermen_explainer.js" data-tab="boids"></script>       -> alleen dat onderdeel
 *   <script src="Applets/zwermen_explainer.js" data-tab="boids" data-nav="1"></script> -> tabbladen, start op dat tabblad
 * data-tab: overzicht | boids | sweep | quiz
 * data-theme="dark|light" forceert een thema; anders volgt de app het boek (ook de donkere modus).
 * (Alternatief: <div data-zwerm-app data-tab="..."></div> plus het script één keer op de pagina.)
 */
(function(){
"use strict";
const CS=document.currentScript;
function inlineHost(cs){ if(!cs||!cs.dataset||cs.__appDone||!cs.parentNode) return null;
  const single=!!cs.dataset.tab;
  // a plain <script> with no data-tab becomes the full app with tabs, unless the page already uses <div data-zwerm-app>
  if(!single&&document.querySelector('[data-zwerm-app]')) return null;
  cs.__appDone=true;
  const d=document.createElement('div'); d.setAttribute('data-zwerm-app','');
  if(cs.dataset.tab) d.dataset.tab=cs.dataset.tab;
  if(single&&cs.dataset.nav!=='1') d.dataset.hidenav='1'; if(cs.dataset.theme) d.dataset.theme=cs.dataset.theme;
  cs.parentNode.insertBefore(d,cs); return d; }
if(window.ZwermApp){ const h=inlineHost(CS); if(h) window.ZwermApp.mount(h); window.ZwermApp.init(); return; }
const STYLE=`


:host{
  --bg: var(--bs-body-bg, #ffffff);
  --fg: var(--bs-body-color, #1d2327);
  --line: var(--bs-border-color, #d9e0dc);
  --card: color-mix(in srgb, var(--fg) 4%, var(--bg));
  --muted: color-mix(in srgb, var(--fg) 62%, var(--bg));
  --grid: color-mix(in srgb, var(--fg) 9%, var(--bg));
  --axis: color-mix(in srgb, var(--fg) 50%, var(--bg));
  --accent:#D55E00; --accent-soft:#D55E0022; --accent2:#0072B2; --blue:#0072B2; --warn:#555b61;
  --font: var(--bs-body-font-family, system-ui,-apple-system,\"Segoe UI\",Roboto,\"Helvetica Neue\",sans-serif);
  --mono: var(--bs-font-monospace, ui-monospace,SFMono-Regular,Menlo,Consolas,monospace);
  display:block; color:var(--fg); font-family:var(--font); font-size:.95em; line-height:1.5; margin:1rem 0;
}
:host([data-dark]){
  --bg: var(--bs-body-bg, #16191b);
  --fg: var(--bs-body-color, #e6e9e7);
  --line: var(--bs-border-color, #30383c);
  --accent:#F08A3E; --accent-soft:#F08A3E2B; --accent2:#56B4E9; --blue:#56B4E9; --warn:#c3cad0; --muted: color-mix(in srgb, var(--fg) 82%, var(--bg)); --axis: color-mix(in srgb, var(--fg) 62%, var(--bg));
}
.app{color:var(--fg)}
.ovb{display:inline-block;border-top:1.5px solid currentColor;line-height:1.05;padding-top:1px}
*{box-sizing:border-box}
h2{font-size:1.15em;margin:.2em 0 .4em;border:0;padding:0}
h3{font-size:1em;margin:1em 0 .3em}
p{margin:.35rem 0 .6rem}
b{font-weight:600}
.muted{color:var(--muted)}
.mono{font-family:var(--mono)}
.eqn,.formula{font-family:\"Latin Modern Math\",\"STIX Two Math\",\"Cambria Math\",\"Times New Roman\",Georgia,serif;font-size:1.06em;white-space:nowrap}
.mi{font-style:italic}
.mu{font-style:normal}
/* tabs */
nav.tabs{display:flex;gap:4px;flex-wrap:wrap;border-bottom:1px solid var(--line);margin-bottom:12px}
.tab{appearance:none;border:0;background:none;color:var(--muted);font:inherit;font-size:.92rem;padding:7px 11px;border-radius:8px 8px 0 0;cursor:pointer;border-bottom:2px solid transparent;margin-bottom:-1px}
.tab:hover{color:var(--fg)}
.tab[aria-selected=\"true\"]{color:var(--accent);border-bottom-color:var(--accent);font-weight:600}
/* layout */
.grid2{display:grid;grid-template-columns:1fr 1fr;gap:14px}
@media (max-width:680px){.grid2{grid-template-columns:1fr}}
.card{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:10px 12px}
canvas{display:block;width:100%;border-radius:8px}
.sq{display:block;width:100%;max-width:460px;aspect-ratio:1/1;margin:0 auto;image-rendering:pixelated}
.plot-half{aspect-ratio:16/7}
.plot{aspect-ratio:16/10}
.plot-wide{aspect-ratio:16/8}
@media (max-width:680px){.plot-wide{aspect-ratio:16/10}}
.controls{position:relative;display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:6px 16px;margin:8px 0}
.ctl label{display:flex;justify-content:space-between;font-size:.88rem;color:var(--muted)}
.ctl label span{color:var(--fg);font-family:var(--mono);font-size:.85rem}
.ctl label span.lbl{color:var(--muted);font-family:var(--font);font-size:.88rem}
input[type=range]{width:100%;accent-color:var(--accent)}
.btns{display:flex;gap:6px;flex-wrap:wrap;align-items:center;margin:6px 0}
button.b{appearance:none;font:inherit;font-size:.88rem;border:1px solid var(--line);background:var(--card);color:var(--fg);padding:5px 12px;border-radius:7px;cursor:pointer}
button.b:hover{border-color:var(--accent)}
button.b:disabled{opacity:.5;cursor:default}
button.b.primary{background:var(--accent);border-color:var(--accent);color:#fff}
.chk{font-size:.88rem;color:var(--muted);display:inline-flex;gap:5px;align-items:center;margin-right:10px}
.chk input{accent-color:var(--accent)}
.seg{display:inline-flex;border:1px solid var(--line);border-radius:8px;overflow:hidden;flex-wrap:wrap}
.seg button{appearance:none;font:inherit;font-size:.86rem;border:0;background:none;color:var(--muted);padding:5px 10px;cursor:pointer}
.seg button[aria-pressed=\"true\"]{background:var(--accent);color:#fff}
.callout{border-left:3px solid var(--accent);background:var(--accent-soft);padding:7px 11px;border-radius:0 8px 8px 0;margin:8px 0}
.callout.note{border-left-color:var(--accent2);background:color-mix(in srgb,var(--accent2) 10%,transparent);font-size:.92em}
.callout.warn{border-left-color:var(--accent2);background:color-mix(in srgb,var(--accent2) 13%,transparent)}
.formula{font-size:1.06em;display:inline-block;margin:2px 0}
/* overview */
.cards{display:flex;flex-wrap:wrap;gap:10px;margin-top:8px}
.ov{display:flex;flex-direction:column;gap:4px;flex:1 1 230px;min-width:0}
.ov .num{font-size:.75rem;color:var(--muted);font-family:var(--mono)}
.ov h4{margin:0;font-size:.95rem}
.ov p{margin:0;font-size:.88rem;flex:1}
.ov button{align-self:flex-start;margin-top:6px}
table.cmp{border-collapse:collapse;width:100%;font-size:.88rem;margin:6px 0}
table.cmp td,table.cmp th{border-bottom:1px solid var(--line);padding:5px 6px;text-align:left;vertical-align:top}
table.cmp th{color:var(--muted);font-weight:600}
details{margin-top:10px}
summary{cursor:pointer;color:var(--accent);font-weight:600}
/* flows */
.flows{display:grid;grid-template-columns:auto 1fr auto;gap:4px 8px;align-items:center;font-size:.86rem;margin-top:8px}
.bar{height:10px;border-radius:5px;background:var(--line);overflow:hidden}
.bar i{display:block;height:100%;width:0;border-radius:5px;transition:width .08s linear}
.legend{display:flex;gap:12px;font-size:.8rem;color:var(--muted);margin-top:4px;flex-wrap:wrap}
.dot{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:4px;vertical-align:-1px}
/* units */
.eq{display:flex;align-items:flex-start;justify-content:center;gap:10px;flex-wrap:wrap;margin:14px 0 6px}
.term{display:flex;flex-direction:column;align-items:center;gap:6px;min-width:90px}
.term .sym{height:2.6em;display:flex;align-items:center;font-size:1.45rem;font-family:\"Cambria Math\",\"STIX Two Math\",Georgia,serif;padding:4px 12px;border-radius:8px;border:2px solid transparent;transition:all .35s}
.term.hl .sym{border-color:var(--accent);background:var(--accent-soft)}
.term .unit{min-height:46px;font-size:.92rem;color:var(--accent2);opacity:0;transform:translateY(-6px);transition:all .45s;text-align:center}
.term .unit.show{opacity:1;transform:none}
.op{font-size:1.45rem;height:2.6em;display:flex;align-items:center;font-family:Georgia,serif}
.frac{display:inline-flex;flex-direction:column;align-items:center;vertical-align:middle;line-height:1.15;margin:0 2px}
.frac>span:first-child{border-bottom:1.5px solid currentColor;padding:0 3px}
.frac>span:last-child{padding:0 3px}
.derive{min-height:120px;display:flex;flex-direction:column;align-items:center;gap:8px;margin:6px 0}
.derive .ln{opacity:0;animation:fadein .5s forwards;font-size:1.05rem;display:flex;align-items:center;gap:8px;flex-wrap:wrap;justify-content:center}
@keyframes fadein{from{opacity:0;transform:translateY(6px)}to{opacity:1;transform:none}}
.cancel{position:relative;display:inline-block}
.cancel::after{content:\"\";position:absolute;left:-2px;top:50%;height:2px;background:var(--warn);width:0;animation:strike .5s forwards;animation-delay:inherit;transform:rotate(-12deg)}
@keyframes strike{to{width:calc(100% + 4px)}}
.stepper{display:flex;justify-content:center;align-items:center;gap:10px;margin-top:6px}
.dots{display:flex;gap:5px}
.dots i{width:8px;height:8px;border-radius:50%;background:var(--line);cursor:pointer}
.dots i.on{background:var(--accent)}
.steptext{min-height:4.5em;text-align:center;max-width:620px;margin:6px auto}
.term .unit.same{background:var(--accent-soft);border:1.5px solid var(--accent);border-radius:8px;padding:2px 8px;color:var(--fg);animation:same .9s ease-out}
@keyframes same{0%{transform:scale(1)}40%{transform:scale(1.12)}100%{transform:scale(1)}}
/* quiz */
.q{margin:10px 0;padding:10px 12px}
.q .opts{display:flex;flex-wrap:wrap;gap:6px;margin:6px 0}
.q .opts button{appearance:none;font:inherit;font-size:.92rem;border:1px solid var(--line);background:var(--bg);color:var(--fg);padding:5px 12px;border-radius:7px;cursor:pointer;min-height:38px}
.q .opts button.right{border-color:var(--accent);background:var(--accent-soft)}
.q .opts button.wrong{border-color:var(--warn);opacity:.7}
.q .fb{font-size:.88rem;display:none}
.q .fb.show{display:block}
.score{font-size:.9rem;color:var(--muted)}
/* animated marks */
.qhead{display:flex;gap:10px;justify-content:space-between;align-items:flex-start}
.qmark{position:relative;width:34px;height:34px;flex:none}
.mark{width:100%;height:100%;overflow:visible}
.mark circle,.mark path{fill:none;stroke-linecap:round;stroke-linejoin:round}
.mark circle{stroke-width:2.6;stroke-dasharray:88;stroke-dashoffset:88;animation:draw .45s ease-out forwards;transform:rotate(-90deg);transform-origin:center}
.mark path{stroke-width:3.4;stroke-dasharray:30;stroke-dashoffset:30;animation:draw .35s .4s ease-out forwards}
.mark.ok circle,.mark.ok path{stroke:var(--accent)}
.mark.no circle,.mark.no path{stroke:var(--warn)}
.mark.ok{animation:pop .35s .7s ease-out}
@keyframes draw{to{stroke-dashoffset:0}}
@keyframes pop{50%{transform:scale(1.22)}}
.burst{position:absolute;left:50%;top:50%;width:0;height:0;pointer-events:none}
.burst i{position:absolute;width:6px;height:6px;margin:-3px;border-radius:50%;background:var(--accent);opacity:0;animation:burst .7s .55s ease-out forwards}
.burst i:nth-child(odd){background:var(--accent2)}
@keyframes burst{0%{opacity:1;transform:rotate(var(--a)) translateY(0)}100%{opacity:0;transform:rotate(var(--a)) translateY(calc(-1 * var(--d,26px)))}}
.shake{animation:shake .4s}
@keyframes shake{20%,60%{transform:translateX(-5px)}40%,80%{transform:translateX(5px)}}
.q .fb.ok{color:var(--fg)}
.q .fb.no{color:var(--warn)}
.q .opts button.right{font-weight:600}
.slidein{animation:fadein .35s}
.progress{height:6px;background:var(--line);border-radius:3px;overflow:hidden;margin:6px 0 10px}
.progress i{display:block;height:100%;width:0;background:var(--accent);transition:width .4s}
.endcard{text-align:center;padding:24px 12px}
.bigmark{position:relative;width:84px;height:84px;margin:0 auto 6px;--d:70px}
.bigmark .burst i{width:9px;height:9px;margin:-4.5px}
@media (prefers-reduced-motion:reduce){.mark,.mark *,.burst i,.shake{animation-duration:.01s!important;animation-delay:0s!important}}
.ctl.locked{opacity:.5}
.ctl.locked input{cursor:not-allowed}
.toast{position:absolute;left:50%;bottom:calc(100% + 10px);transform:translate(-50%,6px);z-index:5;background:var(--fg);color:var(--bg);padding:9px 13px;border-radius:9px;font-size:.88rem;line-height:1.4;width:max-content;max-width:min(540px,94%);box-shadow:0 6px 20px rgba(0,0,0,.25);opacity:0;pointer-events:none;transition:opacity .2s,transform .2s}
.toast::after{content:\"\";position:absolute;left:50%;top:100%;margin-left:-7px;border:7px solid transparent;border-top-color:var(--fg)}
.toast.show{opacity:1;transform:translate(-50%,0)}
.toast b{font-weight:700}
.ptitle{font-weight:600;font-size:.92rem;margin:0 0 4px}
/* quiz scoreboard */
.qboard{display:flex;flex-wrap:wrap;gap:5px;align-items:center;margin:-2px 0 10px}
.qdot{appearance:none;background:none;font:inherit;padding:0;cursor:pointer;width:24px;height:24px;border-radius:50%;display:inline-flex;align-items:center;justify-content:center;font-size:.72rem;border:1.5px solid var(--line);color:var(--muted);transition:transform .15s}
.qdot:not(:disabled):hover{transform:scale(1.12)}
.qdot:disabled{cursor:default;opacity:.55}
.qdot.here{box-shadow:0 0 0 2px var(--bg),0 0 0 3.5px var(--fg)}
.qdot.fin{border-color:transparent;font-size:.85rem}
.qdot.ok{background:var(--accent);border-color:var(--accent);color:#fff;font-weight:700}
.qdot.retry{border-color:var(--accent);color:var(--accent);font-weight:700}
.qdot.cur{border-color:var(--fg);color:var(--fg);font-weight:700}
.qreset{margin-left:auto;appearance:none;border:0;background:none;color:var(--muted);font:inherit;font-size:.8rem;cursor:pointer;text-decoration:underline}
.tabcount{font-size:.75em;font-weight:600;background:var(--accent-soft);color:var(--accent);border-radius:9px;padding:1px 6px;margin-left:2px}
[hidden]{display:none!important}


/* exponentiële groei: extra */
pre.code{font-family:var(--mono);font-size:.84rem;line-height:1.6;background:var(--card);border:1px solid var(--line);border-radius:8px;padding:8px 6px;margin:6px 0;overflow-x:auto;white-space:pre;color:var(--fg)}
pre.code .ln{display:block;padding:0 8px;border-radius:5px;transition:background .25s}
pre.code .ln.on{background:var(--accent-soft);box-shadow:inset 3px 0 0 var(--accent)}
pre.code i{color:var(--muted);font-style:italic}
pre.code em{font-style:normal;color:var(--accent);font-weight:600}
.vec{display:flex;flex-wrap:wrap;gap:4px;margin:6px 0;font-family:var(--mono);font-size:.78rem}
.vec .cell{display:flex;flex-direction:column;align-items:center;min-width:54px;padding:3px 5px;border:1px solid var(--line);border-radius:6px;background:var(--bg);transition:all .2s}
.vec .cell b{font-weight:600;color:var(--muted);font-size:.68rem}
.vec .cell.rd{border-color:var(--accent2);background:color-mix(in srgb,var(--accent2) 14%,transparent)}
.vec .cell.wr{border-color:var(--accent);background:var(--accent-soft);transform:scale(1.06)}
.vec .cell.empty{opacity:.4;border-style:dashed}
.guess{display:flex;gap:8px;flex-wrap:wrap;margin:10px 0 4px}
.guess button{font-size:1rem;padding:8px 20px;min-height:42px}
.guess button.right{border-color:var(--accent);background:var(--accent-soft);font-weight:600}
.guess button.wrong{border-color:var(--warn);opacity:.6}
.callout.intro{margin:0 0 14px}
.callout.intro p{margin:.3rem 0}
.subst{display:block;text-align:center;margin:8px 0 2px;min-height:1.5em;font-size:1.05em}
/* zwermen: extra */
.sq.smooth{image-rendering:auto}
.rules{display:grid;grid-template-columns:auto 1fr;gap:4px 10px;font-size:.88rem;margin:6px 0 10px;align-items:baseline}
.rules b{font-family:var(--mono);font-size:.8rem;color:var(--accent);font-weight:600}
.swlbl{font-size:.85rem;min-height:2.6em;margin-top:4px}
.hm-legend{display:flex;align-items:center;gap:6px;font-size:.8rem;color:var(--muted);margin-top:4px}
.hm-legend i{display:inline-block;width:70px;height:9px;border-radius:4px;background:linear-gradient(90deg,color-mix(in srgb,var(--accent) 8%,var(--bg)),var(--accent))}
`;
const HTML=`
<nav class="tabs" role="tablist">
  <button class="tab" data-tab="overzicht">In één oogopslag</button>
  <button class="tab" data-tab="boids">Boids</button>
  <button class="tab" data-tab="sweep">Parameter-sweep</button>
  <button class="tab" data-tab="quiz">Quiz ✓</button>
</nav>

<section class="panel" id="tab-overzicht">
  <h2>Hoofdstuk 16 in één oogopslag</h2>
  <div class="grid2">
    <div>
      <p>Bij elk model kies je of de <b>toestand, tijd en ruimte</b> <b>discreet</b> (hokjes, hele aantallen, stappen) of <b>continu</b> (elke waarde mogelijk) zijn. Die keuze doet ertoe: een ODE kan je een "atto-vos" van 10<sup class="nomath">−18</sup> vos opleveren, en een grid kan geen vloeiende beweging laten zien.</p>
      <p>In dit hoofdstuk bewegen individuen vrij door een <b>continue ruimte</b>. Ze volgen alleen <b>simpele, lokale regels</b> (aantrekken, afstoten, uitlijnen, naar de groep toe), en toch ontstaan er complexe patronen: wormen in particle life, murmuraties van spreeuwen, groepjes aapjes, mierenpaadjes. Zo'n patroon zit niet in één individu, maar ontstaat uit de interacties: een <b>emergente eigenschap</b>.</p>
      <p>Omdat zulke modellen veel parameters hebben, bestudeer je ze met een <b>parameter-sweep</b>: je laat de simulatie systematisch over een reeks parameterwaarden lopen en zet de uitkomst in een grafiek of <i>heatmap</i>. Dat is het simulatie-equivalent van een bifurcatie-diagram.</p>
    </div>
    <div class="card">
      <canvas id="hero-sim" class="sq smooth" style="max-width:320px"></canvas>
      <div class="muted" id="hero-lbl" style="font-size:.85rem;text-align:center;margin-top:6px;min-height:3.9em"></div></div>
  </div>

  <div class="cards">
    <div class="card ov"><span class="num">16.1</span><h4>Continu of discreet?</h4>
      <p>Tijd, aantallen en ruimte kun je elk discreet of continu modelleren. Een ODE rekent met continue dichtheden, en dat gaf bij de kanaaltunnel een voorspelling met 10<sup class="nomath">−18</sup> vos: de <b>atto-vos</b>. Een heel klein stukje vos kan geen rabiës verspreiden.</p></div>
    <div class="card ov"><span class="num">16.2</span><h4>Particle life</h4>
      <p>Honderd punten met decimale coördinaten die elkaar van een afstand aantrekken en van dichtbij afstoten. In <b>pseudocode</b> beschreven, niet in <span class="nomath">R</span>-code. Met meerdere kleuren ontstaan draaiende wormen die lijken te "beslissen": een <b>emergente eigenschap</b> van een <i>toy model</i>.</p></div>
    <div class="card ov"><span class="num">16.3</span><h4>Zwermpatronen in boids</h4>
      <p>Elke vogel ziet alleen nabije buren en volgt drie regels: <b>lijn je richting uit</b> met de groep, <b>beweeg naar het midden</b> van de groep, <b>voorkom botsingen</b>. Samen geven die murmuraties. Hiermee wordt echt onderzoek gedaan, bijvoorbeeld naar spreeuwen en haviken.</p>
      <button class="b" data-go="boids">Draai aan de regels →</button></div>
    <div class="card ov"><span class="num">16.4</span><h4>Aapjes met gedrag</h4>
      <p>Gedragsregels in pseudocode: te weinig apen in de buurt en geen enorme honger? Ren terug naar de groep. Genoeg apen en honger? Loop een willekeurige kant op naar fruit. Zes stukken gevonden? Ga zitten en eet. Virtueel experimenteren is snel, goedkoop en verstoort geen echte apen.</p></div>
    <div class="card ov"><span class="num">16.5</span><h4>Parameter-sweeps</h4>
      <p>Varieer één of twee parameters systematisch en classificeer per waarde de uitkomst, bijvoorbeeld in een tabel of heatmap. Zo zie je welke parameters ertoe doen en of gedrag alleen bij heel specifieke waarden optreedt. Vergelijk het met het bifurcatie-diagram van het predator-prooimodel.</p>
      <button class="b" data-go="sweep">Doe een sweep →</button></div>
    <div class="card ov"><span class="num">16.6</span><h4>Foeragerende mieren</h4>
      <p>Mieren zien elkaar niet, maar ruiken <b>feromonen</b> die ze achterlaten en die ook weer afbreken of verdunnen. Via de omgeving ontstaan zo paadjes naar het voedsel, maar ook de beruchte <b>mierenkring</b> (<i>ant mill</i>) waarin ze eindeloos achter elkaar aan lopen.</p></div>
    <div class="card ov"><span class="num">Opg.</span><h4>Aapjes die gele pixels eten</h4>
      <p>Varieer de eetsnelheid van de aapjes, herhaal elk experiment vijf keer en noteer hoeveel fruit er na 1000 tijdstappen is gevonden. Die tabel analyseer je later bij Statistiek: is het verschil echt, of toeval?</p></div>
    <div class="card ov"><span class="num">✓</span><h4>Test jezelf</h4>
      <p>Tien korte vragen over continu en discreet, emergentie, boids, pseudocode, parameter-sweeps en feromonen. Bij een fout antwoord mag je het opnieuw proberen.</p>
      <button class="b primary" data-go="quiz">Start de quiz →</button></div>
  </div>

  <details>
    <summary>Terminologie</summary>
    <table class="cmp">
      <tr><th>Nederlands</th><th>Engels</th><th>Betekenis</th></tr>
      <tr><td>Discrete variabelen</td><td>Discrete variables</td><td>Variabelen die alleen waarden met een vaste afstand kunnen innemen, meestal 1, 2, 3, <span class="nomath">etc.</span></td></tr>
      <tr><td>Continue variabelen</td><td>Continuous variables</td><td>Variabelen die alle waarden in een bereik kunnen innemen, bijvoorbeeld alles tussen 0 en 1.</td></tr>
      <tr><td>Discrete ruimte</td><td>Discrete space</td><td>Ruimte opgedeeld in vaste posities, zoals een grid van vierkanten (4 of 8 buren) of hexagons (6 buren).</td></tr>
      <tr><td>Continue ruimte</td><td>Continuous space</td><td>Ruimte zonder vaste posities: individuen kunnen elke positie innemen. Vloeiende beweging, maar zwaarder om door te rekenen.</td></tr>
      <tr><td>Pseudocode</td><td>Pseudocode</td><td>Een verbale omschrijving van wat een programma doet (en hoe), zonder syntax, functienamen of komma's.</td></tr>
      <tr><td>Emergente eigenschappen</td><td>Emergent properties</td><td>Eigenschappen van een systeem die ontstaan door interacties tussen de onderdelen en niet in de losse onderdelen zitten.</td></tr>
    </table>
  </details>
</section>

<section class="panel" id="tab-boids" hidden>
  <h2>Zwermpatronen in boids</h2>
  <p>Elke vogel ziet alleen de buren binnen zijn <b>waarnemingsstraal</b>, en volgt met die buren drie simpele regels. Er is geen leider en niemand heeft overzicht over de hele zwerm. Toch ontstaat er een zwerm: een <b>emergente eigenschap</b>. Zet regels aan en uit en kijk wat er met het patroon gebeurt. De wereld loopt rond: wat er rechts uit vliegt, komt links weer binnen.</p>
  <div class="rules">
    <b>regel 1</b><span>Probeer je richting uit te lijnen met de groep</span>
    <b>regel 2</b><span>Beweeg je naar het midden van de groep toe</span>
    <b>regel 3</b><span>Probeer te voorkomen dat je met anderen botst</span>
  </div>
  <div class="grid2">
    <div class="card" style="align-self:start"><canvas id="bo-sim" class="sq smooth" style="max-width:440px"></canvas>
      <div class="muted" id="bo-lbl" style="font-size:.85rem;text-align:center;margin-top:6px;min-height:1.3em"></div></div>
    <div>
      <div class="ptitle">Presets</div>
      <div class="btns" id="bo-presets">
        <button class="b" data-preset="murm">Alle drie de regels</button>
        <button class="b" data-preset="al">Alleen uitlijnen</button>
        <button class="b" data-preset="co">Alleen naar het midden</button>
        <button class="b" data-preset="sep">Alleen botsen vermijden</button>
        <button class="b" data-preset="blind">Niemand ziet elkaar</button>
      </div>
      <div class="controls">
        <div class="ctl"><label><span class="lbl">regel 1: uitlijnen</span> <span id="bo-al-v"></span></label><input type="range" id="bo-al" min="0" max="1" step="0.05" value="1"></div>
        <div class="ctl"><label><span class="lbl">regel 2: naar het midden</span> <span id="bo-co-v"></span></label><input type="range" id="bo-co" min="0" max="1" step="0.05" value="1"></div>
        <div class="ctl"><label><span class="lbl">regel 3: botsen vermijden</span> <span id="bo-sep-v"></span></label><input type="range" id="bo-sep" min="0" max="1" step="0.05" value="1"></div>
        <div class="ctl"><label><span class="lbl">waarnemingsstraal</span> <span id="bo-r-v"></span></label><input type="range" id="bo-r" min="0.01" max="0.3" step="0.01" value="0.12"></div>
        <div class="ctl"><label><span class="lbl">ruis in de richting</span> <span id="bo-noise-v"></span></label><input type="range" id="bo-noise" min="0" max="2" step="0.05" value="0.3"></div>
      </div>
      <div class="btns">
        <button class="b" id="bo-play">⏸ Pauze</button>
        <button class="b" id="bo-reset">↺ Nieuwe start</button>
        <label class="chk"><input type="checkbox" id="bo-ring" checked> toon wie één vogel ziet</label>
      </div>
      <div class="card"><div class="ptitle">Hoe goed zijn ze uitgelijnd? De "polarisatie"</div><canvas id="bo-plot" class="plot-half"></canvas>
        <div class="muted" style="font-size:.82rem;margin-top:4px">Om te meten hoe goed de vogels dezelfde kant op vliegen, gebruiken we hier een getal dat <b>polarisatie</b> heet: de lengte van de gemiddelde vliegrichting. Vliegt iedereen dezelfde kant op, dan is die 1; vliegen ze alle kanten op, dan heffen de richtingen elkaar op en is die 0. Dit begrip komt niet in het hoofdstuk voor en is <b>geen leerstof</b>; het is alleen een handige maat om de simulatie mee samen te vatten.</div></div>
    </div>
  </div>
  <div class="callout" id="bo-text"></div>
</section>

<section class="panel" id="tab-sweep" hidden>
  <h2>Parameter-sweep</h2>
  <p>Een individual-based model zoals boids heeft veel parameters. Bij een <b>parameter-sweep</b> laat je de simulatie automatisch over een reeks waarden van een parameter lopen, meet je per waarde de uitkomst, en zet je die in een grafiek. Hier is de uitkomst de <b>polarisatie</b> (gemiddeld over de laatste 100 van 400 tijdstappen): de maat voor hoe goed de vogels zijn uitgelijnd uit het tabblad Boids, van 0 (alle kanten op) tot 1 (allemaal dezelfde kant op). Die maat is geen leerstof, de werkwijze van de sweep wel. Omdat de start willekeurig is en er ruis in zit, verschillen herhalingen van elkaar: daarom draait elke waarde <b>drie keer</b>.</p>
  <div class="btns">
    <span class="seg" id="sw-mode"><button data-m="1" aria-pressed="true">1 parameter</button><button data-m="2">2 parameters (heatmap)</button></span>
    <span class="seg" id="sw-par"><button data-p="R" aria-pressed="true">waarnemingsstraal</button><button data-p="noise">ruis</button><button data-p="al">uitlijnen</button></span>
  </div>
  <div class="btns">
    <button class="b primary" id="sw-run">▶ Start de sweep</button>
    <button class="b" id="sw-clear">↺ Wis resultaten</button>
    <span class="muted" id="sw-prog" style="font-size:.85rem"></span>
  </div>
  <div class="grid2">
    <div class="card"><div class="ptitle" id="sw-ptitle">Uitkomst per parameterwaarde</div><canvas id="sw-plot" class="plot"></canvas>
      <div class="hm-legend" id="sw-legend" hidden><span>lage polarisatie</span><i></i><span>hoge polarisatie</span></div></div>
    <div class="card"><div class="ptitle">Huidige simulatie</div><canvas id="sw-sim" class="sq smooth" style="max-width:250px"></canvas>
      <div class="muted swlbl" id="sw-lbl" style="text-align:center"></div></div>
  </div>
  <div class="callout" id="sw-text"></div>
</section>

<section class="panel" id="tab-quiz" hidden>
  <h2>Quiz: zwermen en foerageren</h2>
  <div style="display:flex;justify-content:space-between;font-size:.88rem" class="muted"><span id="qz-count"></span><span id="qz-score"></span></div>
  <div class="progress"><i id="qz-prog"></i></div>
  <div id="qz-box"></div>
  <div class="btns" style="justify-content:flex-end"><button class="b primary" id="qz-next" hidden>Volgende vraag →</button></div>
</section>
`;
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
const SUP='⁰¹²³⁴⁵⁶⁷⁸⁹';
const sup=n=>String(n).split('').map(c=>c==='-'?'⁻':SUP[+c]).join('');
/* labels on a log axis: plain numbers up to 10 000, then powers of ten */
const logLab=k=>k>=0&&k<=4?String(Math.pow(10,k)).replace(/\B(?=(\d{3})+$)/g,'.'):'10'+sup(k);
function big(v){ if(v<1e6) return Math.round(v).toLocaleString('nl-NL'); const e=Math.floor(Math.log10(v)), m=v/Math.pow(10,e); return m.toFixed(1).replace('.',',')+'·10'+sup(e); }
/* o.ylog: y-values are log10(N); ticks at whole powers of ten */
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
  let yt; if(o.ylog){ const st=Math.max(1,Math.round(niceStep(o.ymax-o.ymin,o.ny||5))); yt=[]; for(let k=Math.ceil(o.ymin-1e-9); k<=o.ymax+1e-9; k+=st) yt.push(k); } else yt=niceTicks(o.ymin,o.ymax,o.ny||5);
  for(const t of yt){ const y=Y(t); ctx.beginPath(); ctx.moveTo(p.l,y); ctx.lineTo(p.l+pw,y); ctx.stroke(); ctx.fillText(o.ylog?logLab(t):fmt(t),p.l-6,y); }
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
function dots(f,pts,color,r=3){ for(const [x,y] of pts){ if(isFinite(y)) disc(f.ctx,f.X(x),f.Y(y),r,color,col('bg'),1); } }
/* log-scale range with a little padding; ymin/ymax are log10 values */
function logRange(vals){ const v=vals.filter(x=>x>0).map(Math.log10); let a=Math.min(...v), b=Math.max(...v); if(b-a<0.5){ const m=(a+b)/2; a=m-0.25; b=m+0.25; } const pad=0.06*(b-a); return {ymin:a-pad,ymax:b+pad}; }
/* series plotted on a lin or log y-axis */
function series(c,vals,opts){
  const T=vals.length-1, x0=opts.x0||0, lg=!!opts.log;
  const o=lg?{...logRange(vals),ylog:true}:{ymin:0,ymax:niceCeil(Math.max(...vals)*1.05)};
  const f=frame(c,{xmin:x0,xmax:x0+T,...o,xlab:opts.xlab||'tijd (stappen)',ylab:opts.ylab||(lg?'aantal (log-schaal)':'aantal'),pad:{l:66}});
  const pts=vals.map((v,i)=>[x0+i,lg?Math.log10(v):v]).slice(0,opts.n!=null?opts.n:vals.length);
  poly(f,pts,col('accent'),2.4); if(opts.dots!==false&&pts.length<=70) dots(f,pts,col('accent'),T>30?2.4:3.2);
  f.ctx.restore(); return f;
}
const pct=r=>{ const p=(r-1)*100; return (p>0?'+':'')+(+p.toFixed(1))+'%'; };

/* ---------- math typesetting: variables & parameters italic, the "d" of d/dt upright ---------- */
function mathRuns(s){
  const runs=[], push=(t,k)=>{ if(!t) return; const l=runs[runs.length-1]; if(l&&l.k===k) l.t+=t; else runs.push({t,k}); };
  const re=/[A-Za-zÀ-ÖØ-öø-ɏͰ-Ͽ]+/g; let last=0, mt;
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
const MATH_SKIP='script,style,nav,canvas,svg,input,textarea,pre,.code,.vec,.guess,.frac,.mi,.mu,.nomath,.tab,.num';
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
const shuffle=a=>{ a=a.slice(); for(let k=a.length-1;k>0;k--){ const j=Math.floor(Math.random()*(k+1)); [a[k],a[j]]=[a[j],a[k]]; } return a; };
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
   BOIDS: gedeelde simulatiekern (continue ruimte 0..1, de wereld loopt rond)
   ========================================================= */
function rng(seed){ let s=(seed>>>0)||1; return ()=>{ s=(s*1103515245+12345)%2147483648; return s/2147483648; }; }
function makeFlock(n,seed){ const r=rng(seed), F=[]; for(let i=0;i<n;i++){ const a=r()*2*Math.PI; F.push({x:r(),y:r(),dx:Math.cos(a),dy:Math.sin(a)}); } F.rnd=r; return F; }
const wrapD=d=>d>0.5?d-1:d<-0.5?d+1:d;
/* P: R = waarnemingsstraal, al/co/sep = gewicht van regel 1/2/3 (0..1), noise = ruis in de richting (rad), speed = stapgrootte */
function stepFlock(F,P){
  const n=F.length, R2=P.R*P.R, SR=Math.min(P.R,0.03), SR2=SR*SR, r=F.rnd, nd=new Float64Array(2*n);
  for(let i=0;i<n;i++){ const b=F[i]; let cnt=0,ax=0,ay=0,cx=0,cy=0,sx=0,sy=0;
    for(let j=0;j<n;j++){ if(j===i) continue; const o=F[j], dx=wrapD(o.x-b.x), dy=wrapD(o.y-b.y), d2=dx*dx+dy*dy;
      if(d2<R2){ cnt++; ax+=o.dx; ay+=o.dy; cx+=dx; cy+=dy;                       // regel 1: richting van de buren, regel 2: midden van de buren
        if(d2<SR2){ const d=Math.sqrt(d2)+1e-5, w=(1-d/SR)/d; sx-=dx*w; sy-=dy*w; } } }   // regel 3: weg van wie te dichtbij is
    let fx=b.dx, fy=b.dy;
    if(cnt){ const al=Math.hypot(ax,ay)||1, cl=Math.hypot(cx,cy)||1, sl=Math.hypot(sx,sy)||1;
      fx+=P.al*0.35*ax/al+P.co*0.25*cx/cl+P.sep*0.6*sx/sl; fy+=P.al*0.35*ay/al+P.co*0.25*cy/cl+P.sep*0.6*sy/sl; }
    const a=Math.atan2(fy,fx)+(r()-0.5)*2*P.noise; nd[2*i]=Math.cos(a); nd[2*i+1]=Math.sin(a); }
  for(let i=0;i<n;i++){ const b=F[i]; b.dx=nd[2*i]; b.dy=nd[2*i+1]; b.x=(b.x+P.speed*b.dx+1)%1; b.y=(b.y+P.speed*b.dy+1)%1; }
}
/* orde-maat: lengte van de gemiddelde richting (1 = iedereen dezelfde kant op, 0 = alle kanten op) */
function polar(F){ let x=0,y=0; for(const b of F){ x+=b.dx; y+=b.dy; } return F.length?Math.hypot(x,y)/F.length:0; }
function drawFlock(c,F,P,ring){
  const {ctx,w,h}=fitCanvas(c), s=Math.min(w,h); ctx.clearRect(0,0,w,h);
  ctx.fillStyle=col('card'); ctx.fillRect(0,0,w,h);
  const L=Math.max(4,s*0.016), b0=F[0]; let seen=null;
  if(ring&&b0){ seen=new Set(); const R2=P.R*P.R; for(let j=1;j<F.length;j++){ const dx=wrapD(F[j].x-b0.x), dy=wrapD(F[j].y-b0.y); if(dx*dx+dy*dy<R2) seen.add(j); }
    ctx.save(); ctx.strokeStyle=col('accent2'); ctx.setLineDash([4,3]); ctx.lineWidth=1.2; ctx.beginPath(); ctx.arc(b0.x*s,b0.y*s,P.R*s,0,2*Math.PI); ctx.stroke(); ctx.restore(); }
  F.forEach((b,i)=>{ const x=b.x*s, y=b.y*s, hl=seen&&(i===0||seen.has(i));
    ctx.fillStyle=hl?col('accent2'):col('accent'); ctx.beginPath();
    ctx.moveTo(x+b.dx*L,y+b.dy*L); ctx.lineTo(x-b.dx*L*0.7-b.dy*L*0.5,y-b.dy*L*0.7+b.dx*L*0.5); ctx.lineTo(x-b.dx*L*0.7+b.dy*L*0.5,y-b.dy*L*0.7-b.dx*L*0.5); ctx.closePath(); ctx.fill(); });
  ctx.strokeStyle=col('line'); ctx.lineWidth=1; ctx.strokeRect(0.5,0.5,w-1,h-1);
}
const PRESETS={
  murm:{al:1,co:1,sep:1,R:0.12,noise:0.3,name:'Alle drie de regels',
    why:'Uitlijnen, naar het midden en botsen vermijden samen: een zwerm die als één geheel beweegt en toch blijft golven en splitsen. Dit zijn de murmuraties uit het hoofdstuk.'},
  al:{al:1,co:0,sep:0,R:0.12,noise:0.3,name:'Alleen uitlijnen',
    why:'Alleen regel 1: na een tijdje vliegt iedereen dezelfde kant op (hoge polarisatie), maar de vogels blijven over het hele veld verspreid. Wel orde, geen groep.'},
  co:{al:0,co:1,sep:0.3,R:0.12,noise:0.3,name:'Alleen naar het midden',
    why:'Vooral regel 2: de vogels klonteren samen tot dichte groepjes die rondtollen. Wel groepen, maar weinig orde in de richting: de polarisatie blijft laag.'},
  sep:{al:0,co:0,sep:1,R:0.12,noise:0.3,name:'Alleen botsen vermijden',
    why:'Alleen regel 3: de vogels duwen elkaar weg en verspreiden zich als gasmoleculen over het veld. Geen zwerm, geen orde.'},
  blind:{al:1,co:1,sep:1,R:0.01,noise:0.3,name:'Niemand ziet elkaar',
    why:'De regels staan aan, maar met een piepkleine waarnemingsstraal ziet een vogel vrijwel nooit een buur. Iedereen vliegt in zijn eigen (ruisige) rechte lijn. Zonder interacties geen emergentie.'}};
const SPEED=0.005;

/* =========================================================
   IN ÉÉN OOGOPSLAG: een kleine zwerm die de presets afwisselt
   ========================================================= */
const hero=(()=>{
  const c=$('#hero-sim'), lbl=$('#hero-lbl'), ORDER=['blind','al','murm'];
  let k=0, F=makeFlock(45,11), P={...PRESETS[ORDER[0]],speed:SPEED}, acc=0;
  const TXT={blind:'<b>Niemand ziet elkaar.</b> Zonder interacties doet elke vogel maar wat.',
    al:'<b>Regel 1: lijn je uit met je buren.</b> Iedereen vliegt al snel dezelfde kant op, maar er is nog geen groep.',
    murm:'<b>Alle drie de regels.</b> Uitlijnen + naar het midden + botsen vermijden: een zwerm. Niemand heeft de leiding, het patroon is <b>emergent</b>.'};
  function frameFn(dt){
    acc+=dt; if(acc>8){ acc=0; k=(k+1)%ORDER.length; P={...PRESETS[ORDER[k]],speed:SPEED}; if(k===0) F=makeFlock(45,11+Math.floor(Math.random()*1000)); }
    for(let s=0;s<2;s++) stepFlock(F,P);
    drawFlock(c,F,P,false);
    lbl.innerHTML=TXT[ORDER[k]]+` <span class="mono nomath">polarisatie ${polar(F).toFixed(2)}</span>`;
  }
  return {frame:frameFn};
})();

/* =========================================================
   BOIDS: draai aan de drie regels
   ========================================================= */
const boids=(()=>{
  const c=$('#bo-sim'), pc=$('#bo-plot'), lbl=$('#bo-lbl'), txt=$('#bo-text'), ring=$('#bo-ring'), bPlay=$('#bo-play'), bReset=$('#bo-reset');
  const S={al:$('#bo-al'),co:$('#bo-co'),sep:$('#bo-sep'),R:$('#bo-r'),noise:$('#bo-noise')};
  const V={al:$('#bo-al-v'),co:$('#bo-co-v'),sep:$('#bo-sep-v'),R:$('#bo-r-v'),noise:$('#bo-noise-v')};
  const N=60, HIST=300; let F=makeFlock(N,3), P={speed:SPEED}, hist=[], playing=true, preset='murm', t=0;
  function readP(){ for(const k in S) P[k]=+S[k].value; V.al.textContent=P.al.toFixed(2); V.co.textContent=P.co.toFixed(2); V.sep.textContent=P.sep.toFixed(2); V.R.textContent=P.R.toFixed(2); V.noise.textContent=P.noise.toFixed(2); }
  function setText(){ const pr=preset&&PRESETS[preset];
    txt.innerHTML=pr?`<b>${pr.name}.</b> ${pr.why}`:`<b>Eigen instellingen.</b> Let op de polarisatie rechts: wordt de zwerm geordend (richting 1) of blijft hij wanordelijk (richting 0)? Regel 1 zorgt vooral voor orde in de richting, regel 2 voor groepjes, regel 3 houdt de groepjes uit elkaar. Met meer ruis of een kleinere waarnemingsstraal valt de orde weer uit elkaar.`; }
  function applyPreset(k){ preset=k; const pr=PRESETS[k]; for(const j of ['al','co','sep','R','noise']) S[j].value=pr[j]; readP(); setText();
    root.querySelectorAll('#bo-presets button').forEach(b=>b.classList.toggle('primary',b.dataset.preset===k)); }
  function reset(){ F=makeFlock(N,Math.floor(Math.random()*1e6)); hist=[]; t=0; }
  function setPlay(p){ playing=p; bPlay.textContent=p?'⏸ Pauze':'▶ Afspelen'; }
  function plot(){
    const f=frame(pc,{xmin:0,xmax:HIST,ymin:0,ymax:1,ny:4,xlab:'laatste '+HIST+' tijdstappen',ylab:'polarisatie',pad:{l:46,b:34}});
    poly(f,hist.map((v,i)=>[i,v]),col('accent'),2); f.ctx.restore(); }
  for(const k in S) S[k].oninput=()=>{ preset=null; readP(); setText(); root.querySelectorAll('#bo-presets button').forEach(b=>b.classList.remove('primary')); };
  root.querySelectorAll('#bo-presets button').forEach(b=>b.onclick=()=>applyPreset(b.dataset.preset));
  bPlay.onclick=()=>setPlay(!playing); bReset.onclick=reset;
  applyPreset('murm');
  function frameFn(){
    if(playing){ for(let s=0;s<2;s++){ stepFlock(F,P); t++; } hist.push(polar(F)); if(hist.length>HIST) hist.shift(); }
    drawFlock(c,F,P,ring.checked); plot();
    lbl.innerHTML=`${N} vogels · tijdstap ${t} · polarisatie <span class="mono nomath">${polar(F).toFixed(2)}</span>`;
  }
  return {frame:frameFn};
})();

/* =========================================================
   PARAMETER-SWEEP: de simulatie loopt automatisch over een reeks waarden
   ========================================================= */
const sweep=(()=>{
  const pc=$('#sw-plot'), sc=$('#sw-sim'), lbl=$('#sw-lbl'), prog=$('#sw-prog'), txt=$('#sw-text'), bRun=$('#sw-run'), bClear=$('#sw-clear'),
        segM=$('#sw-mode'), segP=$('#sw-par'), ptitle=$('#sw-ptitle'), legend=$('#sw-legend');
  const BASE={al:1,co:1,sep:1,R:0.12,noise:0.3,speed:SPEED}, N=50, T=400, M=100, REPS=3;
  const PARS={R:{lab:'waarnemingsstraal',vals:[0.02,0.04,0.06,0.08,0.1,0.12,0.15,0.2,0.25,0.3],
      why:'Bij een kleine waarnemingsstraal ziet een vogel bijna nooit een buur: geen interacties, lage polarisatie. Rond een straal van ongeveer 0,06 tot 0,08 slaat het gedrag om en ontstaat een geordende zwerm. Zo\'n omslagpunt is het simulatie-equivalent van een <b>bifurcatie</b>. Merk op hoe de drie herhalingen juist rond de omslag het meest verschillen: daar beslist het toeval.'},
    noise:{lab:'ruis in de richting',vals:[0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25],
      why:'Weinig ruis: de vogels lijnen zich netjes uit en de polarisatie is hoog. Hoe meer ruis, hoe meer elke vogel zijn eigen gang gaat, tot de orde rond een ruis van ongeveer 1 instort. Ook dit is een omslag tussen twee kwalitatief verschillende toestanden, net als in een bifurcatie-diagram.'},
    al:{lab:'gewicht van regel 1 (uitlijnen)',vals:[0,0.1,0.2,0.3,0.4,0.5,0.6,0.8,1],
      why:'Zonder regel 1 ontstaan wel groepjes (regel 2), maar geen gemeenschappelijke richting. Hoe zwaarder het uitlijnen weegt, hoe hoger de polarisatie. Let op de spreiding tussen de herhalingen: één run per waarde zou een vertekend beeld kunnen geven.'}};
  const HM={x:{lab:'waarnemingsstraal',vals:[0.02,0.06,0.1,0.14,0.18,0.22,0.26,0.3],d:0.04},y:{lab:'ruis in de richting',vals:[0,0.3,0.6,0.9,1.2,1.5,1.8,2.1],d:0.3}};
  let mode='1', par='R', jobs=[], results=[], cur=-1, F=null, lastF=null, t=0, acc=0, running=false, done=false, Pnow=null;
  function build(){ jobs=[]; results=[]; cur=-1; running=false; done=false; F=null; lastF=null; t=0; Pnow=null;
    if(mode==='1'){ PARS[par].vals.forEach((v,ix)=>{ for(let r=0;r<REPS;r++) jobs.push({ix,rep:r,x:v,P:{...BASE,[par]:v}}); }); }
    else { HM.y.vals.forEach((ny,iy)=>HM.x.vals.forEach((rx,ix)=>jobs.push({ix,iy,x:rx,y:ny,rep:0,P:{...BASE,R:rx,noise:ny}}))); }
    segP.hidden=mode!=='1'; legend.hidden=mode!=='2';
    ptitle.textContent=mode==='1'?`Polarisatie per ${PARS[par].lab} (${REPS} herhalingen per waarde)`:'Polarisatie per combinatie van twee parameters';
    bRun.textContent='▶ Start de sweep'; bRun.disabled=false; prog.textContent=`${jobs.length} simulaties van ${T} tijdstappen staan klaar.`;
    lbl.innerHTML='Nog niet gestart.';
    txt.innerHTML=mode==='1'?`Druk op <b>Start</b>. De simulatie draait dan ${REPS} keer voor elk van de ${PARS[par].vals.length} waarden van de ${PARS[par].lab}, met alle andere parameters vast. Elke stip in de grafiek is één run; de lijn verbindt de gemiddelden.`
      :`Druk op <b>Start</b>. Nu worden twee parameters tegelijk gevarieerd: ${HM.x.vals.length} waarden van de ${HM.x.lab} × ${HM.y.vals.length} waarden van de ${HM.y.lab}, dus ${jobs.length} simulaties. De uitkomsten komen in een tabel die als <b>heatmap</b> is gekleurd, zoals in figuur 16.5 van het hoofdstuk.`; }
  function nextJob(){ cur++; if(cur>=jobs.length){ running=false; done=true; bRun.textContent='✓ Klaar'; bRun.disabled=true; finish(); return; }
    const j=jobs[cur]; F=makeFlock(N,1000*cur+7); t=0; acc=0; Pnow=j.P; }
  function finish(){ txt.innerHTML=mode==='1'?`<b>Klaar.</b> ${PARS[par].why}`
    :`<b>Klaar.</b> Links onder (grote straal, weinig ruis) is de zwerm geordend, rechts boven (kleine straal, veel ruis) niet. De grens daartussen laat zien hoe de twee parameters samen bepalen waar de omslag ligt: bij veel ruis heb je een grotere waarnemingsstraal nodig om toch een zwerm te krijgen. Met één parameter tegelijk had je dat verband gemist.`; }
  /* a fixed number of simulation steps per frame, so the plot fills at a pace you can follow (about 8 frames per run) */
  function work(){ let budget=50;
    while(running&&budget>0){
      if(!F) nextJob(); if(!running) break;
      for(let s=0;s<10&&t<T;s++){ stepFlock(F,Pnow); t++; if(t>T-M) acc+=polar(F); } budget-=10;
      if(t>=T){ const j=jobs[cur]; results.push({...j,v:acc/M}); lastF=F; F=null; } } }
  function status(){ const j=jobs[Math.min(cur,jobs.length-1)]; if(!j||cur<0) return;
    const nd=results.length; prog.textContent=`Run ${Math.min(cur+1,jobs.length)} van ${jobs.length}`;
    lbl.innerHTML=mode==='1'?`${PARS[par].lab} = <span class="mono nomath">${j.x}</span>, herhaling ${j.rep+1} · tijdstap ${t}${nd?` · laatste uitkomst: polarisatie ${results[nd-1].v.toFixed(2)}`:''}`
      :`straal = <span class="mono nomath">${j.x}</span>, ruis = <span class="mono nomath">${j.y}</span> · tijdstap ${t}${nd?` · laatste uitkomst: polarisatie ${results[nd-1].v.toFixed(2)}`:''}`; }
  function plot1(){ const vals=PARS[par].vals, xmax=vals[vals.length-1]*1.08;
    const f=frame(pc,{xmin:0,xmax,ymin:0,ymax:1,ny:4,xlab:PARS[par].lab,ylab:'polarisatie',pad:{l:48,b:36}});
    if(cur>=0&&cur<jobs.length&&running){ const x=f.X(jobs[cur].x); f.ctx.save(); f.ctx.strokeStyle=col('accent2'); f.ctx.setLineDash([3,3]); f.ctx.beginPath(); f.ctx.moveTo(x,f.p.t); f.ctx.lineTo(x,f.p.t+f.ph); f.ctx.stroke(); f.ctx.restore(); }
    const means=[]; vals.forEach((v,ix)=>{ const rs=results.filter(r=>r.ix===ix); if(rs.length===REPS) means.push([v,rs.reduce((a,r)=>a+r.v,0)/REPS]); });
    poly(f,means,col('accent'),2);
    for(const r of results) disc(f.ctx,f.X(r.x),f.Y(r.v),3.2,col('accent'),col('bg'),1);
    f.ctx.restore(); }
  function plot2(){ const xs=HM.x.vals, ys=HM.y.vals;
    const f=frame(pc,{xmin:xs[0]-HM.x.d/2,xmax:xs[xs.length-1]+HM.x.d/2,ymin:ys[0]-HM.y.d/2,ymax:ys[ys.length-1]+HM.y.d/2,nx:5,ny:5,xlab:HM.x.lab,ylab:HM.y.lab,pad:{l:48,b:36}});
    const c=f.ctx, cw=f.X(xs[0]+HM.x.d)-f.X(xs[0]), ch=f.Y(ys[0])-f.Y(ys[0]+HM.y.d);
    c.font='10px '+FONT.slice(5); c.textAlign='center'; c.textBaseline='middle';
    for(const r of results){ const x=f.X(r.x-HM.x.d/2), y=f.Y(r.y+HM.y.d/2);
      c.globalAlpha=0.08+0.92*r.v; c.fillStyle=col('accent'); c.fillRect(x,y,cw,ch); c.globalAlpha=1;
      if(cw>26&&ch>14){ c.fillStyle=r.v>0.55?'#fff':col('fg'); c.fillText(r.v.toFixed(2),x+cw/2,y+ch/2); } }
    if(cur>=0&&cur<jobs.length&&running){ const j=jobs[cur]; c.strokeStyle=col('accent2'); c.lineWidth=2; c.strokeRect(f.X(j.x-HM.x.d/2)+1,f.Y(j.y+HM.y.d/2)+1,cw-2,ch-2); }
    c.restore(); }
  segM.querySelectorAll('button').forEach(b=>b.onclick=()=>{ mode=b.dataset.m; segM.querySelectorAll('button').forEach(x=>x.setAttribute('aria-pressed',x===b)); build(); });
  segP.querySelectorAll('button').forEach(b=>b.onclick=()=>{ par=b.dataset.p; segP.querySelectorAll('button').forEach(x=>x.setAttribute('aria-pressed',x===b)); build(); });
  bRun.onclick=()=>{ if(done) return; running=!running; bRun.textContent=running?'⏸ Pauze':'▶ Verder'; };
  bClear.onclick=build;
  build();
  function frameFn(){ work(); status(); const G=F||lastF; if(G) drawFlock(sc,G,Pnow,false); else { const {ctx,w,h}=fitCanvas(sc); ctx.clearRect(0,0,w,h); ctx.fillStyle=col('bg'); ctx.fillRect(0,0,w,h); ctx.strokeStyle=col('line'); ctx.lineWidth=1; ctx.strokeRect(0.5,0.5,w-1,h-1); ctx.font=FONT; ctx.fillStyle=col('muted'); ctx.textAlign='center'; ctx.textBaseline='middle'; ctx.fillText('Druk op Start',w/2,h/2); }
    if(mode==='1') plot1(); else plot2(); }
  return {frame:frameFn};
})();

/* =========================================================
   QUIZ
   ========================================================= */
/* small sweep plot as inline SVG: xs = parameter values, reps = per value an array of outcomes (0..1) */
function sweepSVG(xs,reps,xlab){
  const W=320,H=170,l=44,r=14,t=12,b=32, xmax=xs[xs.length-1]*1.08;
  const X=x=>l+x/xmax*(W-l-r), Y=y=>t+(1-y)*(H-t-b);
  const means=xs.map((x,i)=>[x,reps[i].reduce((a,v)=>a+v,0)/reps[i].length]);
  const d=means.map(([x,y],k)=>(k?'L':'M')+X(x).toFixed(1)+' '+Y(y).toFixed(1)).join('');
  const pts=xs.map((x,i)=>reps[i].map(v=>`<circle cx="${X(x).toFixed(1)}" cy="${Y(v).toFixed(1)}" r="3" style="fill:var(--accent);stroke:var(--bg);stroke-width:1"/>`).join('')).join('');
  const yt=[0,0.5,1].map(v=>`<line x1="${l-3}" y1="${Y(v)}" x2="${l}" y2="${Y(v)}" style="stroke:currentColor"/><text x="${l-6}" y="${Y(v)+4}" text-anchor="end" style="font-size:11px;fill:currentColor">${v}</text>`).join('');
  const xt=xs.filter(v=>[0.02,0.1,0.2,0.3].includes(+v.toFixed(2))).map(v=>`<text x="${X(v).toFixed(1)}" y="${H-b+14}" text-anchor="middle" style="font-size:11px;fill:currentColor">${String(v).replace('.',',')}</text>`).join('');
  return `<svg viewBox="0 0 ${W} ${H}" width="${W}" style="max-width:100%;display:block;margin:8px 0;color:var(--muted)" role="img" aria-label="grafiek van een parameter-sweep">
    <line x1="${l}" y1="${t}" x2="${l}" y2="${H-b}" style="stroke:currentColor"/>
    <line x1="${l}" y1="${H-b}" x2="${W-r}" y2="${H-b}" style="stroke:currentColor"/>
    ${yt}${xt}<path d="${d}" style="fill:none;stroke:var(--accent);stroke-width:2"/>${pts}
    <text x="${l+(W-l-r)/2}" y="${H-4}" text-anchor="middle" style="font-size:11px;fill:currentColor">${xlab}</text>
    <text x="${l+4}" y="${t+10}" text-anchor="start" style="font-size:11px;fill:currentColor">polarisatie</text></svg>`;
}
const quiz=(()=>{
  const G1=sweepSVG([0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.20,0.22,0.24,0.26,0.28,0.30],
    [[0.12,0.09,0.15],[0.14,0.18,0.11],[0.17,0.22,0.13],[0.45,0.78,0.31],[0.86,0.74,0.91],[0.90,0.93,0.85],[0.92,0.95,0.90],[0.94,0.91,0.96],[0.95,0.93,0.96],[0.96,0.94,0.95],[0.95,0.97,0.94],[0.96,0.95,0.97],[0.96,0.97,0.95],[0.97,0.96,0.97],[0.97,0.97,0.96]],'waarnemingsstraal');
  const BASE=[
    {q:'Welke van deze modelgrootheden is <b>continu</b>?',
     o:['De positie (0,713; 0,284) van een vogel in de simulatie','Het aantal apen in een groep in het aapjesmodel','Het gridvakje waarin een konijn op de ruimtelijke kaart zit','De tijdstap t = 1, 2, 3, … in een differentievergelijking'], a:0,
     h:'Continu betekent: elke waarde in een bereik is mogelijk, ook alle decimalen ertussen.',
     e:'In een continue ruimte kan een coördinaat elke waarde tussen 0 en 1 aannemen. Aantallen individuen, gridvakjes en tijdstappen van een differentievergelijking zijn discreet.'},
    {q:'Wat laat het probleem van de <b>atto-vos</b> zien?',
     o:['Continue aantallen kunnen onrealistisch kleine "stukjes vos" geven','ODE-modellen zijn ongeschikt om de verspreiding van rabiës te volgen','Discrete tijdstappen geven bij lage dichtheden verkeerde uitkomsten','Ruimtelijke modellen werken alleen als je elke vos apart bijhoudt'], a:0,
     h:'Hoeveel vos is 10<sup class="nomath">−18</sup> vos, en kan die rabiës verspreiden?',
     e:'Het ODE-model voorspelde een vossendichtheid van 10<sup class="nomath">−18</sup>. Zo\'n atto-vos bestaat niet: hier had de keuze voor continue aantallen een vertekend beeld gegeven, waar een discreet aantal logischer was geweest. Bij elk model moet je nagaan of toestand, tijd en ruimte discreet of continu horen te zijn.'},
    {q:'Wat is een <b>emergente eigenschap</b>?',
     o:['Gedrag van het geheel dat ontstaat uit interacties van de onderdelen','Een eigenschap die je als parameter of regel in het model stopt','Een eigenschap die alleen in modellen met continue ruimte voorkomt','Een gedragsregel die elk individu afzonderlijk volgt in de simulatie'], a:0,
     h:'Denk aan de worm in particle life: welke stip "beslist" om te draaien?',
     e:'Eén stip of één vogel vertoont het gedrag niet: de eigenschap zit niet in de losse onderdelen, de zwerm of de worm ontstaat pas uit de interacties. Van membranen tot murmuraties: emergentie kom je overal in de biologie tegen.'},
    {q:'Welke van de volgende is <b>geen</b> regel uit het boids-model?',
     o:['Volg de leider die vooraan in de zwerm vliegt','Probeer je richting uit te lijnen met de groep','Beweeg je naar het midden van de groep toe','Probeer te voorkomen dat je met anderen botst'], a:0,
     h:'Heeft een boid überhaupt overzicht over de hele zwerm?',
     e:'Boids hebben geen leider en geen overzicht: ze reageren alleen op buren in hun waarnemingsstraal met de drie regels uitlijnen, naar het midden, en botsen vermijden. De zwerm als geheel is emergent.'},
    {q:'Je zet in de boids-simulatie de <b>waarnemingsstraal</b> op bijna nul. Wat verwacht je?',
     o:['Geen zwerm: elke vogel ziet geen buren en vliegt zijn eigen kant op','Een nog strakkere zwerm, want de vogels vliegen nu veel dichter op elkaar','Precies dezelfde zwerm, want de drie regels zijn niet veranderd','De vogels stoppen met bewegen zodra ze geen buren meer zien'], a:0,
     h:'De regels werken alleen op buren die de vogel ziet.',
     e:'De drie regels gebruiken alleen de buren binnen de waarnemingsstraal. Ziet een vogel niemand, dan hebben de regels niets om op te reageren en verdwijnt de interactie, en daarmee het emergente patroon.'},
    {q:'Waarom beschrijven we een simulatie in <b>pseudocode</b>?',
     o:['Om de regels zo te omschrijven dat iemand anders ze kan nabouwen','Omdat pseudocode sneller draait dan dezelfde regels in R-code','Omdat de uitkomst van een model afhangt van de programmeertaal','Om de precieze code van het model voor anderen verborgen te houden'], a:0,
     h:'Waar zouden de resultaten van een computermodel van af moeten hangen: de taal, of de aannames?',
     e:'Pseudocode beschrijft verbaal de regels en aannames, zonder syntax, functienamen en komma\'s. Zo kan een ander het model precies genoeg reproduceren, ook in een andere programmeertaal, en de uitkomst hangt af van de aannames, niet van de taal.'},
    {q:'Hieronder staat het resultaat van een <b>parameter-sweep</b>: drie herhalingen per waarde van de waarnemingsstraal, de lijn verbindt de gemiddelden. Wat lees je af?'+G1,
     o:['Rond een straal van 0,08 slaat het gedrag om van wanorde naar zwerm','De polarisatie neemt over het hele bereik lineair toe met de straal','De waarnemingsstraal heeft geen meetbare invloed op de polarisatie','De herhalingen verschillen zo veel dat je niets kunt concluderen'], a:0,
     h:'Kijk waar de lijn steil omhoog gaat, en waar hij vlak is.',
     e:'Links is de polarisatie laag, rechts hoog, en de overgang zit in een smal gebied. Zo\'n omslag tussen twee kwalitatief verschillende toestanden is wat een bifurcatie-diagram ook laat zien. De herhalingen spreiden het meest rond de omslag.'},
    {q:'Hoe verhoudt een <b>parameter-sweep</b> zich tot een <b>bifurcatie-analyse</b>?',
     o:['Beide variëren een parameter om te zien waar het gedrag omslaat','Een sweep bekijkt één parameterwaarde heel precies, met herhalingen','Een bifurcatie-analyse kan alleen bij een individual-based model','Een sweep test alleen of herhalingen van één simulatie overeenkomen'], a:0,
     h:'Wat deed je met K in het bifurcatie-diagram van het predator-prooimodel?',
     e:'In het bifurcatie-diagram varieerde je K en zag je overgangen in de evenwichten. Een sweep doet hetzelfde voor een computermodel, met simulaties in plaats van wiskunde: per parameterwaarde (of per combinatie, in een heatmap) classificeer je de uitkomst van de simulatie, vaak met herhalingen vanwege het toeval.'},
    {q:'Welk <b>voordeel</b> van virtueel experimenteren met het aapjesmodel noemt het hoofdstuk?',
     o:['Veel simulaties, snel en goedkoop, zonder echte apen te verstoren','Virtuele apen gedragen zich precies zoals de echte apen in het veld','Je hoeft geen aannames meer te doen over het gedrag van de apen','De resultaten zijn altijd betrouwbaarder dan die van veldwerk'], a:0,
     h:'Wat is lastig aan veldwerk, en wat niet aan een simulatie?',
     e:'Simulaties zijn een efficiënte en niet-verstorende manier om gedrag te bestuderen en vullen veldwerk goed aan. Maar het blijft een model: de uitkomst hangt af van je aannames.'},
    {q:'Hoe vinden foeragerende <b>mieren</b> in het model de weg naar het voedsel?',
     o:['Ze volgen feromonen van andere mieren, die ook weer afbreken','Ze zien elkaar en lopen achter de dichtstbijzijnde andere mier aan','Ze onthouden de positie van het voedsel en lopen er rechtstreeks heen','Ze lopen allemaal in een rechte lijn heen en weer naar het voedsel'], a:0,
     h:'Sommige mieren zijn volledig blind.',
     e:'Mieren reageren niet direct op elkaar, maar op feromonen in de omgeving die ook weer afbreken en verdunnen: een wisselwerking van mier naar omgeving en terug. Dat geeft mooie paadjes, maar soms ook een mierenkring (<i>ant mill</i>) waarin ze eindeloos rondlopen.'},
  ];
  const box=$('#qz-box'), prog=$('#qz-prog'), cnt=$('#qz-count'), sc=$('#qz-score'), next=$('#qz-next');
  let Q=[], i=0, score=0, S=[]; const KEY='bioms-quiz-'+qhash(BASE);
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
    const msg=frac===1?'Perfect! Je hebt de kern van dit hoofdstuk goed te pakken.':frac>=.75?'Heel goed! Kijk nog even naar de vragen die je niet in één keer goed had.':frac>=.5?'Op de goede weg. Speel nog eens met de boids en de parameter-sweep.':'Lees het hoofdstuk nog eens door en speel met de interactieve figuren. Daarna gaat het vast beter!';
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
const modules={overzicht:hero,boids:boids,sweep:sweep,quiz:quiz}; host.__modules=modules;
let current='overzicht';
const TABKEY='bioms-tab-zwerm';   // the last opened tab is remembered in this browser
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
function init(){ document.querySelectorAll('[data-zwerm-app]').forEach(mount); }
window.ZwermApp={mount,init};
const H0=inlineHost(CS); if(H0) mount(H0);
if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();
