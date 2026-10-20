/* Competitie-explainer (Biologische Modellen en Statistiek, hoofdstuk 15).
 * Gebruik in Quarto (één regel, op de plek waar de app moet komen):
 *   <script src="Applets/competitie_explainer.js"></script>                        -> hele app met tabbladen
 *   <script src="Applets/competitie_explainer.js" data-tab="fase"></script>        -> alleen dat onderdeel
 *   <script src="Applets/competitie_explainer.js" data-tab="fase" data-nav="1"></script> -> tabbladen, start op dat tabblad
 * data-tab: overzicht | fase | schets | quiz
 * data-theme="dark|light" forceert een thema; anders volgt de app het boek (ook de donkere modus).
 * (Alternatief: <div data-comp-app data-tab="..."></div> plus het script één keer op de pagina.)
 */
(function(){
"use strict";
const CS=document.currentScript;
function inlineHost(cs){ if(!cs||!cs.dataset||cs.__appDone||!cs.parentNode) return null;
  const single=!!cs.dataset.tab;
  // a plain <script> with no data-tab becomes the full app with tabs, unless the page already uses <div data-comp-app>
  if(!single&&document.querySelector('[data-comp-app]')) return null;
  cs.__appDone=true;
  const d=document.createElement('div'); d.setAttribute('data-comp-app','');
  if(cs.dataset.tab) d.dataset.tab=cs.dataset.tab;
  if(single&&cs.dataset.nav!=='1') d.dataset.hidenav='1'; if(cs.dataset.theme) d.dataset.theme=cs.dataset.theme;
  cs.parentNode.insertBefore(d,cs); return d; }
if(window.CompApp){ const h=inlineHost(CS); if(h) window.CompApp.mount(h); window.CompApp.init(); return; }
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
  --accent:#F08A3E; --accent-soft:#F08A3E2B; --accent2:#56B4E9; --blue:#56B4E9; --warn:#aab2b8;
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


/* competitie: extra */
:host{--n1:#0072B2;--n2:#E69F00}
:host([data-dark]){--n1:#56B4E9;--n2:#F5B942}
.c1{color:var(--n1);font-weight:600}
.c2{color:var(--n2);font-weight:600}
.ph{aspect-ratio:1/1;max-width:440px;margin:0 auto;cursor:crosshair}
.eqs{display:flex;flex-direction:column;gap:3px;align-items:center;margin:6px 0 10px}
.eqs .eqn{font-size:1.1em}
.ev{font-size:.85rem;margin-top:6px;line-height:1.6}
.stepbox{min-height:9em;margin:0 0 10px}
.stepbox h4{margin:0 0 4px;font-size:.95rem}
.stepbox p{margin:.25rem 0}
.legend .sw{display:inline-block;width:18px;height:3px;vertical-align:middle;margin-right:5px;border-radius:2px}
`;
const N1='<span class="c1">N<sub>1</sub></span>', N2='<span class="c2">N<sub>2</sub></span>', D='<span class="mu">d</span>';
const EQ1=`${D}${N1}/${D}t = r${N1}(1 − c<sub>11</sub>${N1} − c<sub>12</sub>${N2})`;
const EQ2=`${D}${N2}/${D}t = r${N2}(1 − c<sub>22</sub>${N2} − c<sub>21</sub>${N1})`;
const HTML=`
<nav class="tabs" role="tablist">
  <button class="tab" data-tab="overzicht">In één oogopslag</button>
  <button class="tab" data-tab="fase">Faseruimte</button>
  <button class="tab" data-tab="schets">Schets stap voor stap</button>
  <button class="tab" data-tab="quiz">Quiz ✓</button>
</nav>

<section class="panel" id="tab-overzicht">
  <h2>Hoofdstuk 15 in één oogopslag</h2>
  <div class="grid2">
    <div>
      <p>Soorten strijden om voedsel, ruimte of nestplekken. <b>Intraspecifieke</b> competitie is competitie <i>binnen</i> een soort (denk aan de <span class="nomath">Intratuin</span>), <b>interspecifieke</b> competitie is competitie <i>tussen</i> soorten. Hoe meer de niches van twee soorten overlappen, hoe sterker de interspecifieke competitie.</p>
      <p>In het <b>Lotka-Volterra competitiemodel</b> groeien twee populaties logistisch en remmen ze elkaar. Met K = 1:</p>
      <div class="eqs"><span class="eqn">${EQ1}</span><span class="eqn">${EQ2}</span></div>
      <p>c<sub>11</sub> en c<sub>22</sub> schalen de competitie binnen een soort, c<sub>12</sub> de competitie die soort 1 van soort 2 ondervindt en c<sub>21</sub> andersom. Zet een afgeleide op nul en je vindt per soort twee <b>nullclines</b>: een as en een dalende rechte lijn. Waar een <span class="c1">blauwe</span> en een <span class="c2">oranje</span> nullcline snijden ligt een <b>evenwicht</b>. De ligging van de lijnen bepaalt de uitkomst:</p>
      <table class="cmp">
        <tr><th>Nullclines</th><th>Uitkomst</th></tr>
        <tr><td>Snijden, intra &gt; inter</td><td><b>Coexistentie</b>: stabiel evenwicht met beide soorten</td></tr>
        <tr><td>Snijden, inter &gt; intra</td><td><b>Bistabiliteit</b>: zadelpunt, wie er het meest is wint (<i>founder control</i>)</td></tr>
        <tr><td>Blauwe lijn helemaal buiten de oranje</td><td><b>Exclusie</b>: soort 1 wint altijd</td></tr>
        <tr><td>Oranje lijn helemaal buiten de blauwe</td><td><b>Exclusie</b>: soort 2 wint altijd</td></tr>
      </table>
    </div>
    <div class="card">
      <canvas id="hero-ph" class="plot"></canvas>
      <div class="muted" id="hero-lbl" style="font-size:.85rem;margin-top:6px;min-height:4em"></div></div>
  </div>

  <div class="cards">
    <div class="card ov"><span class="num">15.2</span><h4>Competitie binnen en tussen soorten</h4>
      <p>Binnen een soort overlapt de niche per definitie volledig, dus intraspecifieke competitie om voedsel is meestal het sterkst. Maar toxines, territoria en verjagen kunnen de interspecifieke competitie juist de overhand geven.</p></div>
    <div class="card ov"><span class="num">15.3</span><h4>Lotka-Volterra competitie</h4>
      <p>Twee logistisch groeiende soorten met een gedeeld draagvermogen, plus vier parameters c<sub>11</sub>, c<sub>22</sub>, c<sub>12</sub> en c<sub>21</sub> die de intra- en interspecifieke competitie schalen. Met K = 1 vallen de breuken weg.</p></div>
    <div class="card ov"><span class="num">15.4</span><h4>Faseruimte van het competitiemodel</h4>
      <p>Het stappenplan: assen kiezen, nullclines vinden (${N1} = 0 en ${N2} = (1 − c<sub>11</sub>${N1})/c<sub>12</sub>), snijpunten met de assen bepalen, een extreme hoek bekijken en pijltjes omklappen bij elke nullcline.</p>
      <button class="b" data-go="schets">Schets mee →</button></div>
    <div class="card ov"><span class="num">15.5</span><h4>Coexistentie of exclusie?</h4>
      <p>Sterke intraspecifieke competitie: alle pijlen wijzen naar het snijpunt, een <b>attractor</b>. Sterke interspecifieke competitie: het snijpunt is een <b>zadelpunt</b> en de soort die er het meest is wint. Dat heet <i>founder control</i>.</p>
      <button class="b" data-go="fase">Draai aan de competitie →</button></div>
    <div class="card ov"><span class="num">15.6</span><h4>Evolutionaire vraagstukken</h4>
      <p>Hetzelfde model als <b>invasie-experiment</b>: kan een zeldzame mutant groeien? Drie uitkomsten: niet groeien, samenleven, of de gevestigde populatie vervangen. Bij een zadelpunt kan de mutant niet invaderen, maar de gevestigde soort andersom ook niet.</p></div>
    <div class="card ov"><span class="num">Opg.</span><h4>Opgaven</h4>
      <p>Het "werkelijke" vectorveld en waarom trajectories zichzelf nooit snijden, evenwichten benoemen en trajectories schetsen, cheaters en cooperators in een ODE, en als verdieping parameter fitting op de <i>Paramecium</i>-data van Gause.</p></div>
    <div class="card ov"><span class="num">✓</span><h4>Test jezelf</h4>
      <p>Tien korte vragen over competitie, nullclines, evenwichten en invasies. Bij een fout antwoord mag je het opnieuw proberen.</p>
      <button class="b primary" data-go="quiz">Start de quiz →</button></div>
  </div>

  <details>
    <summary>Terminologie</summary>
    <table class="cmp">
      <tr><th>Nederlands</th><th>Engels</th><th>Betekenis</th></tr>
      <tr><td>Intraspecifieke competitie</td><td>Intraspecific competition</td><td>Competitie binnen een soort (tussen soortgenoten).</td></tr>
      <tr><td>Interspecifieke competitie</td><td>Interspecific competition</td><td>Competitie tussen verschillende soorten.</td></tr>
      <tr><td>Coexistentie</td><td>Coexistence</td><td>De samenleving van twee of meer soorten.</td></tr>
      <tr><td>Competitieve exclusie</td><td>Competitive exclusion</td><td>Twee soorten die niet duurzaam kunnen samenleven; de ene verdringt de andere.</td></tr>
      <tr><td>Zadelpunt</td><td>Saddle point</td><td>Evenwicht waar de pijlen in de ene richting naartoe en in de andere richting vanaf wijzen.</td></tr>
      <tr><td>Parameter fitting</td><td>Parameter fitting</td><td>Zoeken naar de parameters waarmee een model het best bij de data past. Een goede fit bewijst niet dat het model klopt.</td></tr>
    </table>
  </details>
</section>

<section class="panel" id="tab-fase" hidden>
  <h2>Faseruimte van het competitiemodel</h2>
  <p>Links de faseruimte met ${N1} op de x-as en ${N2} op de y-as: <span class="c1">blauwe</span> nullclines (${D}${N1}/${D}t = 0), <span class="c2">oranje</span> nullclines (${D}${N2}/${D}t = 0), het vectorveld en de evenwichten (● stabiel, ○ instabiel of zadelpunt). <b>Klik in de faseruimte</b> om vanaf dat punt een nieuwe simulatie te starten; rechts zie je de bijbehorende oplossing. Schuif met de competitiecoëfficiënten of kies een van de vier scenario's (r = 1 en K = 1, zoals in het hoofdstuk).</p>
  <div class="btns"><span class="seg" id="fs-preset"></span><button class="b" id="fs-clear">✕ Wis trajectorie</button></div>
  <div class="controls">
    <div class="ctl"><label><span class="lbl">c<sub>11</sub> (intra, soort 1)</span> <span id="fs-c11-v"></span></label><input type="range" id="fs-c11" min="0.4" max="4" step="0.1" value="2"></div>
    <div class="ctl"><label><span class="lbl">c<sub>22</sub> (intra, soort 2)</span> <span id="fs-c22-v"></span></label><input type="range" id="fs-c22" min="0.4" max="4" step="0.1" value="2"></div>
    <div class="ctl"><label><span class="lbl">c<sub>12</sub> (effect van 2 op 1)</span> <span id="fs-c12-v"></span></label><input type="range" id="fs-c12" min="0.4" max="4" step="0.1" value="1"></div>
    <div class="ctl"><label><span class="lbl">c<sub>21</sub> (effect van 1 op 2)</span> <span id="fs-c21-v"></span></label><input type="range" id="fs-c21" min="0.4" max="4" step="0.1" value="1"></div>
  </div>
  <div class="grid2">
    <div class="card"><div class="ptitle">Faseruimte (klik om een simulatie te starten)</div><canvas id="fs-ph" class="ph"></canvas>
      <div class="legend"><span><i class="sw" style="background:var(--n1)"></i>nullclines ${N1}</span><span><i class="sw" style="background:var(--n2)"></i>nullclines ${N2}</span><span>● stabiel</span><span>○ instabiel / zadelpunt</span></div></div>
    <div class="card"><div class="ptitle">Oplossing van de trajectorie</div><canvas id="fs-tp" class="plot"></canvas>
      <div class="ev" id="fs-ev"></div></div>
  </div>
  <div class="callout" id="fs-text"></div>
</section>

<section class="panel" id="tab-schets" hidden>
  <h2>Schets de faseruimte stap voor stap</h2>
  <p>Het stappenplan uit het hoofdstuk, toegepast op het competitiemodel. Kies een scenario en loop de stappen door met de knoppen.</p>
  <div class="btns"><span class="seg" id="sk-scen"></span></div>
  <div class="stepper" style="justify-content:flex-start;margin:0 0 10px"><button class="b" id="sk-prev">← Vorige</button><div class="dots" id="sk-dots"></div><button class="b primary" id="sk-next">Volgende →</button></div>
  <div class="grid2">
    <div class="card"><canvas id="sk-ph" class="ph"></canvas></div>
    <div><div class="callout stepbox" id="sk-text"></div>
      <div class="card" id="sk-tpwrap" hidden><div class="ptitle">De oplossing: ${N1} en ${N2} tegen de tijd</div><canvas id="sk-tp" class="plot-half"></canvas></div></div>
  </div>
</section>

<section class="panel" id="tab-quiz" hidden>
  <h2>Quiz: competitie en evolutie</h2>
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
   COMPETITIEMODEL: gedeelde rekenhulpen (r = 1, K = 1 zoals in het hoofdstuk)
   ========================================================= */
const R=1;
const dN=(P,x,y)=>[R*x*(1-P.c11*x-P.c12*y), R*y*(1-P.c22*y-P.c21*x)];
/* type evenwicht via de Jacobiaan (teken van determinant en trace) */
function classify(P,x,y){ const a=R*(1-2*P.c11*x-P.c12*y), b=-R*P.c12*x, c=-R*P.c21*y, d=R*(1-2*P.c22*y-P.c21*x);
  const tr=a+d, det=a*d-b*c; if(det<-1e-9) return 'zadelpunt'; if(Math.abs(det)<1e-9) return 'grensgeval'; return tr<0?'stabiel':'instabiel'; }
function equilibria(P){ const E=[[0,0],[1/P.c11,0],[0,1/P.c22]], det=P.c11*P.c22-P.c12*P.c21;
  if(Math.abs(det)>1e-9){ const x=(P.c22-P.c12)/det, y=(P.c11-P.c21)/det; if(x>1e-9&&y>1e-9) E.push([x,y]); }
  return E.map(([x,y])=>({x,y,type:classify(P,x,y)})); }
const OUT={
  coex:{name:'Coexistentie',short:'intraspecifieke competitie is sterker dan interspecifieke. De trajectorie loopt naar het stabiele evenwicht met beide soorten, vanaf elke beginsituatie.',
    t:'<b>Coexistentie.</b> Intraspecifieke competitie is sterker dan interspecifieke (c<sub>12</sub> &lt; c<sub>22</sub> en c<sub>21</sub> &lt; c<sub>11</sub>): elke soort remt zichzelf meer dan de ander. Alle pijlen wijzen naar het inwendige evenwicht, een <b>attractor</b>. Welke beginsituatie je ook kiest (met beide soorten aanwezig), je komt daar uit. Een zeldzame mutant of invasieve soort kan dus altijd binnenkomen en blijft.'},
  bist:{name:'Bistabiliteit (founder control)',short:'interspecifieke competitie is sterker. Het snijpunt is een zadelpunt: wie er aan het begin het meest is, wint (hier soort 2).',
    t:'<b>Competitieve exclusie met founder control.</b> Interspecifieke competitie is sterker dan intraspecifieke. Het inwendige evenwicht is een <b>zadelpunt</b>: in de ene richting wijzen de pijlen ernaartoe, in de andere ervan af. De evenwichten met één soort zijn allebei stabiel, dus de soort die aanvankelijk in de meerderheid is wint. Een zeldzame mutant kan niet invaderen, maar de gevestigde soort zou andersom ook niet kunnen invaderen.'},
  w1:{name:'Soort 1 wint',short:'de blauwe nullcline ligt helemaal buiten de oranje: soort 2 wordt altijd verdrongen (competitieve exclusie).',
    t:'<b>Soort 1 wint: competitieve exclusie.</b> De <span class="c1">blauwe</span> nullcline ligt helemaal buiten de <span class="c2">oranje</span>: tussen de lijnen groeit soort 1 nog terwijl soort 2 al afneemt. De nullclines snijden niet, dus er is geen evenwicht met beide soorten. Alleen (1/c<sub>11</sub>, 0) is stabiel, ongeacht de beginsituatie.'},
  w2:{name:'Soort 2 wint',short:'de oranje nullcline ligt helemaal buiten de blauwe: soort 1 wordt altijd verdrongen.',
    t:'<b>Soort 2 wint: competitieve exclusie.</b> De <span class="c2">oranje</span> nullcline ligt helemaal buiten de <span class="c1">blauwe</span>: tussen de lijnen groeit soort 2 nog terwijl soort 1 al afneemt. Er is geen evenwicht met beide soorten; alleen (0, 1/c<sub>22</sub>) is stabiel, ongeacht de beginsituatie.'},
  neut:{name:'Nullclines vallen samen',short:'een hele lijn van evenwichten; waar je eindigt hangt af van de start.',
    t:'<b>De nullclines vallen samen.</b> Elk punt op die lijn is een evenwicht en waar je eindigt hangt af van de beginsituatie. Dit is een niet-generiek geval: de kleinste verandering van een parameter maakt er coexistentie of exclusie van.'}};
function outcome(P){ const E=equilibria(P), s1=E[1].type==='stabiel', s2=E[2].type==='stabiel';
  return s1&&s2?'bist':s1?'w1':s2?'w2':(E[3]&&E[3].type==='stabiel')?'coex':'neut'; }
function traject(P,x0,y0,T,dt=0.02){ const pts=[[0,x0,y0]]; let x=x0,y=y0; const n=Math.round(T/dt);
  for(let k=1;k<=n;k++){ const [f,g]=dN(P,x,y); x=Math.max(0,x+dt*f); y=Math.max(0,y+dt*g); if(k%3===0) pts.push([k*dt,x,y]); } return pts; }
const mkTraj=(P,x0,y0,T=30)=>({pts:traject(P,x0,y0,T),x0,y0,T,prog:0});
const nl=v=>fmt(v,2).replace('.',',');
const eqList=P=>equilibria(P).map(e=>`(${nl(e.x)}, ${nl(e.y)}): <b>${e.type}</b>`).join(' · ');
const extent=P=>[niceCeil(1.15*Math.max(1/P.c11,1/P.c21)), niceCeil(1.15*Math.max(1/P.c12,1/P.c22))];
function localXY(e,c){ const r=c.getBoundingClientRect(); return [e.clientX-r.left,e.clientY-r.top]; }
function arrow(ctx,x1,y1,x2,y2,color,lw=1.2,hs=4){ ctx.strokeStyle=color; ctx.fillStyle=color; ctx.lineWidth=lw; ctx.beginPath(); ctx.moveTo(x1,y1); ctx.lineTo(x2,y2); ctx.stroke();
  const a=Math.atan2(y2-y1,x2-x1); ctx.beginPath(); ctx.moveTo(x2,y2); ctx.lineTo(x2-hs*Math.cos(a-0.5),y2-hs*Math.sin(a-0.5)); ctx.lineTo(x2-hs*Math.cos(a+0.5),y2-hs*Math.sin(a+0.5)); ctx.closePath(); ctx.fill(); }
/* o: ncl 0|1|2 (nullclines van hoeveel soorten), labels 0|1|2 (snijpunten met de assen), field 'none'|'corner'|'regions'|'full', eq, trajs */
function drawPhase(c,P,o){
  const [xm,ym]=extent(P), f=frame(c,{xmin:0,xmax:xm,ymin:0,ymax:ym,nx:5,ny:5,xlab:'N₁ (soort 1)',ylab:'N₂ (soort 2)',pad:{l:46,b:34,r:14,t:14}});
  const {ctx,X,Y}=f, n1=col('n1'), n2=col('n2');
  const line=(x0,y0,x1,y1,cl,w,dx=0,dy=0)=>{ ctx.strokeStyle=cl; ctx.lineWidth=w; ctx.beginPath(); ctx.moveTo(X(x0)+dx,Y(y0)+dy); ctx.lineTo(X(x1)+dx,Y(y1)+dy); ctx.stroke(); };
  ctx.font='600 11px '+FONT.slice(5); ctx.textAlign='left';
  const lab=(s,x,y,cl,bl)=>{ ctx.fillStyle=cl; ctx.textBaseline=bl; drawMath(ctx,s,x,y); };
  /* snijpunt-labels: boven de eigen lijn; een x-label dat niet meer rechts past gaat links van het snijpunt */
  const labX=(s,v,dy,cl)=>{ const right=X(v)+42>X(xm); ctx.textAlign=right?'right':'left'; lab(s,X(v)+(right?-5:5),Y(0)-dy,cl,'bottom'); ctx.textAlign='left'; };
  if(o.ncl>=1){ line(0,0,0,ym,n1,3,1.5,0); line(0,1/P.c12,1/P.c11,0,n1,2.4);
    if(o.labels>=1){ lab('1/c₁₂',X(0)+7,Y(1/P.c12)-5,n1,'bottom'); labX('1/c₁₁',1/P.c11,5,n1); } }
  if(o.ncl>=2){ line(0,0,xm,0,n2,3,0,-1.5); line(0,1/P.c22,1/P.c21,0,n2,2.4);
    if(o.labels>=2){ const cy=Math.abs(Y(1/P.c22)-Y(1/P.c12))<16, cx=Math.abs(X(1/P.c21)-X(1/P.c11))<48;
      lab('1/c₂₂',X(0)+7,Y(1/P.c22)+(cy?6:-5),n2,cy?'top':'bottom'); labX('1/c₂₁',1/P.c21,cx?19:5,n2); } }
  if(o.field==='full'){ const n=8; ctx.save(); ctx.globalAlpha=.7;
    for(let i=0;i<n;i++) for(let j=0;j<n;j++){ const x=xm*(i+0.5)/n, y=ym*(j+0.5)/n, [fx,fy]=dN(P,x,y), L=Math.hypot(fx,fy); if(L<1e-6) continue;
      const s=8/L, px=X(x), py=Y(y); arrow(ctx,px-fx*s,py+fy*s,px+fx*s,py-fy*s,col('axis'),1.1,3.5); } ctx.restore(); }
  else if(o.field==='corner'||o.field==='regions'){ const n=4, pts=[]; if(o.field==='corner') pts.push([0.9*xm,0.9*ym]); else for(let i=0;i<n;i++) for(let j=0;j<n;j++) pts.push([xm*(i+0.5)/n,ym*(j+0.5)/n]);
    for(const [x,y] of pts){ const [fx,fy]=dN(P,x,y), px=X(x), py=Y(y), L=14; disc(ctx,px,py,2.2,col('axis'));
      if(Math.abs(fx)>1e-6) arrow(ctx,px,py,px+Math.sign(fx)*L,py,n1,2.2,5); if(Math.abs(fy)>1e-6) arrow(ctx,px,py,px,py-Math.sign(fy)*L,n2,2.2,5); } }
  for(const t of o.trajs||[]){ const vis=t.pts.filter(p=>p[0]<=t.prog), pts=vis.map(p=>[p[1],p[2]]); poly(f,pts,col('fg'),1.8,null,.85);
    disc(ctx,X(t.x0),Y(t.y0),3.5,col('fg')); const l=vis[vis.length-1]; if(l&&t.prog<t.T) disc(ctx,X(l[1]),Y(l[2]),3.5,col('accent'),col('bg'),1); }
  if(o.eq) for(const e of equilibria(P)){ const st=e.type==='stabiel'; disc(ctx,X(e.x),Y(e.y),5.5,st?col('fg'):col('bg'),col('fg'),2); }
  ctx.restore(); return f;
}
function drawTime(c,t,P,T){
  const [xm,ym]=extent(P), f=frame(c,{xmin:0,xmax:T,ymin:0,ymax:Math.max(xm,ym),nx:6,ny:4,xlab:'tijd',ylab:'dichtheid',pad:{l:46,b:34,r:14,t:14}});
  if(t){ const vis=t.pts.filter(p=>p[0]<=t.prog); poly(f,vis.map(p=>[p[0],p[1]]),col('n1'),2.4); poly(f,vis.map(p=>[p[0],p[2]]),col('n2'),2.4); }
  f.ctx.restore(); return f;
}
const PRESETS=[{k:'coex',n:'Coexistentie',p:{c11:2,c22:2,c12:1,c21:1}},{k:'w1',n:'Soort 1 wint',p:{c11:2,c22:2,c12:1,c21:3}},
  {k:'w2',n:'Soort 2 wint',p:{c11:2,c22:2,c12:3,c21:1}},{k:'bist',n:'Bistabiel',p:{c11:1,c22:1,c12:2,c21:2}}];
const cstr=P=>`c<sub>11</sub> = ${nl(P.c11)}, c<sub>22</sub> = ${nl(P.c22)}, c<sub>12</sub> = ${nl(P.c12)}, c<sub>21</sub> = ${nl(P.c21)}`;

/* =========================================================
   IN ÉÉN OOGOPSLAG: de vier scenario's in de faseruimte
   ========================================================= */
const hero=(()=>{
  const cv=$('#hero-ph'), lbl=$('#hero-lbl'), T=20;
  const ST=[[0.15,0.9]];   // één trajectorie per scenario, steeds vanaf dezelfde beginsituatie
  let si=0, trajs=[], prog=0;
  function load(){ const P=PRESETS[si].p, [xm,ym]=extent(P); trajs=ST.map(([a,b])=>mkTraj(P,a*xm,b*ym,T)); prog=0; }
  function frameFn(dt){
    prog+=dt*5; if(prog>T+12){ si=(si+1)%PRESETS.length; load(); }
    const S=PRESETS[si]; trajs.forEach(t=>t.prog=prog);
    drawPhase(cv,S.p,{ncl:2,field:'full',eq:true,trajs});
    lbl.innerHTML=`<b>${OUT[S.k].name}</b> (${cstr(S.p)}): ${OUT[S.k].short}`;
  }
  load(); return {frame:frameFn};
})();

/* =========================================================
   FASERUIMTE: sliders, presets, klikbare trajectories
   ========================================================= */
const fase=(()=>{
  const ph=$('#fs-ph'), tp=$('#fs-tp'), ev=$('#fs-ev'), txt=$('#fs-text'), seg=$('#fs-preset'), T=30;
  const S={c11:$('#fs-c11'),c22:$('#fs-c22'),c12:$('#fs-c12'),c21:$('#fs-c21')};
  let P={}, trajs=[], lastF=null;
  PRESETS.forEach((pr,k)=>{ const b=document.createElement('button'); b.textContent=pr.n; b.dataset.k=k; b.onclick=()=>setP(pr.p); seg.appendChild(b); });
  function readP(){ for(const k in S){ P[k]=+S[k].value; $('#fs-'+k+'-v').textContent=nl(P[k]); }
    seg.querySelectorAll('button').forEach(b=>{ const pr=PRESETS[+b.dataset.k].p; b.setAttribute('aria-pressed',Object.keys(pr).every(k=>Math.abs(pr[k]-P[k])<1e-9)); }); }
  function addTraj(x,y){ trajs=[mkTraj(P,x,y,T)]; }   // één trajectorie tegelijk: klikken start een nieuwe simulatie
  function update(){ readP(); trajs=trajs.map(t=>({...mkTraj(P,t.x0,t.y0,T),prog:t.prog}));
    const k=outcome(P); txt.innerHTML=OUT[k].t; ev.innerHTML='<b>Evenwichten</b> ('+cstr(P)+'):<br>'+eqList(P); postHeight(); }
  function setP(p){ for(const k in p) S[k].value=p[k]; trajs=[]; update(); const [xm,ym]=extent(P); addTraj(0.03*xm,0.02*ym); }
  for(const k in S) S[k].oninput=update;
  $('#fs-clear').onclick=()=>{ trajs=[]; };
  ph.addEventListener('click',e=>{ if(!lastF) return; const [px,py]=localXY(e,ph); if(!lastF.inside(px,py)) return; const [x,y]=lastF.inv(px,py); addTraj(x,y); });
  setP(PRESETS[0].p);
  return {frame:dt=>{ trajs.forEach(t=>{ t.prog=Math.min(t.T,t.prog+dt*5); });
    lastF=drawPhase(ph,P,{ncl:2,field:'full',eq:true,trajs}); drawTime(tp,trajs[trajs.length-1],P,T); }};
})();

/* =========================================================
   SCHETS STAP VOOR STAP: het stappenplan uit het hoofdstuk
   ========================================================= */
const schets=(()=>{
  const cv=$('#sk-ph'), tp=$('#sk-tp'), tw=$('#sk-tpwrap'), txt=$('#sk-text'), dots=$('#sk-dots'), seg=$('#sk-scen'), bP=$('#sk-prev'), bN=$('#sk-next'), T=30;
  const SCEN=[{n:'Intra sterker',p:{c11:2,c22:2,c12:1,c21:1}},{n:'Inter sterker',p:{c11:1,c22:1,c12:2,c21:2}},{n:'Soort 2 heeft veel last van soort 1',p:{c11:2,c22:2,c12:1,c21:3}}];
  let sc=0, step=0, start=[0.1,0.2], tr=null, lastF=null;
  const P=()=>SCEN[sc].p;
  const cross=P=>!!equilibria(P)[3];
  const STEPS=[
    {h:'Stap 1: het assenstelsel',o:{ncl:0},t:P=>`<p>We zetten ${N1} (soort 1) op de x-as en ${N2} (soort 2) op de y-as. Die keuze verandert niets aan het model, alleen aan de tekening. Het model, met r = 1 en K = 1:</p><div class="eqs"><span class="eqn">${EQ1}</span><span class="eqn">${EQ2}</span></div><p>In dit scenario: ${cstr(P)}.</p>`},
    {h:'Stap 2: de nullclines van N₁',o:{ncl:1,labels:1},t:P=>`<p>Los ${D}${N1}/${D}t = 0 op. Het product r${N1}(1 − c<sub>11</sub>${N1} − c<sub>12</sub>${N2}) is nul als ${N1} = 0 (de y-as) óf als 1 − c<sub>11</sub>${N1} − c<sub>12</sub>${N2} = 0, dus ${N2} = (1 − c<sub>11</sub>${N1})/c<sub>12</sub>.</p><p>Dat is een dalende rechte lijn. Ze snijdt de y-as bij ${N2} = 1/c<sub>12</sub> = ${nl(1/P.c12)} (vul ${N1} = 0 in) en de x-as bij ${N1} = 1/c<sub>11</sub> = ${nl(1/P.c11)} (daar is de teller nul). Beide lijnen zijn <span class="c1">blauw</span>: het zijn nullclines van ${N1}, ook al is de tweede geschreven als ${N2} = f(${N1}).</p>`},
    {h:'Stap 3 en 4: de nullclines van N₂',o:{ncl:2,labels:2},t:P=>`<p>Hetzelfde voor ${D}${N2}/${D}t = 0: ${N2} = 0 (de x-as) óf ${N2} = (1 − c<sub>21</sub>${N1})/c<sub>22</sub>, met snijpunten ${N2} = 1/c<sub>22</sub> = ${nl(1/P.c22)} op de y-as en ${N1} = 1/c<sub>21</sub> = ${nl(1/P.c21)} op de x-as.</p><p>Vergelijk de snijpunten met de assen: ${cross(P)?'de schuine lijnen snijden elkaar, dus er is een evenwicht met beide soorten.':'de ene schuine lijn ligt helemaal buiten de andere, dus de nullclines snijden niet. Er is geen evenwicht met beide soorten.'}</p>`},
    {h:'Stap 5 en 6: een extreme waarde',o:{ncl:2,field:'corner'},t:()=>`<p>Rechtsboven zijn beide populaties groot. Vermenigvuldig de ODEs uit: de groei is lineair (r${N1}), maar de competitietermen zijn kwadratisch (−rc<sub>11</sub>${N1}<sup class="nomath">2</sup> en −rc<sub>12</sub>${N1}${N2}). Bij grote aantallen winnen de kwadratische termen: beide soorten nemen af.</p><p>Teken dat als twee pijltjes: het <span class="c1">horizontale</span> (${N1}, de x-as) wijst naar links, het <span class="c2">verticale</span> (${N2}, de y-as) naar beneden.</p>`},
    {h:'Stap 7: het vectorveld afmaken',o:{ncl:2,field:'regions'},t:()=>`<p>Vanuit die hoek vul je de rest in zonder te rekenen. Steek je een <span class="c1">blauwe</span> ${N1}-nullcline over, dan klapt het <span class="c1">horizontale</span> pijltje om (${D}${N1}/${D}t wisselt daar van teken). Steek je een <span class="c2">oranje</span> ${N2}-nullcline over, dan klapt het <span class="c2">verticale</span> pijltje om.</p><p>Controleer: tussen welke lijnen groeit soort 1 nog terwijl soort 2 al afneemt, of andersom?</p>`},
    {h:'De evenwichten benoemen',o:{ncl:2,field:'regions',eq:true},t:P=>`<p>Waar een <span class="c1">blauwe</span> en een <span class="c2">oranje</span> nullcline snijden zijn beide afgeleiden nul: een evenwicht. Dat is altijd (0, 0), de punten met één soort op de assen, en eventueel het snijpunt van de twee schuine lijnen. Kijk naar de pijltjes eromheen: ● alle pijlen ernaartoe is <b>stabiel</b>, ○ alle pijlen ervan af is <b>instabiel</b>, ○ ernaartoe én ervan af is een <b>zadelpunt</b>.</p><p>${eqList(P)}.<br><b>${OUT[outcome(P)].name}</b>: ${OUT[outcome(P)].short}</p>`},
    {h:'Stap 8: een trajectorie schetsen',o:{ncl:2,field:'regions',eq:true,traj:true},t:()=>`<p>Kies een begintoestand (hier ${N1} = ${nl(start[0])}, ${N2} = ${nl(start[1])}; <b>klik in de faseruimte</b> om een andere te kiezen) en volg de pijltjes. Een trajectorie steekt een <span class="c1">blauwe</span> nullcline altijd verticaal over (daar is ${D}${N1}/${D}t = 0) en een <span class="c2">oranje</span> nullcline horizontaal.</p><p>De pijlen zeggen alleen óf iets toe- of afneemt, niet hoe snel. Een schets is goed als ze de richtingen van het vectorveld volgt.</p>`},
    {h:'Stap 9: de oplossing schetsen',o:{ncl:2,field:'regions',eq:true,traj:true,time:true},t:P=>`<p>Dezelfde trajectorie, maar nu ${N1} en ${N2} uitgezet tegen de tijd. Elke keer dat de trajectorie een <span class="c1">blauwe</span> nullcline kruist heeft de <span class="c1">blauwe</span> curve een top of dal (${D}${N1}/${D}t = 0); bij een <span class="c2">oranje</span> nullcline de <span class="c2">oranje</span> curve.</p><p>Uitkomst: <b>${OUT[outcome(P)].name}</b>. Kies boven een ander scenario of klik een andere begintoestand en kijk of je voorspelling klopt.</p>`}];
  SCEN.forEach((s,k)=>{ const b=document.createElement('button'); b.textContent=s.n; b.onclick=()=>{ sc=k; render(); }; seg.appendChild(b); });
  dots.innerHTML=STEPS.map(()=>'<i></i>').join(''); dots.querySelectorAll('i').forEach((d,k)=>d.onclick=()=>{ step=k; render(); });
  function render(){ const p=P(); seg.querySelectorAll('button').forEach((b,k)=>b.setAttribute('aria-pressed',k===sc));
    dots.querySelectorAll('i').forEach((d,k)=>d.classList.toggle('on',k===step));
    tr=mkTraj(p,start[0],start[1],T); const S=STEPS[step];
    txt.innerHTML=`<h4>${S.h}</h4>${S.t(p)}`; tw.hidden=!S.o.time; bP.disabled=step===0; bN.disabled=step===STEPS.length-1; postHeight(); }
  bP.onclick=()=>{ if(step>0){ step--; render(); } }; bN.onclick=()=>{ if(step<STEPS.length-1){ step++; render(); } };
  cv.addEventListener('click',e=>{ if(!lastF) return; const [px,py]=localXY(e,cv); if(!lastF.inside(px,py)) return; start=lastF.inv(px,py); render(); });
  render();
  return {frame:dt=>{ const S=STEPS[step].o; if(S.traj) tr.prog=Math.min(tr.T,tr.prog+dt*5);
    lastF=drawPhase(cv,P(),{...S,trajs:S.traj?[tr]:[]}); if(S.time) drawTime(tp,tr,P(),T); }};
})();

/* =========================================================
   QUIZ
   ========================================================= */
function phSVG(P,o){ o=o||{}; const W=300,H=200,l=38,r=12,t=14,b=28;
  const xm=1.15*Math.max(1/P.c11,1/P.c21), ym=1.15*Math.max(1/P.c12,1/P.c22);
  const X=x=>(l+x/xm*(W-l-r)).toFixed(1), Y=y=>(t+(ym-y)/ym*(H-t-b)).toFixed(1);
  const ln=(x0,y0,x1,y1,c,w)=>`<line x1="${X(x0)}" y1="${Y(y0)}" x2="${X(x1)}" y2="${Y(y1)}" style="stroke:${c};stroke-width:${w}"/>`;
  const tx=(s,x,y,a,st)=>`<text x="${x}" y="${y}" text-anchor="${a}" style="font-size:11px;fill:currentColor;${st||''}">${s}</text>`;
  let s=`<svg viewBox="0 0 ${W} ${H}" width="${W}" style="max-width:100%;display:block;margin:8px 0;color:var(--muted)" role="img" aria-label="faseruimte met nullclines">
    ${ln(0,0,0,ym,'currentColor',1)}${ln(0,0,xm,0,'currentColor',1)}
    <line x1="${+X(0)+2}" y1="${Y(0)}" x2="${+X(0)+2}" y2="${Y(ym)}" style="stroke:var(--n1);stroke-width:2.5"/><line x1="${X(0)}" y1="${+Y(0)-2}" x2="${X(xm)}" y2="${+Y(0)-2}" style="stroke:var(--n2);stroke-width:2.5"/>
    ${ln(0,1/P.c12,1/P.c11,0,'var(--n1)',2.5)}${ln(0,1/P.c22,1/P.c21,0,'var(--n2)',2.5)}
    ${tx('<tspan font-style="italic">N</tspan>₁',W-r,H-8,'end')}${tx('<tspan font-style="italic">N</tspan>₂',l-6,t+10,'end')}
    ${tx('0',l-6,+Y(0)+4,'end')}`;
  if(o.corner){ const px=+X(0.9*xm), py=+Y(0.9*ym);
    s+=`<circle cx="${px}" cy="${py}" r="2.5" style="fill:currentColor"/><path d="M${px} ${py}h-16 M${px-16} ${py}l5 -4 M${px-16} ${py}l5 4" style="stroke:var(--n1);stroke-width:2.2;fill:none"/><path d="M${px} ${py}v16 M${px} ${py+16}l-4 -5 M${px} ${py+16}l4 -5" style="stroke:var(--n2);stroke-width:2.2;fill:none"/>`; }
  return s+'</svg>';
}
const quiz=(()=>{
  const G1=phSVG({c11:2,c22:2,c12:1,c21:3}), G2=phSVG({c11:1,c22:1,c12:2,c21:2},{corner:true});
  const BASE=[
    {q:'Wat is het verschil tussen <b>intraspecifieke</b> en <b>interspecifieke</b> competitie?',
     o:['Intra: binnen een soort; inter: tussen twee soorten','Intra: tussen twee soorten; inter: binnen een soort','Intra: om voedsel; inter: om ruimte of nestplekken','Intra: in het model; inter: in het echte experiment'], a:0,
     h:'Denk aan de Intratuin: "in de tuin".',
     e:'Intra = binnen (tussen soortgenoten), inter = tussen (verschillende soorten). Hoe meer de niches van twee soorten overlappen, hoe sterker de interspecifieke competitie.'},
    {q:`In <span class="eqn">${EQ1}</span>: wat beschrijft c<sub>12</sub>?`,
     o:['De competitie die soort 1 ondervindt van soort 2','De competitie die soort 2 ondervindt van soort 1','De intraspecifieke competitie van soort 1','De groeisnelheid van soort 2 bij lage dichtheden'], a:0,
     h:'Kijk in welke vergelijking c<sub>12</sub> staat, en welke variabele erachter staat.',
     e:`c<sub>12</sub> staat in de vergelijking van ${N1} en vermenigvuldigt ${N2}: het effect van soort 2 op soort 1 (interspecifiek). c<sub>11</sub> en c<sub>22</sub> schalen de intraspecifieke competitie, c<sub>21</sub> het effect van soort 1 op soort 2.`},
    {q:`Hoeveel nullclines heeft ${N1} in het competitiemodel, en welke?`,
     o:[`Twee: ${N1} = 0 en ${N2} = (1 − c<sub>11</sub>${N1})/c<sub>12</sub>`,`Eén: ${N2} = (1 − c<sub>11</sub>${N1})/c<sub>12</sub>; ${N1} = 0 telt niet mee`,`Twee: ${N1} = 0 en ${N2} = 0, de beide assen van de faseruimte`,'Drie: één per parameter in de vergelijking (r, c<sub>11</sub> en c<sub>12</sub>)'], a:0,
     h:`Los ${D}${N1}/${D}t = r${N1}(1 − c<sub>11</sub>${N1} − c<sub>12</sub>${N2}) = 0 op: een product is nul als een van de factoren nul is.`,
     e:`r${N1}(…) = 0 als ${N1} = 0 (de y-as) óf als 1 − c<sub>11</sub>${N1} − c<sub>12</sub>${N2} = 0, dus ${N2} = (1 − c<sub>11</sub>${N1})/c<sub>12</sub>. Beide zijn nullclines van ${N1} (blauw), ook al is de tweede uitgedrukt als ${N2} = f(${N1}).`},
    {q:`Waar snijdt de nullcline ${N2} = (1 − c<sub>11</sub>${N1})/c<sub>12</sub> de x-as (de ${N1}-as)?`,
     o:[`Bij ${N1} = 1/c<sub>11</sub>`,`Bij ${N1} = 1/c<sub>12</sub>`,`Bij ${N1} = c<sub>11</sub>`,`Bij ${N1} = 1`], a:0,
     h:`Op de x-as geldt ${N2} = 0. Wanneer is de teller van de breuk nul?`,
     e:`Een breuk is nul als de teller nul is: 1 − c<sub>11</sub>${N1} = 0, dus ${N1} = 1/c<sub>11</sub>. De y-as snijdt deze nullcline bij ${N2} = 1/c<sub>12</sub> (vul ${N1} = 0 in).`},
    {q:`Je steekt in de faseruimte een <span class="c1">blauwe</span> ${N1}-nullcline over. Wat verandert er aan het vectorveld?`,
     o:['Het horizontale pijltje klapt om, het verticale niet','Het verticale pijltje klapt om, het horizontale niet','Beide pijltjes klappen om: het horizontale én het verticale','Niets: alleen bij een evenwicht verandert de richting'], a:0,
     h:`${N1} staat op de x-as, en op de ${N1}-nullcline is ${D}${N1}/${D}t = 0.`,
     e:`Op een ${N1}-nullcline wisselt ${D}${N1}/${D}t van teken, en ${N1} staat op de x-as: het horizontale pijltje klapt om. Over een oranje ${N2}-nullcline klapt juist het verticale pijltje om.`},
    {q:'Onder welke voorwaarde leven de twee soorten stabiel samen (coexistentie)?',
     o:['Als intraspecifieke competitie sterker is dan interspecifieke','Als interspecifieke competitie sterker is dan intraspecifieke','Als beide soorten precies dezelfde groeisnelheid r hebben','Als de nullclines elkaar nergens in de faseruimte snijden'], a:0,
     h:'Elke soort moet zichzelf meer remmen dan de ander.',
     e:'Remt elke soort zichzelf meer dan de ander (c<sub>12</sub> &lt; c<sub>22</sub> en c<sub>21</sub> &lt; c<sub>11</sub>), dan wijzen alle pijlen naar het snijpunt van de nullclines: een stabiel evenwicht (attractor) met beide soorten. Sterke interspecifieke competitie geeft juist een zadelpunt.'},
    {q:`Hieronder staan de nullclines van het competitiemodel (<span class="c1">blauw</span>: ${N1}, <span class="c2">oranje</span>: ${N2}). Wat is de uitkomst?`+G1,
     o:['Soort 1 wint altijd: competitieve exclusie','Soort 2 wint altijd: competitieve exclusie','Coexistentie met beide soorten in evenwicht','Wie er het eerst is, wint: founder control'], a:0,
     h:'De nullclines snijden niet. Tussen de lijnen: welke soort groeit nog, en welke neemt al af?',
     e:`De blauwe ${N1}-nullcline ligt helemaal buiten de oranje. Tussen de lijnen is ${D}${N1}/${D}t &gt; 0 maar ${D}${N2}/${D}t &lt; 0: soort 1 groeit nog terwijl soort 2 al krimpt. Er is geen evenwicht met beide soorten; het systeem eindigt in (1/c<sub>11</sub>, 0).`},
    {q:'En in deze faseruimte? De nullclines snijden elkaar en rechtsboven is het vectorveld al gegeven. Welk type evenwicht is het snijpunt?'+G2,
     o:['Een zadelpunt: wie er het meest is, wint (founder control)','Een stabiel punt: beide soorten blijven samen bestaan','Een instabiel punt: beide soorten sterven op den duur uit','Een stabiele spiraal: de populaties draaien naar het snijpunt toe'], a:0,
     h:'Klap de pijltjes om bij elke nullcline. Vergelijk ook de snijpunten met de assen: ligt de nullcline van elke soort op zijn eigen as buiten die van de ander?',
     e:`Op de ${N1}-as ligt het blauwe snijpunt (1/c<sub>11</sub>) voorbij het oranje (1/c<sub>21</sub>), en op de ${N2}-as ligt het oranje (1/c<sub>22</sub>) voorbij het blauwe (1/c<sub>12</sub>): interspecifieke competitie is sterker. Maak je het vectorveld af, dan wijzen de pijlen in de ene richting naar het snijpunt en in de andere ervan af: een zadelpunt. De evenwichten met één soort zijn stabiel, en de soort die aanvankelijk in de meerderheid is wint.`},
    {q:'Twee runs van een ODE-model starten met exact dezelfde begincondities. Kunnen de trajectories verschillende kanten op gaan?',
     o:['Nee: het vectorveld geeft in elk punt één unieke richting','Ja: welke kant het opgaat, hangt van het toeval af','Ja, maar alleen als ze precies op een zadelpunt starten','Nee, behalve als de populaties heel klein zijn'], a:0,
     h:'Wat geeft het vectorveld in elk punt van de faseruimte?',
     e:'Een ODE is deterministisch: in elk punt ligt de richting van verandering vast. Daarom kunnen trajectories elkaar, en zichzelf, ook nooit snijden. In een echt experiment, met toeval en kleine verschillen in de begincondities, kan dat wél anders uitpakken.'},
    {q:'Een zeldzame mutant verschijnt in een gevestigde populatie. In de faseruimte is het evenwicht met beide typen een <b>zadelpunt</b>. Wat voorspelt het model?',
     o:['De mutant verdwijnt; andersom zou de gevestigde soort ook niet kunnen invaderen','De mutant groeit en bereikt een stabiel evenwicht met de gevestigde populatie','De mutant vervangt de gevestigde populatie, want een zadelpunt is instabiel','Beide populaties sterven uit, omdat een zadelpunt geen attractor is'], a:0,
     h:'Een zadelpunt betekent founder control: wie er het eerst is, wint.',
     e:'Bij een zadelpunt wint de soort die er al in grote aantallen is. Een zeldzame mutant verdwijnt, maar andersom zou de gevestigde soort ook niet kunnen invaderen. Founder control speelt dus ook een rol in evolutionaire vragen.'},
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
    const msg=frac===1?'Perfect! Nullclines, evenwichten en invasies: je hebt het door.':frac>=.75?'Heel goed! Kijk nog even naar de vragen die je niet in één keer goed had.':frac>=.5?'Op de goede weg. Loop het stappenplan nog eens door en speel met de faseruimte.':'Lees het hoofdstuk nog eens door en speel met de interactieve figuren. Daarna gaat het vast beter!';
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
const modules={overzicht:hero,fase:fase,schets:schets,quiz:quiz};
let current='overzicht';
const TABKEY='bioms-tab-comp';   // the last opened tab is remembered in this browser
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
function init(){ document.querySelectorAll('[data-comp-app]').forEach(mount); }
window.CompApp={mount,init};
const H0=inlineHost(CS); if(H0) mount(H0);
if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();
