/* Predator-prooi-explainer (Biologische Modellen en Statistiek, hoofdstuk 14).
 * Gebruik in Quarto (één regel, op de plek waar de app moet komen):
 *   <script src="Applets/predatorprooi_explainer.js"></script>                        -> hele app met tabbladen
 *   <script src="Applets/predatorprooi_explainer.js" data-tab="fase"></script>        -> alleen dat onderdeel
 *   <script src="Applets/predatorprooi_explainer.js" data-tab="fase" data-nav="1"></script> -> tabbladen, start op dat tabblad
 * data-tab: overzicht | fase | stappen | quiz
 * data-theme="dark|light" forceert een thema; anders volgt de app het boek (ook de donkere modus).
 * (Alternatief: <div data-pp-app data-tab="..."></div> plus het script één keer op de pagina.)
 */
(function(){
"use strict";
const CS=document.currentScript;
function inlineHost(cs){ if(!cs||!cs.dataset||cs.__appDone||!cs.parentNode) return null;
  const single=!!cs.dataset.tab;
  // a plain <script> with no data-tab becomes the full app with tabs, unless the page already uses <div data-pp-app>
  if(!single&&document.querySelector('[data-pp-app]')) return null;
  cs.__appDone=true;
  const d=document.createElement('div'); d.setAttribute('data-pp-app','');
  if(cs.dataset.tab) d.dataset.tab=cs.dataset.tab;
  if(single&&cs.dataset.nav!=='1') d.dataset.hidenav='1'; if(cs.dataset.theme) d.dataset.theme=cs.dataset.theme;
  cs.parentNode.insertBefore(d,cs); return d; }
if(window.PpApp){ const h=inlineHost(CS); if(h) window.PpApp.mount(h); window.PpApp.init(); return; }
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




/* predator-prooi: extra */
:host{--prey:#2d43fc;--pred:#e8700a}
:host([data-dark]){--prey:#8591ff;--pred:#ffa050}
.prey{color:var(--prey);font-weight:600}
.pred{color:var(--pred);font-weight:600}
.eqs{display:flex;flex-direction:column;align-items:center;gap:4px;margin:8px 0 10px}
.eqs .eqn{font-size:1.1em}
.callout.intro{margin:0 0 14px}
.callout.intro p{margin:.3rem 0}
.steps{display:flex;flex-direction:column;gap:6px}
.steps .steptext{text-align:left;max-width:none;margin:0;min-height:0}
.steps h3{margin:.2em 0}
.legend .ln{display:inline-block;width:22px;height:0;border-top:2.5px solid currentColor;vertical-align:middle;margin-right:4px}
`;
const HTML=`
<nav class="tabs" role="tablist">
  <button class="tab" data-tab="overzicht">In één oogopslag</button>
  <button class="tab" data-tab="fase">Faseruimte</button>
  <button class="tab" data-tab="stappen">Stap voor stap</button>
  <button class="tab" data-tab="quiz">Quiz ✓</button>
</nav>

<section class="panel" id="tab-overzicht">
  <h2>Hoofdstuk 14 in één oogopslag</h2>
  <div class="grid2">
    <div>
      <p>Populaties leven niet alleen. Als vossen konijnen eten, komen de variabelen in <b>elkaars vergelijking</b> terug: een <b><span class="nomath">2D</span>-systeem van ODEs</b>. Voor de <span class="prey">prooi</span> R en de <span class="pred">predator</span> N (het <b>Lotka-Volterra model</b> uit het hoofdstuk):</p>
      <div class="eqs">
        <span class="eqn">dR/dt = b(1 − R/K)R − dR − aRN</span>
        <span class="eqn">dN/dt = <i class="mi">caRN</i> − δN</span>
      </div>
      <p>Alle mogelijke toestanden (combinaties van R en N) vormen samen de <b>faseruimte</b>. Daarin teken je <b>nullclines</b> (lijnen waarop één variabele niet verandert) en het <b>vectorveld</b> (pijltjes: welke kant beweegt het systeem op). Waar nullclines van verschillende variabelen elkaar snijden, ligt een <b>evenwicht</b>; aan de pijltjes eromheen zie je of het <b>stabiel</b> (●) of <b>instabiel</b> (○) is.</p>
      <p class="muted" style="font-size:.88rem">Rechts loopt een trajectorie door de faseruimte: vanuit weinig konijnen en een paar vossen spiraliseert het systeem naar het stabiele evenwicht. Daaronder zie je dezelfde oplossing tegen de tijd.</p>
    </div>
    <div class="card">
      <canvas id="hero-phase" class="plot"></canvas>
      <canvas id="hero-time" class="plot-half" style="margin-top:8px"></canvas>
      <div class="muted" id="hero-lbl" style="font-size:.85rem;text-align:center;margin-top:4px;min-height:2.6em"></div></div>
  </div>

  <div class="cards">
    <div class="card ov"><span class="num">14.2</span><h4>Konijnen en vossen</h4>
      <p>Konijnen (R) hebben dichtheidsafhankelijke geboorte b en sterfte d. Predatie gaat met aRN: evenredig met beide dichtheden. Vossen (N) groeien alleen door predatie (<i class="mi">caRN</i>, met conversie c ≤ 1) en sterven met snelheid δ.</p></div>
    <div class="card ov"><span class="num">14.3</span><h4>Evenwichtsanalyse van <span class="nomath">2D</span>-systemen</h4>
      <p>Een <span class="nomath">2D</span>-systeem is pas in evenwicht als <b>beide</b> ODEs nul zijn. Oplossen kan, maar vertelt niet hoe de populaties <i>in de tijd</i> veranderen. Daarvoor gebruik je een faseruimte.</p></div>
    <div class="card ov"><span class="num">14.4</span><h4>Wat zijn faseruimtes?</h4>
      <p>Een assenstelsel met R op de x-as en N op de y-as: elk punt is een toestand. Bij lage dichtheden zijn de termen met R² en RN verwaarloosbaar: de prooi groeit (b &gt; d) en de predator sterft uit (−δN).</p>
      <button class="b" data-go="stappen">Bouw de faseruimte op →</button></div>
    <div class="card ov"><span class="num">14.5</span><h4>Nullclines en vectorvelden</h4>
      <p>Predator-nullclines: N = 0 en R = δ/(ca). Prooi-nullclines: R = 0 en N = (b(1 − R/K) − d)/a. Als je een nullcline passeert, klapt het pijltje van die variabele om.</p>
      <button class="b" data-go="fase">Naar de faseruimte →</button></div>
    <div class="card ov"><span class="num">14.6</span><h4>Stabiele en instabiele evenwichten</h4>
      <p>Verstoor het evenwicht een beetje: keert het systeem na <b>elke</b> verstoring terug, dan is het stabiel (●). Zo niet, dan instabiel (○). Het punt (0, 0) is instabiel: een paar konijnen groeien meteen uit.</p></div>
    <div class="card ov"><span class="num">14.7</span><h4>Verhoogde death rate van predatoren</h4>
      <p>Een hogere δ schuift de nullcline R = δ/(ca) naar rechts. Schuift hij voorbij het prooi-evenwicht, dan sterven de vossen uit. Een hogere b van de prooi geeft juist alleen méér vossen: tegenintuïtief!</p>
      <button class="b" data-go="fase">Probeer de scenario's →</button></div>
    <div class="card ov"><span class="num">Opg.</span><h4>Zeehonden, ruimte en virussen</h4>
      <p>Zieke zeehonden (een predator-prooi-achtig ziektemodel), Lotka-Volterra in een ruimtelijke simulatie, een SIR-virusuitbraak en een <span class="nomath">3D</span>-voedselketen. Overal dezelfde gereedschappen: nullclines, vectorveld, evenwichten.</p></div>
    <div class="card ov"><span class="num">✓</span><h4>Test jezelf</h4>
      <p>Tien korte vragen over systemen van ODEs, faseruimtes, nullclines en de stabiliteit van evenwichten. Bij een fout antwoord mag je het opnieuw proberen.</p>
      <button class="b primary" data-go="quiz">Start de quiz →</button></div>
  </div>

  <details>
    <summary>Terminologie</summary>
    <table class="cmp">
      <tr><th>Nederlands</th><th>Engels</th><th>Betekenis</th></tr>
      <tr><td>Systeem van differentiaalvergelijkingen</td><td>System of differential equations</td><td>Meerdere ODEs waarvan de variabelen elkaar beïnvloeden.</td></tr>
      <tr><td>Faseruimte</td><td>Phase space</td><td>De ruimte van alle mogelijke toestanden; bij 2 vergelijkingen een <span class="nomath">2D</span>-grafiek met op elke as één variabele.</td></tr>
      <tr><td>Nullcline</td><td>Nullcline</td><td>Lijn in de faseruimte waarop één van de variabelen niet verandert.</td></tr>
      <tr><td>Vectorveld</td><td>Vector field</td><td>De richting van verandering in elk punt van de faseruimte, meestal versimpeld tot twee pijltjes per sector.</td></tr>
      <tr><td>Trajectorie</td><td>Trajectory</td><td>Het pad dat het systeem vanuit een begintoestand door de faseruimte aflegt: een specifieke oplossing.</td></tr>
      <tr><td>Parameterverandering</td><td>Parameter change</td><td>Eén van de parameters verandert, met een biologische betekenis: meer sterfte, hogere geboorte, enzovoort.</td></tr>
    </table>
  </details>
</section>

<section class="panel" id="tab-fase" hidden>
  <h2>De faseruimte van konijnen en vossen</h2>
  <div class="eqs">
    <span class="eqn">dR/dt = b(1 − R/K)R − dR − aRN</span>
    <span class="eqn">dN/dt = <i class="mi">caRN</i> − δN</span>
  </div>
  <p>Links zie je de faseruimte met de <span class="prey">prooi-nullclines</span>, de <span class="pred">predator-nullclines</span>, het vectorveld en de evenwichten (● stabiel, ○ instabiel). <b>Klik in de faseruimte</b> om vanuit die toestand een trajectorie te starten; rechts zie je dezelfde populaties tegen de tijd. Draai aan de parameters en kijk hoe de nullclines verschuiven.</p>
  <div class="btns"><span class="muted" style="font-size:.88rem">Scenario uit het hoofdstuk:</span><div class="seg" id="fs-preset"></div></div>
  <div class="grid2">
    <div class="card"><div class="ptitle">Faseruimte (klik om een trajectorie te starten)</div><canvas id="fs-phase" class="plot"></canvas>
      <div class="legend"><span><i class="ln" style="color:var(--prey)"></i>prooi-nullclines (dR/dt = 0)</span><span><i class="ln" style="color:var(--pred)"></i>predator-nullclines (dN/dt = 0)</span><span>● stabiel</span><span>○ instabiel</span></div></div>
    <div class="card"><div class="ptitle">Dezelfde populaties tegen de tijd</div><canvas id="fs-time" class="plot"></canvas>
      <div class="legend"><span><i class="dot" style="background:var(--prey)"></i>konijnen R</span><span><i class="dot" style="background:var(--pred)"></i>vossen N</span><span class="muted">(laatst gestarte trajectorie)</span></div></div>
  </div>
  <div class="controls" id="fs-ctl"></div>
  <div class="btns">
    <button class="b" id="fs-clear">↺ Trajectories wissen</button>
    <span class="muted" style="font-size:.88rem">Pijltjes:</span>
    <div class="seg" id="fs-arrows"><button data-m="split" aria-pressed="true">per variabele</button><button data-m="joint">gecombineerd</button></div>
  </div>
  <div class="callout" id="fs-text"></div>
</section>

<section class="panel" id="tab-stappen" hidden>
  <h2>Stap voor stap: een faseruimte tekenen</h2>
  <p>Zo bouw je, met pen en papier, de faseruimte van het predator-prooi model op (paragrafen 14.4 tot en met 14.6). Druk op <b>Volgende</b> om de stappen één voor één te doorlopen.</p>
  <div class="grid2">
    <div class="card"><div class="ptitle" id="sp-title"></div><canvas id="sp-phase" class="plot"></canvas></div>
    <div class="steps">
      <div class="stepper" style="margin:0 0 4px"><button class="b" id="sp-prev">← Vorige</button><div class="dots" id="sp-dots"></div><button class="b primary" id="sp-next">Volgende →</button></div>
      <div class="callout steptext" id="sp-text"></div>
    </div>
  </div>
</section>

<section class="panel" id="tab-quiz" hidden>
  <h2>Quiz: predator-prooi interacties</h2>
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
function localXY(e,c){ const r=c.getBoundingClientRect(); return [e.clientX-r.left,e.clientY-r.top]; }
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
   HET MODEL: Lotka-Volterra met dichtheidsafhankelijke geboorte (vgl. 14.1)
   dR/dt = b(1 − R/K)R − dR − aRN,  dN/dt = caRN − δN
   De standaardwaarden geven de nullclines uit het hoofdstuk: N = 80 − R en R = 40.
   ========================================================= */
const P0={b:1,d:0.2,K:100,a:0.01,c:0.5,dl:0.2};
const PARS=[{k:'b',l:'geboorte konijnen b',min:0.3,max:2,step:0.05},{k:'d',l:'sterfte konijnen d',min:0,max:0.8,step:0.05},{k:'K',l:'draagvermogen K',min:40,max:200,step:5},
  {k:'a',l:'predatie a',min:0.002,max:0.03,step:0.001},{k:'c',l:'conversie c',min:0.1,max:1,step:0.05},{k:'dl',l:'sterfte vossen δ',min:0.05,max:1,step:0.05}];
function deriv(R,N,P){ return [P.b*(1-R/P.K)*R-P.d*R-P.a*R*N, P.c*P.a*R*N-P.dl*N]; }
const preyNull=(R,P)=>(P.b*(1-R/P.K)-P.d)/P.a;      // N = f(R): prooi-nullcline 2
const predNull=P=>P.dl/(P.c*P.a);                    // R = δ/(ca): predator-nullcline 2
function equilibria(P){
  const out=[{R:0,N:0}], Rk=P.K*(1-P.d/P.b); if(Rk>0) out.push({R:Rk,N:0});
  const Rs=predNull(P), Ns=preyNull(Rs,P); if(Ns>0&&Rs>0) out.push({R:Rs,N:Ns});
  for(const e of out){ const h=1e-4, [f0,g0]=deriv(e.R,e.N,P), [fR,gR]=deriv(e.R+h,e.N,P), [fN,gN]=deriv(e.R,e.N+h,P);
    const J11=(fR-f0)/h, J12=(fN-f0)/h, J21=(gR-g0)/h, J22=(gN-g0)/h, tr=J11+J22, det=J11*J22-J12*J21;
    e.stable=tr<0&&det>0; e.spiral=det>0&&tr*tr<4*det; }
  return out;
}
function domain(P){ return {xmax:niceCeil(P.K*1.1), ymax:niceCeil(Math.max(10,(P.b-P.d)/P.a*1.25))}; }
/* één trajectorie, geïntegreerd met Euler (stap 0,01) */
function newTraj(R,N,t=0){ return {R,N,t,hist:[[t,R,N]]}; }
function stepTraj(tr,P,dtModel){ const n=Math.max(1,Math.ceil(dtModel/0.01)), h=dtModel/n;
  for(let i=0;i<n;i++){ const [f,g]=deriv(tr.R,tr.N,P); tr.R=Math.max(0,tr.R+f*h); tr.N=Math.max(0,tr.N+g*h); }
  tr.t+=dtModel; tr.hist.push([tr.t,tr.R,tr.N]); }
function arrow(ctx,x0,y0,x1,y1,color,lw=2){ const dx=x1-x0, dy=y1-y0, L=Math.hypot(dx,dy); if(L<1) return; const ux=dx/L, uy=dy/L, hs=Math.min(7,L*0.5);
  ctx.save(); ctx.strokeStyle=color; ctx.fillStyle=color; ctx.lineWidth=lw; ctx.lineCap='round'; ctx.beginPath(); ctx.moveTo(x0,y0); ctx.lineTo(x1-ux*hs*0.6,y1-uy*hs*0.6); ctx.stroke();
  ctx.beginPath(); ctx.moveTo(x1,y1); ctx.lineTo(x1-ux*hs-uy*hs*0.55,y1-uy*hs+ux*hs*0.55); ctx.lineTo(x1-ux*hs+uy*hs*0.55,y1-uy*hs-ux*hs*0.55); ctx.closePath(); ctx.fill(); ctx.restore(); }
/* tekent de faseruimte; o: {nullR,nullN,eq,arrows:'grid'|[[R,N],...],joint,big,trajs,active} */
function drawPhase(c,P,o){
  const D=domain(P), F=frame(c,{xmin:0,xmax:D.xmax,ymin:0,ymax:D.ymax,xlab:'konijnen R (prooi)',ylab:'vossen N (predator)',pad:{l:50,r:14,t:14,b:38}});
  const cx=F.ctx, prey=col('prey'), pred=col('pred');
  if(o.nullN){ cx.save(); cx.strokeStyle=pred; cx.lineWidth=2.4; cx.beginPath(); cx.moveTo(F.p.l,F.Y(0)); cx.lineTo(F.p.l+F.pw,F.Y(0)); cx.stroke();
    const Rs=predNull(P); if(Rs<=D.xmax){ cx.beginPath(); cx.moveTo(F.X(Rs),F.p.t); cx.lineTo(F.X(Rs),F.p.t+F.ph); cx.stroke(); } cx.restore(); }
  if(o.nullR){ cx.save(); cx.strokeStyle=prey; cx.lineWidth=2.4; cx.beginPath(); cx.moveTo(F.X(0),F.p.t); cx.lineTo(F.X(0),F.p.t+F.ph); cx.stroke(); cx.restore();
    poly(F,[[0,preyNull(0,P)],[D.xmax,preyNull(D.xmax,P)]],prey,2.4); }
  const pts=o.arrows==='grid'?(()=>{ const a=[]; for(let i=0;i<8;i++) for(let j=0;j<7;j++) a.push([D.xmax*(i+0.5)/8,D.ymax*(j+0.5)/7]); return a; })():(o.arrows||[]);
  const L=o.big?22:9, lw=o.big?2.4:1.6;
  for(const [R,N] of pts){ const [f,g]=deriv(R,N,P), px=F.X(R), py=F.Y(N);
    if(o.joint){ const dx=f/D.xmax*F.pw, dy=-g/D.ymax*F.ph, m=Math.hypot(dx,dy); if(m<1e-9) continue; arrow(cx,px-dx/m*L*0.5,py-dy/m*L*0.5,px+dx/m*L*0.5,py+dy/m*L*0.5,col('muted'),lw); }
    else { const sx=Math.sign(f), sy=Math.sign(g); if(sx) arrow(cx,px,py,px+sx*L,py,prey,lw); if(sy) arrow(cx,px,py,px,py-sy*L,pred,lw); } }
  const trajs=o.trajs||[];
  trajs.forEach((tr,k)=>{ const act=k===trajs.length-1; poly(F,tr.hist.map(h=>[h[1],h[2]]),col('fg'),act?2.2:1.4,null,act?.9:.35);
    disc(cx,F.X(tr.R),F.Y(tr.N),act?5:3.5,col('fg'),col('bg'),1.2); if(tr.hist.length>1) disc(cx,F.X(tr.hist[0][1]),F.Y(tr.hist[0][2]),3,col('bg'),col('fg'),1.2); });
  cx.restore();
  if(o.eq) for(const e of equilibria(P)){ if(e.R>D.xmax||e.N>D.ymax) continue; disc(cx,F.X(e.R),F.Y(e.N),6.5,e.stable?col('fg'):col('bg'),col('fg'),2); }
  return F;
}
function drawTime(c,P,tr,Tend){
  const D=domain(P), ym=Math.max(D.xmax,D.ymax);
  const F=frame(c,{xmin:0,xmax:Tend,ymin:0,ymax:ym,xlab:'tijd',ylab:'dichtheid',pad:{l:50,r:14,t:10,b:36}});
  if(tr){ poly(F,tr.hist.map(h=>[h[0],h[1]]),col('prey'),2.2); poly(F,tr.hist.map(h=>[h[0],h[2]]),col('pred'),2.2); }
  F.ctx.restore(); return F;
}
const eqLabel=e=>`(${fmt(e.R,3)}, ${fmt(e.N,3)}) <span class="muted">${e.stable?'stabiel ●':'instabiel ○'}</span>`;

/* =========================================================
   IN ÉÉN OOGOPSLAG: een trajectorie spiraliseert naar het evenwicht
   ========================================================= */
const hero=(()=>{
  const pc=$('#hero-phase'), tc=$('#hero-time'), lbl=$('#hero-lbl'), TEND=60;
  let tr=newTraj(10,5), hold=0;
  function frameFn(dt){
    if(hold>0){ hold-=dt; if(hold<=0) tr=newTraj(10,5); }
    else { stepTraj(tr,P0,Math.min(dt,0.05)*7); if(tr.t>=TEND) hold=3; }
    drawPhase(pc,P0,{nullR:true,nullN:true,eq:true,arrows:'grid',trajs:[tr]});
    drawTime(tc,P0,tr,TEND);
    const ph=tr.t<8?'Weinig konijnen en een paar vossen: de konijnen groeien eerst bijna exponentieel, de vossen nemen af.':tr.t<25?'Met veel konijnen groeien de vossen, en daardoor nemen de konijnen weer af: het systeem draait om het evenwicht heen.':'De slingering dempt uit: het snijpunt van de twee nullclines is een <b>stabiel evenwicht</b> (●).';
    if(lbl.__last!==ph){ lbl.innerHTML=ph; lbl.__last=ph; }
  }
  return {frame:frameFn};
})();

/* =========================================================
   FASERUIMTE: sliders, scenario's, klikbare trajectories
   ========================================================= */
const fase=(()=>{
  const pc=$('#fs-phase'), tc=$('#fs-time'), ctl=$('#fs-ctl'), seg=$('#fs-preset'), txt=$('#fs-text'), TEND=80;
  const PRESETS=[{n:'Standaard',p:{}},{n:'Hogere sterfte vossen (δ × 2,5)',p:{dl:0.5}},{n:'Hogere geboorte konijnen (b × 1,3)',p:{b:1.3}}];
  let P={...P0}, trajs=[], joint=false, lastF=null, sliders=[];
  sliders=PARS.map(d=>{ const w=document.createElement('div'); w.className='ctl';
    w.innerHTML=`<label><span class="lbl">${d.l}</span> <span class="val"></span></label><input type="range" min="${d.min}" max="${d.max}" step="${d.step}" value="${P0[d.k]}">`;
    ctl.appendChild(w); const inp=w.querySelector('input'); inp.addEventListener('input',()=>{ readP(); markPreset(); reset(); });
    return {d,inp,out:w.querySelector('.val')}; });
  PRESETS.forEach((pr,k)=>{ const b=document.createElement('button'); b.textContent=pr.n; b.dataset.k=k; b.onclick=()=>setPreset(k); seg.appendChild(b); });
  $('#fs-arrows').querySelectorAll('button').forEach(b=>b.onclick=()=>{ joint=b.dataset.m==='joint'; $('#fs-arrows').querySelectorAll('button').forEach(x=>x.setAttribute('aria-pressed',x===b)); });
  function readP(){ P={}; for(const s of sliders){ P[s.d.k]=+s.inp.value; s.out.textContent=s.inp.value; } }
  function setPreset(k){ const Q={...P0,...PRESETS[k].p}; for(const s of sliders) s.inp.value=Q[s.d.k]; readP(); markPreset(); reset(); }
  function markPreset(){ [...seg.children].forEach((b,k)=>{ const Q={...P0,...PRESETS[k].p}; b.setAttribute('aria-pressed',PARS.every(d=>Math.abs(Q[d.k]-P[d.k])<1e-9)); }); }
  function reset(){ trajs=[newTraj(10,5)]; }
  $('#fs-clear').onclick=()=>{ trajs=[]; };
  pc.addEventListener('click',e=>{ if(!lastF) return; const [px,py]=localXY(e,pc); if(!lastF.inside(px,py)) return; const [R,N]=lastF.inv(px,py);
    trajs.push(newTraj(Math.max(0.01,R),Math.max(0.01,N))); if(trajs.length>5) trajs.shift(); });
  setPreset(0);
  function frameFn(dt){
    const ds=Math.min(dt,0.05)*7; for(const tr of trajs) if(tr.t<TEND) stepTraj(tr,P,ds);
    lastF=drawPhase(pc,P,{nullR:true,nullN:true,eq:true,arrows:'grid',joint,trajs});
    drawTime(tc,P,trajs[trajs.length-1],TEND);
    const eqs=equilibria(P), inner=eqs.find(e=>e.N>0), Rs=predNull(P), Rk=P.K*(1-P.d/P.b);
    let s='<b>Evenwichten</b> (snijpunten van een prooi- en een predator-nullcline): '+eqs.map(eqLabel).join(', ')+'. ';
    if(P.b<=P.d) s+='Omdat b ≤ d kunnen de konijnen zelfs zonder vossen niet groeien: alles sterft uit.';
    else if(inner) s+=`De predator-nullcline R = δ/(ca) = ${fmt(Rs,3)} ligt links van het prooi-evenwicht K(1 − d/b) = ${fmt(Rk,3)}, dus de vossen kunnen zich handhaven. Het evenwicht met beide soorten is stabiel${inner.spiral?' en wordt via een inwaartse spiraal bereikt':''}. Let op: de hoeveelheid konijnen in evenwicht hangt alleen af van δ, c en a; meer geboorte b levert dus alleen méér vossen op.`;
    else s+=`De predator-nullcline R = δ/(ca) = ${fmt(Rs,3)} ligt rechts van het prooi-evenwicht K(1 − d/b) = ${fmt(Rk,3)}: zoveel konijnen komen er nooit, dus de vossen sterven uit. Het evenwicht met alleen konijnen is nu stabiel geworden.`;
    if(txt.__last!==s){ txt.innerHTML=s; txt.__last=s; }
  }
  return {frame:frameFn};
})();

/* =========================================================
   STAP VOOR STAP: de faseruimte opbouwen (14.4 t/m 14.6)
   ========================================================= */
const stap=(()=>{
  const pc=$('#sp-phase'), title=$('#sp-title'), txt=$('#sp-text'), dots=$('#sp-dots'), bP=$('#sp-prev'), bN=$('#sp-next');
  const LOW=[[10,14]], HIGH=[[100,8]], ALL=[[10,14],[100,8],[55,8],[18,88]];
  const STEPS=[
    {t:'1. Het assenstelsel',o:{},x:'Zet de prooi R op de x-as en de predator N op de y-as. Elk punt in dit vlak is één <b>toestand</b>: een combinatie van konijnen en vossen. We gaan uitzoeken welke kant het systeem vanuit elk punt op beweegt.'},
    {t:'2. Weinig konijnen, weinig vossen',o:{arrows:LOW,big:true},x:'Bij lage dichtheden zijn de termen met R² (dichtheidseffect) en RN (predatie) verwaarloosbaar klein. Er blijft over: dR/dt ≈ bR − dR &gt; 0 (want b &gt; d) en dN/dt ≈ −δN &lt; 0. Dus linksonder: <span class="prey">konijnen nemen toe →</span>, <span class="pred">vossen nemen af ↓</span>.'},
    {t:'3. Veel konijnen, weinig vossen',o:{arrows:HIGH,big:true},x:'Met veel konijnen gaat de geboorte per konijn omlaag en worden er meer konijnen opgegeten: dR/dt wordt negatief. De vossen hebben juist voordeel bij veel prooi: dN/dt wordt positief. Rechtsonder dus <span class="prey">←</span> en <span class="pred">↑</span>. Ergens tussen beide punten zijn de pijltjes omgeklapt.'},
    {t:'4. Predator-nullclines',o:{arrows:HIGH.concat(LOW),big:true,nullN:true},x:'Waar klapt het oranje pijltje om? Daar waar dN/dt = 0: <i class="mi">caRN</i> − δN = N(<i class="mi">caR</i> − δ) = 0. Dat geeft <b>N = 0</b> (de x-as) en <b>R = δ/(ca)</b> (een verticale lijn, hier bij R = 40). Op deze <span class="pred">nullclines</span> verandert het aantal vossen niet.'},
    {t:'5. Prooi-nullclines',o:{arrows:HIGH.concat(LOW),big:true,nullN:true,nullR:true},x:'Hetzelfde voor dR/dt = 0: R(b(1 − R/K) − d − aN) = 0. Dus <b>R = 0</b> (de y-as) en <b>N = (b(1 − R/K) − d)/a</b>: een schuine lijn die daalt met R (hier N = 80 − R). Op deze <span class="prey">nullclines</span> verandert het aantal konijnen niet.'},
    {t:'6. Het vectorveld',o:{arrows:ALL,big:true,nullN:true,nullR:true},x:'Als je een nullcline passeert, passeer je de 0 en klapt het pijltje van <i>die</i> variabele om. Zo vul je alle sectoren in: rechts van R = 40 wijzen de oranje pijlen omhoog, erboven de blauwe lijn wijzen de blauwe pijlen naar links. Samen vormen de pijltjes het <b>vectorveld</b>.'},
    {t:'7. Evenwichten en stabiliteit',o:{arrows:ALL,big:true,nullN:true,nullR:true,eq:true},x:'Waar een prooi-nullcline een predator-nullcline snijdt, veranderen <b>beide</b> populaties niet: een evenwicht. (0, 0) en (80, 0) zijn <b>instabiel</b> (○): een paar konijnen erbij groeien uit, een paar vossen erbij bij (80, 0) groeien ook. Alleen (40, 40) is <b>stabiel</b> (●): na elke kleine verstoring keert het systeem terug.'},
    {t:'8. Een trajectorie aflezen',o:{arrows:'grid',nullN:true,nullR:true,eq:true,traj:true},x:'Volg de pijltjes vanuit een begintoestand en je tekent een <b>trajectorie</b>: een specifieke oplossing, zonder dat je de ODEs hoeft op te lossen. Vanuit (10, 5) spiraliseert het systeem naar het stabiele evenwicht. Speel verder op het tabblad <b>Faseruimte</b>.'}];
  let i=0, tr=null;
  function show(){ const S=STEPS[i]; title.textContent=S.t; txt.innerHTML=S.x; tr=S.o.traj?newTraj(10,5):null;
    bP.disabled=i===0; bN.disabled=i===STEPS.length-1;
    dots.innerHTML=STEPS.map((_,k)=>`<i class="${k===i?'on':''}" data-k="${k}"></i>`).join(''); dots.querySelectorAll('i').forEach(d=>d.onclick=()=>{ i=+d.dataset.k; show(); }); postHeight(); }
  bP.onclick=()=>{ if(i>0){ i--; show(); } }; bN.onclick=()=>{ if(i<STEPS.length-1){ i++; show(); } };
  show();
  return {frame:dt=>{ if(tr&&tr.t<60) stepTraj(tr,P0,Math.min(dt,0.05)*7); drawPhase(pc,P0,{...STEPS[i].o,trajs:tr?[tr]:[]}); }};
})();

/* =========================================================
   QUIZ
   ========================================================= */
/* faseruimte als inline svg: o.arrows=[[R,N,dR,dN]], o.eq=[[R,N,stable,label]], o.nulls, o.mark=[R,N,label] */
function ppSVG(o){
  const W=300,H=200,l=34,r=12,t=12,b=26, xm=120, ym=100, X=x=>l+x/xm*(W-l-r), Y=y=>t+(ym-y)/ym*(H-t-b);
  const AR=(x0,y0,x1,y1,c)=>`<line x1="${x0}" y1="${y0}" x2="${x1}" y2="${y1}" style="stroke:${c};stroke-width:2.2" marker-end="url(#pp-ah-${c.replace(/[^a-z]/g,'')})"/>`;
  let s=`<svg viewBox="0 0 ${W} ${H}" width="${W}" style="max-width:100%;display:block;margin:8px 0;color:var(--muted)" role="img" aria-label="faseruimte"><defs>`;
  for(const c of ['var(--prey)','var(--pred)']) s+=`<marker id="pp-ah-${c.replace(/[^a-z]/g,'')}" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="5" markerHeight="5" orient="auto"><path d="M0 0L10 5L0 10z" style="fill:${c}"/></marker>`;
  s+=`</defs><line x1="${l}" y1="${t}" x2="${l}" y2="${H-b}" style="stroke:currentColor"/><line x1="${l}" y1="${H-b}" x2="${W-r}" y2="${H-b}" style="stroke:currentColor"/>`;
  if(o.nulls){ s+=`<line x1="${X(0)}" y1="${t}" x2="${X(0)}" y2="${Y(0)}" style="stroke:var(--prey);stroke-width:2.4"/><line x1="${X(0)}" y1="${Y(80)}" x2="${X(80)}" y2="${Y(0)}" style="stroke:var(--prey);stroke-width:2.4"/>`;
    s+=`<line x1="${X(0)}" y1="${Y(0)}" x2="${X(xm)}" y2="${Y(0)}" style="stroke:var(--pred);stroke-width:2.4"/><line x1="${X(40)}" y1="${t}" x2="${X(40)}" y2="${Y(0)}" style="stroke:var(--pred);stroke-width:2.4"/>`; }
  for(const [R,N,dR,dN] of o.arrows||[]){ if(dR) s+=AR(X(R),Y(N),X(R)+dR*18,Y(N),'var(--prey)'); if(dN) s+=AR(X(R),Y(N),X(R),Y(N)-dN*18,'var(--pred)'); }
  for(const [R,N,st,lab] of o.eq||[]) s+=`<circle cx="${X(R)}" cy="${Y(N)}" r="5" style="fill:${st?'var(--fg)':'var(--bg)'};stroke:var(--fg);stroke-width:1.8"/><text x="${X(R)+9}" y="${Y(N)-7}" style="font-size:11px;fill:var(--fg);font-weight:600">${lab}</text>`;
  if(o.mark){ const [R,N,lab]=o.mark; s+=`<circle cx="${X(R)}" cy="${Y(N)}" r="6" style="fill:var(--accent)"/><text x="${X(R)-9}" y="${Y(N)-9}" text-anchor="end" style="font-size:11px;fill:var(--accent);font-weight:600">${lab}</text>`; }
  s+=`<text x="${W-r}" y="${H-8}" text-anchor="end" font-style="italic" style="font-size:12px;fill:var(--prey)">R</text><text x="${l+6}" y="${t+10}" font-style="italic" style="font-size:12px;fill:var(--pred)">N</text></svg>`;
  return s;
}
const quiz=(()=>{
  const G1=ppSVG({nulls:true,mark:[100,20,'P']});
  const G2=ppSVG({nulls:true,arrows:[[10,14,1,-1],[100,8,-1,1],[55,8,1,1],[24,78,-1,-1]],eq:[[0,0,false,'1'],[80,0,false,'2'],[40,40,true,'3']]});
  const BASE=[
    {q:'Waarom noemen we het konijn-vos model een <b>systeem</b> van differentiaalvergelijkingen?',
     o:['R en N staan in elkaars vergelijking: hun dynamica is gekoppeld','Er zijn twee vergelijkingen, en twee vergelijkingen is altijd een systeem','De vergelijkingen gebruiken dezelfde parameters, dus horen ze bij elkaar','Het model beschrijft een ecosysteem, en een ecosysteem is een systeem'], a:0,
     h:'Kijk naar de term aRN: in welke vergelijkingen staat die?',
     e:'In dR/dt staat N (via de predatie aRN) en in dN/dt staat R (via <i class="mi">caRN</i>). De vergelijkingen hangen van elkaar af: je kunt ze niet los van elkaar oplossen.'},
    {q:'De predatieterm is aRN. Waarom het product R·N?',
     o:['Het aantal ontmoetingen konijn–vos is evenredig met beide dichtheden','Omdat elke vos per tijdseenheid precies één konijn eet, onafhankelijk van R','Omdat de predatie alleen van het aantal vossen afhangt, niet van de konijnen','Omdat de konijnen anders sneller groeien dan de vossen ze kunnen opeten'], a:0,
     h:'Wat gebeurt er met de predatie als je R verdubbelt? En als je N verdubbelt?',
     e:'Predatie vereist een ontmoeting tussen een konijn en een vos. Het aantal ontmoetingen is evenredig met beide dichtheden (massawerking), en a schroeft de predatie omhoog of omlaag.'},
    {q:'Wat is een <b>nullcline</b>?',
     o:['Een lijn waarop één van de twee variabelen niet verandert','Een lijn waarop beide variabelen tegelijk niet veranderen','De x-as of de y-as van de faseruimte, waar een variabele nul is','Het pad dat het systeem in de loop van de tijd door de faseruimte aflegt'], a:0,
     h:'Wat betekent het als dN/dt = 0 op een hele lijn?',
     e:'Op een nullcline is de ODE van één variabele gelijk aan 0. Aan de ene kant van de lijn is die verandering positief, aan de andere kant negatief: daar klapt het pijltje om.'},
    {q:'Je stelt dN/dt = <i class="mi">caRN</i> − δN gelijk aan 0. Welke predator-nullclines vind je?',
     o:['N = 0 en de lijn R = δ/(ca)','N = 0 en de lijn N = δ/(ca)','R = 0 en N = (b(1 − R/K) − d)/a','Alleen de lijn R = δ/(ca)'], a:0,
     h:'Haal eerst N buiten haakjes: N(<i class="mi">caR</i> − δ) = 0. Een product is nul als één van de factoren nul is.',
     e:'N(<i class="mi">caR</i> − δ) = 0 geeft N = 0 (de x-as) of R = δ/(ca): een verticale lijn. Vergeet de eerste oplossing niet: zonder vossen verandert het aantal vossen ook niet.'},
    {q:'In de faseruimte hieronder ligt punt P: veel konijnen, weinig vossen. Welke kant beweegt het systeem vanuit P op?'+G1,
     o:['Konijnen nemen af, vossen nemen toe','Konijnen nemen toe, vossen nemen af','Konijnen nemen toe, vossen nemen toe','Konijnen nemen af, vossen nemen af'], a:0,
     h:'P ligt rechts van de verticale predator-nullcline en rechts van de schuine prooi-nullcline. Aan welke kant van elke lijn ligt P ten opzichte van de oorsprong?',
     e:'Rechts van R = δ/(ca) is er genoeg prooi: dN/dt &gt; 0. Boven of rechts van de prooi-nullcline is er te veel dichtheidseffect en predatie: dR/dt &lt; 0. Dus ← voor de konijnen en ↑ voor de vossen.'},
    {q:'Hieronder zijn de drie evenwichten genummerd. Welk evenwicht is <b>stabiel</b>?'+G2,
     o:['Alleen evenwicht 3','Alleen evenwicht 1','Alleen evenwicht 2','Evenwicht 2 en 3'], a:0,
     h:'Een evenwicht is stabiel als het systeem na elke kleine verstoring terugkeert. Kijk naar de pijltjes rondom elk punt.',
     e:'Bij (0, 0) groeit een handvol konijnen meteen uit, bij (80, 0) groeit een handvol vossen uit: allebei instabiel. Alleen bij het snijpunt van de twee schuine/verticale nullclines nemen verstoringen af: dat is de attractor.'},
    {q:'Wanneer is een evenwicht in een <span class="nomath">2D</span>-systeem <b>stabiel</b>?',
     o:['Als het systeem na een kleine verstoring naar het evenwicht terugkeert','Als beide ODEs er gelijk zijn aan 0, zodat er niets meer verandert','Als het precies op het snijpunt van een prooi- en een predator-nullcline ligt','Als er zowel konijnen als vossen in het evenwicht aanwezig zijn'], a:0,
     h:'Alle evenwichten liggen op snijpunten van nullclines en hebben beide ODEs gelijk aan 0. Wat onderscheidt stabiel van instabiel?',
     e:'Elk snijpunt van een prooi- en een predator-nullcline is een evenwicht, maar alleen als het systeem na een kleine verstoring (een beetje prooi of predator erbij) terugkeert, is het stabiel. Eén verstoring die groeit, maakt het al instabiel.'},
    {q:'De sterfte van de vossen δ wordt veel hoger. Wat gebeurt er met de faseruimte?',
     o:['De nullcline R = δ/(ca) schuift naar rechts en de vossen kunnen uitsterven','De prooi-nullcline schuift omhoog en in het evenwicht komen meer vossen','De nullclines blijven liggen; alleen de snelheid van het vectorveld verandert','De nullcline R = δ/(ca) schuift naar links, zodat er meer konijnen overblijven'], a:0,
     h:'Welke van de vier nullclines bevat δ? Wat gebeurt er met δ/(ca) als δ groter wordt?',
     e:'Alleen R = δ/(ca) hangt van δ af, en die waarde wordt groter: de verticale lijn schuift naar rechts. Komt hij voorbij het prooi-evenwicht K(1 − d/b), dan is er geen snijpunt met beide soorten meer en wordt het evenwicht met alleen konijnen stabiel.'},
    {q:'De geboortesnelheid b van de konijnen gaat omhoog. Wat verandert er in het evenwicht met beide soorten?',
     o:['Evenveel konijnen, maar meer vossen','Meer konijnen, evenveel vossen','Meer konijnen én meer vossen','Meer konijnen, maar juist minder vossen'], a:0,
     h:'Het evenwicht ligt op R = δ/(ca). Staat b daarin?',
     e:'De hoeveelheid konijnen in evenwicht wordt bepaald door de predator-nullcline R = δ/(ca), waar b niet in voorkomt. De prooi-nullcline schuift omhoog, dus het snijpunt komt hoger te liggen: méér vossen. Tegenintuïtief, maar zo werkt het.'},
    {q:'Wat betekent de parameter c in dN/dt = <i class="mi">caRN</i> − δN, en waarom nemen we c ≤ 1?',
     o:['De omzetting van prooi in nieuwe vossen; per gegeten konijn hooguit één vos','De sterfte van de vossen per tijdseenheid; meer dan alle vossen kan niet','De kans dat een vos een konijn tegenkomt; een kans is nooit groter dan 1','Het draagvermogen van de vossen, uitgedrukt als een fractie tussen nul en één'], a:0,
     h:'De predatieterm aRN komt terug in de vossenvergelijking, maar met een extra factor ervoor.',
     e:'c is de conversieparameter: de biomassa van konijnen vertaalt niet één op één in vossen. Met c ≤ 1 levert elk geconsumeerd konijn hooguit één nieuwe vos op.'},
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
    const msg=frac===1?'Perfect! Nullclines, vectorvelden en stabiliteit: je hebt het door.':frac>=.75?'Heel goed! Kijk nog even naar de vragen die je niet in één keer goed had.':frac>=.5?'Op de goede weg. Loop de stappen nog eens door en speel met de scenario\'s in de faseruimte.':'Lees het hoofdstuk nog eens door en bouw de faseruimte stap voor stap op. Daarna gaat het vast beter!';
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
const modules={overzicht:hero,fase:fase,stappen:stap,quiz:quiz};
let current='overzicht';
const TABKEY='bioms-tab-pp';   // the last opened tab is remembered in this browser
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
function init(){ document.querySelectorAll('[data-pp-app]').forEach(mount); }
window.PpApp={mount,init};
const H0=inlineHost(CS); if(H0) mount(H0);
if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();
