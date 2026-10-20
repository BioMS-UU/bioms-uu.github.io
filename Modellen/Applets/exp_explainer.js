/* Exponentiële-groei-explainer (Biologische Modellen en Statistiek, hoofdstuk 10).
 * Gebruik in Quarto (één regel, op de plek waar de app moet komen):
 *   <script src="Applets/exp_explainer.js"></script>                        -> hele app met tabbladen
 *   <script src="Applets/exp_explainer.js" data-tab="raden"></script>       -> alleen dat onderdeel
 *   <script src="Applets/exp_explainer.js" data-tab="raden" data-nav="1"></script> -> tabbladen, start op dat tabblad
 * data-tab: overzicht | raden | stappen | quiz
 * data-theme="dark|light" forceert een thema; anders volgt de app het boek (ook de donkere modus).
 * (Alternatief: <div data-exp-app data-tab="..."></div> plus het script één keer op de pagina.)
 */
(function(){
"use strict";
const CS=document.currentScript;
function inlineHost(cs){ if(!cs||!cs.dataset||cs.__appDone||!cs.parentNode) return null;
  const single=!!cs.dataset.tab;
  // a plain <script> with no data-tab becomes the full app with tabs, unless the page already uses <div data-exp-app>
  if(!single&&document.querySelector('[data-exp-app]')) return null;
  cs.__appDone=true;
  const d=document.createElement('div'); d.setAttribute('data-exp-app','');
  if(cs.dataset.tab) d.dataset.tab=cs.dataset.tab;
  if(single&&cs.dataset.nav!=='1') d.dataset.hidenav='1'; if(cs.dataset.theme) d.dataset.theme=cs.dataset.theme;
  cs.parentNode.insertBefore(d,cs); return d; }
if(window.ExpApp){ const h=inlineHost(CS); if(h) window.ExpApp.mount(h); window.ExpApp.init(); return; }
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
.mem{display:flex;flex-wrap:wrap;gap:6px 18px;font-family:var(--mono);font-size:.85rem;margin:4px 0 8px}
.mem span.undef{color:var(--muted)}
.mem span.hot{color:var(--accent)}
.mem b{font-weight:700}
`;
const HTML=`
<nav class="tabs" role="tablist">
  <button class="tab" data-tab="overzicht">In één oogopslag</button>
  <button class="tab" data-tab="raden">Snel ≠ exponentieel</button>
  <button class="tab" data-tab="stappen">Stap voor stap</button>
  <button class="tab" data-tab="quiz">Quiz ✓</button>
</nav>

<section class="panel" id="tab-overzicht">
  <div class="callout note intro">
    <p><b>Wat is dit?</b> Dit is het eerste van een reeks zelfstudie-blokken. Aan het eind van elk hoofdstuk over biologische modellen vind je er één, steeds met dezelfde drie delen: het hoofdstuk <b>in één oogopslag</b>, een paar <b>interactieve figuren</b> waarin je zelf aan de knoppen draait, en een korte <b>quiz</b>.</p>
    <p>Ze zijn bedoeld om zelf na te gaan of je de kern van het hoofdstuk begrijpt. Je hoeft niets in te leveren. Je antwoorden in de quiz worden in deze browser onthouden, dus je kunt later terugkomen, bijvoorbeeld bij de voorbereiding op de toets. Kom je er niet uit, blader dan terug naar het hoofdstuk of vraag het aan je studentassistent.</p>
  </div>
  <h2>Hoofdstuk 10 in één oogopslag</h2>
  <div class="grid2">
    <div>
      <p>Groei is <b>exponentieel</b> als de <b>percentuele toename constant</b> blijft: elke stap wordt het aantal met dezelfde factor r vermenigvuldigd. Verdubbelen (+100%) is exponentieel, maar +5% per stap óók. Snelle groei is dus niet hetzelfde als exponentiële groei.</p>
      <p class="muted" style="font-size:.88rem">Rechts loopt het schaakbord uit de legende vol: één korrel op het eerste vakje, en op elk volgend vakje het dubbele. Op een <b>log-schaal</b> vormen de aantallen een rechte lijn. Dat is dé test voor exponentiële groei.</p>
      <table class="cmp">
        <tr><th></th><th>Per stap</th><th>Op log-schaal</th></tr>
        <tr><td>Lineair</td><td>+ c (vast aantal)</td><td>buigt af</td></tr>
        <tr><td>Exponentieel</td><td>× r (vast percentage)</td><td>rechte lijn</td></tr>
        <tr><td>Logistisch</td><td>× r, maar steeds minder bij hoge N</td><td>eerst recht, dan vlak</td></tr>
      </table>
    </div>
    <div class="card">
      <canvas id="hero-cb" class="sq" style="max-width:230px"></canvas>
      <canvas id="hero-plot" class="plot-half" style="margin-top:8px"></canvas>
      <div class="muted" id="hero-lbl" style="font-size:.85rem;text-align:center;margin-top:4px;min-height:2.6em"></div></div>
  </div>

  <div class="cards">
    <div class="card ov"><span class="num">10.2</span><h4>Bergen met rijst</h4>
      <p>Eén korrel op het eerste vakje, en elk vakje het dubbele. Op het 64e vakje liggen 2<sup class="nomath">63</sup> korrels, in totaal 2<sup class="nomath">64</sup> − 1 ≈ 1,8·10<sup class="nomath">19</sup>. Wat klein begint, wordt met verdubbelingen onvoorstelbaar groot.</p></div>
    <div class="card ov"><span class="num">10.3</span><h4>Verdubbelingen modelleren</h4>
      <p>Variabele R, parameters r = 2 en n = 64, en een <b>initiële conditie</b> R(0) = 1. De <b>differentievergelijking</b> R(t+1) = 2·R(t) rekent van de ene stap naar de volgende. In <span class="nomath">R</span> doe je dat met een for-loop.</p>
      <button class="b" data-go="stappen">Loop de loop door →</button></div>
    <div class="card ov"><span class="num">10.4</span><h4>Wat is exponentieel?</h4>
      <p>Niet "snel", maar: een <b>constante percentuele toename</b>. Of dat zo is, zie je door te plotten met <span class="mono nomath">log="y"</span>: exponentiële groei is dan een rechte lijn.</p>
      <button class="b" data-go="raden">Test je oog →</button></div>
    <div class="card ov"><span class="num">10.5</span><h4>Konijnen in Australië</h4>
      <p>N(t+1) = r·N(t), met r als <b>vrije parameter</b>. Bij r &gt; 1 groeit de populatie, bij r &lt; 1 sterft ze uit. Van 24 konijnen naar meer dan honderd miljoen in tien jaar.</p></div>
    <div class="card ov"><span class="num">10.6</span><h4>Dichtheidseffecten en logistische groei</h4>
      <p>Hoe meer individuen, hoe minder groei: <b>dichtheidsafhankelijkheid</b>. De groei begint exponentieel en buigt af naar het <b>draagvermogen</b> K: een <span class="nomath">S-curve</span>. Boven K neemt de populatie af, eronder groeit ze.</p></div>
    <div class="card ov"><span class="num">Opg.</span><h4>Miniatuur Australië</h4>
      <p>Konijnen in hokjes, met een geboorte- en sterftekans. Door het toeval is de groei nooit perfect, maar wel <i>bij benadering</i> exponentieel. Een draagvermogen ontstaat vanzelf als de grashokjes op raken. Meer hierover in hoofdstuk 13.</p></div>
    <div class="card ov"><span class="num">✓</span><h4>Test jezelf</h4>
      <p>Negen korte vragen over exponentiële groei, log-schalen en draagvermogen. Bij een fout antwoord mag je het opnieuw proberen.</p>
      <button class="b primary" data-go="quiz">Start de quiz →</button></div>
  </div>

  <details>
    <summary>Terminologie</summary>
    <table class="cmp">
      <tr><th>Nederlands</th><th>Engels</th><th>Betekenis</th></tr>
      <tr><td>Initiële conditie</td><td>Initial condition</td><td>De beginwaarde van de variabele, bijvoorbeeld R(0) = 1.</td></tr>
      <tr><td>Differentievergelijking</td><td>Difference equation</td><td>Formule die de volgende waarde uit de huidige berekent, in discrete tijdstappen.</td></tr>
      <tr><td>Exponentiële groei</td><td>Exponential growth</td><td>Groei waarbij het percentage van toename constant blijft.</td></tr>
      <tr><td>Logistische groei</td><td>Logistic growth</td><td>Groei die exponentieel begint, maar afbuigt naar een evenwicht.</td></tr>
      <tr><td>Draagvermogen</td><td>Carrying capacity</td><td>Het maximale aantal individuen dat een systeem in evenwicht kan ondersteunen.</td></tr>
    </table>
  </details>
</section>

<section class="panel" id="tab-raden" hidden>
  <h2>Snel ≠ exponentieel</h2>
  <p>Links staat een groeicurve op een gewone (lineaire) schaal, rechts dezelfde curve op een <b>log-schaal</b>. Op de lineaire schaal kun je vaak niet zien of groei exponentieel is. Op de log-schaal wel: alleen een constant percentage per stap (exponentiële groei of afname) geeft daar een rechte lijn. Is deze groei <b>exponentieel</b>?</p>
  <div style="display:flex;justify-content:space-between;font-size:.88rem" class="muted"><span id="rd-count"></span><span id="rd-score"></span></div>
  <div class="grid2">
    <div class="card"><div class="ptitle">Lineaire schaal</div><canvas id="rd-lin" class="plot"></canvas></div>
    <div class="card"><div class="ptitle">Log-schaal (<span class="mono nomath">log="y"</span>)</div><canvas id="rd-log" class="plot"></canvas></div>
  </div>
  <div class="guess" id="rd-guess"><button class="b" data-ans="1">Exponentieel</button><button class="b" data-ans="0">Niet exponentieel</button></div>
  <div class="qhead" id="rd-fbwrap" hidden><div class="callout" id="rd-fb" style="flex:1;margin:0"></div><div class="qmark" id="rd-mark"></div></div>
  <div class="btns" style="justify-content:flex-end"><button class="b primary" id="rd-next" hidden>Volgende curve →</button></div>
  <div id="rd-end"></div>

  <h3>Draai zelf aan r</h3>
  <p>In N(t+1) = r·N(t) bepaalt r de percentuele verandering per stap. Welke r je ook kiest, op de log-schaal is het altijd een rechte lijn. Alleen de helling verandert.</p>
  <div class="grid2">
    <div class="card"><div class="ptitle">Lineaire schaal</div><canvas id="rs-lin" class="plot-half"></canvas></div>
    <div class="card"><div class="ptitle">Log-schaal</div><canvas id="rs-log" class="plot-half"></canvas></div>
  </div>
  <div class="controls"><div class="ctl"><label><span class="lbl">groeifactor r</span> <span id="rs-r-v"></span></label><input type="range" id="rs-r" min="0.5" max="3" step="0.05" value="2"></div></div>
  <div class="callout" id="rs-text"></div>
</section>

<section class="panel" id="tab-stappen" hidden>
  <h2>Stap voor stap door het script</h2>
  <p>Dit is het script uit het hoofdstuk, voor 10 tijdstappen. Druk op <b>Volgende regel</b> om het regel voor regel uit te voeren, precies zoals <span class="nomath">R</span> dat doet. Rechts zie je wat er daarbij in het geheugen van de computer gebeurt. De grafiek verschijnt pas als de regel met <span class="mono nomath">plot()</span> aan de beurt is.</p>
  <div class="grid2">
    <div>
      <pre class="code" id="st-code"><span class="ln" data-l="0">N &lt;- <em id="st-c-n0">24</em>          <i># initiële conditie</i></span><span class="ln" data-l="1">r &lt;- <em id="st-c-r">2</em></span><span class="ln" data-l="2">for (t in 1:10) {</span><span class="ln" data-l="3">  N[t+1] &lt;- r * N[t]   <i># volgende = r × huidige</i></span><span class="ln" data-l="4">}</span><span class="ln" data-l="5">plot(N, type='l'<em id="st-c-log"></em>)</span></pre>
      <div class="btns">
        <button class="b primary" id="st-step">Volgende regel →</button>
        <button class="b" id="st-play">▶ Afspelen</button>
        <button class="b" id="st-skip">⏭ Rest van de loop</button>
        <button class="b" id="st-reset">↺ Opnieuw</button>
      </div>
      <div class="controls">
        <div class="ctl"><label><span class="lbl">beginaantal (regel 1)</span> <span id="st-n0-v"></span></label><input type="range" id="st-n0" min="1" max="100" step="1" value="24"></div>
        <div class="ctl"><label><span class="lbl">groeifactor r (regel 2)</span> <span id="st-r-v"></span></label><input type="range" id="st-r" min="0.5" max="3" step="0.05" value="2"></div>
      </div>
      <div class="btns"><label class="chk"><input type="checkbox" id="st-log"> <span class="mono nomath">log="y"</span> in de plot-regel</label></div>
    </div>
    <div>
      <div class="card"><div class="ptitle">In het geheugen van de computer</div>
        <div class="mem" id="st-mem"></div>
        <div class="vec" id="st-vec"></div></div>
      <div class="callout" id="st-text" style="margin-top:10px"></div>
    </div>
  </div>
  <div class="card" id="st-plotwrap" hidden><div class="ptitle">De grafiek van plot(N)</div><canvas id="st-plot" class="plot-wide"></canvas>
    <div class="muted" style="font-size:.82rem;margin-top:4px">Op de x-as staat de index in de vector. Index 1 is tijdstap 0, dus na 10 stappen staat het aantal in N[11].</div></div>
</section>

<section class="panel" id="tab-quiz" hidden>
  <h2>Quiz: exponentiële groei</h2>
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
   IN ÉÉN OOGOPSLAG: het schaakbord loopt vol
   ========================================================= */
const hero=(()=>{
  const cb=$('#hero-cb'), pl=$('#hero-plot'), lbl=$('#hero-lbl');
  const LOG2=Math.log10(2);
  let k=0, acc=0, hold=1;      // k = aantal gevulde vakjes
  function board(){
    const {ctx,w,h}=fitCanvas(cb); const s=Math.min(w,h), ox=(w-s)/2, oy=(h-s)/2, cell=s/8;
    ctx.clearRect(0,0,w,h);
    for(let i=0;i<64;i++){ const r=Math.floor(i/8), c=i%8, x=ox+c*cell, y=oy+r*cell;
      ctx.fillStyle=(r+c)%2?col('grid'):col('bg'); ctx.fillRect(x,y,cell,cell);
      if(i<k){ ctx.save(); ctx.globalAlpha=0.18+0.72*i/63; ctx.fillStyle=col('accent'); ctx.fillRect(x+1,y+1,cell-2,cell-2); ctx.restore();
        ctx.fillStyle=col('fg');
        if(i<=4){ const n=1<<i, cols=Math.ceil(Math.sqrt(n)), rows=Math.ceil(n/cols), g=cell/(cols+1), rr=Math.max(1.4,cell*0.085);
          for(let j=0;j<n;j++){ const jx=x+g*(1+(j%cols)), jy=y+cell/2+(Math.floor(j/cols)-(rows-1)/2)*g; disc(ctx,jx,jy,rr,col('fg')); } }
        else { ctx.font='600 '+Math.round(cell*0.42)+'px '+FONT.slice(5); ctx.textAlign='center'; ctx.textBaseline='middle'; ctx.fillText('2'+sup(i),x+cell/2,y+cell/2+1); }
      } }
    ctx.strokeStyle=col('axis'); ctx.lineWidth=1; ctx.strokeRect(ox+0.5,oy+0.5,s-1,s-1);
  }
  function plot(){
    const f=frame(pl,{xmin:0,xmax:64,ymin:0,ymax:19.5,ylog:true,ny:4,xlab:'vakje',ylab:'korrels (log-schaal)',pad:{l:48,b:30}});
    const pts=[]; for(let i=0;i<k;i++) pts.push([i+1,i*LOG2]);
    poly(f,pts,col('accent'),2); if(k) dots(f,pts.filter((_,i)=>i%3===0||i===k-1),col('accent'),2.4);
    f.ctx.restore();
  }
  function frameFn(dt){
    if(hold>0){ hold-=dt; if(hold<=0&&k>=64) k=0; }
    else { acc+=dt; const step=k<8?0.5:k<20?0.25:0.12; if(acc>=step){ acc=0; k++; if(k>=64){ k=64; hold=3.5; } } }
    board(); plot();
    if(k===0) lbl.innerHTML='Eén korrel op het eerste vakje, en dan steeds het dubbele…';
    else if(k>=64) lbl.innerHTML=`Vakje 64: 2${sup(63)} ≈ ${big(Math.pow(2,63))} korrels. Elk vakje × 2: op de log-schaal een <b>rechte lijn</b>.`;
    else { const n=Math.pow(2,k-1); lbl.innerHTML=`Vakje ${k}: ${k<=5?n+(n===1?' korrel':' korrels'):'2'+sup(k-1)+' = '+big(n)+' korrels'}`; }
  }
  return {frame:frameFn};
})();

/* =========================================================
   SNEL ≠ EXPONENTIEEL: raadspel + sandbox
   ========================================================= */
const raden=(()=>{
  const lc=$('#rd-lin'), gc=$('#rd-log'), guess=$('#rd-guess'), fbw=$('#rd-fbwrap'), fb=$('#rd-fb'), mk=$('#rd-mark'),
        next=$('#rd-next'), cnt=$('#rd-count'), sc=$('#rd-score'), endBox=$('#rd-end');
  let seed=7; const rnd=()=>{ seed=(seed*1103515245+12345)%2147483648; return seed/2147483648; };
  const noise=Array.from({length:31},()=>1+(rnd()-0.5)*0.5);
  const logi=(()=>{ const a=[5]; for(let k=0;k<40;k++) a.push(a[k]+0.35*a[k]*(1-a[k]/5000)); return a; })();
  const mk_=(T,f)=>Array.from({length:T+1},(_,t)=>f(t));
  const CURVES=[
    {name:'Verdubbeling', v:mk_(20,t=>Math.pow(2,t)), exp:true,
     why:'Elke stap × 2: een constante toename van 100%. Op de log-schaal een rechte lijn.'},
    {name:'Vijf procent per stap', v:mk_(60,t=>Math.pow(1.05,t)), exp:true,
     why:'Langzaam, maar elke stap +5%. Een constant percentage is exponentieel, hoe klein ook. Op de log-schaal een rechte (flauwe) lijn.'},
    {name:'Lineair', v:mk_(30,t=>10+50*t), exp:false,
     why:'Elke stap komen er 50 bij: een constant <i>aantal</i>, geen constant <i>percentage</i>. Het percentage wordt juist steeds kleiner. Op de log-schaal buigt de lijn af.'},
    {name:'Derdemacht', v:mk_(30,t=>1+Math.pow(t,3)), exp:false,
     why:'Snel, maar niet exponentieel: de procentuele toename per stap daalt van 100% naar een paar procent. Op de log-schaal buigt de lijn af. Snel is dus niet hetzelfde als exponentieel.'},
    {name:'Logistisch', v:logi, exp:false,
     why:'Begint exponentieel (links een rechte lijn op de log-schaal), maar buigt af naar het draagvermogen. Als geheel is dit dus geen exponentiële groei.'},
    {name:'Afname', v:mk_(30,t=>1000*Math.pow(0.85,t)), exp:true,
     why:'Elke stap −15%: ook een constant percentage. Dit is exponentiële <i>afname</i> (r &lt; 1). Op de log-schaal een dalende rechte lijn.'},
    {name:'Metingen met ruis', v:mk_(30,t=>Math.pow(1.25,t)*noise[t]), exp:true,
     why:'Echte metingen zijn nooit perfect. Op de log-schaal liggen de punten bij benadering op een rechte lijn: bij benadering exponentieel, zoals de konijnen in Miniatuur Australië.'},
  ];
  let order=[], i=0, score=0, answered=false, done=false;
  const cur=()=>CURVES[order[i]];
  function start(){ order=shuffle(CURVES.map((_,k)=>k)); i=0; score=0; done=false; endBox.innerHTML=''; show(); }
  function show(){ answered=false; fbw.hidden=true; next.hidden=true; guess.hidden=false;
    guess.querySelectorAll('button').forEach(b=>{ b.className='b'; });
    cnt.textContent=`Curve ${i+1} van ${CURVES.length}`; sc.textContent=`${score} goed`; postHeight(); }
  function draw(){
    if(done) return;
    const C=cur();
    series(lc,C.v,{dots:true});
    series(gc,C.v,{log:true});
  }
  guess.querySelectorAll('button').forEach(b=>b.onclick=()=>{
    if(answered) return; answered=true; const C=cur(), ok=(b.dataset.ans==='1')===C.exp;
    if(ok) score++;
    guess.querySelectorAll('button').forEach(x=>x.classList.add((x.dataset.ans==='1')===C.exp?'right':'wrong'));
    mk.innerHTML=ok?MARK_OK:MARK_NO; if(ok) burst(mk);
    fb.innerHTML=`<b>${ok?'Goed!':'Niet goed.'}</b> <b>${C.name}.</b> ${C.why}`; fbw.hidden=false;
    next.textContent=i===CURVES.length-1?'Bekijk resultaat →':'Volgende curve →'; next.hidden=false;
    sc.textContent=`${score} goed`; postHeight();
  });
  next.onclick=()=>{ i++; if(i>=CURVES.length) finish(); else show(); };
  function finish(){ done=true; guess.hidden=true; fbw.hidden=true; next.hidden=true; cnt.textContent='Klaar!';
    const frac=score/CURVES.length;
    const msg=frac===1?'Perfect!':frac>=.7?'Goed gedaan. Onthoud: exponentieel is een constant percentage, en op de log-schaal een rechte lijn.':'Kijk nog eens goed naar de log-schaal: alleen een constant percentage geeft daar een rechte lijn.';
    endBox.innerHTML=`<div class="card endcard slidein"><div class="bigmark">${frac>=.5?MARK_OK:MARK_NO}</div><h3>${score} van ${CURVES.length} goed</h3><p>${msg}</p><button class="b primary" id="rd-again">↺ Nog een keer (andere volgorde)</button></div>`;
    if(frac>=.5) burst(endBox.querySelector('.bigmark'));
    $('#rd-again').onclick=start; postHeight(); }
  /* sandbox */
  const sl=$('#rs-lin'), sg=$('#rs-log'), sr=$('#rs-r'), srv=$('#rs-r-v'), st=$('#rs-text');
  function sandbox(){ const r=+sr.value, v=mk_(20,t=>10*Math.pow(r,t));
    srv.textContent=r.toFixed(2); series(sl,v,{}); series(sg,v,{log:true});
    st.innerHTML=r>1?`r = ${r.toFixed(2)}: elke stap × ${r.toFixed(2)}, dus ${pct(r)}. Een constant percentage: exponentiële groei. Hoe groter r, hoe steiler de rechte lijn op de log-schaal.`
      :r===1?`r = 1: elke stap × 1, er verandert niets. Op beide schalen een horizontale lijn.`
      :`r = ${r.toFixed(2)}: elke stap × ${r.toFixed(2)}, dus ${pct(r)}. Een constant percentage, maar nu afname: exponentiële afname. Op de log-schaal een dalende rechte lijn.`; }
  start();
  return {frame:()=>{ draw(); sandbox(); }};
})();

/* =========================================================
   STAP VOOR STAP: het script regel voor regel uitvoeren
   ========================================================= */
const stap=(()=>{
  const T=10;
  const sN0=$('#st-n0'), sR=$('#st-r'), vN0=$('#st-n0-v'), vR=$('#st-r-v'), cb=$('#st-log'), code=$('#st-code'),
        mem=$('#st-mem'), vec=$('#st-vec'), txt=$('#st-text'), pc_=$('#st-plot'), pw=$('#st-plotwrap'),
        bStep=$('#st-step'), bPlay=$('#st-play'), bSkip=$('#st-skip'), bReset=$('#st-reset');
  let N0=24, rv=2;                       // de waarden op de sliders (regel 1 en 2 van het script)
  let N=null, r=null, t=null, rd=-1, wr=-1, plotted=false, trace=[], pc=0, playing=false, acc=0, msg='';
  const hl=l=>code.querySelectorAll('.ln').forEach(s=>s.classList.toggle('on',+s.dataset.l===l));
  function setPlay(p){ playing=p; bPlay.textContent=p?'⏸ Pauze':'▶ Afspelen'; }
  function rtext(){ const p=pct(rv);
    return rv>1?` Elke stap wordt het aantal × ${rv}, oftewel ${p}: een constant percentage, dus <b>exponentiële groei</b>. Vink <span class="mono nomath">log="y"</span> aan en je ziet een rechte lijn.`
      :rv===1?` Met r = 1 verandert er niets: elke stap × 1.`
      :` Elke stap wordt het aantal × ${rv}, oftewel ${p}: een constant percentage, dus <b>exponentiële afname</b>. Op de log-schaal een dalende rechte lijn.`; }
  /* de uitvoering als lijst van stappen: welke regel, wat er intern gebeurt, en de uitleg daarbij */
  function build(){ trace=[];
    trace.push({l:0, run:()=>{ N=[N0]; }, m:()=>`<span class="nomath">R</span> maakt in het geheugen een vector <b>N</b> met één getal erin: N[1] = ${N0}. Let op: dit is tijdstap 0.`});
    trace.push({l:1, run:()=>{ r=rv; }, m:()=>`De parameter <b>r</b> krijgt de waarde ${rv}. Die staat nu ook in het geheugen.`});
    for(let k=1;k<=T;k++){
      trace.push({l:2, run:()=>{ t=k; rd=-1; wr=-1; }, m:()=>k===1?`De for-loop begint. <b>t</b> krijgt de eerste waarde uit 1:${T}, dus t = 1. Alles tussen { en } wordt nu één keer uitgevoerd.`
        :`Terug bij de for-regel: <b>t</b> wordt ${k}. Dat zit nog in 1:${T}, dus de regel tussen { en } wordt opnieuw uitgevoerd.`});
      trace.push({l:3, run:()=>{ const v=r*N[k-1]; N.push(v); rd=k-1; wr=k; }, m:()=>`<span class="nomath">R</span> rekent de rechterkant uit: r × N[t] = ${r} × N[${k}] = ${r} × ${fmt(N[k-1])} = <b>${fmt(N[k])}</b>. Dat getal komt op plek N[t+1] = N[${k+1}]: de vector wordt één getal langer.`});
    }
    trace.push({l:2, run:()=>{ rd=-1; wr=-1; }, m:()=>`Terug bij de for-regel: 1:${T} is op, t = ${T} was de laatste. De loop stopt en <span class="nomath">R</span> gaat verder ná de }.`});
    trace.push({l:5, run:()=>{ plotted=true; }, m:()=>`plot(N, type='l') tekent de vector N: de index op de x-as, de waarde op de y-as. Pas nu verschijnt de grafiek.`+rtext()+` Klaar: het script is helemaal uitgevoerd.`});
  }
  function reset(){ N0=+sN0.value; rv=+sR.value; vN0.textContent=N0; vR.textContent=rv.toFixed(2);
    $('#st-c-n0').textContent=N0; $('#st-c-r').textContent=rv;
    N=null; r=null; t=null; rd=-1; wr=-1; plotted=false; pc=0; acc=0; setPlay(false); build(); hl(-1);
    msg='Nog niets uitgevoerd. Het geheugen is leeg. Druk op <b>Volgende regel</b>.'; render(); }
  function step(){ if(pc>=trace.length) return; const st=trace[pc]; st.run(); hl(st.l); msg=st.m(); pc++;
    if(pc>=trace.length) setPlay(false); render(); }
  function render(){
    const done=pc>=trace.length; bStep.disabled=done; bPlay.disabled=done; bSkip.disabled=done||pc<2||plotted;
    const item=(k,v,hot)=>`<span class="${v==null?'undef':''}${hot?' hot':''}"><b>${k}</b>${v==null?': nog niet gedefinieerd':' = '+v}</span>`;
    const cur=pc>0?trace[pc-1].l:-1;
    mem.innerHTML=item('r',r,cur===1)+item('t',t,cur===2)+(N?`<span><b>N</b>: vector met ${N.length} getal${N.length===1?'':'len'}</span>`:item('N',null,false));
    vec.innerHTML=N?N.map((v,i)=>`<span class="cell${i===rd?' rd':''}${i===wr?' wr':''}"><b>N[${i+1}]</b><span>${fmt(v)}</span></span>`).join(''):'';
    txt.innerHTML=msg; pw.hidden=!plotted; postHeight();
  }
  function plot(){ const lg=cb.checked;
    const o=lg?{...logRange(N),ylog:true}:{ymin:0,ymax:niceCeil(Math.max(...N)*1.05)};
    const f=frame(pc_,{xmin:0,xmax:T+1,...o,xlab:'index in N',ylab:lg?'N (log-schaal)':'N',pad:{l:66}});
    const pts=N.map((v,i)=>[i+1,lg?Math.log10(v):v]);
    poly(f,pts,col('accent'),2.4); dots(f,pts,col('accent'),3.2); f.ctx.restore(); }
  bStep.onclick=()=>{ setPlay(false); step(); };
  bPlay.onclick=()=>{ if(pc>=trace.length) return; setPlay(!playing); acc=0; };
  bSkip.onclick=()=>{ setPlay(false); while(pc<trace.length&&trace[pc].l!==5) step(); };
  bReset.onclick=reset;
  sN0.oninput=reset; sR.oninput=reset;
  cb.onchange=()=>{ $('#st-c-log').textContent=cb.checked?", log='y'":''; };
  reset();
  return {frame:dt=>{ if(playing){ acc+=dt; if(acc>=0.6){ acc=0; step(); } } if(plotted) plot(); }};
})();

/* =========================================================
   QUIZ
   ========================================================= */
function logSVG(pts,yt,xmax){
  const W=300,H=150,l=46,r=14,t=12,b=28;
  const ys=pts.map(p=>p[1]), ymin=Math.min(...ys,yt[0][0])-0.15, ymax=Math.max(...ys,yt[yt.length-1][0])+0.15;
  const X=x=>l+x/xmax*(W-l-r), Y=y=>t+(ymax-y)/(ymax-ymin)*(H-t-b);
  const d=pts.map(([x,y],k)=>(k?'L':'M')+X(x).toFixed(1)+' '+Y(y).toFixed(1)).join('');
  const tk=yt.map(([v,lab])=>`<line x1="${l-3}" y1="${Y(v)}" x2="${l}" y2="${Y(v)}" style="stroke:currentColor"/><text x="${l-6}" y="${Y(v)+4}" text-anchor="end" style="font-size:11px;fill:currentColor">${lab}</text>`).join('');
  return `<svg viewBox="0 0 ${W} ${H}" width="${W}" style="max-width:100%;display:block;margin:8px 0;color:var(--muted)" role="img" aria-label="grafiek met logaritmische y-as">
    <line x1="${l}" y1="${t}" x2="${l}" y2="${H-b}" style="stroke:currentColor"/>
    <line x1="${l}" y1="${H-b}" x2="${W-r}" y2="${H-b}" style="stroke:currentColor"/>
    ${tk}<path d="${d}" style="fill:none;stroke:var(--accent);stroke-width:2.6"/>
    <text x="${l+(W-l-r)/2}" y="${H-6}" text-anchor="middle" style="font-size:11px;fill:currentColor">tijd</text>
    <text x="${l+4}" y="${t+10}" text-anchor="start" style="font-size:11px;fill:currentColor">aantal (log-schaal)</text></svg>`;
}
const quiz=(()=>{
  const G1=logSVG(Array.from({length:21},(_,t)=>[t,0.15*t]),[[0,'1'],[1,'10'],[2,'100'],[3,'1000']],20);
  const lg=(()=>{ const a=[5]; for(let k=0;k<40;k++) a.push(a[k]+0.35*a[k]*(1-a[k]/5000)); return a.map((v,t)=>[t,Math.log10(v)]); })();
  const G2=logSVG(lg,[[1,'10'],[2,'100'],[3,'1000']],40);
  const BASE=[
    {q:'Wanneer noemen we groei <b>exponentieel</b>?',
     o:['Als het percentage per stap constant is','Als er per stap een vast aantal bijkomt','Als de populatie heel snel groter wordt','Als het aantal elke stap precies verdubbelt'], a:0,
     h:'Verdubbelen is exponentieel, maar 5% per stap ook. Wat hebben die gemeen?',
     e:'Exponentieel betekent: elke stap × dezelfde factor r. Of dat nu × 2 (+100%) of × 1,05 (+5%) is, maakt niet uit.'},
    {q:'Op het schaakbord komt er per vakje niet het dubbele, maar <b>18% rijst bij</b>. Is de groei dan exponentieel?',
     o:['Ja, want het percentage per stap is constant','Nee, 18% is te langzaam voor exponentiële groei','Alleen op de eerste vakjes van het bord','Alleen als het percentage groter is dan 100%'], a:0,
     h:'Gaat het om hoe snel, of om hoe de toename per stap eruitziet?',
     e:'R(t+1) = 1,18·R(t) is een constante factor. Langzamer dan verdubbelen, maar exponentieel. Op een log-schaal is het een (minder steile) rechte lijn.'},
    {q:'Een populatie groeit van 100 naar 150, dan naar 200, dan naar 250. Is dit exponentiële groei?',
     o:['Nee: het percentage per stap neemt af','Ja: de populatie wordt elke stap groter','Ja: de toename van 50 per stap is constant','Dat kun je zonder log-grafiek niet zien'], a:0,
     h:'Reken het percentage per stap uit: 100 → 150 is +50%, en 150 → 200?',
     e:'+50%, dan +33%, dan +25%: het percentage neemt af. Er komt steeds 50 bij, en een vast aantal per stap is lineaire groei, geen exponentiële.'},
    {q:'Hoe controleer je het snelst of gemeten groei exponentieel is?',
     o:['Plot op een logaritmische y-as en kijk of het een rechte lijn is','Plot de data op een gewone schaal en kijk of de curve steil is','Trek elke waarde van de volgende af en kijk of het verschil constant is','Kijk of de populatie binnen de gemeten periode minstens verdubbelt'], a:0,
     h:'Welk argument voegde je in R toe aan plot()?',
     e:'Met <span class="mono nomath">log="y"</span> wordt exponentiële groei een rechte lijn, want elke vermenigvuldiging met r is op de log-schaal een even grote stap omhoog.'},
    {q:'Hieronder staat een grafiek met een <b>logaritmische</b> y-as. Wat kun je concluderen?'+G1,
     o:['De groei is exponentieel','De groei verloopt lineair','De groei neemt steeds af','De populatie nadert een maximum'], a:0,
     h:'Wat betekent een rechte lijn op een log-schaal?',
     e:'Een rechte lijn op de log-schaal betekent een constante factor per stap: exponentiële groei. Op een gewone schaal zou deze curve steeds steiler worden.'},
    {q:'En deze grafiek, ook met een <b>logaritmische</b> y-as?'+G2,
     o:['Eerst exponentieel, dan een draagvermogen','Exponentiële groei over de hele periode','Eerst lineaire groei, daarna stopt de groei','Eerst groei, daarna sterft de populatie uit'], a:0,
     h:'Links is de lijn recht, rechts wordt hij vlak. Wat betekent vlak?',
     e:'Eerst een rechte lijn (exponentieel), daarna buigt hij af naar een horizontale lijn: de populatie verandert niet meer en zit op het draagvermogen. Dit is logistische groei.'},
    {q:'Wat is in het rijstmodel R(t+1) = 2·R(t) de <b>initiële conditie</b>?',
     o:['R(0) = 1, de korrel op vakje 1','De factor 2 in de vergelijking','De 64 vakjes van het schaakbord','R(63), het aantal op vakje 64'], a:0,
     h:'De initiële conditie is de startwaarde van de variabele.',
     e:'De initiële conditie is de beginwaarde waarmee je gaat rekenen: één korrel op het eerste vakje. De factor 2 en het aantal vakjes zijn parameters.'},
    {q:'Bij welke waarde van r groeit de populatie in N(t+1) = r·N(t)?',
     o:['r > 1','r > 0','r > 2','Altijd'], a:0,
     h:'Wat gebeurt er als je elke stap met 0,9 vermenigvuldigt?',
     e:'Bij r > 1 wordt het aantal elke stap groter. Bij r = 1 blijft het gelijk, en bij r < 1 (zoals 0,9) neemt het exponentieel af. Let op: dit is een differentievergelijking. In het volgende hoofdstuk, bij ODEs, ligt de grens bij r > 0.'},
    {q:'Een populatie is <b>groter</b> dan het draagvermogen. Wat gebeurt er?',
     o:['Ze neemt af richting het draagvermogen','Ze groeit gewoon exponentieel verder','Ze blijft precies zo groot als ze nu is','Ze verdubbelt eerst en stort dan helemaal in'], a:0,
     h:'Veel konijnen, weinig eten: wat betekent dat voor geboorte en sterfte?',
     e:'Boven het draagvermogen is de sterfte groter dan de geboorte, dus de populatie krimpt. Onder het draagvermogen groeit ze. Het draagvermogen is dus een evenwicht.'},
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
    const msg=frac===1?'Perfect! Je weet precies wat exponentiële groei is, en wat niet.':frac>=.75?'Heel goed! Kijk nog even naar de vragen die je niet in één keer goed had.':frac>=.5?'Op de goede weg. Speel nog eens met het raadspel en de for-loop.':'Lees het hoofdstuk nog eens door en speel met de interactieve figuren. Daarna gaat het vast beter!';
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
const modules={overzicht:hero,raden:raden,stappen:stap,quiz:quiz};
let current='overzicht';
const TABKEY='bioms-tab-exp';   // the last opened tab is remembered in this browser
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
function init(){ document.querySelectorAll('[data-exp-app]').forEach(mount); }
window.ExpApp={mount,init};
const H0=inlineHost(CS); if(H0) mount(H0);
if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();
