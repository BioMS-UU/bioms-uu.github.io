/* Exciteerbare-media-explainer (Biologische Modellen en Statistiek, hoofdstuk 18).
 * Gebruik in Quarto (één regel, op de plek waar de app moet komen):
 *   <script src="Applets/exciteerbaar_explainer.js"></script>                        -> hele app met tabbladen
 *   <script src="Applets/exciteerbaar_explainer.js" data-tab="cel"></script>         -> alleen dat onderdeel
 *   <script src="Applets/exciteerbaar_explainer.js" data-tab="cel" data-nav="1"></script> -> tabbladen, start op dat tabblad
 * data-tab: overzicht | cel | golven | quiz
 * data-theme="dark|light" forceert een thema; anders volgt de app het boek (ook de donkere modus).
 * (Alternatief: <div data-exc-app data-tab="..."></div> plus het script één keer op de pagina.)
 */
(function(){
"use strict";
const CS=document.currentScript;
function inlineHost(cs){ if(!cs||!cs.dataset||cs.__appDone||!cs.parentNode) return null;
  const single=!!cs.dataset.tab;
  // a plain <script> with no data-tab becomes the full app with tabs, unless the page already uses <div data-exc-app>
  if(!single&&document.querySelector('[data-exc-app]')) return null;
  cs.__appDone=true;
  const d=document.createElement('div'); d.setAttribute('data-exc-app','');
  if(cs.dataset.tab) d.dataset.tab=cs.dataset.tab;
  if(single&&cs.dataset.nav!=='1') d.dataset.hidenav='1'; if(cs.dataset.theme) d.dataset.theme=cs.dataset.theme;
  cs.parentNode.insertBefore(d,cs); return d; }
if(window.ExcApp){ const h=inlineHost(CS); if(h) window.ExcApp.mount(h); window.ExcApp.init(); return; }
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


/* exciteerbare media: extra */
.wide{display:block;width:100%;aspect-ratio:192/80;image-rendering:pixelated;cursor:crosshair;border:1px solid var(--line)}
.eqs{display:flex;flex-direction:column;gap:2px;align-items:center;margin:6px 0 8px}
.stat{margin-left:auto;font-family:var(--mono);font-size:.78rem}
`;
const HTML=`
<nav class="tabs" role="tablist">
  <button class="tab" data-tab="overzicht">In één oogopslag</button>
  <button class="tab" data-tab="cel">Eén cel</button>
  <button class="tab" data-tab="golven">Golven en spiralen</button>
  <button class="tab" data-tab="quiz">Quiz ✓</button>
</nav>

<section class="panel" id="tab-overzicht">
  <h2>Hoofdstuk 18 in één oogopslag</h2>
  <div class="grid2">
    <div>
      <p>Een zenuw- of hartcel is <b>exciteerbaar</b>: in <b>rust</b> gebeurt er niets, een kleine prikkel dooft meteen uit, maar een prikkel boven de <b>drempel</b> geeft een <i>all-or-nothing</i> respons: een <b>actiepotentiaal</b>. Daarna is de cel even ongevoelig voor nieuwe prikkels: de <b>refractaire periode</b>.</p>
      <p>Hodgkin en Huxley beschreven dit met vier ODEs (V en de natrium- en kaliumkanalen). FitzHugh en Nagumo versimpelden dat tot twee vergelijkingen, met V als voltage en W als "rem":</p>
      <div class="eqs"><span class="eqn">dV/dt = −V(V − a)(V − 1) − W</span><span class="eqn">dW/dt = ε(V − bW)</span></div>
      <p>Zet je deze cellen in hokjes en laat je V diffunderen (een PDE), dan loopt een actiepotentiaal als <b>golf</b> door het weefsel. De refractaire band erachter geeft de golf een richting. Een <b>blokkade</b> (littekenweefsel) laat een golf krullen, en een gebroken golf kan om zichzelf heen draaien: een <b>spiraal</b>, de oorzaak van hartritmestoornissen. Epidemieën en bosbranden zijn óók exciteerbare media.</p>
      <p class="muted" style="font-size:.88rem">Rechts loopt een golf door een stukje weefsel. Een tijdelijke blokkade (grijs) breekt de golf, en het losse uiteinde krult om zichzelf heen.</p>
    </div>
    <div class="card"><canvas id="hero" class="sq" style="max-width:360px"></canvas>
      <div class="muted" id="hero-lbl" style="font-size:.85rem;text-align:center;margin-top:4px;min-height:2.6em"></div></div>
  </div>

  <div class="cards">
    <div class="card ov"><span class="num">18.1</span><h4>Actiepotentialen, hoe zat dat ook alweer?</h4>
      <p>Rustpotentiaal (binnen negatief), prikkel boven de drempel, natriumkanalen open (positieve feedback: <b>depolarisatie</b>), kaliumkanalen open (<b>repolarisatie</b>), doorschieter omlaag (<b>hyperpolarisatie</b>) en herstel.</p></div>
    <div class="card ov"><span class="num">18.2</span><h4>Hodgkin-Huxley model</h4>
      <p>Vier ODEs (V, m, n, h) met precies gemeten parameters. Zonder prikkel een rustpotentiaal, kleine prikkels herstellen, een grote prikkel geeft een piek: <i>all-or-nothing</i>. Tijdens de <b>refractaire periode</b> ligt de drempel verder weg.</p></div>
    <div class="card ov"><span class="num">18.3</span><h4>Een simpeler ODE-model</h4>
      <p>Het <b>FitzHugh-Nagumo model</b>: twee vergelijkingen, dus een faseruimte met twee nullclines en één stabiel evenwicht (de rust). Een actiepotentiaal is een grote omweg door die faseruimte; de refractaire periode komt doordat de rem W maar langzaam afneemt (ε ≪ 1).</p>
      <button class="b" data-go="cel">Geef een prikkel →</button></div>
    <div class="card ov"><span class="num">18.4</span><h4>Actiepotentialen als PDE</h4>
      <p>Dezelfde ODEs in hokjes, met diffusie van V: een golf (rood) loopt door het weefsel met een band refractair weefsel (blauw) erachter. Twee golven die botsen doven uit; een prikkel vlak achter een golf dooft meteen.</p>
      <button class="b" data-go="golven" data-preset="vrij">Laat een golf lopen →</button></div>
    <div class="card ov"><span class="num">18.5</span><h4>Ophopingen, blokkades en spiralen</h4>
      <p>Littekenweefsel geleidt niet (W = 1). Een golf krult om een vaste blokkade heen en trekt zichzelf weer recht. Maar een <b>tijdelijke</b> blokkade breekt de golf, die om zichzelf heen krult: een <b>spiraal</b>, oftewel een hartritmestoornis. Met virtueel experimenteren bedenk je ingrepen die anders "ondenkbaar" zijn.</p>
      <button class="b" data-go="golven" data-preset="tijdelijk">Maak een spiraal →</button></div>
    <div class="card ov"><span class="num">18.6</span><h4>Exciteerbare media en andere golven</h4>
      <p>Kenmerken: een drempelwaarde, een <i>all-or-nothing</i> respons door positieve feedback, propagatie door de ruimte, en een refractaire periode die de golven een richting geeft. Voorbeelden: epidemieën, bosbranden, calciumgolven op een eicel.</p></div>
    <div class="card ov"><span class="num">Opg.</span><h4>Clear!, epidemie, bosbrand</h4>
      <p>Een spiraal op het hart stoppen (alles tegelijk prikkelen werkt beter dan lokaal ingrijpen), een ziektegolf die een gevaccineerd dorp binnenrolt, een bosbrand die een drempel in de bomendichtheid heeft, en de drempelwaarde van het model bij a = −0,2.</p></div>
    <div class="card ov"><span class="num">✓</span><h4>Test jezelf</h4>
      <p>Negen korte vragen over drempels, de faseruimte, refractaire periodes, golven en spiralen. Bij een fout antwoord mag je het opnieuw proberen.</p>
      <button class="b primary" data-go="quiz">Start de quiz →</button></div>
  </div>

  <details>
    <summary>Terminologie</summary>
    <table class="cmp">
      <tr><th>Nederlands</th><th>Engels</th><th>Betekenis</th></tr>
      <tr><td>Exciteerbare media</td><td>Excitable media</td><td>Systemen waarin een prikkel een lokale reactie veroorzaakt die zich kan verspreiden. Door de refractaire periode lopen de golven in één richting.</td></tr>
      <tr><td>Virtueel experimenteren</td><td>Virtual experimentation</td><td>Experimenten uitvoeren met computersimulaties in plaats van in echte (biologische) systemen.</td></tr>
      <tr><td>Actiepotentiaal</td><td>Action potential</td><td>Kortdurende verandering van de lading over het membraan: depolarisatie, repolarisatie, hyperpolarisatie.</td></tr>
      <tr><td>Refractaire periode</td><td>Refractory period</td><td>Periode na een actiepotentiaal waarin de cel (vrijwel) niet opnieuw te prikkelen is.</td></tr>
      <tr><td>Spiraalgolf</td><td>Spiral wave</td><td>Een golf die om zichzelf heen krult en zichzelf in stand houdt; op het hart een hartritmestoornis.</td></tr>
    </table>
  </details>
</section>

<section class="panel" id="tab-cel" hidden>
  <h2>Eén cel: het FitzHugh-Nagumo model</h2>
  <p>Het model uit 18.3, met b = 1. Standaard staan a = 0,05 en ε = 0,015 ingesteld, dezelfde waarden als in de weefselsimulatie hiernaast; in het hoofdstuk zelf wordt met a = 0,2 en ε = 0,003 gerekend, schuif gerust. De cel zit in rust in het evenwicht V = 0, W = 0. <b>Geef een prikkel</b> zet V in één keer een stukje omhoog: een stap naar rechts in de faseruimte. Is die stap groot genoeg om voorbij het stijgende deel van de V-nullcline te komen, dan volgt een grote omweg: een actiepotentiaal. Geef daarna snel een tweede prikkel, of klik ergens in de faseruimte, en kijk wat er gebeurt.</p>
  <div class="eqs"><span class="eqn">dV/dt = −V(V − a)(V − 1) − W</span><span class="eqn">dW/dt = ε(V − bW)</span></div>
  <div class="btns">
    <button class="b primary" id="ce-stim">⚡ Geef een prikkel</button>
    <button class="b" id="ce-stim2">⚡⚡ Twee prikkels, <span id="ce-gap-l">40</span> tijdstappen na elkaar</button>
    <button class="b" id="ce-reset">↺ Terug naar rust</button>
    <button class="b" id="ce-play">⏸ Pauze</button>
  </div>
  <div class="controls">
    <div class="ctl"><label><span class="lbl">prikkelsterkte ΔV</span> <span id="ce-dv-v"></span></label><input type="range" id="ce-dv" min="0.05" max="0.6" step="0.01" value="0.3"></div>
    <div class="ctl"><label><span class="lbl">drempelparameter a</span> <span id="ce-a-v"></span></label><input type="range" id="ce-a" min="-0.3" max="0.5" step="0.01" value="0.05"></div>
    <div class="ctl"><label><span class="lbl">snelheid van de rem ε</span> <span id="ce-e-v"></span></label><input type="range" id="ce-e" min="0.001" max="0.02" step="0.0005" value="0.015"></div>
    <div class="ctl"><label><span class="lbl">tijd tussen de twee prikkels</span> <span id="ce-gap-v"></span></label><input type="range" id="ce-gap" min="10" max="300" step="5" value="40"></div>
  </div>
  <div class="grid2">
    <div class="card"><div class="ptitle">Voltage V en rem W tegen de tijd</div><canvas id="ce-ts" class="plot"></canvas>
      <div class="legend"><span><i class="dot" style="background:var(--accent)"></i>V</span><span><i class="dot" style="background:var(--accent2)"></i>W</span><span>stippellijn: drempel V = a (bij W = 0)</span></div></div>
    <div class="card"><div class="ptitle">Faseruimte (klik om de cel ergens neer te zetten)</div><canvas id="ce-pp" class="plot" style="cursor:crosshair"></canvas>
      <div class="legend"><span><i class="dot" style="background:var(--accent)"></i>V-nullcline (dV/dt = 0)</span><span><i class="dot" style="background:var(--accent2)"></i>W-nullcline (dW/dt = 0)</span></div></div>
  </div>
  <div class="callout" id="ce-text"></div>
</section>

<section class="panel" id="tab-golven" hidden>
  <h2>Golven en spiralen in een stukje weefsel</h2>
  <p>Hetzelfde model, maar nu in hokjes: elk hokje is een cel met een eigen V en W, en V diffundeert naar de buurhokjes (een PDE, zoals in 18.4). Een geprikkelde cel prikkelt zo haar buren, en een actiepotentiaal loopt als <b>golf</b> door het weefsel. De parameters zijn die van de simulaties in het hoofdstuk (a = 0,05, b = 3, ε = 0,005), op een weefsel van 192 bij 80 hokjes. <b>Klik of sleep</b> op het weefsel om het te prikkelen.</p>
  <div class="btns"><span class="muted" style="font-size:.88rem">Scenario:</span><div class="seg" id="wv-presets"><button data-p="vrij" aria-pressed="true">Vrije golf (18.4)</button><button data-p="blok">Blokkade (18.5)</button><button data-p="tijdelijk">Tijdelijke blokkade: spiraal (18.5)</button><button data-p="doolhof">Doolhof</button></div></div>
  <div class="card"><canvas id="wv-grid" class="wide"></canvas>
    <div class="legend"><span><i class="dot" style="background:var(--accent)"></i>V hoog: actiepotentiaal</span><span><i class="dot" style="background:var(--accent2)"></i>W hoog: refractair</span><span><i class="dot" style="background:var(--muted)"></i>blokkade (W = 1)</span><span class="stat nomath" id="wv-stat"></span></div></div>
  <div class="btns">
    <button class="b" id="wv-play">⏸ Pauze</button>
    <button class="b primary" id="wv-wave">⚡ Nieuwe golf van links</button>
    <button class="b" id="wv-clear">Clear! (alles tegelijk prikkelen)</button>
    <button class="b" id="wv-reset">↺ Opnieuw</button>
  </div>
  <div class="btns">
    <label class="chk"><input type="checkbox" id="wv-auto" checked> elke 600 stappen vanzelf een nieuwe golf van links</label>
    <span class="muted" style="font-size:.88rem">Muis:</span><div class="seg" id="wv-brush"><button data-b="stim" aria-pressed="true">prikkelen (V = 0,3)</button><button data-b="erase">wissen (V = 0)</button></div>
  </div>
  <div class="callout" id="wv-text"></div>
</section>

<section class="panel" id="tab-quiz" hidden>
  <h2>Quiz: exciteerbare media</h2>
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
   WEEFSEL: FitzHugh-Nagumo in hokjes (PDE), zoals de simulaties in 18.4 en 18.5
   V diffundeert naar de vier buren; W niet. Parameters en tijdstap als in het hoofdstuk.
   ========================================================= */
const TP={a:0.05,b:3,e:0.005,dt:3,D:0.2};
const rgb=n=>(col(n).match(/[\d.]+/g)||[0,0,0]).slice(0,3).map(Number);
function makeTissue(Wd,H){
  const n=Wd*H, V=new Float32Array(n), W=new Float32Array(n), nV=new Float32Array(n), block=new Uint8Array(n);
  const off=document.createElement('canvas'); off.width=Wd; off.height=H; const octx=off.getContext('2d'); const img=octx.createImageData(Wd,H);
  const T={Wd,H,V,W,block,step:0};
  T.reset=()=>{ V.fill(0); W.fill(0); block.fill(0); T.step=0; };
  T.leftWave=()=>{ for(let y=0;y<H;y++) V[y*Wd]=1; };
  T.all=v=>{ V.fill(v); };
  T.disc=(cx,cy,r,v)=>{ for(let y=Math.max(0,cy-r);y<=Math.min(H-1,cy+r);y++) for(let x=Math.max(0,cx-r);x<=Math.min(Wd-1,cx+r);x++) if((x-cx)*(x-cx)+(y-cy)*(y-cy)<=r*r) V[y*Wd+x]=v; };
  T.rect=(x0,x1,y0,y1,on)=>{ for(let y=y0;y<=Math.min(H-1,y1);y++) for(let x=x0;x<=Math.min(Wd-1,x1);x++) block[y*Wd+x]=on?1:0; };
  T.excited=()=>{ let k=0; for(let i=0;i<n;i++) if(V[i]>0.5) k++; return k; };
  /* doolhof: gangen van 6 hokjes breed tussen muren van littekenweefsel (vaste willekeur, dus steeds hetzelfde doolhof) */
  T.maze=()=>{ const P=8, mx=Math.floor(Wd/P), my=Math.floor(H/P); let seed=12345; const rnd=()=>{ seed=(seed*1103515245+12345)%2147483648; return seed/2147483648; };
    block.fill(1); const seen=new Uint8Array(mx*my), st=[[0,Math.floor(my/2)]]; seen[st[0][1]*mx]=1;
    const carve=(x0,x1,y0,y1)=>{ for(let y=y0;y<=y1;y++) for(let x=x0;x<=x1;x++) if(x>=0&&y>=0&&x<Wd&&y<H) block[y*Wd+x]=0; };
    const cell=(i,j)=>carve(i*P+1,i*P+P-2,j*P+1,j*P+P-2);
    cell(0,Math.floor(my/2)); carve(0,0,Math.floor(my/2)*P+1,Math.floor(my/2)*P+P-2);   // ingang aan de linkerkant
    carve(1,P-2,0,0);                                                                   // uitgang linksboven (in de bovenrand)
    while(st.length){ const [i,j]=st[st.length-1]; const nb=[[i+1,j],[i-1,j],[i,j+1],[i,j-1]].filter(([a,b])=>a>=0&&b>=0&&a<mx&&b<my&&!seen[b*mx+a]);
      if(!nb.length){ st.pop(); continue; } const [a,b]=nb[Math.floor(rnd()*nb.length)]; seen[b*mx+a]=1; cell(a,b);
      carve(Math.min(i,a)*P+1,Math.max(i,a)*P+P-2,Math.min(j,b)*P+1,Math.max(j,b)*P+P-2); st.push([a,b]); } };
  T.tick=()=>{
    const {a,b,e,dt,D}=TP;
    for(let i=0;i<n;i++){ if(block[i]){ W[i]=1; V[i]=0; }   // blokkade: altijd refractair (W = 1)
      const v=V[i], w=W[i]; let nv=v+dt*(-v*(v-a)*(v-1)-w); W[i]=w+dt*e*(v-b*w); if(nv<0.001) nv=0; nV[i]=nv; }
    for(let y=0;y<H;y++){ const up=y>0?-Wd:0, dn=y<H-1?Wd:0;
      for(let x=0;x<Wd;x++){ const i=y*Wd+x, v=nV[i]; if(block[i]) continue;
        const l=x>0&&!block[i-1]?nV[i-1]:v, r=x<Wd-1&&!block[i+1]?nV[i+1]:v, u=up&&!block[i+up]?nV[i+up]:v, d=dn&&!block[i+dn]?nV[i+dn]:v;
        V[i]=v+D*(l+r+u+d-4*v); } }
    T.step++;
  };
  T.draw=c=>{
    const bg=rgb('bg'), ac=rgb('accent'), bl=rgb('accent2'), fg=rgb('fg'), d=img.data;
    for(let i=0;i<n;i++){ const o=i*4;
      if(block[i]){ for(let k=0;k<3;k++) d[o+k]=bg[k]+0.35*(fg[k]-bg[k]); }
      else { const f=Math.min(1,Math.max(0,V[i])), g=Math.min(1,Math.max(0,W[i]/0.3))*(1-f);
        for(let k=0;k<3;k++) d[o+k]=bg[k]+f*(ac[k]-bg[k])+g*(bl[k]-bg[k]); }
      d[o+3]=255; }
    octx.putImageData(img,0,0);
    const {ctx,w,h}=fitCanvas(c); ctx.imageSmoothingEnabled=false; ctx.clearRect(0,0,w,h); ctx.drawImage(off,0,0,w,h);
  };
  return T;
}

/* =========================================================
   IN ÉÉN OOGOPSLAG: een golf, een tijdelijke blokkade, een spiraal
   ========================================================= */
const hero=(()=>{
  const c=$('#hero'), lbl=$('#hero-lbl'), T=makeTissue(48,48);
  let acc=0;
  function start(){ T.reset(); T.leftWave(); T.rect(24,27,11,47,true); }
  function frameFn(dt){
    acc+=dt*120; let k=0;
    while(acc>=1&&k<6){ acc--; k++; if(T.step===70) T.rect(24,27,11,47,false); T.tick(); if(T.step>=2600) start(); }
    T.draw(c);
    lbl.innerHTML=T.step<70?'Een golf loopt van links naar rechts. Het grijze stuk is een <b>blokkade</b>: altijd refractair (W = 1).'
      :T.step<330?'De blokkade verdwijnt. De gebroken golf heeft een los uiteinde, dat om zichzelf heen krult…'
      :'Een <b>spiraal</b>: het weefsel blijft zichzelf prikkelen, zonder prikkel van buiten. Op het hart is dat een hartritmestoornis (18.5).';
  }
  start();
  return {frame:frameFn};
})();

/* =========================================================
   EÉN CEL: FitzHugh-Nagumo ODE met prikkels, tijdreeks en faseruimte
   ========================================================= */
const cel=(()=>{
  const ts=$('#ce-ts'), pp=$('#ce-pp'), txt=$('#ce-text'), sDV=$('#ce-dv'), sA=$('#ce-a'), sE=$('#ce-e'), sGap=$('#ce-gap'), vGap=$('#ce-gap-v'), lGap=$('#ce-gap-l'),
        vDV=$('#ce-dv-v'), vA=$('#ce-a-v'), vE=$('#ce-e-v'), bStim=$('#ce-stim'), bStim2=$('#ce-stim2'), bReset=$('#ce-reset'), bPlay=$('#ce-play');
  const DT=0.1, WIN=800, SPEED=120, B=1;
  let V=0, W=0, t=0, a=0.05, e=0.015, dV=0.3, gap=40, playing=true, acc=0, trace=[], stims=[], pending=null, watch=null, verdict='', lastStim=null;
  const f=(V,W)=>-V*(V-a)*(V-1)-W, g=(V,W)=>e*(V-B*W);
  function reset(){ V=0; W=0; t=0; trace=[]; stims=[]; pending=null; watch=null; verdict=''; lastStim=null; acc=0; }
  function stim(){ lastStim={V0:V,W,t}; V+=dV; stims.push(t); watch={t0:t,fired:false}; verdict=''; }
  function readSliders(){ a=+sA.value; e=+sE.value; dV=+sDV.value; gap=+sGap.value; vA.textContent=a.toFixed(2); vE.textContent=e.toFixed(4); vDV.textContent=dV.toFixed(2); vGap.textContent=gap; lGap.textContent=gap; }
  function step(){
    if(pending!=null&&t>=pending){ pending=null; stim(); }
    const k1V=f(V,W), k1W=g(V,W); V+=DT*k1V; W+=DT*k1W; t+=DT;
    if(V>1.6) V=1.6; if(V<-1) V=-1;
    if(trace.length===0||t-trace[trace.length-1][0]>=0.5) trace.push([t,V,W]);
    while(trace.length&&trace[0][0]<t-WIN) trace.shift();
    if(watch){ if(V>0.5) watch.fired=true;
      if(t-watch.t0>80){ verdict=watch.fired?'<b>Actiepotentiaal!</b> De prikkel kwam voorbij het stijgende deel van de V-nullcline: V schoot omhoog en maakt nu de grote omweg.'
        :'<b>Geen actiepotentiaal.</b> De prikkel was te zwak, of de cel was nog refractair (W nog te hoog): V zakte meteen terug.'; watch=null; } }
  }
  function setPlay(p){ playing=p; bPlay.textContent=p?'⏸ Pauze':'▶ Verder'; }
  function drawTS(){
    const x0=Math.max(0,t-WIN), fr=frame(ts,{xmin:x0,xmax:x0+WIN,ymin:-0.6,ymax:1.1,xlab:'tijd',ylab:'V, W',pad:{l:44,b:34},nx:5});
    poly(fr,[[x0,a],[x0+WIN,a]],col('muted'),1,[3,4]);
    for(const s of stims) if(s>=x0) poly(fr,[[s,-0.6],[s,1.1]],col('axis'),1,[2,3]);
    poly(fr,trace.map(p=>[p[0],p[2]]),col('accent2'),1.6,[5,3]);
    poly(fr,trace.map(p=>[p[0],p[1]]),col('accent'),2.2);
    if(trace.length) disc(fr.ctx,fr.X(t),fr.Y(V),4,col('accent'),col('bg'),1.2);
    fr.ctx.restore();
  }
  function drawPP(){
    const fr=frame(pp,{xmin:-0.6,xmax:1.1,ymin:-0.1,ymax:0.3,xlab:'V',ylab:'W',pad:{l:48,b:34},nx:5,ny:4});
    const nc=[]; for(let v=-0.6;v<=1.1001;v+=0.01) nc.push([v,-v*(v-a)*(v-1)]);
    poly(fr,nc,col('accent'),2);
    poly(fr,[[-0.6,-0.6/B],[1.1,1.1/B]],col('accent2'),2);
    disc(fr.ctx,fr.X(0),fr.Y(0),4.5,a>=0?col('fg'):col('bg'),col('fg'),1.6);
    poly(fr,trace.map(p=>[p[1],p[2]]),col('fg'),1.4,null,0.75);
    if(lastStim&&t-lastStim.t<60){ const c=fr.ctx, x0=fr.X(lastStim.V0), x1=fr.X(lastStim.V0+dV), y=fr.Y(lastStim.W);
      c.save(); c.strokeStyle=col('warn'); c.fillStyle=col('warn'); c.lineWidth=2; c.beginPath(); c.moveTo(x0,y); c.lineTo(x1,y); c.stroke();
      c.beginPath(); c.moveTo(x1,y); c.lineTo(x1-7,y-4); c.lineTo(x1-7,y+4); c.closePath(); c.fill(); c.restore(); }
    disc(fr.ctx,fr.X(V),fr.Y(W),5,col('accent'),col('bg'),1.5);
    fr.ctx.restore();
    return fr;
  }
  function status(){
    let s;
    if(a<0) s=`a = ${a.toFixed(2)} &lt; 0: het evenwicht is <b>instabiel</b>. De cel vuurt vanzelf, periodiek, zonder prikkel: een pacemakercel (zoals in de sinusknoop). In de faseruimte is de baan een gesloten lus (limietcyclus).`;
    else if(V>0.5) s='<b>Actiepotentiaal:</b> V is hoog en loopt langs de rechter tak van de V-nullcline, terwijl de rem W langzaam oploopt. Zodra W boven de top van de nullcline komt, valt V terug.';
    else if(V>0.03&&f(V,W)>0.001) s='<b>Depolarisatie:</b> de prikkel kwam voorbij het stijgende deel van de V-nullcline. Door de positieve feedback neemt V nu steeds sneller toe.';
    else if(V<-0.02&&W>0.005) s='<b>Refractaire periode:</b> V is alweer negatief (hyperpolarisatie), maar W is nog hoog en neemt maar langzaam af. Een stap naar rechts komt nu niet voorbij het stijgende deel van de V-nullcline: een nieuwe prikkel doet niets.';
    else if(Math.abs(V)<0.02&&Math.abs(W)<0.005) s='<b>Rust:</b> de cel zit in het stabiele evenwicht waar de nullclines snijden (V = 0, W = 0). Een prikkel kleiner dan a = '+a.toFixed(2)+' dooft uit; een grotere start een actiepotentiaal.';
    else s='De cel is onderweg terug naar het evenwicht.';
    return s+(verdict?' '+verdict:'')+(e>0.0151?' Met deze grote ε komt de rem bijna net zo snel op gang als V: de piek wordt lager en korter, en de refractaire periode kort.':'');
  }
  bStim.onclick=()=>{ stim(); setPlay(true); };
  bStim2.onclick=()=>{ stim(); pending=t+gap; setPlay(true); };
  bReset.onclick=()=>{ reset(); setPlay(true); };
  bPlay.onclick=()=>setPlay(!playing);
  [sDV,sA,sE,sGap].forEach(s=>s.oninput=readSliders);
  let ppFrame=null;
  pp.addEventListener('click',ev=>{ if(!ppFrame) return; const r=pp.getBoundingClientRect(); const [v,w]=ppFrame.inv(ev.clientX-r.left,ev.clientY-r.top);
    if(!ppFrame.inside(ev.clientX-r.left,ev.clientY-r.top)) return; lastStim={V0:V,W,t:-1e9}; V=Math.max(-0.6,Math.min(1.1,v)); W=Math.max(-0.1,Math.min(0.3,w)); watch={t0:t,fired:false}; verdict=''; setPlay(true); });
  readSliders(); reset();
  return {frame:dt=>{ if(playing){ acc+=dt*SPEED/DT; let k=0; while(acc>=1&&k<60){ acc--; k++; step(); } }
    drawTS(); ppFrame=drawPP(); txt.innerHTML=status(); }};
})();

/* =========================================================
   GOLVEN EN SPIRALEN: weefsel met blokkades
   ========================================================= */
const golven=(()=>{
  const c=$('#wv-grid'), txt=$('#wv-text'), stat=$('#wv-stat'), bPlay=$('#wv-play'), bWave=$('#wv-wave'), bClear=$('#wv-clear'), bReset=$('#wv-reset'), auto=$('#wv-auto'),
        segP=$('#wv-presets'), segB=$('#wv-brush');
  const Wd=192, H=80, T=makeTissue(Wd,H);
  let preset='vrij', brush='stim', playing=true, acc=0;
  const BLK=[62,69,22,H-1];   // dezelfde plek als in het hoofdstuk (daar kolommen 31-34 vanaf rij 11, hier twee keer zo fijn)
  const TEXT={
    vrij:'<b>18.4.</b> Links start een golf (V = 1 in de linkerkolom) die naar rechts loopt. Daarachter volgt een bredere blauwe band van W: refractair weefsel, waardoor de golf niet terug kan. <b>Klik vóór de golf:</b> een nieuwe golf loopt twee kanten op en dooft samen met de oude uit (annihilatie). <b>Klik vlak achter de golf:</b> de prikkel dooft meteen uit.',
    blok:'<b>18.5.</b> Littekenweefsel (grijs) geleidt niet: W staat er altijd op 1. De golf krult om de blokkade heen en trekt zichzelf daarna weer recht, omdat weefsel vooraan in de golf minder activatie krijgt van de buren. Voor een hart is dit litteken nog geen ramp.',
    tijdelijk:'<b>18.5.</b> Dezelfde blokkade, maar na 160 stappen verdwijnt hij. De gebroken golf heeft een los uiteinde dat om zichzelf heen krult: een <b>spiraal</b> die zichzelf in stand houdt. Op het hart: een hartritmestoornis. Probeer de spiraal weg te wissen met de muis (V = 0): vaak maak je er alleen meer bij. <b>Clear!</b> prikkelt alles tegelijk; daarna is alles refractair en is de spiraal weg (zie de opgave Clear!).',
    doolhof:'<b>Doolhof.</b> Exciteerbaar weefsel in een doolhof van littekenweefsel. De golf vindt vanzelf élke gang: hij loopt alleen weefsel in dat nog niet refractair is, dus hij komt nooit terug en dooft in doodlopende gangen uit. Zo "lost" een exciteerbaar medium een doolhof op: de golf die het eerst ergens aankomt, heeft de kortste route genomen. De ingang zit halverwege links, de uitgang linksboven; elke 600 stappen start bij de ingang een nieuwe golf. Klik in een gang om zelf een golf te starten.'};
  function start(p){ if(p) preset=p; T.reset(); T.leftWave(); acc=0;
    if(preset==='doolhof') T.maze(); else if(preset!=='vrij') T.rect(...BLK,true);
    segP.querySelectorAll('button').forEach(b=>b.setAttribute('aria-pressed',b.dataset.p===preset));
    txt.innerHTML=TEXT[preset]; setPlay(true); }
  function setPlay(p){ playing=p; bPlay.textContent=p?'⏸ Pauze':'▶ Verder'; }
  function tick(){ if(preset==='tijdelijk'&&T.step===160) T.rect(...BLK,false);
    if(auto.checked&&T.step>0&&T.step%600===0) T.leftWave(); T.tick(); }
  segP.querySelectorAll('button').forEach(b=>b.onclick=()=>start(b.dataset.p));
  segB.querySelectorAll('button').forEach(b=>b.onclick=()=>{ brush=b.dataset.b; segB.querySelectorAll('button').forEach(x=>x.setAttribute('aria-pressed',x===b)); });
  bPlay.onclick=()=>setPlay(!playing); bWave.onclick=()=>T.leftWave(); bClear.onclick=()=>T.all(1); bReset.onclick=()=>start();
  function paint(ev){ const r=c.getBoundingClientRect(); const x=Math.floor((ev.clientX-r.left)/r.width*Wd), y=Math.floor((ev.clientY-r.top)/r.height*H);
    if(x<0||y<0||x>=Wd||y>=H) return; if(brush==='stim') T.disc(x,y,5,0.3); else T.disc(x,y,7,0); }
  c.addEventListener('pointerdown',ev=>{ ev.preventDefault(); c.setPointerCapture(ev.pointerId); paint(ev); });
  c.addEventListener('pointermove',ev=>{ if(ev.buttons&1) paint(ev); });
  start('vrij');
  return {frame:dt=>{ if(playing){ acc+=dt*180; let k=0; while(acc>=1&&k<9){ acc--; k++; tick(); } }
      T.draw(c); stat.textContent='stap '+T.step+' · '+T.excited()+' hokjes geprikkeld'; },
    preset:p=>start(p)};
})();

/* =========================================================
   QUIZ
   ========================================================= */
/* faseruimte met nullclines (a = 0,2, b = 1) en twee punten */
function ppSVG(){
  const W=300,H=160,l=40,r=14,t=12,b=28, a=0.2, xmin=-0.35,xmax=1.05,ymin=-0.07,ymax=0.17;
  const X=v=>l+(v-xmin)/(xmax-xmin)*(W-l-r), Y=w=>t+(ymax-w)/(ymax-ymin)*(H-t-b);
  let d=''; for(let i=0;i<=140;i++){ const v=xmin+(xmax-xmin)*i/140, w=-v*(v-a)*(v-1); if(w<ymin-0.02||w>ymax+0.02){ d+=' '; continue; } d+=(d.endsWith(' ')||!d?'M':'L')+X(v).toFixed(1)+' '+Y(w).toFixed(1); }
  const lab=(v,w,s,dx,dy)=>`<circle cx="${X(v)}" cy="${Y(w)}" r="4" style="fill:var(--fg)"/><text x="${X(v)+dx}" y="${Y(w)+dy}" style="font-size:12px;font-weight:600;fill:var(--fg)">${s}</text>`;
  return `<svg viewBox="0 0 ${W} ${H}" width="${W}" style="max-width:100%;display:block;margin:8px 0;color:var(--muted)" role="img" aria-label="faseruimte met nullclines">
    <line x1="${l}" y1="${t}" x2="${l}" y2="${H-b}" style="stroke:currentColor"/><line x1="${l}" y1="${Y(0)}" x2="${W-r}" y2="${Y(0)}" style="stroke:currentColor"/>
    <path d="${d}" style="fill:none;stroke:var(--accent);stroke-width:2.4"/>
    <line x1="${X(-0.07)}" y1="${Y(-0.07)}" x2="${X(0.17)}" y2="${Y(0.17)}" style="stroke:var(--accent2);stroke-width:2.4"/>
    ${lab(0,0,'rust',-38,15)}${lab(0.12,0,'A',-4,-8)}${lab(0.4,0,'B',-4,-8)}
    <text x="${W-r}" y="${Y(0)-6}" text-anchor="end" font-style="italic" style="font-size:12px;fill:currentColor">V</text>
    <text x="${l-6}" y="${t+10}" text-anchor="end" font-style="italic" style="font-size:12px;fill:currentColor">W</text></svg>`;
}
/* tijdreeks: twee even sterke prikkels, de tweede tijdens de refractaire periode */
function tsSVG(){
  const W=300,H=150,l=36,r=12,t=12,b=26, a=0.2,e=0.003,DT=0.1,TM=500, ymin=-0.5,ymax=1.1;
  const X=x=>l+x/TM*(W-l-r), Y=y=>t+(ymax-y)/(ymax-ymin)*(H-t-b);
  let V=0,Wv=0,d=''; for(let k=0;k*DT<=TM;k++){ const tt=k*DT; if(Math.abs(tt-30)<DT/2||Math.abs(tt-150)<DT/2) V+=0.3;
    V+=DT*(-V*(V-a)*(V-1)-Wv); Wv+=DT*e*(V-Wv); if(k%5===0) d+=(k?'L':'M')+X(tt).toFixed(1)+' '+Y(V).toFixed(1); }
  const arrow=(x,s)=>`<line x1="${X(x)}" y1="${Y(-0.45)}" x2="${X(x)}" y2="${Y(-0.08)}" style="stroke:var(--fg);stroke-width:1.6"/><polygon points="${X(x)},${Y(-0.04)} ${X(x)-4},${Y(-0.12)} ${X(x)+4},${Y(-0.12)}" style="fill:var(--fg)"/><text x="${X(x)+6}" y="${Y(-0.4)}" style="font-size:12px;font-weight:600;fill:var(--fg)">${s}</text>`;
  return `<svg viewBox="0 0 ${W} ${H}" width="${W}" style="max-width:100%;display:block;margin:8px 0;color:var(--muted)" role="img" aria-label="voltage tegen de tijd met twee prikkels">
    <line x1="${l}" y1="${t}" x2="${l}" y2="${H-b}" style="stroke:currentColor"/><line x1="${l}" y1="${Y(0)}" x2="${W-r}" y2="${Y(0)}" style="stroke:currentColor"/>
    <path d="${d}" style="fill:none;stroke:var(--accent);stroke-width:2.4"/>${arrow(30,'1')}${arrow(150,'2')}
    <text x="${l+(W-l-r)/2}" y="${H-5}" text-anchor="middle" style="font-size:11px;fill:currentColor">tijd</text>
    <text x="${l+6}" y="${t+10}" font-style="italic" style="font-size:12px;fill:currentColor">V</text></svg>`;
}
const quiz=(()=>{
  const G1=ppSVG(), G2=tsSVG();
  const BASE=[
    {q:'Wat is een <b>actiepotentiaal</b>, bekeken in de faseruimte van het FitzHugh-Nagumo model?',
     o:['Een grote omweg door de faseruimte, terug naar het ene evenwicht','Een sprong naar een tweede stabiel evenwicht, met een hoog voltage','Een limietcyclus die blijft rondgaan, ook zonder nieuwe prikkel','Een tijdelijke verschuiving van de nullclines door de prikkel'], a:0,
     h:'Waar komt de cel na een actiepotentiaal uiteindelijk weer terecht?',
     e:'Na een voldoende grote prikkel maakt het systeem een grote omweg (V omhoog, W langzaam omhoog, V omlaag, W langzaam omlaag) en keert het terug naar het enige, stabiele evenwicht (de rustpotentiaal): het snijpunt van de nullclines.'},
    {q:'Hieronder staat de faseruimte met de V-nullcline (oranje) en de W-nullcline (blauw). Een prikkel zet de cel vanuit rust op punt <b>B</b>. Wat gebeurt er?'+G1,
     o:['V schiet omhoog: B ligt voorbij het stijgende deel van de V-nullcline','V zakt meteen terug naar rust, net als vanuit A: de prikkel is te klein','De cel blijft in B staan, want op de W-nullcline is dW/dt gelijk aan nul','V daalt eerst tot onder nul (hyperpolarisatie) en stijgt daarna pas'], a:0,
     h:'Welke kant wijst het vectorveld op vlak rechts van het stijgende deel van de oranje curve?',
     e:'Tussen het evenwicht en het stijgende deel van de V-nullcline (bij V = a) wijst het vectorveld terug naar links: vanuit A keert de cel terug naar rust. Voorbij dat stijgende deel wijst het naar rechts: vanuit B schiet V omhoog en volgt een actiepotentiaal.'},
    {q:'Waardoor is er in het FitzHugh-Nagumo model een <b>refractaire periode</b>?',
     o:['Na de piek is W nog hoog en neemt maar langzaam af','De parameter a wordt na elke piek tijdelijk groter','Het evenwicht verschuift tijdelijk naar een hoger voltage','V kan pas weer stijgen als W weer precies nul is'], a:0,
     h:'Welke variabele is de "rem", en hoe snel verandert die?',
     e:'Na de piek zit het systeem linksboven in de faseruimte: V negatief, W hoog. Omdat ε ≪ 1 neemt W maar langzaam af. Zolang W hoog is, ligt de drempel verder weg: een stapje naar rechts komt niet voorbij het stijgende deel van de V-nullcline.'},
    {q:'Twee even sterke prikkels (pijl 1 en pijl 2). De tweede geeft geen actiepotentiaal. Waarom niet?'+G2,
     o:['De cel is nog refractair: de rem W is nog niet weggezakt','De tweede prikkel is zwakker dan de eerste en haalt de drempel niet','Na een actiepotentiaal is het evenwicht instabiel geworden','Het voltage kan niet twee keer achter elkaar positief worden'], a:0,
     h:'De prikkels zijn even sterk. Wat is er anders aan de toestand van de cel bij pijl 2?',
     e:'Bij pijl 2 is V al weer negatief (hyperpolarisatie), maar is de rem W nog hoog. Dezelfde stap in V is nu niet genoeg om voorbij de V-nullcline te komen: de prikkel dooft uit. Pas als W is weggezakt, kan een nieuwe actiepotentiaal starten.'},
    {q:'In het FitzHugh-Nagumo model (<span class="eqn">dW/dt = ε(V − bW)</span>) staat ε ≪ 1. Wat betekent dat?',
     o:['W verandert veel langzamer dan V','De prikkel moet heel klein zijn','Het voltage V blijft altijd klein','De W-nullcline ligt bijna horizontaal'], a:0,
     h:'ε staat vóór de hele vergelijking van dW/dt.',
     e:'ε schaalt de snelheid van W. Een kleine ε betekent een trage rem, die langzaam op gang komt en langzaam verdwijnt: daardoor duurt de piek even, en daardoor duurt het lang voordat de cel weer prikkelbaar is (de refractaire periode).'},
    {q:'Je zet de ODEs in hokjes en laat V <b>diffunderen</b> naar de buurhokjes (een PDE). Wat zie je dan?',
     o:['De actiepotentiaal loopt als een golf door het weefsel','Alle hokjes gaan tegelijk een actiepotentiaal maken','Het voltage vervaagt door de diffusie en er gebeurt niets meer','De refractaire periode verdwijnt, want W diffundeert niet mee'], a:0,
     h:'Wat doet diffusie van V met een buurcel die in rust is?',
     e:'Een geprikkelde cel prikkelt haar buren: diffusie brengt een deel van het hoge voltage naar de buren. Komen die daarmee voorbij hun drempel, dan maken zij ook een actiepotentiaal, en zo verder: een lopende golf, met een refractaire band erachter.'},
    {q:'Welke uitspraak over <b>blokkades</b> (littekenweefsel, W = 1) klopt?',
     o:['Een tijdelijke blokkade kan de golf breken en een spiraal opleveren','Elke blokkade, vast of tijdelijk, houdt de golf volledig tegen','Een vaste blokkade breekt de golf en geeft altijd een spiraal','Achter een vaste blokkade blijft een gat in de golf, dat niet meer sluit'], a:0,
     h:'Denk aan de drie simulaties in 18.5.',
     e:'Een vaste blokkade is niet rampzalig: de golf krult eromheen en herstelt zichzelf. Een tijdelijke blokkade breekt de golf; het losse uiteinde krult om zichzelf heen tot een spiraal die zichzelf in stand houdt: een hartritmestoornis.'},
    {q:'Welk systeem is <b>géén</b> exciteerbaar medium?',
     o:['Een populatie die logistisch groeit naar het draagvermogen','Een bosbrand: brand breekt uit, het bos ligt in as, het bos herstelt','Een epidemie waarbij immuniteit na een tijd weer verzwakt','Calciumgolven op het membraan van een bevruchte eicel'], a:0,
     h:'Een exciteerbaar medium heeft een drempel, een all-or-nothing respons die zich verspreidt, en een refractaire periode.',
     e:'Logistische groei heeft geen drempel en geen refractaire periode: niets verspreidt zich als golf. Bosbrand, epidemie en calciumgolven hebben alle drie een drempel, een respons die zich door de ruimte verspreidt en een periode waarin opnieuw "ontbranden" niet kan.'},
    {q:'Je zet in het FitzHugh-Nagumo model (<span class="eqn">dV/dt = −V(V − a)(V − 1) − W</span>) a = −0,2 in plaats van 0,2 (de verdiepende opgave). Wat verandert er in de faseruimte, en wat betekent dat?',
     o:['Het evenwicht wordt instabiel: de cel vuurt vanzelf periodiek','Er komen twee extra evenwichten bij: de cel wordt bistabiel','De cel kan geen actiepotentiaal meer maken, hoe sterk de prikkel ook is','De drempel komt hoger te liggen: er is een grotere prikkel nodig'], a:0,
     h:'Kijk naar de helling van de V-nullcline bij het evenwicht, en naar de simulatie met de slider a.',
     e:'Bij a &lt; 0 wijst het vectorveld vlak bij het snijpunt van de nullclines van het evenwicht af. De cel heeft dan geen rustpotentiaal meer, maar maakt een eindeloze rondgang: periodieke actiepotentialen zonder prikkel, zoals de pacemakercellen van de sinusknoop.'},
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
    const msg=frac===1?'Perfect! Drempels, nullclines, refractaire periodes en spiralen: je hebt het door.':frac>=.75?'Heel goed! Kijk nog even naar de vragen die je niet in één keer goed had.':frac>=.5?'Op de goede weg. Speel nog eens met de prikkels in de faseruimte en met de blokkades in het weefsel.':'Lees het hoofdstuk nog eens door en speel met de interactieve figuren. Daarna gaat het vast beter!';
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
const modules={overzicht:hero,cel:cel,golven:golven,quiz:quiz};
let current='overzicht';
const TABKEY='bioms-tab-exc';   // the last opened tab is remembered in this browser
function showTab(id){ if(!modules[id]) id='overzicht'; current=id;
  root.querySelectorAll('.tab').forEach(b=>b.setAttribute('aria-selected',b.dataset.tab===id));
  root.querySelectorAll('.panel').forEach(p=>p.hidden=p.id!=='tab-'+id);
  if(params.get('hidenav')!=='1') store.set(TABKEY,id);
  postHeight(); }
root.querySelectorAll('.tab').forEach(b=>b.onclick=()=>showTab(b.dataset.tab));
root.querySelectorAll('[data-go]').forEach(b=>b.onclick=()=>{ showTab(b.dataset.go); if(b.dataset.preset&&modules[b.dataset.go].preset) modules[b.dataset.go].preset(b.dataset.preset); if(host.getBoundingClientRect().top<0) host.scrollIntoView({behavior:'smooth'}); });
if(params.get('hidenav')==='1') $('nav.tabs').hidden=true;
showTab(params.get('tab')||store.get(TABKEY)||'overzicht');
let last=performance.now();
let visible=true; new IntersectionObserver(es=>{ visible=es[0].isIntersecting; },{rootMargin:'100px'}).observe(host);
function loop(now){ const dt=Math.min(0.05,(now-last)/1000); last=now; const m=modules[current]; if(visible&&m&&m.frame&&!document.hidden) m.frame(dt); requestAnimationFrame(loop); }
requestAnimationFrame(loop);
}
function init(){ document.querySelectorAll('[data-exc-app]').forEach(mount); }
window.ExcApp={mount,init};
const H0=inlineHost(CS); if(H0) mount(H0);
if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();
