/* Reactie-diffusie-explainer (Biologische Modellen en Statistiek, hoofdstuk 17).
 * Gebruik in Quarto (één regel, op de plek waar de app moet komen):
 *   <script src="Applets/reactiediffusie_explainer.js"></script>                          -> hele app met tabbladen
 *   <script src="Applets/reactiediffusie_explainer.js" data-tab="turing"></script>        -> alleen dat onderdeel
 *   <script src="Applets/reactiediffusie_explainer.js" data-tab="turing" data-nav="1"></script> -> tabbladen, start op dat tabblad
 * data-tab: overzicht | diffusie | turing | quiz
 * data-theme="dark|light" forceert een thema; anders volgt de app het boek (ook de donkere modus).
 * (Alternatief: <div data-rd-app data-tab="..."></div> plus het script één keer op de pagina.)
 */
(function(){
"use strict";
const CS=document.currentScript;
function inlineHost(cs){ if(!cs||!cs.dataset||cs.__appDone||!cs.parentNode) return null;
  const single=!!cs.dataset.tab;
  // a plain <script> with no data-tab becomes the full app with tabs, unless the page already uses <div data-rd-app>
  if(!single&&document.querySelector('[data-rd-app]')) return null;
  cs.__appDone=true;
  const d=document.createElement('div'); d.setAttribute('data-rd-app','');
  if(cs.dataset.tab) d.dataset.tab=cs.dataset.tab;
  if(single&&cs.dataset.nav!=='1') d.dataset.hidenav='1'; if(cs.dataset.theme) d.dataset.theme=cs.dataset.theme;
  cs.parentNode.insertBefore(d,cs); return d; }
if(window.RdApp){ const h=inlineHost(CS); if(h) window.RdApp.mount(h); window.RdApp.init(); return; }
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
`;
const EXTRA=`
/* reactie-diffusie: extra */
.plot-prof{aspect-ratio:16/5}
@media (max-width:680px){.plot-prof{aspect-ratio:16/8}}
.grid-small{max-width:300px}
canvas.click{cursor:pointer}
.stepnote{font-size:.9rem}
.stepnote b{font-weight:600}
.eqs{display:flex;flex-direction:column;gap:2px;margin:4px 0 8px;font-size:1.02em}
`;
const HTML=`
<nav class="tabs" role="tablist">
  <button class="tab" data-tab="overzicht">In één oogopslag</button>
  <button class="tab" data-tab="diffusie">Diffusie tussen hokjes</button>
  <button class="tab" data-tab="turing">Turing-patronen</button>
  <button class="tab" data-tab="quiz">Quiz ✓</button>
</nav>

<section class="panel" id="tab-overzicht">
  <h2>Hoofdstuk 17 in één oogopslag</h2>
  <div class="grid2">
    <div>
      <p>Een <b>reactievergelijking</b> zoals DNA → mRNA → eiwit vertaal je in ODEs door elk proces een term te geven: aanmaak met een constante snelheid, afbraak evenredig met de hoeveelheid (<span class="eqn">dM/dt = e − dM</span>). Zet je die cellen in <b>hokjes</b>, dan kun je ook <b>diffusie</b> modelleren: elk hokje wisselt een fractie van zijn inhoud uit met zijn buren. Dat vervaagt verschillen, zoals een foto die je blurt.</p>
      <p>Toch kan diffusie óók patronen <i>maken</i>. Een <b>activator</b> A stimuleert zichzelf en een <b>inhibitor</b> I; de inhibitor remt beide. Diffundeert de inhibitor veel sneller dan de activator (activatie van dichtbij, remming op afstand), dan ontstaan uit willekeurige ruis stabiele vlekken of strepen: <b>Turing-patronen</b>.</p>
      <p class="muted" style="font-size:.88rem">Rechts begint elk hokje met een willekeurige hoeveelheid activator en zonder inhibitor. Reactie, diffusie, reactie, diffusie… en er verschijnt een patroon. Maar let op: niet elk streepjespatroon in de natuur is een Turing-patroon.</p>
    </div>
    <div class="card">
      <canvas id="hero" class="sq" style="max-width:300px"></canvas>
      <div class="muted" id="hero-lbl" style="font-size:.85rem;text-align:center;margin-top:4px;min-height:2.6em"></div></div>
  </div>

  <div class="cards">
    <div class="card ov"><span class="num">17.1</span><h4>Van reactievergelijking naar ODEs</h4>
      <p>Elk pijltje wordt een term. mRNA wordt afgeschreven (+e), breekt af (−dM) en wordt vertaald naar eiwit (+vM in dE/dt). Een reactie tussen twee stoffen, zoals E + S → C, gaat met snelheid k·E·S: evenredig met beide concentraties.</p></div>
    <div class="card ov"><span class="num">17.2</span><h4>Diffusie tussen hokjes</h4>
      <p>Deel de ruimte op in hokjes. Per stap stroomt een fractie D van de inhoud naar elk buurhokje. De totale hoeveelheid blijft gelijk, de verschillen worden kleiner. Met oneindig kleine hokjes krijg je een <b>PDE</b>: tijd én ruimte als onafhankelijke variabelen.</p>
      <button class="b" data-go="diffusie">Stap voor stap →</button></div>
    <div class="card ov"><span class="num">17.3</span><h4>Activerende en remmende eiwitten</h4>
      <p>De activator maakt meer van zichzelf en van de inhibitor; de inhibitor remt allebei. Het ODE-systeem heeft twee gebogen nullclines en meerdere evenwichten: <b>bistabiliteit</b>. Waar je uitkomt, hangt af van waar je begint (zie ook hysterese).</p></div>
    <div class="card ov"><span class="num">17.4</span><h4>Turing-patronen</h4>
      <p>Zet A en I in hokjes en laat ze diffunderen. Als de inhibitor veel sneller diffundeert dan de activator, ontstaan uit ruis stabiele vlekken en strepen. Diffundeert de activator net zo snel, dan verdwijnt het patroon.</p>
      <button class="b" data-go="turing">Maak een zebra →</button></div>
    <div class="card ov"><span class="num">17.5</span><h4>Niet elk patroon is een Turing-patroon</h4>
      <p>Dat een mechanisme een patroon <i>kan</i> verklaren, bewijst niet dat het dat <i>doet</i>. De strepen van een fruitvliegembryo komen van een genencascade, en een simpele meerderheidsregel maakt ook "struikgewas".</p></div>
    <div class="card ov"><span class="num">Opg.</span><h4>Opgaven</h4>
      <p>Michaelis-Menten-kinetiek omzetten in ODEs, diffusie met een te grote stap (D = 0,3: wat gaat er mis?), en bacteriën die hun buren helpen maar op afstand vergiftigen met bijproducten: ook daar ontstaan groepjes.</p></div>
    <div class="card ov"><span class="num">✓</span><h4>Test jezelf</h4>
      <p>Tien korte vragen over reactietermen, diffusie, bistabiliteit en Turing-patronen. Bij een fout antwoord mag je het opnieuw proberen.</p>
      <button class="b primary" data-go="quiz">Start de quiz →</button></div>
  </div>

  <details>
    <summary>Terminologie</summary>
    <table class="cmp">
      <tr><th>Nederlands</th><th>Engels</th><th>Betekenis</th></tr>
      <tr><td>Reactie-diffusievergelijkingen</td><td>Reaction-diffusion equations</td><td>Model waarin variabelen zowel in de tijd (reacties) als in de ruimte (diffusie) veranderen. Geschikt om het ontstaan van patronen te onderzoeken.</td></tr>
      <tr><td>Partiële differentiaalvergelijking</td><td>Partial differential equation (PDE)</td><td>Zoals een ODE, maar met tijd én ruimte als onafhankelijke variabelen: "oneindig kleine hokjes".</td></tr>
      <tr><td>Turing-patronen</td><td>Turing patterns</td><td>Zelforganiserende patronen uit twee stoffen die reageren en met verschillende snelheden diffunderen: activatie van dichtbij, remming op afstand.</td></tr>
      <tr><td>Bistabiliteit</td><td>Bistability</td><td>Het systeem heeft twee stabiele evenwichten; de beginconditie bepaalt waar het terechtkomt.</td></tr>
      <tr><td>Hysterese</td><td>Hysteresis</td><td>Bij meerdere evenwichten kan een systeem over een omslagpunt kiepen, en komt het niet zomaar terug.</td></tr>
    </table>
  </details>
</section>

<section class="panel" id="tab-diffusie" hidden>
  <h2>Diffusie tussen hokjes</h2>
  <p>Een druppel verf midden op een gel. Elke stap geeft elk hokje een fractie D van zijn inhoud aan <b>elk van zijn buren</b> (boven, onder, links, rechts), en krijgt het diezelfde fractie van de inhoud van elke buur. Dat is de regel uit het hoofdstuk: <span class="eqn">nieuw = oud + D·(som van de buren − 4·oud)</span>. Druk op <b>Volgende stap</b>, of klik op een hokje om er extra verf op te druppelen.</p>
  <div class="grid2">
    <div class="card"><div class="ptitle">Het grid (7 × 7), hoeveelheid verf per hokje</div><canvas id="df-grid" class="sq click grid-small"></canvas></div>
    <div class="card"><div class="ptitle">Hoeveelheid verf door de tijd</div><canvas id="df-plot" class="plot"></canvas>
      <div class="legend"><span><i class="dot" style="background:var(--accent)"></i>midden</span><span><i class="dot" style="background:var(--accent2)"></i>buur van het midden</span><span><i class="dot" style="background:var(--muted)"></i>hoek</span><span><i class="dot" style="background:var(--fg)"></i>totaal (alle hokjes)</span></div></div>
  </div>
  <div class="controls"><div class="ctl"><label><span class="lbl">diffusieconstante D</span> <span id="df-d-v"></span></label><input type="range" id="df-d" min="0" max="0.3" step="0.01" value="0.1"></div></div>
  <div class="btns">
    <button class="b primary" id="df-step">Volgende stap →</button>
    <button class="b" id="df-play">▶ Afspelen</button>
    <button class="b" id="df-reset">↺ Opnieuw</button>
    <span class="muted" id="df-t" style="font-size:.88rem"></span>
  </div>
  <div class="callout stepnote" id="df-text"></div>
</section>

<section class="panel" id="tab-turing" hidden>
  <h2>Turing-patronen: activator en inhibitor in hokjes</h2>
  <p>Elk hokje bevat een activator A en een inhibitor I die reageren volgens de ODEs uit het hoofdstuk, met <span class="nomath">k<sub>a</sub> = 4, k<sub>i</sub> = 1, d<sub>a</sub> = 0,8, d<sub>i</sub> = 1,5</span>. Elke tijdstap: eerst de ODEs een stapje doorrekenen (Δt = 0,5), dan een stapje diffusie tussen de buurhokjes, met aparte diffusieconstanten <span class="nomath">D<sub>A</sub></span> en <span class="nomath">D<sub>I</sub></span>. We beginnen met een willekeurige hoeveelheid A per hokje en zonder I. <b>Teken met de muis</b> op een van de twee grids om zelf activator toe te voegen of weg te halen: zo start of verstoor je een patroon.</p>
  <div class="eqs"><span class="eqn">dA/dt = k<sub class="nomath">a</sub>A² / (1 + A²) − k<sub class="nomath">i</sub>A·I − d<sub class="nomath">a</sub>A</span><span class="eqn">dI/dt = k<sub class="nomath">a</sub>A − k<sub class="nomath">i</sub>I² − d<sub class="nomath">i</sub>I</span></div>
  <div class="btns"><span class="muted" style="font-size:.88rem">Probeer:</span><div class="seg" id="tu-presets"></div></div>
  <div class="grid2">
    <div class="card"><div class="ptitle">Activator A (96 × 96)</div><canvas id="tu-a" class="sq" style="cursor:crosshair"></canvas></div>
    <div class="card"><div class="ptitle">Inhibitor I</div><canvas id="tu-i" class="sq" style="cursor:crosshair"></canvas></div>
  </div>
  <div class="controls">
    <div class="ctl"><label><span class="lbl">diffusie activator D<sub class="nomath">A</sub></span> <span id="tu-da-v"></span></label><input type="range" id="tu-da" min="0" max="2" step="0.05" value="0.1"></div>
    <div class="ctl"><label><span class="lbl">diffusie inhibitor D<sub class="nomath">I</sub></span> <span id="tu-di-v"></span></label><input type="range" id="tu-di" min="0" max="5" step="0.1" value="2"></div>
  </div>
  <div class="btns">
    <button class="b primary" id="tu-play">⏸ Pauze</button>
    <button class="b" id="tu-reset">↺ Opnieuw (nieuwe ruis)</button>
    <span class="muted" id="tu-t" style="font-size:.88rem"></span>
    <span class="muted" style="font-size:.88rem;margin-left:8px">Muis:</span><div class="seg" id="tu-brush"><button type="button" data-b="add" aria-pressed="true">activator toevoegen</button><button type="button" data-b="del">activator weghalen</button></div>
  </div>
  <div class="card"><div class="ptitle">Concentraties langs de middelste rij van het grid</div><canvas id="tu-prof" class="plot-prof"></canvas>
    <div class="legend"><span><i class="dot" style="background:var(--accent)"></i>activator A</span><span><i class="dot" style="background:var(--accent2)"></i>inhibitor I</span></div></div>
  <div class="callout" id="tu-text"></div>
</section>

<section class="panel" id="tab-quiz" hidden>
  <h2>Quiz: reacties en diffusie</h2>
  <div style="display:flex;justify-content:space-between;font-size:.88rem" class="muted"><span id="qz-count"></span><span id="qz-score"></span></div>
  <div class="progress"><i id="qz-prog"></i></div>
  <div id="qz-box"></div>
  <div class="btns" style="justify-content:flex-end"><button class="b primary" id="qz-next" hidden>Volgende vraag →</button></div>
</section>
`;
function mount(host){
if(host.__appMounted) return; host.__appMounted=true;
const root=host.attachShadow({mode:'open'});
root.innerHTML='<style>'+STYLE+EXTRA+'</style><div class="app">'+HTML+'</div>';
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
   GEDEELD: kleurenschaal, veld tekenen, activator-inhibitor-simulatie
   ========================================================= */
const VIR=[[68,1,84],[59,82,139],[33,145,140],[94,201,98],[253,231,37]];        // viridis, zoals in het hoofdstuk
const INF=[[0,0,4],[87,16,110],[188,55,84],[249,142,9],[252,255,164]];          // inferno, voor de inhibitor
function cmap(tab,v){ v=v<0?0:v>1?1:v; const p=v*4, k=Math.min(3,Math.floor(p)), f=p-k, a=tab[k], b=tab[k+1];
  return [a[0]+(b[0]-a[0])*f, a[1]+(b[1]-a[1])*f, a[2]+(b[2]-a[2])*f]; }
/* paints a W×H field (values 0..vmax) pixel for pixel via an offscreen canvas; the browser scales it up without smoothing */
function fieldPainter(W,H){
  const off=document.createElement('canvas'); off.width=W; off.height=H; const octx=off.getContext('2d'), img=octx.createImageData(W,H);
  return function(c,F,vmax,tab){ const d=img.data;
    for(let i=0;i<W*H;i++){ const [r,g,b]=cmap(tab,F[i]/vmax); d[i*4]=r; d[i*4+1]=g; d[i*4+2]=b; d[i*4+3]=255; }
    octx.putImageData(img,0,0);
    const {ctx,w,h}=fitCanvas(c); const s=Math.min(w,h), ox=(w-s)/2, oy=(h-s)/2;
    ctx.clearRect(0,0,w,h); ctx.imageSmoothingEnabled=false; ctx.drawImage(off,ox,oy,s,s);
    ctx.strokeStyle=col('line'); ctx.lineWidth=1; ctx.strokeRect(ox+.5,oy+.5,s-1,s-1); };
}
const dnum=(v,d=2)=>v.toFixed(d).replace('.',',');
/* activator-inhibitor model from 17.3/17.4, on a wrapped W×H grid. Per step: Euler step of the ODEs (dt = 0.5,
   as in the chapter's simulation), then the local diffusion rule from 17.2 for A and I separately.
   Diffusion with D > 0.2 is split into sub-steps of at most 0.2, so it stays stable at every slider setting. */
function turingSim(W,H,rnd=Math.random){
  const N=W*H, A=new Float32Array(N), I=new Float32Array(N), T=new Float32Array(N);
  const P={ka:4,ki:1,da:0.8,di:1.5,dt:0.5};
  const S={A,I,W,H,t:0,P};
  S.reset=()=>{ for(let i=0;i<N;i++){ A[i]=rnd(); I[i]=0; } S.t=0; };
  function diffuse(C,D){ if(D<=0) return; const n=Math.max(1,Math.ceil(D/0.2-1e-9)), d=D/n;
    for(let k=0;k<n;k++){
      for(let y=0;y<H;y++){ const yu=((y+H-1)%H)*W, yd=((y+1)%H)*W, y0=y*W;
        for(let x=0;x<W;x++){ const c=C[y0+x]; T[y0+x]=c+d*(C[yu+x]+C[yd+x]+C[y0+(x+W-1)%W]+C[y0+(x+1)%W]-4*c); } }
      C.set(T); } }
  S.step=(DA,DI)=>{ const {ka,ki,da,di,dt}=P;
    for(let i=0;i<N;i++){ const a=A[i], b=I[i];
      const na=a+dt*(ka*a*a/(1+a*a)-ki*a*b-da*a), nb=b+dt*(ka*a-ki*b*b-di*b);
      A[i]=na>0?na:0; I[i]=nb>0?nb:0; }
    diffuse(A,DA); diffuse(I,DI); S.t++; };
  S.reset(); return S;
}

/* =========================================================
   IN ÉÉN OOGOPSLAG: een Turing-patroon ontstaat uit ruis
   ========================================================= */
const hero=(()=>{
  const c=$('#hero'), lbl=$('#hero-lbl'), W=48, sim=turingSim(W,W), paint=fieldPainter(W,W);
  const TEND=420; let hold=0;
  function frameFn(dt){
    if(hold>0){ hold-=dt; if(hold<=0){ sim.reset(); } }
    else { for(let k=0;k<3;k++) sim.step(0.1,2); if(sim.t>=TEND) hold=4; }
    paint(c,sim.A,1.6,VIR);
    lbl.innerHTML=sim.t<30?'Stap '+sim.t+': willekeurige ruis van activator, nog geen inhibitor.'
      :sim.t<TEND?`Stap ${sim.t}: de inhibitor diffundeert 20× sneller dan de activator…`
      :`Stap ${sim.t}: een <b>stabiel patroon</b> van activator (geel = veel). Activatie dichtbij, remming op afstand.`;
  }
  return {frame:frameFn};
})();

/* =========================================================
   DIFFUSIE TUSSEN HOKJES (17.2)
   ========================================================= */
const diffu=(()=>{
  const n=7, N=n*n, MAXT=60, cv=$('#df-grid'), pc=$('#df-plot'), sD=$('#df-d'), vD=$('#df-d-v'), txt=$('#df-text'), tl=$('#df-t'),
        bStep=$('#df-step'), bPlay=$('#df-play'), bReset=$('#df-reset');
  const MID=3*n+3, NB=3*n+4, CORNER=0;
  let C=new Float64Array(N), hist=[], t=0, playing=false, acc=0, exploded=false, lastD=0.1;
  const total=()=>C.reduce((a,b)=>a+b,0);
  const snap=()=>[C[MID],C[NB],C[CORNER],total()];
  function setPlay(p){ playing=p; bPlay.textContent=p?'⏸ Pauze':'▶ Afspelen'; }
  function reset(){ C.fill(0); C[MID]=4; t=0; acc=0; exploded=false; hist=[snap()]; setPlay(false); render(); }
  /* the rule from the chapter: new = old + D·(sum of neighbours − 4·old). At the edge a cell has fewer neighbours,
     and only exchanges with the ones it has, so the total amount is conserved. */
  function step(){ if(t>=MAXT||exploded) return; const D=+sD.value, T=new Float64Array(N);
    for(let y=0;y<n;y++) for(let x=0;x<n;x++){ const i=y*n+x, c=C[i]; let s=0;
      if(y>0) s+=C[i-n]-c; if(y<n-1) s+=C[i+n]-c; if(x>0) s+=C[i-1]-c; if(x<n-1) s+=C[i+1]-c;
      T[i]=c+D*s; }
    C=T; t++; lastD=D; hist.push(snap());
    if(C.some(v=>Math.abs(v)>50)){ exploded=true; setPlay(false); }
    if(t>=MAXT) setPlay(false);
    render(); }
  function render(){
    const D=+sD.value; vD.textContent=dnum(D); bStep.disabled=t>=MAXT||exploded; bPlay.disabled=t>=MAXT||exploded;
    tl.textContent=`stap ${t} van ${MAXT} · totaal ${dnum(total())}`;
    const mid=C[MID], nb=C[NB], weg=4*D;
    let s=`<b>Stap ${t}.</b> `;
    if(t===0) s+=`Het middelste hokje bevat 4,0; de rest 0. Met D = ${dnum(D)} raakt het midden straks 4 × ${dnum(D)} = ${dnum(weg)} × 4,0 = ${dnum(weg*4)} kwijt, en krijgt elke buur ${dnum(D*4)}.`;
    else { const h0=hist[t-1];
      s+=`Het midden had ${dnum(h0[0])} en heeft nu ${dnum(mid)}; de buur had ${dnum(h0[1])} en heeft nu ${dnum(nb)}. `;
      s+=`Het totaal blijft ${dnum(total())}: diffusie <b>verplaatst</b> alleen, er wordt niets aangemaakt of afgebroken. `;
      if(t>=6&&Math.max(...C)-Math.min(...C)<0.6&&!exploded) s+=`De verschillen zijn bijna weg: het patroon (de stip in het midden) is vervaagd.`; }
    if(D>0.25) s+=` <b>Let op:</b> bij D = ${dnum(D)} gaat er 4 × ${dnum(D)} = ${Math.round(weg*100)}% van de inhoud weg: meer dan er in zit. De hoeveelheid wordt negatief, en het model ontploft. De tijdstap is te groot. Dit is vergelijkbaar met de "te grote" tijdstappen bij de groei van de konijnen in hoofdstuk 11 (zie ook de opgave Diffusie simuleren).`;
    else if(D===0.25) s+=` Bij D = 0,25 geeft een hokje precies alles weg aan zijn vier buren: de grens van wat nog kan.`;
    if(exploded) s+=` <b>Ontploft!</b> Druk op Opnieuw en kies een kleinere D.`;
    txt.innerHTML=s; }
  function drawGrid(){
    const {ctx,w,h}=fitCanvas(cv); const s=Math.min(w,h)/n, ox=(w-s*n)/2, oy=(h-s*n)/2;
    ctx.clearRect(0,0,w,h); ctx.font=`600 ${Math.max(9,Math.round(s*0.27))}px ${FONT.slice(5)}`; ctx.textAlign='center'; ctx.textBaseline='middle';
    for(let y=0;y<n;y++) for(let x=0;x<n;x++){ const v=C[y*n+x], X=ox+x*s, Y=oy+y*s;
      ctx.fillStyle=col('bg'); ctx.fillRect(X,Y,s,s);
      ctx.save(); ctx.globalAlpha=Math.min(1,Math.max(0,v)/4); ctx.fillStyle=v<0?col('warn'):col('accent'); ctx.fillRect(X,Y,s,s); ctx.restore();
      ctx.strokeStyle=col('line'); ctx.lineWidth=1; ctx.strokeRect(X+.5,Y+.5,s-1,s-1);
      ctx.fillStyle=v<0?col('warn'):v>2?'#fff':col('fg'); ctx.fillText(Math.abs(v)<1e3?dnum(v):fmt(v,2),X+s/2,Y+s/2+1); }
  }
  function drawPlot(){
    let hi=0, lo=0; for(const r of hist){ hi=Math.max(hi,r[3]); lo=Math.min(lo,r[0],r[1]); }
    const f=frame(pc,{xmin:0,xmax:MAXT,ymin:Math.max(-2,Math.floor(lo)),ymax:niceCeil(Math.min(12,hi*1.05)),xlab:'stap',ylab:'hoeveelheid verf',pad:{l:48,b:34}});
    const L=k=>hist.map((r,i)=>[i,r[k]]);
    poly(f,L(3),col('fg'),1.6,[5,4]); poly(f,L(2),col('muted'),2); poly(f,L(1),col('accent2'),2.2); poly(f,L(0),col('accent'),2.4);
    f.ctx.restore();
  }
  cv.addEventListener('click',e=>{ const r=cv.getBoundingClientRect(), s=Math.min(r.width,r.height)/n, ox=(r.width-s*n)/2, oy=(r.height-s*n)/2;
    const x=Math.floor((e.clientX-r.left-ox)/s), y=Math.floor((e.clientY-r.top-oy)/s); if(x<0||y<0||x>=n||y>=n||exploded) return;
    C[y*n+x]+=4; hist[t]=snap(); render(); });
  bStep.onclick=()=>{ setPlay(false); step(); };
  bPlay.onclick=()=>{ if(t>=MAXT||exploded) return; setPlay(!playing); acc=0; };
  bReset.onclick=reset; sD.oninput=render;
  reset();
  return {frame:dt=>{ if(playing){ acc+=dt; if(acc>=0.35){ acc=0; step(); } } drawGrid(); drawPlot(); }};
})();

/* =========================================================
   TURING-PATRONEN (17.4)
   ========================================================= */
const turing=(()=>{
  const W=96, sim=turingSim(W,W), paint=fieldPainter(W,W);
  const ca=$('#tu-a'), ci=$('#tu-i'), cp=$('#tu-prof'), segB=$('#tu-brush'), sDA=$('#tu-da'), sDI=$('#tu-di'), vDA=$('#tu-da-v'), vDI=$('#tu-di-v'),
        bPlay=$('#tu-play'), bReset=$('#tu-reset'), tl=$('#tu-t'), txt=$('#tu-text'), seg=$('#tu-presets');
  const PRESETS=[['Turing-patroon',0.1,2],['Nog snellere inhibitor',0.1,5],['Even snel',1,1],['Snelle activator',2,2],['Geen diffusie',0,0]];
  let playing=true;
  function setPlay(p){ playing=p; bPlay.textContent=p?'⏸ Pauze':'▶ Verder'; }
  function labels(){ const DA=+sDA.value, DI=+sDI.value; vDA.textContent=dnum(DA); vDI.textContent=dnum(DI);
    seg.querySelectorAll('button').forEach(b=>b.setAttribute('aria-pressed',+b.dataset.da===DA&&+b.dataset.di===DI));
    const ratio=DA>0?DI/DA:Infinity;
    txt.innerHTML=(DA===0&&DI===0)?`<b>Zonder diffusie</b> staat elk hokje op zichzelf. Het ODE-systeem is bistabiel (17.3): afhankelijk van de willekeurige beginwaarde gaat een hokje naar het evenwicht met veel A of naar het evenwicht zonder A. Het resultaat is ruis, geen patroon.`
      :ratio>=8?`<b>D<sub class="nomath">I</sub> is ${isFinite(ratio)?dnum(ratio,0)+'×':'veel'} groter dan D<sub class="nomath">A</sub>.</b> Een hokje met wat meer activator maakt via zelfactivatie nog meer activator (dichtbij), maar ook inhibitor, die snel wegdiffundeert en de omgeving remt (op afstand). Zo ontstaan stabiele vlekken en strepen: een <b>Turing-patroon</b>.`
      :ratio>=3?`<b>D<sub class="nomath">I</sub> is ${dnum(ratio,1)}× D<sub class="nomath">A</sub>.</b> De inhibitor diffundeert wel sneller, maar niet veel. Vaak wint de remming overal en dooft alles uit, of het patroon wordt zwak. Vergroot het verschil tussen de twee diffusieconstanten.`
      :`<b>D<sub class="nomath">A</sub> en D<sub class="nomath">I</sub> zijn ongeveer even groot.</b> Activator en inhibitor spreiden zich samen uit; de remming houdt de activatie overal bij. Diffusie vervaagt nu alleen maar, zoals bij de verf: geen patroon. Turing-patronen vereisen een inhibitor die veel sneller diffundeert dan de activator.`;
  }
  function profile(){
    const y0=(W>>1)*W, f=frame(cp,{xmin:0,xmax:W-1,ymin:0,ymax:2,ny:4,xlab:'hokje (x)',ylab:'concentratie',pad:{l:52,b:34}});
    const pa=[], pi=[]; for(let x=0;x<W;x++){ pa.push([x,sim.A[y0+x]]); pi.push([x,sim.I[y0+x]]); }
    poly(f,pi,col('accent2'),2); poly(f,pa,col('accent'),2.4); f.ctx.restore();
  }
  seg.innerHTML=PRESETS.map(([nm,da,di])=>`<button type="button" data-da="${da}" data-di="${di}">${nm}</button>`).join('');
  seg.querySelectorAll('button').forEach(b=>b.onclick=()=>{ sDA.value=b.dataset.da; sDI.value=b.dataset.di; sim.reset(); setPlay(true); labels(); });
  sDA.oninput=labels; sDI.oninput=labels;
  bPlay.onclick=()=>setPlay(!playing);
  bReset.onclick=()=>{ sim.reset(); setPlay(true); };
  /* tekenen met de muis: activator toevoegen (A = 2) of weghalen (A = 0) in een klein rondje */
  let brush='add', drawing=false;
  segB.querySelectorAll('button').forEach(b=>b.onclick=()=>{ brush=b.dataset.b; segB.querySelectorAll('button').forEach(x=>x.setAttribute('aria-pressed',x===b)); });
  function dab(c,e){ const r=c.getBoundingClientRect(), s=Math.min(r.width,r.height), ox=(r.width-s)/2, oy=(r.height-s)/2;
    const cx=Math.floor((e.clientX-r.left-ox)/s*W), cy=Math.floor((e.clientY-r.top-oy)/s*W); if(cx<0||cy<0||cx>=W||cy>=W) return;
    const R=2; for(let dy=-R;dy<=R;dy++) for(let dx=-R;dx<=R;dx++){ if(dx*dx+dy*dy>R*R+1) continue;
      const i=((cy+dy+W)%W)*W+(cx+dx+W)%W; sim.A[i]=brush==='add'?2:0; } }
  for(const c of [ca,ci]){
    c.addEventListener('pointerdown',e=>{ drawing=true; c.setPointerCapture(e.pointerId); dab(c,e); e.preventDefault(); });
    c.addEventListener('pointermove',e=>{ if(drawing) dab(c,e); });
    const stop=()=>{ drawing=false; }; c.addEventListener('pointerup',stop); c.addEventListener('pointercancel',stop); c.addEventListener('lostpointercapture',stop); }
  labels();
  return {frame:()=>{ if(playing){ const DA=+sDA.value, DI=+sDI.value; for(let k=0;k<3;k++) sim.step(DA,DI); }
    paint(ca,sim.A,1.6,VIR); paint(ci,sim.I,1.6,INF); profile();
    tl.textContent=`tijdstap ${sim.t} (t = ${dnum(sim.t*sim.P.dt,1)})`; }};
})();

/* =========================================================
   QUIZ
   ========================================================= */
/* two concentration profiles along a line: narrow peaks (accent) and broad humps (accent2) */
/* twee zwart-witte grids (licht = hoge concentratie) uit het echte Turing-model, met vaste willekeur */
function grayPNG(F,W){ const c=document.createElement('canvas'); c.width=W; c.height=W; const ctx=c.getContext('2d'), img=ctx.createImageData(W,W), d=img.data;
  let mx=0; for(let i=0;i<W*W;i++) if(F[i]>mx) mx=F[i]; mx=mx||1;
  for(let i=0;i<W*W;i++){ const g=Math.round(255*Math.min(1,F[i]/mx)); d[i*4]=g; d[i*4+1]=g; d[i*4+2]=g; d[i*4+3]=255; }
  ctx.putImageData(img,0,0); return c.toDataURL(); }
function gridsHTML(){ const W=40; let seed=20241; const rnd=()=>{ seed=(seed*1103515245+12345)%2147483648; return seed/2147483648; };
  const sim=turingSim(W,W,rnd); for(let k=0;k<400;k++) sim.step(0.1,2);
  const fig=(src,lab)=>`<figure style="margin:0;text-align:center"><img src="${src}" alt="${lab}" width="150" height="150" style="display:block;image-rendering:pixelated;border:1px solid var(--line);border-radius:4px"><figcaption class="nomath" style="font-size:.85rem;margin-top:4px">${lab}</figcaption></figure>`;
  return `<div class="nomath" style="display:flex;gap:18px;flex-wrap:wrap;margin:8px 0">${fig(grayPNG(sim.I,W),'Grid 1')}${fig(grayPNG(sim.A,W),'Grid 2')}</div>`; }
const quiz=(()=>{
  const G1=gridsHTML();
  const BASE=[
    {q:'Enzym E en substraat S vormen samen een complex C, met snelheidsconstante <span class="nomath"><i>k</i><sub>1</sub></span>: <span class="nomath"><i>E</i> + <i>S</i> → <i>C</i></span>. Welke term beschrijft dit proces in dC/dt?',
     o:['+ k<sub class="nomath">1</sub>·E·S','+ k<sub class="nomath">1</sub>·(E + S)','+ k<sub class="nomath">1</sub>·C','− k<sub class="nomath">1</sub>·E·S'], a:0,
     h:'De reactie heeft een E én een S nodig. Hoe vaak komen die elkaar tegen?',
     e:'De snelheid van een reactie tussen twee stoffen is evenredig met beide concentraties: k<sub class="nomath">1</sub>·E·S. Het complex neemt daardoor toe (+). In dE/dt en dS/dt staat dezelfde term met een minteken.'},
    {q:'Bij Michaelis-Menten-kinetiek valt het complex met snelheid <span class="nomath"><i>k</i><sub>cat</sub></span> uiteen in enzym en product: <span class="nomath"><i>C</i> → <i>E</i> + <i>P</i></span>. Welke ODE hoort bij het product P?',
     o:['dP/dt = k<sub class="nomath">cat</sub>·C','dP/dt = k<sub class="nomath">cat</sub>·E·P','dP/dt = k<sub class="nomath">cat</sub>·C − k<sub class="nomath">cat</sub>·P','dP/dt = k<sub class="nomath">cat</sub>·E·S'], a:0,
     h:'Product ontstaat uit complex, en in de reactievergelijking verdwijnt product nergens.',
     e:'Elke keer dat een complex uiteenvalt, komt er één product bij: dP/dt = k<sub class="nomath">cat</sub>·C. Er is geen afbraakterm, want in deze reactievergelijking wordt P niet omgezet. Dezelfde term staat als −k<sub class="nomath">cat</sub>·C in dC/dt.'},
    {q:'Een hokje bevat 4,0 verf, zijn vier buren 0. De diffusieconstante is D = 0,1. Hoeveel zit er na <b>één stap</b> in het middelste hokje en in elke buur?',
     o:['Midden 2,4 en elke buur 0,4','Midden 3,6 en elke buur 0,1','Midden 3,6 en elke buur 0,4','Midden 0 en elke buur 1,0'], a:0,
     h:'Er gaat 10% naar <i>elk</i> van de vier buren. Tel ook na of het totaal nog 4,0 is.',
     e:'Naar elke buur gaat 0,1 × 4,0 = 0,4, dus in totaal 4 × 0,4 = 1,6 weg: 4,0 − 1,6 = 2,4. Controle: 2,4 + 4 × 0,4 = 4,0. Diffusie verplaatst, maar maakt niets aan of kapot.'},
    {q:'In de opgave probeer je een diffusieconstante van D = 0,3. Wat gaat er mis?',
     o:['Het middelste hokje geeft 4 × 30% = 120% weg en wordt negatief','Niets, de verf verspreidt zich alleen wat sneller over de vier buren','De buren krijgen samen meer verf dan het midden verliest','De totale hoeveelheid verf neemt elke stap een beetje toe'], a:0,
     h:'Hoeveel procent van de inhoud gaat er in één stap naar de vier buren samen?',
     e:'Met vier buren verdwijnt 4 × 0,3 = 120% van de inhoud: onmogelijk, de tijdstap is te groot. Dit is vergelijkbaar met de "te grote" tijdstappen bij de groei van de konijnen in hoofdstuk 11. Een PDE met oneindig kleine hokjes en stapjes heeft dit probleem niet.'},
    {q:'Wat is het verschil tussen een PDE en een ODE?',
     o:['Een PDE volgt een stof in tijd én ruimte, een ODE alleen in tijd','Een PDE beschrijft losse individuen, een ODE hele populaties','Een PDE heeft meer toestandsvariabelen dan een ODE, maar dezelfde vorm','Een PDE is een ODE waarin ook toeval (ruis) is opgenomen'], a:0,
     h:'Waar staat de P voor? Denk aan "oneindig kleine hokjes".',
     e:'Een partiële differentiaalvergelijking heeft tijd én ruimte als onafhankelijke variabelen: ze beschrijft hoe een concentratie verandert in de tijd én per plek in de ruimte. In de computer benaderen we dat met voldoende kleine hokjes.'},
    {q:'Hieronder zie je de activator en de inhibitor uit het Turing-model op hetzelfde moment, allebei in zwart-wit (licht = hoge concentratie). Welk grid is de <b>inhibitor</b>?'+G1,
     o:['Grid 1: het patroon is vager en breder uitgesmeerd','Grid 2: het patroon is scherper en fijner getekend','Allebei: activator en inhibitor zijn hier niet te onderscheiden','Geen van beide: zonder kleurenschaal kun je dat niet zien'], a:0,
     h:'Welke stof diffundeert het snelst, en wat doet diffusie met scherpe randen?',
     e:'De inhibitor diffundeert veel sneller dan de activator en spreidt zich dus uit over een grotere afstand: remming op afstand. Daardoor is het inhibitorpatroon een vage, uitgesmeerde versie van het activatorpatroon. De scherpe vlekken en strepen zijn de activator, die dicht bij huis blijft.'},
    {q:'Het activator-inhibitorsysteem is <b>bistabiel</b>. Wat betekent dat?',
     o:['Er zijn twee stabiele evenwichten; de beginconditie bepaalt welke','Het systeem slingert eindeloos heen en weer tussen twee toestanden','Beide variabelen gaan vanuit elke beginwaarde naar hetzelfde evenwicht','Het evenwicht is stabiel in de richting van A, maar niet van I'], a:0,
     h:'In de faseruimte starten twee trajecten op verschillende plekken. Komen ze op dezelfde plek uit?',
     e:'Begin je met weinig activator, dan dooft de expressie uit; met wat meer activator groeit die naar een hoog evenwicht. De geschiedenis van het systeem telt. Bij omslagpunten in ecosystemen heet dit hysterese.'},
    {q:'Wanneer ontstaan er Turing-patronen in een activator-inhibitormodel?',
     o:['Als de inhibitor veel sneller diffundeert dan de activator','Als de activator veel sneller diffundeert dan de inhibitor','Als beide stoffen precies even snel door het weefsel diffunderen','Als er helemaal geen diffusie is en elke cel op zichzelf staat'], a:0,
     h:'Activatie van dichtbij, remming op afstand. Welke stof moet dan ver komen?',
     e:'Een plek met veel activator versterkt zichzelf lokaal, maar stuurt via de snel diffunderende inhibitor remming naar de omgeving. Diffundeert de activator even snel, dan vervaagt alles, zoals de verf.'},
    {q:'Een fruitvliegembryo heeft strepen, en een Turing-model kan strepen maken. Wat mag je concluderen?',
     o:['Niets zeker: een mechanisme dat een patroon kán maken, is geen bewijs','De strepen zijn een Turing-patroon, want het model maakt ze precies na','De strepen zijn zeker geen Turing-patroon, want een embryo is te klein','Turing-modellen gelden alleen voor de vachtpatronen van zoogdieren'], a:0,
     h:'Hoe bleken de strepen van de fruitvlieg echt te ontstaan?',
     e:'De strepen van de fruitvlieg komen van een cascade van genen die het lijf in steeds kleinere stukken verdeelt. Ook een simpele meerderheidsregel maakt struikgewas-achtige patronen. Een passend model is nog geen bewijs.'},
    {q:'In de opgave Bijproducten reproduceren bacteriën met een kans die afhangt van het aantal buren in een <b>kleine</b> straal, en sterven ze door bijproducten van buren in een <b>grote</b> straal. Waarom ontstaan er groepjes?',
     o:['Hulp van dichtbij en remming op afstand, net als bij Turing-patronen','Omdat de bacteriën willekeurig rondbewegen en toevallig samenklonteren','Omdat het totale aantal bacteriën in het model behouden blijft','Omdat de bijproducten de reproductie van verre buren versnellen'], a:0,
     h:'Vergelijk de kleine straal met de activator en de grote straal met de inhibitor.',
     e:'Hulp op korte afstand (meer reproductie) en vergiftiging op grote afstand (meer sterfte) zijn precies de ingrediënten van een Turing-patroon, nu in een individual-based model: groepjes met lege ruimte ertussen.'},
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
    const msg=frac===1?'Perfect! Reacties, diffusie en Turing-patronen: je hebt het door.':frac>=.75?'Heel goed! Kijk nog even naar de vragen die je niet in één keer goed had.':frac>=.5?'Op de goede weg. Speel nog eens met de hokjes en de Turing-simulatie.':'Lees het hoofdstuk nog eens door en speel met de interactieve figuren. Daarna gaat het vast beter!';
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
const modules={overzicht:hero,diffusie:diffu,turing:turing,quiz:quiz};
let current='overzicht';
const TABKEY='bioms-tab-rd';   // the last opened tab is remembered in this browser
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
function init(){ document.querySelectorAll('[data-rd-app]').forEach(mount); }
window.RdApp={mount,init};
const H0=inlineHost(CS); if(H0) mount(H0);
if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();
