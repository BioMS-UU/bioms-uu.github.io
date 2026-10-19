/* ODE-explainer (Biologische Modellen en Statistiek, hoofdstuk 11).
 * Gebruik in Quarto (één regel, op de plek waar de app moet komen):
 *   <script src="Applets/odes_explainer.js"></script>                        -> hele app met tabbladen
 *   <script src="Applets/odes_explainer.js" data-tab="evenwicht"></script>   -> alleen dat onderdeel
 *   <script src="Applets/odes_explainer.js" data-tab="evenwicht" data-nav="1"></script> -> tabbladen, start op dat tabblad
 * data-tab: overzicht | tijdstappen | herten | eenheden | evenwicht | quiz
 * data-theme="dark|light" forceert een thema; anders volgt de app het boek (ook de donkere modus).
 * (Alternatief: <div data-ode-app data-tab="..."></div> plus het script één keer op de pagina.)
 */
(function(){
"use strict";
const CS=document.currentScript;
function inlineHost(cs){ if(!cs||!cs.dataset||cs.__odeDone||!cs.parentNode) return null;
  const single=!!cs.dataset.tab;
  // a plain <script> with no data-tab becomes the full app with tabs, unless the page already uses <div data-ode-app>
  if(!single&&document.querySelector('[data-ode-app]')) return null;
  cs.__odeDone=true;
  const d=document.createElement('div'); d.setAttribute('data-ode-app','');
  d.dataset.tab=cs.dataset.tab||'overzicht';
  if(single&&cs.dataset.nav!=='1') d.dataset.hidenav='1'; if(cs.dataset.theme) d.dataset.theme=cs.dataset.theme;
  cs.parentNode.insertBefore(d,cs); return d; }
if(window.OdeApp){ const h=inlineHost(CS); if(h) window.OdeApp.mount(h); window.OdeApp.init(); return; }
const STYLE="\n\n:host{\n  --bg: var(--bs-body-bg, #ffffff);\n  --fg: var(--bs-body-color, #1d2327);\n  --line: var(--bs-border-color, #d9e0dc);\n  --card: color-mix(in srgb, var(--fg) 4%, var(--bg));\n  --muted: color-mix(in srgb, var(--fg) 62%, var(--bg));\n  --grid: color-mix(in srgb, var(--fg) 9%, var(--bg));\n  --axis: color-mix(in srgb, var(--fg) 50%, var(--bg));\n  --accent:#D55E00; --accent-soft:#D55E0022; --accent2:#0072B2; --blue:#0072B2; --warn:#555b61;\n  --font: var(--bs-body-font-family, system-ui,-apple-system,\"Segoe UI\",Roboto,\"Helvetica Neue\",sans-serif);\n  --mono: var(--bs-font-monospace, ui-monospace,SFMono-Regular,Menlo,Consolas,monospace);\n  display:block; color:var(--fg); font-family:var(--font); font-size:.95em; line-height:1.5; margin:1rem 0;\n}\n:host([data-dark]){\n  --bg: var(--bs-body-bg, #16191b);\n  --fg: var(--bs-body-color, #e6e9e7);\n  --line: var(--bs-border-color, #30383c);\n  --accent:#F08A3E; --accent-soft:#F08A3E2B; --accent2:#56B4E9; --blue:#56B4E9; --warn:#aab2b8;\n}\n.app{color:var(--fg)}\n.ovb{display:inline-block;border-top:1.5px solid currentColor;line-height:1.05;padding-top:1px}\n*{box-sizing:border-box}\nh2{font-size:1.15em;margin:.2em 0 .4em;border:0;padding:0}\nh3{font-size:1em;margin:1em 0 .3em}\np{margin:.35rem 0 .6rem}\nb{font-weight:600}\n.muted{color:var(--muted)}\n.mono{font-family:var(--mono)}\n.eqn,.formula{font-family:\"Latin Modern Math\",\"STIX Two Math\",\"Cambria Math\",\"Times New Roman\",Georgia,serif;font-size:1.06em;white-space:nowrap}\n.mi{font-style:italic}\n.mu{font-style:normal}\n/* tabs */\nnav.tabs{display:flex;gap:4px;flex-wrap:wrap;border-bottom:1px solid var(--line);margin-bottom:12px}\n.tab{appearance:none;border:0;background:none;color:var(--muted);font:inherit;font-size:.92rem;padding:7px 11px;border-radius:8px 8px 0 0;cursor:pointer;border-bottom:2px solid transparent;margin-bottom:-1px}\n.tab:hover{color:var(--fg)}\n.tab[aria-selected=\"true\"]{color:var(--accent);border-bottom-color:var(--accent);font-weight:600}\n/* layout */\n.grid2{display:grid;grid-template-columns:1fr 1fr;gap:14px}\n@media (max-width:680px){.grid2{grid-template-columns:1fr}}\n.card{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:10px 12px}\ncanvas{display:block;width:100%;border-radius:8px}\n.plot{aspect-ratio:16/10}\n.plot-wide{aspect-ratio:16/8}\n@media (max-width:680px){.plot-wide{aspect-ratio:16/10}}\n.controls{position:relative;display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:6px 16px;margin:8px 0}\n.ctl label{display:flex;justify-content:space-between;font-size:.88rem;color:var(--muted)}\n.ctl label span{color:var(--fg);font-family:var(--mono);font-size:.85rem}\n.ctl label span.lbl{color:var(--muted);font-family:var(--font);font-size:.88rem}\ninput[type=range]{width:100%;accent-color:var(--accent)}\n.btns{display:flex;gap:6px;flex-wrap:wrap;align-items:center;margin:6px 0}\nbutton.b{appearance:none;font:inherit;font-size:.88rem;border:1px solid var(--line);background:var(--card);color:var(--fg);padding:5px 12px;border-radius:7px;cursor:pointer}\nbutton.b:hover{border-color:var(--accent)}\nbutton.b:disabled{opacity:.5;cursor:default}\nbutton.b.primary{background:var(--accent);border-color:var(--accent);color:#fff}\n.chk{font-size:.88rem;color:var(--muted);display:inline-flex;gap:5px;align-items:center;margin-right:10px}\n.chk input{accent-color:var(--accent)}\n.seg{display:inline-flex;border:1px solid var(--line);border-radius:8px;overflow:hidden;flex-wrap:wrap}\n.seg button{appearance:none;font:inherit;font-size:.86rem;border:0;background:none;color:var(--muted);padding:5px 10px;cursor:pointer}\n.seg button[aria-pressed=\"true\"]{background:var(--accent);color:#fff}\n.callout{border-left:3px solid var(--accent);background:var(--accent-soft);padding:7px 11px;border-radius:0 8px 8px 0;margin:8px 0}\n.callout.warn{border-left-color:var(--accent2);background:color-mix(in srgb,var(--accent2) 13%,transparent)}\n.formula{font-size:1.06em;display:inline-block;margin:2px 0}\n/* overview */\n.cards{display:grid;grid-template-columns:repeat(auto-fill,minmax(230px,1fr));gap:10px;margin-top:8px}\n.ov{display:flex;flex-direction:column;gap:4px}\n.ov .num{font-size:.75rem;color:var(--muted);font-family:var(--mono)}\n.ov h4{margin:0;font-size:.95rem}\n.ov p{margin:0;font-size:.88rem;flex:1}\n.ov button{align-self:flex-start;margin-top:6px}\ntable.cmp{border-collapse:collapse;width:100%;font-size:.88rem;margin:6px 0}\ntable.cmp td,table.cmp th{border-bottom:1px solid var(--line);padding:5px 6px;text-align:left;vertical-align:top}\ntable.cmp th{color:var(--muted);font-weight:600}\ndetails{margin-top:10px}\nsummary{cursor:pointer;color:var(--accent);font-weight:600}\n/* flows */\n.flows{display:grid;grid-template-columns:auto 1fr auto;gap:4px 8px;align-items:center;font-size:.86rem;margin-top:8px}\n.bar{height:10px;border-radius:5px;background:var(--line);overflow:hidden}\n.bar i{display:block;height:100%;width:0;border-radius:5px;transition:width .08s linear}\n.legend{display:flex;gap:12px;font-size:.8rem;color:var(--muted);margin-top:4px;flex-wrap:wrap}\n.dot{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:4px;vertical-align:-1px}\n/* units */\n.eq{display:flex;align-items:flex-start;justify-content:center;gap:10px;flex-wrap:wrap;margin:14px 0 6px}\n.term{display:flex;flex-direction:column;align-items:center;gap:6px;min-width:90px}\n.term .sym{height:2.6em;display:flex;align-items:center;font-size:1.45rem;font-family:\"Cambria Math\",\"STIX Two Math\",Georgia,serif;padding:4px 12px;border-radius:8px;border:2px solid transparent;transition:all .35s}\n.term.hl .sym{border-color:var(--accent);background:var(--accent-soft)}\n.term .unit{min-height:46px;font-size:.92rem;color:var(--accent2);opacity:0;transform:translateY(-6px);transition:all .45s;text-align:center}\n.term .unit.show{opacity:1;transform:none}\n.op{font-size:1.45rem;height:2.6em;display:flex;align-items:center;font-family:Georgia,serif}\n.frac{display:inline-flex;flex-direction:column;align-items:center;vertical-align:middle;line-height:1.15;margin:0 2px}\n.frac>span:first-child{border-bottom:1.5px solid currentColor;padding:0 3px}\n.frac>span:last-child{padding:0 3px}\n.derive{min-height:120px;display:flex;flex-direction:column;align-items:center;gap:8px;margin:6px 0}\n.derive .ln{opacity:0;animation:fadein .5s forwards;font-size:1.05rem;display:flex;align-items:center;gap:8px;flex-wrap:wrap;justify-content:center}\n@keyframes fadein{from{opacity:0;transform:translateY(6px)}to{opacity:1;transform:none}}\n.cancel{position:relative;display:inline-block}\n.cancel::after{content:\"\";position:absolute;left:-2px;top:50%;height:2px;background:var(--warn);width:0;animation:strike .5s forwards;animation-delay:inherit;transform:rotate(-12deg)}\n@keyframes strike{to{width:calc(100% + 4px)}}\n.stepper{display:flex;justify-content:center;align-items:center;gap:10px;margin-top:6px}\n.dots{display:flex;gap:5px}\n.dots i{width:8px;height:8px;border-radius:50%;background:var(--line);cursor:pointer}\n.dots i.on{background:var(--accent)}\n.steptext{min-height:4.5em;text-align:center;max-width:620px;margin:6px auto}\n.term .unit.same{background:var(--accent-soft);border:1.5px solid var(--accent);border-radius:8px;padding:2px 8px;color:var(--fg);animation:same .9s ease-out}\n@keyframes same{0%{transform:scale(1)}40%{transform:scale(1.12)}100%{transform:scale(1)}}\n/* quiz */\n.q{margin:10px 0;padding:10px 12px}\n.q .opts{display:flex;flex-wrap:wrap;gap:6px;margin:6px 0}\n.q .opts button{appearance:none;font:inherit;font-size:.92rem;border:1px solid var(--line);background:var(--bg);color:var(--fg);padding:5px 12px;border-radius:7px;cursor:pointer;min-height:38px}\n.q .opts button.right{border-color:var(--accent);background:var(--accent-soft)}\n.q .opts button.wrong{border-color:var(--warn);opacity:.7}\n.q .fb{font-size:.88rem;display:none}\n.q .fb.show{display:block}\n.score{font-size:.9rem;color:var(--muted)}\n/* animated marks */\n.qhead{display:flex;gap:10px;justify-content:space-between;align-items:flex-start}\n.qmark{position:relative;width:34px;height:34px;flex:none}\n.mark{width:100%;height:100%;overflow:visible}\n.mark circle,.mark path{fill:none;stroke-linecap:round;stroke-linejoin:round}\n.mark circle{stroke-width:2.6;stroke-dasharray:88;stroke-dashoffset:88;animation:draw .45s ease-out forwards;transform:rotate(-90deg);transform-origin:center}\n.mark path{stroke-width:3.4;stroke-dasharray:30;stroke-dashoffset:30;animation:draw .35s .4s ease-out forwards}\n.mark.ok circle,.mark.ok path{stroke:var(--accent)}\n.mark.no circle,.mark.no path{stroke:var(--warn)}\n.mark.ok{animation:pop .35s .7s ease-out}\n@keyframes draw{to{stroke-dashoffset:0}}\n@keyframes pop{50%{transform:scale(1.22)}}\n.burst{position:absolute;left:50%;top:50%;width:0;height:0;pointer-events:none}\n.burst i{position:absolute;width:6px;height:6px;margin:-3px;border-radius:50%;background:var(--accent);opacity:0;animation:burst .7s .55s ease-out forwards}\n.burst i:nth-child(odd){background:var(--accent2)}\n@keyframes burst{0%{opacity:1;transform:rotate(var(--a)) translateY(0)}100%{opacity:0;transform:rotate(var(--a)) translateY(calc(-1 * var(--d,26px)))}}\n.shake{animation:shake .4s}\n@keyframes shake{20%,60%{transform:translateX(-5px)}40%,80%{transform:translateX(5px)}}\n.q .fb.ok{color:var(--fg)}\n.q .fb.no{color:var(--warn)}\n.q .opts button.right{font-weight:600}\n.slidein{animation:fadein .35s}\n.progress{height:6px;background:var(--line);border-radius:3px;overflow:hidden;margin:6px 0 10px}\n.progress i{display:block;height:100%;width:0;background:var(--accent);transition:width .4s}\n.endcard{text-align:center;padding:24px 12px}\n.bigmark{position:relative;width:84px;height:84px;margin:0 auto 6px;--d:70px}\n.bigmark .burst i{width:9px;height:9px;margin:-4.5px}\n@media (prefers-reduced-motion:reduce){.mark,.mark *,.burst i,.shake{animation-duration:.01s!important;animation-delay:0s!important}}\n.ctl.locked{opacity:.5}\n.ctl.locked input{cursor:not-allowed}\n.toast{position:absolute;left:50%;bottom:calc(100% + 10px);transform:translate(-50%,6px);z-index:5;background:var(--fg);color:var(--bg);padding:9px 13px;border-radius:9px;font-size:.88rem;line-height:1.4;width:max-content;max-width:min(540px,94%);box-shadow:0 6px 20px rgba(0,0,0,.25);opacity:0;pointer-events:none;transition:opacity .2s,transform .2s}\n.toast::after{content:\"\";position:absolute;left:50%;top:100%;margin-left:-7px;border:7px solid transparent;border-top-color:var(--fg)}\n.toast.show{opacity:1;transform:translate(-50%,0)}\n.toast b{font-weight:700}\n.ptitle{font-weight:600;font-size:.92rem;margin:0 0 4px}\n[hidden]{display:none!important}\n";
const HTML="\n\n<nav class=\"tabs\" role=\"tablist\">\n  <button class=\"tab\" data-tab=\"overzicht\">Overzicht<\/button>\n  <button class=\"tab\" data-tab=\"tijdstappen\">Tijdstappen<\/button>\n  <button class=\"tab\" data-tab=\"herten\">ODE = snelheid<\/button>\n  <button class=\"tab\" data-tab=\"eenheden\">Eenheden<\/button>\n  <button class=\"tab\" data-tab=\"quiz\">Quiz \u2713<\/button>\n<\/nav>\n\n\n<section class=\"panel\" id=\"tab-overzicht\">\n  <h2>Hoofdstuk 11 in \u00e9\u00e9n oogopslag<\/h2>\n  <div class=\"grid2\">\n    <div>\n      <p>Een <b>differentievergelijking<\/b> rekent in sprongen: van <i>N(t)<\/i> naar <i>N(t+1)<\/i>. Een <b>differentiaalvergelijking (ODE)<\/b> beschrijft hoe <i>snel<\/i> iets verandert, op elk moment. Dat komt neer op oneindig kleine tijdstapjes.<\/p>\n      <p class=\"muted\" style=\"font-size:.88rem\">De animatie rechts rekent hetzelfde logistische model door met steeds kleinere stappen \u0394t. Grote stappen schieten door en gaan schommelen. Kleine stappen volgen de soepele ODE-oplossing (stippellijn).<\/p>\n      <table class=\"cmp\">\n        <tr><th><\/th><th>Differentievergelijking<\/th><th>ODE<\/th><\/tr>\n        <tr><td>Vorm<\/td><td class=\"eqn\">N(t+1) = rN(t)<\/td><td class=\"eqn\">dN/dt = rN<\/td><\/tr>\n        <tr><td>Beschrijft<\/td><td>de volgende <i>waarde<\/i><\/td><td>de <i>snelheid<\/i> van verandering<\/td><\/tr>\n        <tr><td>Tijd<\/td><td>discreet (stappen)<\/td><td>continu<\/td><\/tr>\n        <tr><td>Groei als<\/td><td>r &gt; 1<\/td><td>r &gt; 0<\/td><\/tr>\n      <\/table>\n    <\/div>\n    <div class=\"card\"><canvas id=\"hero\" class=\"plot\"><\/canvas>\n      <div class=\"muted\" id=\"hero-lbl\" style=\"font-size:.85rem;text-align:center;margin-top:4px\"><\/div><\/div>\n  <\/div>\n\n  <div class=\"cards\">\n    <div class=\"card ov\"><span class=\"num\">11.2<\/span><h4>Chaotische konijnen<\/h4>\n      <p>De logistische differentievergelijking gaat bij een grote groeisnelheid wild schommelen. Een iets ander beginaantal geeft een totaal andere uitkomst. Dat heet <b>deterministische chaos<\/b>.<\/p>\n      <button class=\"b\" data-go=\"tijdstappen\">Probeer het \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">11.3<\/span><h4>Het probleem van tijdstappen<\/h4>\n      <p>De chaos ontstaat doordat de tijdstap te groot is. Met kleinere stappen krijg je een soepele S-curve. Een ODE neemt oneindig kleine stappen.<\/p>\n      <button class=\"b\" data-go=\"tijdstappen\">Bekijk animatie \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">11.4\u201311.6<\/span><h4>ODEs beschrijven snelheden<\/h4>\n      <p>Bij het hertenfeestje geldt <span class=\"eqn\">dH/dt = m<\/span>: elk uur komen er <i>m<\/i> herten bij. Een ODE vertelt de <b>helling<\/b> van de grafiek, niet de waarde zelf.<\/p>\n      <button class=\"b\" data-go=\"herten\">Naar het hertenfeestje \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">11.7<\/span><h4>Functies als oplossingen<\/h4>\n      <p>De oplossing van een ODE is een <b>functie<\/b> <i>H(t)<\/i>, geen getal. Elke beginwaarde geeft een andere <b>specifieke oplossing<\/b>. Samen vormen ze een familie van oplossingen.<\/p>\n      <button class=\"b\" data-go=\"herten\">Teken oplossingen \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">11.8<\/span><h4>Rekenen met eenheden<\/h4>\n      <p>\"Per\" betekent \"gedeeld door\". Elke term in een ODE heeft dezelfde eenheid als <i>dX/dt<\/i>. Daaruit volgt de eenheid van elke parameter.<\/p>\n      <button class=\"b\" data-go=\"eenheden\">Stap voor stap \u2192<\/button><\/div>\n    <div class=\"card ov\"><span class=\"num\">11.8<\/span><h4>Aannames van ODEs<\/h4>\n      <p>ODEs gaan uit van drie dingen. Populaties zijn continu (dichtheden). Alle individuen zijn gelijk. De populatie is goed gemengd (<i>well-mixed<\/i>).<\/p><\/div>\n    \n    <div class=\"card ov\"><span class=\"num\">\u2713<\/span><h4>Test jezelf<\/h4>\n      <p>Negen korte vragen over het hele hoofdstuk. Bij een fout antwoord mag je het opnieuw proberen.<\/p>\n      <button class=\"b primary\" data-go=\"quiz\">Start de quiz \u2192<\/button><\/div>\n  <\/div>\n\n  <details>\n    <summary>Terminologie<\/summary>\n    <table class=\"cmp\">\n      <tr><th>Nederlands<\/th><th>Engels<\/th><th>Betekenis<\/th><\/tr>\n      <tr><td>Differentiaalvergelijking<\/td><td>ODE<\/td><td>Beschrijft hoe snel een variabele verandert.<\/td><\/tr>\n      <tr><td>Continue variabele<\/td><td>Continuous variable<\/td><td>Kan elke waarde aannemen, ook decimalen.<\/td><\/tr>\n      <tr><td>Discrete variabele<\/td><td>Discrete variable<\/td><td>Alleen specifieke waarden, vaak gehele getallen.<\/td><\/tr>\n      <tr><td>Specifieke oplossing<\/td><td>Specific solution<\/td><td>E\u00e9n tijdserie bij een gekozen beginwaarde en parameters.<\/td><\/tr>\n      <tr><td>Algemene oplossing<\/td><td>General solution<\/td><td>Formule waar alle specifieke oplossingen aan voldoen.<\/td><\/tr>\n    <\/table>\n  <\/details>\n<\/section>\n\n\n<section class=\"panel\" id=\"tab-tijdstappen\" hidden>\n  <h2>Hoe groot mag een tijdstap zijn?<\/h2>\n  <p>We rekenen de logistische groei <span class=\"formula\">dN/dt = rN(1 \u2212 N/K)<\/span> door in stappen van \u0394t. Elke stap volgen we de helling gedurende \u0394t. Elk <b style=\"color:var(--accent)\">oranje lijnstukje<\/b> is zo'n stap. Tijdens het afspelen zie je de stap die nu berekend wordt als <b>stippellijn met een open rondje<\/b>.<\/p>\n  <p style=\"text-align:center\"><span class=\"formula\">N(t+\u0394t) = N(t) + \u0394t rN(t)(1 \u2212 N(t)/K)<\/span><\/p>\n  <div class=\"card\">\n    <canvas id=\"ts-plot\" class=\"plot-wide\"><\/canvas>\n    <div class=\"legend\"><span><i class=\"dot\" style=\"background:var(--accent)\"><\/i>stappen van \u0394t<\/span><span><i class=\"dot\" style=\"background:var(--muted)\"><\/i>echte ODE-oplossing (stippellijn)<\/span><span id=\"ts-leg-twin\" hidden><i class=\"dot\" style=\"background:var(--blue)\"><\/i>beginwaarde +4%<\/span><\/div>\n    <div class=\"controls\">\n      <div class=\"ctl\"><label><span class=\"lbl\">groeisnelheid r<\/span> <span id=\"ts-r-v\"><\/span><\/label><input type=\"range\" id=\"ts-r\" min=\"0.1\" max=\"3\" step=\"0.05\" value=\"2.6\"><\/div>\n      <div class=\"ctl\"><label><span class=\"lbl\">tijdstap \u0394t<\/span> <span id=\"ts-dt-v\"><\/span><\/label><input type=\"range\" id=\"ts-dt\" min=\"0\" max=\"6\" step=\"1\" value=\"5\"><\/div>\n    <\/div>\n    <div class=\"btns\">\n      <button class=\"b primary\" id=\"ts-play\">\u25b6 Afspelen<\/button>\n      <label class=\"chk\"><input type=\"checkbox\" id=\"ts-exact\" checked> echte ODE-oplossing<\/label>\n      <label class=\"chk\"><input type=\"checkbox\" id=\"ts-twin\"> vergelijk met een n\u00e9t andere beginwaarde (+4%)<\/label>\n    <\/div>\n    <div class=\"callout\" id=\"ts-regime\"><\/div>\n  <\/div>\n  <p class=\"muted\" style=\"font-size:.88rem\">Het gedrag hangt af van het product <b>r\u0394t<\/b>. Een snelle groei met een grote stap geeft dezelfde chaos als bij de konijnen uit hoofdstuk 10. Vink \"vergelijk met een n\u00e9t andere beginwaarde\" aan: dan verschijnt een <b style=\"color:var(--blue)\">blauwe stippellijn<\/b> die met 4% meer konijnen begint. Bij chaos lopen de oranje en de blauwe lijn steeds verder uit elkaar, ook al verschilde het begin nauwelijks. Bij een soepele S-curve blijven ze vrijwel samen. Maak \u0394t klein genoeg en het model gedraagt zich weer als een soepele S-curve.<\/p>\n<\/section>\n\n\n<section class=\"panel\" id=\"tab-herten\" hidden>\n  <h2>Het hertenfeestje: een ODE beschrijft snelheid<\/h2>\n  <p>Er lopen <b>m<\/b> herten per uur het bos in, en elk hert vertrekt met snelheid <b>d<\/b> per uur:\n    <span class=\"formula\">dH/dt = m \u2212 dH<\/span>. Zet <i>d<\/i> op 0 voor het eenvoudigste feestje, <span class=\"eqn\">dH/dt = m<\/span>.<\/p>\n  <div class=\"grid2\">\n    <div class=\"card\">\n      <canvas id=\"forest\" class=\"plot\"><\/canvas>\n      <div class=\"muted\" style=\"font-size:.82rem;margin-top:4px\">Zo tekenen biologen een ODE: \u00e9\u00e9n <b>compartiment<\/b> (het bos) met een pijl per proces. De dikte van een pijl is de snelheid. H is een <b>continue dichtheid<\/b> in een goed gemengd bos, geen verzameling losse herten. Daarom kleurt het hele bos gelijkmatig donkerder naarmate H groter wordt. Meer herten betekent een dikkere vertrekpijl (dH).<\/div>\n      <div class=\"flows\">\n        <span>instroom <b>m<\/b><\/span><div class=\"bar\"><i id=\"bar-in\" style=\"background:var(--accent)\"><\/i><\/div><span class=\"mono\" id=\"v-in\"><\/span>\n        <span>uitstroom <b>dH<\/b><\/span><div class=\"bar\"><i id=\"bar-out\" style=\"background:var(--accent2)\"><\/i><\/div><span class=\"mono\" id=\"v-out\"><\/span>\n        <span>netto <b>dH/dt<\/b><\/span><div class=\"bar\"><i id=\"bar-net\"><\/i><\/div><span class=\"mono\" id=\"v-net\"><\/span>\n      <\/div>\n    <\/div>\n    <div class=\"card\">\n      <canvas id=\"herten-plot\" class=\"plot\"><\/canvas>\n      <div class=\"muted\" style=\"font-size:.82rem;margin-top:4px\"><b>Het rechte lijnstuk door de stip<\/b> is de helling op dit moment = <i>dH/dt<\/i>. \ud83d\udc46 <b>Klik in de grafiek<\/b> om een andere beginwaarde te kiezen: dan verschijnt de bijbehorende <b>specifieke oplossing<\/b> (<span style=\"color:var(--blue)\">blauw<\/span>).<\/div>\n    <\/div>\n  <\/div>\n  <div class=\"controls\">\n    <div class=\"toast\" id=\"h-toast\" role=\"status\" aria-live=\"polite\"><\/div>\n    <div class=\"ctl\"><label><span class=\"lbl\">migratie m<\/span> <span id=\"h-m-v\"><\/span><\/label><input type=\"range\" id=\"h-m\" min=\"0\" max=\"10\" step=\"0.5\" value=\"3\"><\/div>\n    <div class=\"ctl\"><label><span class=\"lbl\">vertrek d<\/span> <span id=\"h-d-v\"><\/span><\/label><input type=\"range\" id=\"h-d\" min=\"0\" max=\"0.2\" step=\"0.01\" value=\"0.05\"><\/div>\n    <div class=\"ctl\"><label><span class=\"lbl\">begin H\u2080<\/span> <span id=\"h-h0-v\"><\/span><\/label><input type=\"range\" id=\"h-h0\" min=\"0\" max=\"200\" step=\"5\" value=\"100\"><\/div>\n  <\/div>\n  <div class=\"btns\">\n    <button class=\"b primary\" id=\"h-play\">\u25b6 Start<\/button>\n    <button class=\"b\" id=\"h-reset\">\u21ba Opnieuw<\/button>\n    <button class=\"b\" id=\"h-clear\" hidden>Wis oplossingen<\/button>\n  <\/div>\n  <div class=\"callout\" id=\"h-text\"><\/div>\n<\/section>\n\n\n<section class=\"panel\" id=\"tab-eenheden\" hidden>\n  <h2>Rekenen met eenheden<\/h2>\n  <div class=\"card\">\n    <div class=\"eq\">\n      <div class=\"term\" id=\"u-lhs\"><div class=\"sym\"><span class=\"frac\"><span><span class=\"mu\">d<\/span><i class=\"mi\">H<\/i><\/span><span><span class=\"mu\">d<\/span><i class=\"mi\">t<\/i><\/span><\/span><\/div><div class=\"unit\"><\/div><\/div>\n      <div class=\"op\">=<\/div>\n      <div class=\"term\" id=\"u-m\"><div class=\"sym\">m<\/div><div class=\"unit\"><\/div><\/div>\n      <div class=\"op\">\u2212<\/div>\n      <div class=\"term\" id=\"u-dH\"><div class=\"sym\">dH<\/div><div class=\"unit\"><\/div><\/div>\n    <\/div>\n    <div class=\"derive\" id=\"u-derive\"><\/div>\n    <p class=\"steptext\" id=\"u-text\"><\/p>\n    <div class=\"stepper\">\n      <button class=\"b\" id=\"u-prev\">\u2190 Vorige<\/button>\n      <div class=\"dots\" id=\"u-dots\"><\/div>\n      <button class=\"b primary\" id=\"u-next\">Volgende \u2192<\/button>\n    <\/div>\n  <\/div>\n\n  <h3>Oefen zelf<\/h3>\n  <p class=\"muted\" style=\"font-size:.88rem\">Gebruik hetzelfde recept: eenheid van links \u2192 elke term dezelfde eenheid \u2192 los op voor de parameter.<\/p>\n  <div id=\"quiz\"><\/div>\n  <div class=\"score\" id=\"quiz-score\"><\/div>\n<\/section>\n\n\n<section class=\"panel\" id=\"tab-quiz\" hidden>\n  <h2>Quiz: modelleren met ODEs<\/h2>\n  <div style=\"display:flex;justify-content:space-between;font-size:.88rem\" class=\"muted\"><span id=\"qz-count\"><\/span><span id=\"qz-score\"><\/span><\/div>\n  <div class=\"progress\"><i id=\"qz-prog\"><\/i><\/div>\n  <div id=\"qz-box\"><\/div>\n  <div class=\"btns\" style=\"justify-content:flex-end\"><button class=\"b primary\" id=\"qz-next\" hidden>Volgende vraag \u2192<\/button><\/div>\n<\/section>\n\n";
function mount(host){
if(host.__odeMounted) return; host.__odeMounted=true;
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
   OVERZICHT: hero
   ========================================================= */
const hero=(()=>{
  const c=$('#hero'), lbl=$('#hero-lbl');
  const dts=[2,1,0.5,0.25,0.1,0.02], r=1.2, x0=0.03, T=12, DUR=2.6;
  let ph=0, tt=0;
  function frameFn(dt){
    tt+=dt; if(tt>DUR){ tt=0; ph=(ph+1)%dts.length; }
    const f=frame(c,{xmin:0,xmax:T,ymin:0,ymax:1.6,xlab:'tijd (jaren)',ylab:'N / K',pad:{l:40}});
    const ex=[]; for(let i=0;i<=200;i++){ const t=T*i/200; ex.push([t,logisticExact(t,x0,r)]); }
    poly(f,ex,col('muted'),1.6,[5,4]);
    const dtv=dts[ph], {pts}=euler(r,dtv,x0,T);
    const k=Math.max(1,Math.floor(Math.min(1,tt/(DUR*0.6))*pts.length));
    poly(f,pts.slice(0,k),col('accent'),2.4);
    if(dtv>=0.25) for(const [x,y] of pts.slice(0,k)) disc(f.ctx,f.X(x),f.Y(y),3,col('accent'));
    f.ctx.restore();
    lbl.innerHTML=`Δt = <b>${dtv}</b> jaar · ${dtv>=1?'te grote stappen: doorschieten en schommelen':dtv>=0.25?'kleinere stappen: bijna soepel':'heel kleine stappen ≈ de ODE'}`;
  }
  return {frame:frameFn};
})();

/* =========================================================
   TIJDSTAPPEN
   ========================================================= */
const tijd=(()=>{
  const c=$('#ts-plot'), sr=$('#ts-r'), sdt=$('#ts-dt'), btn=$('#ts-play'), cbE=$('#ts-exact'), cbT=$('#ts-twin'), reg=$('#ts-regime');
  const DTS=[0.02,0.1,0.25,0.5,0.75,1,1.5], T=15, x0=0.02;
  let k=Infinity, playing=false;
  function regime(a,broke){
    if(broke||a>3) return ['warn','<b>Model breekt.</b> De stap is zo groot dat de populatie negatief wordt of explodeert. Biologisch onzin!'];
    if(a>2.57) return ['warn','<b>Deterministische chaos.</b> De populatie springt onvoorspelbaar heen en weer en komt nooit tot rust.'];
    if(a>2.45) return ['warn','<b>Periodeverdubbeling.</b> De populatie wisselt tussen 4, 8, … verschillende waarden.'];
    if(a>2) return ['warn','<b>Periode-2 cyclus.</b> De populatie springt eeuwig heen en weer tussen twee waarden rond K.'];
    if(a>1) return ['', '<b>Doorschieten.</b> De populatie schiet over K heen en dempt daarna uit. Een echte ODE-oplossing doet dat niet.'];
    return ['','<b>Soepel.</b> De stappen volgen de echte ODE-oplossing netjes. Hoe kleiner Δt, hoe beter.'];
  }
  function upd(){
    const r=+sr.value, dt=DTS[+sdt.value];
    $('#ts-r-v').textContent=r.toFixed(2)+' per jaar';
    $('#ts-dt-v').textContent=dt+' jaar';
  }
  function play(){ k=0; playing=true; btn.textContent='⏸ Pauze'; }
  btn.onclick=()=>{ if(playing){ playing=false; btn.textContent='▶ Afspelen'; } else { if(k>=1e8||!isFinite(k)) k=0; play(); } };
  [sr,sdt].forEach(s=>s.addEventListener('input',()=>{ upd(); k=Infinity; playing=false; btn.textContent='▶ Afspelen'; }));
  upd();
  function frameFn(dt){
    const r=+sr.value, dtv=DTS[+sdt.value], a=r*dtv;
    const {pts,broke}=euler(r,dtv,x0,T);
    const twin=cbT.checked?euler(r,dtv,x0*1.04,T).pts:null; $('#ts-leg-twin').hidden=!cbT.checked;
    if(playing){ k+=dt*Math.max(4,pts.length/3.5); if(k>=pts.length){ k=pts.length; playing=false; btn.textContent='▶ Afspelen'; } }
    const n=Math.min(pts.length,Math.max(1,Math.floor(k)));
    const f=frame(c,{xmin:0,xmax:T,ymin:-0.1,ymax:1.6,xlab:'tijd (jaren)',ylab:'N / K'});
    const cx=f.ctx;
    cx.save(); cx.setLineDash([3,4]); cx.strokeStyle=col('muted'); cx.lineWidth=1; cx.beginPath(); cx.moveTo(f.X(0),f.Y(1)); cx.lineTo(f.X(T),f.Y(1)); cx.stroke(); cx.restore();
    cx.fillStyle=col('muted'); cx.textAlign='right'; cx.textBaseline='bottom'; drawMath(cx,'K',f.X(T)-4,f.Y(1)-2);
    if(cbE.checked){ const ex=[]; for(let i=0;i<=300;i++){ const t=T*i/300; ex.push([t,logisticExact(t,x0,r)]); } poly(f,ex,col('muted'),1.8,[6,4]); }
    const shown=pts.slice(0,n);
    poly(f,shown,col('accent'),2.4);
    if(twin) poly(f,twin.slice(0,n),col('blue'),2,[6,4],0.95);
    const showTan=pts.length<=80;
    if(showTan){
      for(let i=0;i<n;i++){ const [t,x]=shown[i]; disc(cx,f.X(t),f.Y(x),3.2,col('accent')); }
      if(n<pts.length){ const [t,x]=pts[n-1]; const s=r*x*(1-x);
        cx.save(); cx.strokeStyle=col('fg'); cx.lineWidth=2.2; cx.setLineDash([4,3]);
        cx.beginPath(); cx.moveTo(f.X(t),f.Y(x)); cx.lineTo(f.X(t+dtv),f.Y(x+s*dtv)); cx.stroke(); cx.restore();
        disc(cx,f.X(t+dtv),f.Y(x+s*dtv),4,null,col('fg'),2); }
    }
    cx.restore();
    const [cls,txt]=regime(a,broke);
    reg.className='callout '+cls;
    reg.innerHTML=`rΔt = ${r.toFixed(2)} × ${dtv} = <b>${a.toFixed(2)}</b>. ${txt}`;
  }
  return {frame:frameFn};
})();

/* =========================================================
   HERTEN
   ========================================================= */
const herten=(()=>{
  const fc=$('#forest'), pc=$('#herten-plot');
  const sm=$('#h-m'), sd=$('#h-d'), sh=$('#h-h0'), btn=$('#h-play'), clr=$('#h-clear');
  const T=60, SPEED=4;
  let m,d,H0,H,t,hist,playing=false,anchors=[],ymax=200,lastF=null,phase=0;
  const Hat=(tt,t1,H1)=> d>0 ? m/d+(H1-m/d)*Math.exp(-d*(tt-t1)) : H1+m*(tt-t1);
  function computeYmax(){ let v=Math.max(H0,H,20); for(const [,h] of hist) v=Math.max(v,h); for(let i=0;i<=20;i++){ v=Math.max(v,Hat(t+(T-t)*i/20,t,H)); } ymax=niceCeil(v*1.12); }
  function readP(){ m=+sm.value; d=+sd.value; $('#h-m-v').textContent=m+' herten/uur'; $('#h-d-v').textContent=d.toFixed(2)+' per uur'; $('#h-h0-v').textContent=sh.value+' herten'; }
  function setPlaying(v){ playing=v; btn.textContent=v?'⏸ Pauze':'▶ Start'; [sm,sd,sh].forEach(x=>x.closest('.ctl').classList.toggle('locked',v)); }
  function reset(){ readP(); H0=+sh.value; H=H0; t=0; hist=[[0,H0]]; setPlaying(false); computeYmax(); }
  btn.onclick=()=>{ if(t>=T) reset(); setPlaying(!playing); };
  $('#h-reset').onclick=reset;
  function updClr(){ clr.hidden=!anchors.length; clr.textContent=anchors.length===1?'Wis oplossing':`Wis ${anchors.length} oplossingen`; }
  clr.onclick=()=>{ anchors=[]; updClr(); };
  // Parameters are constant during a run: block changes while playing and explain why;
  // when paused, any change starts a fresh run from t = 0.
  let toastT=null; const toast=$('#h-toast');
  function lockMsg(){ toast.innerHTML='🔒 <b>Parameters in een ODE zijn constant.</b> Ze veranderen niet halverwege. Pauzeer eerst: met een nieuwe waarde begint een nieuwe run bij t = 0.';
    toast.classList.add('show'); clearTimeout(toastT); toastT=setTimeout(()=>toast.classList.remove('show'),4000); }
  [sm,sd,sh].forEach(x=>{
    const block=e=>{ if(!playing) return; e.preventDefault(); e.stopPropagation(); lockMsg(); };
    x.addEventListener('pointerdown',block,true); x.addEventListener('touchstart',block,{capture:true,passive:false}); x.addEventListener('keydown',block,true);
    x.addEventListener('input',()=>{ if(playing){ sm.value=m; sd.value=d; sh.value=H0; lockMsg(); return; } reset(); });
  });
  pc.addEventListener('click',e=>{ if(!lastF) return; const [px,py]=localXY(e,pc); if(!lastF.inside(px,py)) return; const [tt,hh]=lastF.inv(px,py); anchors.push([tt,Math.max(0,hh)]); if(anchors.length>10) anchors.shift(); updClr(); });
  reset();

  function sim(dtR){
    if(!playing) return;
    phase+=dtR;
    let ds=dtR*SPEED; if(t+ds>=T){ ds=T-t; setPlaying(false); }
    const n=Math.max(1,Math.ceil(ds/0.01)), h=ds/n;
    for(let i=0;i<n;i++){ H+=(m-d*H)*h; t+=h; }
    hist.push([t,H]);
    if(H>ymax*0.97) computeYmax();
  }
  /* Continuous "stock & flow" picture: H is a quantity (a fill level), not a set of individuals. */
  /* Compartment diagram: one box (the forest, well-mixed) with arrows for the processes.
     H is a continuous density, shown as the box's shade; arrow width = rate. */
  const pines=[[.10,.93,.75],[.21,.95,.95],[.33,.93,.7],[.45,.95,.85],[.57,.93,.7],[.68,.95,.95],[.79,.93,.75],[.90,.95,.85]];
  function drawTank(){
    const {ctx,w,h}=fitCanvas(fc); ctx.clearRect(0,0,w,h);
    const bx=w*0.27, bw=w*0.46, by=h*0.14, bh=h*0.72, cy=by+bh*0.5, rad=14;
    const out=d*H, A=col('accent'), O=col('accent2'), fg=col('fg'), mu=col('muted');
    const rateMax=Math.max(10,d*ymax,m), aw=r=>r<=0?0:4+26*Math.min(1,r/rateMax);
    const rr=(x,y,ww,hh,r)=>{ ctx.beginPath(); ctx.moveTo(x+r,y); ctx.arcTo(x+ww,y,x+ww,y+hh,r); ctx.arcTo(x+ww,y+hh,x,y+hh,r); ctx.arcTo(x,y+hh,x,y,r); ctx.arcTo(x,y,x+ww,y,r); ctx.closePath(); };
    // box: shade = density of deer (continuous, uniform = well-mixed)
    const dens=Math.max(0,Math.min(1,H/ymax));
    rr(bx,by,bw,bh,rad); ctx.save(); ctx.clip();
    ctx.fillStyle=col('card'); ctx.fillRect(bx,by,bw,bh);
    ctx.globalAlpha=0.08+0.55*dens; ctx.fillStyle=A; ctx.fillRect(bx,by,bw,bh);
    ctx.globalAlpha=0.22; ctx.fillStyle=fg;
    for(const [px,py,s] of pines){ const x=bx+px*bw, y=by+py*bh, k=Math.min(bw,bh)*0.11*s;
      ctx.beginPath(); ctx.moveTo(x,y-k*1.6); ctx.lineTo(x+k*0.7,y); ctx.lineTo(x-k*0.7,y); ctx.closePath(); ctx.fill(); ctx.fillRect(x-k*0.09,y,k*0.18,k*0.3); }
    ctx.restore();
    rr(bx,by,bw,bh,rad); ctx.strokeStyle=col('axis'); ctx.lineWidth=2; ctx.stroke();
    // labels in box
    ctx.textAlign='center'; ctx.fillStyle=mu; ctx.font='12px '+FONT.replace(/^\S+\s/,''); ctx.textBaseline='top';
    ctx.fillText('bos · goed gemengd',bx+bw/2,by+8);
    ctx.fillStyle=fg; ctx.textBaseline='middle'; ctx.font='600 '+Math.round(Math.min(30,bw/6))+'px '+FONT.replace(/^\S+\s/,'');
    drawMath(ctx,'H = '+H.toFixed(1),bx+bw/2,cy-6);
    ctx.font='12px '+FONT.replace(/^\S+\s/,''); ctx.fillStyle=mu; ctx.fillText('herten (dichtheid)',bx+bw/2,cy+16);
    // arrows with moving chevrons, width ∝ rate
    const arrow=(x0,x1,rate,color,label,sub)=>{ const wd=aw(rate);
      ctx.save(); ctx.fillStyle=color;
      if(wd>0){ const head=Math.max(10,wd*0.9), xe=x1-head;
        ctx.globalAlpha=.35; ctx.fillRect(x0,cy-wd/2,xe-x0,wd);
        ctx.globalAlpha=.9; ctx.beginPath(); ctx.moveTo(xe,cy-wd/2-head*0.45); ctx.lineTo(x1,cy); ctx.lineTo(xe,cy+wd/2+head*0.45); ctx.closePath(); ctx.fill();
        ctx.save(); ctx.beginPath(); ctx.rect(x0,cy-wd/2,xe-x0,wd); ctx.clip();
        ctx.strokeStyle=color; ctx.globalAlpha=.95; ctx.lineWidth=2; const sp=Math.max(14,wd*0.9), c=wd*0.35;
        for(let x=x0-sp+((phase*40)%sp); x<xe+sp; x+=sp){ ctx.beginPath(); ctx.moveTo(x-c,cy-wd*0.32); ctx.lineTo(x,cy); ctx.lineTo(x-c,cy+wd*0.32); ctx.stroke(); }
        ctx.restore(); }
      ctx.globalAlpha=1; ctx.textAlign='center'; ctx.textBaseline='bottom'; ctx.font='600 12px '+FONT.replace(/^\S+\s/,'');
      const xm=(x0+x1)/2, top=cy-Math.max(wd,8)/2-Math.max(10,wd*0.9)*0.45-4;
      drawMath(ctx,label,xm,top-14); ctx.font='11px '+FONT.replace(/^\S+\s/,''); ctx.fillStyle=mu; ctx.fillText(sub,xm,top);
      ctx.restore(); };
    arrow(4,bx-4,m,A,`m = ${fmt(m)}/uur`,'migratie');
    arrow(bx+bw+4,w-4,out,O,`dH = ${fmt(out)}/uur`,'vertrek');
  }
  function drawPlot(){
    const f=frame(pc,{xmin:0,xmax:T,ymin:0,ymax,xlab:'tijd (uur)',ylab:'aantal herten H'}); lastF=f; const cx=f.ctx;
    for(const [t1,h1] of anchors){ const pts=[]; for(let i=0;i<=120;i++){ const tt=T*i/120, v=Hat(tt,t1,h1); pts.push([tt,v<0?NaN:v]); }
      poly(f,pts,col('blue'),1.5,null,.7); disc(cx,f.X(t1),f.Y(h1),3.5,col('blue')); }
    if(d>0&&m/d<=ymax){ const y=f.Y(m/d); cx.save(); cx.setLineDash([6,4]); cx.strokeStyle=col('muted'); cx.beginPath(); cx.moveTo(f.p.l,y); cx.lineTo(f.p.l+f.pw,y); cx.stroke(); cx.restore();
      cx.fillStyle=col('muted'); cx.textAlign='right'; cx.textBaseline='bottom'; drawMath(cx,'evenwicht m/d = '+fmt(m/d),f.p.l+f.pw-4,y-3); }
    const pred=[]; for(let i=0;i<=60;i++){ const tt=t+(T-t)*i/60; pred.push([tt,Hat(tt,t,H)]); }
    poly(f,pred,col('accent'),1.5,[4,4],.5);
    poly(f,hist,col('accent'),2.8);
    const s=m-d*H, w=6; cx.save(); cx.strokeStyle=col('fg'); cx.lineWidth=2.4;
    cx.beginPath(); cx.moveTo(f.X(t-w),f.Y(H-s*w)); cx.lineTo(f.X(t+w),f.Y(H+s*w)); cx.stroke(); cx.restore();
    disc(cx,f.X(t),f.Y(H),5,col('accent'),col('bg'),2);
    cx.restore();
  }
  function readout(){
    const out=d*H, net=m-out, sc=Math.max(m,out,Math.abs(net),0.5);
    $('#bar-in').style.width=(m/sc*100)+'%'; $('#bar-out').style.width=(out/sc*100)+'%';
    const bn=$('#bar-net'); bn.style.width=(Math.abs(net)/sc*100)+'%'; bn.style.background=col('muted');
    $('#v-in').textContent=fmt(m)+' /uur'; $('#v-out').textContent=fmt(out)+' /uur'; $('#v-net').textContent=(net>=0?'+':'')+fmt(net)+' /uur';
    let txt=`t = <b>${t.toFixed(1)}</b> uur · H = <b>${H.toFixed(1)}</b> herten · <span class="eqn">dH/dt = ${fmt(m)} − ${fmt(d)} × ${H.toFixed(1)} = ${(net>=0?'+':'')+fmt(net)}</span> herten per uur. `;
    if(d===0) txt+='Zonder vertrek is de helling overal gelijk aan m: de oplossing is een <b>rechte lijn</b>, H(t) = H₀ + mt.';
    else if(Math.abs(net)<0.05*Math.max(m,0.1)) txt+='Instroom ≈ uitstroom: de helling is bijna 0. We zitten bij het <b>evenwicht</b>.';
    else txt+= net>0?'Er komen meer herten aan dan er vertrekken, dus de grafiek stijgt. Hoe dichter bij het evenwicht, hoe vlakker.':'Er vertrekken meer herten dan er aankomen, dus de grafiek daalt.';
    $('#h-text').innerHTML=txt;
  }
  function frameFn(dt){ sim(dt); drawTank(); drawPlot(); readout(); }
  return {frame:frameFn};
})();

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
   EENHEDEN
   ========================================================= */
const eenheden=(()=>{
  const F=(a,b)=>`<span class="frac"><span>${a}</span><span>${b}</span></span>`;
  const HU=F('herten','uur');
  const steps=[
    {text:'Een ODE beschrijft geen aantal, maar een <b>snelheid</b>. Welke eenheid hoort bij elke term? De vuistregel: <b>"per" betekent "gedeeld door"</b>. "Herten per uur" schrijf je dus als herten/uur.', u:{}, hl:[]},
    {text:'Links staat de verandering in het aantal herten <b>per uur</b>. De eenheid van dH/dt is dus herten/uur.', u:{lhs:HU}, hl:['lhs']},
    {text:'Je kunt geen appels en peren optellen of aftrekken: 🍎 − 🍐 is onzin. <b>Elke term</b> aan de rechterkant moet dus óók de eenheid herten/uur hebben. Alle drie de termen hebben nu dezelfde eenheid.', u:{lhs:HU,m:HU,dH:HU}, hl:['m','dH'], bal:true},
    {text:'Daarmee weet je direct de eenheid van <b>m</b>: herten per uur. Dat is het aantal herten dat elk uur het bos in loopt. Zo weet je ook wat je in het veld moet meten.', u:{lhs:HU,m:'<b>'+HU+'</b>',dH:HU}, hl:['m']},
    {text:'En <b>d</b>? We weten dat dH de eenheid herten/uur heeft. Los op voor d: deel beide kanten door herten. "Herten" valt weg en er blijft <b>1/uur</b> over.', u:{lhs:HU,m:HU,dH:HU}, hl:['dH'], derive:[
      `[d] · herten = ${HU}`,
      `[d] = ${F('herten','uur')} ÷ herten = ${F('<span class="cancel" style="animation-delay:1.4s">herten</span>','uur · <span class="cancel" style="animation-delay:1.4s">herten</span>')}`,
      `[d] = <b>${F('1','uur')}</b> &nbsp;= "per uur"`]},
    {text:'Omdat "per hert" is weggedeeld, is d een snelheid <b>per individu</b> (<i>per capita</i>). Bij d = 0.05 per uur vertrekt elk hert met snelheid 0.05 per uur. Bij 100 herten zijn dat 0.05 × 100 = <b>5 herten per uur</b>.', u:{lhs:HU,m:HU,dH:HU}, hl:['dH'], derive:[
      `${F('0.05','uur')} × 100 herten = 5 ${HU}`, '<span class="muted">✓ precies de eenheid die dH moet hebben</span>']},
    {text:'<b>Het recept</b> werkt voor elke ODE. (1) Bepaal de eenheid van dX/dt. (2) Elke term krijgt dezelfde eenheid. (3) Los op voor de parameter en streep weg wat in teller én noemer staat.', u:{lhs:HU,m:HU,dH:HU}, hl:['lhs','m','dH']}
  ];
  let i=0;
  const dots=$('#u-dots');
  steps.forEach((_,k)=>{ const e=document.createElement('i'); e.onclick=()=>{ i=k; render(); }; dots.appendChild(e); });
  function render(){
    const s=steps[i];
    for(const id of ['lhs','m','dH']){ const el=$('#u-'+id), u=el.querySelector('.unit');
      el.classList.toggle('hl',s.hl.includes(id));
      if(s.u[id]){ u.innerHTML=s.u[id]; u.classList.add('show'); } else { u.classList.remove('show'); u.innerHTML=''; }
      u.classList.toggle('same',!!s.bal); }
    const dv=$('#u-derive'); dv.innerHTML='';
    (s.derive||[]).forEach((l,k)=>{ const e=document.createElement('div'); e.className='ln'; e.style.animationDelay=(k*0.7)+'s'; e.innerHTML=l; dv.appendChild(e); });
    $('#u-text').innerHTML=s.text;
    [...dots.children].forEach((e,k)=>e.classList.toggle('on',k===i));
    const lastStep=i===steps.length-1; $('#u-prev').disabled=i===0; $('#u-next').disabled=lastStep; $('#u-next').textContent=lastStep?'✓ Klaar':'Volgende →';
  }
  $('#u-prev').onclick=()=>{ if(i>0){ i--; render(); } };
  $('#u-next').onclick=()=>{ if(i<steps.length-1){ i++; render(); } };
  render();

  /* quiz */
  const Q=[
    {q:`Logistische groei: <span class="eqn">dN/dt = rN(1 − N/K)</span>, met N in konijnen en t in jaren. Wat is de eenheid van <b>K</b>?`,
     o:['konijnen',F('konijnen','jaar'),F('1','jaar'),'geen eenheid'], a:0,
     e:'Je trekt N/K af van het getal 1, en 1 heeft geen eenheid. Dus N/K heeft ook geen eenheid. Dan moet K dezelfde eenheid hebben als N: konijnen.'},
    {q:`Zelfde model. Wat is de eenheid van <b>r</b>?`,
     o:['konijnen',F('konijnen','jaar'),F('1','jaar'),'jaar'], a:2,
     e:`Links staat ${F('konijnen','jaar')}. De haakjes hebben geen eenheid, dus rN = ${F('konijnen','jaar')}. Daaruit volgt r = ${F('1','jaar')}. Een eenheid "1 gedeeld door iets" lees je als <b>"per iets"</b>: ${F('1','jaar')} betekent dus <b>per jaar</b>. r is de groeisnelheid <i>per konijn</i>, per jaar.`},
    {q:`Een medicijn in het bloed: <span class="eqn">dC/dt = s − kC</span>, met C in mg/L en t in uren. Wat is de eenheid van <b>s</b>?`,
     o:[F('mg','L'),F('mg','L · uur'),F('1','uur'),'mg'], a:1,
     e:`s moet dezelfde eenheid hebben als dC/dt: (mg/L) per uur = ${F('mg','L · uur')}.`},
    {q:`Zelfde model. Wat is de eenheid van <b>k</b>?`,
     o:[F('mg','L · uur'),F('1','uur'),'uur',F('L','mg')], a:1,
     e:`kC = ${F('mg','L · uur')}. Deel door C (mg/L), dan blijft ${F('1','uur')} over: de afbraaksnelheid per molecuul.`},
    {q:`Een infectiemodel: <span class="eqn">dI/dt = βSI − γI</span>, met S en I in personen en t in dagen. Wat is de eenheid van <b>β</b>?`,
     o:[F('1','dag'),F('personen','dag'),F('1','persoon · dag'),'personen'], a:2,
     e:`βSI = ${F('personen','dag')}. Deel door SI (personen²), dan blijft ${F('1','persoon · dag')} over: de infectiesnelheid per contact tussen twee personen.`}
  ];
  const box=$('#quiz'); let score=0, answered=0;
  Q.forEach((it,qi)=>{
    box.appendChild(renderQuestion(it,qi+1,first=>{ answered++; if(first) score++; $('#quiz-score').textContent=`Score: ${score} van ${answered} in één keer goed`; }));
  });
  return {};
})();

/* =========================================================
   QUIZ TAB
   ========================================================= */
const quiz=(()=>{
  const F=(a,b)=>`<span class="frac"><span>${a}</span><span>${b}</span></span>`;
  const BASE=[
    {q:'Wat beschrijft een differentiaalvergelijking (ODE)?',
     o:['De populatiegrootte op het volgende tijdstip','De snelheid waarmee een variabele verandert','De evenwichtswaarde van een populatie','De kans dat een individu sterft'], a:1,
     h:'Denk aan het hertenfeestje: wat staat er links van het =-teken?',
     e:'Een ODE zoals <span class="eqn">dH/dt = m</span> zegt hoe snel H verandert, oftewel de helling van de grafiek.'},
    {q:'Waarom gaf de logistische <i>differentie</i>vergelijking met een grote groeisnelheid chaotische schommelingen?',
     o:['Er zit toeval in het model','De tijdstap was te groot','Het draagvermogen K was te klein','Door afrondingsfouten in R'], a:1,
     h:'Wat veranderde er toen we elke stap als een half jaar interpreteerden?',
     e:'Met grote stappen schiet de populatie over het evenwicht heen. Kleinere stappen, of een ODE met oneindig kleine stappen, geven een soepele S-curve.'},
    {q:'Deterministische chaos betekent dat…',
     o:['het model willekeurige getallen gebruikt','de uitkomst zeer gevoelig is voor de beginwaarde','de populatie altijd uitsterft','het model fout geprogrammeerd is'], a:1,
     h:'Wat gebeurde er toen we met 24, 25 of 26 konijnen begonnen?',
     e:'Er zit geen toeval in, maar een minuscuul verschil in de beginwaarde groeit uit tot een totaal andere uitkomst (het <i>butterfly effect</i>).'},
    {q:'Bij welke waarden van r groeit de populatie in de ODE <span class="eqn">dN/dt = rN</span>?',
     o:['r > 1','r > 0','r > 2','Bij elke waarde van r'], a:1,
     h:'Een ODE beschrijft een snelheid. Wanneer is die snelheid positief?',
     e:'De groeisnelheid rN is positief zodra r > 0. Bij de differentievergelijking <span class="eqn">N(t+1) = rN(t)</span> moest r juist groter zijn dan 1.'},
    {q:'Wat is de oplossing van een ODE?',
     o:['Een getal','Een functie van de tijd','Een parameter','Altijd het evenwicht'], a:1,
     h:'Bij <span class="eqn">6x = 3</span> is de onbekende een getal. Wat is de onbekende in <span class="eqn">dH/dt = m</span>?',
     e:'De onbekende is de functie H(t). Elke beginwaarde geeft een andere specifieke oplossing.'},
    {q:'Hoe ziet de grafiek van H(t) eruit als <span class="eqn">dH/dt = m</span> (met m &gt; 0)?',
     o:['Een stijgende rechte lijn','Een exponentiële curve','Een S-curve','Een horizontale lijn'], a:0,
     h:'De helling is op elk moment precies gelijk aan m.',
     e:'Een constante helling geeft een rechte lijn: H(t) = H₀ + mt.'},
    {q:`Een schimmelkolonie groeit volgens <span class="eqn">dA/dt = gA</span>, met A in mm² en t in dagen. Wat is de eenheid van <b>g</b>?`,
     o:[F('mm²','dag'),F('1','dag'),'mm²','dagen'], a:1,
     h:`Links staat ${F('mm²','dag')}. Deel gA door A.`,
     e:`gA heeft de eenheid ${F('mm²','dag')}. Deel je door A (mm²), dan blijft ${F('1','dag')} over: een groeisnelheid per mm².`},
    {q:'Mag je in <span class="eqn">dH/dt = m − dH</span> de d van "dH/dt" tegen de parameter d wegdelen?',
     o:['Ja, het is dezelfde letter','Nee, het zijn verschillende dingen','Alleen als d klein is','Alleen in het evenwicht'], a:1,
     h:'Waar staat de d in dH/dt voor?',
     e:'De d in dH/dt betekent "differentiaal" (verandering). De parameter d staat bijvoorbeeld voor <i>depart</i>. Ze hebben niets met elkaar te maken.'},
    {q:'Welke van deze is <b>géén</b> aanname van een ODE-model?',
     o:['Populaties zijn continu','Alle individuen zijn gelijk','De populatie is goed gemengd','Ruimtelijke patronen bepalen de dynamiek'], a:3,
     h:'Denk aan de <i>well-mixed assumption</i>.',
     e:'ODEs nemen juist aan dat er geen ruimtelijke structuur is: elk individu kan elk ander tegenkomen.'}
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
    const msg=frac===1?'Perfect! Je beheerst de basis van ODEs.':frac>=.75?'Heel goed! Kijk nog even naar de vragen die je niet in één keer goed had.':frac>=.5?'Op de goede weg. Bekijk de animaties in de andere tabbladen nog eens.':'Lees het hoofdstuk nog eens door en speel met de animaties. Daarna gaat het vast beter!';
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
const modules={overzicht:hero,tijdstappen:tijd,herten:herten,eenheden:eenheden,quiz:quiz};
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
function init(){ document.querySelectorAll('[data-ode-app]').forEach(mount); }
window.OdeApp={mount,init};
const H0=inlineHost(CS); if(H0) mount(H0);
if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',init); else init();
})();