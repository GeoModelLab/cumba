// CUMBA language switching
// cumbaSwitchLang(lang) -- immediate client-side translation, NO server round-trip.
// Called directly from onclick on all 6 lang buttons.
// The server also receives input$language via Shiny.setInputValue for LLM use.

var _cumbaLang = 'en';

window.cumbaSwitchLang = function(lang) {
  _cumbaLang = lang;
  var isIT = (lang === 'it');
  var isFG = (lang === 'foggiano');

  // 1. Button active states (hero + toolbar)
  var langMap = {
    langBtnEN: 'en', langBtnIT: 'it', langBtnFG: 'foggiano',
    heroLangEN: 'en', heroLangIT: 'it', heroLangFG: 'foggiano'
  };
  Object.keys(langMap).forEach(function(id){
    var btn = document.getElementById(id);
    if (!btn) return;
    if (langMap[id] === lang) btn.classList.add('btn-lang-active');
    else btn.classList.remove('btn-lang-active');
  });

  // 2. Toolbar button labels
  var T = {
    fg: {
      strategyTitle:  '💧 Strategje irrigue',
      phaseVeg:       '🌱 Vegetativa',
      phaseRep:       '🌸 Riproduttiva',
      phaseRip:       '🍅 Maturazzione',
      wsLabel:        'Soglie stress (0=asciutte, 1=ott.)',
      turnLabel:      'Turne minime (juorne)',
      varietyLabel:   '🌱 Varietà',
      toggleMap:      '🗺 Cambie site',
      toggleStrategy: '⚙ Strategje irrigue',
      openAdvanced:   '🔧 Avanzate',
      openLearning:   '📚 Apprendimende',
      openLLM:        '<img src="cumba_avatar.png" style="width:18px;height:18px;object-fit:contain;vertical-align:middle;margin-right:4px;"> CUMBA spieje',
      changeSite:     '↺ Cambie campe',
      toggleSchedule: '📋 Prugramme',
      geolocBtn:      '📍 Use a mije pussizzione',
      chatResetBtn:   '🔄 Conversazzione nove',
      chatSendBtn:    '🍅',
      autoLLMLabel:   'Aggiornamente automatiche',
      llmKeyLabel:    '🔑 Chiave LLM (facoltative)',
      chatPlaceholder: "Chiede a CUMBA: 'e s'aspette 3 juorne?', 'peccé cale u Brix?'...",
      Tcard: 'T card. (°C)', Topt: 'T ott (°C)', Tstress: 'T stress (°C)', RUE: 'RUE (g/MJ)',
      CycleLength: 'Cicle (°C·d)', LightInterception: 'Int. luce', TranFlowLag: 'Lag tras/fior %',
      GrowSenescence: 'Cresce→Sen %', FloweringSlope: 'Pendenze fior.', FloweringMax: 'Max fior.',
      Kc: 'Kc', RootIncrease: 'Aum. radiche', RootDepth: 'Prof. radiche (cm)',
      WSSensitivity: 'Sens. stresse idrich', DepletionFraction: 'Esaur. (% AWC)', SoilWaterInitial: 'Acque iniziale (%AWC)',
      k0: 'k0 zucchre', FruitWater: 'Acque frutto', FruitWaterIncrease: 'Aum. acque',
      FruitWaterDecMax: 'Dim. acque max',
      advGrowth: 'Crescite', advPheno: 'Fenologije e chiome', advWater: 'Acque e terrène', advFruit: 'Qualità du frutto'
    },
    en: {
      strategyTitle:  '💧 Irrigation strategy',
      phaseVeg:       '🌱 Vegetative',
      phaseRep:       '🌸 Reproductive',
      phaseRip:       '🍅 Ripening',
      wsLabel:        'Stress threshold (0=dry, 1=opt.)',
      turnLabel:      'Min. interval (days)',
      varietyLabel:   '🌱 Variety',
      toggleMap:      '🗺 Change site',
      toggleStrategy: '⚙ Irrigation strategy',
      openAdvanced:   '🔧 Advanced',
      openLearning:   '📚 Learning',
      openLLM:        '<img src="cumba_avatar.png" style="width:18px;height:18px;object-fit:contain;vertical-align:middle;margin-right:4px;"> CUMBA explains',
      changeSite:     '↺ Change field',
      toggleSchedule: '📋 Schedule',
      geolocBtn:      '📍 Use my location',
      chatResetBtn:   '🔄 New conversation',
      chatSendBtn:    '🍅',
      autoLLMLabel:   'Auto-update summary',
      llmKeyLabel:    '🔑 LLM key (optional)',
      chatPlaceholder: "Ask CUMBA: 'what if I wait 3 days?', 'why is Brix dropping?'...",
      Tcard: 'T card. (°C)', Topt: 'T opt (°C)', Tstress: 'T stress (°C)', RUE: 'RUE (g/MJ)',
      CycleLength: 'Cycle (°C·d)', LightInterception: 'Light int.', TranFlowLag: 'Tran/Flow lag %',
      GrowSenescence: 'Grow→Sen %', FloweringSlope: 'Flow. slope', FloweringMax: 'Flow. max',
      Kc: 'Kc', RootIncrease: 'Root inc.', RootDepth: 'Root depth (cm)',
      WSSensitivity: 'WS sens.', DepletionFraction: 'Depletion (% AWC)', SoilWaterInitial: 'Init. soil W (%AWC)',
      k0: 'k0 sugar', FruitWater: 'Fruit water', FruitWaterIncrease: 'Water inc.',
      FruitWaterDecMax: 'Water dec. max',
      advGrowth: 'Growth', advPheno: 'Phenology & canopy', advWater: 'Water & soil', advFruit: 'Fruit quality'
    },
    it: {
      strategyTitle:  '💧 Strategia irrigua',
      phaseVeg:       '🌱 Vegetativa',
      phaseRep:       '🌸 Riproduttiva',
      phaseRip:       '🍅 Maturazione',
      wsLabel:        'Soglia stress (0=secco, 1=opt.)',
      turnLabel:      'Turno minimo (giorni)',
      varietyLabel:   '🌱 Varietà',
      toggleMap:      '🗺 Cambia sito',
      toggleStrategy: '⚙ Strategia irrigua',
      openAdvanced:   '🔧 Avanzate',
      openLearning:   '📚 Apprendimento',
      openLLM:        '<img src="cumba_avatar.png" style="width:18px;height:18px;object-fit:contain;vertical-align:middle;margin-right:4px;"> CUMBA spiega',
      changeSite:     '↺ Cambia campo',
      toggleSchedule: '📋 Programma',
      geolocBtn:      '📍 Usa la mia posizione',
      chatResetBtn:   '🔄 Nuova conversazione',
      chatSendBtn:    '🍅',
      autoLLMLabel:   'Aggiornamento automatico',
      llmKeyLabel:    '🔑 Chiave LLM (opzionale)',
      chatPlaceholder: "Chiedi a CUMBA: 'e se aspetto 3 giorni?', 'perché cala il Brix?'...",
      Tcard: 'T card. (°C)', Topt: 'T ott (°C)', Tstress: 'T stress (°C)', RUE: 'RUE (g/MJ)',
      CycleLength: 'Ciclo (°C·d)', LightInterception: 'Int. luce', TranFlowLag: 'Lag tras/fior %',
      GrowSenescence: 'Cresc→Sen %', FloweringSlope: 'Pendenza fior.', FloweringMax: 'Max fior.',
      Kc: 'Kc', RootIncrease: 'Aum. radici', RootDepth: 'Prof. radici (cm)',
      WSSensitivity: 'Sens. stress idrico', DepletionFraction: 'Esaur. (% AWC)', SoilWaterInitial: 'Acqua iniziale (%AWC)',
      k0: 'k0 zucchero', FruitWater: 'Acqua frutto', FruitWaterIncrease: 'Aum. acqua',
      FruitWaterDecMax: 'Dim. acqua max',
      advGrowth: 'Crescita', advPheno: 'Fenologia e chioma', advWater: 'Acqua e suolo', advFruit: 'Qualità del frutto'
    }
  };
  var t = isFG ? T.fg : (isIT ? T.it : T.en);

  // Buttons with innerHTML (contain emoji)
  ['toggleMap','toggleStrategy','openAdvanced','openLearning','openLLM',
   'changeSite','toggleSchedule','chatResetBtn','chatSendBtn','geolocBtn'].forEach(function(id){
    var el = document.getElementById(id);
    if (el && t[id]) el.innerHTML = t[id];
  });

  // 3. Hero text elements
  var heroT = {
    en: {
      'hero-headline': "Hi, I'm CUMBA",
      'hero-sub': "Your agronomic advisor for processing tomato. I'll tell you when to irrigate, how your field is doing, and what to expect compared to past seasons.",
      'hero-cta': 'Tell me where your field is:',
      'hero-or':  'or',
      'hero-hint': '&#128071; click a point on the map'
    },
    it: {
      'hero-headline': 'Ciao, sono CUMBA',
      'hero-sub': 'Il tuo assistente agronomico per il pomodoro da industria. Ti dirò quando irrigare, come sta il tuo campo, e cosa aspettarti rispetto alle stagioni passate.',
      'hero-cta': 'Dimmi dove si trova il tuo campo:',
      'hero-or':  'oppure',
      'hero-hint': '&#128071; clicca un punto sulla mappa'
    },
    fg: {
      'hero-headline': 'Ciao, so CUMBA',
      'hero-sub': "So l'assistente agrunomiche pe u pommidore da industrie. T'adico io quanne irrigà, cumme sta u campe tuoje, e ch'aspettà rispette a le stagione passate.",
      'hero-cta': "Diemme addò sta u campe tuoje:",
      'hero-or':  'o vuère',
      'hero-hint': '&#128071; clicche nu punto d\'a mappa'
    }
  };
  var ht = isFG ? heroT.fg : (isIT ? heroT.it : heroT.en);
  Object.keys(ht).forEach(function(id){
    var el = document.getElementById(id);
    if (el && id !== 'hero-hint') el.textContent = ht[id];
  });
  var hintEl = document.getElementById('hero-hint');
  if (hintEl) hintEl.innerHTML = ht['hero-hint'];

  // 4. Checkbox label for autoLLM
  var cbLabel = document.querySelector('label[for=autoLLM]');
  if (cbLabel) cbLabel.textContent = t.autoLLMLabel;

  // 5. LLM key summary
  var llmSummary = document.querySelector('.llm-modal-controls details summary');
  if (llmSummary) llmSummary.textContent = t.llmKeyLabel;

  // 6. Slider labels for Advanced tab
  var sliderKeyMap = {
    'TGro': 'Tcard', 'Topt': 'Topt', 'TStress': 'Tstress', 'RUE': 'RUE',
    'CycleLength': 'CycleLength', 'LightInterception': 'LightInterception',
    'TransFloLag': 'TranFlowLag', 'GrowthSenescenceCanopy': 'GrowSenescence',
    'FloweringSlope': 'FloweringSlope', 'FloweringMax': 'FloweringMax',
    'Kc': 'Kc', 'RootIncrease': 'RootIncrease', 'RootDepth': 'RootDepth',
    'WaterStressSensitivity': 'WSSensitivity', 'DepletionFraction': 'DepletionFraction',
    'SoilWaterInitial': 'SoilWaterInitial', 'k0': 'k0',
    'FruitWaterContent': 'FruitWater', 'FruitWaterContentInc': 'FruitWaterIncrease',
    'FruitWaterContentDecreaseMax': 'FruitWaterDecMax'
  };
  Object.keys(sliderKeyMap).forEach(function(id) {
    var tKey = sliderKeyMap[id];
    if (!t[tKey]) return;
    var lbl = document.querySelector('label[for=' + id + ']');
    if (lbl) lbl.textContent = t[tKey];
  });
  ['advGrowth','advPheno','advWater','advFruit'].forEach(function(id) {
    var el = document.getElementById(id);
    if (el && t[id]) el.textContent = t[id];
  });

  // 7. Chat textarea placeholder
  var ta = document.getElementById('chatInput');
  if (ta) ta.placeholder = t.chatPlaceholder;

  // 8. LLM modal title
  var llmTitle = document.getElementById('llm-title-text');
  if (llmTitle) llmTitle.textContent = isFG ? 'CUMBA spieje' : (isIT ? 'CUMBA spiega' : 'CUMBA explains');

  // 9. Learning modal translatable strings
  var learningIntro = document.getElementById('learning-intro-text');
  if (learningIntro) {
    var itLearn = '<strong>Modalità apprendimento</strong>: cambia un parametro del modello, ' +
      'riesegui la simulazione, e vedi come cambiano resa, Brix e irrigazione. CUMBA spiega il perché.';
    var enLearn = '<strong>Learning mode</strong>: change one model parameter, ' +
      're-run the simulation, and see how yield, Brix and irrigation change. CUMBA explains why.';
    learningIntro.innerHTML = (isIT || isFG) ? itLearn : enLearn;  }
  var learnBtn = document.getElementById('learningRecalc');
  if (learnBtn) learnBtn.innerHTML = (isIT || isFG) ? '&#9654; Esegui' : '&#9654; Run';
  var learnParamLbl = document.querySelector('label[for=learningParam]');
  if (learnParamLbl) learnParamLbl.textContent = (isIT || isFG) ? 'Parametro' : 'Parameter';

  // 10. Schedule panel
  var irrTitle = document.getElementById('irrFeedbackTitle');
  if (irrTitle) irrTitle.innerHTML = isIT || isFG
    ? '💧 Le tue scelte di irrigazione'
    : '💧 Your irrigation schedule';

  var freezeLbl = document.getElementById('freezeLabel');
  if (freezeLbl) freezeLbl.textContent = isIT || isFG
    ? 'Solo le mie scelte (no consigli automatici)'
    : 'Only my choices (no auto suggestions)';

  var recalcLbl = document.getElementById('recalcLabel');
  if (recalcLbl) recalcLbl.innerHTML = isIT || isFG
    ? '🔄 Ricalcola con le regole della strategia'
    : '🔄 Recalculate with strategy rules';

  var irrHelp = document.getElementById('irrHelpSummary');
  if (irrHelp) irrHelp.innerHTML = isIT || isFG ? 'ℹ️ Come funziona' : 'ℹ️ How to use';

  var irrHelpTxt = document.getElementById('irrHelpText');
  if (irrHelpTxt) irrHelpTxt.innerHTML = isIT || isFG
    ? 'Aggiungi eventi di irrigazione (data + mm). Il modello si aggiorna automaticamente. Usa <em>Freeze</em> per bloccare le tue scelte e vedere l\'impatto puro senza rifill automatici. Salva/carica il calendario come CSV.'
    : 'Add irrigation events (date + mm). The model re-runs automatically. Use <em>Freeze</em> to lock your choices and compare impact without automatic refills. Save/load your schedule as CSV.';

  // 11. Strategy sidebar
  var stratTitle = document.getElementById('strategy-side-title');
  if (stratTitle) stratTitle.textContent = t.strategyTitle;

  var phaseMap = { 'phase-hdr-veg': 'phaseVeg', 'phase-hdr-rep': 'phaseRep', 'phase-hdr-rip': 'phaseRip' };
  Object.keys(phaseMap).forEach(function(id) {
    var el = document.getElementById(id);
    if (el) el.textContent = t[phaseMap[id]];
  });

  ['ws_veg','ws_rep','ws_rip'].forEach(function(id) {
    var lbl = document.querySelector('label[for=' + id + ']');
    if (lbl) lbl.textContent = t.wsLabel;
  });

  ['turn-lbl-veg','turn-lbl-rep','turn-lbl-rip'].forEach(function(id) {
    var el = document.getElementById(id);
    if (el) el.textContent = t.turnLabel;
  });

  var varLbl = document.getElementById('strategy-variety-label');
  if (varLbl) varLbl.textContent = t.varietyLabel;
};

// Server-driven handlers (for server-side language changes)
Shiny.addCustomMessageHandler('cumba_lang_active', function(payload){
  window.cumbaSwitchLang(payload.lang || 'en');
});

Shiny.addCustomMessageHandler('cumba_set_lang', function(payload) {
  var lang = payload.lang || 'en';
  Shiny.setInputValue('language', lang, {priority: 'event'});
  window.cumbaSwitchLang(lang);
});

// Force English on session connect
$(document).on('shiny:connected', function() {
  window.cumbaSwitchLang('en');
  Shiny.setInputValue('language', 'en', {priority: 'event'});
  setTimeout(function() {
    Shiny.setInputValue('language', 'en', {priority: 'event'});
  }, 300);
});

// Enter key in chatInput -> click send button
// Must flush the textarea value to Shiny BEFORE the button click,
// otherwise input$chatInput on the server may have a stale (truncated) value.
document.addEventListener('keydown', function(ev){
  if (ev.target && ev.target.id === 'chatInput' &&
      ev.key === 'Enter' && !ev.shiftKey) {
    ev.preventDefault();
    var ta = ev.target;
    // Trigger change events so Shiny's binding sends the current value
    ta.dispatchEvent(new Event('input',  {bubbles: true}));
    ta.dispatchEvent(new Event('change', {bubbles: true}));
    // Give Shiny ~60ms to receive the value before the button fires
    setTimeout(function() {
      var btn = document.getElementById('chatSendBtn');
      if (btn) btn.click();
    }, 60);
  }
});

// Auto-scroll chat body when new messages arrive
var _cumbaScrollObs = null;
function _cumbaMountScrollObs() {
  var body = document.querySelector('#llmModal .chat-body');
  if (!body || _cumbaScrollObs) return;
  _cumbaScrollObs = new MutationObserver(function(){
    body.scrollTop = body.scrollHeight;
  });
  _cumbaScrollObs.observe(body, {childList: true, subtree: true});
}
document.addEventListener('DOMContentLoaded', _cumbaMountScrollObs);
document.addEventListener('click', function(ev){
  if (ev.target && ev.target.closest && ev.target.closest('#openLLM'))
    setTimeout(_cumbaMountScrollObs, 200);
});

// freezeMode: hide recalcWithRules when 'My choices only' is ON
Shiny.addCustomMessageHandler('cumba_freeze_mode', function(payload){
  var btn = document.getElementById('recalcWithRules');
  if (!btn) return;
  btn.style.display = payload.on ? 'none' : '';
});
