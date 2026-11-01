# shinyApp/varieties.R --------------------------------------------------------
# Variety catalog for processing tomato.
# Each entry provides default CycleLength (GDD, base 10°C), RUE (g/MJ),
# and k0 (sugar coefficient / Brix proxy).
#
# GDD estimates assume ~12 effective °C/day (field avg ~22°C, base 10°C):
#   Precocissimo  85-90 d  → 1020-1080 °C·d
#   Precoce       90-95 d  → 1080-1140 °C·d
#   Medio-precoce 95-100d  → 1140-1200 °C·d
#   Medio        100-105d  → 1200-1260 °C·d
#   Medio-tardivo 105-115d → 1260-1380 °C·d
#   Tardivo       >115 d   → 1380+     °C·d
#
# k0 (Brix proxy): generic varieties use the calibrated package value
# (cumbaParameters$k0); varieties described as "alto Brix" or "Brix
# nettamente superiore" get 4.3-4.6.
# RUE (g/MJ): no per-variety data — all varieties use the calibrated package
# value (cumbaParameters$RUE); may be adjusted once trial data are available.
# ----------------------------------------------------------------------------

.RUE_CAL <- cumba::cumbaParameters$RUE$value
.K0_CAL  <- cumba::cumbaParameters$k0$value
.CL_DEF  <- cumba::cumbaParameters$CycleLength$value

CUMBA_VARIETIES <- list(

  # --- Placeholder (manual mode) -------------------------------------------
  list(id = "custom",      name = "── Custom (manual sliders) ──",
       company = "",        cycle_class = "",
       cycleLength = .CL_DEF,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Use the Advanced tab to set parameters manually."),

  # --- Seminis / Bayer -------------------------------------------------------
  list(id = "docet",       name = "Docet (Seminis)",
       company = "Seminis", cycle_class = "Medio-precoce",
       cycleLength = 1170,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio-precoce 95-100 d. Good production, standard Brix."),

  list(id = "sv5197tp",    name = "SV5197TP (Seminis)",
       company = "Seminis", cycle_class = "Medio-precoce",
       cycleLength = 1170,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio-precoce 95-100 d."),

  list(id = "eventus",     name = "Eventus (Seminis)",
       company = "Seminis", cycle_class = "Medio-precoce",
       cycleLength = 1170,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio-precoce 95-100 d."),

  list(id = "incipit",     name = "Incipit (Seminis)",
       company = "Seminis", cycle_class = "Precoce",
       cycleLength = 1110,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Precoce 90-95 d (corto)."),

  list(id = "sv8840tm",    name = "SV8840TM (Seminis)",
       company = "Seminis", cycle_class = "Medio",
       cycleLength = 1200,  RUE = .RUE_CAL, k0 = 4.3,
       description = "Medio 95-100 d. Buon Brix."),

  list(id = "perfectpeel", name = "PerfectPeel (Seminis)",
       company = "Seminis", cycle_class = "Medio",
       cycleLength = 1200,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio 100 d. Pelatura facilitata."),

  list(id = "barrick",     name = "Barrick (Seminis)",
       company = "Seminis", cycle_class = "Medio",
       cycleLength = 1230,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio 105 d."),

  # --- Nunhems / BASF --------------------------------------------------------
  list(id = "n6438",       name = "N6438 (Nunhems)",
       company = "Nunhems", cycle_class = "Precocissimo",
       cycleLength = 1050,  RUE = 2.7, k0 = .K0_CAL,
       description = "Precocissimo ~85-90 d. Raccolta anticipata."),

  list(id = "n4523",       name = "N4523 (Nunhems)",
       company = "Nunhems", cycle_class = "Medio-precoce",
       cycleLength = 1170,  RUE = .RUE_CAL, k0 = 4.5,
       description = "Medio-precoce. Alto Brix."),

  list(id = "n4510",       name = "N4510 (Nunhems)",
       company = "Nunhems", cycle_class = "Medio",
       cycleLength = 1210,  RUE = .RUE_CAL, k0 = 4.6,
       description = "Medio. Brix nettamente superiore alla media."),

  list(id = "n507",        name = "N507 (Nunhems)",
       company = "Nunhems", cycle_class = "Medio",
       cycleLength = 1210,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio. Buona produzione."),

  list(id = "delfo",       name = "Delfo (Nunhems)",
       company = "Nunhems", cycle_class = "Medio",
       cycleLength = 1210,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio. Ampia adattabilita'."),

  list(id = "fokker",      name = "Fokker (Nunhems)",
       company = "Nunhems", cycle_class = "Tardivo",
       cycleLength = 1380,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Tardivo >115 d. Alta resa."),

  # --- Syngenta --------------------------------------------------------------
  list(id = "tolerix",     name = "Tolerix (Syngenta)",
       company = "Syngenta", cycle_class = "Medio-precoce",
       cycleLength = 1140,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio-precoce 90-95 d (PRCSQ2220). Buona tolleranza."),

  list(id = "miceno",      name = "Miceno (Syngenta)",
       company = "Syngenta", cycle_class = "Medio-precoce",
       cycleLength = 1170,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio-precoce / medio."),

  list(id = "redix",       name = "Redix (Syngenta)",
       company = "Syngenta", cycle_class = "Medio-precoce",
       cycleLength = 1170,  RUE = .RUE_CAL, k0 = 4.3,
       description = "Medio-precoce / medio. Buon Brix."),

  list(id = "waller",      name = "Waller (Syngenta)",
       company = "Syngenta", cycle_class = "Medio",
       cycleLength = 1210,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio. Standard."),

  list(id = "firmus",      name = "Firmus (Syngenta)",
       company = "Syngenta", cycle_class = "Medio-tardivo",
       cycleLength = 1320,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio-tardivo / tardivo 105-115 d."),

  list(id = "bq400",       name = "BQ400 (Syngenta)",
       company = "Syngenta", cycle_class = "Medio",
       cycleLength = 1210,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Medio."),

  list(id = "ercole",      name = "Ercole (Syngenta)",
       company = "Syngenta", cycle_class = "Precoce",
       cycleLength = 1110,  RUE = .RUE_CAL, k0 = .K0_CAL,
       description = "Precoce 90-95 d.")
)

# Named vector for selectInput choices (id -> display name)
.variety_choices <- function() {
  ids   <- vapply(CUMBA_VARIETIES, `[[`, character(1), "id")
  names <- vapply(CUMBA_VARIETIES, `[[`, character(1), "name")
  setNames(ids, names)
}

# Look up a variety by id; returns NULL if not found or id=="custom"
.variety_params <- function(id) {
  if (is.null(id) || !nzchar(id) || id == "custom") return(NULL)
  for (v in CUMBA_VARIETIES) if (identical(v$id, id)) return(v)
  NULL
}
