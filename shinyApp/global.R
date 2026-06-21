# shinyApp/global.R --------------------------------------------------------
# Loaded once at app start.

suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(leaflet.extras)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(httr2)
  library(jsonlite)
  library(lubridate)
  library(shinycssloaders)
})

# --- CUMBA loading --------------------------------------------------------
# Round 15: SEMPRE forza load_all (ricarica i sorgenti R/Main.R) cosi' le
# modifiche al modello (es. branch override) sono attive ad ogni runApp,
# senza serve re-install del pacchetto.
.pkg_root <- normalizePath("..", mustWork = FALSE)
.in_pkg <- file.exists(file.path(.pkg_root, "DESCRIPTION")) &&
           file.exists(file.path(.pkg_root, "R", "Main.R"))

if (.in_pkg && requireNamespace("devtools", quietly = TRUE)) {
  message("[global.R] Loading cumba via devtools::load_all('", .pkg_root, "') ",
          "— ricaricamento FORZATO dei sorgenti R/Main.R")
  # reset = TRUE forza re-caricamento di tutti i file R/, anche se gia'
  # caricati in una sessione precedente (es. dopo edit a Main.R).
  suppressMessages(devtools::load_all(.pkg_root, quiet = TRUE, reset = TRUE))
  # Verifica firma di cumba_scenario per essere sicuri che la versione
  # caricata abbia il parametro irrigationOverride (Round 5+).
  if (!"irrigationOverride" %in% names(formals(cumba::cumba_scenario))) {
    warning("[global.R] cumba_scenario NON ha il parametro irrigationOverride! ",
            "Versione vecchia caricata. Riavvia R completamente.")
  } else {
    message("[global.R] cumba_scenario versione OK (irrigationOverride supportato)")
  }
} else {
  loaded <- tryCatch(
    { suppressMessages(library(cumba)); TRUE },
    error = function(e) FALSE
  )
  if (!loaded)
    stop("Install the cumba package or run from a dev session with devtools.")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- Variety catalog ---------------------------------------------------------
source("varieties.R")

# --- i18n: English (default) / Italian ----------------------------------
# ui.R and server.R call tr(key) to get the translated string.
# The active language is controlled by input$language (already wired) and
# stored in .i18n_lang (session-scoped via set_lang()), with "en" as global
# default.  Only static labels live here; numeric outputs are always English.
.I18N <- list(
  en = list(
    # Site header
    site_soil_label   = "Soil:",
    site_season_label = "Season",
    # Strategy phases
    phase_veg   = "Vegetative",
    phase_rep   = "Reproductive",
    phase_rip   = "Ripening",
    ws_label    = "Water stress threshold",
    turn_label  = "Min. interval (days)",
    # Toolbar buttons
    btn_map      = "Map",
    btn_strategy = "Strategy",
    btn_learning = "Learning",
    btn_advanced = "Advanced",
    btn_change_site = "Change site",
    btn_run      = "Run",
    # KPI labels
    kpi_yield    = "Yield",
    kpi_brix     = "Brix",
    kpi_irr_mm   = "Irrigation (mm)",
    kpi_irr_n    = "Irrig. events",
    kpi_wue      = "WUE",
    # Soil
    soil_sandy   = "Sandy",
    soil_loam    = "Loam",
    soil_clay    = "Clay",
    soil_auto    = "Auto (SoilGrids)",
    # Today card
    today_label  = "TODAY",
    # Chatbot
    chat_placeholder = "Ask a question about your field...",
    chat_btn_send    = "Send",
    chat_btn_explain = "Explain",
    chat_auto        = "Auto",
    chat_status_ok   = "LLM",
    chat_status_fb   = "RULES",
    chat_status_load = "...",
    # Misc
    transplanting_label = "Transplanting",
    history_years_label = "Historical years",
    no_site_selected    = "Select a site on the map to start"
  ),
  it = list(
    site_soil_label   = "Suolo:",
    site_season_label = "Stagione",
    phase_veg   = "Vegetativa",
    phase_rep   = "Riproduttiva",
    phase_rip   = "Maturazione",
    ws_label    = "Soglia stress idrico",
    turn_label  = "Turno min. (giorni)",
    btn_map      = "Mappa",
    btn_strategy = "Strategia",
    btn_learning = "Learning",
    btn_advanced = "Avanzate",
    btn_change_site = "Cambia campo",
    btn_run      = "Esegui",
    kpi_yield    = "Resa",
    kpi_brix     = "Brix",
    kpi_irr_mm   = "Irrigazione (mm)",
    kpi_irr_n    = "Interventi irrig.",
    kpi_wue      = "WUE",
    soil_sandy   = "Sabbioso",
    soil_loam    = "Limoso",
    soil_clay    = "Argilloso",
    soil_auto    = "Auto (SoilGrids)",
    today_label  = "OGGI",
    chat_placeholder = "Fai una domanda sul tuo campo...",
    chat_btn_send    = "Invia",
    chat_btn_explain = "Spiega",
    chat_auto        = "Auto",
    chat_status_ok   = "LLM",
    chat_status_fb   = "REGOLE",
    chat_status_load = "...",
    transplanting_label = "Trapianto",
    history_years_label = "Anni storici",
    no_site_selected    = "Seleziona un sito sulla mappa per iniziare"
  )
)

# Session-scoped language helper.  Default = "en" (can be overridden per
# session by calling set_lang("it")).  tr() falls back gracefully.
.i18n_lang <- "en"
set_lang <- function(lang) {
  l <- if (lang %in% names(.I18N)) lang else "en"
  assign(".i18n_lang", l, envir = parent.env(environment()))
}
tr <- function(key, lang = .i18n_lang) {
  d <- .I18N[[lang %||% "en"]]
  if (is.null(d)) d <- .I18N[["en"]]
  v <- d[[key]]
  if (is.null(v)) .I18N[["en"]][[key]] %||% key else v
}

# --- Round 19: Pedotransfer tipo di suolo -> FieldCapacity / WiltingPoint --
# Valori volumetrici tipici (0..1) per i 3 tessiture principali (Saxton &
# Rawls 2006, valori medi). L'utente sceglie il tipo nell'header.
# SoilGrids auto-fetch rimosso: troppo instabile.
.SOIL_TYPES <- list(
  "sandy" = list(label = "Sandy",   FieldCapacity = 0.18, WiltingPoint = 0.08),
  "loam"  = list(label = "Loam",    FieldCapacity = 0.30, WiltingPoint = 0.13),
  "clay"  = list(label = "Clay",    FieldCapacity = 0.40, WiltingPoint = 0.22)
)
.SOIL_CHOICES <- setNames(names(.SOIL_TYPES),
                          vapply(.SOIL_TYPES, `[[`, character(1), "label"))

# --- SoilGrids API (ISRIC) — no API key required -------------------------
# Fetches volumetric water content at field capacity (wv0033, 33 kPa) and
# wilting point (wv1500, 1500 kPa) from the ISRIC SoilGrids REST API v2.0.
# Returns a list(FieldCapacity, WiltingPoint, source, clay_pct, sand_pct)
# or NULL on error.
#
# Depths averaged: 0-5, 5-15, 15-30, 30-60 cm (typical root zone).
# Units: SoilGrids stores wv0033/wv1500 as integers in units of
#   1/10 cm³/cm³ (i.e., divide by 1000 to get volumetric fraction 0-1).
# Fallback: if wv0033/wv1500 unavailable, applies Saxton & Rawls (2006)
#   PTF from clay/sand percentages.
#
# API reference: https://rest.isric.org/soilgrids/v2.0/properties/query
fetch_soil_soilgrids <- function(lon, lat, timeout_s = 20L) {
  base_url <- "https://rest.isric.org/soilgrids/v2.0/properties/query"

  # Depths to query and average (top-soil + sub-soil up to 60 cm)
  depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm")

  # Helper: extract mean for a given property × depth combination from layers
  extract_mean <- function(layers, prop, depth_label) {
    lay <- layers[vapply(layers, function(l) identical(l$name, prop), logical(1))]
    if (!length(lay)) return(NA_real_)
    lay <- lay[[1L]]
    d_factor <- lay$unit_measure$d_factor
    if (is.null(d_factor) || !is.finite(d_factor) || d_factor == 0) d_factor <- 10
    for (dep in lay$depths) {
      if (identical(dep$label, depth_label)) {
        v <- dep$values$mean
        if (!is.null(v) && is.finite(as.numeric(v)))
          return(as.numeric(v) / d_factor / 100)  # convert to volumetric fraction
      }
    }
    NA_real_
  }

  # Build request for wv0033, wv1500, clay, sand
  build_req <- function(props) {
    req <- httr2::request(base_url)
    req <- httr2::req_url_query(req,
      longitude = format(lon, nsmall = 5L, scientific = FALSE),
      lat       = format(lat, nsmall = 5L, scientific = FALSE)
    )
    # httr2 doesn't support repeated params natively; use raw query string append
    qs <- paste0("&property=", props, collapse = "")
    for (d in depths) qs <- paste0(qs, "&depth=", d)
    qs <- paste0(qs, "&value=mean")
    req$url <- paste0(req$url,
                      "?longitude=", format(lon, nsmall=5L, scientific=FALSE),
                      "&lat=", format(lat, nsmall=5L, scientific=FALSE),
                      qs)
    httr2::req_timeout(req, timeout_s) |>
      httr2::req_user_agent("CUMBA-Shiny (+https://github.com/tomatoModelling/cumba_R_package)") |>
      httr2::req_error(is_error = function(r) FALSE)
  }

  parsed <- tryCatch({
    # Single request for all needed properties
    url <- paste0(base_url,
                  "?longitude=", format(lon, nsmall=5L, scientific=FALSE),
                  "&lat=", format(lat, nsmall=5L, scientific=FALSE),
                  "&property=wv0033&property=wv1500&property=clay&property=sand",
                  "&depth=0-5cm&depth=5-15cm&depth=15-30cm&depth=30-60cm",
                  "&value=mean")
    resp <- httr2::request(url) |>
      httr2::req_timeout(timeout_s) |>
      httr2::req_user_agent("CUMBA-Shiny (+https://github.com/tomatoModelling/cumba_R_package)") |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) >= 400) {
      message(sprintf("[SoilGrids] HTTP %d", httr2::resp_status(resp)))
      return(NULL)
    }
    httr2::resp_body_json(resp)
  }, error = function(e) {
    message("[SoilGrids] fetch error: ", conditionMessage(e))
    NULL
  })

  if (is.null(parsed)) return(NULL)
  layers <- parsed$properties$layers
  if (is.null(layers) || !length(layers)) return(NULL)

  # --- Volumetric water content: average across depths ---
  avg_prop <- function(prop) {
    vals <- vapply(depths, function(d) extract_mean(layers, prop, d), numeric(1))
    vals <- vals[is.finite(vals)]
    if (!length(vals)) NA_real_ else mean(vals)
  }

  fc <- avg_prop("wv0033")
  wp <- avg_prop("wv1500")

  # Extract clay/sand for texture classification and PTF fallback
  clay_frac <- avg_prop("clay")   # in g/kg / 1000 ... need to re-check
  sand_frac <- avg_prop("sand")

  # Clay/sand from SoilGrids are in g/kg (0-1000), stored ×10 with d_factor=10
  # so extract_mean already divides by d_factor(10)/100 -> gives fraction 0-1
  # but g/kg needs /10 more to be %. Re-derive clay/sand as percentages.
  clay_pct <- clay_frac * 100   # fraction 0-1 → percentage 0-100
  sand_pct <- sand_frac * 100

  # Sanity check: FC and WP must be in [0.05, 0.65] and FC > WP
  valid_fc <- !is.na(fc) && fc >= 0.05 && fc <= 0.65
  valid_wp <- !is.na(wp) && wp >= 0.02 && wp <= 0.50
  valid_pair <- valid_fc && valid_wp && (fc > wp)

  if (valid_pair) {
    return(list(
      FieldCapacity = round(fc, 3),
      WiltingPoint  = round(wp, 3),
      clay_pct      = round(clay_pct, 1),
      sand_pct      = round(sand_pct, 1),
      source        = "SoilGrids"
    ))
  }

  # --- Fallback: Saxton & Rawls (2006) PTF from clay/sand ---
  if (!is.na(clay_pct) && !is.na(sand_pct) &&
      is.finite(clay_pct) && is.finite(sand_pct) &&
      clay_pct >= 0 && sand_pct >= 0) {
    cp <- clay_pct / 100; sp <- sand_pct / 100
    # Eq. 1 & 2, Saxton & Rawls 2006 (simplified for theta_FC at -33kPa and theta_WP at -1500kPa)
    a1 <- 0.299; b1 <- -0.251; c1 <- 0.195
    a2 <- 0.208; b2 <-  0.063; c2 <- 0.366
    fc_ptf <- max(0.10, min(0.55, a1 + b1*sp + c1*cp))
    wp_ptf <- max(0.04, min(0.35, a2 + b2*sp + c2*cp))
    if (fc_ptf > wp_ptf) {
      return(list(
        FieldCapacity = round(fc_ptf, 3),
        WiltingPoint  = round(wp_ptf, 3),
        clay_pct      = round(clay_pct, 1),
        sand_pct      = round(sand_pct, 1),
        source        = "SoilGrids+PTF"
      ))
    }
  }

  # Complete failure
  message("[SoilGrids] could not derive valid FC/WP for (", lon, ",", lat, ")")
  NULL
}

# --- LLM API key bootstrap -----------------------------------------------
# Priority order for LLM backend (first match wins):
#   1. GROQ_API_KEY    env var / llm_key.txt   → Groq (recommended, very
#      generous free tier, key does NOT get auto-revoked on shinyapps.io,
#      supports Llama-3.3-70B). Get a free key at https://console.groq.com
#   2. ANTHROPIC_API_KEY env var               → Anthropic Claude (paid)
#   3. OPENROUTER_API_KEY env var / openrouter_key.txt → OpenRouter (legacy)
#   4. User-supplied runtime key via input$user_llm_key (UI widget)
#
# If no key is found anywhere, the app falls back to synth_message() (the
# high-quality rule-based system) and shows a "RULES" badge.
#
# To activate on shinyapps.io FREE tier (no env vars):
#   writeLines("gsk_...", "shinyApp/llm_key.txt")   # Groq key
#   — llm_key.txt is in .gitignore so it's never committed to the repo.
#   Then re-deploy. global.R picks it up automatically.

# Detect provider from a raw key string; returns the backend name.
.llm_detect_provider <- function(k) {
  if (grepl("^AIza",    k, perl = TRUE)) "gemini"
  else if (grepl("^gsk_",    k, perl = TRUE)) "groq"
  else if (grepl("^sk-ant-", k, perl = TRUE)) "anthropic"
  else "openrouter"
}

.bootstrap_llm_key <- function() {
  # 1. Gemini env var (priority: generous free tier, cheap paid)
  if (nzchar(Sys.getenv("GOOGLE_API_KEY", ""))) {
    message("[LLM] Using Gemini API key from GOOGLE_API_KEY env var")
    return(invisible(TRUE))
  }

  # 2. Groq env var
  if (nzchar(Sys.getenv("GROQ_API_KEY", ""))) {
    message("[LLM] Using Groq API key from GROQ_API_KEY env var")
    return(invisible(TRUE))
  }

  # 2. Groq FILE key — checked BEFORE ANTHROPIC_API_KEY env so that an
  #    explicit llm_key.txt always wins over a zero-credit Anthropic key that
  #    happens to be set in the system environment.
  cwd <- tryCatch(normalizePath(getwd(), mustWork = FALSE), error = function(e) ".")
  script_dir <- tryCatch(
    dirname(normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)),
    error = function(e) cwd
  )
  if (is.null(script_dir) || !nzchar(script_dir)) script_dir <- cwd

  # Gemini file key
  gemini_files <- unique(c(
    file.path(script_dir, "gemini_key.txt"),
    file.path(cwd, "gemini_key.txt"),
    file.path(cwd, "shinyApp", "gemini_key.txt"),
    "gemini_key.txt",
    "shinyApp/gemini_key.txt"
  ))
  for (f in gemini_files) {
    if (file.exists(f)) {
      k <- trimws(readLines(f, warn = FALSE, n = 1L))
      if (nzchar(k)) {
        Sys.setenv(GOOGLE_API_KEY = k)
        message(sprintf("[LLM] GOOGLE_API_KEY loaded from %s", f))
        return(invisible(TRUE))
      }
    }
  }

  groq_files <- unique(c(
    file.path(script_dir, "llm_key.txt"),
    file.path(cwd, "llm_key.txt"),
    file.path(cwd, "shinyApp", "llm_key.txt"),
    "llm_key.txt",
    "shinyApp/llm_key.txt"
  ))
  message(sprintf("[LLM] searching GROQ_API_KEY in: %s",
                  paste(groq_files, collapse = " | ")))
  for (f in groq_files) {
    if (file.exists(f)) {
      k <- trimws(readLines(f, warn = FALSE, n = 1L))
      if (nzchar(k)) {
        Sys.setenv(GROQ_API_KEY = k)
        message(sprintf("[LLM] GROQ_API_KEY loaded from %s", f))
        return(invisible(TRUE))
      }
    }
  }

  # 3. Anthropic env var
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY", ""))) {
    message("[LLM] Using Anthropic API key from ANTHROPIC_API_KEY env var")
    return(invisible(TRUE))
  }
  # 4. OpenRouter env var
  if (nzchar(Sys.getenv("OPENROUTER_API_KEY", ""))) {
    message("[LLM] Using OpenRouter API key from OPENROUTER_API_KEY env var")
    return(invisible(TRUE))
  }
  # 5. CUMBA_APP_KEY — app-level default key set at deploy time (shinyapps.io
  #    env vars). Provider auto-detected from prefix: gsk_...=Groq,
  #    sk-ant-...=Anthropic, otherwise OpenRouter.
  app_key <- trimws(Sys.getenv("CUMBA_APP_KEY", ""))
  if (nzchar(app_key)) {
    provider_var <- .LLM_BACKENDS[[.llm_detect_provider(app_key)]]$key_env
    do.call(Sys.setenv, setNames(list(app_key), provider_var))
    message(sprintf("[LLM] App default key (CUMBA_APP_KEY) -> %s", provider_var))
    return(invisible(TRUE))
  }
  # 6. OpenRouter file key
  or_files <- unique(c(
    file.path(script_dir, "openrouter_key.txt"),
    file.path(cwd, "openrouter_key.txt"),
    file.path(cwd, "shinyApp", "openrouter_key.txt"),
    "openrouter_key.txt",
    "shinyApp/openrouter_key.txt"
  ))
  message(sprintf("[LLM] searching OPENROUTER_API_KEY in: %s",
                  paste(or_files, collapse = " | ")))
  for (f in or_files) {
    if (file.exists(f)) {
      k <- trimws(readLines(f, warn = FALSE, n = 1L))
      if (nzchar(k)) {
        Sys.setenv(OPENROUTER_API_KEY = k)
        message(sprintf("[LLM] OPENROUTER_API_KEY loaded from %s", f))
        return(invisible(TRUE))
      }
    }
  }
  message("[LLM] no key file found — falling back to rule-based mode")
  invisible(FALSE)
}
.bootstrap_llm_key()

# --- Costanti agronomiche -------------------------------------------------
# Soglia oltre cui un giorno e' considerato "piovoso" agronomicamente:
# sotto ~10 mm la pioggia viene perlopiu' intercettata dal canopy o
# evapora prima di entrare nel suolo radicale, quindi NON sostituisce
# l'irrigazione. Cambia qui per modificare il comportamento di:
#   - Pannello 1 (barre pioggia mostrate solo se >= soglia)
#   - today_action() (rain_next3 conta solo i giorni >= soglia)
#   - forecast strip (advice "pioggia" solo se >= soglia)
#   - synth_message() / weather_3day_summary() etichette
.RAIN_DAY_MM <- 10

# --- Helpers --------------------------------------------------------------
fill_with_avg <- function(x) {
  na <- which(is.na(x))
  if (!length(na)) return(x)
  for (i in na) {
    prev_val <- if (i > 1)         tail(x[seq_len(i - 1L)], 1L) else NA_real_
    next_val <- if (i < length(x)) head(x[(i + 1L):length(x)], 1L) else NA_real_
    x[i] <- if (!is.na(prev_val) && !is.na(next_val)) (prev_val + next_val) / 2
            else if (!is.na(prev_val)) prev_val
            else if (!is.na(next_val)) next_val
            else NA_real_
  }
  x
}

build_param_df <- function(input, soil_data = NULL) {
  # Round 7: GUARD — se il modal Avanzate non e' mai stato aperto, gli
  # slider potrebbero non essere ancora inizializzati e input$XXX = NULL.
  # In tal caso usiamo i default agronomici di tomatoFoggia (parametri
  # calibrati). Cosi' la simulazione PARTE subito dopo il click sulla
  # mappa, anche se l'utente non ha mai aperto il pannello Avanzate.
  # soil_data: optional list(FieldCapacity, WiltingPoint) from fetch_soil_soilgrids().
  v <- function(x, fallback) if (is.null(x) || length(x) == 0L) fallback else x
  TGro    <- v(input$TGro,    c(10, 35))
  Topt    <- v(input$Topt,    24)
  TStress <- v(input$TStress, c(5, 45))
  RUE     <- v(input$RUE,     2.8)
  CycleLength       <- v(input$CycleLength, 1216)
  LightInterception <- v(input$LightInterception, c(0.001, 0.9))
  TransFloLag <- v(input$TransFloLag, c(9, 30))
  GrowthSenescenceCanopy <- v(input$GrowthSenescenceCanopy, c(19, 113))
  Kc <- v(input$Kc, c(0.3, 1.15))
  RootIncrease <- v(input$RootIncrease, 0.55)
  RootDepth    <- v(input$RootDepth,    c(4, 60))
  DepletionFraction      <- v(input$DepletionFraction, 50)   # percentage (Main.R divides /100)
  SoilWaterInitial       <- v(input$SoilWaterInitial, 100)  # percentage (Main.R divides /100)
  WaterStressSensitivity <- v(input$WaterStressSensitivity, 3)
  FloweringSlope <- v(input$FloweringSlope, 0.5)
  FloweringMax   <- v(input$FloweringMax,   80)
  k0 <- v(input$k0, 4)
  FruitWaterContent <- v(input$FruitWaterContent, c(0.8, 0.95))
  FruitWaterContentInc <- v(input$FruitWaterContentInc, 0.01)
  FruitWaterContentDecreaseMax <- v(input$FruitWaterContentDecreaseMax, 0.01)

  data.frame(
    Parameter = c("Tbase","Topt","Tmax","Theat","Tcold",
                  "FIntMax","CycleLength","TransplantingLag","FloweringLag",
                  "HalfIntGrowth","HalfIntSenescence","InitialInt",
                  "RUE","KcIni","KcMax",
                  "RootIncrease","RootDepthMax","RootDepthInitial",
                  "FieldCapacity","WiltingPoint","DepletionFraction",
                  "SoilWaterInitial","WaterStressSensitivity",
                  "FloweringSlope","FloweringMax",
                  "k0","FruitWaterContentMin","FruitWaterContentMax",
                  "FruitWaterContentInc","FruitWaterContentDecreaseMax"),
    Value = c(TGro[[1]], Topt, TGro[[2]],
              TStress[[2]], TStress[[1]],
              LightInterception[[2]], CycleLength,
              TransFloLag[[1]], TransFloLag[[2]],
              GrowthSenescenceCanopy[[1]], GrowthSenescenceCanopy[[2]],
              LightInterception[[1]],
              RUE, Kc[[1]], Kc[[2]],
              RootIncrease, RootDepth[[2]], RootDepth[[1]],
              # Soil hydraulic properties: priority is
              #   1. SoilGrids auto-fetch (soil_data arg)  when soilType == "auto"
              #   2. Manual texture class (soilType select)
              #   3. Loam defaults
              {
                st <- input$soilType %||% "sandy"
                if (identical(st, "auto") && !is.null(soil_data) &&
                    is.finite(soil_data$FieldCapacity))
                  soil_data$FieldCapacity
                else if (!is.null(.SOIL_TYPES[[st]]) &&
                         is.finite(.SOIL_TYPES[[st]]$FieldCapacity))
                  .SOIL_TYPES[[st]]$FieldCapacity
                else 0.30
              },
              {
                st <- input$soilType %||% "sandy"
                if (identical(st, "auto") && !is.null(soil_data) &&
                    is.finite(soil_data$WiltingPoint))
                  soil_data$WiltingPoint
                else if (!is.null(.SOIL_TYPES[[st]]) &&
                         is.finite(.SOIL_TYPES[[st]]$WiltingPoint))
                  .SOIL_TYPES[[st]]$WiltingPoint
                else 0.13
              },
              DepletionFraction,
              SoilWaterInitial, WaterStressSensitivity,
              FloweringSlope, FloweringMax,
              k0, FruitWaterContent[[1]], FruitWaterContent[[2]],
              FruitWaterContentInc, FruitWaterContentDecreaseMax),
    stringsAsFactors = FALSE
  ) |> pivot_wider(names_from = Parameter, values_from = Value)
}

# --- Open-Meteo weather ---------------------------------------------------
# Strategy that GUARANTEES no gap between archive and forecast:
#   1. archive-api  ............  start  .. today - 14
#   2. forecast-api with past_days=14 + forecast_days=16
#                   ............  today - 14 .. today + 16
#   merged + deduplicated by DATE.
# Why: the forecast endpoint with `past_days` reliably backfills the
# last 1..92 days from its own model output, and connects seamlessly
# with the +16 day forecast — no missing days around "now".
.om_daily_pars <- "temperature_2m_max,temperature_2m_min,precipitation_sum"

.om_to_df <- function(resp, is_forecast) {
  d <- resp$daily
  if (is.null(d) || is.null(d$time) || length(d$time) == 0L) return(NULL)
  data.frame(
    DATE        = as.Date(d$time),
    Tx          = as.numeric(d$temperature_2m_max),
    Tn          = as.numeric(d$temperature_2m_min),
    P           = as.numeric(d$precipitation_sum),
    is_forecast = is_forecast,
    stringsAsFactors = FALSE
  )
}

# Archive endpoint: takes start_date/end_date
.om_archive <- function(lon, lat, start, end) {
  httr2::request("https://archive-api.open-meteo.com/v1/archive") |>
    httr2::req_url_query(
      latitude   = lat,  longitude = lon,
      start_date = format(as.Date(start), "%Y-%m-%d"),
      end_date   = format(as.Date(end),   "%Y-%m-%d"),
      daily      = .om_daily_pars,
      timezone   = "auto"
    ) |>
    httr2::req_timeout(45) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}

# Forecast endpoint: use past_days/forecast_days (NOT start/end) — robust
# and chiude qualunque buco intorno a "oggi".
.om_forecast <- function(lon, lat, past_days = 14L, forecast_days = 16L) {
  httr2::request("https://api.open-meteo.com/v1/forecast") |>
    httr2::req_url_query(
      latitude       = lat,  longitude = lon,
      past_days      = as.integer(past_days),
      forecast_days  = as.integer(forecast_days),
      daily          = .om_daily_pars,
      timezone       = "auto"
    ) |>
    httr2::req_timeout(45) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}

fetch_openmeteo <- function(lon, lat, start, end) {
  today <- Sys.Date()
  start <- as.Date(start); end <- as.Date(end)
  if (end < start) stop("end < start")

  archive_cutoff <- today - 14L     # piu' generoso: niente gap di confine

  hist_df <- NULL
  if (start <= archive_cutoff) {
    a_end <- min(end, archive_cutoff)
    hist_df <- tryCatch(
      .om_to_df(.om_archive(lon, lat, start, a_end), is_forecast = FALSE),
      error = function(e) {
        warning("Open-Meteo archive: ", conditionMessage(e))
        NULL
      }
    )
  }

  fc_df <- NULL
  if (end > archive_cutoff) {
    fc_df <- tryCatch(
      .om_to_df(.om_forecast(lon, lat, past_days = 14L, forecast_days = 16L),
                is_forecast = TRUE),
      error = function(e) {
        warning("Open-Meteo forecast: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(fc_df)) {
      # Tutto cio' che e' STRETTAMENTE futuro = forecast vero, il resto e'
      # "best estimate" dal modello atmosferico (osservato/quasi-osservato).
      fc_df$is_forecast <- fc_df$DATE >= today
      # Limita al range richiesto
      fc_df <- fc_df[fc_df$DATE >= start & fc_df$DATE <= end, , drop = FALSE]
    }
  }

  out <- dplyr::bind_rows(hist_df, fc_df)
  if (!nrow(out)) stop("Open-Meteo returned no rows")
  # Se archive e forecast si sovrappongono (oggi-14..oggi-14), tieni archive
  out <- out[order(out$DATE, !out$is_forecast), ]
  out <- out[!duplicated(out$DATE), ]
  out <- out[order(out$DATE), ]

  # SANITY CHECK — verifica che non ci siano buchi
  d_seq <- seq(min(out$DATE), max(out$DATE), by = "day")
  missing <- setdiff(as.character(d_seq), as.character(out$DATE))
  if (length(missing)) {
    warning(sprintf("Open-Meteo: %d giorni mancanti (%s..%s).",
                    length(missing), missing[1], tail(missing, 1)))
  }

  out$Lat  <- lat
  out$Site <- sprintf("%.3f_%.3f", lat, lon)
  out
}

# --- Riassunto testuale dei prossimi 3 giorni meteo ---------------------
# Esempio output:
# "Domani sole, max 27°C. Lunedì pioggia (12 mm). Martedì sereno, 28°C."
weather_3day_summary <- function(om, today = Sys.Date(), lang = "en") {
  if (is.null(om) || !nrow(om))
    return(if (lang == "en") "Weather data not available." else "Meteo non disponibile.")
  fut <- om[as.Date(om$DATE) > today &
            as.Date(om$DATE) <= today + 3L, , drop = FALSE]
  if (!nrow(fut))
    return(if (lang == "en") "3-day forecast not available for this site."
           else "Forecast a 3 giorni non disponibile per questo sito.")
  weekday_en <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  weekday_it <- c("Domenica","Lunedi","Martedi","Mercoledi","Giovedi","Venerdi","Sabato")
  parts <- vapply(seq_len(nrow(fut)), function(i) {
    d <- as.Date(fut$DATE[i])
    wday <- as.POSIXlt(d)$wday + 1L
    name <- if (as.integer(d - today) == 1L) {
      if (lang == "en") "Tomorrow" else "Domani"
    } else {
      if (lang == "en") weekday_en[wday] else weekday_it[wday]
    }
    Tx <- fut$Tx[i]; P <- fut$P[i]
    desc <- if (!is.na(P) && P >= .RAIN_DAY_MM)
              sprintf(if (lang == "en") "rain (%.0f mm)" else "pioggia (%.0f mm)", P)
            else if (!is.na(Tx) && Tx >= 32)
              if (lang == "en") "intense heat" else "caldo intenso"
            else if (!is.na(Tx) && Tx <= 12)
              if (lang == "en") "cool" else "fresco"
            else
              if (lang == "en") "clear" else "sereno"
    sprintf("%s %s, max %.0f\u00b0C", name, desc, if (is.na(Tx)) 0 else Tx)
  }, character(1))
  paste0(paste(parts, collapse = ". "), ".")
}

# Convert to the cumba weather schema (Site, Tx, Tn, P, DATE, Lat).
om_to_cumba <- function(om) {
  om |>
    mutate(
      Tx = fill_with_avg(Tx),
      Tn = fill_with_avg(Tn),
      P  = tidyr::replace_na(P, 0)
    ) |>
    select(Site, Tx, Tn, P, DATE, Lat)
}

# --- Reverse geocoding (Nominatim) ---------------------------------------
# Restituisce un nome leggibile per (lat, lon) — es. "Cerignola, FG" o
# "San Severo, Foggia, Apulia". Usa Nominatim (OpenStreetMap), gratuito ma
# con rate limit 1 req/s: chiamiamo solo dopo un clic. In caso di errore
# o rete assente ritorna "" (la UI cade sul fallback "lat/lon").
.NOMINATIM_USER_AGENT <- "CUMBA-Shiny (https://github.com/tomatoModelling/cumba_R_package)"

reverse_geocode_nominatim <- function(lat, lon, lang = "it",
                                      timeout = 10) {
  if (!is.finite(lat) || !is.finite(lon)) return("")
  url <- "https://nominatim.openstreetmap.org/reverse"
  resp <- tryCatch({
    httr2::request(url) |>
      httr2::req_url_query(
        format         = "json",
        lat            = format(lat, nsmall = 5L, scientific = FALSE),
        lon            = format(lon, nsmall = 5L, scientific = FALSE),
        zoom           = 12,            # livello "comune"
        addressdetails = 1,
        `accept-language` = lang
      ) |>
      httr2::req_user_agent(.NOMINATIM_USER_AGENT) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
  }, error = function(e) NULL)
  if (is.null(resp)) return("")
  if (httr2::resp_status(resp) >= 400) return("")
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(parsed)) return("")

  a <- parsed$address
  if (is.null(a)) {
    if (!is.null(parsed$display_name) && nzchar(parsed$display_name))
      return(as.character(parsed$display_name))
    return("")
  }
  # Costruisci "Comune (Provincia), Regione" usando i campi piu' utili
  comune <- a$city %||% a$town %||% a$village %||% a$municipality %||%
            a$hamlet %||% a$county %||% ""
  prov   <- a$county %||% a$state_district %||% ""
  reg    <- a$state %||% a$region %||% ""
  paese  <- a$country %||% ""

  parts <- c(
    if (nzchar(comune)) comune else NULL,
    if (nzchar(prov) && prov != comune) prov else NULL,
    if (nzchar(reg) && reg != prov && reg != comune) reg else NULL,
    if (nzchar(paese) && paese != "Italia") paese else NULL
  )
  if (!length(parts) && !is.null(parsed$display_name))
    return(as.character(parsed$display_name))
  paste(parts, collapse = ", ")
}

# --- LLM interpretation — multi-backend (Groq primary, Anthropic, OpenRouter)
# -----------------------------------------------------------------------
# Backend resolution order (first available key wins at startup):
#   1. Groq   (GROQ_API_KEY / llm_key.txt)   — FREE tier, Llama-3.3-70B
#      Key does NOT get auto-revoked on shinyapps.io.
#      Get a free key at https://console.groq.com (no credit card needed).
#   2. Anthropic (ANTHROPIC_API_KEY)          — claude-3-haiku-20240307 (paid)
#   3. OpenRouter (OPENROUTER_API_KEY / openrouter_key.txt) — legacy :free models
#   4. User runtime key (input$user_llm_key in the UI, session-scoped)
#
# If NO key is found → falls back to synth_message() rule-based engine.
# The "RULES" badge is shown in the UI when running in fallback mode.
#
# -----------------------------------------------------------------------

# Backend descriptor: list(name, base_url, model_primary, model_fallbacks,
#                          key_env, auth_header_fn)
.LLM_BACKENDS <- list(

  gemini = list(
    name        = "Gemini",
    base_url    = "https://generativelanguage.googleapis.com/v1beta/openai",
    models      = c("gemini-2.5-flash",
                    "gemini-2.5-flash-lite",
                    "gemini-flash-latest",
                    "gemini-2.0-flash-001"),
    key_env     = "GOOGLE_API_KEY",
    extra_headers = function(url) list()
  ),

  groq = list(
    name        = "Groq",
    base_url    = "https://api.groq.com/openai/v1",
    models      = c("llama-3.3-70b-versatile",
                    "llama-3.1-8b-instant",
                    "gemma2-9b-it",
                    "mixtral-8x7b-32768"),
    key_env     = "GROQ_API_KEY",
    extra_headers = function(url) list()
  ),

  anthropic = list(
    name        = "Anthropic",
    base_url    = "https://api.anthropic.com/v1",
    models      = c("claude-haiku-4-5-20251001",
                    "claude-3-haiku-20240307"),
    key_env     = "ANTHROPIC_API_KEY",
    extra_headers = function(url) list(
      "anthropic-version" = "2023-06-01",
      "x-api-key"         = Sys.getenv("ANTHROPIC_API_KEY", "")
    )
  ),

  openrouter = list(
    name        = "OpenRouter",
    base_url    = "https://openrouter.ai/api/v1",
    models      = c("meta-llama/llama-3.3-70b-instruct:free",
                    "deepseek/deepseek-chat-v3-0324:free",
                    "qwen/qwen-2.5-72b-instruct:free",
                    "google/gemma-3-27b-it:free",
                    "mistralai/mistral-small-3.1-24b-instruct:free",
                    "meta-llama/llama-3.1-8b-instruct:free"),
    key_env     = "OPENROUTER_API_KEY",
    extra_headers = function(url) list(
      "HTTP-Referer" = "https://github.com/tomatoModelling/cumba_R_package",
      "X-Title"      = "CUMBA Shiny"
    )
  )
)

# Resolve the active backend at call time (allows runtime key injection).
# Returns list(backend_name, key) or list(backend_name=NA, key="").
.llm_resolve_backend <- function(runtime_key = NULL) {
  # Runtime key (from UI widget) always wins
  if (!is.null(runtime_key) && nzchar(runtime_key)) {
    # Detect provider from key prefix
    name <- if (grepl("^AIza",    runtime_key, perl=TRUE)) "gemini"
            else if (grepl("^gsk_",    runtime_key, perl=TRUE)) "groq"
            else if (grepl("^sk-ant-", runtime_key, perl=TRUE)) "anthropic"
            else "openrouter"
    # Override env var so .LLM_BACKENDS key_env lookup works
    do.call(Sys.setenv, setNames(list(runtime_key),
                                 .LLM_BACKENDS[[name]]$key_env))
    return(list(name = name, key = runtime_key))
  }
  for (nm in names(.LLM_BACKENDS)) {
    k <- Sys.getenv(.LLM_BACKENDS[[nm]]$key_env, "")
    if (nzchar(k)) return(list(name = nm, key = k))
  }
  list(name = NA_character_, key = "")
}

# Single call to one model on one backend.
.llm_call_one <- function(model, messages, key, backend) {
  is_anthropic <- identical(backend$name, "Anthropic")

  if (is_anthropic) {
    # Anthropic Messages API: different endpoint, body schema, and auth header.
    # System message must be a top-level "system" key (not inside messages[]).
    url       <- paste0(backend$base_url, "/messages")
    sys_msgs  <- Filter(function(m) m$role == "system", messages)
    user_msgs <- Filter(function(m) m$role != "system", messages)
    body <- list(
      model       = model,
      max_tokens  = 2000L,
      temperature = 0.4,
      messages    = user_msgs
    )
    if (length(sys_msgs) > 0L)
      body$system <- sys_msgs[[1L]]$content
  } else {
    # OpenAI-compatible (Groq, OpenRouter, Gemini): standard /chat/completions
    # Send both max_tokens and max_completion_tokens for broad API compatibility
    url  <- paste0(backend$base_url, "/chat/completions")
    body <- list(
      model       = model,
      max_tokens  = 8192L,
      temperature = 0.4,
      messages    = messages
    )
  }
  extra <- backend$extra_headers(url)

  resp <- tryCatch({
    req <- httr2::request(url) |>
      httr2::req_headers("Content-Type" = "application/json")
    # Anthropic authenticates via x-api-key (added by extra_headers above);
    # OpenAI-compatible backends use Authorization: Bearer.
    if (!is_anthropic)
      req <- req |> httr2::req_headers("Authorization" = paste("Bearer", key))
    if (length(extra))
      req <- do.call(httr2::req_headers, c(list(req), extra))
    req |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(60) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
  }, error = function(e) structure(list(message = conditionMessage(e)),
                                   class = "cumba_net_err"))

  if (inherits(resp, "cumba_net_err"))
    return(list(ok = FALSE, status = -1L, retryable = TRUE,
                err = paste("network:", resp$message)))

  status <- httr2::resp_status(resp)
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)

  if (status >= 400) {
    err <- if (!is.null(parsed$error$message)) parsed$error$message
           else if (!is.null(parsed$message))  parsed$message
           else tryCatch(httr2::resp_body_string(resp), error = function(e) "?")
    retryable <- status == 429L || status >= 500L || status == 404L
    return(list(ok = FALSE, status = status, retryable = retryable,
                err = substr(err, 1, 400)))
  }

  # Parse response text — format differs between APIs
  txt <- if (is_anthropic) {
    tryCatch(parsed$content[[1L]]$text, error = function(e) NULL)
  } else {
    tryCatch(parsed$choices[[1L]]$message$content, error = function(e) NULL)
  }
  if (is.null(txt) || !nzchar(txt))
    return(list(ok = FALSE, status = status, retryable = TRUE,
                err = "empty response"))

  # Log finish_reason to help diagnose truncation
  finish_reason <- tryCatch(
    if (is_anthropic) parsed$stop_reason else parsed$choices[[1L]]$finish_reason,
    error = function(e) "unknown"
  )
  message(sprintf("[llm_call_one] model=%s finish_reason=%s nchar=%d",
                  model, finish_reason %||% "NULL", nchar(txt)))

  list(ok = TRUE, status = status, text = txt,
       model_used = model, backend_used = backend$name)
}

# Build the LLM system prompt (agronomic decision support for processing tomato).
.llm_system_prompt <- function(language = "en") {
  default_lang_label <- switch(
    tolower(language),
    "italiano" = , "it" = "Italian",
    "foggiano" = "Foggiano dialect (Puglia, Italy)",
    "English"
  )
  lang_directive <- paste0(
    "CRITICAL LANGUAGE RULE: Look at the language of the user's LAST message in the conversation. ",
    "Respond in THAT EXACT language — English if they write in English, Italian if they write in Italian, ",
    "Foggiano dialect if they write in Foggiano. ",
    "Do NOT be influenced by the language of any earlier message, including the technical data context. ",
    "Default to ", default_lang_label, " only when the user's message language is genuinely ambiguous (e.g. a single word or number)."
  )

  paste(
    # LANGUAGE DIRECTIVE FIRST — must override everything else
    lang_directive,

    # ROLE
    "You are CUMBA, an agronomic decision-support assistant specialised in",
    "processing tomato (Solanum lycopersicum L.) cultivation and deficit irrigation",
    "management. Your role is to interpret outputs of the CUMBA crop model (yield,",
    "Brix, water requirements, WUE) and support informed irrigation decisions.",

    # SCOPE FILTER
    "SCOPE: You ONLY answer questions about processing tomato irrigation management,",
    "crop physiology, water-stress strategy, and the CUMBA model outputs.",
    "Out-of-scope includes: economics or profitability of tomato farming, market",
    "prices, general philosophy, politics, other crops, food recipes, life advice.",
    "For ANY out-of-scope question reply with exactly ONE short sentence, written",
    "in the same language specified in the LINGUA/LANGUAGE directive above.",
    "Do NOT switch language for the refusal. Do NOT elaborate. Do NOT apologise.",

    # KNOWLEDGE BASE
    "KNOWLEDGE BASE.",
    "Deficit Irrigation (DI) intentionally supplies less water than full ETc to",
    "optimise the balance between water consumption, productivity and fruit quality.",
    "Three strategies: (i) uniform DI throughout the season; (ii) Regulated DI",
    "(RDI) — deficit only in vegetative/ripening phases, full water during",
    "flowering and fruit fill; (iii) Partial Root-Zone Drying (PRDI) — alternating",
    "irrigation sides to trigger ABA-mediated stomatal control.",
    "Moderate deficits can reduce vegetative growth while maintaining acceptable",
    "yield and often improving Brix. Greater biomass is NOT automatically better.",

    # irrigationStopCycle parameter
    "CUMBA MODEL — irrigationStopCycle parameter:",
    "Irrigation is automatically suspended when cycleCompletion",
    "(= gddState / CycleLength * 100) reaches the irrigationStopCycle threshold",
    "(default 85%). This simulates the agronomic practice of stopping irrigation",
    "before physiological maturity to concentrate fruit solutes and raise Brix.",
    "CycleLength is the total thermal time to maturity in degree-days (GDD, base",
    "10°C); default 1216 °C·d (~100 days at average 22°C). Increasing",
    "irrigationStopCycle delays the stop (more water, lower Brix); decreasing it",
    "applies earlier stress (less water, higher Brix, possible yield penalty).",

    # VARIETY CATALOG (use these when variety questions arise)
    "VARIETY CATALOG (processing tomato, Italy). CycleLength in GDD (base 10C),",
    "k0 = sugar/Brix coefficient (2-5 scale, higher = better Brix potential).",
    "Seminis: Docet/SV5197TP/Eventus medio-precoce 1170GDD k0=4.0;",
    "Incipit/Ercole precoce 1110GDD k0=4.0; SV8840TM medio 1200GDD k0=4.3;",
    "PerfectPeel medio 1200GDD k0=4.0; Barrick medio 1230GDD k0=4.0.",
    "Nunhems: N6438 precocissimo 1050GDD k0=4.0; N4523 medio-precoce 1170GDD",
    "k0=4.5 (alto Brix); N4510 medio 1210GDD k0=4.6 (Brix nettamente superiore);",
    "N507/Delfo medio 1210GDD k0=4.0; Fokker tardivo 1380GDD k0=4.0.",
    "Syngenta: Tolerix medio-precoce 1140GDD k0=4.0; Miceno/Redix",
    "medio-precoce 1170GDD k0=4.0-4.3; Waller/BQ400 medio 1210GDD k0=4.0;",
    "Firmus medio-tardivo 1320GDD k0=4.0.",
    "When asked about variety choice: refer to these values, compare CycleLength",
    "with the available growing season, and note Brix potential from k0.",

    # PARETO OPTIMISATION — EVIDENCE BASE
    "EVIDENCE BASE (multi-objective Pareto optimisation: 256 irrigation strategies x",
    "22 years = 5632 simulation-years; objectives: maximise yield, Brix, IWUE;",
    "minimise total irrigation volume, number of irrigation events).",
    "CORE TRADE-OFF: reproductive-phase water supply (repWS) is the dominant lever.",
    "repWS WSI=0.9 vs WSI=0.6 adds +22.8 t/ha yield but costs -0.475 Brix units —",
    "this yield/quality tension is irreducible and must be stated explicitly.",
    "Phase ranking — effect on yield (WSI 0.9 vs 0.6):",
    "repWS +22.8 t/ha >> vegWS +11.5 t/ha > ripWS +5.3 t/ha.",
    "Phase ranking — effect on Brix (WSI 0.9 vs 0.6):",
    "repWS -0.475 >> ripWS -0.027; vegWS slightly positive (+0.013).",
    "Irrigation cost (WSI 0.9 vs 0.6): vegWS +75 mm; repWS +53 mm; ripWS +34 mm.",
    "PARETO COMPROMISE STRATEGY (balanced, equal weights across all objectives):",
    "vegWS=0.7 repWS=0.7 ripWS=0.6 DOY=110 ->",
    "yield=109 t/ha Brix=5.76 irrigation=415 mm IWUE=0.270 ~12 events.",
    "Single-objective Pareto benchmarks:",
    "Max yield: vegWS=0.7 repWS=0.9 ripWS=0.9 DOY=110 -> 132 t/ha Brix=5.39 490 mm 18 events.",
    "Max Brix: vegWS=0.7 repWS=0.6 ripWS=0.6 DOY=125 -> 99 t/ha Brix=5.91 405 mm 11 events.",
    "Min irrigation/events: vegWS=0.6 repWS=0.6 ripWS=0.6 DOY=115 -> 99 t/ha Brix=5.87 384 mm 10 events.",
    "Max IWUE: vegWS=0.7 repWS=0.9 ripWS=0.6 DOY=110 -> 126 t/ha Brix=5.41 454 mm IWUE=0.285.",
    "Transplanting: DOY=110 maximises yield; DOY=125 gains +0.018 Brix but loses ~5 t/ha yield.",
    "Inter-year variability is very high (year ICC ~0.9); weather-year effect dominates all outcomes.",
    "Use these benchmarks when interpreting the user's field-specific CUMBA outputs.",

    # INTERPRETATION RULES
    "RULES: (1) Highest yield does not equal best management. (2) Evaluate yield,",
    "Brix, irrigation volume, water savings, WUE and IWUE together. (3) Moderate",
    "yield reductions may be acceptable if water is saved or Brix improves.",
    "(4) Treat DI as optimisation, not maximisation. (5) State uncertainty when",
    "data is insufficient. (6) Reference only numbers provided — never invent values.",

    # RESPONSE FORMAT
    "RESPONSE FORMAT (follow exactly): MAXIMUM 3 sentences total. No bullet points,",
    "no AI disclaimers, no preamble, no intro phrases like 'Based on CUMBA' or",
    "'Great question'. Jump straight to the answer. For the initial summary cover:",
    "(1) key number (yield/Brix); (2) ONE recommended action; (3) main trade-off.",
    "For FOLLOW-UP questions: 2 sentences maximum. Be blunt and direct.",
    "REMINDER: Never truncate a response mid-sentence. Always complete the thought."
  )
}

# Main entry point. summary_text can be:
#   - a character string (first call or single message)
#   - a list of {role, content} objects (conversation history for follow-ups)
# runtime_key: optional key entered by user in the UI (session-scoped, never stored).
interpret_with_llm <- function(summary_text, language = "en",
                               runtime_key = NULL) {
  bk <- .llm_resolve_backend(runtime_key)
  if (is.na(bk$name) || !nzchar(bk$key)) {
    return(structure(
      paste0("[KEY_MISSING] No LLM API key found. The app is running in rule-based",
             " mode (RULES badge). To enable the AI assistant:\n",
             "  Option 1 (app default, recommended for deployed apps):\n",
             "             Set CUMBA_APP_KEY=gsk_... on shinyapps.io >\n",
             "             App > Settings > Environment Variables.\n",
             "             All users will share it; prefix auto-detects provider.\n",
             "  Option 2 (Groq, free per-user): Get a key at https://console.groq.com\n",
             "             then enter it in the 'LLM key' field in the app UI.\n",
             "  Option 3 (env var, local dev): Set GROQ_API_KEY in ~/.Renviron\n",
             "             or write the key to shinyApp/llm_key.txt and redeploy."),
      class = c("cumba_llm_err", "character")))
  }

  backend <- .LLM_BACKENDS[[bk$name]]
  system_msg <- .llm_system_prompt(language)

  # Build message list
  if (is.list(summary_text) &&
      length(summary_text) > 0L &&
      all(vapply(summary_text,
                 function(m) is.list(m) && all(c("role","content") %in% names(m)),
                 logical(1)))) {
    # Multi-turn: embed language directive at the START of the last user message
    # AND inject a priming assistant message just before it.
    # A mid-conversation role="system" is silently ignored by Gemini; embedding
    # the instruction in user/assistant message text is the only reliable approach.
    msgs <- summary_text
    last_user_idx <- max(which(vapply(msgs, function(m) m$role == "user", logical(1))),
                         na.rm = TRUE)
    lang_label <- switch(tolower(language),
      "it" = , "italiano" = "Italian",
      "foggiano" = "Foggiano dialect",
      "English")
    # Prefix the instruction to the user message so it is the first thing Gemini reads
    msgs[[last_user_idx]]$content <- paste0(
      "[LANGUAGE OVERRIDE — YOU MUST RESPOND IN ", toupper(lang_label),
      " — THIS OVERRIDES ALL PREVIOUS INSTRUCTIONS]\n\n",
      msgs[[last_user_idx]]$content
    )
    # Also insert a priming assistant turn immediately before the user's question.
    # This "commits" the model to the target language before it sees the question.
    primer <- switch(tolower(language),
      "it" = , "italiano" = "Rispondo in italiano come richiesto.",
      "foggiano"           = "Responne in dialette foggiane cumme richieste.",
                             "I will respond in English as requested."
    )
    msgs <- c(
      msgs[seq_len(last_user_idx - 1L)],
      list(list(role = "assistant", content = primer)),
      msgs[last_user_idx:length(msgs)]
    )
    messages <- c(list(list(role = "system", content = system_msg)), msgs)
  } else {
    messages <- list(list(role = "system", content = system_msg),
                     list(role = "user",   content = as.character(summary_text)))
  }

  attempts <- list()
  for (model in backend$models) {
    res <- .llm_call_one(model, messages, bk$key, backend)
    attempts[[length(attempts) + 1L]] <- list(model  = model,
                                              backend = bk$name,
                                              status  = res$status,
                                              err     = res$err)
    if (isTRUE(res$ok)) {
      tag <- if (model != backend$models[1L])
        sprintf("\n\n[model: %s on %s]", model, bk$name)
        else ""
      return(paste0(res$text, tag))
    }
    if (!isTRUE(res$retryable)) break
  }

  # All attempts failed
  log_lines <- vapply(attempts, function(a)
    sprintf("  [%s/%s] HTTP %s: %s",
            a$backend %||% "?", a$model %||% "?",
            a$status %||% "?", a$err %||% "?"), character(1))

  last <- attempts[[length(attempts)]]
  hint <- if (!is.null(last$status) && last$status == 429L)
    "[RATE_LIMITED] All models are busy. Retry in ~1 minute."
  else
    sprintf("[LLM_HTTP_%s] Check your API key and connection.",
            last$status %||% "??")

  structure(paste0(
    "LLM error on ", bk$name, " (key env: ", .LLM_BACKENDS[[bk$name]]$key_env, ").\n",
    paste(log_lines, collapse = "\n"), "\n\n", hint),
    class = c("cumba_llm_err", "character"))
}

# Backward-compat alias
interpret_with_claude <- interpret_with_llm

# --- Agent: extract parameter-change intents from a user chat message ----
# Returns a list of list(slider=<id>, value=<num>), or list() if none found.
# Uses a fast LLM call with temperature=0 and a strict JSON schema.
.llm_extract_agent_intents <- function(msg, runtime_key = NULL) {
  bk <- .llm_resolve_backend(runtime_key)
  if (is.na(bk$name) || !nzchar(bk$key)) return(list())

  # Resolve full backend descriptor (bk only carries name + key)
  backend <- .LLM_BACKENDS[[bk$name]]
  model   <- backend$models[[1L]]   # fastest/primary model for this backend

  param_desc <- paste0(
    "ws_veg=vegetative stress threshold (0.5-1.0), ",
    "ws_rep=reproductive stress threshold (0.5-1.0), ",
    "ws_rip=ripening stress threshold (0.5-1.0), ",
    "turn_veg=vegetative irrigation interval days (1-7), ",
    "turn_rep=reproductive interval days (1-7), ",
    "turn_rip=ripening interval days (1-7), ",
    "irrigationStopCycle=% cycle completion to stop irrigation (50-100), ",
    "CycleLength=thermal cycle GDD (1000-1400), ",
    "RUE=radiation use efficiency g/MJ (1-3), ",
    "k0=Brix/sugar coefficient (2-5)"
  )

  prompt <- paste0(
    'You extract parameter change requests from user messages for a crop model.\n',
    'Return ONLY valid JSON: {"actions":[{"slider":"<id>","value":<number>},...]}.\n',
    'Return {"actions":[]} when no parameter change is requested.\n',
    'Parameters: ', param_desc, '\n',
    'Examples:\n',
    '"try 0.7 stress threshold in ripening" -> {"actions":[{"slider":"ws_rip","value":0.7}]}\n',
    '"prova con soglia 0.65 vegetativa, intervallo 3 giorni" -> ',
    '{"actions":[{"slider":"ws_veg","value":0.65},{"slider":"turn_veg","value":3}]}\n',
    '"stop irrigation at 80%" -> {"actions":[{"slider":"irrigationStopCycle","value":80}]}\n',
    '"cosa significa Brix?" -> {"actions":[]}\n',
    'User message: "', gsub('"', "'", msg), '"'
  )

  is_anthropic <- identical(backend$name, "Anthropic")
  url <- if (is_anthropic) paste0(backend$base_url, "/messages")
         else paste0(backend$base_url, "/chat/completions")

  body_obj <- if (is_anthropic) {
    list(model = model, max_tokens = 200L, temperature = 0,
         system  = "Extract JSON only. Return nothing else.",
         messages = list(list(role = "user", content = prompt)))
  } else {
    list(model = model, max_tokens = 200L, temperature = 0,
         messages = list(list(role = "user", content = prompt)))
  }

  extra <- backend$extra_headers(url)

  tryCatch({
    req <- httr2::request(url) |>
      httr2::req_headers("Content-Type" = "application/json")
    if (!is_anthropic)
      req <- req |> httr2::req_headers("Authorization" = paste("Bearer", bk$key))
    if (length(extra))
      req <- do.call(httr2::req_headers, c(list(req), extra))
    resp <- req |>
      httr2::req_body_json(body_obj) |>
      httr2::req_timeout(8L) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()

    parsed  <- httr2::resp_body_json(resp)
    raw_txt <- if (is_anthropic)
      tryCatch(parsed$content[[1L]]$text, error = function(e) NULL)
    else
      tryCatch(parsed$choices[[1L]]$message$content, error = function(e) NULL)

    if (is.null(raw_txt) || !nzchar(raw_txt)) return(list())

    # Strip markdown code fences (some models wrap JSON in ```json...```)
    clean <- gsub("```(?:json)?\\s*|\\s*```", "", raw_txt, perl = TRUE)
    # Greedy match outermost {...} to handle nested JSON {"actions":[{...}]}
    json_str <- regmatches(clean, regexpr("\\{.*\\}", clean, perl = TRUE))
    if (!length(json_str)) return(list())

    result  <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    actions <- result$actions
    if (is.null(actions) || !length(actions)) return(list())
    Filter(function(a) is.character(a$slider) && nzchar(a$slider) &&
                       is.numeric(a$value),
           actions)
  }, error = function(e) {
    message("[agent_intents] ", conditionMessage(e))
    list()
  })
}

# --- Stato fenologico in italiano (per today-card) -----------------------
# Translates CUMBA phenoStage into user-friendly strings (bilingual).
.pheno_lbl <- function(stage, lang = "en") {
  if (is.null(stage) || is.na(stage)) return(c(label = "—", icon = "—"))
  s <- tolower(as.character(stage))
  if (identical(lang, "foggiano")) {
    switch(s,
      "vegetative"   = c(label = "U' pummarò crèsce",       icon = "🌱"),
      "reproductive" = c(label = "Fa u' pummarò",            icon = "🍅"),
      "flowering"    = c(label = "Fiòre e fiòre",            icon = "🌼"),
      "fruiting"     = c(label = "Attacche u' pummarò",      icon = "🍅"),
      "fruitfilling" = c(label = "U' pummarò si riempèje",   icon = "🍅"),
      "ripening"     = c(label = "Mature u' pummarò",        icon = "🍅"),
      "harvested"    = c(label = "Raccolta fatte",           icon = "✓"),
      c(label = paste0("Fase: ", stage), icon = "🌿")
    )
  } else if (lang == "en") {
    switch(s,
      "vegetative"   = c(label = "Vegetative",     icon = "🌱"),
      "reproductive" = c(label = "Reproductive",   icon = "🍅"),
      "flowering"    = c(label = "Flowering",      icon = "🌼"),
      "fruiting"     = c(label = "Fruit set",      icon = "🍅"),
      "fruitfilling" = c(label = "Fruit filling",  icon = "🍅"),
      "ripening"     = c(label = "Ripening",       icon = "🍅"),
      "harvested"    = c(label = "Cycle complete", icon = "✓"),
      c(label = paste0("Stage: ", stage), icon = "🌿")
    )
  } else {
    switch(s,
      "vegetative"   = c(label = "Fase vegetativa",         icon = "🌱"),
      "reproductive" = c(label = "Fase riproduttiva",        icon = "🍅"),
      "flowering"    = c(label = "Fioritura",                icon = "🌼"),
      "fruiting"     = c(label = "Allegagione",              icon = "🍅"),
      "fruitfilling" = c(label = "Riempimento frutti",       icon = "🍅"),
      "ripening"     = c(label = "Maturazione",              icon = "🍅"),
      "harvested"    = c(label = "Ciclo concluso",           icon = "✓"),
      c(label = paste0("Fase: ", stage), icon = "🌿")
    )
  }
}
# Keep old name as alias for backward compat (e.g. synth_message)
.pheno_it <- function(stage) .pheno_lbl(stage, lang = "it")

# --- Today action: cosa fare OGGI sul campo ------------------------------
# Restituisce una list(action, headline, detail, color, icon, phase, phase_icon)
# costruita dal run corrente del modello. Non chiede LLM: e' regole basate sul modello.
today_action <- function(cur, om, transplantingDOY, depletionFraction = 30,
                         today = Sys.Date(), lang = "en") {

  if (is.null(cur) || !nrow(cur)) {
    cur_yr <- as.integer(format(today, "%Y"))
    trans_date <- as.Date(sprintf("%d-01-01", cur_yr)) +
                  (as.integer(transplantingDOY) - 1L)
    if (today < trans_date) {
      n_days <- as.integer(trans_date - today)
      return(list(
        action   = "wait",
        headline = if (lang == "en")
          sprintf("Transplanting in %d day%s", n_days, if (n_days == 1L) "" else "s")
        else
          sprintf("Trapianto fra %d giorni", n_days),
        detail   = if (lang == "en")
          sprintf("Planned for %s. Season not yet started: prepare the field.",
                  format(trans_date, "%d %B"))
        else
          sprintf("Previsto per %s. La stagione non e' ancora iniziata: prepara il terreno.",
                  format(trans_date, "%d %B")),
        color    = "#6b7480", icon = "\U0001F331",
        phase    = if (lang == "en") "Pre-transplant" else "Pre-trapianto",
        phase_icon = "\U0001F331"
      ))
    }
    return(list(
      action   = "wait",
      headline = if (lang == "en") "Season ended or no data" else "Stagione conclusa o nessun dato",
      detail   = if (lang == "en")
        "Set a transplanting date for the current season."
      else
        "Imposta una data di trapianto per la stagione corrente.",
      color    = "#6b7480", icon = "\u2014",
      phase    = "\u2014", phase_icon = "\u2014"
    ))
  }

  i_today <- which(as.Date(cur$DATE) == today)
  if (!length(i_today)) i_today <- nrow(cur)  # fallback: ultimo giorno

  # ---- Stato fenologico OGGI -------------------------------------------
  phase_lbl <- "\u2014"; phase_icon <- "\U0001f33f"
  if ("phenoStage" %in% names(cur)) {
    p <- .pheno_lbl(cur$phenoStage[i_today], lang = lang)
    phase_lbl <- unname(p["label"]); phase_icon <- unname(p["icon"])
  }

  irr_today <- as.numeric(cur$irrigation[i_today])
  if (is.na(irr_today)) irr_today <- 0

  ftsw_today <- as.numeric(cur$ftsw[i_today])
  ws_today   <- as.numeric(cur$waterStress[i_today])
  ftsw_threshold <- 1 - as.numeric(depletionFraction) / 100  # sotto questa --> stress

  # Pioggia attesa nei prossimi 3 giorni
  rain_next3 <- 0
  if (!is.null(om) && nrow(om)) {
    om2 <- om[as.Date(om$DATE) > today & as.Date(om$DATE) <= today + 3L, , drop = FALSE]
    if (nrow(om2)) {
      P <- as.numeric(om2$P); P[is.na(P)] <- 0
      rain_next3 <- sum(P[P >= .RAIN_DAY_MM])
    }
  }

  # Prossima irrigazione consigliata (entro 7 giorni)
  fut <- cur[as.Date(cur$DATE) > today & as.Date(cur$DATE) <= today + 7L, , drop = FALSE]
  next_irr_idx <- which(fut$irrigation > 0)[1]

  base <- list(phase = phase_lbl, phase_icon = phase_icon)

  acqua_pct   <- ftsw_today * 100
  stress_pct  <- (1 - ws_today) * 100

  it <- lang %in% c("it", "foggiano")  # foggiano usa base italiana
  fg <- identical(lang, "foggiano")    # override per frasi caratteristiche

  if (irr_today > 0) {
    return(c(list(
      action   = "irrigate",
      headline = if (fg) sprintf("ANNAFFI\u00c0 OSCE \u2014 %.0f mm, uagli\u00f9!", irr_today)
                 else if (it) sprintf("IRRIGUA OGGI \u2014 %.0f mm", irr_today)
                 else    sprintf("IRRIGATE TODAY \u2014 %.0f mm", irr_today),
      detail   = if (fg) sprintf("U' pummar\u00f2 t\u00e8ne s\u00e8te! Terra: %.0f%%. Acqua dal ci\u00e8lo: %.0f mm.",
                                 acqua_pct, rain_next3)
                 else if (it) sprintf("Umidit\u00e0 suolo: %.0f%%. Pioggia attesa nei prossimi 3 giorni: %.0f mm.",
                                 acqua_pct, rain_next3)
                 else    sprintf("Soil water: %.0f%%. Rain expected next 3 days: %.0f mm.",
                                 acqua_pct, rain_next3),
      color = "#00838f", icon = "\U0001f4a7"
    ), base))
  }

  if (rain_next3 >= .RAIN_DAY_MM) {
    return(c(list(
      action   = "wait_rain",
      headline = if (fg) sprintf("Asp\u00e8tte \u2014 v\u00e8ne a piogg\u00e8lla: %.0f mm in 3 jurnate", rain_next3)
                 else if (it) sprintf("Attendi \u2014 pioggia prevista: %.0f mm in 3 giorni", rain_next3)
                 else    sprintf("Wait \u2014 rain expected: %.0f mm in 3 days", rain_next3),
      detail   = if (fg) sprintf("Terra: %.0f%%. Ti\u00e8n firme: a piogg\u00e8lla fa u' l\u00e0vore (>= %.0f mm).",
                                 acqua_pct, .RAIN_DAY_MM)
                 else if (it) sprintf("Umidit\u00e0 suolo: %.0f%%. Trattieni l'irrigazione: la pioggia coprira' il fabbisogno (giorni >= %.0f mm).",
                                 acqua_pct, .RAIN_DAY_MM)
                 else    sprintf(paste("Soil water today: %.0f%%.",
                                      "Hold irrigation: rain will cover the need",
                                      "(counting only days >= %.0f mm)."),
                                 acqua_pct, .RAIN_DAY_MM),
      color = "#1565c0", icon = "\U0001f327"
    ), base))
  }

  if (!is.na(next_irr_idx)) {
    when    <- as.Date(fut$DATE[next_irr_idx])
    days_to <- as.integer(when - today)
    mm      <- as.numeric(fut$irrigation[next_irr_idx])
    return(c(list(
      action   = "wait_irr",
      headline = if (fg) sprintf("U' pummar\u00f2 asp\u00e8tte! Annaffi\u00e0 tra %d jurnate",  days_to)
                 else if (it) sprintf("Prossima irrigazione fra %d giorn%s",
                                 days_to, if (days_to == 1L) "o" else "i")
                 else    sprintf("Next irrigation in %d day%s",
                                 days_to, if (days_to == 1L) "" else "s"),
      detail   = if (fg) sprintf("U' %s, ~%.0f mm. Terra: %.0f%% acquata.",
                                 format(when, "%d %b"), mm, acqua_pct)
                 else if (it) sprintf("Il %s, ~%.0f mm. Umidit\u00e0 suolo oggi: %.0f%%.",
                                 format(when, "%d %b"), mm, acqua_pct)
                 else    sprintf("On %s, ~%.0f mm. Soil water today: %.0f%%.",
                                 format(when, "%a %d %b"), mm, acqua_pct),
      color = "#388e3c", icon = "\u2705"
    ), base))
  }

  c(list(
    action   = "ok",
    headline = if (fg) "U' pummar\u00f2 sta bb\u00f2ne, nun ce v\u00f2le acqua pe' m\u00f2"
               else if (it) "Nessuna irrigazione necessaria nell'immediato"
               else    "No irrigation needed soon",
    detail   = if (fg) sprintf("Terra: %.0f%% acquata. Stress: %.0f%%. St\u00e0tt ser\u00e8ne!",
                               acqua_pct, stress_pct)
               else if (it) sprintf("Umidit\u00e0 suolo: %.0f%%. Stress idrico attuale: %.0f%%.",
                               acqua_pct, stress_pct)
               else    sprintf("Soil water: %.0f%%. Current water stress: %.0f%%.",
                               acqua_pct, stress_pct),
    color = "#2e7d32", icon = "\u2705"
  ), base)
}

# --- Suggest transplanting date ------------------------------------------
suggest_transplanting_date <- function(om, today = Sys.Date(),
                                       window_days = 30L,
                                       max_rain_2d = 1) {
  if (is.null(om) || !nrow(om)) return(NA)
  fut <- om[as.Date(om$DATE) >= today &
            as.Date(om$DATE) <= today + as.integer(window_days), , drop = FALSE]
  if (nrow(fut) < 2L) return(NA)
  fut <- fut[order(fut$DATE), , drop = FALSE]
  P  <- as.numeric(fut$P);  P[is.na(P)]  <- 0
  Tn <- as.numeric(fut$Tn); Tx <- as.numeric(fut$Tx)
  for (i in seq_len(nrow(fut) - 1L)) {
    rain2 <- P[i] + P[i + 1L]
    if (is.na(Tn[i]) || is.na(Tx[i])) next
    if (rain2 <= max_rain_2d && Tn[i] >= 8 && Tx[i] >= 14)
      return(as.Date(fut$DATE[i]))
  }
  NA
}

# --- Rule-based synthesis (fallback when LLM is unavailable) --------------
# Generates an agronomic message using only model data.
synth_message <- function(today_info, om, cur, ens = NULL,
                          today = Sys.Date(), lang = "en") {
  paragraphs <- character()

  it <- lang %in% c("it", "foggiano")
  fg <- identical(lang, "foggiano")
  open <- character()
  ph <- today_info$phase %||% "\u2014"
  pre_labels <- c("Pre-trapianto", "Pre-transplant", "Pre-trapiante")
  if (!is.null(ph) && nzchar(ph) && ph != "\u2014" && !ph %in% pre_labels) {
    open <- c(open, if (fg)
      sprintf("U' campo j\u00e8 in %s fase %s.", today_info$phase_icon, tolower(ph))
    else if (it)
      sprintf("Il campo e' in %s fase %s.", today_info$phase_icon, tolower(ph))
    else
      sprintf("The field is in %s phase %s.", today_info$phase_icon, tolower(ph)))
  } else if (ph %in% pre_labels) {
    open <- c(open, if (fg)
      "St\u00e8me prima du' trapiante; a stagione nun j\u00e8 ancora cuminci\u00e0te."
    else if (it)
      "Siamo prima del trapianto; la stagione non e' ancora iniziata."
    else
      "We are before transplanting; the season has not started.")
  }
  if (!is.null(today_info$headline))
    open <- c(open, paste0(today_info$icon, " ", today_info$headline, "."))
  if (!is.null(today_info$detail))
    open <- c(open, today_info$detail)
  if (length(open)) paragraphs <- c(paragraphs, paste(open, collapse = " "))

  if (!is.null(om) && nrow(om))
    paragraphs <- c(paragraphs,
                    paste0(if (it) "Previsioni meteo: " else "Weather forecast: ",
                           weather_3day_summary(om, today, lang = lang)))

  if (!is.null(ens) && nrow(ens) &&
      "fruitFreshWeightAct" %in% names(ens) &&
      "template_year" %in% names(ens)) {
    yq <- tapply(ens$fruitFreshWeightAct, ens$template_year, function(x) {
      x <- x[is.finite(x)]
      if (length(x)) tail(x, 1L) / 100 else NA_real_
    })
    yq <- yq[is.finite(yq)]
    if (length(yq) >= 3L) {
      bq <- if ("brixAct" %in% names(ens))
              tapply(ens$brixAct, ens$template_year, function(x) {
                x <- x[is.finite(x)]
                if (length(x)) tail(x, 1L) else NA_real_
              }) else NULL
      bq <- if (!is.null(bq)) bq[is.finite(bq)] else NULL
      brix_part <- if (!is.null(bq) && length(bq) >= 2L)
        sprintf(", median Brix %.2f (P10-P90: %.2f-%.2f)",
                quantile(bq, .50, na.rm = TRUE),
                quantile(bq, .10, na.rm = TRUE),
                quantile(bq, .90, na.rm = TRUE))
        else ""
      paragraphs <- c(paragraphs, sprintf(
        paste("End-of-cycle forecast based on %d historical analogues:",
              "median yield %.1f t/ha (P10-P90: %.1f-%.1f t/ha)%s.",
              "The range narrows as the season progresses."),
        length(yq),
        quantile(yq, .50, na.rm = TRUE),
        quantile(yq, .10, na.rm = TRUE),
        quantile(yq, .90, na.rm = TRUE),
        brix_part))
    }
  }

  paste(paragraphs, collapse = "\n\n")
}

# --- How dry is the field TODAY compared to past years? ------------------
# Returns the FTSW percentile of today vs the distribution of FTSW values
# from historical runs at the same DOY.
# Example: 0.20 => "drier than 80% of past years at this date".
ftsw_percentile_today <- function(cur, hist_runs, today = Sys.Date()) {
  if (is.null(cur) || is.null(hist_runs) || !nrow(cur) || !nrow(hist_runs))
    return(NA_real_)
  i <- which(as.Date(cur$DATE) == today)
  if (!length(i)) return(NA_real_)
  ftsw_today <- as.numeric(cur$ftsw[i])
  doy_today  <- as.integer(format(today, "%j"))
  hist_at_doy <- hist_runs$ftsw[hist_runs$doy == doy_today]
  hist_at_doy <- hist_at_doy[is.finite(hist_at_doy)]
  if (!length(hist_at_doy)) return(NA_real_)
  mean(hist_at_doy <= ftsw_today, na.rm = TRUE)  # 0..1
}
