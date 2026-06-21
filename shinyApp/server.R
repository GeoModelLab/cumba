# shinyApp/server.R --------------------------------------------------------
# Reactive graph (with caching for historical runs):
#
#   selected_point + history_years + forceRun  ───►  weather_data
#                                                       │
#                            ┌──────────────────────────┤
#                            ▼                          ▼
#                 historical_runs                  current_run
#                 (isolate params:                 (sliders trigger
#                  re-runs only if                  re-run, debounced)
#                  weather changes)
#                            │                          │
#                            ▼                          │
#                       hist_envelope                   │
#                       (P10/P50/P90 by DOY,            │
#                        mapped on current year)        │
#                            └────────────┬─────────────┘
#                                         ▼
#                       status / KPI / forecast strip /
#                       always-on plot (historical baseline +
#                       current run overlay) / LLM

function(input, output, session) {

  # ===== Map ==============================================================
  output$growthMap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     attributionControl = FALSE)) |>
      addProviderTiles("CartoDB.Voyager",   group = "Map") |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      addLayersControl(baseGroups = c("Map", "Satellite"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(lng = 15.5, lat = 41.5, zoom = 6) |>
      addSearchOSM(options = searchOptions(collapsed = TRUE,
                                           zoom = 9, autoCollapse = TRUE)) |>
      addScaleBar(position = "bottomleft")
  })

  # ===== Selected site ====================================================
  selected_point <- reactiveVal(NULL)
  observeEvent(input$growthMap_click, {
    cl <- input$growthMap_click
    selected_point(c(cl$lng, cl$lat))
  })
  observeEvent(input$growthMap_search_marker, {
    m <- input$growthMap_search_marker
    selected_point(c(m$lng, m$lat))
  })
  # Stato della UI: pick (iniziale) / focus (dashboard solo) / work (mappa+strategia+dashboard)
  ui_mode <- reactiveVal("pick")
  set_mode <- function(m) {
    ui_mode(m)
    session$sendCustomMessage("cumba_set_mode", m)
    # Sincronizza i bottoni della toolbar (.active).
    # toggleMap e' "active" solo quando siamo in pick; toggleStrategy quando in strategy.
    active_ids <- character()
    if (m == "strategy") active_ids <- c(active_ids, "toggleStrategy")
    session$sendCustomMessage("cumba_set_toggle_active",
                              list(ids = c("toggleMap", "toggleStrategy"),
                                   active = active_ids))
  }

  # ===== Language management ================================================
  # input$language is driven purely via Shiny.setInputValue from the browser
  # (onclick on 6 lang buttons, shiny:connected handler, cumba_set_lang message).
  # There is NO selectInput — it was removed to fix browser-cached "foggiano".
  .update_lang_buttons <- function(lang) {
    session$sendCustomMessage("cumba_lang_active",
                              list(lang = lang %||% "en"))
  }
  # Language is set exclusively via Shiny.setInputValue from onclick attributes
  # on the 6 lang buttons (heroLangEN/IT/FG + langBtnEN/IT/FG) and from the
  # shiny:connected / cumba_set_lang JS handlers. There is NO selectInput for
  # language — the hidden selectize was removed because the browser cached
  # "foggiano" and re-asserted it after our setTimeout override.
  observeEvent(input$language, {
    .update_lang_buttons(input$language)
  }, ignoreInit = FALSE)

  # On first flush: send EN if the client hasn't already chosen a language.
  # Must use isolate() — onFlushed is not a reactive consumer.
  session$onFlushed(function() {
    cur <- isolate(input$language) %||% ""
    if (!nzchar(cur)) {
      session$sendCustomMessage("cumba_set_lang", list(lang = "en"))
      .update_lang_buttons("en")
    }
  }, once = TRUE)

  # ===== Variety catalog =====================================================
  # When user selects a variety, update CycleLength, RUE, k0 sliders and show
  # a description badge. Selecting "custom" leaves sliders unchanged.
  output$variety_desc_ui <- renderUI({ NULL })  # default: empty

  observeEvent(input$variety, {
    v <- .variety_params(input$variety)
    if (is.null(v)) {
      # Custom: clear description
      output$variety_desc_ui <- renderUI({ NULL })
      return()
    }
    # Update the three key model sliders
    updateSliderInput(session, "CycleLength", value = v$cycleLength)
    updateSliderInput(session, "RUE",         value = v$RUE)
    updateSliderInput(session, "k0",          value = v$k0)
    # Show variety description badge
    output$variety_desc_ui <- renderUI({
      div(style = paste("margin-top:4px; padding:5px 8px;",
                        "background:#e8f5e9; border-left:3px solid #388e3c;",
                        "border-radius:4px; font-size:11px; color:#1b5e20;",
                        "line-height:1.4;"),
          HTML(sprintf("<strong>%s</strong> &middot; %s<br/>",
                       v$cycle_class, v$company)),
          HTML(sprintf("Cycle: <strong>%d °C·d</strong> &nbsp;|&nbsp; ",
                       v$cycleLength)),
          HTML(sprintf("RUE: <strong>%.1f g/MJ</strong> &nbsp;|&nbsp; ", v$RUE)),
          HTML(sprintf("k0: <strong>%.1f</strong><br/>", v$k0)),
          HTML(sprintf("<em>%s</em>", v$description))
      )
    })
  }, ignoreInit = TRUE)

  # Reverse geocoding (Nominatim) — mostra il nome del luogo dopo il clic.
  place_name <- reactiveVal(NULL)

  observeEvent(selected_point(), {
    pt <- selected_point()
    if (is.null(pt) || length(pt) != 2) return()
    pin_icon <- makeAwesomeIcon(icon = "map-marker", markerColor = "red",
                                iconColor = "white", library = "fa")
    leafletProxy("growthMap") |>
      clearGroup("pick") |>
      addAwesomeMarkers(lng = pt[1], lat = pt[2], group = "pick",
                        icon = pin_icon,
                        popup = sprintf("📍 %.3f°N, %.3f°E", pt[2], pt[1]),
                        label = sprintf("%.3f°N, %.3f°E", pt[2], pt[1])) |>
      flyTo(lng = pt[1], lat = pt[2], zoom = 9)
    # Passaggio pick -> focus (dashboard a tutta larghezza, default)
    set_mode("focus")
    # Reverse-geocode in background
    place_name(NULL)
    nm <- tryCatch(reverse_geocode_nominatim(pt[2], pt[1]),
                   error = function(e) NULL)
    if (!is.null(nm) && nzchar(nm)) place_name(nm)
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # Bottone "cambia campo" -> torna a pick
  observeEvent(input$changeSite, {
    selected_point(NULL)
    place_name(NULL)
    leafletProxy("growthMap") |> clearGroup("pick")
    set_mode("pick")
  })

  # I due bottoni della toolbar fanno cose DISTINTE:
  #   🗺 Mappa     -> torna alla vista pick (selezione di un altro sito).
  #                   Niente dashboard, mappa fullscreen.
  #   ⚙ Strategia -> apre la sidebar laterale con le 3 fasi una sotto l'altra.
  #                   Riclic = chiudi (torna a focus).
  observeEvent(input$toggleMap, {
    # Salviamo il punto: l'utente puo' poi clic-cambiarlo.
    set_mode("pick")
  })
  observeEvent(input$toggleStrategy, {
    set_mode(if (ui_mode() == "strategy") "focus" else "strategy")
  })

  # ===== Feedback agricoltore: skip / aggiungi irrigazioni ================
  # irr_overrides() e' una named-list keyed-by-date (chr "YYYY-MM-DD") che
  # registra cosa l'agricoltore ha effettivamente fatto vs il consiglio del
  # modello:
  #   list(action = "skip")               -> consiglio NON eseguito
  #   list(action = "applied", mm = 12)   -> irrigazione fatta in piu' (o
  #                                          al posto del consiglio)
  # Le override NON re-eseguono il modello: vengono solo SOVRAPPOSTE sul
  # plot e usate per generare un summary onesto verso il LLM ("modello
  # consigliava 8 mm il 14/05; agricoltore non ha irrigato"). Ri-runnare il
  # modello con irrigazioni custom richiederebbe un argomento dedicato in
  # cumba_scenario; lo faremo dopo.
  irr_overrides <- reactiveVal(list())

  # Singolo handler che processa TUTTI i click sui bottoncini .irrf-act
  # (skip / undo-skip / remove-applied) inviati via custom JS
  observeEvent(input$irr_action, {
    a <- input$irr_action
    if (is.null(a) || is.null(a$act) || is.null(a$date)) return()
    key <- as.character(a$date)
    ovr <- irr_overrides()
    if (a$act == "skip") {
      ovr[[key]] <- list(action = "skip")
    } else if (a$act == "undo-skip") {
      ovr[[key]] <- NULL
    } else if (a$act == "remove-applied") {
      ovr[[key]] <- NULL
    }
    irr_overrides(ovr)
  }, ignoreInit = TRUE)

  # Round 7: Reset di tutte le scelte agronomiche (skip + applied)
  observeEvent(input$irr_clear_overrides, {
    irr_overrides(list())
    showNotification(
      if ((input$language %||% "en") == "it")
        "Schedule resettato — tutte le scelte agronomiche cancellate."
      else
        "Schedule reset — all agronomic choices cleared.",
      type = "default", duration = 3)
  })

  # Round 7: Export CSV — l'agricoltore scarica il file con tutte le sue
  # scelte (skip + applied), poi puo' ricaricarlo a una sessione successiva
  # invece di re-inserire tutto.
  output$irr_export <- downloadHandler(
    filename = function() {
      sprintf("cumba_irrigazioni_%s.csv",
              format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      ovr <- irr_overrides()
      if (!length(ovr)) {
        # CSV vuoto ma con header, cosi' ricaricarlo non rompe.
        write.csv(data.frame(date = as.Date(character()),
                             mm = numeric(), action = character(),
                             stringsAsFactors = FALSE),
                  file, row.names = FALSE)
        return(invisible(NULL))
      }
      df <- do.call(rbind, lapply(names(ovr), function(k) {
        e <- ovr[[k]]
        data.frame(date = as.Date(k),
                   mm = as.numeric(e$mm %||% 0),
                   action = as.character(e$action %||% "skip"),
                   stringsAsFactors = FALSE)
      }))
      df <- df[order(df$date), ]
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Round 7: Import CSV — popola irr_overrides() leggendo un file salvato
  # in precedenza (formato: date,mm,action). Il modello si re-esegue in
  # automatico per via della reattivita' su irr_overrides().
  observeEvent(input$irr_import, {
    f <- input$irr_import
    req(f, f$datapath)
    df <- tryCatch(
      utils::read.csv(f$datapath, stringsAsFactors = FALSE),
      error = function(e) {
        showNotification(sprintf(
          if ((input$language %||% "en") == "it") "File non leggibile: %s"
          else "Cannot read file: %s", conditionMessage(e)),
          type = "error", duration = 6)
        NULL
      }
    )
    if (is.null(df) || !nrow(df)) return()
    if (!all(c("date", "action") %in% names(df))) {
      showNotification(
        if ((input$language %||% "en") == "it")
          "Il CSV deve avere le colonne: date, mm, action"
        else
          "CSV must have columns: date, mm, action",
        type = "error", duration = 6)
      return()
    }
    df$date   <- as.Date(df$date)
    df$action <- as.character(df$action)
    if ("mm" %in% names(df)) df$mm <- as.numeric(df$mm)
    df <- df[!is.na(df$date) & df$action %in% c("skip", "applied"), ]
    new_ovr <- list()
    for (i in seq_len(nrow(df))) {
      key <- format(df$date[i], "%Y-%m-%d")
      if (df$action[i] == "skip") {
        new_ovr[[key]] <- list(action = "skip")
      } else {
        new_ovr[[key]] <- list(action = "applied",
                               mm = as.numeric(df$mm[i] %||% 0))
      }
    }
    irr_overrides(new_ovr)
    showNotification(sprintf(
      if ((input$language %||% "en") == "it")
        "Caricate %d scelte agronomiche dal file."
      else
        "Loaded %d agronomic choices from file.", length(new_ovr)),
      type = "default", duration = 4)
  })

  # Aggiunta manuale (form a fondo lista): data + mm
  observeEvent(input$irr_apply_btn, {
    d <- input$irr_apply_date
    mm <- suppressWarnings(as.numeric(input$irr_apply_mm))
    if (is.null(d) || is.na(d) || !is.finite(mm) || mm <= 0) {
      showNotification(
        if ((input$language %||% "en") == "en")
          "Please enter a date and mm > 0 to log an irrigation."
        else
          "Indica data + mm > 0 per registrare l'irrigazione.",
        type = "warning", duration = 4)
      return()
    }
    key <- format(as.Date(d), "%Y-%m-%d")
    ovr <- irr_overrides()
    ovr[[key]] <- list(action = "applied", mm = round(mm, 1))
    irr_overrides(ovr)
    showNotification(sprintf(
      if ((input$language %||% "en") == "it") "✓ Registrata: %.1f mm il %s"
      else "✓ Logged: %.1f mm on %s",
      mm, format(as.Date(d), "%d %b")),
      type = "default", duration = 3)
  })

  # ===== Geolocalizzazione browser ========================================
  observeEvent(input$geolocBtn, {
    session$sendCustomMessage("cumba_geolocate", list())
  })
  observeEvent(input$geoloc_result, {
    r <- input$geoloc_result
    if (!is.null(r$lng) && !is.null(r$lat)) {
      selected_point(c(as.numeric(r$lng), as.numeric(r$lat)))
      showNotification(sprintf("📍 %.3f°N, %.3f°E",
                               as.numeric(r$lat), as.numeric(r$lng)),
                       type = "default", duration = 3)
    }
  })
  observeEvent(input$geoloc_error, {
    showNotification(paste(
      if ((input$language %||% "en") == "it") "Geolocalizzazione fallita:"
      else "Geolocation failed:",
      input$geoloc_error$msg,
      if ((input$language %||% "en") == "it") "— prova a cliccare sulla mappa."
      else "— try clicking the map."),
      type = "warning", duration = 6)
  })

  # ===== Suggerisci data trapianto (no pioggia 2 gg, no gelate) ===========
  observeEvent(input$suggestTransplant, {
    om <- weather_data()
    if (is.null(om) || !nrow(om)) {
      showNotification(
        if ((input$language %||% "en") == "it")
          "Nessun dato meteo disponibile — clicca prima sulla mappa."
        else
          "No weather data available — click the map first.",
        type = "warning", duration = 5)
      return()
    }
    d <- suggest_transplanting_date(om, today = Sys.Date(),
                                    window_days = 30L, max_rain_2d = 1)
    lang <- input$language %||% "en"
    if (is.na(d)) {
      showNotification(
        if (lang == "en")
          "No ideal rain-free window found in the next 30 days. Try picking a date manually."
        else
          paste("Nei prossimi 30 giorni non trovo una finestra ideale",
                "(senza pioggia + abbastanza calda). Prova a scegliere a mano."),
        type = "warning", duration = 6)
      return()
    }
    updateDateInput(session, "transplantingDate", value = d)
    showNotification(
      sprintf(if (lang == "en") "💡 Suggested date: %s — rain-free, warm soil."
              else "💡 Data suggerita: %s — finestra senza pioggia e suolo caldo.",
              format(d, "%a %d %B")),
      type = "default", duration = 6)
  })

  # ===== Trapianto: reactive DOY <- date dell'utente ======================
  # L'agricoltore manipola una data; convertiamo in DOY per CUMBA.
  transplantingDOY <- reactive({
    d <- input$transplantingDate
    if (is.null(d) || length(d) == 0L) return(120L)
    as.integer(format(as.Date(d), "%j"))
  })

  # ===== Weather (Open-Meteo: archive + forecast) =========================
  weather_data <- eventReactive(
    list(selected_point(), input$history_years, input$forceRun),
    {
      pt <- selected_point()
      req(pt, length(pt) == 2)
      today  <- Sys.Date()
      cur_yr <- as.integer(format(today, "%Y"))
      hyears <- if (is.null(input$history_years)) 8L else input$history_years
      start  <- as.Date(sprintf("%d-01-01", cur_yr - hyears))
      end    <- today + 16L
      withProgress(message = "🌦️ Open-Meteo (archive + forecast)…",
                   value = 0.3, {
        om <- tryCatch(
          fetch_openmeteo(pt[1], pt[2], start, end),
          error = function(e) {
            showNotification(paste("Open-Meteo:", conditionMessage(e)),
                             type = "error", duration = 8)
            NULL
          }
        )
        setProgress(1)
        om
      })
    },
    ignoreNULL = TRUE
  )

  # ===== Parameters (debounced) ===========================================
  # SoilGrids removed: pass NULL for soil_data (3 manual choices only)
  params_raw <- reactive({ build_param_df(input, soil_data = NULL) })
  params_df  <- params_raw |> debounce(350)

  # ===== Run cumba safely for one year ====================================
  # Round 5: aggiunto parametro `irrigationOverride` opzionale (data.frame
  # con date+mm+action) per re-runnare la stagione con le scelte agronomiche
  # dell'agricoltore (skip / applied). Se NULL, si comporta come prima.
  run_one_year <- function(weather_year, par, transplantingDOY,
                           ws_v, ws_r, ws_p, t_v, t_r, t_p,
                           irrigationOverride = NULL,
                           irrigationStopCycle = 100) {
    if (is.null(weather_year) || !nrow(weather_year)) return(NULL)
    res <- tryCatch({
      cumba_scenario(
        weather             = weather_year,
        param               = par,
        estimateRad         = TRUE,
        estimateET0         = TRUE,
        transplantingDOY    = transplantingDOY,
        irrigationStrategy  = list(
          vegetative   = list(wsLevel = ws_v, turnMin = t_v),
          reproductive = list(wsLevel = ws_r, turnMin = t_r),
          ripening     = list(wsLevel = ws_p, turnMin = t_p)
        ),
        irrigationStopCycle = irrigationStopCycle,
        fullOut             = TRUE,
        irrigationOverride  = irrigationOverride
      )
    }, error = function(e) {
      # Round 7: solo log nei server logs (debug), niente popup utente.
      # In caso di errore ritentiamo senza override piu' avanti in current_run.
      message("[run_one_year] cumba_scenario ERROR: ", conditionMessage(e))
      NULL
    })
    if (is.null(res) || !nrow(res)) return(NULL)
    res$DATE <- as.Date(paste(res$year, res$doy, sep = "-"), format = "%Y-%j")
    res
  }

  # ======================================================================
  # M12: LEARNING CUMBA — metadati dei parametri esponibili nel modal.
  # Per ognuno: nome stampabile, valore default, range slider, passo, e
  # un breve "agronomic hint" che il chatbot puo' citare nel commento.
  # ======================================================================
  learning_meta <- list(
    "RUE" = list(label = "RUE — radiation use efficiency (g MJ-1)",
                 default = 2.95, min = 2.0, max = 3.5, step = 0.05,
                 hint = "Controls biomass accumulation rate: higher RUE → more canopy and yield."),
    "CycleLength" = list(label = "Cycle length (CycleLength, degree days)",
                        default = 1216, min = 1000, max = 1600, step = 10,
                        hint = "Longer cycles delay maturation and increase summer water stress."),
    "FloweringLag" = list(label = "Flowering lag (FloweringLag, % of cycle)",
                         default = 30, min = 20, max = 50, step = 1,
                         hint = "Earlier flowering shifts the critical water stress phase."),
    "FIntMax" = list(label = "Max. light interception (FIntMax)",
                    default = 0.9, min = 0.7, max = 1.0, step = 0.02,
                    hint = "Reflects full canopy closure fraction."),
    "WaterStressSensitivity" = list(label = "Water stress sensitivity (WSsens)",
                                   default = 3.2, min = 2, max = 8, step = 0.2,
                                   hint = "Higher values make the model more reactive to water stress."),
    "RootIncrease" = list(label = "Root growth rate (RootInc, cm/d)",
                         default = 0.42, min = 0.3, max = 0.8, step = 0.02,
                         hint = "Faster roots reach deep water earlier in the season."),
    "RootDepthMax" = list(label = "Max. root depth (RootMax, cm)",
                         default = 88, min = 60, max = 100, step = 2,
                         hint = "Deeper roots = larger soil water reservoir."),
    "FruitWaterContentMin" = list(label = "Min. fruit water content (FruWCmin)",
                                 default = 0.80, min = 0.75, max = 0.85, step = 0.01,
                                 hint = "Controls the available space for sugar accumulation (Brix)."),
    "FruitWaterContentMax" = list(label = "Max. fruit water content (FruWCmax)",
                                 default = 0.91, min = 0.88, max = 0.95, step = 0.01,
                                 hint = "Sets the initial water equilibrium point of the fruit."),
    "k0" = list(label = "Sugar consumption rate (k0)",
               default = 4, min = 3, max = 5, step = 0.1,
               hint = "Controls the balance between carbohydrate accumulation and consumption."),
    "DepletionFraction" = list(label = "Soil water depletion fraction",
                              default = 60, min = 40, max = 70, step = 1,
                              hint = "Available water threshold below which stress response begins.")
  )
  learning_pretty_names <- vapply(learning_meta, `[[`, character(1), "label")

  # Stato Learning: il valore del parametro corrente (se NULL -> default)
  learning_param_value <- reactiveVal(NULL)
  learning_result <- reactiveVal(NULL)   # list(before=..., after=..., delta=...)
  learning_comment_txt <- reactiveVal(NULL)

  # UI dinamica per lo slider, dipende dal parametro selezionato
  output$learningSliderUI <- renderUI({
    pname <- input$learningParam
    if (is.null(pname) || !pname %in% names(learning_meta)) return(NULL)
    meta <- learning_meta[[pname]]
    cur_val <- learning_param_value()
    val <- if (is.null(cur_val)) meta$default else as.numeric(cur_val)
    tagList(
      sliderInput("learningValue",
                  label = meta$label,
                  min = meta$min, max = meta$max,
                  value = val, step = meta$step,
                  width = "100%"),
      div(class = "learning-hint",
          style = "font-size:11.5px; color:#6b7480; margin-top:-6px;",
          em(meta$hint),
          br(),
          tags$small(sprintf("Calibrated default: %s", as.character(meta$default))))
    )
  })

  # Reset al default
  observeEvent(input$learningReset, {
    learning_param_value(NULL)
    learning_result(NULL)
    learning_comment_txt(NULL)
  })

  # ---- Funzione: esegui il modello sostituendo UN parametro nel par-df ----
  run_with_param_override <- function(par_df, pname, new_value, wy,
                                      trDOY, snap) {
    par_mod <- par_df
    if (pname %in% names(par_mod))
      par_mod[[pname]][1] <- new_value
    run_one_year(wy, par_mod, trDOY,
                 snap$ws_veg, snap$ws_rep, snap$ws_rip,
                 snap$turn_veg, snap$turn_rep, snap$turn_rip,
                 irrigationOverride = NULL)
  }

  # Estrai i 4 KPI principali da un run
  summarise_run_kpis <- function(run) {
    if (is.null(run) || !nrow(run))
      return(list(yield = NA_real_, brix = NA_real_,
                  n_irr = NA_integer_, mm_irr = NA_real_))
    yield <- {
      yt <- as.numeric(run$fruitFreshWeightAct) / 100
      yt <- yt[is.finite(yt)]
      if (length(yt)) tail(yt, 1L) else NA_real_
    }
    brix <- {
      if (all(c("carbonSugarState","fruitFreshWeightAct") %in% names(run))) {
        gs <- 0.42
        cs <- as.numeric(run$carbonSugarState)
        fw <- as.numeric(run$fruitFreshWeightAct)
        bx <- ifelse(is.finite(cs) & cs > 0 & is.finite(fw) & fw > 0,
                     (100*cs)/(gs*fw), 0)
        bx <- bx[is.finite(bx) & bx > 0]
        if (length(bx)) tail(bx, 1L) else NA_real_
      } else NA_real_
    }
    n_irr <- sum(run$irrigation > 0, na.rm = TRUE)
    mm_irr <- sum(run$irrigation, na.rm = TRUE)
    list(yield = yield, brix = brix,
         n_irr = as.integer(n_irr), mm_irr = mm_irr)
  }

  # ---- Esegui simulazione "after" e calcola delta ----
  observeEvent(input$learningRecalc, {
    pname <- input$learningParam
    if (is.null(pname)) return()
    meta <- learning_meta[[pname]]
    if (is.null(meta)) return()
    # input$learningValue can be NULL if ionRangeSlider didn't initialise inside
    # the hidden modal — fall back to the calibrated default in that case.
    new_val <- input$learningValue %||% meta$default

    par_df <- params_df()
    om <- weather_data()
    snap <- strategy_snapshot()
    if (is.null(par_df) || is.null(om) || is.null(snap)) {
      showNotification(
        if ((input$language %||% "en") == "en") "Please select a field on the map first."
        else "Scegli prima un campo sulla mappa.",
                       type = "warning", duration = 4)
      return()
    }

    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    om_cumba <- om_to_cumba(om)
    om_cumba$year <- lubridate::year(om_cumba$DATE)
    wy <- om_cumba[om_cumba$year == cur_yr, , drop = FALSE]

    learning_param_value(new_val)

    withProgress(message = "Simulo before / after...", value = 0.2, {
      # Run BEFORE: con parametro al default
      setProgress(0.3, detail = "before (default)")
      run_before <- run_with_param_override(par_df, pname, meta$default,
                                            wy, transplantingDOY(), snap)
      # Run AFTER: con parametro al nuovo valore
      setProgress(0.7, detail = "after (nuovo valore)")
      run_after <- run_with_param_override(par_df, pname, new_val,
                                           wy, transplantingDOY(), snap)
      setProgress(1)
    })

    kpi_b <- summarise_run_kpis(run_before)
    kpi_a <- summarise_run_kpis(run_after)
    learning_result(list(before = kpi_b, after = kpi_a,
                         param = pname, new_value = new_val,
                         default = meta$default, label = meta$label,
                         hint = meta$hint))

    # ---- Commento agronomico del chatbot ----
    learning_comment_txt(if ((input$language %||% "en") == "en")
      "⏳ CUMBA is thinking..." else "⏳ CUMBA sta riflettendo...")
    delta_yield <- kpi_a$yield - kpi_b$yield
    delta_brix  <- kpi_a$brix  - kpi_b$brix
    delta_mm    <- kpi_a$mm_irr - kpi_b$mm_irr
    delta_n     <- kpi_a$n_irr - kpi_b$n_irr

    learning_summary <- sprintf(paste(
      "LEARNING MODE - the user moved one CUMBA parameter to see its effect.",
      "Parameter: %s.",
      "Default (calibrated) value: %s. New value: %s.",
      "Brief agronomic hint stored with the parameter: \"%s\".",
      "All other parameters are kept at their calibrated values. The current",
      "field is the one already selected on the map; transplanting date and",
      "irrigation strategy are the user's current settings.",
      "BEFORE (default parameter) KPIs at end of cycle:",
      "  yield = %.1f t/ha; Brix = %.2f deg; total irrigation = %.0f mm in %d events.",
      "AFTER (new parameter value) KPIs at end of cycle:",
      "  yield = %.1f t/ha; Brix = %.2f deg; total irrigation = %.0f mm in %d events.",
      "Deltas (after minus before):",
      "  yield = %+.1f t/ha; Brix = %+.2f deg;",
      "  irrigation = %+.0f mm; n. events = %+d.",
      "TASK: write 2-3 short sentences (no bullets, no preamble) explaining,",
      "in plain agronomic terms, WHY the change in this parameter produced",
      "this combination of deltas. Then add ONE practical takeaway for the",
      "farmer learning about the model. Stay anchored to the numbers above;",
      "do not invent values. Avoid generic comments."),
      pname, as.character(meta$default), as.character(new_val), meta$hint,
      kpi_b$yield %||% NA, kpi_b$brix %||% NA,
      kpi_b$mm_irr %||% NA, as.integer(kpi_b$n_irr %||% NA),
      kpi_a$yield %||% NA, kpi_a$brix %||% NA,
      kpi_a$mm_irr %||% NA, as.integer(kpi_a$n_irr %||% NA),
      delta_yield, delta_brix, delta_mm, as.integer(delta_n)
    )

    txt <- tryCatch(
      interpret_with_claude(learning_summary, language = input$language %||% "en"),
      error = function(e) {
        message("[learning] LLM ERROR: ", conditionMessage(e))
        structure(sprintf("Errore di rete: %s", conditionMessage(e)),
                  class = c("cumba_llm_err", "character"))
      })
    if (inherits(txt, "cumba_llm_err") || is.null(txt) || !nzchar(txt)) {
      # Fallback rule-based: messaggio sintetico
      lang_fb <- input$language %||% "en"
      arrow <- function(x, unit = "") {
        no_change <- if (lang_fb == "en") "unchanged" else "invariato"
        if (!is.finite(x)) return(no_change)
        if (abs(x) < 1e-6) return(no_change)
        sprintf("%s %.2f%s", if (x > 0) "+" else "", x, unit)
      }
      learning_comment_txt(
        if (lang_fb == "en")
          sprintf(paste("Shifting %s from %s to %s changes the simulation:",
                        "yield %s t/ha, Brix %s, irrigation %s mm.",
                        "Tip: %s"),
                  pname, as.character(meta$default), as.character(new_val),
                  arrow(delta_yield), arrow(delta_brix), arrow(delta_mm),
                  meta$hint)
        else
          sprintf(paste("Spostando %s da %s a %s la simulazione cambia:",
                        "yield %s t/ha, Brix %s, irrigazione %s mm.",
                        "Suggerimento: %s"),
                  pname, as.character(meta$default), as.character(new_val),
                  arrow(delta_yield), arrow(delta_brix), arrow(delta_mm),
                  meta$hint)
      )
    } else {
      learning_comment_txt(as.character(txt))
    }
  })

  # ----- Output: KPI box before/after ------
  output$learningKpiBox <- renderUI({
    res <- learning_result()
    lang <- input$language %||% "en"
    if (is.null(res)) {
      return(div(class = "learning-kpi-box",
                 em(style = "color:#888; font-size:12px;",
                    if (lang=="en") "Press \U25B6 Run simulation to compare KPIs."
                    else "Premi \U25B6 Esegui simulazione per confrontare i KPI.")))
    }
    b <- res$before; a <- res$after
    fmt_kpi <- function(label, vb, va, fmt = "%.1f", unit = "") {
      d <- as.numeric(va) - as.numeric(vb)
      dir <- if (!is.finite(d) || abs(d) < 1e-6) "eq"
             else if (d > 0) "up" else "down"
      d_str <- if (!is.finite(d) || abs(d) < 1e-6) "0"
               else sprintf("%s%s%s", if (d > 0) "+" else "",
                            sprintf(fmt, d), unit)
      div(class = "learning-kpi-row",
          span(class = "learning-kpi-label", label),
          span(class = "learning-kpi-before",
               if (is.finite(vb)) sprintf(fmt, vb) else "—"),
          span(class = "learning-kpi-arrow", "\U2192"),
          span(class = paste("learning-kpi-after", dir),
               if (is.finite(va)) sprintf(fmt, va) else "—"),
          span(class = paste("learning-kpi-delta", dir), d_str))
    }
    div(class = "learning-kpi-box",
        div(style = "font-size:11.5px; color:#6b7480; margin-bottom:6px;",
            sprintf(if (lang=="en") "Default %s = %s  \U2192  New = %s"
                    else "Default %s = %s  \U2192  Nuovo = %s",
                    res$param,
                    as.character(res$default),
                    as.character(res$new_value))),
        fmt_kpi("Yield (t/ha)",               b$yield,  a$yield,  "%.1f"),
        fmt_kpi("Brix (°)",                   b$brix,   a$brix,   "%.2f"),
        fmt_kpi("Irrigation (mm)",            b$mm_irr, a$mm_irr, "%.0f"),
        fmt_kpi(if (lang=="en") "# irrigations" else "N. irrigazioni",
                b$n_irr, a$n_irr, "%.0f"))
  })

  output$learningComment <- renderUI({
    txt <- learning_comment_txt()
    lang <- input$language %||% "en"
    if (is.null(txt))
      return(em(style = "color:#888;",
                if (lang == "en")
                  "Run the simulation and CUMBA will explain what changed."
                else
                  "Esegui la simulazione e CUMBA commentera cosa e cambiato."))
    HTML(gsub("\n", "<br/>", htmltools::htmlEscape(txt)))
  })

  # Helper: trasforma irr_overrides() (named list) -> data.frame standard
  # accettato da cumba_scenario.
  build_override_df <- function(ovr) {
    if (!length(ovr)) return(NULL)
    rows <- lapply(names(ovr), function(k) {
      e <- ovr[[k]]
      if (identical(e$action, "skip"))
        return(data.frame(date = as.Date(k), mm = 0, action = "skip",
                          stringsAsFactors = FALSE))
      if (identical(e$action, "applied"))
        return(data.frame(date = as.Date(k),
                          mm = as.numeric(e$mm %||% 0),
                          action = "applied",
                          stringsAsFactors = FALSE))
      NULL
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(NULL)
    do.call(rbind, rows)
  }

  # ===== Historical runs ==================================================
  # CACHED: triggers only when weather_data changes (i.e. on
  # site / years / forceRun). Slider parameter changes do NOT re-run
  # the historical batch — only the current year is re-simulated.
  historical_runs <- reactive({
    om <- weather_data();  req(om)

    # Snapshot params at this moment (no reactive dependency on them)
    par     <- isolate(params_df())
    trDOY   <- isolate(transplantingDOY())
    ws_v    <- isolate(input$ws_veg);  ws_r <- isolate(input$ws_rep);  ws_p <- isolate(input$ws_rip)
    t_v     <- isolate(input$turn_veg); t_r <- isolate(input$turn_rep); t_p <- isolate(input$turn_rip)
    req(par)

    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    om_cumba <- om_to_cumba(om)
    om_cumba$year <- lubridate::year(om_cumba$DATE)
    past_years <- sort(unique(om_cumba$year[om_cumba$year < cur_yr]))
    if (!length(past_years)) return(NULL)

    withProgress(message = "📊 Historical — simulating 1 run/year…", value = 0, {
      n <- length(past_years)
      all <- lapply(seq_along(past_years), function(i) {
        y <- past_years[i]
        setProgress(i / n, detail = sprintf("anno %d/%d", i, n))
        wy <- om_cumba[om_cumba$year == y, , drop = FALSE]
        run_one_year(wy, par, trDOY, ws_v, ws_r, ws_p, t_v, t_r, t_p)
      })
      setProgress(1)
      all <- Filter(Negate(is.null), all)
      if (!length(all)) return(NULL)
      do.call(rbind, all)
    })
  })

  # ===== Current-season run (re-fires on slider changes E su irr_overrides) ==
  # Round 5: la simulazione si ri-esegue ANCHE quando l'agricoltore aggiunge
  # o salta un'irrigazione, applicando l'override al modello (vedi
  # cumba_scenario(irrigationOverride=...) in R/Main.R).
  # Round 9: reactiveVal per il banner status override (DEVE stare PRIMA
  # di current_run, perche' current_run lo aggiorna).
  override_info <- reactiveVal(list(n_skip = 0L, n_applied = 0L,
                                     dates = character(0)))

  # Round 18: strategia con DEBOUNCE 1.2s. Gli slider aggiornano lo
  # snapshot dopo 1.2s di inattivita'; l'utente puo' anche premere
  # "🔄 Ricalcola con regole" nello scheduling per forzare subito.
  strategy_snapshot <- reactive({
    list(
      ws_veg = input$ws_veg %||% 0.7, ws_rep = input$ws_rep %||% 0.7,
      ws_rip = input$ws_rip %||% 0.7,
      turn_veg = input$turn_veg %||% 1,
      turn_rep = input$turn_rep %||% 1,
      turn_rip = input$turn_rip %||% 1
    )
  }) |> debounce(1200)

  # Trigger manuale per "Ricalcola con regole" (nello scheduling)
  recalc_trigger <- reactiveVal(0L)
  observeEvent(input$recalcWithRules, {
    recalc_trigger(recalc_trigger() + 1L)
    showNotification(
      if ((input$language %||% "en") == "it")
        "✓ Ricalcolo con la strategia attuale..."
      else
        "✓ Recalculating with current strategy...",
      type = "default", duration = 2)
  })
  # When freezeMode toggles, hide/show recalcWithRules in UI
  observeEvent(input$freezeMode, {
    session$sendCustomMessage("cumba_freeze_mode",
                              list(on = isTRUE(input$freezeMode)))
  }, ignoreInit = TRUE)

  current_run <- reactive({
    om  <- weather_data();  req(om)
    par <- params_df();     req(par)
    snap <- strategy_snapshot();  req(snap)

    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    today  <- Sys.Date()
    om_cumba <- om_to_cumba(om)
    om_cumba$year <- lubridate::year(om_cumba$DATE)
    wy <- om_cumba[om_cumba$year == cur_yr, , drop = FALSE]

    # Round 10: ESTENDO il weather oltre today+16 con la coda dell'ULTIMO
    # anno storico disponibile (DOY-mappato sul cur_yr). Senza questo, il
    # modello si ferma a today+16 e gli override su date future (es. il
    # 28/05 quando oggi e' 27/04) NON vengono applicati perche' i giorni
    # corrispondenti non vengono processati. Con la coda climatologica,
    # cumba_scenario percorre TUTTA la stagione e applica l'override.
    wy_max_date <- if (nrow(wy)) max(as.Date(wy$DATE), na.rm = TRUE) else today
    cutoff_doy  <- as.integer(format(wy_max_date, "%j"))
    past_years <- sort(unique(om_cumba$year[om_cumba$year < cur_yr]),
                       decreasing = TRUE)
    if (length(past_years)) {
      tail_y <- past_years[1L]
      tail_w <- om_cumba[om_cumba$year == tail_y, , drop = FALSE]
      tail_w$.doy <- as.integer(format(tail_w$DATE, "%j"))
      tail_w <- tail_w[tail_w$.doy > cutoff_doy, , drop = FALSE]
      if (nrow(tail_w)) {
        tail_w$DATE <- as.Date(sprintf("%d-%03d", cur_yr, tail_w$.doy),
                               format = "%Y-%j")
        tail_w$year <- cur_yr
        tail_w$.doy <- NULL
        wy <- dplyr::bind_rows(wy, tail_w)
        wy <- wy[!duplicated(wy$DATE), ]
        wy <- wy[order(wy$DATE), ]
        message(sprintf("[current_run] meteo esteso: cur=%d gg + climatologia=%d gg (anno %d)",
                        sum(as.Date(wy$DATE) <= wy_max_date),
                        sum(as.Date(wy$DATE) > wy_max_date),
                        tail_y))
      }
    }

    ovr_df <- build_override_df(irr_overrides())

    # Round 20: modalita' "Solo le mie scelte" (freeze).
    # Quando ON, il modello NON propone irrigazioni automatiche oltre quelle
    # gia' decise dall'agricoltore. Cosi' l'utente vede l'IMPATTO PURO del
    # togliere/aggiungere senza compensazione.
    # FREEZE semplificato (v2): in modalità freeze impostare irrigationStopCycle=0
    # impedisce qualsiasi irrigazione automatica. Solo le scelte dell'agricoltore
    # (action="applied" in irr_overrides) producono irrigazione. Niente pre-run.
    freeze_mode <- isTRUE(input$freezeMode)
    isc <- if (freeze_mode) 0L else (input$irrigationStopCycle %||% 85L)

    if (!is.null(ovr_df))
      message(sprintf("[current_run] OVERRIDE -> %d righe: %d skip + %d applied",
                      nrow(ovr_df),
                      sum(ovr_df$action == "skip"),
                      sum(ovr_df$action == "applied")))
    if (freeze_mode)
      message("[current_run] FREEZE ON: irrigationStopCycle=0, solo applied dell'utente")

    out <- run_one_year(wy, par, transplantingDOY(),
                        snap$ws_veg, snap$ws_rep, snap$ws_rip,
                        snap$turn_veg, snap$turn_rep, snap$turn_rip,
                        irrigationOverride  = ovr_df,
                        irrigationStopCycle = isc)
    if (is.null(out)) {
      if (!is.null(ovr_df)) {
        message("[current_run] retry SENZA override (run originale fallito)")
        out <- run_one_year(wy, par, transplantingDOY(),
                            snap$ws_veg, snap$ws_rep, snap$ws_rip,
                            snap$turn_veg, snap$turn_rep, snap$turn_rip,
                            irrigationOverride  = NULL,
                            irrigationStopCycle = isc)
      }
      if (is.null(out)) return(NULL)
    }

    # Round 9: salva info override in reactive separato per banner UI
    override_info(if (!is.null(ovr_df))
      list(n_skip = sum(ovr_df$action == "skip"),
           n_applied = sum(ovr_df$action == "applied"),
           dates = format(ovr_df$date, "%d %b"))
      else list(n_skip = 0L, n_applied = 0L, dates = character(0)))

    # Round 17: il modello cumba mette brixAct=0/NA quasi sempre (la
    # condizione fwcPot >= fwcMax*0.99 e' rara). Calcoliamo il brix
    # MANUALMENTE dalla formula:
    #   brix = (100 * carbonSugarState) / (gammaSugar * fruitFreshWeightAct)
    # dove gammaSugar=0.42 (costante hard-coded in cumba). Cosi' la curva
    # brix mostra l'accumulo dello zucchero durante la maturazione.
    if (all(c("carbonSugarState", "fruitFreshWeightAct") %in% names(out))) {
      gs <- 0.42
      cs <- as.numeric(out$carbonSugarState)
      fw <- as.numeric(out$fruitFreshWeightAct)
      bx_calc <- ifelse(is.finite(cs) & cs > 0 &
                        is.finite(fw) & fw > 0,
                        (100 * cs) / (gs * fw),
                        0)
      out$brixAct_calc <- bx_calc
      message(sprintf("[current_run] brix CALCOLATO: max=%.2f n_pos=%d (max carbonSugar=%.4f, max fwAct=%.2f)",
                      max(bx_calc, na.rm = TRUE),
                      sum(bx_calc > 0, na.rm = TRUE),
                      max(cs, na.rm = TRUE),
                      max(fw, na.rm = TRUE)))
    }

    # Round 9: log diff bilancio idrico nei giorni override
    if (!is.null(ovr_df) && "ftsw" %in% names(out)) {
      ovr_dates_chr <- format(ovr_df$date, "%Y-%m-%d")
      out_dates_chr <- format(as.Date(out$DATE), "%Y-%m-%d")
      ix_match <- which(out_dates_chr %in% ovr_dates_chr)
      if (length(ix_match)) {
        message(sprintf("[current_run] post-override ftsw + irrigation:"))
        for (i in ix_match) {
          message(sprintf("    %s : irrig=%.1f mm, ftsw=%.3f, ws=%.3f",
                          out_dates_chr[i],
                          as.numeric(out$irrigation[i]),
                          as.numeric(out$ftsw[i]),
                          if ("waterStress" %in% names(out))
                            as.numeric(out$waterStress[i]) else NA_real_))
        }
      }
    }

    fc_lookup <- om[, c("DATE", "is_forecast")]
    fc_lookup$DATE <- as.Date(fc_lookup$DATE)
    out <- dplyr::left_join(out, fc_lookup, by = "DATE")
    out$is_forecast[is.na(out$is_forecast)] <- FALSE
    out
  })

  # ===== Rainfed baseline (zero irrigation) for freeze mode ================
  # Shown as a dashed grey line so the user can compare their manually-added
  # irrigation choices against the no-irrigation scenario.
  rainfed_baseline <- reactive({
    if (!isTRUE(input$freezeMode)) return(NULL)
    om   <- weather_data(); if (is.null(om)) return(NULL)
    par  <- params_df();    if (is.null(par)) return(NULL)
    snap <- strategy_snapshot(); if (is.null(snap)) return(NULL)
    cur_yr  <- as.integer(format(Sys.Date(), "%Y"))
    om_cumba <- om_to_cumba(om)
    om_cumba$year <- lubridate::year(om_cumba$DATE)
    wy <- om_cumba[om_cumba$year == cur_yr, , drop = FALSE]
    if (!nrow(wy)) return(NULL)
    tryCatch(
      run_one_year(wy, par, transplantingDOY(),
                   snap$ws_veg, snap$ws_rep, snap$ws_rip,
                   snap$turn_veg, snap$turn_rep, snap$turn_rip,
                   irrigationOverride  = NULL,
                   irrigationStopCycle = 0),   # 0 = never irrigate
      error = function(e) NULL
    )
  })

  output$override_status_banner <- renderUI({
    info <- override_info()
    if ((info$n_skip + info$n_applied) == 0L) return(NULL)
    div(class = "override-banner",
        HTML(sprintf("⚙ <b>Le tue scelte agronomiche sono ATTIVE nella simulazione</b>: %d salt%s, %d aggiunt%s &mdash; %s",
                     info$n_skip, ifelse(info$n_skip == 1L, "o", "i"),
                     info$n_applied, ifelse(info$n_applied == 1L, "a", "e"),
                     paste(info$dates, collapse = ", "))))
  })

  # ===== Ensemble proiezione (analoghi storici) ==========================
  # Costruisce N "scenari di continuazione" della stagione corrente:
  #   prefisso comune  : meteo cur_yr (osservato + forecast Open-Meteo +16d)
  #   coda variabile   : meteo dell'anno passato y rimappato a cur_yr,
  #                      a partire dal giorno (oggi + 17).
  # Output: data.frame "lungo" con colonna template_year. Tutti i membri
  # sono IDENTICI fino a today+16 e divergono dopo: e' la base del
  # "ventaglio" che il fattore richiede.
  current_run_ensemble <- reactive({
    om  <- weather_data();  req(om)
    par <- params_df();     req(par)

    cur_yr      <- as.integer(format(Sys.Date(), "%Y"))
    today       <- Sys.Date()
    cutoff      <- today + 16L
    cutoff_doy  <- as.integer(format(cutoff, "%j"))

    om_cumba <- om_to_cumba(om)
    om_cumba$year <- lubridate::year(om_cumba$DATE)
    cur_w <- om_cumba[om_cumba$year == cur_yr & om_cumba$DATE <= cutoff, ,
                      drop = FALSE]
    if (!nrow(cur_w)) return(NULL)

    past_years <- sort(unique(om_cumba$year[om_cumba$year < cur_yr]))
    if (length(past_years) < 2L) return(NULL)

    withProgress(message = "🎲 Ensemble (analoghi storici)…", value = 0, {
      n <- length(past_years)
      runs <- lapply(seq_along(past_years), function(i) {
        y <- past_years[i]
        setProgress(i / n, detail = sprintf("analogo %d/%d (%d)", i, n, y))
        tail_w <- om_cumba[om_cumba$year == y, , drop = FALSE]
        if (!nrow(tail_w)) return(NULL)
        tail_w$.doy <- as.integer(format(tail_w$DATE, "%j"))
        tail_w <- tail_w[tail_w$.doy > cutoff_doy, , drop = FALSE]
        if (!nrow(tail_w)) return(NULL)
        # Rimappa la coda all'anno corrente (stesso DOY)
        tail_w$DATE <- as.Date(sprintf("%d-%03d", cur_yr, tail_w$.doy),
                               format = "%Y-%j")
        tail_w$year <- cur_yr
        tail_w$.doy <- NULL
        stitched <- dplyr::bind_rows(cur_w, tail_w)
        stitched <- stitched[!duplicated(stitched$DATE), ]
        stitched <- stitched[order(stitched$DATE), ]
        out <- run_one_year(stitched, par, transplantingDOY(),
                            input$ws_veg, input$ws_rep, input$ws_rip,
                            input$turn_veg, input$turn_rep, input$turn_rip)
        if (is.null(out)) return(NULL)
        out$template_year <- y
        out
      })
      setProgress(1)
      runs <- Filter(Negate(is.null), runs)
      if (!length(runs)) return(NULL)
      do.call(rbind, runs)
    })
  }) |> debounce(800)

  # Envelope ensemble (P10/P50/P90 per DOY) per il "fan" sul plot.
  # Round 8: per il brix, usiamo la mediana SOLO dei valori > 0 (mean_pos),
  # cosi' la curva mostra il brix "tipico" durante l'accumulo zuccheri
  # invece di essere schiacciata a 0 dai giorni pre-fioritura.
  current_run_envelope <- reactive({
    ens <- current_run_ensemble()
    if (is.null(ens) || !nrow(ens)) return(NULL)
    cols <- intersect(c("fIntAct","brixAct","fruitFreshWeightAct","ftsw",
                        "waterStress","irrigation"), names(ens))
    if (!length(cols)) return(NULL)
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    ens |>
      group_by(doy) |>
      summarise(across(all_of(cols),
                       list(p10 = ~quantile(., 0.10, na.rm = TRUE),
                            p50 = ~quantile(., 0.50, na.rm = TRUE),
                            p90 = ~quantile(., 0.90, na.rm = TRUE),
                            mean_pos = ~{
                              x <- .[is.finite(.) & . > 0]
                              if (length(x)) mean(x) else 0
                            }),
                       .names = "{.col}_{.fn}"),
                .groups = "drop") |>
      mutate(DATE = as.Date(sprintf("%d-%03d", cur_yr, doy),
                            format = "%Y-%j"))
  })

  # ===== Historical envelope (mapped onto current year calendar) ==========
  hist_envelope <- reactive({
    h <- historical_runs()
    if (is.null(h) || !nrow(h)) return(NULL)

    cols <- intersect(c("fIntAct", "waterStress", "heatStress", "coldStress",
                        "brixAct", "fruitsStateAct", "fruitFreshWeightAct",
                        "irrigation", "p", "tMax", "tMin", "ftsw"),
                      names(h))
    if (!length(cols)) return(NULL)

    env <- h |>
      group_by(doy) |>
      summarise(across(all_of(cols),
                       list(p10 = ~quantile(., 0.10, na.rm = TRUE),
                            p50 = ~quantile(., 0.50, na.rm = TRUE),
                            p90 = ~quantile(., 0.90, na.rm = TRUE)),
                       .names = "{.col}_{.fn}"),
                .groups = "drop")

    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    env$DATE <- as.Date(sprintf("%d-01-01", cur_yr)) + (env$doy - 1L)
    env
  })

  # ===== SITE HEADER (sito + data trapianto inline, molto visibile) ======
  # Round 8: la data trapianto e' SETTABILE direttamente qui (non piu' nel
  # pannello strategia). E' la prima leva agronomica; deve stare in cima.
  output$site_header <- renderUI({
    pt <- selected_point()
    if (is.null(pt)) return(NULL)

    nm <- place_name()
    coords <- sprintf("%.3f°N, %.3f°E", pt[2], pt[1])
    where_name <- if (!is.null(nm) && nzchar(nm)) nm else "Selected field"

    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    lang <- input$language %||% "en"

    div(class = "site-header",
        div(class = "site-pin", "📍"),
        div(class = "site-where",
            div(class = "site-name", where_name),
            div(class = "site-coords", coords)),
        # Soil type: 3 manual choices only (SoilGrids removed)
        div(class = "site-soil",
            tags$label(if (lang == "en") "Soil:" else "Suolo:",
                       class = "ss-trans-label"),
            selectInput("soilType",
                        label = NULL,
                        choices = .SOIL_CHOICES,
                        selected = isolate({
                          prev <- input$soilType %||% "sandy"
                          if (prev %in% names(.SOIL_CHOICES)) prev else "sandy"
                        }),
                        width = "130px"),
            uiOutput("soil_status_span", inline = TRUE)
        ),
        div(class = "site-season",
            div(class = "ss-label",
                sprintf(if (lang == "en") "Season %d" else "Stagione %d", cur_yr)),
            div(class = "ss-trans-row",
                tags$label(if (lang == "en") "Transplanting:" else "Trapianto:",
                           class = "ss-trans-label"),
                dateInput("transplantingDate",
                          label = NULL,
                          value = isolate(input$transplantingDate %||%
                                          as.Date(sprintf("%d-04-20", cur_yr))),
                          format = "dd/mm/yyyy",
                          weekstart = 1,
                          language = if (lang == "en") "en" else "it",
                          width = "120px"),
                actionButton("suggestTransplant",
                             HTML("💡"),
                             class = "btn btn-suggest btn-suggest-inline",
                             title = if (lang == "en")
                               "Suggest a rain-free transplanting date"
                             else
                               "Suggerisci una data senza pioggia")))
    )
  })

  # ===== TODAY HERO CARD ==================================================
  # Card grossa in cima: "irriga oggi N mm" / "aspetta pioggia" / "prossima
  # irrig. fra X giorni". Costruita con today_action() (regole, niente LLM).
  today_info <- reactive({
    cur <- current_run()
    om  <- weather_data()
    today_action(cur, om,
                 transplantingDOY  = transplantingDOY(),
                 depletionFraction = input$DepletionFraction,
                 lang              = input$language %||% "en")
  })

  # ===== Soil status span (independent of site_header re-render) ==========
  output$soil_status_span <- renderUI({
    pt  <- selected_point(); if (is.null(pt)) return(NULL)
    st  <- input$soilType %||% "sandy"
    span_style <- paste0("font-size:10px; color:#fff;",
                         "text-shadow:0 0 3px rgba(0,0,0,.55);",
                         "white-space:nowrap;")
    sm <- .SOIL_TYPES[[st]]
    if (!is.null(sm) && is.finite(sm$FieldCapacity)) {
      awc <- sm$FieldCapacity - sm$WiltingPoint
      tags$span(style = span_style,
                HTML(sprintf("&#10003; FC&nbsp;%.2f&nbsp;WP&nbsp;%.2f&nbsp;AWC&nbsp;%.2f",
                             sm$FieldCapacity, sm$WiltingPoint, awc)))
    } else NULL
  })

  output$today_card <- renderUI({
    pt <- selected_point()
    if (is.null(pt)) {
      {
        lang_wc <- input$language %||% "en"
        return(div(class = "today-card", style = "--today-color:#2e7d32",
          div(class = "today-icon",
              tags$img(src = "cumba_avatar.png",
                       style = "width:56px;height:56px;object-fit:contain;",
                       onerror = "this.replaceWith(document.createTextNode('🍅'))")),
          div(class = "today-body",
            div(class = "today-date", if (lang_wc == "it") "CIAO!" else "HI THERE!"),
            div(class = "today-headline",
                if (lang_wc == "it") "Clicca un punto sulla mappa per selezionare il campo"
                else "Click a point on the map to select your field"),
            div(class = "today-detail",
                if (lang_wc == "it")
                  "Recupereremo meteo storico e previsioni da Open-Meteo, simula la stagione, e ti diremo cosa fare oggi."
                else
                  "We'll pull historical weather and forecast from Open-Meteo, simulate the season, and tell you what to do today."))
        ))
      }
    }

    info <- today_info()
    today <- Sys.Date()
    lang <- input$language %||% "en"
    weekday_en <- c("Sunday","Monday","Tuesday","Wednesday",
                    "Thursday","Friday","Saturday")
    weekday_it <- c("Domenica","Lunedì","Martedì","Mercoledì",
                    "Giovedì","Venerdì","Sabato")
    weekdays_lbl <- if (lang == "en") weekday_en else weekday_it
    date_str <- sprintf("%s %s",
                        weekdays_lbl[as.POSIXlt(today)$wday + 1L],
                        format(today, "%d %B %Y"))

    phase_badge <- if (!is.null(info$phase) && nzchar(info$phase) &&
                       info$phase != "—")
      span(class = "phase-badge",
           sprintf("%s %s", info$phase_icon, info$phase))
      else NULL

    # Round 8: percentuale completamento ciclo + fase, da current_run.
    # Mostriamo "Ciclo: X% - fase: Y" SEMPRE visibile (anche quando
    # il consiglio di oggi e' "nessuna irrigazione" => l'agricoltore
    # vede comunque a che punto e' della stagione).
    cur <- current_run()
    cycle_str <- NULL
    if (!is.null(cur) && nrow(cur)) {
      ix <- which(as.Date(cur$DATE) == today)
      if (length(ix) && "cycleCompletion" %in% names(cur)) {
        cc <- as.numeric(cur$cycleCompletion[ix[1]])
        if (is.finite(cc) && cc >= 0)
          cycle_str <- if (lang == "en")
          sprintf("· Cycle %.0f%% complete", min(cc, 100))
        else
          sprintf("· Ciclo %.0f%% completato", min(cc, 100))
      }
    }

    div(class = "today-card",
        style = sprintf("--today-color:%s", info$color),
        div(class = "today-icon", info$icon),
        div(class = "today-body",
            div(class = "today-date",
                date_str,
                if (!is.null(cycle_str))
                  span(class = "today-cycle", cycle_str)),
            div(class = "today-headline", info$headline),
            div(class = "today-detail", phase_badge, info$detail))
    )
  })

  # ===== Round 19: 2 mini-grafici (yield + brix) con boxplot storico
  # vs punto corrente. Compatto, di lato ai 2 grafici principali.
  output$mini_yield_brix <- renderPlotly({
    h <- historical_runs(); cur <- current_run()
    if (is.null(h) || !nrow(h)) return(NULL)
    last_h <- h |> dplyr::group_by(year) |> dplyr::slice_tail(n = 1L) |> dplyr::ungroup()
    hist_yield <- as.numeric(last_h$fruitFreshWeightAct) / 100
    hist_brix  <- as.numeric(last_h$brixAct)
    cur_yield <- if (!is.null(cur) && nrow(cur))
      tail(as.numeric(cur$fruitFreshWeightAct), 1L) / 100 else NA_real_
    cur_brix  <- if (!is.null(cur) && nrow(cur) && "brixAct_calc" %in% names(cur)) {
      bx <- as.numeric(cur$brixAct_calc)
      bx <- bx[is.finite(bx) & bx > 0]
      if (length(bx)) tail(bx, 1L) else NA_real_
    } else NA_real_
    lang_mc <- input$language %||% "en"
    lbl_hist <- if (lang_mc == "en") "Historical" else "Storico"
    lbl_curr <- if (lang_mc == "en") "This year"  else "Quest'anno"
    p_yield <- plot_ly() |>
      add_trace(y = hist_yield, type = "box", name = lbl_hist,
                marker = list(color = "#9e9e9e"),
                line = list(color = "#9e9e9e"),
                fillcolor = "rgba(158,158,158,0.18)") |>
      add_trace(y = cur_yield, x = lbl_hist, type = "scatter",
                mode = "markers", name = lbl_curr,
                marker = list(color = "#6a1b9a", size = 14,
                              symbol = "diamond",
                              line = list(color = "#fff", width = 2))) |>
      layout(title = list(text = "<b>🍅 Yield (t/ha)</b>",
                           font = list(size = 12, color = "#6a1b9a")),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             showlegend = FALSE,
             margin = list(l = 30, r = 5, t = 30, b = 20),
             yaxis = list(zeroline = FALSE, gridcolor = "#eef0f2"),
             xaxis = list(showticklabels = FALSE))
    p_brix <- plot_ly() |>
      add_trace(y = hist_brix, type = "box", name = lbl_hist,
                marker = list(color = "#9e9e9e"),
                line = list(color = "#9e9e9e"),
                fillcolor = "rgba(158,158,158,0.18)") |>
      add_trace(y = cur_brix, x = lbl_hist, type = "scatter",
                mode = "markers", name = lbl_curr,
                marker = list(color = "#c62828", size = 14,
                              symbol = "diamond",
                              line = list(color = "#fff", width = 2))) |>
      layout(title = list(text = "<b>🍯 Brix (°)</b>",
                           font = list(size = 12, color = "#c62828")),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             showlegend = FALSE,
             margin = list(l = 30, r = 5, t = 30, b = 20),
             yaxis = list(zeroline = FALSE, gridcolor = "#eef0f2"),
             xaxis = list(showticklabels = FALSE))
    subplot(p_yield, p_brix, nrows = 2, margin = 0.08, titleY = TRUE) |>
      config(displayModeBar = FALSE)
  })

  # ===== Round 16: 4 KPI FENOLOGICI (fine trapianto, fioritura inizio,
  # massima fioritura, maturazione). Calcolati dal run corrente (cur).
  output$pheno_kpis <- renderUI({
    cur <- current_run()
    if (is.null(cur) || !nrow(cur)) return(NULL)
    today <- Sys.Date()

    # Fine trapianto: ultimo giorno con phenoCode == 0 (oppure ultimo
    # giorno della fase di transplanting prima di passare a vegetativa).
    end_transplant <- if ("phenoCode" %in% names(cur)) {
      ix <- which(as.integer(cur$phenoCode) == 1L)
      if (length(ix)) as.Date(cur$DATE[min(ix)]) else NA
    } else NA

    # Inizio fioritura: primo giorno con floweringRateAct > 0
    start_flower <- if ("floweringRateAct" %in% names(cur)) {
      flo <- as.numeric(cur$floweringRateAct)
      ix <- which(is.finite(flo) & flo > 0)
      if (length(ix)) as.Date(cur$DATE[min(ix)]) else NA
    } else NA

    # Massima fioritura: giorno con max(floweringRateAct)
    peak_flower <- if ("floweringRateAct" %in% names(cur)) {
      flo <- as.numeric(cur$floweringRateAct)
      flo[!is.finite(flo)] <- 0
      if (max(flo) > 0) as.Date(cur$DATE[which.max(flo)]) else NA
    } else NA

    # Round 17: maturazione = giorno con cycleCompletion=100 (= raccolto),
    # NON inizio della fase ripening (che e' al 70% del ciclo).
    start_ripening <- if ("cycleCompletion" %in% names(cur)) {
      cc <- as.numeric(cur$cycleCompletion)
      ix <- which(is.finite(cc) & cc >= 100)
      if (length(ix)) as.Date(cur$DATE[min(ix)])
      else {
        # Fallback: se non raggiunge 100, l'ultimo giorno simulato
        as.Date(tail(cur$DATE, 1L))
      }
    } else if ("phenoCode" %in% names(cur)) {
      ix <- which(as.integer(cur$phenoCode) == 3L)
      if (length(ix)) as.Date(cur$DATE[max(ix)]) else NA
    } else NA

    lang <- input$language %||% "en"
    month_en <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")
    month_it <- c("Gen","Feb","Mar","Apr","Mag","Giu",
                  "Lug","Ago","Set","Ott","Nov","Dic")
    months_lbl <- if (lang == "en") month_en else month_it

    pheno_kpi <- function(label, icon, d, color, today) {
      passed <- !is.na(d) && d <= today
      cls <- if (passed) "pheno-kpi pheno-kpi-passed"
             else "pheno-kpi"
      d_str <- if (is.na(d)) "—"
               else sprintf("%d %s",
                            as.integer(format(d, "%d")),
                            months_lbl[as.integer(format(d, "%m"))])
      delta_str <- if (is.na(d)) ""
                   else {
                     delta <- as.integer(d - today)
                     if (lang == "en") {
                       if (delta < 0)       sprintf("%d d ago", -delta)
                       else if (delta == 0) "today"
                       else                 sprintf("in %d d", delta)
                     } else {
                       if (delta < 0)       sprintf("%d gg fa", -delta)
                       else if (delta == 0) "oggi"
                       else                 sprintf("fra %d gg", delta)
                     }
                   }
      div(class = cls, style = sprintf("--pheno-color: %s;", color),
          div(class = "pheno-kpi-icon", icon),
          div(class = "pheno-kpi-body",
              div(class = "pheno-kpi-label", label),
              div(class = "pheno-kpi-date", d_str),
              div(class = "pheno-kpi-delta", delta_str)))
    }

    lbl_end_trans   <- if (lang == "en") "Transplanting end" else "Fine trapianto"
    lbl_start_flow  <- if (lang == "en") "Flowering start"   else "Inizio fioritura"
    lbl_peak_flow   <- if (lang == "en") "Peak flowering"    else "Piena fioritura"
    lbl_harvest     <- if (lang == "en") "Harvest"           else "Raccolto previsto"

    div(class = "pheno-kpis-row",
        pheno_kpi(lbl_end_trans,  "🌱", end_transplant, "#43a047", today),
        pheno_kpi(lbl_start_flow, "🌼", start_flower,   "#fbc02d", today),
        pheno_kpi(lbl_peak_flow,  "🌻", peak_flower,    "#fb8c00", today),
        pheno_kpi(lbl_harvest,    "🍅", start_ripening, "#e64a19", today)
    )
  })

  # ===== Confronto col passato in linguaggio naturale =====================
  output$vs_hist_text <- renderUI({
    cur <- current_run();  h <- historical_runs()
    if (is.null(cur) || is.null(h) || !nrow(cur) || !nrow(h)) return(NULL)

    pct <- ftsw_percentile_today(cur, h, today = Sys.Date())
    if (!is.finite(pct)) return(NULL)
    lang <- input$language %||% "en"

    # pct = 0.20 -> drier than 80% of years; pct = 0.80 -> wetter
    if (lang == "en") {
      descr <- if (pct < 0.25)       sprintf("DRIER than %.0f%% of past years", (1 - pct) * 100)
               else if (pct < 0.50)  sprintf("slightly drier than average (%.0f%%-ile)", pct * 100)
               else if (pct < 0.75)  sprintf("slightly wetter than average (%.0f%%-ile)", pct * 100)
               else                  sprintf("WETTER than %.0f%% of past years", pct * 100)
      div(class = "vs-hist-text",
          strong("Historical vs today: "),
          sprintf("at this date, soil water is %s.", descr))
    } else {
      descr <- if (pct < 0.25)       sprintf("piu' SECCO del %.0f%% degli anni passati", (1 - pct) * 100)
               else if (pct < 0.50)  sprintf("leggermente piu' secco della media (%.0f%%-ile)", pct * 100)
               else if (pct < 0.75)  sprintf("leggermente piu' umido della media (%.0f%%-ile)", pct * 100)
               else                  sprintf("piu' UMIDO del %.0f%% degli anni passati", pct * 100)
      div(class = "vs-hist-text",
          strong("Storico vs oggi: "),
          sprintf("a questa data, l'acqua nel terreno e' %s.", descr))
    }
  })

  # ===== Status bar =======================================================
  output$status_bar <- renderUI({
    pt <- selected_point()
    nm <- place_name()
    lang <- input$language %||% "en"
    pt_txt <- if (is.null(pt))
      tags$em(if (lang == "en")
        "👉 click the map (or search) to pick a location"
      else
        "👉 clicca sulla mappa (o usa la ricerca) per scegliere un sito")
    else if (!is.null(nm) && nzchar(nm))
      HTML(sprintf("📍 <strong>%s</strong> <span style='color:#888'>(%.3f°N, %.3f°E)</span>",
                   htmltools::htmlEscape(nm), pt[2], pt[1]))
    else sprintf("📍 %.3f°N  %.3f°E", pt[2], pt[1])

    cur     <- current_run()
    h       <- historical_runs()
    cur_yr  <- as.integer(format(Sys.Date(), "%Y"))
    sim_txt  <- if (is.null(cur))
                  tags$em(if (lang == "en") "pre-transplanting / no data" else "pre-trapianto / nessun dato")
                else sprintf(if (lang == "en") "%d days" else "%d giorni", nrow(cur))
    hist_txt <- if (is.null(h))   tags$em("—")
                else sprintf(if (lang == "en") "%d years" else "%d anni",
                             length(unique(h$year)))

    fc_badge <- NULL
    if (!is.null(cur) && any(cur$is_forecast, na.rm = TRUE)) {
      n_fc <- sum(cur$is_forecast, na.rm = TRUE)
      fc_badge <- span(
        style = paste("background:#ff9800;color:#fff;padding:1px 8px;",
                      "border-radius:10px;font-size:11px;margin-left:8px;",
                      "font-weight:600;"),
        sprintf(if (lang == "en") "\U0001F52E %d days forecast" else "\U0001F52E %d giorni forecast", n_fc)
      )
    }

    tagList(
      strong(if (lang == "en") "Location: " else "Sito: "), pt_txt, "   • ",
      strong(if (lang == "en") "Season: " else "Stagione: "), cur_yr, "   • ",
      strong(if (lang == "en") "In season: " else "In stagione: "), sim_txt, "   • ",
      strong(if (lang == "en") "Historical: " else "Storico: "), hist_txt,
      fc_badge
    )
  })

  # ===== KPI box ==========================================================
  # 4 carte essenziali. Per yield/brix/irrig usa la MEDIANA dell'ensemble
  # (= proiezione di fine ciclo basata su attuale + analoghi storici) e
  # mostra l'intervallo P10-P90 sotto, vs lo storico assoluto.
  output$kpi_box <- renderUI({
    cur <- current_run()
    h   <- historical_runs()
    ens <- current_run_ensemble()

    # ---- Mediane storico (riferimento "vs storico") ----------------------
    hist_yield <- hist_brix <- hist_irr_mm <- NA_real_
    if (!is.null(h) && nrow(h)) {
      last_h <- h |> group_by(year) |> slice_tail(n = 1L) |> ungroup()
      hist_yield <- median(last_h$fruitFreshWeightAct / 100, na.rm = TRUE)
      if (!is.finite(hist_yield))
        hist_yield <- median(last_h$fruitsStateAct / 100, na.rm = TRUE)
      hist_brix   <- median(last_h$brixAct, na.rm = TRUE)
      tot         <- tapply(h$irrigation, h$year, sum, na.rm = TRUE)
      hist_irr_mm <- median(tot, na.rm = TRUE)
    }

    # ---- Quantili ensemble (P10/P50/P90) ---------------------------------
    ens_endcol_q <- function(col, scale = 1) {
      if (is.null(ens) || !nrow(ens)) return(NULL)
      pp <- tapply(ens[[col]], ens$template_year, function(x) {
        x <- x[is.finite(x)]
        if (length(x)) tail(x, 1L) * scale else NA_real_
      })
      pp <- pp[is.finite(pp)]
      if (length(pp) < 2L) return(NULL)
      unname(quantile(pp, c(0.10, 0.50, 0.90), na.rm = TRUE))
    }
    ens_sumcol_q <- function(col) {
      if (is.null(ens) || !nrow(ens)) return(NULL)
      pp <- tapply(ens[[col]], ens$template_year, sum, na.rm = TRUE)
      pp <- pp[is.finite(pp)]
      if (length(pp) < 2L) return(NULL)
      unname(quantile(pp, c(0.10, 0.50, 0.90), na.rm = TRUE))
    }
    yield_q <- ens_endcol_q("fruitFreshWeightAct", 1/100)
    brix_q  <- ens_endcol_q("brixAct")
    irr_q   <- ens_sumcol_q("irrigation")

    # ---- Helpers UI -------------------------------------------------------
    delta_pct <- function(a, b) {
      if (!is.finite(a) || !is.finite(b) || b == 0) return(NA_real_)
      100 * (a - b) / b
    }
    vs_tag <- function(d) {
      if (!is.finite(d)) return(NULL)
      arrow <- if (d > 0) "▲" else if (d < 0) "▼" else "•"
      col   <- if (d > 0) "#388e3c" else if (d < 0) "#c62828" else "#888"
      span(class = "kpi-vs", style = sprintf("color:%s;", col),
           sprintf(if ((input$language %||% "en") == "en") "%s %+.0f%% vs hist." else "%s %+.0f%% vs storico",
                   arrow, d))
    }
    # NB: i ventagli P10-P90 sono ancora calcolati dietro le quinte (yield_q,
    # brix_q, irr_q) ma NON mostrati sulle KPI card per chiarezza/sintesi.
    # Compaiono ancora nel testo dell'LLM e nel sintetico rule-based.
    kpi <- function(label, value, unit = "", delta = NA,
                    range = NULL, klass = "", value_fmt = "%.1f") {
      div(class = paste("kpi-card", klass),
          div(class = "kpi-label", label),
          div(class = "kpi-value",
              if (is.finite(value)) sprintf(value_fmt, value) else "—"),
          if (nzchar(unit)) div(class = "kpi-unit", unit),
          vs_tag(delta))
    }

    # ---- No simulazione corrente -> mostra solo storico ------------------
    lang <- input$language %||% "en"
    if (is.null(cur) || !nrow(cur)) {
      return(div(class = "kpi-row",
        kpi(if (lang=="en") "🍅 Yield (hist.)"  else "🍅 Yield (storico)",  hist_yield,  "t·ha⁻¹"),
        kpi(if (lang=="en") "🍯 Brix (hist.)"   else "🍯 Brix (storico)",   hist_brix,   "°"),
        kpi(if (lang=="en") "💧 Irrig. (hist.)" else "💧 Irrig. (storico)", hist_irr_mm, "mm"),
        kpi(if (lang=="en") "⏳ Season"          else "⏳ Stagione",         NA_real_,    "—")
      ))
    }

    # Round 12: con weather esteso, cur copre tutta la stagione, quindi
    # i tail() di cur sono valori di FINE CICLO. Per il brix prendo
    # l'ULTIMO valore non-NA non-zero (che e' quello "vero" a maturita').
    yield_t <- {
      yt <- as.numeric(cur$fruitFreshWeightAct) / 100
      yt <- yt[is.finite(yt)]
      if (length(yt)) tail(yt, 1L) else NA_real_
    }
    brix <- {
      # Round 17: usa brixAct_calc (calcolato lato shiny da carbonSugar)
      bx <- if ("brixAct_calc" %in% names(cur))
              as.numeric(cur$brixAct_calc)
            else as.numeric(cur$brixAct)
      bx <- bx[is.finite(bx) & bx > 0]
      if (length(bx)) tail(bx, 1L) else NA_real_
    }
    irr_mm <- sum(cur$irrigation, na.rm = TRUE)
    if (!is.finite(irr_mm)) irr_mm <- NA_real_

    # Per il KPI "irrig. fatte finora" usiamo il deterministico (e' osservato).
    today <- Sys.Date()
    irr_to_date <- sum(cur$irrigation[as.Date(cur$DATE) <= today],
                       na.rm = TRUE)
    if (!is.finite(irr_to_date)) irr_to_date <- 0

    # Round 18: aggiungi le irrigazioni APPLICATE manualmente (override
    # action="applied") che potrebbero non essere in cur$irrigation se la
    # data e' diversa da quella consigliata dal modello.
    ovr_now <- irr_overrides()
    if (length(ovr_now)) {
      for (k in names(ovr_now)) {
        e <- ovr_now[[k]]
        if (identical(e$action, "applied") && as.Date(k) <= today) {
          # Evita doppio conteggio: se la data dell'override e' gia'
          # nelle date di cur con irrigation>0, l'avevamo gia' contata
          # tramite cur$irrigation. Aggiungo solo se NON gia' presente.
          d_ovr <- as.Date(k)
          ix_cur <- which(as.Date(cur$DATE) == d_ovr)
          mm_in_cur <- if (length(ix_cur))
            as.numeric(cur$irrigation[ix_cur[1]]) else 0
          if (mm_in_cur == 0) {
            irr_to_date <- irr_to_date + as.numeric(e$mm %||% 0)
          }
        }
      }
    }

    # Round 12: con weather esteso (Round 10) cur copre TUTTA la stagione,
    # quindi NON serve piu' sommare l'ensemble. Cosi' KPI e grafico
    # mostrano lo STESSO numero di irrigazioni (no piu' mismatch 24 vs 12).
    n_irr_future <- sum(cur$irrigation > 0 & as.Date(cur$DATE) > today,
                        na.rm = TRUE)
    n_irr_past   <- sum(cur$irrigation > 0 & as.Date(cur$DATE) <= today,
                        na.rm = TRUE)
    n_irr_total  <- n_irr_past + n_irr_future

    div(class = "kpi-row",
        kpi(if (lang=="en") "🍅 Yield forecast"     else "🍅 Yield previsto",
            yield_t, "t·ha⁻¹",
            delta = delta_pct(yield_t, hist_yield),
            range = yield_q),
        kpi(if (lang=="en") "🍯 Brix forecast"      else "🍯 Brix previsto",
            brix, "°",
            delta = delta_pct(brix, hist_brix),
            range = brix_q,
            value_fmt = "%.2f"),
        kpi(if (lang=="en") "💧 Total irrigation"   else "💧 Irrig. fine ciclo",
            irr_mm, "mm",
            delta = delta_pct(irr_mm, hist_irr_mm),
            range = irr_q,
            klass = "irrig-events",
            value_fmt = "%.0f"),
        kpi(sprintf(if (lang=="en") "📅 Irrigations (%d future)" else "📅 N° irrigazioni (%d future)",
                    as.integer(n_irr_future)),
            n_irr_total,
            if (lang=="en") "events" else "interventi",
            klass = "irrig-events",
            value_fmt = "%.0f"),
        kpi(if (lang=="en") "💦 Irrig. to date"     else "💦 Irrig. fatte",
            irr_to_date, "mm",
            klass = "irrig-events",
            value_fmt = "%.0f")
    )
  })

  # ===== Forecast strip (horizontal, farmer-friendly) =====================
  output$forecast_strip <- renderUI({
    om <- weather_data()
    if (is.null(om) || !nrow(om)) return(NULL)

    today <- Sys.Date()
    fc <- om[as.Date(om$DATE) >= today, , drop = FALSE]
    if (!nrow(fc)) return(NULL)

    cur <- current_run()
    fc$irr_mm <- 0
    if (!is.null(cur) && nrow(cur)) {
      m <- match(as.Date(fc$DATE), as.Date(cur$DATE))
      irrs <- as.numeric(cur$irrigation[m])
      irrs[is.na(irrs)] <- 0
      fc$irr_mm <- irrs
    }

    cur_yr     <- as.integer(format(today, "%Y"))
    trans_date <- as.Date(sprintf("%d-01-01", cur_yr)) +
                  (as.integer(transplantingDOY()) - 1L)
    end_date   <- trans_date + 150L

    weather_icon <- function(P, Tx, irr_mm) {
      if (is.na(irr_mm)) irr_mm <- 0
      if (irr_mm > 0)    return("\U0001F4A6")    # 💦 sprinkler
      if (is.na(P))  P  <- 0
      if (is.na(Tx)) Tx <- 20
      # Sotto la soglia .RAIN_DAY_MM (default 10mm) NON e' "pioggia vera".
      if (P >= .RAIN_DAY_MM) return("\U0001F327")     # 🌧
      if (Tx < 12)      return("\U0001F325")     # 🌥
      if (Tx > 32)      return("\U0001F525")     # 🔥
      "\u2600\uFE0F"                              # ☀
    }
    lang <- input$language %||% "en"

    advice_for <- function(date, P, irr_mm) {
      if (date < trans_date)
        return(list(text = if (lang=="en") "🌱 pre-transplant" else "🌱 pre-trapianto",
                    cls = "advice-pre"))
      if (date > end_date)
        return(list(text = if (lang=="en") "🍅 post-cycle" else "🍅 post-ciclo",
                    cls = "advice-pre"))
      if (irr_mm > 0)
        return(list(text = sprintf("💧 %.0f mm", irr_mm),
                    cls = "advice-irrigate"))
      if (!is.na(P) && P >= .RAIN_DAY_MM)
        return(list(text = sprintf(if (lang=="en") "🌧 rain %.0f mm" else "🌧 pioggia %.0f mm", P),
                    cls = "advice-rain"))
      list(text = "\u2713 ok", cls = "advice-ok")
    }

    weekday_lbl <- function(d) {
      en <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
      it <- c("Domenica","Lunedì","Martedì","Mercoledì","Giovedì","Venerdì","Sabato")
      if (lang == "en") en[as.POSIXlt(d)$wday + 1L]
      else it[as.POSIXlt(d)$wday + 1L]
    }

    cards <- lapply(seq_len(nrow(fc)), function(i) {
      d  <- as.Date(fc$DATE[i])
      Tx <- fc$Tx[i]; Tn <- fc$Tn[i]; P <- fc$P[i]
      adv <- advice_for(d, P, fc$irr_mm[i])
      day_class <- "fc-day"
      if (d == today)      day_class <- paste(day_class, "today")
      if (d == trans_date) day_class <- paste(day_class, "transplant")
      if (fc$irr_mm[i] > 0) day_class <- paste(day_class, "irr-day")

      big_mm <- if (fc$irr_mm[i] > 0)
        div(class = "fc-bigmm", sprintf("%.0f mm", fc$irr_mm[i]))
        else NULL

      div(class = day_class,
          div(class = "fc-icon",  HTML(weather_icon(P, Tx, fc$irr_mm[i]))),
          div(class = "fc-day-name", weekday_lbl(d)),
          div(class = "fc-date",  format(d, "%d/%m/%Y")),
          big_mm,
          div(class = "fc-stats",
              div(sprintf("Tmax %.1f °C", ifelse(is.na(Tx), 0, Tx))),
              div(sprintf("Tmin %.1f °C", ifelse(is.na(Tn), 0, Tn))),
              div(sprintf(if (lang=="en") "Rain %.1f mm" else "Pioggia %.1f mm",
                          ifelse(is.na(P), 0, P)))),
          div(class = paste("fc-advice", adv$cls), HTML(adv$text)))
    })

    n_irr_fc  <- sum(fc$irr_mm > 0, na.rm = TRUE)
    irr_mm_fc <- sum(fc$irr_mm,     na.rm = TRUE)

    tagList(
      div(class = "fc-strip-header",
          strong(sprintf(if (lang=="en") "\U0001F5D3 Next %d days" else "\U0001F5D3 Prossimi %d giorni",
                         nrow(fc))),
          span(class = "fc-strip-sub",
               if (lang == "en")
                 sprintf("%d irrigation event%s recommended — %.0f mm total",
                         n_irr_fc, ifelse(n_irr_fc == 1L, "", "s"), irr_mm_fc)
               else
                 sprintf("%d intervent%s consigliat%s — %.0f mm totali",
                         n_irr_fc,
                         ifelse(n_irr_fc == 1L, "o", "i"),
                         ifelse(n_irr_fc == 1L, "o", "i"),
                         irr_mm_fc))),
      div(class = "forecast-strip", do.call(tagList, cards))
    )
  })

  # ===== Plot wrapper: empty placeholder OR plotly ========================
  output$mainPlot_wrap <- renderUI({
    cur  <- current_run()
    env  <- hist_envelope()
    hist <- historical_runs()
    if (is.null(cur) && is.null(env) && (is.null(hist) || !nrow(hist))) {
      pt <- selected_point()
      {
        lang_pl <- input$language %||% "en"
        if (is.null(pt))
          return(div(class = "plot-empty",
                     div(class = "big", "\U0001F5FA️"),
                     if (lang_pl %in% c("it","foggiano")) "Clicca un punto sulla mappa per avviare la simulazione."
                     else "Click on the map to start the simulation."))
        return(div(class = "plot-empty",
                   div(class = "big", "⏳"),
                   if (lang_pl %in% c("it","foggiano")) "Caricamento meteo / simulazione in corso…"
                   else "Loading weather / simulation running…"))
      }
    }
    lang_pl  <- input$language %||% "en"
    cur_yr   <- as.integer(format(Sys.Date(), "%Y"))
    hist_yrs <- if (!is.null(hist) && "year" %in% names(hist))
      sort(unique(as.integer(hist$year)), decreasing = TRUE) else integer(0)
    yr_choices <- c(
      setNames(as.character(cur_yr),
               if (lang_pl %in% c("it","foggiano")) sprintf("%d (corrente)", cur_yr)
               else                                  sprintf("%d (current)", cur_yr)),
      setNames(as.character(hist_yrs), as.character(hist_yrs)))
    tagList(
      div(style = "display:flex; align-items:center; gap:8px; margin-bottom:4px;",
          tags$label(if (lang_pl %in% c("it","foggiano")) "Stagione:" else "Season:",
                     style = "font-size:12px; color:#555; margin:0;"),
          if (length(hist_yrs) > 0)
            selectInput("viewYear", NULL, choices = yr_choices,
                        selected = isolate(input$viewYear %||% as.character(cur_yr)),
                        width = "160px")
          else
            span(style = "font-size:12px; color:#888;", as.character(cur_yr))),
      plotlyOutput("mainPlot", height = 640)
    )
  })

  # ===== Main multi-panel plot (SOLO 2 PANNELLI) ==========================
  # Panel 1 (top)    : Canopy % + Yield (t/ha) + Brix * 6 + Fioritura * 80
  #                    Pioggia (>=10mm, blu) + Irrigazione (rosso) come barre y2.
  #                    Tutte le serie scalate per stare comode su 0..150.
  # Panel 2 (bottom) : Acqua nel suolo (FTSW * 100, %) + Water stress
  #                    (riportato sull'asse 0..100 come "stress %"). Tre soglie
  #                    tratteggiate per fase fenologica (vegetativa/riprodutt./ripening).
  #
  # Niente terzo pannello water stress separato: l'utente vede TUTTO l'aspetto
  # idrico (acqua disponibile + stress) sullo stesso grafico, in modo compatto.
  #
  # AFFIDABILITA' DECRESCENTE: dopo today+16 (fine forecast), aggiungiamo una
  # banda grigia che si fa via via piu' opaca per indicare la minore certezza
  # delle proiezioni dell'ensemble (analoghi storici).
  output$mainPlot <- renderPlotly({
    lang    <- input$language %||% "en"
    cur_yr  <- as.integer(format(Sys.Date(), "%Y"))
    view_yr <- suppressWarnings(as.integer(input$viewYear %||% cur_yr))
    if (!is.finite(view_yr)) view_yr <- cur_yr

    # Past-season mode: use historical run for selected year
    if (view_yr != cur_yr) {
      hist <- historical_runs()
      if (!is.null(hist) && "year" %in% names(hist)) {
        cur <- hist[as.integer(hist$year) == view_yr, , drop = FALSE]
        if (!nrow(cur)) cur <- NULL
      } else cur <- NULL
    } else {
      cur <- current_run()
    }

    # Clip simulation at 100% cycle completion (harvest date).
    # Also record harvest_date to constrain x_max.
    harvest_date <- NULL
    if (!is.null(cur) && nrow(cur) > 0 && "cycleCompletion" %in% names(cur)) {
      cc <- suppressWarnings(as.numeric(cur$cycleCompletion))
      cutoff_idx <- which(is.finite(cc) & cc >= 100)
      if (length(cutoff_idx) > 0) {
        cur <- cur[seq_len(cutoff_idx[1L]), , drop = FALSE]
        harvest_date <- as.Date(cur$DATE[nrow(cur)])
      }
    }

    # Rainfed baseline for freeze mode (no irrigation)
    rf  <- if (isTRUE(input$freezeMode)) rainfed_baseline() else NULL
    # Also clip rainfed at harvest date
    if (!is.null(rf) && !is.null(harvest_date)) {
      rf <- rf[as.Date(rf$DATE) <= harvest_date, , drop = FALSE]
    }
    has_rf <- !is.null(rf) && nrow(rf) > 0

    env     <- hist_envelope()
    env_ens <- current_run_envelope()
    if (is.null(cur) && is.null(env)) return(NULL)

    has_cur <- !is.null(cur)     && nrow(cur)     > 0
    has_env <- !is.null(env)     && nrow(env)     > 0
    has_ens <- !is.null(env_ens) && nrow(env_ens) > 0

    # ---- X axis range: clipped to harvest date --------------------------
    today <- Sys.Date()
    xs <- list()
    if (has_env) {
      env_dates <- env$DATE[as.Date(env$DATE) <= (harvest_date %||% max(env$DATE, na.rm=TRUE))]
      if (length(env_dates)) xs <- c(xs, list(range(as.Date(env_dates), na.rm = TRUE)))
    }
    if (has_cur) xs <- c(xs, list(range(as.Date(cur$DATE), na.rm = TRUE)))
    if (has_ens && is.null(harvest_date))
      xs <- c(xs, list(range(as.Date(env_ens$DATE), na.rm = TRUE)))
    xs <- c(xs, list(c(today, today + 16L)))
    x_min <- min(do.call(c, xs), na.rm = TRUE)
    # x_max = harvest date if available; otherwise max of all series
    x_max <- if (!is.null(harvest_date)) harvest_date
             else max(do.call(c, xs), na.rm = TRUE)

    # ---- Reference shapes (today + forecast band) -----------------------
    has_fc <- has_cur && any(cur$is_forecast, na.rm = TRUE)
    fc_start <- today
    fc_end   <- min(today + 16L, x_max)

    base_shapes <- list(
      # Today (solid green)
      list(type = "line", xref = "x", yref = "paper",
           x0 = today, x1 = today, y0 = 0, y1 = 1,
           line = list(color = "#2e7d32", width = 1.6)),
      # Forecast band 0..16 gg (arancione tenue: previsione AFFIDABILE)
      list(type = "rect", xref = "x", yref = "paper",
           x0 = fc_start, x1 = fc_end, y0 = 0, y1 = 1,
           fillcolor = "rgba(255,160,0,0.10)",
           line = list(width = 0), layer = "below"),
      # Forecast end (dashed orange)
      list(type = "line", xref = "x", yref = "paper",
           x0 = fc_end, x1 = fc_end, y0 = 0, y1 = 1,
           line = list(color = "#ef6c00", width = 1.4, dash = "dash"))
    )

    # Round 9: bande grigie di incertezza RIMOSSE — confondevano l'occhio,
    # facevano sembrare che il grafico avesse uno sfondo grigio. La perdita
    # di affidabilita' del forecast si capisce gia' dalla riga tratteggiata
    # arancione "fine forecast" e dal ventaglio ensemble.

    # Marker fioritura piena (giorno con max floweringRateAct)
    flo_peak_date <- NULL
    if (has_cur && "floweringRateAct" %in% names(cur)) {
      idx <- which.max(cur$floweringRateAct)
      if (length(idx) && cur$floweringRateAct[idx] > 0)
        flo_peak_date <- as.Date(cur$DATE[idx])
    }
    if (!is.null(flo_peak_date)) {
      base_shapes <- c(base_shapes, list(
        list(type = "line", xref = "x", yref = "paper",
             x0 = flo_peak_date, x1 = flo_peak_date, y0 = 0, y1 = 1,
             line = list(color = "#f9a825", width = 1.6, dash = "dot"))
      ))
    }

    # Solo ribbon (P10-P90) per lo storico — niente mediana tratteggiata,
    # mantiene l'occhio sull'andamento corrente.
    add_ribbon <- function(p, var, color_rgba, color_line, name_pref,
                           yaxis = "y", scale = 1) {
      if (!has_env) return(p)
      lo <- paste0(var, "_p10"); hi <- paste0(var, "_p90")
      if (!all(c(lo, hi) %in% names(env))) return(p)
      p |>
        add_trace(x = env$DATE, y = env[[hi]] * scale,
                  type = "scatter", mode = "lines",
                  line = list(width = 0), showlegend = FALSE,
                  hoverinfo = "skip", yaxis = yaxis, inherit = FALSE) |>
        add_trace(x = env$DATE, y = env[[lo]] * scale,
                  type = "scatter", mode = "lines",
                  line = list(width = 0), fill = "tonexty",
                  fillcolor = color_rgba,
                  name = paste0(name_pref, if (lang == "it") " storico P10–P90" else " hist. P10–P90"),
                  hovertemplate = paste0(name_pref, " hist<extra></extra>"),
                  yaxis = yaxis, inherit = FALSE)
    }

    # Forecast (analoghi storici) — SOLO mediana tratteggiata, niente ribbon.
    # Il ribbon resta riservato allo storico (per non sovraccaricare il plot).
    add_ens_fan <- function(p, var, color_rgba, color_line, name_pref,
                            yaxis = "y", scale = 1) {
      if (!has_ens) return(p)
      md <- paste0(var, "_p50")
      if (!(md %in% names(env_ens))) return(p)
      ef <- env_ens[env_ens$DATE >= today + 16L, , drop = FALSE]
      if (!nrow(ef)) return(p)
      p |>
        add_trace(x = ef$DATE, y = ef[[md]] * scale,
                  type = "scatter", mode = "lines",
                  line = list(color = color_line, width = 2.2, dash = "dash"),
                  name = paste0(name_pref, if (lang == "it") " proiezione (mediana analoghi)" else " projection (median analogues)"),
                  hovertemplate = paste0(name_pref, if (lang == "it") " proiezione: %{y:.1f}<extra></extra>" else " projection: %{y:.1f}<extra></extra>"),
                  yaxis = yaxis, inherit = FALSE)
    }

    # ============== Panel 1: Canopy + Yield + Brix + Fioritura + bars ====
    # SCALE per metterli tutti su asse 0..150 senza che si schiaccino:
    #   Canopy %         : 0..100  (gia' su scala)
    #   Yield t/ha       : 0..120  (gia' su scala)
    #   Brix °           : 0..15  -> *6  -> 0..90 visibile
    #   Fioritura (0..1) : 0..1   -> *80 -> 0..80 visibile (campana)
    BRIX_SCALE <- 6
    FLO_SCALE  <- 80
    p1 <- plot_ly() |>
      add_ribbon("fIntAct", "rgba(76,175,80,0.18)", "#558b2f",
                 "Canopy %", scale = 100) |>
      # Fan ensemble Yield (viola chiaro) — solo proiezione futura
      add_ens_fan("fruitFreshWeightAct", "rgba(106,27,154,0.18)", "#6a1b9a",
                  "Yield (t/ha)", scale = 1/100) |>
      add_ens_fan("brixAct", "rgba(239,108,0,0.16)", "#c62828",
                  "Brix ×6", scale = BRIX_SCALE)

    # Irrigazioni STORICHE (mediana per DOY) come barre tenue, asse y2.
    # Cosi' l'agricoltore vede dove e quanto ha irrigato il modello negli
    # anni passati, e puo' confrontare col run corrente.
    if (has_env && "irrigation_p50" %in% names(env)) {
      p1 <- p1 |>
        add_trace(x = env$DATE, y = env$irrigation_p50,
                  type = "bar", name = if (lang == "it") "Irrig. storica mediana" else "Hist. median irrig.",
                  marker = list(color = "rgba(229,57,53,0.30)",
                                line = list(width = 0)),
                  yaxis = "y2", opacity = 0.85,
                  hovertemplate = if (lang == "it") "Irrig. storica mediana: %{y:.0f} mm<extra></extra>" else "Hist. median irrig.: %{y:.0f} mm<extra></extra>",
                  inherit = FALSE)
    }
    # Round 12: barre "Irrig. previste oltre 16gg" RIMOSSE — duplicate
    # con cur$irrigation che ora copre tutta la stagione (weather esteso).
    if (has_cur) {
      # Ordine traces studiato per VISIBILITA':
      #  1) Canopy con fill TENUE (alpha 0.18) sotto tutto.
      #  2) Yield, brix, fioritura come linee SOPRA.
      #  3) Pioggia (>=10mm) e irrigazione come barre su y2.
      p1 <- p1 |>
        # Canopy (line + fill tenue, sotto le linee)
        add_trace(x = cur$DATE, y = cur$fIntAct * 100,
                  type = "scatter", mode = "lines",
                  fill = "tozeroy", fillcolor = "rgba(76,175,80,0.18)",
                  name = "🌿 Canopy %",
                  line = list(color = "#558b2f", width = 1.8),
                  hovertemplate = "Canopy: %{y:.1f}%<extra></extra>",
                  inherit = FALSE)

      # Fioritura — sempre disegnata. Scaliamo (0..1) -> (0..FLO_SCALE) per
      # essere visibile sul grafico canopy/yield. La forma a campana resta.
      if ("floweringRateAct" %in% names(cur) ||
          "floweringStateAct" %in% names(cur)) {
        # Preferiamo floweringRateAct (campana) se c'e'. Altrimenti usiamo
        # un proxy come derivata discreta di floweringStateAct.
        flo_y <- if ("floweringRateAct" %in% names(cur)) {
          as.numeric(cur$floweringRateAct)
        } else {
          fs <- as.numeric(cur$floweringStateAct)
          c(0, diff(fs))
        }
        flo_max_loc <- max(flo_y, na.rm = TRUE)
        if (!is.finite(flo_max_loc) || flo_max_loc <= 0) flo_max_loc <- 1
        flo_norm <- pmin(flo_y / flo_max_loc, 1)        # 0..1
        flo_disp <- flo_norm * FLO_SCALE                # 0..80
        # Round 11: fiore di pomodoro = GIALLO (non rosa).
        p1 <- p1 |>
          add_trace(x = cur$DATE, y = flo_disp,
                    type = "scatter", mode = "lines",
                    name = if (lang == "it") "🌼 Fioritura" else "🌼 Flowering",
                    line = list(color = "#fbc02d", width = 3.5,
                                shape = "spline", smoothing = 1),
                    fill = "tozeroy",
                    fillcolor = "rgba(255, 193, 7, 0.25)",
                    customdata = flo_norm,
                    hovertemplate = "Fioritura: %{customdata:.2f} (norm.)<extra></extra>",
                    inherit = FALSE)
      }
      p1 <- p1 |>
        # Yield (viola)
        add_trace(x = cur$DATE, y = cur$fruitFreshWeightAct / 100,
                  type = "scatter", mode = "lines",
                  name = "🍅 Yield (t/ha)",
                  line = list(color = "#6a1b9a", width = 2.8),
                  hovertemplate = "Yield: %{y:.1f} t/ha<extra></extra>",
                  inherit = FALSE) |>
        # Brix (rosso) — Round 7: il run deterministico (cur) ha brix=0
        # per quasi tutto il ciclo (il modello calcola brix solo a maturita',
        # vedi BRIX_model in Main.R). Per dare all'agricoltore un'idea della
        # CURVA di brix completa, sostituiamo cur$brixAct con la mediana
        # ensemble (env_ens$brixAct_p50) che proietta tutta la stagione fino
        # al raccolto. Pre-flowering resta 0 (corretto).
        # Fall-back: se ensemble non c'e', usiamo cur$brixAct.
        # Round 17: usa brixAct_calc (calcolato manualmente da
        # carbonSugarState / fruitFreshWeightAct, vedi current_run).
        add_trace(x = cur$DATE,
                  y = {
                    bx <- if ("brixAct_calc" %in% names(cur))
                            as.numeric(cur$brixAct_calc)
                          else as.numeric(cur$brixAct)
                    bx[!is.finite(bx)] <- 0
                    bx * BRIX_SCALE
                  },
                  type = "scatter", mode = "lines",
                  name = "🍯 Brix (×6, °)",
                  line = list(color = "#c62828", width = 3,
                              shape = "spline", smoothing = 1),
                  connectgaps = TRUE,
                  customdata = {
                    bx <- if ("brixAct_calc" %in% names(cur))
                            as.numeric(cur$brixAct_calc)
                          else as.numeric(cur$brixAct)
                    bx[!is.finite(bx)] <- 0
                    bx
                  },
                  hovertemplate = "Brix: %{customdata:.2f}°<extra></extra>",
                  inherit = FALSE) |>
        # Pioggia EFFICACE (>= 10 mm) — bars on y2.
        add_trace(x = cur$DATE,
                  y = ifelse(is.finite(cur$p) & cur$p >= .RAIN_DAY_MM,
                             cur$p, 0),
                  type = "bar",
                  name = sprintf(if (lang == "it") "🌧 Pioggia >=%.0f mm" else "🌧 Rain >=%.0f mm", .RAIN_DAY_MM),
                  marker = list(color = "rgba(25,118,210,0.75)"),
                  hovertemplate = "Pioggia: %{y:.0f} mm<extra></extra>",
                  yaxis = "y2", inherit = FALSE) |>
        # Irrigazione consigliata dal modello (rosso, asse y2). Disegnata
        # con un bordo cosi' si vede chiaramente vs. pioggia blu.
        add_trace(x = cur$DATE, y = cur$irrigation,
                  type = "bar", name = if (lang == "it") "💧 Irrig. consigliata" else "💧 Suggested irrig.",
                  marker = list(color = "rgba(229,57,53,0.95)",
                                line = list(color = "#b71c1c", width = 0.5)),
                  yaxis = "y2", inherit = FALSE)

      # Sovrapponiamo le OVERRIDE dell'agricoltore (skip + applied) come
      # marker addizionali su y2. Cosi' a colpo d'occhio:
      #   barra rossa  = consiglio del modello (puo' essere anche saltato)
      #   X grigia     = consigliata MA saltata (sopra alla barra)
      #   barra verde  = irrigazione FATTA dall'agricoltore (in piu')
      ovr <- irr_overrides()
      if (length(ovr)) {
        skip_keys <- names(ovr)[vapply(ovr,
          function(x) identical(x$action, "skip"), logical(1))]
        appl_keys <- names(ovr)[vapply(ovr,
          function(x) identical(x$action, "applied"), logical(1))]

        # X sui giorni saltati (asse y2, alla stessa altezza della barra
        # consigliata, simbolo "x" grigio scuro)
        if (length(skip_keys)) {
          skip_dates <- as.Date(skip_keys)
          skip_y <- vapply(skip_dates, function(d) {
            ix <- which(as.Date(cur$DATE) == d)
            if (length(ix)) as.numeric(cur$irrigation[ix[1]]) else 0
          }, numeric(1))
          p1 <- p1 |>
            add_trace(x = skip_dates, y = skip_y,
                      type = "scatter", mode = "markers",
                      name = if (lang == "it") "🚫 Saltata" else "🚫 Skipped",
                      marker = list(symbol = "x-thin", size = 14,
                                    color = "#424242",
                                    line = list(color = "#212121",
                                                width = 2.5)),
                      yaxis = "y2", inherit = FALSE,
                      hovertemplate = if (lang == "it") "Saltata: %{y:.0f} mm<extra></extra>" else "Skipped: %{y:.0f} mm<extra></extra>")
        }

        # Barre verdi per le irrigazioni FATTE dall'agricoltore
        if (length(appl_keys)) {
          appl_dates <- as.Date(appl_keys)
          appl_mm <- vapply(appl_keys, function(k)
                            as.numeric(ovr[[k]]$mm %||% 0),
                            numeric(1))
          p1 <- p1 |>
            add_trace(x = appl_dates, y = appl_mm,
                      type = "bar", name = if (lang == "it") "💚 Fatta da te" else "💚 Manual",
                      marker = list(color = "rgba(46,125,50,0.92)",
                                    line = list(color = "#1b5e20",
                                                width = 0.5)),
                      yaxis = "y2", inherit = FALSE,
                      hovertemplate = if (lang == "it") "Tu hai irrigato: %{y:.0f} mm<extra></extra>" else "You irrigated: %{y:.0f} mm<extra></extra>")
        }
      }
    }

    # Rainfed baseline (dashed grey): shown in freeze mode for comparison
    if (has_rf && "fruitFreshWeightAct" %in% names(rf)) {
      p1 <- p1 |>
        add_trace(x = rf$DATE, y = rf$fruitFreshWeightAct / 100,
                  type = "scatter", mode = "lines",
                  name = if (lang %in% c("it","foggiano")) "🌵 Senz'acqua (asciutto)" else "🌵 Rainfed (no irrig.)",
                  line = list(color = "rgba(120,120,120,0.6)", width = 1.8, dash = "dot"),
                  hovertemplate = if (lang %in% c("it","foggiano"))
                    "Senz'acqua: %{y:.1f} t/ha<extra></extra>"
                  else
                    "Rainfed: %{y:.1f} t/ha<extra></extra>",
                  inherit = FALSE)
    }

    # Round 11: titoli asse Y CORTI per non sovrapporsi. Le scale sono
    # gia' chiare dalla legenda; il titolo dice solo "scala unificata".
    p1 <- p1 |> layout(
      yaxis  = list(title = list(text = if (lang == "it") "scala (vedi legenda)" else "scale (see legend)",
                                 font = list(size = 10, color = "#999"),
                                 standoff = 6),
                    range = c(0, 150), zeroline = TRUE),
      yaxis2 = list(title = list(text = if (lang == "it") "mm/giorno" else "mm/day",
                                 font = list(size = 10, color = "#999")),
                    overlaying = "y",
                    side = "right", showgrid = FALSE,
                    range = c(0, 50)),
      shapes = base_shapes,
      barmode = "group"
    )

    # ============== Panel 2: ACQUA NEL SUOLO + WATER STRESS ==============
    # Asse 0..100 (% di acqua disponibile nel suolo). Niente "FTSW".
    # Sovrapposto: water stress come "stress %" (linea rossa, 0=ok, 100=stress).
    # Tre soglie per fase fenologica disegnate solo durante la fase relativa.
    # Round 13: tolta linea tratteggiata mediana proiezione (ridondante).
    # Resta solo il ribbon storico P10-P90 sotto.
    p2 <- plot_ly() |>
      add_ribbon("ftsw", "rgba(63,81,181,0.18)", "#3949ab",
                 "Acqua nel suolo storico", scale = 100)

    if (has_cur && "ftsw" %in% names(cur)) {
      # Round 19: SOLO linea, no fill (l'utente ha chiesto "linea non area")
      p2 <- p2 |>
        add_trace(x = cur$DATE, y = cur$ftsw * 100,
                  type = "scatter", mode = "lines",
                  name = if (lang %in% c("it","foggiano")) "\U0001F4A7 Acqua nel suolo (%)" else "\U0001F4A7 Soil water (%)",
                  line = list(color = "#1565c0", width = 2.6),
                  customdata = cur$ftsw,
                  hovertemplate = if (lang %in% c("it","foggiano")) "Acqua nel suolo: %{y:.0f}%<extra></extra>" else "Soil water: %{y:.0f}%<extra></extra>",
                  inherit = FALSE)
    }

    # Rainfed ftsw (dashed grey) in freeze mode
    if (has_rf && "ftsw" %in% names(rf)) {
      p2 <- p2 |>
        add_trace(x = rf$DATE, y = rf$ftsw * 100,
                  type = "scatter", mode = "lines",
                  name = if (lang %in% c("it","foggiano")) "🌵 Acqua (asciutto)" else "🌵 Soil water (rainfed)",
                  line = list(color = "rgba(120,120,120,0.55)", width = 1.6, dash = "dot"),
                  hovertemplate = "Rainfed soil water: %{y:.0f}%<extra></extra>",
                  inherit = FALSE)
    }

    # Stress idrico (1 - waterStress per leggibilita': 0=ok, 100=stress max)
    if (has_cur && "waterStress" %in% names(cur)) {
      stress_pct <- (1 - cur$waterStress) * 100
      p2 <- p2 |>
        add_trace(x = cur$DATE, y = stress_pct,
                  type = "scatter", mode = "lines",
                  name = if (lang %in% c("it","foggiano")) "\U0001F975 Stress idrico (%)" else "\U0001F975 Water stress (%)",
                  line = list(color = "#c62828", width = 2.2, dash = "dot"),
                  hovertemplate = "Stress idrico: %{y:.0f}%<extra></extra>",
                  inherit = FALSE)
    }

    # Marker sul valore di OGGI (acqua nel suolo)
    today_marker_shape <- list()
    today_marker_ann   <- list()
    if (has_cur) {
      it <- which(as.Date(cur$DATE) == today)
      if (length(it)) {
        wpct_today <- as.numeric(cur$ftsw[it]) * 100
        today_marker_shape <- list(
          list(type = "circle", xref = "x", yref = "y",
               x0 = today - 1L, x1 = today + 1L,
               y0 = wpct_today - 2.5, y1 = wpct_today + 2.5,
               fillcolor = "#d32f2f",
               line = list(color = "#d32f2f", width = 1))
        )
        today_marker_ann <- list(list(
          x = today, y = wpct_today + 8, xref = "x", yref = "y",
          text = sprintf(if (lang == "it") "oggi: %.0f%%" else "today: %.0f%%", wpct_today),
          showarrow = FALSE,
          font = list(size = 11, color = "#d32f2f"),
          bgcolor = "rgba(255,255,255,0.85)"
        ))
      }
    }

    # ---- Soglie water stress per fase, riportate su scala 0..100 --------
    # (mostriamo come "soglia stress %" = (1 - ws_threshold) * 100 cosi' chi
    # legge il grafico vede dove il modello inizia a irrigare).
    p2_shapes <- c(base_shapes, today_marker_shape)
    p2_anns   <- today_marker_ann
    phase_threshold_segment <- function(phase_code, ws_threshold,
                                        color, label) {
      if (!has_cur) return(NULL)
      if (!("phenoCode" %in% names(cur))) return(NULL)
      idx <- which(as.integer(cur$phenoCode) == phase_code)
      if (!length(idx)) return(NULL)
      x0 <- as.Date(min(cur$DATE[idx]))
      x1 <- as.Date(max(cur$DATE[idx]))
      stress_pct_threshold <- (1 - ws_threshold) * 100
      shp <- list(
        type = "line", xref = "x", yref = "y",
        x0 = x0, x1 = x1,
        y0 = stress_pct_threshold, y1 = stress_pct_threshold,
        line = list(color = color, width = 2.2, dash = "dash")
      )
      # Round 17: label SOLO icona+nome breve, posta sull'INIZIO del
      # segmento (non al centro), font 9, no soglia (rumore).
      ann <- list(
        x = x0 + 1L,
        y = stress_pct_threshold + 4,
        xref = "x", yref = "y",
        text = label,
        showarrow = FALSE,
        font = list(size = 9, color = color),
        bgcolor = "rgba(255,255,255,0.92)",
        borderpad = 1, xanchor = "left"
      )
      list(shape = shp, ann = ann)
    }

    # Round 12: FIX phenoCode mapping. Il modello cumba usa
    #   phenoCode = 0 (pre-trapianto)
    #   phenoCode = 1 -> VEGETATIVA
    #   phenoCode = 2 -> RIPRODUTTIVA
    #   phenoCode = 3 -> RIPENING / MATURAZIONE
    # Prima cercavo 2,3,4: la maturazione non veniva MAI disegnata.
    seg_v <- phase_threshold_segment(1L, as.numeric(input$ws_veg),
                                     "#43a047", if (lang == "it") "🌱 Vegetativa" else "🌱 Vegetative")
    seg_r <- phase_threshold_segment(2L, as.numeric(input$ws_rep),
                                     "#1e88e5", if (lang == "it") "🌸 Riproduttiva" else "🌸 Reproductive")
    seg_p <- phase_threshold_segment(3L, as.numeric(input$ws_rip),
                                     "#fb8c00", if (lang == "it") "🍅 Maturazione" else "🍅 Ripening")
    for (s in list(seg_v, seg_r, seg_p)) {
      if (!is.null(s)) {
        p2_shapes <- c(p2_shapes, list(s$shape))
        p2_anns   <- c(p2_anns,   list(s$ann))
      }
    }

    p2 <- p2 |> layout(
      yaxis  = list(title = list(text = if (lang %in% c("it","foggiano")) "% acqua / stress" else "% water / stress",
                                  font = list(size = 10, color = "#999"),
                                  standoff = 6),
                    range = c(0, 105), zeroline = TRUE),
      shapes = p2_shapes,
      annotations = p2_anns
    )

    # ============== Stitch panels (Round 6 layout fix) ===================
    # Round 6: i titoli paper-coords si sovrapponevano ai dati. Sposto tutto
    # nel margine superiore con `t = 90` e uso un titolo unico in alto +
    # un titolo sopra il pannello 2 a y = 0.46. Aggiungo bgcolor bianco ai
    # titoli per non sovrapporsi alle linee del grafico.
    sp <- subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE,
                  heights = c(0.55, 0.45)) |>
      layout(paper_bgcolor = "#ffffff",
             plot_bgcolor  = "#ffffff",
             # Round 8: legenda ORIZZONTALE in basso, font piu' grande.
             legend = list(orientation = "h",
                           x = 0.5, y = -0.22, xanchor = "center",
                           font = list(size = 12, color = "#222"),
                           bgcolor = "rgba(255,255,255,0.95)",
                           bordercolor = "#cfd8dc", borderwidth = 1,
                           itemsizing = "constant"),
             # Round 8: margine sinistro 75 cosi' i tick y non si
             # sovrappongono ai titoli; margine top 100 per i titoli
             # del pannello 1.
             margin = list(l = 75, r = 30, t = 100, b = 100),
             hovermode = "x unified",
             hoverlabel = list(font = list(size = 12)),
             xaxis = list(range = c(x_min, x_max),
                          title = "",
                          type = "date",
                          tickformat = "%d %b",
                          dtick = "M1",
                          tickfont = list(size = 11, color = "#222"),
                          showgrid = TRUE,
                          gridcolor = "#eef0f2",
                          ticks = "outside",
                          ticklen = 5))

    fan_lbl <- if (has_ens) " | inizio ventaglio" else ""
    # Round 8: titoli ben FUORI dalle aree dati. Pannello 1 a y=1.18
    # (dentro al margin top di 100px), pannello 2 a y=0.46 (fuori
    # dall'area dei dati, sopra al gap tra pannelli). Marker oggi/fine
    # forecast a y=1.05.
    top_anns <- list(
      list(x = 0, y = 1.18, xref = "paper", yref = "paper",
           text = if (lang == "it") "<b>🌿 Crescita: canopy, yield, fioritura, brix</b>" else "<b>🌿 Growth: canopy, yield, flowering, brix</b>",
           showarrow = FALSE,
           font = list(size = 14, color = "#2e7d32"),
           xanchor = "left"),
      list(x = 0, y = 0.46, xref = "paper", yref = "paper",
           text = if (lang == "it") "<b>💧 Acqua nel suolo + stress idrico</b>" else "<b>💧 Soil water + water stress</b>",
           showarrow = FALSE,
           font = list(size = 13, color = "#1565c0"),
           xanchor = "left",
           bgcolor = "rgba(255,255,255,0.97)",
           borderpad = 3),
      list(x = today, y = 1.04, xref = "x", yref = "paper",
           text = if (lang == "it") "oggi" else "today", showarrow = FALSE,
           font = list(size = 10, color = "#2e7d32"), xanchor = "center"),
      list(x = fc_end, y = 1.04, xref = "x", yref = "paper",
           text = paste0(if (lang == "it") "fine forecast" else "forecast end", fan_lbl), showarrow = FALSE,
           font = list(size = 10, color = "#ef6c00"), xanchor = "center")
    )
    if (!is.null(flo_peak_date)) {
      top_anns <- c(top_anns, list(list(
        x = flo_peak_date, y = 1.06, xref = "x", yref = "paper",
        text = if (lang == "it") "🌻 fioritura piena" else "🌻 full flowering", showarrow = FALSE,
        font = list(size = 10, color = "#fbc02d"), xanchor = "center")))
    }
    sp <- sp |> layout(annotations = top_anns)

    sp |> config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  # ===== Feedback agricoltore: tabella consigli vs fatti ==================
  # Mostra SOLO le righe (consigliate, saltate, applicate). Il titolo e il
  # form "aggiungi mia irrigazione" stanno in ui.R come UI STATICA, cosi' la
  # date/mm non si resettano ogni volta che le override cambiano.
  output$irrigation_feedback_rows <- renderUI({
    cur <- current_run()
    # Round 6: durante il ricalcolo current_run() puo' tornare NULL per un
    # istante. Usiamo req(..., cancelOutput=TRUE) cosi' il render NON resetta
    # la lista a "In attesa": Shiny lascia visibile l'ultima versione valida.
    req(cur, !is.null(cur) && nrow(cur) > 0, cancelOutput = TRUE)

    today <- Sys.Date()
    ovr   <- irr_overrides()

    # Round 18: date formatting — language-aware
    sched_lang <- input$language %||% "en"
    sched_it   <- sched_lang %in% c("it", "foggiano")
    mesi_it <- c("Gen","Feb","Mar","Apr","Mag","Giu",
                 "Lug","Ago","Set","Ott","Nov","Dic")
    gg_it   <- c("Dom","Lun","Mar","Mer","Gio","Ven","Sab")
    mesi_en <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
    gg_en   <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
    fmt_it <- function(d) {
      if (sched_it)
        sprintf("%s %d %s", gg_it[as.POSIXlt(d)$wday + 1L],
                as.integer(format(d, "%d")), mesi_it[as.integer(format(d, "%m"))])
      else
        sprintf("%s %d %s", gg_en[as.POSIXlt(d)$wday + 1L],
                as.integer(format(d, "%d")), mesi_en[as.integer(format(d, "%m"))])
    }

    # Round 18: assemblo lista UNICA di eventi (consigliate + applicate +
    # skip "fantasma") ORDINATA cronologicamente, cosi' l'agricoltore vede
    # tutto in linea temporale.
    events <- list()

    # 1) Consigliate dal modello
    rec_idx <- which(cur$irrigation > 0)
    for (i in seq_along(rec_idx)) {
      d   <- as.Date(cur$DATE[rec_idx[i]])
      key <- format(d, "%Y-%m-%d")
      mm  <- as.numeric(cur$irrigation[rec_idx[i]])
      is_skipped <- !is.null(ovr[[key]]) && identical(ovr[[key]]$action, "skip")
      events[[length(events) + 1L]] <- list(
        date = d, key = key, mm = mm,
        type = if (is_skipped) "skipped" else "suggested"
      )
    }

    # 2) Applicate manualmente
    applied_keys <- names(ovr)[vapply(ovr,
                                      function(x) identical(x$action, "applied"),
                                      logical(1))]
    for (key in applied_keys) {
      events[[length(events) + 1L]] <- list(
        date = as.Date(key), key = key,
        mm = as.numeric(ovr[[key]]$mm %||% 0), type = "applied"
      )
    }

    # 3) Skip "fantasma" — date saltate dall'agricoltore ma che il modello
    # non consiglia piu' (perche' ha ricalcolato dopo lo skip). L'utente
    # deve poterli vedere e ripristinare.
    skip_keys <- names(ovr)[vapply(ovr,
                                   function(x) identical(x$action, "skip"),
                                   logical(1))]
    seen_skip <- vapply(events,
                        function(e) e$type == "skipped" && e$key %in% skip_keys,
                        logical(1))
    seen_skip_keys <- if (length(seen_skip) && any(seen_skip))
                        vapply(events[seen_skip], function(e) e$key,
                               character(1))
                      else character(0)
    ghost_skips <- setdiff(skip_keys, seen_skip_keys)
    for (key in ghost_skips) {
      events[[length(events) + 1L]] <- list(
        date = as.Date(key), key = key, mm = 0, type = "ghost_skip"
      )
    }

    # freezeMode: mostra solo le scelte dell'utente (applied + ghost_skip),
    # non le irrigazioni suggerite dal modello (suggested / skipped auto)
    if (isTRUE(input$freezeMode))
      events <- Filter(function(e) e$type %in% c("applied", "ghost_skip"), events)

    # Ordina cronologicamente
    if (length(events)) {
      ord <- order(vapply(events, function(e) as.numeric(e$date), numeric(1)))
      events <- events[ord]
    }

    rows <- lapply(events, function(e) {
      d <- e$date; key <- e$key; mm <- e$mm; type <- e$type
      is_past <- d < today
      cls <- switch(type,
        "suggested" = if (is_past) "suggested past" else "suggested",
        "skipped"   = "skipped",
        "applied"   = "applied",
        "ghost_skip" = "skipped ghost"
      )
      btn <- switch(type,
        "suggested"  = tags$button(class = "irrf-act btn-skip",
                                   `data-act` = "skip",
                                   `data-date` = key,
                                   if (sched_it) "✕ salto" else "✕ skip"),
        "skipped"    = tags$button(class = "irrf-act btn-undo",
                                   `data-act` = "undo-skip",
                                   `data-date` = key,
                                   if (sched_it) "↩ ripristina" else "↩ restore"),
        "ghost_skip" = tags$button(class = "irrf-act btn-undo",
                                   `data-act` = "undo-skip",
                                   `data-date` = key,
                                   if (sched_it) "↩ annulla" else "↩ undo"),
        "applied"    = tags$button(class = "irrf-act btn-remove",
                                   `data-act` = "remove-applied",
                                   `data-date` = key, "🗑")
      )
      div(class = paste("irrf-row", cls),
          div(class = "irrf-date", fmt_it(d)),
          div(class = "irrf-mm",   sprintf("%.0f mm", mm)),
          div(class = "irrf-actions", btn))
    })

    if (!length(rows)) {
      rows <- list(div(class = "irrf-row empty",
                       em(if (sched_it) "Nessuna irrigazione in questa stagione."
                          else "No irrigation events this season.")))
    }

    do.call(tagList, rows)
  })

  # ===== LLM interpretation ===============================================
  llm_text <- reactiveVal(NULL)

  # Costruisce il summary text. Estratto in funzione cosi' lo usano sia il
  # bottone "Spiega" sia l'auto-fire.
  build_llm_summary <- function() {
    cur <- current_run()
    h   <- historical_runs()
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    today  <- Sys.Date()

    hist_summary <- ""
    if (!is.null(h) && nrow(h)) {
      last_h <- h |> group_by(year) |> slice_tail(n = 1L) |> ungroup()
      hist_y <- last_h$fruitFreshWeightAct / 100
      hist_b <- last_h$brixAct
      tot <- tapply(h$irrigation, h$year, sum, na.rm = TRUE)
      cnt <- tapply(h$irrigation > 0, h$year, sum, na.rm = TRUE)
      pct_today <- ftsw_percentile_today(cur, h, today)
      pct_line <- if (is.finite(pct_today))
        sprintf("- Soil water today vs historical same DOY: percentile %.0f%% (0=driest ever, 100=wettest).\n",
                pct_today * 100) else ""
      hist_summary <- paste0(sprintf(
        "- Historical baseline (%d years): median yield %.1f t/ha (P10=%.1f, P90=%.1f); brix %.2f; median irrigation %.0f mm in %.0f events.\n",
        length(unique(h$year)),
        median(hist_y, na.rm = TRUE),
        quantile(hist_y, .10, na.rm = TRUE),
        quantile(hist_y, .90, na.rm = TRUE),
        median(hist_b, na.rm = TRUE),
        median(tot, na.rm = TRUE),
        median(cnt, na.rm = TRUE)
      ), pct_line)
    }

    pt <- selected_point()
    pt_lat <- if (!is.null(pt)) pt[2] else NA
    pt_lon <- if (!is.null(pt)) pt[1] else NA

    if (is.null(cur) || !nrow(cur)) {
      return(sprintf(
        "Stagione %d non ancora avviata (pre-trapianto o senza dati).
- Sito: %.3f deg N, %.3f deg E
- Trapianto: DOY %d
- Strategia: veg WS=%.2f/turn=%dd, repr WS=%.2f/turn=%dd, ripen WS=%.2f/turn=%dd
%s
Compito: 4-5 frasi su cosa aspettarsi alla luce dello storico e della strategia.",
        cur_yr, pt_lat, pt_lon,
        transplantingDOY(),
        input$ws_veg, as.integer(input$turn_veg),
        input$ws_rep, as.integer(input$turn_rep),
        input$ws_rip, as.integer(input$turn_rip),
        hist_summary))
    }

    yield_t <- tail(cur$fruitFreshWeightAct, 1L) / 100
    if (!is.finite(yield_t)) yield_t <- tail(cur$fruitsStateAct, 1L) / 100
    n_irr  <- sum(cur$irrigation > 0, na.rm = TRUE)
    irr_mm <- sum(cur$irrigation, na.rm = TRUE)
    n_fc   <- sum(cur$is_forecast, na.rm = TRUE)

    # FTSW e azione di oggi — always EN for hidden LLM context
    info_en <- today_action(cur, weather_data(),
                            transplantingDOY  = transplantingDOY(),
                            depletionFraction = input$DepletionFraction,
                            lang              = "en")
    today_line <- sprintf("- TODAY's recommended action: %s. %s\n",
                          info_en$headline, info_en$detail)
    fc_line <- if (n_fc > 0L) sprintf(
      "- Last %d days of run are FORECAST (Open-Meteo +16d).\n",
      n_fc) else ""

    # Riassunto meteo prossimi 3 gg
    om_now <- weather_data()
    w3_line <- ""
    if (!is.null(om_now)) w3_line <- sprintf("- Weather 3d: %s\n",
                                             weather_3day_summary(om_now, lang = "en"))

    # Feedback agricoltore: irrigazioni saltate / aggiunte (override)
    fb_line <- ""
    ovr_now <- irr_overrides()
    if (length(ovr_now)) {
      sk <- vapply(ovr_now,
                   function(x) identical(x$action, "skip"),
                   logical(1))
      ap <- vapply(ovr_now,
                   function(x) identical(x$action, "applied"),
                   logical(1))
      sk_n  <- sum(sk)
      ap_n  <- sum(ap)
      ap_mm <- if (any(ap)) sum(vapply(ovr_now[ap],
                                       function(x) as.numeric(x$mm %||% 0),
                                       numeric(1)), na.rm = TRUE) else 0
      fb_line <- sprintf(
        "- FARMER FEEDBACK: %d recommended events SKIPPED, %d irrigations added (%.0f mm total).\n",
        sk_n, ap_n, ap_mm)
    }

    # Quantili ensemble fine ciclo
    ens_now <- current_run_ensemble()
    ens_line <- ""
    if (!is.null(ens_now) && nrow(ens_now)) {
      yq <- tapply(ens_now$fruitFreshWeightAct, ens_now$template_year,
                   function(x) {
                     x <- x[is.finite(x)]
                     if (length(x)) tail(x, 1L) / 100 else NA_real_
                   })
      yq <- yq[is.finite(yq)]
      if (length(yq) >= 2L)
        ens_line <- sprintf(
          "- Ensemble (historical analogues N=%d): projected yield P10/P50/P90 = %.1f / %.1f / %.1f t/ha.\n",
          length(yq),
          quantile(yq, .10, na.rm = TRUE),
          quantile(yq, .50, na.rm = TRUE),
          quantile(yq, .90, na.rm = TRUE))
    }

    sprintf(
      "CUMBA results — processing tomato, season %d, date %s.
- Site: %.3f deg N, %.3f deg E
- Transplanting: DOY %d   Cycle: %d degree-days
- Irrigation strategy: veg WS=%.2f/turn=%dd, repr WS=%.2f/turn=%dd, ripen WS=%.2f/turn=%dd
- Current run: projected yield %.1f t/ha, brix %.2f deg, %d events for %.0f mm total, min WS %.2f
%s%s%s%s%s%s
Task: 4-5 short sentences. What to do in the next few days? How does it compare to historical baseline?
One concrete slider change if needed. If the farmer has skipped/added irrigations, briefly comment on the choice.
LANGUAGE INSTRUCTION: Respond in the same language the user writes in.",
      cur_yr, format(today, "%d %B %Y"),
      pt_lat, pt_lon,
      transplantingDOY(), as.integer(input$CycleLength),
      input$ws_veg, as.integer(input$turn_veg),
      input$ws_rep, as.integer(input$turn_rep),
      input$ws_rip, as.integer(input$turn_rip),
      yield_t, tail(cur$brixAct, 1L), n_irr, irr_mm,
      min(cur$waterStress, na.rm = TRUE),
      today_line, w3_line, fb_line, ens_line, hist_summary, fc_line)
  }

  # Stato dell'ultima chiamata LLM:
  # status: "idle"|"loading"|"ok"|"fallback"  + note diagnostica + diag (testo
  # tecnico opzionale con HTTP/model attempts).
  llm_state <- reactiveVal(list(status = "idle", note = NULL, diag = NULL))

  run_llm <- function() {
    summary_text <- build_llm_summary()
    llm_state(list(status = "loading", note = NULL, diag = NULL))
    # Resolve active backend for diagnostics
    bk_info <- .llm_resolve_backend(input$user_llm_key %||% NULL)
    message(sprintf("[run_llm] backend=%s key_present=%s",
                    bk_info$name %||% "none",
                    nzchar(bk_info$key)))
    withProgress(message = "🤖 CUMBA is thinking...", value = 0.4, {
      txt <- tryCatch(
        interpret_with_claude(summary_text,
                              language    = "en",  # auto-summary always EN; user drives lang via chat
                              runtime_key = input$user_llm_key %||% NULL),
        error = function(e) {
          message("[run_llm] interpret_with_claude ERROR: ", conditionMessage(e))
          structure(sprintf("Network error: %s", conditionMessage(e)),
                    class = c("cumba_llm_err", "character"))
        })
      setProgress(1)
    })
    message(sprintf("[run_llm] risposta inherits(cumba_llm_err)=%s, nchar=%d",
                    inherits(txt, "cumba_llm_err"),
                    if (is.null(txt)) 0L else nchar(as.character(txt))))

    # Determina se la risposta e' un errore. Il nostro interpret_with_llm
    # marca i fallimenti con classe "cumba_llm_err"; come safety net usa
    # anche heuristic sui prefissi diagnostici.
    is_err <- inherits(txt, "cumba_llm_err") ||
              is.null(txt) || !nzchar(txt) ||
              grepl("^\\[KEY_MISSING\\]", txt) ||
              grepl("^\\[ALL_RATE_LIMITED\\]", txt) ||
              grepl("^\\[LLM_HTTP_", txt) ||
              grepl("^Errore LLM", txt)

    if (is_err) {
      lang_fb <- input$language %||% "en"
      # ---- 1) Fallback rule-based message ---------------------------
      fb <- synth_message(today_info(), weather_data(), current_run(),
                          current_run_ensemble(), lang = lang_fb)
      if (!nzchar(fb))
        fb <- if (lang_fb == "en")
          "Select a field on the map and I'll tell you how things are going."
        else
          "Aspetto che tu scelga un sito sulla mappa, poi ti racconto come va il campo."
      llm_text(fb)

      # ---- 2) User-facing diagnostic --------------------------------
      raw <- as.character(txt)
      note <- if (grepl("\\[KEY_MISSING\\]", raw))
                if (lang_fb == "en")
                  paste("No LLM key set. Using rule-based fallback (reliable but less",
                        "conversational). For free LLM: get a Groq key at console.groq.com,",
                        "then set GROQ_API_KEY in your environment or enter it in the UI.")
                else
                  paste("Chiave LLM non impostata. Ti rispondo con le regole.",
                        "Per attivare l'LLM gratis: console.groq.com > API Keys.")
              else if (grepl("\\[ALL_RATE_LIMITED\\]", raw))
                if (lang_fb == "en")
                  paste("LLM rate-limited. Using rule-based fallback. Retry in 1-2 minutes.")
                else
                  paste("Modelli LLM saturi. Riprova fra 1-2 minuti.")
              else if (grepl("\\[LLM_HTTP_", raw))
                if (lang_fb == "en")
                  paste("LLM returned an HTTP error. Using rule-based fallback.",
                        "Open the technical detail to see what happened.")
                else
                  paste("Errore HTTP dall'LLM. Uso il fallback rule-based.",
                        "Apri il dettaglio per i dettagli.")
              else
                if (lang_fb == "en")
                  paste("LLM did not respond. Using rule-based fallback — see detail for attempts.")
                else
                  paste("L'LLM non ha risposto. Uso il fallback rule-based.")

      diag <- if (grepl("Tentativi:|LLM error", raw)) raw else NULL
      llm_state(list(status = "fallback", note = note, diag = diag))
    } else {
      llm_text(as.character(txt))
      llm_state(list(status = "ok", note = NULL, diag = NULL))
    }
  }

  observeEvent(input$interpretBtn, { run_llm() })

  # ===== M11: CHATBOT MULTI-TURN =========================================
  # chat_history e' la lista di messaggi (role=user|assistant, content=string)
  # che vengono mandati al LLM ad ogni follow-up. Il primo messaggio user
  # invisible viene generato da build_llm_summary (dati simulazione + meteo),
  # poi l'utente puo' chiedere chiarimenti, alternative strategiche, ecc.
  chat_history       <- reactiveVal(list())
  # Flag: TRUE when llm_text was set via chatSendBtn (multi-turn follow-up).
  llm_is_chat        <- reactiveVal(FALSE)
  # Flag: TRUE when agent has applied slider changes and is waiting for re-run.
  agent_pending      <- reactiveVal(FALSE)
  # Flag: TRUE when run_llm() was triggered by the agent (not autoLLM).
  # Tells the llm_text observer to APPEND rather than RESET chat history.
  agent_llm_triggered <- reactiveVal(FALSE)
  # Stores pending proposals by ID: list(proposal_id = list_of_intents, ...)
  agent_proposals    <- reactiveVal(list())

  # When run_llm() fires, update chat history.
  # Three cases:
  #   llm_is_chat=TRUE       → follow-up chat; chatSendBtn already updated history
  #   agent_llm_triggered=TRUE → append agent commentary to existing history
  #   otherwise               → auto-summary; RESET to [hidden-context + response]
  observeEvent(llm_text(), {
    if (isTRUE(llm_is_chat())) { llm_is_chat(FALSE); return() }
    txt <- llm_text()
    if (is.null(txt) || !nzchar(txt)) return()
    auto_entry <- list(role = "assistant", content = as.character(txt))
    if (isTRUE(agent_llm_triggered())) {
      agent_llm_triggered(FALSE)
      chat_history(c(chat_history(), list(auto_entry)))
    } else {
      # Auto-summary: always reset to a clean [hidden-context, response] pair
      summary_text <- tryCatch(build_llm_summary(), error = function(e) "")
      chat_history(list(
        list(role = "user", content = as.character(summary_text), hidden = TRUE),
        auto_entry
      ))
    }
  }, ignoreNULL = TRUE)

  # Reset chat
  observeEvent(input$chatResetBtn, {
    chat_history(list())
    llm_text(NULL)
    showNotification(
      if ((input$language %||% "en") == "en") "New conversation with CUMBA."
      else "Nuova conversazione con CUMBA.",
      type = "default", duration = 2)
  }, ignoreInit = TRUE)

  # ===== Chat thread renderer =============================================
  output$interpretation_text <- renderUI({
    history <- chat_history()
    lang    <- input$language %||% "en"
    it      <- lang %in% c("it", "foggiano")
    if (length(history) == 0L) {
      return(div(class = "chat-msg chat-msg-empty",
                 div(class = "chat-msg-body",
                     if (it) "\U0001f449 premi ↻ per un briefing, o scrivi una domanda qui sotto."
                     else    "\U0001f449 press ↻ for an agronomic briefing, or type a question below.")))
    }
    visible <- Filter(function(m) !isTRUE(m$hidden), history)
    if (length(visible) == 0L)
      return(div(class = "chat-msg chat-msg-empty",
                 div(class = "chat-msg-body",
                     if (it) "⏳ CUMBA sta elaborando..."
                     else    "⏳ CUMBA is thinking...")))
    div(class = "chat-stream",
        lapply(visible, function(m) {
          is_user <- identical(m$role, "user")
          is_err  <- grepl("^\\[|^Errore|^Error|^Network|^LLM error", m$content %||% "")
          cls  <- if (is_user) "chat-msg chat-msg-user"
                  else if (is_err) "chat-msg chat-msg-error"
                  else "chat-msg chat-msg-assistant"
          icon <- if (is_user) "\U0001f464"
                  else tags$img(src = "cumba_avatar.png",
                                style = "width:20px;height:20px;object-fit:contain;",
                                onerror = "this.replaceWith(document.createTextNode('\U0001f345'))")
          div(class = cls,
              div(class = "chat-msg-icon", icon),
              div(class = "chat-msg-body",
                  HTML(gsub("\n", "<br>",
                            htmltools::htmlEscape(m$content %||% "")))))
        }))
  })

  # ===== Chat send handler =================================================
  observeEvent(input$chatSendBtn, {
    msg <- trimws(isolate(input$chatInput) %||% "")
    if (!nzchar(msg)) return()
    updateTextAreaInput(session, "chatInput", value = "")
    # Detect language from the user's actual message, not the UI setting.
    ui_lang <- input$language %||% "en"
    has_it_chars <- grepl("[àèìòùáéíóúâêîôû]", msg, perl = TRUE)
    has_en_words <- grepl(
      "\\b(the|is|are|this|how|what|why|when|will|can|should|best|season|strategy|yield|brix|irrigation|water|crop|field|tomato|deficit|does|do|which|tell|explain|give|show|compare|help|maximise|maximize|improve|increase|reduce|decrease|achieve|optimal|advice|suggest|recommend)\\b",
      tolower(msg), perl = TRUE)
    has_it_words <- grepl(
      "\\b(come|cosa|quando|dove|perche|questo|questa|questi|quale|quali|posso|voglio|devo|bisogna|fare|avere|essere|con|per|del|della|dei|delle|nel|nella|nei|nelle|che|non|una|uno|ho|mi|si|ma|se|piu|meno|resa|irrigazione|irrigare|pomodoro|stagione|campo|acqua|strategia|deficit|varieta|raccolto|massimizzare|aumentare|ridurre|migliorare|ottimale|consiglio|suggerisci|dimmi|spiega|mostra|confronta|aiutami)\\b",
      tolower(msg), perl = TRUE)
    lang <- if      (has_it_chars || has_it_words) ui_lang   # Italian/dialect
            else if (has_en_words)                 "en"       # clearly English
            else                                   ui_lang    # ambiguous: follow UI

    cur <- chat_history()

    # Seed hidden context if no user message yet
    if (!any(vapply(cur, function(m) identical(m$role, "user"), logical(1)))) {
      ctx <- tryCatch(build_llm_summary(), error = function(e) "No simulation data yet.")
      cur <- c(cur, list(list(role = "user", content = as.character(ctx), hidden = TRUE)))
    }

    cur <- c(cur, list(list(role = "user", content = msg, hidden = FALSE)))
    chat_history(cur)
    llm_is_chat(TRUE)
    llm_state(list(status = "loading", note = NULL, diag = NULL))

    msgs_for_llm <- lapply(cur, function(m) list(role = m$role, content = m$content))
    txt <- tryCatch(
      interpret_with_llm(msgs_for_llm,
                         language    = lang,
                         runtime_key = input$user_llm_key %||% NULL),
      error = function(e) structure(conditionMessage(e), class = c("cumba_llm_err","character"))
    )

    is_err <- inherits(txt, "cumba_llm_err") || is.null(txt) || !nzchar(as.character(txt %||% ""))
    if (is_err) {
      reply <- if (lang %in% c("it","foggiano")) "Spiacente, errore LLM. Riprova."
               else "Sorry, couldn't reach the LLM. Please retry."
      chat_history(c(cur, list(list(role = "assistant", content = reply, hidden = FALSE))))
      llm_state(list(status = "fallback", note = as.character(txt %||% ""), diag = NULL))
    } else {
      chat_history(c(cur, list(list(role = "assistant", content = as.character(txt), hidden = FALSE))))
      llm_state(list(status = "ok", note = NULL, diag = NULL))
    }
    llm_is_chat(FALSE)
  }, ignoreInit = TRUE)

  # LLM pulse notification (toolbar badge)
  observeEvent(llm_text(), {
    if (!is.null(llm_text()))
      session$sendCustomMessage("cumba_llm_pulse", TRUE)
  }, ignoreNULL = TRUE)
  observeEvent(input$llm_modal_open, {
    a <- input$llm_modal_open
    if (isTRUE(a$open))
      session$sendCustomMessage("cumba_llm_pulse", FALSE)
  }, ignoreInit = TRUE)

  output$llm_status <- renderUI({
    st <- llm_state()
    if (is.null(st) || st$status == "idle" || st$status == "ok") return(NULL)
    if (st$status == "loading") {
      return(div(class = "llm-status", "\U0001f916 sto pensando..."))
    }
    if (st$status == "fallback") {
      note_html <- if (!is.null(st$note))
        div(style = "color:#bf360c;",
            HTML(paste0("⚠ ", htmltools::htmlEscape(st$note))))
        else NULL
      diag_html <- if (!is.null(st$diag) && nzchar(st$diag))
        tags$details(style = "margin-top:6px;",
                     tags$summary("Mostra dettaglio tecnico",
                                  style = "cursor:pointer; font-size:11px; color:#6d4c41;"),
                     tags$pre(st$diag,
                              style = paste("font-size:10.5px; background:#fff;",
                                            "padding:6px 8px; border-radius:4px;",
                                            "border:1px solid #eee; margin-top:4px;",
                                            "white-space:pre-wrap;")))
        else NULL
      return(div(class = "llm-status", note_html, diag_html))
    }
    NULL
  })

  # Pill colorata accanto al titolo "CUMBA spiega"
  output$llm_pill <- renderUI({
    st <- llm_state()
    if (is.null(st) || st$status == "idle") return(NULL)
    if (st$status == "loading")
      return(span(class = "llm-pill fb", "..."))
    if (st$status == "ok")
      return(span(class = "llm-pill ok", "LLM"))
    if (st$status == "fallback")
      return(span(class = "llm-pill fb",
                  title = if (!is.null(st$note)) st$note else "",
                  "REGOLE"))
    NULL
  })

  # Prevent Shiny from suspending outputs inside display:none modal panels
  outputOptions(output, "interpretation_text", suspendWhenHidden = FALSE)
  outputOptions(output, "llm_status",          suspendWhenHidden = FALSE)
  outputOptions(output, "llm_pill",            suspendWhenHidden = FALSE)
  outputOptions(output, "learningSliderUI",    suspendWhenHidden = FALSE)
  outputOptions(output, "learningKpiBox",      suspendWhenHidden = FALSE)
  outputOptions(output, "learningComment",     suspendWhenHidden = FALSE)
}
