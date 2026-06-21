# test_irrigationStopCycle.R
# Verifica che cumba_scenario smetta di irrigare quando cycleCompletion
# supera la soglia irrigationStopCycle.
#
# Atteso:
#   - Con irrigationStopCycle = 100 (default) -> irrigazioni per tutto il ciclo
#   - Con irrigationStopCycle = 50            -> nessuna irrigazione dopo il 50%
#   - Con irrigationStopCycle = 0             -> nessuna irrigazione (soglia = 0)

rm(list = ls())
library(cumba)

# ---- carica dati meteo di test ------------------------------------------
excel_file <- "testFiles/Dataset Carucci et al. new.xlsx"
if (!file.exists(excel_file)) stop("File meteo non trovato: ", excel_file)

library(readxl)
all_sheets <- lapply(excel_sheets(excel_file),
                     function(s) read_excel(excel_file, sheet = s))
weather      <- all_sheets[[4]]
weather$DATE <- as.Date(weather$DATE)
weather$Site <- "Foggia"

param <- cumbaParameters()

# ---- helper: run scenario e restituisce il risultato per un anno --------
run_one <- function(stop_pct) {
  res <- cumba_scenario(
    weather             = weather[weather$DATE >= as.Date("2010-01-01") &
                                    weather$DATE <= as.Date("2010-12-31"), ],
    param               = param,
    transplantingDOY    = 120,
    irrigationStopCycle = stop_pct,
    irrigationStrategy  = list(
      vegetative   = list(wsLevel = 0.9, turnMin = 1),
      reproductive = list(wsLevel = 0.9, turnMin = 1),
      ripening     = list(wsLevel = 0.9, turnMin = 1)
    )
  )
  # cumba_scenario restituisce una lista; prendiamo il primo elemento (data.frame)
  if (is.list(res) && !is.data.frame(res)) res[[1]] else res
}

# ---- TEST 1: stop = 100 -> deve esserci almeno 1 irrigazione ------------
cat("\n=== TEST 1: irrigationStopCycle = 100 ===\n")
df100 <- run_one(100)
n_irr100 <- sum(df100$irrigation > 0, na.rm = TRUE)
cat(sprintf("  Giorni irrigati: %d\n", n_irr100))
stopifnot("TEST 1 FAIL: nessuna irrigazione con stop=100" = n_irr100 > 0)
cat("  PASS\n")

# ---- TEST 2: stop = 0 -> nessuna irrigazione ----------------------------
cat("\n=== TEST 2: irrigationStopCycle = 0 ===\n")
df0 <- run_one(0)
n_irr0 <- sum(df0$irrigation > 0, na.rm = TRUE)
cat(sprintf("  Giorni irrigati: %d  (atteso: 0)\n", n_irr0))
stopifnot("TEST 2 FAIL: irrigazione con stop=0" = n_irr0 == 0)
cat("  PASS\n")

# ---- TEST 3: stop = 50 -> nessuna irrigazione DOPO il 50% ---------------
cat("\n=== TEST 3: irrigationStopCycle = 50 ===\n")
df50 <- run_one(50)

# Giorni in cui cycleCompletion > 50 E irrigation > 0 -> non devono esistere
bad <- df50[!is.na(df50$cycleCompletion) &
              df50$cycleCompletion > 50 &
              !is.na(df50$irrigation) &
              df50$irrigation > 0, ]
cat(sprintf("  Giorni irrigati dopo CC>50: %d  (atteso: 0)\n", nrow(bad)))
if (nrow(bad) > 0) {
  cat("  Righe problematiche:\n")
  print(bad[, c("doy", "cycleCompletion", "irrigation")])
}
stopifnot("TEST 3 FAIL: irrigazione trovata dopo soglia 50%" = nrow(bad) == 0)
cat("  PASS\n")

# ---- TEST 4: stop = 50 riduce irrigazione totale rispetto a stop = 100 --
cat("\n=== TEST 4: stop=50 riduce irrigazione totale ===\n")
tot100 <- sum(df100$irrigation, na.rm = TRUE)
tot50  <- sum(df50$irrigation,  na.rm = TRUE)
cat(sprintf("  Totale irr. stop=100: %.1f mm\n", tot100))
cat(sprintf("  Totale irr. stop=50:  %.1f mm\n", tot50))
stopifnot("TEST 4 FAIL: stop=50 non riduce l'irrigazione totale" = tot50 <= tot100)
cat("  PASS\n")

cat("\n=== Tutti i test superati. irrigationStopCycle funziona correttamente. ===\n")
