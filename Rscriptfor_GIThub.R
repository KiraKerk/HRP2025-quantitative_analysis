library(readxl)
library(writexl)

# ==================================================================
# 1. VORBEREITUNG 
# ==================================================================
datei_pfad <- file.choose()
daten <- read_excel(datei_pfad)

# Dateipfad und Name zum Speichern der Auswertungen auslesen
speicherort <- dirname(datei_pfad)
original_name <- basename(datei_pfad)
name_ohne_endung <- gsub("\\.xlsx?$|\\.xls?$", "", original_name)

colnames(daten) <- trimws(colnames(daten))
colnames(daten) <- tolower(colnames(daten))
alle_ergebnisse <- list()
kurven_ids <- unique(daten$kurve_id)

# ==================================================================
# 2. SCHLEIFE ÜBER AUSWERTUNG EINZELNER KURVEN
# ==================================================================
for(id in kurven_ids) {
  
  # ------------------------------------------------------------------
  # Daten anpassen
  # ------------------------------------------------------------------
  aktuelle_kurve <- subset(daten, kurve_id == id)
  
  stds <- subset(aktuelle_kurve, typ == "STD")  
  qcs <- subset(aktuelle_kurve, typ == "QC")
  proben <- subset(aktuelle_kurve, typ == "PROBE")
  
  # Daten für eine Kurve  
  x_data <- as.numeric(stds$x_wert)
  y_data <- as.numeric(stds$y_wert)
  quali_x <- as.numeric(qcs$x_wert)
  quali_y <- as.numeric(qcs$y_wert)
  probe <- as.numeric(proben$y_wert)
  
  # 1. Nullen aus den Standards filtern
  gueltige_punkte <- x_data > 0 & y_data > 0
  x_data <- x_data[gueltige_punkte]
  y_data <- y_data[gueltige_punkte]
  
  # 2. Abbruch, falls die Kurve zu wenig Standards hat (weniger als 3 Punkte)
  if(length(x_data) < 3) {
    print(paste("ACHTUNG: Kurve", id, "hat zu wenig Standards! Wird übersprungen."))
    
    # mit "NA" füllen (für alle 3 Modelle)
    aktuelle_kurve$Konz_Linear <- NA
    aktuelle_kurve$Accuracy_Linear <- NA
    aktuelle_kurve$R2_Linear <- NA
    
    aktuelle_kurve$Konz_Quadratisch <- NA
    aktuelle_kurve$Accuracy_Quadratisch <- NA
    aktuelle_kurve$R2_Quadratisch <- NA
    
    aktuelle_kurve$Konz_Powerlaw <- NA
    aktuelle_kurve$Accuracy_Powerlaw <- NA
    aktuelle_kurve$R2_Powerlaw <- NA
    
    aktuelle_kurve$Konz_LogLog <- NA
    aktuelle_kurve$Accuracy_LogLog <- NA
    aktuelle_kurve$R2_LogLog <- NA
    
    alle_ergebnisse[[as.character(id)]] <- aktuelle_kurve
    next
  }
  
  # ------------------------------------------------------------------
  # 1. & 2. Modell: Linear & Quadratisch (weight 1/x^2)
  # ------------------------------------------------------------------
  weight <- 1/(x_data^2)
  
  # Quadratisch
  quadratic_regression_ww <- lm(y_data~x_data + I(x_data^2),weights=weight)
  a <- quadratic_regression_ww$coefficients[3]
  b <- quadratic_regression_ww$coefficients[2]
  c <- quadratic_regression_ww$coefficients[1]
  r2_quad <- summary(quadratic_regression_ww)$r.squared
  
  # Linear
  linear_regression_ww <- lm(y_data~x_data, weights=weight)
  lin_intercept <- linear_regression_ww$coefficients[1]
  lin_slope <- linear_regression_ww$coefficients[2]
  r2_lin <- summary(linear_regression_ww)$r.squared
  
  # ------------------------------------------------------------------
  # 3. & 4. Modell: Echtes Powerlaw & Lineares Log-Log
  # ------------------------------------------------------------------
  x_data_log = log(x_data)
  y_data_log = log(y_data)
  
  # Log-Log
  loglog_linear_regression <- lm(y_data_log~x_data_log)
  loglog <- summary(loglog_linear_regression)
  a_start<-exp(loglog$coefficients[1,1])
  b_start<-loglog$coefficients[2,1]
  r2_loglog <- loglog$r.squared
  
  # Platzhalter für Powerlaw
  pow_a <- NA
  pow_b <- NA
  r2_pow <- NA
  
  # Powerlaw
  try({
    powerlaw_ww<-nls(y_data~a*x_data^b, start=list(a=a_start,b=b_start), weights=weight, control=nls.control(maxiter = 1000, minFactor = 1/4096, warnOnly = TRUE))
    pow_a <- coef(powerlaw_ww)[1]
    pow_b <- coef(powerlaw_ww)[2]
    r2_pow <- cor(y_data, predict(powerlaw_ww))^2
  }, silent = TRUE)
  
  
  # ------------------------------------------------------------------
  # Plots erzeugen und als svg speichern (paper-Version) 
  # ------------------------------------------------------------------
  
  # NEU: Sonderzeichen aus der ID entfernen, damit Windows nicht meckert
  sichere_id <- gsub("[^[:alnum:]]", "_", id)
  
  # --- Plot 1: Linear, Quadratisch & Powerlaw (X-Achse Logarithmisch) ---
  # NEU: file.path() baut den Dateipfad betriebssystem-sicher zusammen
  svg_dateiname_1 <- file.path(speicherort, paste0(name_ohne_endung, "_Curve_", sichere_id, "_LinQuadPow.svg"))
  svg(svg_dateiname_1, width = 8, height = 6)
  
  # Daten
  plot(x_data, y_data, main="", xlab="Concentration (log scale)", ylab="Signal", log="x")
  
  x_seq <- exp(seq(log(min(x_data)), log(max(x_data)), length.out=500))
  
  # Quadratisch 
  y_pred_quad <- c + b*x_seq + a*x_seq^2
  lines(x_seq, y_pred_quad, col="red", lwd=2)
  
  # Linear 
  y_pred_lin <- lin_intercept + lin_slope*x_seq
  lines(x_seq, y_pred_lin, col="darkgreen", lwd=2, lty=2)
  
  # Powerlaw, falls berechnet 
  if(!is.na(pow_a)) {
    y_pred_pow <- pow_a * x_seq^pow_b
    lines(x_seq, y_pred_pow, col="darkorange", lwd=2, lty=3)
  }
  
  # R²-Werte formatieren (auf 4 Nachkommastellen runden)
  r2_quad_text <- sprintf("%.4f", r2_quad)
  r2_lin_text  <- sprintf("%.4f", r2_lin)
  
  # Legende
  if(!is.na(pow_a)) {
    r2_pow_text <- sprintf("%.4f", r2_pow)
    legend_text <- c(paste0("Quadratic (R² = ", r2_quad_text, ")"), 
                     paste0("Linear (R² = ", r2_lin_text, ")"), 
                     paste0("Powerlaw (R² = ", r2_pow_text, ")"))
    legend_colors <- c("red", "darkgreen", "darkorange")
    legend_lty <- c(1, 2, 3)
  } else {
    legend_text <- c(paste0("Quadratic (R² = ", r2_quad_text, ")"), 
                     paste0("Linear (R² = ", r2_lin_text, ")"))
    legend_colors <- c("red", "darkgreen")
    legend_lty <- c(1, 2)
  }
  legend("topleft", legend=legend_text, col=legend_colors, lty=legend_lty, lwd=2, bty="n")
  
  dev.off()
  
  
  # --- Plot 2: Log-Log Modell (Logarithmische Skala) ---
  # NEU: file.path() nutzen
  svg_dateiname_2 <- file.path(speicherort, paste0(name_ohne_endung, "_Curve_", sichere_id, "_LogLog.svg"))
  svg(svg_dateiname_2, width = 8, height = 6)
  
  # Daten  
  plot(x_data_log, y_data_log, main="", xlab="log(Concentration)", ylab="log(Signal)")
  
  # Log-Log  
  abline(loglog_linear_regression, col="blue", lty=1, lwd=2)
  
  # R²-Wert für Log-Log formatieren
  r2_loglog_text <- sprintf("%.4f", r2_loglog)
  legend_text_log <- paste0("Linear Log-Log (R² = ", r2_loglog_text, ")")
  
  # Legende
  legend("topleft", legend=legend_text_log, col="blue", lty=1, lwd=2, bty="n")
  
  dev.off()
  
  # ------------------------------------------------------------------
  # Ergebnisse für die Excel-Tabelle speichern  
  # ------------------------------------------------------------------
  y_all <- as.numeric(aktuelle_kurve$y_wert)
  
  # --- A) Ergebnisse Linear (1/x^2 gewichtet) ---
  x_kalkuliert_lin <- suppressWarnings( (y_all - lin_intercept) / lin_slope )
  aktuelle_kurve$Konz_Linear <- x_kalkuliert_lin
  aktuelle_kurve$Accuracy_Linear <- aktuelle_kurve$Konz_Linear / as.numeric(aktuelle_kurve$x_wert)
  aktuelle_kurve$R2_Linear <- r2_lin
  
  # --- B) Ergebnisse Quadratisch (1/x^2 gewichtet) ---
  x_kalkuliert_quad <- suppressWarnings( (-b + sqrt(b^2 - 4 * a * (c - y_all))) / (2 * a) )
  aktuelle_kurve$Konz_Quadratisch <- x_kalkuliert_quad
  aktuelle_kurve$Accuracy_Quadratisch <- aktuelle_kurve$Konz_Quadratisch / as.numeric(aktuelle_kurve$x_wert)
  aktuelle_kurve$R2_Quadratisch <- r2_quad
  
  # --- C) Ergebnisse Powerlaw (1/x^2 gewichtet) ---
  x_kalkuliert_pow <- suppressWarnings( (1/pow_a * y_all)^(1/pow_b) )
  aktuelle_kurve$Konz_Powerlaw <- x_kalkuliert_pow
  aktuelle_kurve$Accuracy_Powerlaw <- aktuelle_kurve$Konz_Powerlaw / as.numeric(aktuelle_kurve$x_wert)
  aktuelle_kurve$R2_Powerlaw <- r2_pow
  
  # --- D) Ergebnisse Log-Log Linear (1/y^2 gewichtet) ---
  log_a <- loglog$coefficients[1,1]
  log_b <- loglog$coefficients[2,1]
  r2_loglog <- loglog$r.squared
  
  x_kalkuliert_loglog <- suppressWarnings( exp((log(y_all) - log_a) / log_b) )
  aktuelle_kurve$Konz_LogLog <- x_kalkuliert_loglog
  aktuelle_kurve$Accuracy_LogLog <- aktuelle_kurve$Konz_LogLog / as.numeric(aktuelle_kurve$x_wert)
  aktuelle_kurve$R2_LogLog <- r2_loglog
  
  alle_ergebnisse[[as.character(id)]] <- aktuelle_kurve
  
} # Ende for(id in kurven_ids)

# ==================================================================
# 3. EXCEL DATEI SCHREIBEN 
# ==================================================================
finale_tabelle <- do.call(rbind, alle_ergebnisse)

# NEU: Auch hier file.path() nutzen
excel_dateiname <- file.path(speicherort, paste0(name_ohne_endung, "_Fertige_Auswertung.xlsx"))

write_xlsx(finale_tabelle, excel_dateiname)

print(paste("FERTIG! Alles wurde berechnet. Dateien wurden gespeichert in:", speicherort))