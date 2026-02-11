# -------------------------------------------------------------------------
# Chile-specific wildfire simulation module
#
# Authors:
# - Adrián Regos
#   ORCID: https://orcid.org/0000-0003-1983-936X
# - Augusto Pablo Salonio Carbó
#   Email: augusto93921@gmail.com
#   ORCID: https://orcid.org/0009-0005-9460-0764
#
# Reference:
# Original REMAINS project
# https://github.com/FirESmart-Project/REMAINS/tree/master
#
# Project context:
# Proyecto ANID – Objetivo Específico 4
# "Calibración del submodelo de incendios forestales de REMAINS
#  en la región Centro–Sur de Chile"
# CSIC / Misión Biológica de Galicia (MBG)
# Regos, A. & Salonio Carbó, A. P. (Mayo 2025)
#
# Created / substantially modified: 2025–2026
# -------------------------------------------------------------------------

#Fire risk function custom version:

#Purpose: Calculate per-cell fire risk score fore each raster cell, combining:
#A. Danger (ignition probability * slope gradient * land-cover susceptibility)
#B. Damage potential (vulnerability * economic/ecological value of the land cover)

#Key formula
#risk = (vulnerability * value + small constant) * danger

#Benefits:

#Fixes the critical flaw of hidden global dependency on orography
#Adds proper alignment check via join (prevents wrong slope assignment if cell IDs are misaligned)
#Provides better slope differentiation on steep Chilean terrain
#Includes damage/value in final risk (original omits it from risk, only uses it in damage)
#Much easier to debug and maintain thanks to prints and explicit steps
#Handles NA values gracefully instead of silently propagating them

#The only minor downside is the small performance cost of the left_join() — but for a ~1 km grid (tens of thousands of cells), it's negligible (milliseconds).


fire.risk = function(land, orography, ha.cell) {
  # Step 1: Align land and orography by joining on cell.id
  data = land %>% 
    left_join(dplyr::select(orography, cell.id, slope), by = "cell.id")
  
  # Debug: Check lengths after joining
  cat("nrow(data) after join:", nrow(data), "\n")
  cat("length(data$prob.fire):", length(data$prob.fire), "\n")
  cat("length(data$slope):", length(data$slope), "\n")
  
  # Step 2: Compute gradient based on slope
  gradient = ifelse(data$slope <= 5, 2,
                    ifelse(data$slope <= 10, 3,
                           ifelse(data$slope <= 15, 4,
                                  ifelse(data$slope <= 20, 5,
                                         ifelse(data$slope <= 30, 6,
                                                ifelse(data$slope <= 50, 7,
                                                       ifelse(data$slope <= 80, 8, 9)))))))
  
  # Handle NAs in gradient (e.g., if slope is missing after join)
  gradient = ifelse(is.na(gradient), 0, gradient)
  
  # Step 3: Compute susceptibility based on land-cover type
  land.fire.suscep = ifelse(data$lct %in% c("urban", "water"), 0,
                            ifelse(data$lct %in% c("crop"), 2,
                                   ifelse(data$lct == "grass", 3, 4)))
  
  # Step 4: Compute danger
  danger = data$prob.fire * gradient * land.fire.suscep
  
  # Handle NAs in danger (e.g., if prob.fire is NA)
  danger = ifelse(is.na(danger), 0, danger)
  
  # Step 5: Compute damage (vulnerability * value)
  vulner = ifelse(data$lct == "pine", 1,
                  ifelse(data$lct == "oak", 0.6,
                         ifelse(data$lct == "urban", 0.75,
                                ifelse(data$lct %in% c("shrub"), 0.4, 0))))
  
  valor = ifelse(data$lct == "pine", 91 * ha.cell,
                 ifelse(data$lct == "oak", 87 * ha.cell,
                        ifelse(data$lct == "urban", 0.074148 * ha.cell,
                               ifelse(data$lct %in% c("shrub"), 52 * ha.cell, 0))))
  
  damage = vulner * valor + 10^(-6)
  
  # Step 6: Compute risk
  risk = damage * danger
  
  # Debug: Check output length
  cat("length(risk):", length(risk), "\n")
  
  return(risk)
}