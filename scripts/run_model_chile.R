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

# Clean the workspace
rm(list = ls())

# Load essential packages
library(tidyverse)      # for add_column, left_join, group_by, summarise, etc.
library(terra)          # for raster operations

# Load functions
source("C:/Repos/REMAINS_Chile/scripts/internals.r")
source("C:/Repos/REMAINS_Chile/scripts/interface.r")
source("C:/Repos/REMAINS_Chile/scripts/fire_risk_chile.R")
source("C:/Repos/REMAINS_Chile/scripts/wildfires_chile.R")
source("C:/Repos/REMAINS_Chile/scripts/params_chile.R")
source("C:/Repos/REMAINS_Chile/scripts/land.dyn.mdl_chile.R")

# For reproducibility
set.seed(20260209) #Puede ser por las seeds

# Common settings
common_params <- list(
  time.horizon      = 20,        # 20 years (from 2018 to 2038)
  annual.area.pb    = 1000,      # will be overridden per scenario
  out.maps          = FALSE,     # only enable for first scenario if needed
  verbose           = TRUE,
  nrun              = 30,        # 30 simulations per scenario
  write.outputs     = FALSE,
  save.land         = FALSE
)

# Scenarios definition (easy to extend or modify)
scenarios <- list(
  "0.1" = list(
    fuel_th = 0.1,
    pb_area = 1000,
    out_maps = TRUE,           # only generate initial maps for this one
    lcc = data.frame(
      SmartPlant   = round(runif(30, 1, 1)),
      AgriConver   = round(runif(30, 250, 250)),
      RuralAbnd    = round(runif(30, 0, 0)),
      PastureAbnd  = round(runif(30, 0, 0)),
      PastureConver = round(runif(30, 800, 800))
    )
  ),
  "0.5" = list(
    fuel_th = 0.5,
    pb_area = 20000,
    out_maps = FALSE,
    lcc = data.frame(
      SmartPlant   = round(runif(30, 0, 0)),
      AgriConver   = round(runif(30, 0, 0)),
      RuralAbnd    = round(runif(30, 200, 200)),
      PastureAbnd  = round(runif(30, 180, 180)),
      PastureConver = round(runif(30, 0, 0))   # added missing column
    )
  ),
  "0.9" = list(
    fuel_th = 0.9,
    pb_area = 20000,
    out_maps = FALSE,
    lcc = data.frame(
      SmartPlant   = round(runif(30, 0, 0)),
      AgriConver   = round(runif(30, 0, 0)),
      RuralAbnd    = round(runif(30, 200, 200)),
      PastureAbnd  = round(runif(30, 180, 180)),
      PastureConver = round(runif(30, 0, 0)) #puede ser acá
    )
  )
)

# ── Simplified loading ───────────────────────────────────────────────────────

# Load landscape
load("C:/Repos/REMAINS_Chile/data/landscape.prob.fire.50.rda")
landscape.prob.fire <- landscape.prob.fire.50
landscape <- landscape.prob.fire.50   # rename to 'landscape' for simplicity
land <- landscape

# Load orography
load("C:/Repos/REMAINS_Chile/data/orography.rda")
orography <- oro_chile               # rename to 'orography' for simplicity

# Load mask
mask.study.area <- readRDS("C:/Repos/REMAINS_Chile/data/mask.study.area.rds")

# Load params base
params_base <- params.chile()

# Extract land-cover types
land.cover.type <- params_base$land.cover.type

# Load all distributions (burnt area and fire size) — assume standard names
dist_path <- "C:/Repos/REMAINS_Chile/data"

# Burnt area distributions calibrated for each region
load(file.path(dist_path, "burnt.area.dist.nub.1999.2018_2.5.rda"))
burnt.area.dist.nub <- abaNUB.discret
load(file.path(dist_path, "burnt.area.dist.bio.1999.2018_2.5.rda"))
burnt.area.dist.bio <- abaBIO.discret
load(file.path(dist_path, "burnt.area.dist.ara.1999.2018_2.0.rda"))
burnt.area.dist.ara <- abaARA.discret

#To simulate a climate-change scenario use burnt.area.dist_4.0.rda

# Fire size distributions
load(file.path(dist_path, "fire.size.dist.nub.1999.2018.rda"))
load(file.path(dist_path, "fire.size.dist.bio.1999.2018.rda"))
load(file.path(dist_path, "fire.size.dist.ara.1999.2018.rda"))

# Optional: Quick sanity check
stopifnot(exists("burnt.area.dist.nub"))  # example — add more if needed

#Run all scritps before executing run_model_chile.R:
#internals.r
#interface.r
#fire_risk_chile.R
#wildfires_chile.R
#params_chile.R
#land.dyn.mdl_chile.R

# ── Loop over scenarios ──────────────────────────────────────────────────────

results <- list()

for (scen in names(scenarios)) {
  cat("\n=======================================\n")
  cat("Running scenario:", scen, "\n")
  cat("=======================================\n")
  
  s <- scenarios[[scen]]
  
  scenDir <- paste0("outputs/", scen)
  if (!dir.exists(scenDir)) dir.create(scenDir, recursive = TRUE)
  
  # Copy base params and override
  params <- params_base
  params$time.horizon    <- common_params$time.horizon
  params$annual.area.pb  <- s$pb_area
  params$out.maps        <- s$out_maps        # only TRUE for 0.1 if desired
  
  # Set suppression
  params$fire.suppression$fuel.th[params$fire.suppression$region %in% c("Ñuble", "Biobío", "La Araucanía")] <- s$fuel_th
  params$fire.suppression$mosaic.th[params$fire.suppression$region %in% c("Ñuble", "Biobío", "La Araucanía")] <- 1
  
  # Initial maps only for scenarios where requested
  if (s$out_maps) {
    cat("Generating initial maps at t = 0\n")
    
    # Create maps subfolder if it doesn't exist
    maps_dir <- file.path(scenDir, "maps")
    if (!dir.exists(maps_dir)) {
      dir.create(maps_dir, recursive = TRUE)
    }
    
    out.raster <- mask.study.area
    
    dta <- data.frame(cell.id = 1:ncell(out.raster)) %>%
      left_join(land[, c("cell.id", "lct")], by = "cell.id") %>%
      left_join(land.cover.type, by = "lct")
    
    dta$fire.risk <- NA
    
    idx <- dta$cell.id %in% land$cell.id
    
    # Correct call: land + ha.cell (orography is used globally inside the function)
    fire_risk_values <- fire.risk(land, orography, params$ha.cell)
    
    if (sum(idx) != length(fire_risk_values)) {
      warning("Length mismatch in initial map generation. Adjusting.")
      fire_risk_values <- fire_risk_values[1:min(length(fire_risk_values), sum(idx))]
    }
    
    dta$fire.risk[idx] <- fire_risk_values
    
    # Write rasters — now using full safe paths
    terra::writeRaster(out.raster, 
                       filename = file.path(maps_dir, "wildfire.ids_r1_t0.tif"), 
                       overwrite = TRUE)
    
    out.raster[] <- 0
    terra::writeRaster(out.raster, 
                       filename = file.path(maps_dir, "wildfire.step_r1_t0.tif"), 
                       overwrite = TRUE)
    
    out.raster[] <- dta$fire.risk
    terra::writeRaster(out.raster, 
                       filename = file.path(maps_dir, "firerisk_r1_t0.tif"), 
                       overwrite = TRUE)
    
    out.raster[] <- dta$lct.id
    terra::writeRaster(out.raster, 
                       filename = file.path(maps_dir, "land.cover.type_run1_time0.tif"), 
                       overwrite = TRUE)
    
    cat("Initial maps generated successfully for t = 0\n")
  }
  
  # Run model
  sink(file.path(scenDir, "model_output.txt"))
  res <- land.dyn.mdl.chile(
    scenDir           = scenDir,
    is.land.cover.change = FALSE,
    is.wildfire       = TRUE,
    is.prescribed.burn = FALSE,
    is.postfire.rege  = FALSE,
    is.forest.recover = FALSE,
    is.afforestation  = FALSE,
    is.encroachment   = FALSE,
    nrun              = common_params$nrun,
    save.land         = common_params$save.land,
    params            = params,
    lcc.demand        = s$lcc,
    out.maps          = s$out_maps,
    write.outputs     = common_params$write.outputs
  )
  sink()
  
  # Save full result
  save(res, file = file.path(scenDir, "result.rda"))
  
  # Write individual tables
  for (nm in names(res)) {
    write.table(res[[nm]], file.path(scenDir, paste0(nm, ".txt")),
                quote = FALSE, row.names = FALSE, sep = "\t")
  }
  
  # Quick summary with totals for debugging
  fires_summary <- res$fires %>%
    group_by(year, region) %>%
    summarise(
      atarget = sum(atarget, na.rm = TRUE),
      aburnt  = sum(aburnt, na.rm = TRUE),
      asupp   = sum(asupp.fuel + asupp.mosaic, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(region) %>%
    summarise(
      atarget_total = sum(atarget),
      aburnt_total  = sum(aburnt),
      asupp_total   = sum(asupp),
      pburnt        = 100 * aburnt_total / atarget_total,
      psupp         = 100 * asupp_total / atarget_total,
      .groups = "drop"
    )
  
  # Report-style summary for "Calibración del submodelo de incendios forestales de REMAINS en la región Centro – Sur de Chile."
  # Burnt area median (Scenario 0.5 for Table 1 column "2019-2038" in page 4), 
  # Suppressed area by fuel median (Scenario 0.5 for Table 2 column "2019-2038" in page 5), and 
  # Suppressed area by mosaic median (Scenario 0.5 for Table 3 column "2019-2038" in page 6)
  
  
  report_summary <- res$fires %>%
    group_by(run, region) %>%
    summarise(
      cumulative_burnt = sum(aburnt, na.rm = TRUE),
      cumulative_supp_fuel = sum(asupp.fuel, na.rm = TRUE),
      cumulative_supp_mosaic = sum(asupp.mosaic, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(region) %>%
    summarise(
      median_burnt = median(cumulative_burnt),
      median_supp_fuel = median(cumulative_supp_fuel),
      median_supp_mosaic = median(cumulative_supp_mosaic),
      .groups = "drop"
    )
  
  cat("Summary for", scen, ":\n")
  print(fires_summary)
  
  cat("\nReport-style summary (median per scenario):\n")
  print(report_summary)
  
  # Store for later comparison if needed
  results[[scen]] <- list(
    res = res, 
    summary = fires_summary,
    report = report_summary
  )
}

cat("\nAll scenarios completed.\n")
