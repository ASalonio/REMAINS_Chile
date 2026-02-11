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

wildfires_chile = function(land, params, out.maps = FALSE, verbose = FALSE) {
  ## Validate inputs
  if (!exists("mask.study.area")) {
    stop("mask.study.area is not defined.")
  }
  if (inherits(mask.study.area, "SpatRaster")) {
    mask_matrix <- as.matrix(mask.study.area, wide = TRUE)
  } else if (is.matrix(mask.study.area)) {
    mask_matrix <- mask.study.area
  } else {
    stop("mask.study.area must be a matrix or SpatRaster.")
  }
  if (any(is.na(land$lct))) warning("NA values found in land$lct.")
  if (any(is.na(landscape.prob.fire$prob.fire))) warning("NA values found in landscape.prob.fire$prob.fire.")
  if (any(is.na(orography$region)) || any(is.na(orography$elevation))) warning("NA values found in orography$region or orography$elevation.")
  
  ## Ensure cell.id alignment
  land = land[land$cell.id %in% orography$cell.id, ]
  land = land[land$cell.id %in% landscape.prob.fire$cell.id, ]
  if (nrow(land) == 0) stop("No valid cells in land after aligning with orography and landscape.prob.fire.")
  
  ## Initialize tracking variables
  fire.id = 0
  track.fires = NULL
  visit.cells = eval.cells = burnt.cells = integer()
  
  ## Sub-select the 'land' data frame
  subland = land %>% 
    filter(lct != "water", lct != "urban", lct != "baresoil") %>% 
    dplyr::select(-trans.type)
  subland$is.source.supp.fuel = subland$is.source.supp.mosaic = FALSE
  subland = subland %>% left_join(dplyr::select(orography, cell.id, region, elevation), by = "cell.id")
  if (any(is.na(subland$region))) {
    warning("NA values found in subland$region. Removing affected rows.")
    subland = subland[!is.na(subland$region), ]
  }
  if (any(is.na(subland$elevation))) {
    warning("NA values found in subland$elevation. Removing affected rows.")
    subland = subland[!is.na(subland$elevation), ]
  }
  if (nrow(subland) == 0) stop("subland is empty after removing NA values.")
  
  cat("Burnable cells by region:\n")
  print(table(subland$region))
  
  ## Initialize map
  if (out.maps) map = data.frame(cell.id = subland$cell.id, id = NA, step = NA)
  
  ## Set fire probability
  subland$pigni = landscape.prob.fire$prob.fire[match(subland$cell.id, landscape.prob.fire$cell.id)]
  if (any(is.na(subland$pigni))) {
    warning("NA values found in subland$pigni. Removing affected rows.")
    subland = subland[!is.na(subland$pigni), ]
    if (nrow(subland) == 0) stop("subland is empty after removing NA pigni values.")
  }
  
  ## Build neighbor data frame
  pos = c(-1, 0, 1)
  ncol = ncol(mask_matrix)
  d1 = sqrt(2 * params$cell.size^2)
  default.neigh = data.frame(x = c(-ncol + pos, pos, ncol + pos), dist = c(d1, params$cell.size, d1, params$cell.size, 0, params$cell.size, d1, params$cell.size, d1))
  default.neigh = default.neigh[default.neigh$dist != 0, ]
  default.nneigh = nrow(default.neigh)
  
  ## Wildfires by region
  for (region in c("Ñuble", "Biobío", "La Araucanía")) {
    cat(paste0(" Wildfires in ", region, "\n"))
    if (region == "Ñuble") { burnt.area.dist = burnt.area.dist.nub; fire.size.dist = fire.size.dist.nub }
    if (region == "Biobío") { burnt.area.dist = burnt.area.dist.bio; fire.size.dist = fire.size.dist.bio }
    if (region == "La Araucanía") { burnt.area.dist = burnt.area.dist.ara; fire.size.dist = fire.size.dist.ara }
    
    if (is.null(burnt.area.dist) || nrow(burnt.area.dist) == 0 || any(is.na(burnt.area.dist$upper.th)) || any(is.na(burnt.area.dist$freq)) || sum(burnt.area.dist$freq, na.rm = TRUE) == 0) {
      warning(paste("Invalid burnt.area.dist for region", region, ". Skipping region."))
      next
    }
    burnt.area.dist = burnt.area.dist[!is.na(burnt.area.dist$freq), ]
    burnt.area.dist$freq = burnt.area.dist$freq / sum(burnt.area.dist$freq)
    burnt.area.dist = burnt.area.dist[order(burnt.area.dist$upper.th), ]
    ba.cls = sample(burnt.area.dist$upper.th, 1, prob = burnt.area.dist$freq)
    ba.pos = which(burnt.area.dist$upper.th == ba.cls)
    annual.target.area = if (length(ba.pos) == 0 || ba.pos == 1) {
      warning(paste("Invalid ba.pos for region", region, ". Using min/max of upper.th."))
      valid_upper_th = burnt.area.dist$upper.th[!is.na(burnt.area.dist$upper.th)]
      if (length(valid_upper_th) < 2) {
        warning(paste("Not enough valid upper.th values for region", region, ". Skipping region."))
        next
      }
      round(runif(1, min(valid_upper_th), valid_upper_th[2]))
    } else {
      round(runif(1, burnt.area.dist$upper.th[ba.pos - 1], burnt.area.dist$upper.th[ba.pos]))
    }
    if (is.na(annual.target.area)) {
      warning(paste("NA produced in annual.target.area for region", region, ". Skipping region."))
      next
    }
    cat(paste("   Annual target area:", annual.target.area, "ha", "\n"))
    
    fuel.th = params$fire.suppression$fuel.th[params$fire.suppression$region == region]
    mosaic.th = params$fire.suppression$mosaic.th[params$fire.suppression$region == region]
    if (length(fuel.th) == 0 || is.na(fuel.th)) { warning(paste("Invalid fuel.th for region", region, ". Setting to 0.")); fuel.th = 0 }
    if (length(mosaic.th) == 0 || is.na(mosaic.th)) { warning(paste("Invalid mosaic.th for region", region, ". Setting to Inf.")); mosaic.th = Inf }
    cat(paste0("   Fuel suppression th: ", fuel.th * 100, "%  -  Mosaic suppression th: ", mosaic.th, " pixel \n"))
    
    while (annual.target.area > 0) {
      fire.id = fire.id + 1
      step = 1
      region_cells = subland$cell.id[subland$region == region]
      region_pigni = subland$pigni[subland$region == region]
      if (length(region_cells) == 0 || all(is.na(region_pigni)) || sum(region_pigni, na.rm = TRUE) == 0) {
        warning(paste("No valid cells or probabilities for ignition in region", region, ". Skipping fire."))
        break
      }
      igni.id = sample(region_cells, 1, replace = FALSE, prob = region_pigni)
      fire.size.dist = fire.size.dist[!is.na(fire.size.dist$freq), ]
      if (nrow(fire.size.dist) == 0 || sum(fire.size.dist$freq, na.rm = TRUE) == 0) {
        warning(paste("No valid frequencies in fire.size.dist for region", region, ". Skipping fire."))
        next
      }
      fire.size.dist$freq = fire.size.dist$freq / sum(fire.size.dist$freq)
      fire.size.dist = fire.size.dist[order(fire.size.dist$upper.th), ]
      fs.cls = sample(fire.size.dist$upper.th, 1, prob = fire.size.dist$freq)
      fs.pos = which(fire.size.dist$upper.th == fs.cls)
      fire.target.area = if (length(fs.pos) == 0 || fs.pos == 1) {
        warning(paste("Invalid fs.pos for region", region, ". Using min/max of upper.th."))
        valid_upper_th = fire.size.dist$upper.th[!is.na(fire.size.dist$upper.th)]
        if (length(valid_upper_th) < 2) {
          warning(paste("Not enough valid upper.th values for fire.size.dist in region", region, ". Skipping fire."))
          next
        }
        round(runif(1, min(valid_upper_th), valid_upper_th[2]))
      } else {
        round(runif(1, fire.size.dist$upper.th[fs.pos - 1], fire.size.dist$upper.th[fs.pos]))
      }
      if (is.na(fire.target.area)) {
        warning(paste("NA produced in fire.target.area for region", region, ". Skipping fire."))
        next
      }
      if (fire.target.area > annual.target.area) fire.target.area = annual.target.area
      mx.ncell.ff = ifelse(fire.target.area <= 500, 12, ifelse(fire.target.area <= 1500, 20, ifelse(fire.target.area <= 5000, 30, 40)))
      mn.ncell.ff = ifelse(fire.target.area <= 5000, 8, 16)
      thruky = ifelse(fire.target.area <= 5000, 0.85, 0.95)
      fire.front = igni.id
      cumul.source = 1
      cumul.agri = 0
      aburnt = params$ha.cell
      asupp.fuel = asupp.mosaic = 0
      visit.cells = c(visit.cells, igni.id)
      eval.cells = c(eval.cells, igni.id)
      burnt.cells = c(burnt.cells, igni.id)
      if (out.maps) {
        map$id[map$cell.id == igni.id] = fire.id
        map$step[map$cell.id == igni.id] = step
      }
      if (verbose) cat(paste0("\n     Fire ", fire.id, " of target area ", fire.target.area, " ha\n"))
      
      while ((aburnt + asupp.fuel + asupp.mosaic) < fire.target.area) {
        if (verbose) cat(paste0("    Step ", step, ", aburnt+asupp = ", aburnt + asupp.fuel + asupp.mosaic, " ha\n"))
        step = step + 1
        neigh.id = data.frame(
          cell.id = as.integer(rep(fire.front, each = default.nneigh) + rep(default.neigh$x, length(fire.front))),
          source.id = rep(fire.front, each = default.nneigh),
          dist = rep(default.neigh$dist, length(fire.front)),
          position = rep(cumul.source, length(fire.front) * default.nneigh),  # Ensure correct length
          agri.front = rep(cumul.agri, length(fire.front) * default.nneigh)   # Ensure correct length
        ) %>%
          filter(!(cell.id %in% eval.cells), cell.id %in% subland$cell.id) %>%
          left_join(dplyr::select(subland, cell.id, lct, elevation, is.source.supp.fuel, is.source.supp.mosaic), by = c("source.id" = "cell.id")) %>%
          mutate(is.source.agri = (lct == "crop"), elevation.source = elevation) %>%
          dplyr::select(-lct, -elevation)
        if (verbose) {
          cat("    Number of neighbors in neigh.id:", nrow(neigh.id), "\n")
          if (nrow(neigh.id) > 0) cat("    Range of neigh.id$cell.id:", range(neigh.id$cell.id, na.rm = TRUE), "\n")
        }
        if (nrow(neigh.id) == 0) {
          if (verbose) cat("    No new valid neighbors available to spread to. Stopping fire spread.\n")
          break
        }
        neigh.land = subland[subland$cell.id %in% neigh.id$cell.id, c("cell.id", "lct", "tschg", "elevation")] %>%
          left_join(params$lct.fire.prone, by = "lct") %>%
          left_join(neigh.id, by = "cell.id") %>%
          mutate(
            elevation = ifelse(is.na(elevation), 0, elevation),
            elevation.source = ifelse(is.na(elevation.source), 0, elevation.source),
            dist = ifelse(is.na(dist) | dist == 0, 1e-6, dist),
            dif.elev = (elevation - elevation.source) / dist,
            slope = pmax(pmin(dif.elev, 0.5), -0.5) + 0.5,
            agri.front = ifelse(lct == "crop" & is.source.agri, agri.front + 1, 0),
            flam = ifelse(is.na(flam), 0, flam),  # Add flam from lct.fire.prone
            sr = params$wslope * slope + params$wlc * flam,
            sr = ifelse((lct == "shrub" & tschg <= 4) | (lct %in% c("pine", "oak") & tschg <= 8), 0.2, sr),
            sr = ifelse(is.na(sr) | is.infinite(sr), 0, sr)
          )
        neigh.land$pb = 1 - exp(-params$facc * pmax(neigh.land$sr, 0)) + runif(nrow(neigh.land), -params$rpb, params$rpb)
        neigh.land$pb = pmin(pmax(neigh.land$pb, 0), 1)
        if (any(is.na(neigh.land$sr)) || any(is.na(neigh.land$pb))) {
          warning("NA values found in neigh.land$sr or neigh.land$pb.")
          if (verbose) {
            cat("    NA in neigh.land$sr:", sum(is.na(neigh.land$sr)), "rows\n")
            cat("    NA in neigh.land$pb:", sum(is.na(neigh.land$pb)), "rows\n")
            print(head(neigh.land[is.na(neigh.land$sr) | is.na(neigh.land$pb), ], 10))
          }
        }
        sprd.rate = group_by(neigh.land, cell.id) %>%
          summarise(sr = max(sr, na.rm = TRUE), pb = max(pb, na.rm = TRUE), nsource = sum(position, na.rm = TRUE)) %>%
          mutate(sr = ifelse(is.infinite(sr), 0, sr), pb = ifelse(is.infinite(pb), 0, pb)) %>%
          left_join(dplyr::select(neigh.land, cell.id, pb, agri.front, is.source.supp.fuel, is.source.supp.mosaic), by = c("cell.id", "pb"))
        if (verbose) cat("    Number of rows in sprd.rate:", nrow(sprd.rate), "\n")
        if (nrow(sprd.rate) == 0) {
          if (verbose) cat("    No cells to evaluate for burning or suppression. Stopping fire spread.\n")
          break
        }
        sprd.rate = sprd.rate %>%
          mutate(tosupp.fuel = (is.source.supp.fuel | sr <= fuel.th), tosupp.mosaic = (is.source.supp.mosaic | agri.front >= mosaic.th))
        sprd.rate$burn = sprd.rate$pb >= runif(nrow(sprd.rate), params$pb.lower.th, params$pb.upper.th)
        if (any(is.na(sprd.rate$burn))) {
          warning("NA values found in sprd.rate$burn.")
          if (verbose) cat("    NA in sprd.rate$burn:", any(is.na(sprd.rate$burn)), "\n")
        }
        if (verbose) cat("    sprd.rate$burn:", sprd.rate$burn, "\n")
        subland$is.source.supp.fuel[subland$cell.id %in% sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.mosaic & sprd.rate$tosupp.fuel]] = TRUE
        subland$is.source.supp.mosaic[subland$cell.id %in% sprd.rate$cell.id[sprd.rate$burn & sprd.rate$tosupp.mosaic]] = TRUE
        if (out.maps) {
          map$id[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.fuel & !sprd.rate$tosupp.mosaic]] = fire.id
          map$step[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn]] = step
        }
        visit.cells = c(visit.cells, sprd.rate$cell.id)
        eval.cells = c(eval.cells, sprd.rate$cell.id[sprd.rate$burn])
        burnt.cells = c(burnt.cells, sprd.rate$cell.id[sprd.rate$burn & !sprd.rate$tosupp.fuel & !sprd.rate$tosupp.mosaic])
        if (!any(sprd.rate$burn, na.rm = TRUE)) {
          if (verbose) cat("    No cells burn in this step. Stopping fire spread.\n")
          break
        }
        aburnt = aburnt + sum(sprd.rate$burn & !sprd.rate$tosupp.fuel & !sprd.rate$tosupp.mosaic, na.rm = TRUE) * params$ha.cell
        asupp.fuel = asupp.fuel + sum(sprd.rate$burn & !sprd.rate$tosupp.mosaic & sprd.rate$tosupp.fuel, na.rm = TRUE) * params$ha.cell
        asupp.mosaic = asupp.mosaic + sum(sprd.rate$burn & sprd.rate$tosupp.mosaic, na.rm = TRUE) * params$ha.cell
        nburn = sum(sprd.rate$burn, na.rm = TRUE)
        if (is.na(nburn) || nburn == 0) {
          if (verbose) cat("    No new cells burnt (nburn = ", nburn, "). Stopping fire spread.\n")
          break
        }
        if (nburn <= mn.ncell.ff) {
          fire.front = sprd.rate$cell.id[sprd.rate$burn]
        } else {
          ratio.burnt = (aburnt + asupp.fuel + asupp.mosaic) / fire.target.area
          z = runif(1, mx.ncell.ff - 5, mx.ncell.ff)
          ncell.ff = min(nburn * runif(1, 0.5, 0.7), round(z), na.rm = TRUE)
          if (ncell.ff == z | (ratio.burnt >= thruky & runif(1, 0, 1) >= 0.75)) {
            p = sprd.rate$nsource[sprd.rate$burn] / 100
            fire.front = if (class(try(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace = FALSE, prob = p), silent = TRUE)) == "try-error") {
              sort(sprd.rate$cell.id[sprd.rate$burn])
            } else {
              sort(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace = FALSE, prob = p))
            }
          } else {
            fire.front = if (class(try(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace = FALSE), silent = TRUE)) == "try-error") {
              sort(sprd.rate$cell.id[sprd.rate$burn])
            } else {
              sort(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), replace = FALSE))
            }
          }
        }
        cumul.source = sprd.rate$nsource[sprd.rate$cell.id %in% fire.front]
        cumul.agri = sprd.rate$agri.front[sprd.rate$cell.id %in% fire.front]
        if (length(fire.front) == 0) {
          if (verbose) cat("    No cells in fire front. Stopping fire spread.\n")
          break
        }
      }
      if (is.null(track.fires)) {
        track.fires = data.frame(region, fire.id, atarget = aburnt + asupp.fuel + asupp.mosaic, aburnt, asupp.fuel, asupp.mosaic)
      } else {
        track.fires = rbind(track.fires, data.frame(region, fire.id, atarget = aburnt + asupp.fuel + asupp.mosaic, aburnt, asupp.fuel, asupp.mosaic))
      }
      annual.target.area = annual.target.area - (aburnt + asupp.fuel + asupp.mosaic)
      if (verbose) cat(paste("     remaining:", annual.target.area, "ha", "\n"))
    }
  }
  track.fires$rem = pmax(0, track.fires$atarget - track.fires$aburnt - track.fires$asupp.fuel - track.fires$asupp.mosaic)
  lc.burnt = subland %>% filter(cell.id %in% burnt.cells) %>% group_by(region, lct) %>% summarise(burnt = length(cell.id) * params$ha.cell, .groups = "drop")
  track.lc.burnt.supp = subland %>% filter(cell.id %in% eval.cells) %>% group_by(region, lct) %>% summarise(target = length(cell.id) * params$ha.cell, .groups = "drop") %>% 
    left_join(lc.burnt, by = c("region", "lct")) %>% mutate(supp = ifelse(is.na(target - burnt), target, target - burnt))
  track.lc.burnt.supp[is.na(track.lc.burnt.supp)] = 0
  track.forest.age.burnt = subland %>% filter(cell.id %in% burnt.cells, lct %in% c("pine", "oak")) %>% group_by(region, lct, tschg) %>% summarise(area = length(cell.id) * params$ha.cell, .groups = "drop")
  track.forest.age.supp = subland %>% filter(cell.id %in% eval.cells, lct %in% c("pine", "oak")) %>% group_by(region, lct, tschg) %>% summarise(target = length(cell.id) * params$ha.cell, .groups = "drop") %>% 
    left_join(track.forest.age.burnt, by = c("region", "lct", "tschg")) %>% mutate(area = ifelse(is.na(target - area), target, target - area)) %>% dplyr::select(-target)
  if (out.maps) {
    return(list(burnt.cells = burnt.cells, track.fires = track.fires, track.lc.burnt.supp = track.lc.burnt.supp, 
                track.forest.age.burnt = track.forest.age.burnt, track.forest.age.supp = track.forest.age.supp, map = map))
  } else {
    return(list(burnt.cells = burnt.cells, track.fires = track.fires, track.lc.burnt.supp = track.lc.burnt.supp,
                track.forest.age.burnt = track.forest.age.burnt, track.forest.age.supp = track.forest.age.supp))
  }
}
