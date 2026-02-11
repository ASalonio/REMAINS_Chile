# Script to compare result.rda and results_2018.rda
# This will help identify which scenario each file came from

# Clean workspace
rm(list = ls())

cat("===== ANALYZING RESULT FILES =====\n\n")

write.table(results_now, "C:/Repos/REMAINS_Chile/Chile/outputs/results_now.txt",
            quote = FALSE, row.names = FALSE, sep = "\t")

write.table(results_2018, "C:/Repos/REMAINS_Chile/Chile/outputs/results_2018.txt",
            quote = FALSE, row.names = FALSE, sep = "\t")

# ---- Load first file ----
cat("Loading result.rda...\n")
load("C:/Repos/REMAINS_Chile/Chile/outputs/0.5/result.rda")  # Adjust path if needed

# The load() command will load an object - we need to find its name
objects_from_file1 <- ls()
cat("Objects loaded from result.rda:", paste(objects_from_file1, collapse=", "), "\n")

# Assuming the main result object is called 'res' or similar
# Save it with a different name to avoid overwriting
if ("res" %in% objects_from_file1) {
  result1 <- res
  rm(res)
} else {
  # If not 'res', take the first object
  result1 <- get(objects_from_file1[1])
}

cat("\n---- RESULT.RDA SUMMARY ----\n")
cat("Structure of result object:\n")
print(names(result1))

# Check if there's a 'fires' data frame
if ("fires" %in% names(result1)) {
  fires1 <- result1$fires
  
  cat("\nTotal burnt area (aburnt):", sum(fires1$aburnt, na.rm=TRUE), "hectares\n")
  cat("Total target area (atarget):", sum(fires1$atarget, na.rm=TRUE), "hectares\n")
  cat("Total suppressed area:", sum(fires1$asupp.fuel + fires1$asupp.mosaic, na.rm=TRUE), "hectares\n")
  cat("Number of fires:", nrow(fires1), "\n")
  cat("Years covered:", min(fires1$year), "to", max(fires1$year), "\n")
  cat("Number of runs:", length(unique(fires1$run)), "\n")
  
  # Calculate average annual area
  avg_annual_1 <- sum(fires1$atarget, na.rm=TRUE) / (length(unique(fires1$year)) * length(unique(fires1$run)))
  cat("Average annual target area per run:", round(avg_annual_1, 2), "hectares\n")
}

# ---- Load second file ----
cat("\n\nLoading results_2018.rda...\n")
load("C:/Repos/REMAINS_Chile/Chile/outputs/0.5/results_2018.rda")  # Adjust path if needed

objects_from_file2 <- ls()
objects_from_file2 <- objects_from_file2[!objects_from_file2 %in% c("result1", "fires1", "objects_from_file1", "avg_annual_1")]
cat("Objects loaded from results_2018.rda:", paste(objects_from_file2, collapse=", "), "\n")

if ("res" %in% objects_from_file2) {
  result2 <- res
  rm(res)
} else {
  result2 <- get(objects_from_file2[1])
}

cat("\n---- RESULTS_2018.RDA SUMMARY ----\n")
cat("Structure of result object:\n")
print(names(result2))

if ("fires" %in% names(result2)) {
  fires2 <- result2$fires
  
  cat("\nTotal burnt area (aburnt):", sum(fires2$aburnt, na.rm=TRUE), "hectares\n")
  cat("Total target area (atarget):", sum(fires2$atarget, na.rm=TRUE), "hectares\n")
  cat("Total suppressed area:", sum(fires2$asupp.fuel + fires2$asupp.mosaic, na.rm=TRUE), "hectares\n")
  cat("Number of fires:", nrow(fires2), "\n")
  cat("Years covered:", min(fires2$year), "to", max(fires2$year), "\n")
  cat("Number of runs:", length(unique(fires2$run)), "\n")
  
  # Calculate average annual area
  avg_annual_2 <- sum(fires2$atarget, na.rm=TRUE) / (length(unique(fires2$year)) * length(unique(fires2$run)))
  cat("Average annual target area per run:", round(avg_annual_2, 2), "hectares\n")
}

# ---- Comparison ----
cat("\n\n===== COMPARISON =====\n")
if (exists("fires1") && exists("fires2")) {
  cat("\nTotal burnt area comparison:\n")
  cat("  result.rda:       ", format(sum(fires1$aburnt, na.rm=TRUE), big.mark=","), "ha\n")
  cat("  results_2018.rda: ", format(sum(fires2$aburnt, na.rm=TRUE), big.mark=","), "ha\n")
  
  cat("\nAverage annual target area per run:\n")
  cat("  result.rda:       ", format(round(avg_annual_1, 2), big.mark=","), "ha\n")
  cat("  results_2018.rda: ", format(round(avg_annual_2, 2), big.mark=","), "ha\n")
  
  cat("\n** INTERPRETATION **\n")
  if (avg_annual_1 < 5000) {
    cat("result.rda appears to use pb_area = 1000 (Scenario 0.1)\n")
  } else {
    cat("result.rda appears to use pb_area = 20000 (Scenario 0.5 or 0.9)\n")
  }
  
  if (avg_annual_2 < 5000) {
    cat("results_2018.rda appears to use pb_area = 1000 (Scenario 0.1)\n")
  } else {
    cat("results_2018.rda appears to use pb_area = 20000 (Scenario 0.5 or 0.9)\n")
  }
}

cat("\n===== DONE =====\n")
