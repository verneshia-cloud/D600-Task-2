# run_optimization.R
# Helper to run the D2 optimization end-to-end

message("Running scripts/02_optimize_model.R ...")
source("scripts/02_optimize_model.R")
message("Done. Artifacts written to ./outputs:")
print(list.files("outputs", full.names = TRUE))
