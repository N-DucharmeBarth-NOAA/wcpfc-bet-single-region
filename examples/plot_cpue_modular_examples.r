# Example usage of plot_cpue_modular function
# This script demonstrates how to use the new plot_cpue_modular() function

library(data.table)
library(ggplot2)

# Source the helper functions
proj_dir = this.path::this.proj()
source(file.path(proj_dir, "code", "ss3", "helper-fns", "report-helpers.r"))

# Example 1: Single model from file path
# ========================================
ss3_cpue_file = file.path(proj_dir, "model-files", "ss3", "01-bet-base", "cpue.csv")
if (file.exists(ss3_cpue_file)) {
  p1 = plot_cpue_modular(ss3_cpue_file, show_se = TRUE, use_log_scale = FALSE)
  print(p1)
  # ggsave("cpue_single_model.png", p1, width = 10, height = 8)
}

# Example 2: Single model from data.table
# ========================================
if (file.exists(ss3_cpue_file)) {
  ss3_cpue = fread(ss3_cpue_file)
  p2 = plot_cpue_modular(ss3_cpue, show_se = TRUE, use_log_scale = FALSE)
  print(p2)
}

# Example 3: Multiple models comparison
# ======================================
ss3_cpue_file = file.path(proj_dir, "model-files", "ss3", "01-bet-base", "cpue.csv")
mfcl_cpue_file = file.path(proj_dir, "model-files", "mfcl", "v11", "cpue.csv")

if (file.exists(ss3_cpue_file) && file.exists(mfcl_cpue_file)) {
  # Read CPUE data from both models
  ss3_cpue = fread(ss3_cpue_file)
  ss3_cpue[, model_label := "SS3"]
  
  mfcl_cpue = fread(mfcl_cpue_file)
  mfcl_cpue[, model_label := "MFCL"]
  
  # Combine the data
  cpue_combined = rbindlist(list(ss3_cpue, mfcl_cpue), use.names = TRUE, fill = TRUE)
  
  # Create the plot
  p3 = plot_cpue_modular(cpue_combined, show_se = TRUE, use_log_scale = FALSE)
  print(p3)
  # ggsave("cpue_multi_model.png", p3, width = 12, height = 8)
}

# Example 4: Log scale plot
# ==========================
if (file.exists(ss3_cpue_file)) {
  ss3_cpue = fread(ss3_cpue_file)
  p4 = plot_cpue_modular(ss3_cpue, show_se = TRUE, use_log_scale = TRUE)
  print(p4)
}

# Example 5: Custom fleet names
# ==============================
if (file.exists(ss3_cpue_file)) {
  ss3_cpue = fread(ss3_cpue_file)
  
  # Get unique fleet IDs
  unique_fleets = sort(unique(ss3_cpue$Fleet))
  n_fleets = length(unique_fleets)
  
  # Create custom names
  custom_names = paste0("Custom Fleet ", seq_len(n_fleets))
  
  p5 = plot_cpue_modular(ss3_cpue, 
                         fleet_names = custom_names,
                         show_se = TRUE, 
                         use_log_scale = FALSE)
  print(p5)
}

# Example 6: Without error bars
# ==============================
if (file.exists(ss3_cpue_file)) {
  ss3_cpue = fread(ss3_cpue_file)
  p6 = plot_cpue_modular(ss3_cpue, show_se = FALSE, use_log_scale = FALSE)
  print(p6)
}

# Example 7: Different facet layout
# ==================================
if (file.exists(ss3_cpue_file)) {
  ss3_cpue = fread(ss3_cpue_file)
  p7 = plot_cpue_modular(ss3_cpue, show_se = TRUE, use_log_scale = FALSE, n_col = 3)
  print(p7)
}
