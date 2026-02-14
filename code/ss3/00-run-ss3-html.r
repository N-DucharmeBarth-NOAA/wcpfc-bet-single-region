# Quick script to generate SS3 HTML output
# SS_plots() automatically generates HTML when png=TRUE (default)
library(r4ss)

# Set the path to your SS3 model directory
model_dir <- "model-files/ss3/01-bet-base"

# Read the SS3 output (Report.sso, data.ss_new, control.ss_new, etc.)
replist <- SS_output(dir = model_dir, verbose = TRUE)

# Generate all plots and HTML in one call
# This creates:
#   - PNG files in model_dir/plots/
#   - plotInfoTable CSV file (plot metadata)
#   - _SS_output.html and tabbed category pages
# By default: png=TRUE, html=TRUE, so HTML is auto-generated
SS_plots(
  replist = replist,
  dir = model_dir,
  plot = 1:26,           # which plot groups (1:26 for all, subset as needed)
  png = TRUE,            # create PNG files
  html = TRUE,           # automatically generate HTML from PNGs
  printfolder = "plots", # subdirectory for PNG and HTML files
  verbose = TRUE,
  catchasnumbers = FALSE # change to TRUE if catch is in numbers (not biomass)
)

cat("\n✓ Complete SS3 model output generated with tabbed HTML viewer!\n")
