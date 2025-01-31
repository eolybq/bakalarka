# start by loading in raw data and getting it clean for analysis 
do_clean_data <- TRUE
# now run the main statistical model
do_estimate <- TRUE
# now plot the analysis
do_plot <- TRUE

if (do_clean_data) source("clean_data.R", echo = TRUE)

if (do_estimate) source("estimate.R", echo = TRUE)

if (do_plot) source("plot.R", echo = TRUE)