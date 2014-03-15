library(shiny)
suppressPackageStartupMessages(library(googleVis))
library(election2011)

load("analysis_code/shiny/motionchart/IncomeTA_median.rda")

# google way
plot(gvisMotionChart(IncomeTA_median, idvar="NAME", timevar="Year"))
