## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
##Example how the package works
library(EMDSVRhybrid)

#Application
# A Random time series dataset generation
set.seed(6)
example_data=rnorm(500,30,5)

#Parameter setting
k <-  0.9

#Application of EMDANN model
EMDSVRhybrid(example_data,0.9,funct="radial")


