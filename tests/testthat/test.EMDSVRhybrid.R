library(testthat)
require(EMDSVRhybrid)

#Dataset generation

set.seed(6)
example_data=rnorm(500,30,5)

#Parameter setting
k <-  0.9 #partition of data into 90:10 ratio

#Application of EMDANN model (Using radial basis kernel function)
EMDSVRhybrid(example_data,0.9,funct="radial")
