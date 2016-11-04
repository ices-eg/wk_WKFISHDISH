
# ----------------------------
#
#   load packages
#
# ----------------------------

# ices webservice packages
library(icesDatras)
library(icesVocab)

# for modelling
library(mgcv, quietly = TRUE)
library(gmrf, quietly = TRUE)
library(Matrix, quietly = TRUE)

# spatial stuff
library(rgdal, quietly = TRUE)
library(rgeos, quietly = TRUE)
library(sp, quietly = TRUE)
library(spdep, quietly = TRUE)
library(raster, quietly = TRUE)

# for plotting
library(lattice, quietly = TRUE)
library(latticeExtra, quietly = TRUE)


# clear workspace -------------------

rm(list = ls())

# load local utilites -------------------

source("scripts/utilities.R")
