
# ----------------------------
#
#   load packages
#
# ----------------------------

# spatial stuff
library(rgdal, quietly = TRUE)
library(rgeos, quietly = TRUE)
library(sp, quietly = TRUE)
library(spdep, quietly = TRUE)
library(raster, quietly = TRUE)


# ices webservice packages
library(icesDatras)
library(icesVocab)

# workflow
library(purrr)
library(tidyr)
library(magrittr)
library(dplyr)

# database
library(DBI)
library(RSQLite)

# for modelling
library(mgcv, quietly = TRUE)
library(gmrf, quietly = TRUE)
library(Matrix, quietly = TRUE)
library(lubridate, quietly = TRUE)


# for plotting
library(lattice, quietly = TRUE)
library(latticeExtra, quietly = TRUE)


# clear workspace -------------------

rm(list = ls())

# load local utilites -------------------

source("scripts/utilities.R")
