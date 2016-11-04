# ----------------------------
#
#   install required packages
#
# ----------------------------

# install other dependencies used in the analysis
pkgs <- c("mgcv", "rgdal","rgeos","sp","spdep","Matrix", "raster","lattice","latticeExtra")
if (length(pkgs)) install.packages(pkgs)



# install
# NOTE must have devtools installed
devtools::install_github("ices-tools-prod/icesVocab")
devtools::install_github("ices-tools-prod/icesDatras")
devtools::install_github("faskally/gmrf")
