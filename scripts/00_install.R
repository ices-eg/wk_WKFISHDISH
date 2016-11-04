# ----------------------------
#
#   install required packages
#
# ----------------------------

# install other dependencies used in the analysis
pkgs <- c("devtools")
if (length(pkgs)) install.packages(pkgs)

# install
devtools::install_github("ices-tools-prod/icesVocab")
