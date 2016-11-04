# -------------------------------------
#
# download data files
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")



force.download <- FALSE

if (!file.exists("shapefiles/ICES_Areas_20160601_dense.shp") | force.download) {
  download.ICESshape("ICES_areas")
}

if (!file.exists("shapefiles/ICES_StatRec_mapto_ICES_Areas.shp") | force.download) {
  download.ICESshape("ICES_StatRec_mapto_ICES_Areas")
}

# read design table and look download all required surveys
ctab <- getControlTable()

# surveys to get are:
toget <- unique(ctab[c("Survey.name", "Quarter", "Start.year")])
tab <- with(toget, tapply(Start.year, list(Survey.name, Quarter), min))
toget <- expand.grid(Survey.name = gsub("[[:space:]]*$", "",rownames(tab)),
                     Quarter = as.integer(colnames(tab)),
                     stringsAsFactors = FALSE,
                     KEEP.OUT.ATTRS = FALSE)
toget$Start.year <- c(tab)
toget <- toget[!is.na(toget$Start.year),]
row.names(toget) <- NULL

# loop over surveys and download (NOTE final year fixed at 2015)
for (i in 1:nrow(toget)) {
  # create file name
  fname <- datras.fname("ca",
                        toget[i, "Survey.name"],
                        toget[i, "Start.year"],
                        2015,
                        toget[i, "Quarter"])

  # download
  if (!file.exists(fname) | force.download) {
    download.Datras(toget[i, "Survey.name"], toget[i, "Start.year"], 2015, toget[i, "Quarter"])
  }
}

# done ---------------------------------------------
