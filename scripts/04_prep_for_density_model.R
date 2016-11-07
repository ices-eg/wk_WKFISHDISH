# -------------------------------------
#
# Prepare for density modelling
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")



# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

# read in spatial datasets
area <- readOGR("shapefiles", "ICES_Areas_20160601_dense")
# hard code projection in case GDAL is not installed
proj4string(area) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
statrec <- readOGR("shapefiles", "ICES_StatRec_mapto_ICES_Areas")
# hard code projection in case GDAL is not installed
proj4string(statrec) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +no_defs")
statrec <- spTransform(statrec, crs(area)) # transform to wgs84


# create intermediate data files: striped down spatial data etc.

for (i in seq_along(species)) {
  cat("\rWorking on species:", species[i], " (", i ,"/", length(species), ")", rep(" ", 50)); flush.console()

  dat <- read.csv(paste0("species/", species[i], "/hh_with_weight.csv"))

  # keep only some columns
  hkeep <- c("HaulDur", "ShootLong", "ShootLat", "StatRec",
             "Survey", "Quarter", "Year", "Month", "Day",
             "weight")
  dat <- dat[hkeep]

  # remove NAs
  dat <- dat[complete.cases(dat),]

  # convert dat to spatial
  coordinates(dat) <- ~ ShootLong + ShootLat
  crs(dat) <- crs(statrec)

  # loop over surveys
  stab <- subset(fulltab, Species == species[i])
  stab <- unique(stab[c("Survey.name", "Quarter")])

  for (j in 1:nrow(stab)) {

    sdat <- dat[dat$Survey == stab$Survey.name[j] & dat$Quarter == stab$Quarter[j],]

    if (nrow(sdat) == 0) {
      message("missing data for: ", species[i], " ", stab$Survey.name[j], " ", stab$Quarter[j])
      next
    }

    # keep only sampled areas
    tmp <- gContains(area, sdat, byid = TRUE)
    sarea <- area[colSums(tmp) > 0,]
    sstatrec <- statrec[statrec$Max_Area %in%
                          gsub(".NA", "",
                               paste(sarea$SubArea, sarea$Division, sarea$SubDivisio, sep = ".")),]

    # predict over statsquares that have been sampled at least once ? twice?
    tmp <- gContains(sstatrec, sdat, byid = TRUE)
    statrec_pred <- sstatrec[colSums(tmp) > 0,]

    # prepare GMRF for spatial model
    adj <- spdep::poly2nb(sstatrec, queen = FALSE)
    adj <- spdep::nb2mat(adj, style = "B", zero.policy = TRUE)
    Q <- methods::as(adj, "dgTMatrix")

    Q @ x[] <- -1/Q @ x
    diag(Q) <- rowSums(adj)
    Q <- as.matrix(Q)
    colnames(Q) <- rownames(Q) <- sstatrec $ ICESNAME
    sstatrec $ StatRec <- sstatrec $ ICESNAME
    statrec_pred $ StatRec <- statrec_pred $ ICESNAME

    if (!dir.exists(paste0("species/", species[i], "/intermediate_data/"))) {
      dir.create(paste0("species/", species[i], "/intermediate_data/"))
    }

    save(sarea, sstatrec, statrec_pred, sdat, Q,
         file = paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_data.rData"))
  }
}

