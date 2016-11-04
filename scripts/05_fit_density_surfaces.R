# -------------------------------------
#
# Fit density surface for each survey
#
# -------------------------------------


# load packages etc.
source("scripts/header.R")


# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

# read in spatial datasets
area <- readOGR("shapefiles", "ICES_Areas_20160601_dense")
statrec <- readOGR("shapefiles", "ICES_StatRec_mapto_ICES_Areas")
statrec <- spTransform(statrec, crs(area)) # transdorm to wgs84


for (i in seq_along(species)) {
  cat("                                    \rWorking on species:", species[i], " (", i ,"/", length(species), ")"); flush.console()

  # loop over surveys
  stab <- subset(fulltab, Species == species[i])
  stab <- unique(stab[c("Survey.name", "Quarter")])

  for (j in 1:nrow(stab)) {
    # load data
    file <- paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_data.rData")
    if (!file.exists(file)) {
      warning("missing data for: ", species[i], " ", stab$Survey.name[j], " ", stab$Quarter[j])
      next
    }
    load(file)

    # fit a gam


  }
}



  # do some modelling
  years <- unique(dat$Year)
  nyears <- length(years)
  dat$fStatRec <- factor(dat$StatRec, levels = statrec $ StatRec)
  statrec$fStatRec <- factor(statrec$StatRec)

  # substitute zero with half minumum observed catch weight
  min_weight <- min(dat$weight[dat$weight>0], na.rm = TRUE)
  dat$adj_weight <- dat$weight
  dat$adj_weight[dat$adj_weight == 0] <- min_weight/2

  # container for results
  gs <- list()

  # run models
  for (i in years) {
    cat("                              \rfitting year", i); flush.console()
    # subset
    fdat <- subset(data.frame(dat), Year == i)
    # fit
    g <- gam(adj_weight ~ s(fStatRec, bs = "mrf", xt = list(penalty = Q), k = 50),
             data = fdat, family = Gamma(log),
             drop.unused.levels = FALSE)

    gs[[paste(i)]] <- g
  }


  # model fit plotting
  # set up color scale
  cols <- gplots::rich.colors(50)
  fit <- sapply(gs, predict, newdata = statrec@data)
  # make years sum to 1
  fit <- sweep(fit, 2, colSums(fit), "/")
  fit[] <- as.numeric(cut(fit, 50))

  pdf("figure/spatial_fit_plots.pdf", onefile = TRUE)

  for (i in unique(dat$Year)) {
    plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], col = grey(0.5))
    plot(statrec, col = cols[fit[,paste(i)]], add = TRUE)
    plot(area, lwd = 2, add = TRUE)
    mtext(paste(i), font = 2)
  }

  dev.off()


  # model anomaly fit plotting
  # set up color scale
  cols <- gplots::rich.colors(50)
  fit <- sapply(gs, predict, newdata = statrec@data)
  # make years sum to 1
  fit <- sweep(fit, 2, colSums(fit), "/")
  fit <- fit - fit[,1]
  fit[] <- as.numeric(cut(fit, 50))
  pdf("figure/spatial_anomaly_plots.pdf", onefile = TRUE)

  for (i in unique(dat$Year)) {
    plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], col = grey(0.5))
    plot(statrec, col = cols[fit[,paste(i)]], add = TRUE)
    plot(area, lwd = 2, add = TRUE)
    mtext(paste(i), font = 2)
  }

  dev.off()

  # model se plotting
  # set up color scale
  cols <- gplots::rich.colors(50)
  sefit <- sapply(gs, function(x) predict(x, newdata = statrec@data, se.fit = TRUE)$se.fit)
  sefit[] <- as.numeric(cut(sefit, 50))

  pdf("figure/spatial_sefit_plots.pdf", onefile = TRUE)

  for (i in unique(dat$Year)) {
    plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], col = grey(0.5))
    plot(statrec, col = cols[sefit[,paste(i)]], add = TRUE)
    mtext(paste(i), font = 2)
  }

  dev.off()




  # data plotting
  pdf("figure/data_plots.pdf", onefile = TRUE)

  for (i in unique(dat$Year)) {
    base()
    pdat <- dat[dat$Year == i,]
    plot(pdat, pch = 16, cex = 0.25*log(pdat$weight), add = TRUE)
    plot(pdat, pch = 1, cex = 0.1, col = grey(0.3), add = TRUE)
    mtext(paste(i), font = 2)
  }

  dev.off()




