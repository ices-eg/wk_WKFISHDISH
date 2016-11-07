# -------------------------------------
#
# Do some plots of data and model fits
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")

# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

i <- 1; j <- 1
for (i in seq_along(species)) {
  cat("Working on species:", species[i], " (", i ,"/", length(species), ")\n"); flush.console()

  # loop over surveys
  stab <- subset(fulltab, Species == species[i])
  stab <- unique(stab[c("Survey.name", "Quarter")])

  pfile <- paste0("species/", species[i], "/survey_data.pdf")

  pdf(pfile, onefile = TRUE, paper = "a4")


  for (j in 1:nrow(stab)) {
    # load data
    dfile <- paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_data.rData")
    gfile <- paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_gams.rData")
    if (!file.exists(gfile)) {
      message("missing model for: ", species[i], " ", stab$Survey.name[j], " ", stab$Quarter[j])
      next
    }
    cat("\rWorking on", stab$Survey.name[j], "Q", stab$Quarter[j], rep(" ", 50)); flush.console()

    load(dfile)
    load(gfile)

    # only keep models that didn't fail
    gkeep <- sapply(gs, is) == "gam"
    gs <- gs[gkeep]

    # model fit plotting
    # covariate
    statrec_pred$fStatRec <- factor(statrec_pred$StatRec)

    # colours
    cols <- gplots::rich.colors(50)

    # data
    years <- sort(unique(sdat$Year))
    obs <-
      sapply(years,
           function(yr) with(subset(data.frame(sdat), Year == yr),
                             tapply(weight, factor(StatRec, levels = statrec_pred$StatRec), mean, na.rm = TRUE)))
    colnames(obs) <- years
    breaks <- seq(0, max(obs, na.rm = TRUE), length = 50)
    tmp <- obs
    obs[] <- cols[as.numeric(cut.default(obs, breaks = breaks))]
    obs[which(tmp == 0)] <- grey(0.7)


    for (k in names(gs)) {

      plot(sstatrec, xlim = bbox(sstatrec)["x",], ylim = bbox(sstatrec)["y",])
      plot(statrec_pred, col = obs[,k], add = TRUE)
      #plot(sstatrec, col = fit[,k], xlim = bbox(sstatrec)["x",], ylim = bbox(sstatrec)["y",])
      #plot(sarea, col = grey(0.5), add = TRUE)
      #plot(sarea, lwd = 2, add = TRUE)
      mtext(paste0(species[i], " ", stab$Survey.name[j], " Q", stab$Quarter[j], " ", k), font = 2)
    }
  }
  dev.off()
}






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


