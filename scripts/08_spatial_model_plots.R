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

for (i in seq_along(species)) {
  cat("Working on species:", species[i], " (", i ,"/", length(species), ")\n"); flush.console()

  # loop over surveys
  stab <- subset(fulltab, Species == species[i])
  stab <- unique(stab[c("Survey.name", "Quarter")])
  row.names(stab) <- NULL

  if (!dir.exists("figures")) dir.create("figures")

  pfile <- paste0("figures/", species[i], "_spatial_model.pdf")

  pdf(pfile, onefile = TRUE, paper = "a4")

  for (j in 1:nrow(stab)) {

    # load data
    dfile <- paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_data.rData")
    gfile <- paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_gams.rData")
    if (!file.exists(gfile)) {
      message("No model for: ", species[i], " ", stab$Survey.name[j], " ", stab$Quarter[j])
      next
    }
    cat("\r\tWorking on", stab$Survey.name[j], "Q", stab$Quarter[j], rep(" ", 50)); flush.console()

    load(dfile)
    load(gfile)

    # HACK!
    # SP-NORTH and NIGFS did not read in E squares properly
    if (stab$Survey.name[j] %in% c("SP-NORTH", "NIGFS")) {
      sdat$StatRec <- paste0(substring(sdat$StatRec, 1, 2), "E", nchar(substring(sdat$StatRec, 3)))
    }

    # set par - starts a new page
    par(mfrow = c(2,2), mar = c(0,0,1.5,0))

    # plot front plot
    plot(sarea,xlim = bbox(sstatrec)["x",], ylim = bbox(sstatrec)["y",], col = gplots::rich.colors(nrow(sarea), alpha = 0.5))
    plot(sstatrec, add = TRUE)
    plot(statrec_pred, add = TRUE, col = grey(0.5, alpha = 0.5))
    mtext(paste0(stab$Survey.name[j], " Q", stab$Quarter[j]), font = 2)

    # colours
    #cols <- gplots::rich.colors(50)
    cols <- colorRampPalette(c("cyan", "magenta"))(50)

    # covariates
    statrec_pred$fStatRec <- factor(statrec_pred$StatRec, levels = sstatrec$StatRec)

    # data
    years <- names(gs)
    fit <- sapply(years, function(yr) predict(gs[[yr]], newdata = statrec_pred))
    colnames(fit) <- years
    min_weight <- min(sdat$weight[sdat$weight>0], na.rm = TRUE)

    for (yr in years) {
      tmp <- fit[,yr]
      min <- min(tmp)
      min <- log(min_weight/10)
      max <- max(tmp)+0.1
      breaks <- seq(min, max, length = 50)
      tmp[] <- cols[as.numeric(cut.default(tmp, breaks = breaks))]
      # set colour of small values?
      tmp[fit[,yr] < min] <- grey(0.5)

      plot(sstatrec, xlim = bbox(sstatrec)["x",], ylim = bbox(sstatrec)["y",])
      plot(statrec_pred, col = tmp, add = TRUE)
      mtext(paste0(species[i], " ", stab$Survey.name[j], " Q", stab$Quarter[j], " ", yr), font = 2, line = -0.25)
      # plot centre of mass?
      cmass <- colSums(coordinates(statrec_pred) * exp(fit[,yr])) / sum(exp(fit[,yr]))
      cmass <- SpatialPoints(matrix(cmass, 1), proj4string = CRS(proj4string(statrec_pred)))
      points(cmass, pch = 16, col = "red", cex = 1.5)
    }
  }
  dev.off()
}


if (FALSE) {
  ## DO NOT RUN
  # move plots to sharepoint
  from <- paste0("figures/", species, "_survey_data.pdf")
  todir <- paste0("C:/Users/colin/SharePoint/WKFISHDISH - 2016 Meeting docs/04. Working documents/Trend Analyses/",
                  species, "/Maps_survey_data")
  to <- paste0(todir, "/", species, "_survey_data.pdf")
  create <- !dir.exists(todir)
  tmp <- lapply(which(create), function(i) dir.create(todir[i]))
  file.copy(from, to, overwrite = TRUE)
}

