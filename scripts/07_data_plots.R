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

  pfile <- paste0("figures/", species[i], "_survey_data.pdf")

  pdf(pfile, onefile = TRUE, paper = "a4")

  for (j in 1:nrow(stab)) {

    # load data
    dfile <- paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_data.rData")
    if (!file.exists(dfile)) {
      message("No data for: ", species[i], " ", stab$Survey.name[j], " ", stab$Quarter[j])
      next
    }
    cat("\r\tWorking on", stab$Survey.name[j], "Q", stab$Quarter[j], rep(" ", 50)); flush.console()

    load(dfile)

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

    # data
    years <- sort(unique(sdat$Year))
    obs <-
      sapply(years,
             function(yr) with(subset(data.frame(sdat), Year == yr),
                               tapply(weight, factor(StatRec, levels = statrec_pred$StatRec), mean, na.rm = TRUE)))
    colnames(obs) <- years

    for (k in paste(years)) {
      tmp <- obs[,k]
      if (all(is.na(tmp))) next
      if (all(range(tmp, na.rm = TRUE) == c(0,0))) {
        min <- 0.1; max <- 1
      } else {
        min <- min(tmp[which(tmp>0)], na.rm = TRUE)
        min <- max(min/2, min-0.1)
        max <- max(tmp[is.finite(tmp)], na.rm = TRUE) + 0.1
      }
      breaks <- exp(seq(log(min), log(max), length = 50))
      tmp[] <- cols[as.numeric(cut.default(tmp, breaks = breaks))]
      tmp[which(obs[,k] == 0)] <- grey(0.7)
      tmp[which(obs[,k]>max(breaks))] <- cols[length(cols)]

      plot(sstatrec, xlim = bbox(sstatrec)["x",], ylim = bbox(sstatrec)["y",])
      plot(statrec_pred, col = tmp, add = TRUE)
      mtext(paste0(species[i], " ", stab$Survey.name[j], " Q", stab$Quarter[j], " ", k), font = 2, line = -0.25)
    }
  }
  dev.off()
}


if (FALSE) {
  ## DO NOT RUN
  # move plots to sharepoint
  from <- paste0("figures/", species, "_survey_data.pdf")
  todir <- paste0("C:/Users/colin/SharePoint/WKFISHDISH - 2016 Meeting docs/04. Working documents/spatial_model_data")
  to <- paste0(todir, "/", species, "_survey_data.pdf")
  create <- !dir.exists(todir)
  tmp <- lapply(which(create), function(i) dir.create(todir[i]))
  file.copy(from, to, overwrite = TRUE)
}

