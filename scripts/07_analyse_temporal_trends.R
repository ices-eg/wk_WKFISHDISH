# -------------------------------------
#
# Analyse spatial trends
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")

# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

for (i in seq_along(species)) {
  cat("Working on species:", species[i], " (", i ,"/", length(species), ")\n"); flush.console()

  pdf(paste0("figures/", species[i], "_log_ratio_plots.pdf"), onefile = TRUE)

  # what surveys are on offer
  for (qtr in 1:4) {
    stab <- subset(fulltab, Species == species[i] & Quarter == qtr)
    if (nrow(stab) < 2) {
      next
    }
    # so 28 pairings for for Megrim Q1 2007..
    # but what ones have sufficient data to estimate
    # load data
    snames <- unique(stab$Survey.name)
    sfiles <- paste0("species/", species[i], "/intermediate_data/", snames, "_", qtr,"_sims.rData")
    if (!any(file.exists(sfiles))) {
      message("No simulations for: ", species[i]," Q", qtr)
      next
    }
    cat("\r\tWorking on", species[i], " Q", qtr); flush.console()

    which.load <- which(file.exists(sfiles))
    # create environments for each survey
    envs <- lapply(snames[which.load], function(x) new.env(parent = .GlobalEnv))
    names(envs) <- snames[which.load]
    # load data and models into each environment
    for (ii in which.load) load(sfiles[ii], envir = envs[[ii]])

    # what comparisons do we run
    comps <- lapply(stab$Division, function(x)  which(stab$Division != x))
    pairings <- cbind(rep(1:length(comps), sapply(comps, length)),
                      unlist(comps))
    pairings <- unique(t(apply(pairings, 1, sort)))

    # select a pair to analyse
    for (k in 1:nrow(pairings)) {
      pair <- pairings[k,]
      scomp <- stab[pair,]

      # make copies of appropriate data
      sim1 <- envs[[scomp$Survey.name[1]]]$sims[,trimws(scomp$Division[1]),,drop=FALSE]
      sim2 <- envs[[scomp$Survey.name[2]]]$sims[,trimws(scomp$Division[2]),,drop=FALSE]

      common_years <- intersect(dimnames(sim1)[[1]], dimnames(sim2)[[1]])
      if (length(common_years) < 2) {
        next
      }

      # calculate ratio for common years
      lograt <- log(sim1[common_years,]) - log(sim2[common_years,])

      year <- as.numeric(common_years)

      slope.sim <- apply(lograt, 2, function(x) coef(lm(x ~ year))["year"])
      p.value <- 2 * min(sum(slope.sim < 0), sum(slope.sim > 0)) / (length(slope.sim) + 1)

      # quick plot
      # plot data
      matplot(year, lograt, type = "p",
              pch = 16, cex = 0.7, col = paste0(colorRampPalette("grey50")(1), "11"),
              axes = FALSE, ylab = "log cpue", xlab = "Year", main = main)
      box(bty = "l")
      axis(1)
      axis(2, las = 1)

    }
  }

  dev.off()
}

