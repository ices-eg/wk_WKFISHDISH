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

plist <- array(list(), dim = c(length(species), 4),
               dimnames = list(species, 1:4))

for (i in seq_along(species)) {
  cat("\nWorking on species:", species[i], " (", i ,"/", length(species), ")\n"); flush.console()

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
    envs <- lapply(seq_along(which.load), function(x) new.env(parent = .GlobalEnv))
    names(envs) <- snames[which.load]
    # load data and models into each environment
    for (ii in seq_along(which.load)) load(sfiles[which.load][ii], envir = envs[[ii]])

    # what comparisons do we run
    comps <- lapply(stab$Division, function(x)  which(stab$Division != x))
    pairings <- cbind(rep(1:length(comps), sapply(comps, length)),
                      unlist(comps))
    pairings <- unique(t(apply(pairings, 1, sort)))

    # create a matrix to store hypothesis tests
    pnames <- paste0(stab$Survey.name, stab$Division)
    pmat <- matrix(NA, length(comps), length(comps),
                   dimnames = list(pnames, pnames))

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
      lograt <- log(sim1[common_years,,]) - log(sim2[common_years,,])
      # if there are infinites, replace with maximum seen... a bit of a hack
      lograt[lograt == -Inf] <- min(lograt[is.finite(lograt)])
      lograt[lograt == Inf] <- max(lograt[is.finite(lograt)])

      year <- as.numeric(common_years)

      slope.sim <- apply(lograt, 2, function(x) coef(lm(x ~ year))["year"])
      p.value <- 2 * min(sum(slope.sim < 0), sum(slope.sim > 0)) / (length(slope.sim) + 1)
      pmat[pair[1], pair[2]] <- p.value
      pmat[pair[2], pair[1]] <- p.value

      # quick plot
      # plot data
      main <- paste("log ratio", scomp$Division[1], "/", scomp$Division[2], "Q", qtr)
      y <- apply(lograt, 1, quantile, c(0.025, .5, .975), na.rm = TRUE)
      plot(year, y[2,], type = "p",
              pch = 16, cex = 0.7,
              axes = FALSE, ylab = "log cpue ratio", xlab = "Year", main = main,
              ylim = range(y))
      arrows(year, y[1,], y1 = y[3,], code = 3, angle = 90, length = 0.1)
      box(bty = "l")
      axis(1)
      axis(2, las = 1)

    }
    plist[species[i], qtr] <- list(pmat)
  }

  dev.off()
}


if (!dir.exists("output")) dir.create("output")
save(plist, file = "output/plist.rdata")

if (FALSE) {
  ## DO NOT RUN
  # move plots to sharepoint
  from <- paste0("figures/", species, "_log_ratio_plots.pdf")
  todir <- paste0("C:/Users/colin/SharePoint/WKFISHDISH - 2016 Meeting docs/04. Working documents/log_ratio_plots")
  to <- paste0(todir, "/", species, "_log_ratio_plots.pdf")
  create <- !dir.exists(todir)
  tmp <- lapply(which(create), function(i) dir.create(todir[i]))
  file.copy(from, to, overwrite = TRUE)
}

