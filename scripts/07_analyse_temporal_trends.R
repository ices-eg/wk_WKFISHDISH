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

  # what surveys are on offer
  qtr <- 1
  stab <- subset(fulltab, Species == species[i] & Quarter == qtr)

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
  for (i in which.load) load(sfiles[i], envir = envs[[i]])

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
    sim1 <- envs[[scomp$Survey.name[1]]]$sims[,trimws(scomp$Division[1]),]
    sim2 <- envs[[scomp$Survey.name[2]]]$sims[,trimws(scomp$Division[2]),]

    common_years <- intersect(dimnames(sim1)[[1]], dimnames(sim2)[[1]])

    # calculate ratio for common years
    lograt <- log(sim1[common_years,]) - log(sim2[common_years,])

    year <- as.numeric(common_years)

    slope.sim <- apply(lograt, 2, function(x) coef(lm(x ~ year))["year"])
    hist(slope.sim)



  # analyse cpue by area

  # plot
  pdf("figure/temporal_subarea_plots.pdf", onefile = TRUE)

  for (sa in subareas) {
    # plot data
    matplot(years, log(sims[,sa,]), type = "p",
            pch = 16, cex = 0.7, col = paste0(colorRampPalette("lightblue")(1), "11"),
            axes = FALSE, ylab = "log cpue", xlab = "Year", main = sa)
    # plot model fits
    x <- log(fitsims[,sa,])
    matplot(years, x, type = "l", lty = 1, col = paste0(grey(0.1), "11"), add = TRUE)
    box(bty = "l")
    axis(1)
    axis(2, las = 1)
  }

  dev.off()




  # analyse cpue ratio by area pair

  # fit a trend model to each simulated time series
  pairs <- combn(3, 2)
  lratiosims <- array(NA, c(nyears, nsubareas, nsim), dimnames = list(years, 1:ncol(pairs), NULL))
  fitlratiosims <- array(NA, c(nyears, nsubareas, nsim), dimnames = list(years, 1:ncol(pairs), NULL))
  for (j in 1:ncol(pairs)) {
    cat("                              \rfitting comparison", j, "of", ncol(pairs)); flush.console()
    lratiosims[,j,] <- log(sims[,pairs[1,j],]) - log(sims[,pairs[2,j],])
    # fit to each simulation
    fitlratiosims[,j,] <-
      sapply(1:nsim,
             function(i) {
               x <- lratiosims[,j,i]
               fitted(gam(x ~ s(years, m = 2, k = nyears-1)))
             })
  }

  # plot
  pdf("figure/temporal_logratio_plots.pdf", onefile = TRUE)

  for (j in 1:ncol(pairs)) {
    # plot data
    matplot(years, lratiosims[,j,], type = "p",
            pch = 16, cex = 0.7, col = paste0(colorRampPalette("lightblue")(1), "11"),
            axes = FALSE, ylab = "log cpue ratio", xlab = "Year",
            main = paste("log", paste(subareas[pairs[,j]], collapse = "/")))
    # plot model fits
    x <- fitlratiosims[,j,]
    matplot(years, x, type = "l", lty = 1, col = paste0(grey(0.1), "11"), add = TRUE)
    cis <- apply(x, 1, function(x) HPDinterval(mcmc(x), .95))
    lines(years, cis[1,], lty = 2)
    lines(years, cis[2,], lty = 2)
    abline(h = 0, col = "red")
    box(bty = "l")
    axis(1)
    axis(2, las = 1)
  }

  dev.off()


  # Hypothesis test plots

  # plot
  pdf("figure/hypothesis_plots.pdf", onefile = TRUE)

  for (j in 1:ncol(pairs)) {
    # plot model fits
    x <- fitlratiosims[,j,]
    # is there a significant deviation from the first three years
    x <- x - rep(apply(x[1:3,], 2, mean), each = nrow(x))
    # or from the first year only
    #x <- x - rep(x[1,], each = nrow(x))

    matplot(years, x, type = "l", lty = 1, col = paste0(grey(0.1), "11"),
            axes = FALSE, ylab = "log cpue ratio", xlab = "Year",
            main = paste("log", paste(subareas[pairs[,j]], collapse = "/"), "relative to", years[1], "-", years[3]))
    cis <- apply(x, 1, function(x) HPDinterval(mcmc(x), .95))
    lines(years, cis[1,], lty = 2)
    lines(years, cis[2,], lty = 2)

    abline(h = 0, col = "red")
    box(bty = "l")
    axis(1)
    axis(2, las = 1)
  }

  dev.off()

}
