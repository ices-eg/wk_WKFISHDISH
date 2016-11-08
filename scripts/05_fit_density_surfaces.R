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


for (i in seq_along(species)) {
  cat("\rWorking on species:", species[i], " (", i ,"/", length(species), ")", rep(" ", 50)); flush.console()

  # loop over surveys
  stab <- subset(fulltab, Species == species[i])
  stab <- unique(stab[c("Survey.name", "Quarter")])

  for (j in 1:nrow(stab)) {
    # load data
    file <- paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_data.rData")
    if (!file.exists(file)) {
      message("missing data for: ", species[i], " ", stab$Survey.name[j], " ", stab$Quarter[j])
      next
    }
    load(file)

    # HACK!
    # SP-NORTH and NIGFS did not read in E squares properly
    if (stab$Survey.name[j] %in% c("SP-NORTH", "NIGFS")) {
      sdat$StatRec <- paste0(substring(sdat$StatRec, 1, 2), "E", nchar(substring(sdat$StatRec, 3)))
    }

    # do some modelling
    dat <- data.frame(sdat)
    years <- unique(dat$Year)
    nyears <- length(years)
    dat$fStatRec <- factor(dat$StatRec, levels = sstatrec$StatRec)
    sstatrec$fStatRec <- factor(sstatrec$StatRec)
    statrec_pred$fStatRec <- factor(statrec_pred$StatRec, levels = sstatrec$StatRec)

    # substitute zero with half minumum observed catch weight
    min_weight <- min(dat$weight[dat$weight>0], na.rm = TRUE)
    dat$adj_weight <- dat$weight
    dat$adj_weight[dat$adj_weight == 0] <- min_weight/2

    # fit for each year
    gs <- lapply(years,
                 function(yrs) {
                   # check if there is enough data?
                   # or wrap in a try to get it going:
                   # set the smoothing to be related to the number of statsquares in the survey area
                   k <- max(3, min(20, floor(nrow(statrec_pred) / 5)))
                   try(
                     gam(log(adj_weight) ~ s(fStatRec, bs = "mrf", xt = list(penalty = Q), k = k),
                         data = subset(dat, Year == yrs),
                         drop.unused.levels = FALSE)
                   )
                 })
    names(gs) <- paste(years)

    save(gs,
         file = paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_gams.rData"))
  }
}

