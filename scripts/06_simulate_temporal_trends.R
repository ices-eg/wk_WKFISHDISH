# -------------------------------------
#
# Simulate spatial trends
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
  stab <- subset(fulltab, Species == species[i])
  stab_full <- stab

  # simulate by quarter and survey
  stab <- unique(stab[c("Survey.name", "Quarter")])
  stab <- stab[complete.cases(stab),]

  for (j in 1:nrow(stab)) {

    # load data
    dfile <- paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_data.rData")
    gfile <- paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_gams.rData")
    if (!file.exists(gfile)) {
      message("No models for: ", species[i], " ", stab$Survey.name[j], " ", stab$Quarter[j])
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


    # covariates
    statrec_pred$fStatRec <- factor(statrec_pred$StatRec, levels = sstatrec$StatRec)

    # data
    # keep only converged fits
    gs <- gs[sapply(gs, is) == "gam"]
    if (length(gs) == 0) {
      message("No converged models for: ", species[i], " ", stab$Survey.name[j], " ", stab$Quarter[j])
      next
    }

    nsim <- 1000

    years <- names(gs)
    nyears <- length(years)
    subareas <- unique(trimws(stab_full$Division)[stab_full$Survey.name == stab$Survey.name[j]])
    # make sure the survey covers the reported subareas
    subareas <- intersect(subareas,  sstatrec$subarea)
    nsubareas <- length(subareas)

    sstatrec$fStatRec <- factor(sstatrec$StatRec)
    sstatrec$subarea <- sapply(strsplit(sstatrec$Max_Area, "[.]"), function(x) paste(x[1:2], collapse = "."))
    sstatrec <- sstatrec[sstatrec$subarea %in% subareas,]

    # simulate from each subarea
    sims <- array(NA_real_, c(nyears, nsubareas, nsim), dimnames = list(years, subareas, NULL))
    X <- predict(gs[[1]], newdata = data.frame(sstatrec), type = "lpmatrix")
    for (yr in years) {
      g <- gs[[yr]]
      fsim <- MASS::mvrnorm(nsim, coef(g), g$Vp) %*% t(X)
      sims[yr,,] <- apply(exp(fsim), 1, function(x) c(tapply(x, sstatrec$subarea, mean)))
    }

    # save simulations
    save(sims, file = paste0("species/", species[i], "/intermediate_data/", stab$Survey.name[j], "_", stab$Quarter[j],"_sims.rData"))
  }
}

