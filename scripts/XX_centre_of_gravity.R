# -------------------------------------
#
# Fit density surface for each species and survey
#
# -------------------------------------

source("scripts/header.R")


# read in spatial datasets
load("input/spatial_model_data.rData")


for (selected.species in unique(getControlTable()$Species)) {
  # select a species

  sim_fname <- paste0("output/", selected.species, "_sims.rData")
  load(sim_fname)

  # drop null models
  data <- data[!sapply(data$Model, is.null),]

  # nest year
  data <- data %>% select(-Sims) %>% nest(Year, Model, Data)

  # estimate extent for each
  statrec_extent <- function(df) {
    sr_extent <- df$Data %>% sapply(function(x) unique(x$StatRec)) %>% unlist() %>% unique()
    statrec[statrec$StatRec %in% sr_extent,]
  }
  data %<>% mutate(statrec = map(data, statrec_extent))

  #
  df <- data[1,]
  predict_cg <- function(data, statrec) {
    data %>% mutate(cg = map(Model, predict_cg_by, statrec = statrec),
                    cg_ci = map(Model, predict_cg_ci_by, statrec = statrec))
  }
  predict_cg_by <- function(mod, statrec) {
    pred <- exp(predict(mod, newdata = statrec))
    xy <- colSums(coordinates(statrec) * cbind(pred, pred)) / sum(pred)
    tibble(y = xy[1], x = xy[2])
  }
  predict_cg_ci_by <- function(mod, statrec) {
    X <- predict(mod, newdata = statrec, type = "lpmatrix")
    bsim <- MASS::mvrnorm(n = 1000, coef(mod), mod$Vp)
    predsim <- exp(X %*% t(bsim))
    cgsim <- apply(predsim, 2, function(x) colSums(coordinates(statrec) * cbind(x, x)) / sum(x))
    tibble(y.cil = quantile(cgsim[1,], .025),
           y.ciu = quantile(cgsim[1,], .975),
           x.cil = quantile(cgsim[2,], .025),
           x.ciu = quantile(cgsim[2,], .975))
  }

  data %<>% mutate(data = map2(data, statrec, predict_cg))

  save(data, file = paste0("output/", selected.species, "_centre_gravity.rData"))
}
