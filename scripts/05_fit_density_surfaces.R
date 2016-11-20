# -------------------------------------
#
# Fit density surface for each species
#
# -------------------------------------


# load packages etc.
source("scripts/header.R")
source("scripts/fit_surface_utils.R")

# read in spatial datasets
load("input/spatial_model_data.rData")
statrec$fStatRec <- factor(statrec$StatRec)

# Get HH data:
con <- dbConnect(SQLite(), dbname = "db/datras.sqlite")
hh <- dbReadTable(con, "hh")
dbDisconnect(con)

# join onto cpue data
load("input/cpue_lwr.rdata")
data %<>% unnest(cpue)
data %<>% left_join(hh)

# create data.frame for model fits
data <- nest(data, -species, -Survey, -Quarter, -Year, .key = Data)

# for each row fit a surface
data %<>% mutate(Model = map(Data, fit_surface))

# save data
save(data, file = "input/fitted_surfaces.rdata")
































# subset to > 2001
dat <- dat[dat$Year > 2001,]

# modelling setup
years <- unique(dat$Year)
nyears <- length(years)
dat$fStatRec <- factor(dat$StatRec, levels = statrec$StatRec)
statrec$fStatRec <- factor(statrec$StatRec)
dat$fYear <- factor(dat$Year)
dat$SurveyQuart <- factor(paste(dat$Survey, dat$Quarter, sep=":"))

# substitute zero with half minumum observed catch weight
adj_dat <- data.frame(dat)
adj_dat[gsub(" ", "_", species)] <-
  lapply(adj_dat[gsub(" ", "_", species)],
       function(x) {
         min_x <- min(x[x>0], na.rm = TRUE)
         adj_x <- x
         adj_x[adj_x == 0] <- min_x/2
         adj_x
       })

form <- y ~ s(fStatRec, bs = "mrf", xt = list(penalty = Q), k = 40, by = fYear) +
            fYear

i <- 7
for (i in seq_along(species)) {
  cat("\rWorking on species:", species[i], " (", i ,"/", length(species), ")", rep(" ", 50)); flush.console()
  # do some modelling
  adj_dat$y <- log(adj_dat[[gsub(" ", "_", species[i])]])

  g <- gam(form,
           data = subset(adj_dat, SurveyQuart == levels(dat$SurveyQuart)[8]),
           drop.unused.levels = FALSE)

  fits <-
    sapply(sort(years), function(yr) {
      X <- predict(g,
                   newdata = cbind(data.frame(statrec), Year = yr, fYear = factor(yr), SurveyQuart = "NS-IBTS:1"),
                   type = "lpmatrix")
      b <- coef(g)
      b[grep("SurveyQuart", names(b))] <- 0
      fit <- drop(X%*%b)
      c(tapply(exp(fit), statrec$SubAreaDiv, mean))
    })
  fits <- t(fits/apply(fits, 1, mean))

  matplot(sort(years), fits[,grep("4", colnames(fits))], type = "l")
  matplot(sort(years), fits, type = "l")

}

