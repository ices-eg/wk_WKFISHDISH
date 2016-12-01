
# utilities

download.ICESshape <- function(what) {
  if (!dir.exists("zips")) dir.create("zips")

  download.file(paste0("http://gis.ices.dk/shapefiles/", what, ".zip"),
                paste0("zips/", what, ".zip"))

  if (!dir.exists("shapefiles")) dir.create("shapefiles")
  unzip(paste0("zips/", what, ".zip"), exdir = "shapefiles")
}


datras.fname <- function(what, survey, start.year, end.year, quarter) {
  paste0("datras/", what, "_", survey, "_", start.year, "-", end.year, "_", quarter, ".csv")
}


download.Datras <- function(survey, start.year, end.year = 2015, quarter) {
  if (!dir.exists("datras")) dir.create("datras")

  fname <- function(what) {
    datras.fname(what, survey, start.year, end.year, quarter)
  }

  hh <- icesDatras::getDATRAS(record = "HH", survey = survey, year = start.year:end.year, quarter = quarter)
  write.csv(hh, file = fname("hh"), row.names = FALSE)
  rm(hh)
  gc()

  hl <- icesDatras::getDATRAS(record = "HL", survey = survey, year = start.year:end.year, quarter = quarter)
  write.csv(hl, file = fname("hl"), row.names = FALSE)
  rm(hl)
  gc()


  ca <- icesDatras::getDATRAS(record = "CA", survey = survey, year = start.year:end.year, quarter = quarter)
  write.csv(ca, file = fname("ca"), row.names = FALSE)
  rm(ca)
  gc()

}


makekey <- function(x) {
  paste(trimws(paste(x)), collapse = "_")
}

base <- function() {
  plot(area, col = colorRampPalette(c("lightblue", "lightgreen"))(3))
  plot(statrec, add = TRUE)
  plot(samparea, lwd = 2, border = "red", add = TRUE)
}



getControlTable <- function() {
  tab <- read.table("input/Overview of surveys to be used.txt", sep = ",", header = TRUE, stringsAsFactors = FALSE)

  # expand quarter and start year
  qtr <- strsplit(tab$Quarter, ",")
  syr <- strsplit(tab$Start.year, ",")
  orig.row <- rep(1:nrow(tab), sapply(qtr, length))

  tab <- cbind(tab[orig.row,c("Species", "Division", "Survey.name", "Gear")],
               data.frame(Quarter = as.integer(unlist(qtr)),
                          Start.year = as.integer(unlist(syr))) )

  # expand division
  div <- strsplit(tab $ Division, ",")
  orig.row <- rep(1:nrow(tab), sapply(div, length))

  tab <- cbind(tab[orig.row,c("Species", "Survey.name", "Gear", "Quarter", "Start.year")],
               data.frame(Division = unlist(div)) )

  # expand species
  sp <- strsplit(tab $ Species, ",")
  sp <- lapply(sp, function(x) trimws(gsub("\n|(and)", "", x)))
  orig.row <- rep(1:nrow(tab), sapply(sp, length))

  tab <- cbind(data.frame(Species = unlist(sp)),
               tab[orig.row,c("Division", "Survey.name", "Gear", "Quarter", "Start.year")])

  tab$Survey.name <- trimws(tab$Survey.name)
  tab$Division <- trimws(tab$Division)

  row.names(tab) <- NULL
  tab
}



# LWR fitting utilities --------------------------------------------------


fit_lwr <- function(aphia, species) {
  con <- dbConnect(SQLite(), dbname = "db/datras.sqlite")

  # Get CA data:
  sqlstring <-
    paste0("SELECT * \n",
           "FROM ca\n",
           "WHERE Valid_Aphia IN (", paste(aphia, collapse = ", "),")\n")

  res <- dbSendQuery(con, sqlstring)
  ca <- dbFetch(res, n = -1)
  dbClearResult(res)

  dbDisconnect(con)

  # merge
  ca <- dplyr::left_join(ca, hh, by = c("Survey", "Quarter", "haulID"))

  # drop ca data that has no link to hh data
  if (any(is.na(ca$ShootLong))) warning(sum(is.na(ca$ShootLong)), " ca data are not linked to HH")
  ca <- subset(ca, !is.na(ShootLong))


  # remove wrong data
  # ----------------------

  if (species == "Anchovy") {
    ca <- subset(ca, Year != 2015)
  }
  if (species == "Cod") {
    ca$IndWgt[ca$IndWgt == 0] <- min(ca$IndWgt[ca$IndWgt > 0])
  }
  if (species == "Sprat") {
    ca <- subset(ca, !(Year == 2002 & length > 250) &
                   !(Year == 2005 & IndWgt > 43) &
                   !(Year == 2006) &
                   !(Year == 2007 & IndWgt > 50))
    ca$IndWgt[ca$IndWgt == 0] <- min(ca$IndWgt[ca$IndWgt > 0])
  }
  if (species == "Megrim") {
    ca <- subset(ca, !(Year == 2015 & length > 300 & IndWgt < 50))
  }

  # model weight length relationship
  # ----------------------
  # add covariates
  ca <- within(ca, {
    fYear_a <- fYear_b <- factor(Year, levels = sort(unique(hh$Year)))
    Year_a <- Year_b <- Year
    date = lubridate::ymd(paste(Year, Month, Day))
    yday = yday(date)
  })


  # NOTE set k!
  year_k <- min(ceiling(length(unique(ca$Year))/2), 9)

  gam(log(IndWgt) ~ 1 + s(fYear_a, bs = "re") + s(Year_a, k = year_k) +
        log(length) + s(fYear_b, by = log(length), bs = "re") +
        s(Year_b, k = year_k, by = log(length)),
      data = ca, select = TRUE, family = gaussian(),
      drop.unused.levels = FALSE)
}


predict_cpue <- function(model, aphia) {

  # Get HL data:
  con <- dbConnect(SQLite(), dbname = "db/datras.sqlite")
  sqlstring <-
    paste0("SELECT * \n",
           "FROM hl\n",
           "WHERE Valid_Aphia IN (", paste(aphia, collapse = ", "),")\n")

  res <- dbSendQuery(con, sqlstring)
  hl <- dbFetch(res,n=-1)
  dbClearResult(res)
  dbDisconnect(con)

  # merge
  hl <- dplyr::left_join(hl, hh, by = c("Survey", "Quarter", "haulID"))

  # add covariates
  hl <- within(hl, {
    fYear_a <- fYear_b <- factor(Year)
    Year_a <- Year_b <- Year
    date <- lubridate::ymd(paste(Year, Month, Day))
    yday <- lubridate::yday(date)
  })

  # calculate total weight by haul and convert to CPUE
  # then merge onto hh
  hl$weight <- hl$HLNoAtLngt * exp(predict(model, newdata = hl, type = "response"))
  wt_tab <- tapply(hl$weight, hl$haulID, sum, na.rm=TRUE)
  # merge
  hh$weight <- wt_tab[hh$haulID]
  hh$weight[is.na(hh$weight)] <- 0
  # convert to cpue
  hh$weight <- hh$weight / hh$HaulDur * 60

  # write out cpue
  hh[c("weight", "haulID")]
}


# density surface -------------------------------------------------------

fit_surface <- function(df) {
  # do some modelling
  df$fStatRec <- factor(df$StatRec, levels = statrec$StatRec)

  # HACK!!
  df <- subset(df, is.finite(df$weight))

  # substitute zero with half minumum observed catch weight
  df$adj_weight <- df$weight
  if (all(df$weight == 0)) {
    # no observations
    return(NULL)
  } else
    if (any(df$weight == 0)) {
      min_weight <- min(df$weight[df$weight>0], na.rm = TRUE)
      df$adj_weight[df$adj_weight == 0] <- min_weight/2
    }

  # check if there is enough data?
  # or wrap in a try to get it going:
  # set the smoothing to be related to the number of non-zero statsquares observed
  k <- max(3, min(20, floor(length(unique(df$StatRec[df$weight>0])) / 5)))
  out <-
    try(
      gam(log(adj_weight) ~ s(fStatRec, bs = "mrf", xt = list(penalty = Q), k = k),
          data = df,
          drop.unused.levels = FALSE),
      silent = TRUE)

  if (inherits(out, "try-error")) {
    NULL
  } else {
    out
  }
}


# simulate by regions -------------------

sim_cpue <- function(mod, nsim = 1000) {
  if (is.null(mod)) {
    return(NULL)
  }
  X <- predict(mod, newdata = statrec, type = "lpmatrix")
  bsim <- MASS::mvrnorm(nsim, coef(mod), mod$Vp)
  sim <- exp(X %*% t(bsim))
  t(apply(sim, 2, function(x) tapply(x, statrec$SubAreaDiv, mean)))
}



# survey data plotting functions -----------------------------

# plot front plot
front_plot <- function(sstatrec, survey, quarter) {
  plot(area, xlim = bbox(sstatrec)["x",], ylim = bbox(sstatrec)["y",], col = gplots::rich.colors(nrow(area), alpha = 0.2), border = grey(0.2, 0.5))
  plot(statrec, add = TRUE, border = grey(0.5, 0.5))
  plot(sstatrec, add = TRUE, col = grey(0.2, alpha = 0.5))
  mtext(paste0(survey, " Q", quarter), font = 2)
}

plot_one_survey_data <- function(df) {

  sstatrec <- df$statrec[[1]]

  # set par - starts a new page
  par(mfrow = c(2,2), mar = c(0,0,1.5,0))

  front_plot(sstatrec, df$Survey, df$Quarter)

  # colours
  cols <- colorRampPalette(c("cyan", "magenta"))(50)

  # data
  obs <- df %>%
    unnest(Data) %>%
    unnest(Data) %>%
    by(.$Year,
       function(x)
         with(x, tapply(weight,
                        factor(StatRec, levels = sstatrec$StatRec),
                        mean,
                        na.rm = TRUE))) %>%
    unclass() %>% simplify2array()

  for (k in colnames(obs)) {
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

    plot(statrec, xlim = bbox(sstatrec)["x",], ylim = bbox(sstatrec)["y",])
    plot(sstatrec, col = tmp, add = TRUE)
    mtext(paste0(species[i], " ", df$Survey, " Q", df$Quarter, " ", k), font = 2, line = -0.25)
  }
}

