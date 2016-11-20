
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
