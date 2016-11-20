# -------------------------------------
#
# Fit LWR by species
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")
library(purrr)
library(tidyr)
library(magrittr)
library(dplyr)


force.fit <- FALSE

# read design table and look at species
fulltab <- getControlTable()

# get aphia code function
getAphia <- function(sp) {
  aphia <- findAphia(sp)
  if (sp == "Anglerfish") {
    # take black and white bellied? - if so comment out these lines
    # the next line resticts us to white bellied only
    aphia <- findAphia("white anglerfish")
  }
  aphia
}


# create data.frame
data <- data.frame(species = unique(fulltab$Species))
data %<>% mutate(Aphia = map(species, getAphia))

aphia <- data$Aphia[[1]]


for (i in seq_along(species)) {
  cat("\rWorking on species:", species[i], " (", i ,"/", length(species), ")", rep(" ", 50)); flush.console()

  con <- dbConnect(RSQLite::SQLite(), dbname = "db/datras.sqlite")

  # Get HL data:
  sqlstring <-
    paste0("SELECT * \n",
           "FROM hl LEFT JOIN hh\n",
           "WHERE Valid_Aphia IN (", paste(aphia, collapse = ", "),")\n")
  cat(sqlstring)

  res <- dbSendQuery(con, sqlstring)
  hl <- dbFetch(res,n=5)
  dbClearResult(res)

  # Get CA data:
  sqlstring <-
    paste0("SELECT * \n",
           "FROM ca LEFT JOIN hh\n",
           "WHERE Valid_Aphia IN (", paste(aphia, collapse = ", "),")\n")
  cat(sqlstring)

  res <- dbSendQuery(con, sqlstring)
  hl <- dbFetch(res,n=5)
  dbClearResult(res)

  dbGetRowCount(res)

  # Fetch in chunks
  i <- 0
  out <- list()
  while (!dbHasCompleted(res)) {
    i <- i + 1
    out[[i]] <- dbFetch(res, 100)
  }
  dbGetRowCount(res)


  # merge
  hl <- dplyr::left_join(hl, hh, by = c("Survey", "Quarter", "haulID"))
  ca <- dplyr::left_join(ca, hh, by = c("Survey", "Quarter", "haulID"))

  # drop ca data that has no link to hh data
  if (any(is.na(ca$ShootLong))) warning(sum(is.na(ca$ShootLong)), " ca data are not linked to HH")
  ca <- subset(ca, !is.na(ShootLong))


  # remove wrong data
  # ----------------------

  if (species[i] == "Anchovy") {
    ca <- subset(ca, Year != 2015)
  }
  if (species[i] == "Cod") {
    ca$IndWgt[ca$IndWgt == 0] <- min(ca$IndWgt[ca$IndWgt > 0])
  }
  if (species[i] == "Sprat") {
    ca <- subset(ca, !(Year == 2002 & length > 250) &
                   !(Year == 2005 & IndWgt > 43) &
                   !(Year == 2006) &
                   !(Year == 2007 & IndWgt > 50))
    ca$IndWgt[ca$IndWgt == 0] <- min(ca$IndWgt[ca$IndWgt > 0])
  }
  if (species[i] == "Megrim") {
    ca <- subset(ca, !(Year == 2015 & length > 300 & IndWgt < 50))
  }

  # model weight length relationship
  # ----------------------
  # add covariates
  hl <- within(hl, {
    fYear_a <- fYear_b <- factor(Year)
    Year_a <- Year_b <- Year
    date = lubridate::ymd(paste(Year, Month, Day))
    yday = yday(date)
  })
  ca <- within(ca, {
    fYear_a <- fYear_b <- factor(Year, levels = levels(hl$fYear_a))
    Year_a <- Year_b <- Year
    date = lubridate::ymd(paste(Year, Month, Day))
    yday = yday(date)
  })


  # NOTE set k!
  year_k <- min(ceiling(length(unique(ca$Year))/2), 9)

  lwr <- gam(log(IndWgt) ~ 1 + s(fYear_a, bs = "re") + s(Year_a, k = year_k) +
               log(length) + s(fYear_b, by = log(length), bs = "re") +
               s(Year_b, k = year_k, by = log(length)),
             data = ca, select = TRUE, family = gaussian(),
             drop.unused.levels = FALSE)
  func <- exp

  save(lwr, func, file = paste0("species/", species[i], "/lwr.rData"))
  chk1 <- capture.output(summary(lwr))

  # plot to check
  wk <- expand.grid(length = seq(1, max(ca$length, na.rm = TRUE), length = 100), Year = unique(ca$Year))
  wk <-
    within(wk,
           {
             fYear_a <- fYear_b <- factor(Year, levels = levels(hl$fYear_a))
             Year_a <- Year_b <- Year
           })

  # plot model fit and diagnostics
  pdf(paste0("species/", species[i], "/lwr_plots.pdf"), onefile = TRUE)
  plot.gam(lwr, all = TRUE, pages = 1, scale = 0, shade = TRUE)

  op <- par(mfrow = c(2,2), no.readonly = TRUE)
  chk2 <- capture.output(gam.check(lwr,pch=19,cex=.3))
  par(op)

  wk$fit <- func(predict(lwr, newdata = wk, type = "response"))
  p <-
    xyplot(IndWgt ~ length | fYear_a, data = ca) +
    xyplot(fit ~ length | fYear_a, data = wk, type = "l")

  print(p)

  dev.off()

  # write out model diagnostics
  cat(paste(chk1, collapse="\n"), paste(chk2, collapse="\n"),
      file = paste0("species/", species[i], "/lwr_check.txt"),
      sep = "\n\n----------------------------------\n")



  # calculate total weight by haul and convert to CPUE
  # then merge onto hh
  hl$weight <- hl$HLNoAtLngt * func(predict(lwr, newdata = hl, type = "response"))
  wt_tab <- tapply(hl$weight, hl$haulID, sum, na.rm=TRUE)
  # merge
  hh$weight <- wt_tab[hh$haulID]
  hh$weight[is.na(hh$weight)] <- 0
  # convert to cpue
  hh$weight <- hh$weight / hh$HaulDur * 60

  # write out amended hh data file
  write.csv(hh["weight"], file = paste0("species/",species[i], "/hh_weight.csv"),
            row.names = FALSE)

}

# join together hh_weights

hh <- read.csv("input/hh.csv")
for (i in seq_along(species)) {
  x <- read.csv(paste0("species/",species[i], "/hh_weight.csv"))
  hh[gsub(" ", "_", species[i])] <- x
}

write.csv(hh, file = paste0("input/hh_with_weight.csv"), row.names = FALSE)
