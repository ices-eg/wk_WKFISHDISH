# -------------------------------------
#
# Fit LWR by species
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")


force.fit <- FALSE

# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

for (i in seq_along(species)) {
  cat(rep(" ", 30), "\rWorking on species:", species[i], " (", i ,"/", length(species), ")"); flush.console()

  # species already analysed - skip to next
  if (file.exists(paste0("species/", species[i], "/hh_with_weight.csv")) & !force.fit) next

  hh <- read.csv(paste0("species/", species[i], "/hh.csv"))
  hl <- read.csv(paste0("species/", species[i], "/hl.csv"))
  ca <- read.csv(paste0("species/", species[i], "/ca.csv"))

  # merge
  keycols <- intersect(names(hh), names(hl))
  hl <- dplyr::left_join(hl, hh, by = keycols)
  ca <- dplyr::left_join(ca, hh, by = keycols)

  # model weight length relationship
  # ----------------------
  hl <-
    within(hl,
           {
             fYear_a <- fYear_b <- factor(Year)
             Year_a <- Year_b <- Year
           })
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
  ca <-
    within(ca,
           {
             fYear_a <- fYear_b <- factor(Year, levels = levels(hl$fYear_a))
             Year_a <- Year_b <- Year
           })


  # NOTE set k!
  year_k <- min(ceiling(length(unique(ca$Year))/2), 9)

  if (species[i] %in% c("Megrim")) {
    lwr <- gam(IndWgt ~ 1 + s(fYear_a, bs = "re") + s(Year_a, k = year_k) +
                 log(length) + s(fYear_b, by = log(length), bs = "re") +
                 s(Year_b, k = year_k, by = log(length)),
               data = ca, select = TRUE, family = Gamma(log),
               drop.unused.levels = FALSE)
    func <- identity
  } else {
    lwr <- gam(log(IndWgt) ~ 1 + s(fYear_a, bs = "re") + s(Year_a, k = year_k) +
                 log(length) + s(fYear_b, by = log(length), bs = "re") +
                 s(Year_b, k = year_k, by = log(length)),
               data = ca, select = TRUE, family = gaussian(),
               drop.unused.levels = FALSE)
    func <- exp
  }

  save(lwr, file = paste0("species/", species[i], "/lwr.rData"))
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
  plot(lwr, all = TRUE, pages = 1)

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
  hl$weight <- hl$TotalNo * func(predict(lwr, newdata = hl, type = "response"))
  hl$key <- apply(hl[keycols], 1, makekey)
  wt_tab <- tapply(hl$weight, hl$key, sum, na.rm=TRUE)
  # merge
  hh$key <- apply(hh[keycols], 1, makekey)
  hh$weight <- wt_tab[hh$key]
  hh$weight[is.na(hh$weight)] <- 0
  # convert to cpue
  hh$weight <- hh$weight / hh$HaulDur * 60

  # write out amended hh data file
  write.csv(hh, file = paste0("species/",species[i], "/hh_with_weight.csv"),
            row.names = FALSE)

}
