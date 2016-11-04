# -------------------------------------
#
# separate data into species
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")



# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

for (i in seq_along(species)) {
  cat("                                    \rAggregating species:", species[i]); flush.console()
  ctab <- fulltab[fulltab$Species == species[i],]

  # surveys to get are:
  toget <- unique(ctab[c("Survey.name", "Quarter", "Start.year")])
  tab <- with(toget, tapply(Start.year, list(Survey.name, Quarter), min))
  toget <- expand.grid(Survey.name = gsub("[[:space:]]*$", "",rownames(tab)),
                       Quarter = as.integer(colnames(tab)),
                       stringsAsFactors = FALSE,
                       KEEP.OUT.ATTRS = FALSE)
  toget$Start.year <- c(tab)
  toget <- toget[!is.na(toget$Start.year),]
  row.names(toget) <- NULL

  # get species keys
  aphia <- findAphia(paste0("^", species[i], "$"), regex = TRUE)
  if (species[i] == "Anglerfish") {
    # take black and white bellied? - if so comment out these lines
    # the next line resticts us to white bellied only
    aphia <- findAphia("white anglerfish", regex = TRUE)
  }

  # extract single species data for each survey
  hh <-
    do.call(rbind,
            lapply(1:nrow(toget),
                   function(j) {
                     # get survey file names
                     fname <- dir("datras")[grep(paste0("hh_", toget$Survey.name[j], "_[[:digit:]]+-[[:digit:]]+_", toget$Quarter[j], ".csv"), dir("datras"))]

                     # read data
                     hh <- read.csv(paste0("datras/", fname))

                     # drop record type column
                     hh <- hh[!names(hh) %in% "RecordType"]

                     # subset for year
                     subset(hh, Year > toget$Start.year[j])
                   }))

  hl <-
    do.call(rbind,
            lapply(1:nrow(toget),
                   function(j) {
                     # get survey file names
                     fname <- dir("datras")[grep(paste0("hl_", toget$Survey.name[j], "_[[:digit:]]+-[[:digit:]]+_", toget$Quarter[j], ".csv"), dir("datras"))]

                     # read data
                     hl <- read.csv(paste0("datras/", fname))

                     # drop record type column
                     hl <- hl[!names(hl) %in% "RecordType"]

                     # subset for species
                     hl <- hl[hl$Valid_Aphia %in% aphia,]

                     if (length(unique(hl$Valid_Aphia)) > 1) {
                       warning("more than one species selected!!")
                     }

                     # subset for year
                     subset(hl, Year > toget$Start.year[j])
                   }))

  ca <-
    do.call(rbind,
            lapply(1:nrow(toget),
                   function(j) {
                     # get survey file names
                     fname <- dir("datras")[grep(paste0("ca_", toget$Survey.name[j], "_[[:digit:]]+-[[:digit:]]+_", toget$Quarter[j], ".csv"), dir("datras"))]

                     # read data
                     ca <- read.csv(paste0("datras/", fname))

                     # drop record type column
                     ca <- ca[!names(ca) %in% "RecordType"]

                     # subset for species
                     ca <- ca[ca$Valid_Aphia %in% aphia,]

                     if (length(unique(ca$Valid_Aphia)) > 1) {
                       warning("more than one species selected!!")
                     }

                     # subset for year
                     ca <- subset(ca, Year > toget$Start.year[j])

                     ca
                   }))

  # keep only non-NA weights
  ca <- subset(ca, !is.na(IndWgt))

  # standardise length to mm
  ca <- within(ca, {length = LngtClass * ifelse(LngtCode == "1", 10, 1)})
  hl <- within(hl, {length = LngtClass * ifelse(LngtCode == "1", 10, 1)})

  # save to megrim folder
  if (!dir.exists(paste0("species/",species[i]))) {
    dir.create(paste0("species/",species[i]))
  }

  # write out data files
  write.csv(hh, file = paste0("species/",species[i], "/hh.csv"),
            row.names = FALSE)
  write.csv(hl, file = paste0("species/",species[i], "/hl.csv"),
            row.names = FALSE)
  write.csv(ca, file = paste0("species/",species[i], "/ca.csv"),
            row.names = FALSE)

  # done
}

