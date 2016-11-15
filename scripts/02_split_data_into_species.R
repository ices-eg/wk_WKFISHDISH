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

# get hh data
hh <-
  do.call(rbind,
          lapply(dir("datras/", pattern = "hh_"),
                 function(fname) {
                   # read data
                   hh <- read.csv(paste0("datras/", fname))

                   # drop record type column
                   hh <- hh[!names(hh) %in% "RecordType"]
                 }))
# write out comined HH file
write.csv(hh, file = paste0("input/hh.csv"), row.names = FALSE)

# split files by species
aphia <-
  lapply(species, function(sp) {
    aphia <- findAphia(sp)
    if (sp == "Anglerfish") {
      # take black and white bellied? - if so comment out these lines
      # the next line resticts us to white bellied only
      aphia <- findAphia("white anglerfish")
    }
    aphia
  })

# create species folders
create <- !dir.exists(paste0("species/",species))
sapply(paste0("species/",species)[create], dir.create)


# read hl
fnames <- dir("datras/", pattern = "hl_")
for (i in seq_along(fnames)) {
  x <- read.csv(paste0("datras/", fnames[i]))

  # drop record type column
  x <- x[!names(x) %in% "RecordType"]

  # keep only non-NA weights
  x <- subset(x, !is.na(IndWgt))

  # standardise length to mm
  x <- within(x, {length = LngtClass * ifelse(LngtCode == "1", 10, 1)})

  for (j in seq_along(aphia)) {
    # subset for species
    sub_x <- x[x$Valid_Aphia %in% aphia[[j]],]

    # append to file
    write.table(sub_x,
                file = paste0("species/",species[j], "/hl.csv"),
                row.names = FALSE,
                sep = ",",
                col.names = (i == 1),
                append = (i != 1))
  }
}



# read ca
fnames <- dir("datras/", pattern = "ca_")
for (i in seq_along(fnames)) {
  x <- read.csv(paste0("datras/", fnames[i]))

  # drop record type column
  x <- x[!names(x) %in% "RecordType"]

  # keep only non-NA weights
  x <- subset(x, !is.na(IndWgt))

  # standardise length to mm
  x <- within(x, {length = LngtClass * ifelse(LngtCode == "1", 10, 1)})

  for (j in seq_along(aphia)) {
    # subset for species
    sub_x <- x[x$Valid_Aphia %in% aphia[[j]],]

    # append to file
    write.table(sub_x,
                file = paste0("species/",species[j], "/ca.csv"),
                row.names = FALSE,
                sep = ",",
                col.names = (i == 1),
                append = (i != 1))
  }
}

