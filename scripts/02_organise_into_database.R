# -------------------------------------
#
# separate data into species
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")


# Create an ephemeral in-memory RSQLite database
if (!dir.exists("db")) dir.create("db")
unlink("db/datras.sqlite")
con <- dbConnect(SQLite(), dbname = "db/datras.sqlite", flags = SQLITE_RWC)

dbListTables(con)


# get hh data -----------------------
hh <-
  do.call(rbind,
          lapply(dir("datras/", pattern = "hh_"),
                 function(fname) {
                   # read data
                   hh <- read.csv(paste0("datras/", fname))

                   # drop record type column
                   hh <- hh[!names(hh) %in% "RecordType"]
                 }))
# create haul ID
makehaulID <- function(x) {
  with(x, paste(Survey, Quarter, Country, Ship, StNo, HaulNo, Year, sep=":"))
}
hh$haulID <- makehaulID(hh)

# keep only useful columns
hh <- hh[c("Survey", "Quarter", "haulID",
           "Year", "Month", "Day", "ShootLat", "ShootLong", "HaulDur", "StatRec")]


# fix bad statrecs
filt <- which(!substring(hh$StatRec, nchar(hh$StatRec)-1, nchar(hh$StatRec)-1) %in% LETTERS)
hh$StatRec[filt] <- paste0(substring(hh$StatRec[filt], 1, 2), "E", nchar(substring(hh$StatRec[filt], 3)))

# write out comined HH file
dbWriteTable(con, "hh", hh)


# read hl ----------------------------
fnames <- dir("datras/", pattern = "hl_")
for (i in seq_along(fnames)) {
  x <- read.csv(paste0("datras/", fnames[i]))

  # standardise length to mm
  x <- within(x, {length = LngtClass * ifelse(LngtCode == "1", 10, 1)})

  # create key
  x$haulID <- makehaulID(x)

  # keep only useful columns
  x <- x[names(x) %in% c("Survey", "Quarter", "haulID",
                         "Valid_Aphia", "HLNoAtLngt", "length")]

  dbWriteTable(con, "hl", x, append = i > 1, overwrite = i == 1)
}



# read ca -------------------
fnames <- dir("datras/", pattern = "ca_")
for (i in seq_along(fnames)) {
  x <- read.csv(paste0("datras/", fnames[i]))

  # standardise length to mm
  x <- within(x, {length = LngtClass * ifelse(LngtCode == "1", 10, 1)})

  # keep only non-NA weights
  x <- subset(x, !is.na(IndWgt))

  # create key
  x$haulID <- makehaulID(x)

  # keep only useful columns
  x <- x[names(x) %in% c("Survey", "Quarter", "haulID",
                         "Valid_Aphia", "IndWgt", "length")]

  dbWriteTable(con, "ca", x, append = i > 1, overwrite = i == 1)
}

dbDisconnect(con)
