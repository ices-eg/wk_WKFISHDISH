
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

  row.names(tab) <- NULL
  tab
}


