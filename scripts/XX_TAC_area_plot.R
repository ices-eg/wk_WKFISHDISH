# -------------------------------------
#
# separate data into species
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")

# read design table and look at species
fulltab <- getControlTable()
tspecies <- unique(fulltab$Species)
tspecies <- trimws(gsub("Black-bellied|White", "", tspecies))
tspecies <- trimws(gsub("Megrim", "Megrims", tspecies))

# load TAC areas
TACarea <- read.csv("input/TACareas.csv")

nonTAC <- setdiff(tolower(species), tolower(unique(TACarea$species)))

# proceed
TACarea <- subset(TACarea, tolower(species) %in% tolower(tspecies))
# drop Skagerrak.. Kategat
TACarea <- subset(TACarea, !def %in% c("Skagerrak", "Kattegat"))
TACarea <- unique(TACarea[c("species", "unit", "def")])

# load
area <- readOGR("shapefiles", "ICES_Areas_20160601_dense")
# hard code projection in case GDAL is not installed
proj4string(area) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

statrec <- readOGR("shapefiles", "ICES_StatRec_mapto_ICES_Areas")
# hard code projection in case GDAL is not installed
proj4string(statrec) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +no_defs")
statrec <- spTransform(statrec, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # transform to wgs84
statrec$SubAreaDiv <- sapply(strsplit(statrec$Max_Area, "[.]"), function(x) paste(x[1:2], collapse = "."))

statrec@data <- cbind(statrec@data, over(statrec, area))

# assign statsq to each row of TACarea
find_statsq <- function(def) {
 out <- strsplit(def, "[.]")[[1]]
 out <- as.data.frame(lapply(out, identity))
 if (as.numeric(out[1]) > 19) {
   names(out)[1] <- c("SubDivisio")
   out <- out[1]
 } else {
   names(out) <- c("SubArea", "Division", "SubDivisio")[1:ncol(out)]
 }
 ID <- left_join(out, as.data.frame(statrec))$OBJECTID
 if (length(ID) == 0) return(NULL)
 out <- statrec[statrec$OBJECTID %in% ID,]
 out
}

TACarea %<>% as.tbl %>% mutate(statrec = map(def, find_statsq))
TACarea <- subset(TACarea, !sapply(TACarea, is.null))

# load spatial extent of analysis
load("input/spatial_model_data.rData")

pdf("figures/TAC_area_by_species.pdf", onefile = TRUE, paper = "a4")

for (sp in unique(TACarea$species)) {
  # plot by species

  df <- subset(TACarea, species == sp)

  atac <- map(df$statrec, function(x) if(nrow(x)==0) NULL else gUnaryUnion(x))
  atac <- atac[!sapply(atac, is.null)]
  atac <- do.call(bind, atac)

  plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], main = paste(sp, "TAC areas"),
       border = grey(0.5, alpha = 0.5))
  plot(atac, col = gplots::rich.colors(length(atac), alpha = 0.5), add = TRUE, border = "darkblue", lwd = 2)


}

dev.off()

