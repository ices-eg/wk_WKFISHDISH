# -------------------------------------
#
# Do some plots of data and model fits
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")

# read in spatial datasets
load("input/spatial_model_data.rData")

# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

if (!dir.exists("figures")) dir.create("figures")

# Get HH and cpue data:
con <- dbConnect(SQLite(), dbname = "db/datras.sqlite")
hh <- dbReadTable(con, "hh")
data <- dbReadTable(con, "cpue")
dbDisconnect(con)

# join hh onto cpue data
data %<>% as_data_frame() %>%
  left_join(hh)

# subset by year
data %<>% filter(Year > 1995)

# create data.frame for plots
data <- nest(data, -species, -Survey, -Quarter, -Year, .key = Data)
data <- nest(data, -species, -Survey, -Quarter, .key = Data)

# get extent of each survey
statrec_extent <- function(df) {
  sr_extent <- df %>% unnest(Data) %>% select(StatRec) %>% unlist() %>% unique()
  statrec[statrec$StatRec %in% sr_extent,]
}

data %<>% mutate(statrec = map(Data, statrec_extent))

pdf(paste0("figures/survey_coverage.pdf"), onefile = TRUE, paper = "a4")
# set par - starts a new page
par(mfrow = c(2,2), mar = c(0.2,0.2,1.5,0.2))
data %>% filter(species == "Megrim") %>% mutate(tmp = pmap(list(statrec, Survey, Quarter), front_plot))
dev.off()


for (i in seq_along(species)) {
  cat("Working on species:", species[i], " (", i ,"/", length(species), ")\n"); flush.console()
  sp <- species[i]
  sdata <- data %>% filter(species == sp)

  pdf(paste0("figures/", species[i], "_survey_data.pdf"), onefile = TRUE, paper = "a4")
    for (j in 1:nrow(sdata)) {
      plot_one_survey_data(sdata[j,])
    }
  dev.off()
}




if (FALSE) {
  ## DO NOT RUN
  # move plots to sharepoint
  from <- paste0("figures/", species, "_survey_data.pdf")
  todir <- paste0("C:/Users/colin/SharePoint/WKFISHDISH - 2016 Meeting docs/04. Working documents/spatial_model_data")
  to <- paste0(todir, "/", species, "_survey_data.pdf")
  create <- !dir.exists(todir)
  tmp <- lapply(which(create), function(i) dir.create(todir[i]))
  file.copy(from, to, overwrite = TRUE)
}

