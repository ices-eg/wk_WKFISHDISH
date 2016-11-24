# -------------------------------------
#
# Fit density surface for each species and survey
#
# -------------------------------------

source("scripts/header.R")

if (!dir.exists("output")) dir.create("output")
force_resim <- FALSE

# read in spatial datasets
load("input/spatial_model_data.rData")

for (selected.species in unique(getControlTable()$Species)) {
  # select a species

  sim_fname <- paste0("output/", selected.species, "_sims.rData")
  if (!file.exists(sim_fname) | force_resim) {

    # Get HH and cpue data:
    con <- dbConnect(SQLite(), dbname = "db/datras.sqlite")
    hh <- dbReadTable(con, "hh")
    data <- dbReadTable(con, "cpue")
    dbDisconnect(con)

    # join hh onto cpue data
    data %<>% as_data_frame() %>%
      left_join(hh)

    # create data.frame for model fits
    data <- nest(data, -species, -Survey, -Quarter, -Year, .key = Data)

    # for each row fit a surface
    data %<>% filter(species == selected.species, Year > 1995)
    data %<>% mutate(Model = map(Data, fit_surface))

    # simulate
    data %<>% mutate(Sims = map(Model, sim_cpue))

    save(data, file = sim_fname)
  }
}
