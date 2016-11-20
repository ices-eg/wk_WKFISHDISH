# -------------------------------------
#
# Fit density surface for each species and survey
#
# -------------------------------------


# load packages etc.
source("scripts/header.R")

# read in spatial datasets
load("input/spatial_model_data.rData")
statrec$fStatRec <- factor(statrec$StatRec)

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
data %<>% filter(species == "Cod", Year > 2010, Quarter == 1, Survey == "NS-IBTS")
data %<>% mutate(Model = map(Data, fit_surface))

# save data
save(data, file = "input/fitted_surfaces.rdata")

# simulate
data %<>% mutate(Sims = map(Model, sim_cpue))

# join on divisions
fulldata <- getControlTable() %>%
            as_data_frame() %>%
            rename(species = Species, Survey = Survey.name) %>%
            select(species, Division, Survey, Quarter)

data %<>% select(-Data) %>% left_join(fulldata)

# drop sims
data %<>% mutate(Sim = map2(Sims, Division, function(x, div) x[,div])) %>%
          select(-Sims, -Model)

# save data
save(data, file = "input/simulated_cpue.rdata")

