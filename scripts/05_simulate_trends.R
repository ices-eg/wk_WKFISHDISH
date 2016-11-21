# -------------------------------------
#
# Fit density surface for each species and survey
#
# -------------------------------------

source("scripts/header.R")

if (!dir.exists("output")) dir.create("output")

for (selected.species in unique(getControlTable()$Species)) {
  # select a species

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
  data %<>% filter(species == selected.species, Year > 1995)
  data %<>% mutate(Model = map(Data, fit_surface))

  # simulate
  data %<>% mutate(Sims = map(Model, sim_cpue))

  # join on divisions
  fulldata <- getControlTable() %>%
    as_data_frame() %>%
    rename(species = Species, Survey = Survey.name) %>%
    select(species, Division, Survey, Quarter)

  data %<>% select(-Data) %>%
    left_join(fulldata) %>%
    filter(!is.na(Division))

  # drop sims
  data %<>% mutate(Sim = map2(Sims, Division, function(x, div) x[,div])) %>%
    select(-Sims, -Model)

  # read design table and look at species
  fulltab <- getControlTable()
  species <- unique(fulltab$Species)

  # find adjacency of areas
  adj <- spdep::poly2nb(area, queen = FALSE)
  nbs <- cbind(rep(1:length(adj), sapply(adj, length)), unlist(adj))
  nbs <- unique(t(apply(nbs, 1, sort)))
  # drop 7a-7b connection
  nbs <- nbs[!(area$SubAreaDiv[nbs[,1]] == "7.a" & area$SubAreaDiv[nbs[,2]] == "7.b"),]

  # only consider comparisons between adjacent regions
  stocks <- tibble(Division  = area$SubAreaDiv[nbs[,1]],
                   Division2 = area$SubAreaDiv[nbs[,2]]) %>%
    left_join(data) %>%
    select(species, Year, Quarter, Division, Division2, Survey, Sim) %>%
    rename(Sim1 = Sim, Division1 = Division, Survey1 = Survey) %>%
    filter(!is.na(Survey1))

  stocks %<>% rename(Division = Division2) %>%
    left_join(data) %>%
    select(species, Year, Quarter, Division1, Division, Survey1, Survey, Sim1, Sim) %>%
    rename(Sim2 = Sim, Division2 = Division, Survey2 = Survey) %>%
    filter(!is.na(Survey2))

  stocks <- stocks[!sapply(stocks$Sim1, is.null) & !sapply(stocks$Sim2, is.null),]

  # for each row calculate the log ratio and rearange by comparison
  stocks %<>% mutate(lograt = map2(Sim1, Sim2, function(x, y) tibble(id = 1:length(x), val = log(x) - log(y)))) %>%
    select(-Sim1, -Sim2) %>%
    unnest(lograt) %>%
    nest(Year, id, val)

  # now fit something to each?
  test <- function(x) {
    test_by <- function(x) {
      y <- x$val[order(x$Year)]
      out <- try(Kendall::MannKendall(y)$tau, silent = TRUE)
      if (inherits(out, "try-error")) out <- NA
      attributes(out) <- NULL
      tibble(slope = out)
    }

    x %>% group_by(id) %>%
      do(test_by(.)) %>%
      ungroup() %>%
      summarise(p = 2*min(sum(slope>0), sum(slope<0))/(length(slope)+1),
                slope = median(slope))
  }

  res <- stocks %>% mutate(test = map(data, test)) %>%
    select(-data) %>%
    unnest(test) %>%
    filter(!is.na(p))

  # correct for false discovery - do this by species?
  res$p_adj <- p.adjust(res$p, method = "BH")

  save(res, file = paste0("output/", selected.species, "_trends.rData"))
}
