# -------------------------------------
#
# Fit density surface for each species and survey
#
# -------------------------------------

if (FALSE) {
  #' to build run this
  library(knitr)
  stitch("scripts/05_simulate_trends.R")
}

# load packages etc.
source("scripts/header.R")

# select a species
for (selected.species in unique(getControlTable()$Species)) {

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
  data %<>% filter(species == selected.species, Year > 2000, Quarter == 1)
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
  #cbind(area$SubAreaDiv[nbs[,1]], area$SubAreaDiv[nbs[,2]])

  ##xy <- coordinates(area)
  # relocate center of 3.d for plotting
  #xy["3.d",] <- unlist(locator(n=1))
  #xy["3.d",] <- c(16.26910, 54.85013)
  #plot(area)
  #segments(xy[nbs[,1],1], xy[nbs[,1],2],
  #         xy[nbs[,2],1], xy[nbs[,2],2],
  #         col = "blue", lwd = 2)
  #points(xy, pch = 16, col = "red")

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
      out <- Kendall::MannKendall(y)$tau
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
    unnest(test)

  # correct for false discovery - do this by species?
  res$p_adj <- p.adjust(res$p, method = "BH")

  res %>% filter(p_adj < 0.05) %>% select(-p)

  # plot result

  xy <- coordinates(area)
  # relocate center of 3.d for plotting
  #xy["3.d",] <- unlist(locator(n=1))
  xy["3.d",] <- c(16.26910, 54.85013)

  # shorten arrow
  x0 = xy[ifelse(res$slope > 0, res$Division1, res$Division2),1]
  y0 = xy[ifelse(res$slope > 0, res$Division1, res$Division2),2]
  x1 = xy[ifelse(res$slope > 0, res$Division2, res$Division1),1]
  y1 = xy[ifelse(res$slope > 0, res$Division2, res$Division1),2]

  plot(area, border = grey(0.5, alpha = 0.5), main = selected.species)
  segments(x0, y0, x1, y1,
           col = "blue", lwd = 1)

  arrows(x0, y0, x0+(x1-x0)*.45, y0 + (y1-y0)*.45,
         col = "blue", lwd = 2,
         code = 2, length = 0.1)
  points(xy, pch = 16, col = "red", cex = 0.8)

}
