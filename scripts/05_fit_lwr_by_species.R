# -------------------------------------
#
# Fit LWR by species
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")

# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

# get aphia code function
getAphia <- function(sp) {
  aphia <- findAphia(sp)
  if (sp == "Megrim") {
    # take Megrim to mean Lepidorhombus whiffiagonis, Four-spot megrim is specified seperately
    aphia <- findAphia("Lepidorhombus whiffiagonis", latin = TRUE)
  }
  aphia
}


# Get HH data:
con <- dbConnect(SQLite(), dbname = "db/datras.sqlite")
hh <- dbReadTable(con,"hh")
dbDisconnect(con)

# create data.frame for results
data <- tibble(species = unique(fulltab$Species))
data %<>% mutate(Aphia = map_int(species, getAphia))

# check there is enough data
data %>% mutate(nrow = map_int(Aphia, function(aphia) {
sqlstring <-
  paste0("SELECT * \n",
         "FROM ca\n",
         "WHERE Valid_Aphia IN (", paste(aphia, collapse = ", "),")\n")

con <- dbConnect(SQLite(), dbname = "db/datras.sqlite")
res <- dbSendQuery(con, sqlstring)
ca <- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)
nrow(ca)
}))


# fit models
data %<>% mutate(Model = map2(Aphia, species, fit_lwr),
                 cpue = map2(Model, Aphia, predict_cpue))

# some checks
#map(data$Model, summary)
#map(data$Model, function(x) capture.output(gam.check(x)))

# join together hh_weights
cpue <- unnest(data, cpue)

con <- dbConnect(RSQLite::SQLite(), dbname = "db/datras.sqlite")
dbWriteTable(con, "cpue", cpue, append = FALSE, overwrite = TRUE)
dbDisconnect(con)

# save data
save(data, file = "input/cpue_lwr.rdata")

# DONE -------------
























if (FALSE) {
  # incorporate this into a function
# plot to check
wk <- expand.grid(length = seq(1, max(ca$length, na.rm = TRUE), length = 100), Year = unique(ca$Year))
wk <-
  within(wk,
         {
           fYear_a <- fYear_b <- factor(Year, levels = levels(hl$fYear_a))
           Year_a <- Year_b <- Year
         })

# plot model fit and diagnostics
plot.gam(lwr, all = TRUE, pages = 1, scale = 0, shade = TRUE)

op <- par(mfrow = c(2,2), no.readonly = TRUE)
chk2 <- capture.output(gam.check(lwr,pch=19,cex=.3))
par(op)

wk$fit <- func(predict(lwr, newdata = wk, type = "response"))
p <-
  xyplot(IndWgt ~ length | fYear_a, data = ca) +
  xyplot(fit ~ length | fYear_a, data = wk, type = "l")

print(p)

dev.off()
}
