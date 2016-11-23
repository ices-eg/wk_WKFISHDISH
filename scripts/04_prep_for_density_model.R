# -------------------------------------
#
# Prepare for density modelling
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")

# read in spatial datasets
load("input/spatial_data.rData")

# load data
con <- dbConnect(SQLite(), dbname = "db/datras.sqlite")
dat <- dbReadTable(con, "hh")
dbDisconnect(con)

# remove NAs - why are there NAs...
dat <- dat[complete.cases(dat),]

# convert dat to spatial
coordinates(dat) <- ~ ShootLong + ShootLat
crs(dat) <- crs(statrec)

# first adjust haul locations as quit a few lie exactly on the boundaries of statrecs!
dat@coords <- coordinates(dat) + 1e-5
tmp <- gContains(statrec, dat, byid = TRUE)

# add statrec column to data
dat$StatRec <- statrec$ICESNAME[apply(tmp, 1, function(x) which(x==1))]

# only use statsquares that have been sampled at least once
# and add in un sampled stat squares in
statrec <- statrec[statrec$ICESNAME %in% c(unique(dat$StatRec),
                                           "28E3", "28E4",
                                                   "27E4", "27E5", "27E6",
                                                   "26E4", "26E5",
                                                   "25E4",
                                                   "24E4"),]

# prepare GMRF for spatial model
adj <- spdep::poly2nb(statrec, queen = FALSE)

# drop connections
dropcon <- function(adj, x1, x2) {
  id1 <- which(statrec$ICESNAME == x1)
  id2 <- which(statrec$ICESNAME == x2)
  adj[[id1]] <- setdiff(adj[[id1]], id2)
  adj[[id2]] <- setdiff(adj[[id2]], id1)
  adj
}
adj <- dropcon(adj, "39F8", "39F9")
adj <- dropcon(adj, "40G1", "40G2")
adj <- dropcon(adj, "39G1", "39G2")

# form penalty
adjmat <- spdep::nb2mat(adj, style = "B", zero.policy = TRUE)
Q <- methods::as(adjmat, "dgTMatrix")
Q @ x[] <- -1/Q @ x
diag(Q) <- rowSums(adjmat)
Q <- as.matrix(Q)
colnames(Q) <- rownames(Q) <- statrec$ICESNAME
statrec$StatRec <- statrec$ICESNAME
statrec$fStatRec <- factor(statrec$StatRec)

# find adjacency of areas
area_adj <- spdep::poly2nb(area, queen = FALSE)
nbs <- cbind(rep(1:length(area_adj), sapply(area_adj, length)), unlist(area_adj))
nbs <- unique(t(apply(nbs, 1, sort)))
# drop 7a-7b connection
nbs <- nbs[!(area$SubAreaDiv[nbs[,1]] == "7.a" & area$SubAreaDiv[nbs[,2]] == "7.b"),]


save(area, statrec, adj, area_adj, nbs, dat, Q,
     file = paste0("input/spatial_model_data.rData"))

