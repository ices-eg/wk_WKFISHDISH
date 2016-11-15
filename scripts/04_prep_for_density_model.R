# -------------------------------------
#
# Prepare for density modelling
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")

# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

# load data
dat <- read.csv(paste0("input/hh_with_weight.csv"))

# remove NAs - why are there NAs...
dat <- dat[complete.cases(dat),]

# convert dat to spatial
coordinates(dat) <- ~ ShootLong + ShootLat
crs(dat) <- crs(statrec)

# read in spatial datasets
load("input/spatial_data.rData")

# only use statsquares that have been sampled at least once ? twice?
tmp <- colSums(gContains(statrec, dat, byid = TRUE))
statrec <- statrec[which(tmp > 0 |
                         statrec$ICESNAME %in% c("28E3", "28E4",
                                                         "27E4", "27E5", "27E6",
                                                         "26E4", "26E5",
                                                         "25E4",
                                                         "24E4")),]

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
adj <- spdep::nb2mat(adj, style = "B", zero.policy = TRUE)
Q <- methods::as(adj, "dgTMatrix")
Q @ x[] <- -1/Q @ x
diag(Q) <- rowSums(adj)
Q <- as.matrix(Q)
colnames(Q) <- rownames(Q) <- statrec$ICESNAME
statrec$StatRec <- statrec$ICESNAME

save(area, statrec, adj, dat, Q,
     file = paste0("input/spatial_model_data.rData"))

