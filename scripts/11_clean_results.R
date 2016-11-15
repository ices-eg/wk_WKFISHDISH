# -------------------------------------
#
# Clean results
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")

# read design table and look at species
fulltab <- getControlTable()
areas <- unique(fulltab$Division)

# read in spatial datasets
load("input/spatial_data.rData")


# find adjacency of areas
adj <- spdep::poly2nb(area, queen = FALSE)
nbs <- cbind(rep(1:length(adj), sapply(adj, length)), unlist(adj))
nbs <- unique(t(apply(nbs, 1, sort)))
# drop 7a-7b connection
nbs <- nbs[!(area$SubAreaDiv[nbs[,1]] == "7.a" & area$SubAreaDiv[nbs[,2]] == "7.b"),]
#cbind(area$SubAreaDiv[nbs[,1]], area$SubAreaDiv[nbs[,2]])

xy <- coordinates(area)
# relocate center of 3.d for plotting
#xy["3.d",] <- unlist(locator(n=1))
xy["3.d",] <- c(16.26910, 54.85013)
plot(area)
segments(xy[nbs[,1],1], xy[nbs[,1],2],
         xy[nbs[,2],1], xy[nbs[,2],2],
         col = "blue", lwd = 2)
points(xy, pch = 16, col = "red")



load("output/pmat.rdata")

# only consider comparisons between adjacent regions
ncomp <- nrow(nbs)
# i.e. 36 comparisons
divs <- gsub("^.*[ ]", "\\1", rownames(pmat))


lapply(1:nrow(nbs),
       function(i) {
         ids <- area$SubAreaDiv[nbs[i,]]
         rids <- which(divs %in% ids[1])
         cids <- which(divs %in% ids[2])
         pmat[rids, cids]
       }

pmat[]

# filter results
signif <- cbind(c(row(pmat)), c(col(pmat)))[which(pmat < 0.0001),]
signif <- unique(t(apply(signif, 1, sort)))
signif <- cbind(colnames(pmat)[signif[,1]], colnames(pmat)[signif[,2]])

subdiv <-

