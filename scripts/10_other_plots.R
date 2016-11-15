# -------------------------------------
#
# Do some plots of data and model fits
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")

# read in spatial datasets
#load("input/spatial_data.rData")
load("input/spatial_model_data.rData")

#plot
png("figures/TAC_units.png",
    width = 7, height = 7, units = "in", pointsize = 12,
    bg = "white", res = 600,
    type = "cairo-png")
  # plot regions with names
  plot(area, col = gplots::rich.colors(nrow(area), alpha = 0.5), border = grey(0.4))
  text(sp::coordinates(area),
       labels = area$SubAreaDiv,
       cex = 0.45, font = 2)
dev.off()


#plot
png("figures/spatial_smoother_structure.png",
    width = 7, height = 7, units = "in", pointsize = 12,
    bg = "white", res = 600,
    type = "cairo-png")
  # plot regions with names
  plot(area, col = gplots::rich.colors(nrow(area), alpha = 0.5), border = grey(0.4))

  plot(statrec, col = grey(0.5, alpha = 0.5), add = TRUE)
  xy <- coordinates(statrec)
  nbs <- cbind(rep(1:length(adj), sapply(adj, length)), unlist(adj))
  nbs <- unique(t(apply(nbs, 1, sort)))
  segments(xy[nbs[,1],1], xy[nbs[,1],2],
           xy[nbs[,2],1], xy[nbs[,2],2],
           col = "blue")
  points(xy, pch = 16, col = "red", cex = 0.7)
dev.off()






if (FALSE) {

# model anomaly fit plotting
# set up color scale
cols <- gplots::rich.colors(50)
fit <- sapply(gs, predict, newdata = statrec@data)
# make years sum to 1
fit <- sweep(fit, 2, colSums(fit), "/")
fit <- fit - fit[,1]
fit[] <- as.numeric(cut(fit, 50))
pdf("figure/spatial_anomaly_plots.pdf", onefile = TRUE)

for (i in unique(dat$Year)) {
  plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], col = grey(0.5))
  plot(statrec, col = cols[fit[,paste(i)]], add = TRUE)
  plot(area, lwd = 2, add = TRUE)
  mtext(paste(i), font = 2)
}

dev.off()

# model se plotting
# set up color scale
cols <- gplots::rich.colors(50)
sefit <- sapply(gs, function(x) predict(x, newdata = statrec@data, se.fit = TRUE)$se.fit)
sefit[] <- as.numeric(cut(sefit, 50))

pdf("figure/spatial_sefit_plots.pdf", onefile = TRUE)

for (i in unique(dat$Year)) {
  plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], col = grey(0.5))
  plot(statrec, col = cols[sefit[,paste(i)]], add = TRUE)
  mtext(paste(i), font = 2)
}

dev.off()




# data plotting
pdf("figure/data_plots.pdf", onefile = TRUE)

for (i in unique(dat$Year)) {
  base()
  pdat <- dat[dat$Year == i,]
  plot(pdat, pch = 16, cex = 0.25*log(pdat$weight), add = TRUE)
  plot(pdat, pch = 1, cex = 0.1, col = grey(0.3), add = TRUE)
  mtext(paste(i), font = 2)
}

dev.off()

}
