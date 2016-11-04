# -------------------------------------
#
# Do some plots of data and model fits
#
# -------------------------------------

# load packages etc.
source("scripts/header.R")



# model fit plotting
# set up color scale
cols <- gplots::rich.colors(50)
fit <- sapply(gs, predict, newdata = statrec@data)
# make years sum to 1
fit <- sweep(fit, 2, colSums(fit), "/")
fit[] <- as.numeric(cut(fit, 50))

pdf("figure/spatial_fit_plots.pdf", onefile = TRUE)

for (i in unique(dat$Year)) {
  plot(area, xlim = bbox(statrec)["x",], ylim = bbox(statrec)["y",], col = grey(0.5))
  plot(statrec, col = cols[fit[,paste(i)]], add = TRUE)
  plot(area, lwd = 2, add = TRUE)
  mtext(paste(i), font = 2)
}

dev.off()


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


