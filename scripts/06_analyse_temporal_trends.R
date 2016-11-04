# libraries

# load packages etc.
source("scripts/header.R")



require(coda)






# analyse temporal trends in ratio of abundance
nsim <- 1000
subareas <- sort(unique(statrec$Max_Area))
nsubareas <- length(subareas)

# simulate 1000 from each subarea
sims <- array(NA, c(nyears, nsubareas, nsim), dimnames = list(years, subareas, NULL))
X <- predict(gs[[1]], newdata = statrec@data, type = "lpmatrix")
for (i in 1:nyears) {
  g <- gs[[i]]
  fsim <- MASS::mvrnorm(nsim, coef(g), g$Vp) %*% t(X)
  sims[i,,] <- apply(exp(fsim), 1, function(x) c(tapply(x, statrec$Max_Area, mean)))
}


# analyse cpue by area

# fit a trend model to each simulated time series
fitsims <- sims
for (sa in subareas) {
  cat("                              \rfitting area", sa); flush.console()
  # fit to each simulation
  fitsims[,sa,] <-
    sapply(1:nsim,
           function(i) {
             x <- sims[,sa,i]
             exp(fitted(gam(log(x) ~ s(years, m = 2, k = nyears-1))))
           })
}

# plot
pdf("figure/temporal_subarea_plots.pdf", onefile = TRUE)

for (sa in subareas) {
  # plot data
  matplot(years, log(sims[,sa,]), type = "p",
          pch = 16, cex = 0.7, col = paste0(colorRampPalette("lightblue")(1), "11"),
          axes = FALSE, ylab = "log cpue", xlab = "Year", main = sa)
  # plot model fits
  x <- log(fitsims[,sa,])
  matplot(years, x, type = "l", lty = 1, col = paste0(grey(0.1), "11"), add = TRUE)
  box(bty = "l")
  axis(1)
  axis(2, las = 1)
}

dev.off()




# analyse cpue ratio by area pair

# fit a trend model to each simulated time series
pairs <- combn(3, 2)
lratiosims <- array(NA, c(nyears, nsubareas, nsim), dimnames = list(years, 1:ncol(pairs), NULL))
fitlratiosims <- array(NA, c(nyears, nsubareas, nsim), dimnames = list(years, 1:ncol(pairs), NULL))
for (j in 1:ncol(pairs)) {
  cat("                              \rfitting comparison", j, "of", ncol(pairs)); flush.console()
  lratiosims[,j,] <- log(sims[,pairs[1,j],]) - log(sims[,pairs[2,j],])
  # fit to each simulation
  fitlratiosims[,j,] <-
    sapply(1:nsim,
           function(i) {
             x <- lratiosims[,j,i]
             fitted(gam(x ~ s(years, m = 2, k = nyears-1)))
           })
}

# plot
pdf("figure/temporal_logratio_plots.pdf", onefile = TRUE)

for (j in 1:ncol(pairs)) {
  # plot data
  matplot(years, lratiosims[,j,], type = "p",
          pch = 16, cex = 0.7, col = paste0(colorRampPalette("lightblue")(1), "11"),
          axes = FALSE, ylab = "log cpue ratio", xlab = "Year",
          main = paste("log", paste(subareas[pairs[,j]], collapse = "/")))
  # plot model fits
  x <- fitlratiosims[,j,]
  matplot(years, x, type = "l", lty = 1, col = paste0(grey(0.1), "11"), add = TRUE)
  cis <- apply(x, 1, function(x) HPDinterval(mcmc(x), .95))
  lines(years, cis[1,], lty = 2)
  lines(years, cis[2,], lty = 2)
  abline(h = 0, col = "red")
  box(bty = "l")
  axis(1)
  axis(2, las = 1)
}

dev.off()


# Hypothesis test plots

# plot
pdf("figure/hypothesis_plots.pdf", onefile = TRUE)

for (j in 1:ncol(pairs)) {
  # plot model fits
  x <- fitlratiosims[,j,]
  # is there a significant deviation from the first three years
  x <- x - rep(apply(x[1:3,], 2, mean), each = nrow(x))
  # or from the first year only
  #x <- x - rep(x[1,], each = nrow(x))

  matplot(years, x, type = "l", lty = 1, col = paste0(grey(0.1), "11"),
          axes = FALSE, ylab = "log cpue ratio", xlab = "Year",
          main = paste("log", paste(subareas[pairs[,j]], collapse = "/"), "relative to", years[1], "-", years[3]))
  cis <- apply(x, 1, function(x) HPDinterval(mcmc(x), .95))
  lines(years, cis[1,], lty = 2)
  lines(years, cis[2,], lty = 2)

  abline(h = 0, col = "red")
  box(bty = "l")
  axis(1)
  axis(2, las = 1)
}

dev.off()

