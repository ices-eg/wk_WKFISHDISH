# -------------------------------------
#
# Fit density surface for each species and survey
#
# -------------------------------------

source("scripts/header.R")


# read in spatial datasets
load("input/spatial_model_data.rData")

if (!dir.exists("figures")) dir.create("figures")

plot.report <- function(df) {

  if (nrow(df$data[[1]]) == 0) return(NULL)

  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), heights = c(1.5,1))

  plot(df$statrec[[1]], main = paste0(df$species, " ",df$Survey, " Q", df$Quarter), border = grey(0.5, alpha=0.5))
  #plot(area, border = grey(0.7, alpha=0.5), add = TRUE)
  pdat <- df %>% unnest(data) %>% unnest(cg, cg_ci)
  lines(pdat$y, pdat$x)
  years <- pdat$Year - min(pdat$Year) + 1
  nyears <- max(years)
  cols <- colorRampPalette(c("cyan", "magenta"))(nyears)
  points(pdat$y, pdat$x, col = cols[years], pch = 16)

  plot(pdat$Year, pdat$x, type = "l", ylim = range(pdat$x, pdat$x.ciu, pdat$x.cil),
       axes = FALSE, ylab = "Latitude", xlab = "Year")
  points(pdat$Year, pdat$x)
  lines(pdat$Year, pdat$x.cil, lty = 2)
  lines(pdat$Year, pdat$x.ciu, lty = 2)
  axis(1); axis(2, las = 1); box(bty="l")

  plot(pdat$Year, pdat$y, type = "l", ylim = range(pdat$y, pdat$y.ciu, pdat$y.cil),
       axes = FALSE, ylab = "Longitude", xlab = "Year")
  points(pdat$Year, pdat$y)
  lines(pdat$Year, pdat$y.cil, lty = 2)
  lines(pdat$Year, pdat$y.ciu, lty = 2)
  axis(1); axis(2, las = 1); box(bty="l")

}


selected.species <- "Norway Pout"
for (selected.species in unique(getControlTable()$Species)) {

  load(paste0("output/", selected.species, "_centre_gravity.rData"))

  # plot
  pdf(paste0("figures/", selected.species, "_centre_gravity.pdf"), onefile = TRUE, paper = "a4")
  tmp <- sapply(1:nrow(data), function(i) plot.report(data[i,]))
  dev.off()

}
