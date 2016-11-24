
source("scripts/header.R")


cgspecies_plots <- function(sp) {
  gsub("%s", sp,
"## %s

```{r %s_plot, dpi=400, fig.width=7, fig.height=6, echo=FALSE}
load(paste0('output/%s_centre_gravity.rData'))

# plot
tmp <- sapply(1:nrow(data), function(i) plot.report(data[i,]))
```

")
}



species_plots_div <- function(sp) {
  gsub("%s", sp,
"## %s

```{r %s_plot, dpi=600, fig.width=7, fig.height=7, echo=FALSE}
load('output/%s_trends_div.rData')
res$p_adj <- p.adjust(res$median_p, method = 'BH')
res %<>% filter(p_adj < 0.05) %>% select(-p)
if (nrow(res)>0) {
  plot_result(res, main = '%s')
}
```
")
}



species_tables_div <- function(sp) {
  gsub("%s", sp,
"## %s

```{r %s, results='asis', echo=FALSE}
load('output/%s_trends_div.rData')
res$p_adj <- p.adjust(res$median_p, method = 'BH')
res %<>% filter(p_adj < 0.05) %>% select(-p)
if (nrow(res)>0) {
  knitr::kable(res, digits = 3)
}
```

")
}


log_ratio_plots_div <- function(sp) {
  gsub("%s", sp,
"## %s

```{r %s, dpi=300, fig.width=8, fig.height=7, echo=FALSE, message=FALSE}
load('output/%s_trends_div.rData')
res$p_adj <- p.adjust(res$median_p, method = 'BH')
res %<>% filter(p_adj < 0.05) %>% select(-p)

# plot log ratios
tmp <- unnest(stocks, data)
tmp <- left_join(res, tmp)
if (nrow(res)>0) {
p <- xyplot(val ~ Year | paste0(Division1, ' ~ ', Division2, ' (', round(slope,2), ')'), groups = id,
       data = subset(tmp, id %in% 1:100),
       type = c('p', 'r'), col = grey(0.5, alpha = 0.3), pch = 16,
       scale = list(y = list(relation = 'free')),
      main = '%s',
      layout = c(4, 2), as.table = TRUE)
print(p)
}
```

")
}


# same for TAC areas ------------------------------


species_plots_tac <- function(sp) {
  gsub("%s", sp,
"## %s

```{r %s_plot, dpi=600, fig.width=7, fig.height=7, echo=FALSE}
load('output/%s_trends_tac.rData')
res$p_adj <- p.adjust(res$median_p, method = 'BH')
res %<>% filter(p_adj < 0.05) %>% select(-p)
if (nrow(res)>0) {
plot_result(res, main = '%s')
}
```
")
}



species_tables_tac <- function(sp) {
  gsub("%s", sp,
"## %s

```{r %s, results='asis', echo=FALSE}
load('output/%s_trends_tac.rData')
res$p_adj <- p.adjust(res$median_p, method = 'BH')
res %<>% filter(p_adj < 0.05) %>% select(-p)
if (nrow(res)>0) {
knitr::kable(res, digits = 3)
}
```

")
}


log_ratio_plots_tac <- function(sp) {
  gsub("%s", sp,
"## %s

```{r %s, dpi=300, fig.width=8, fig.height=7, echo=FALSE, message=FALSE}
load('output/%s_trends_tac.rData')
res$p_adj <- p.adjust(res$median_p, method = 'BH')
res %<>% filter(p_adj < 0.05) %>% select(-p)

# plot log ratios
tmp <- unnest(stocks, data)
tmp <- left_join(res, tmp)
if (nrow(res)>0) {
p <- xyplot(val ~ Year | paste0(TAC1, ' ~ ', TAC2, ' (', round(slope,2), ')'), groups = id,
data = subset(tmp, id %in% 1:100),
type = c('p', 'r'), col = grey(0.5, alpha = 0.3), pch = 16,
scale = list(y = list(relation = 'free')),
main = '%s',
layout = c(4, 2), as.table = TRUE)
print(p)
}
```

")
}





#species <- c("Spurdog", "Herring", "Sprat", "Anchovy", "Cod", "Haddock", "Whiting", "Blue Whiting",
#             "Pollack", "Saithe", "Hake", "Horse Mackerel", "Mackerel", "Megrim", "Plaice", "Sole",
#             "Anglerfish", "Norway Pout")

species <- unique(getControlTable()$Species)

cg_plots <- sapply(species, cgspecies_plots)

plots_div <- sapply(species, species_plots_div)
tables_div <- sapply(species, species_tables_div)
ratio_plots_div <- sapply(species, log_ratio_plots_div)

plots_tac <- sapply(species, species_plots_tac)
tables_tac <- sapply(species, species_tables_tac)
ratio_plots_tac <- sapply(species, log_ratio_plots_tac)


cat(file = "writeup/centre_of_gravity_plots.Rmd",
    readLines("writeup/cgplots_template.Rmd"),
    cg_plots, sep = "\n")


cat(file = "writeup/significant_trend_maps_div.Rmd",
    readLines("writeup/plots_div_template.Rmd"),
    plots_div, sep = "\n")

cat(file = "writeup/significant_tables_div.Rmd",
    readLines("writeup/tables_template.Rmd"),
    tables_div, sep = "\n")

cat(file = "writeup/significant_log_ratio_plots_div.Rmd",
    readLines("writeup/tables_template.Rmd"),
    ratio_plots_div, sep = "\n")


cat(file = "writeup/significant_trend_maps_tac.Rmd",
    readLines("writeup/plots_tac_template.Rmd"),
    plots_tac, sep = "\n")

cat(file = "writeup/significant_tables_tac.Rmd",
    readLines("writeup/tables_template.Rmd"),
    tables_tac, sep = "\n")

cat(file = "writeup/significant_log_ratio_plots_tac.Rmd",
    readLines("writeup/tables_template.Rmd"),
    ratio_plots_tac, sep = "\n")





# create a table of significant results
#resl <-
#  do.call(rbind,
#  lapply(paste0("output/", dir("output/", pattern = "*_trends.rData")),
#       function(x) {
#         load(x)
#         res$p_adj <- p.adjust(res$median_p, method = 'BH')
#         res %<>% filter(p_adj < 0.05) %>% select(-p)
#         res
#       }))

#res <-
#  rbind(resl[c("species", "Division1", "slope")],
#      rename(mutate(resl[c("species", "Division2", "slope")], slope=-slope), Division1 = Division2))

#with(res, tapply(slope, list(Division1, species), function(x) sum(sign(x))))


