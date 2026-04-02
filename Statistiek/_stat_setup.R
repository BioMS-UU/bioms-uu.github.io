# Pakketten
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(NHANES)
library(HistData)
library(palmerpenguins)
library(cowplot)
library(patchwork)
library(gridExtra)
library(grid)
library(knitr)
library(kableExtra)
library(gt)
library(scales)
library(ggforce)
library(vcd)

# Kleurpalet
opvulkleur  <- "#ffac63" # "YellowOrange"
opvulkleur1 <- "#C58BE2" # "#cc9ae5" # "darkorchid"
lijnkleur0  <- "#ff7701"
lijnkleur1  <- "darkorchid"
lijnkleur2  <- "#c62a01"
lijnkleur3  <- "#0044FB"
lijnkleur4  <- "#589c48"

# Accentkleuren voor tekst / annotaties
accentkleur1 <- lijnkleur0   # oranje tekst
accentkleur2 <- lijnkleur1   # paarse tekst

# Punkleuren voor datapunten
puntkleur1 <- lijnkleur0     # oranje punten
puntkleur2 <- lijnkleur1     # paarse punten
puntkleur3 <- "gray50"       # grijze punten
puntkleur4 <- "red"          # rode punten

# Gearceerd gebied onder een normaalcurve (van a tot b).
# plotdata: data frame met kolommen x en y; standaard de globale variabele `data`.
oppervlak <- function(a, b, plotdata = data) {
  geom_area(
    data  = subset(plotdata, x >= a & x <= b),
    aes(x, y),
    fill  = opvulkleur1,
    alpha = 0.4
  )
}

# Plot van een normale verdeling met gemiddelde mu en standaarddeviatie sigma.
normaalSpecifiek <- function(mu, sigma, xlabel) {
  x    <- seq(mu - 3.5 * sigma, mu + 3.5 * sigma, length.out = 350)
  data <- data.frame(x = x, y = dnorm(x, mean = mu, sd = sigma))
  ggplot(data, aes(x, y)) +
    geom_line(color = lijnkleur0, linewidth = 1) +
    scale_x_continuous(
      breaks       = mu + c(-3, -2, -1, 0, 1, 2, 3) * sigma,
      labels       = mu + c(-3, -2, -1, 0, 1, 2, 3) * sigma,
      minor_breaks = NULL
    ) +
    scale_y_continuous(labels = NULL) +
    labs(y = "Kansdichtheid", x = xlabel) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12))
}
