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
opvulkleur2 <- "#409899"
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

# Plot van een normale verdeling.
# labels: "symbolisch" = mu +/- n*sigma uitdrukkingen (45 graden);
#         "numeriek"   = getallen bij mu +/- 1/2/3 sigma;
#         "z"          = getallen -2,-1,0,1,2 (standaardnormaal);
#         "geen"       = geen aangepaste x-as (y-as labels blijven zichtbaar).
# angle: rotatiehoek voor x-as labels (standaard 45).
normaalCurve <- function(mu, sigma, xlabel, labels = "symbolisch", angle = 45) {
  x <- seq(mu - 3.5 * sigma, mu + 3.5 * sigma, length.out = 350)
  d <- data.frame(x = x, y = dnorm(x, mean = mu, sd = sigma))
  p <- ggplot(d, aes(x, y)) +
    geom_line(color = lijnkleur0, linewidth = 1) +
    scale_y_continuous(labels = NULL) +
    labs(y = "Kansdichtheid", x = xlabel) +
    theme_minimal()
  if (labels == "symbolisch") {
    p + scale_x_continuous(
      breaks = mu + c(-2, -1, 0, 1, 2) * sigma,
      labels = parse(text = c(
        "italic(mu) - 2*italic(sigma)",
        "italic(mu) - italic(sigma)",
        "italic(mu)",
        "italic(mu) + italic(sigma)",
        "italic(mu) + 2*italic(sigma)"
      )),
      minor_breaks = NULL
    ) + theme(axis.text.x = element_text(size = 12, angle = angle))
  } else if (labels == "numeriek") {
    p + scale_x_continuous(
      breaks       = mu + c(-3, -2, -1, 0, 1, 2, 3) * sigma,
      labels       = mu + c(-3, -2, -1, 0, 1, 2, 3) * sigma,
      minor_breaks = NULL
    ) + theme(axis.text.x = element_text(size = 12))
  } else if (labels == "z") {
    p + scale_x_continuous(
      breaks = c(-2, -1, 0, 1, 2),
      labels = c(-2, -1, 0, 1, 2),
      minor_breaks = NULL
    ) + theme(axis.text.x = element_text(size = 12))
  } else {
    # labels = "geen": geen aangepaste x-as, y-as labels zichtbaar
    ggplot(d, aes(x, y)) +
      geom_line(color = lijnkleur0, linewidth = 1) +
      labs(y = "Kansdichtheid", x = xlabel) +
      theme_minimal()
  }
}

# Pijl + label voor de standaarddeviatie op een normaalcurve.
# label:   annotatie-tekst; NULL = alleen pijl, geen label.
# y_scale: y-positie van het label als fractie van de pijlhoogte (standaard 0,9).
# hjust:   horizontale uitlijning van het label (standaard 0,5).
pijlSigma <- function(mu, sigma, label = expression(italic(sigma)),
                      y_scale = 0.9, hjust = 0.5) {
  h <- exp(-0.5) / sqrt(2 * pi) / sigma
  arrow_layer <- geom_segment(
    aes(x = mu, xend = mu + sigma, y = h, yend = h),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "black", linewidth = 0.8
  )
  if (is.null(label)) return(arrow_layer)
  list(
    arrow_layer,
    annotate("text", x = mu + 0.5 * sigma, y = y_scale * h,
             label = label,
             color = "black", size = 5, hjust = hjust, vjust = 0.5)
  )
}

# Proportie-label in het midden van een kansverdeling.
proportie <- function(prop) {
  annotate("text", x = 0, y = 0.15, label = prop,
           color = "black", size = 5, hjust = 0.5, vjust = 0.5)
}

# Kritieke-waardenplot voor een willekeurige verdeling.
# dfun: dichtheidsfunctie (bv. dnorm of function(x) dt(x, df=4))
# qfun: kwantielsfunctie  (bv. qnorm of function(p) qt(p, df=4))
critPlot <- function(alpha, dfun, qfun, xlabel) {
  x_seq  <- seq(-3.5, 3.5, length.out = 350)
  d      <- data.frame(x = x_seq, y = dfun(x_seq))
  crit   <- qfun(1 - alpha / 2)
  xstart <- (crit + 3.5) / 2
  ystart <- dfun(0) / 4
  xend   <- crit + (3.5 - crit) / 7
  yend   <- dfun(xend) / 3
  ggplot(d, aes(x, y)) +
    geom_line(color = lijnkleur0, linewidth = 1) +
    labs(y = "Kansdichtheid", x = xlabel) +
    theme_minimal() +
    oppervlak(-10, -crit, d) +
    oppervlak(crit,  10,  d) +
    scale_x_continuous(
      breaks = c(-crit, 0, crit),
      labels = c(paste0(round(-crit, 2), "..."), "0",
                 paste0(round( crit, 2), "...")),
      minor_breaks = NULL
    ) +
    geom_curve(aes(x = -xstart, y = ystart, xend = -xend, yend = yend),
               curvature =  0.2, arrow = arrow(length = unit(0.2, "cm")),
               color = "black", linewidth = 0.7) +
    geom_curve(aes(x =  xstart, y = ystart, xend =  xend, yend = yend),
               curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")),
               color = "black", linewidth = 0.7) +
    annotate("text", x = -xstart, y = ystart + 0.02,
             label = gsub("\\.", ",", round(alpha / 2, 3)),
             color = "black", hjust = 0.5, size = 4) +
    annotate("text", x =  xstart, y = ystart + 0.02,
             label = gsub("\\.", ",", round(alpha / 2, 3)),
             color = "black", hjust = 0.5, size = 4) +
    proportie(gsub("\\.", ",", round(1 - alpha, 2)))
}

# Wrapper voor de standaardnormale verdeling.
critPlot_norm <- function(alpha) {
  critPlot(alpha, dfun = dnorm, qfun = qnorm,
           xlabel = expression(italic(Z)))
}

# Wrapper voor de t-verdeling met df vrijheidsgraden.
critPlot_t <- function(alpha, df) {
  critPlot(alpha,
           dfun   = function(x) dt(x, df = df),
           qfun   = function(p) qt(p, df = df),
           xlabel = expression(italic(t)))
}

# Basisplot van een t-verdeling met df vrijheidsgraden.
# Analoog aan normaalCurve, maar zonder verborgen y-as (dichtheidswaarden zichtbaar).
# Voeg specifieke lagen (vlijnlijnen, arcering, annotaties) toe op de aanroeplocatie.
tCurve <- function(df, xlabel = expression(italic(t))) {
  x <- seq(-3.5, 3.5, length.out = 350)
  d <- data.frame(x = x, y = dt(x, df = df))
  ggplot(d, aes(x, y)) +
    geom_line(color = lijnkleur0, linewidth = 1) +
    labs(y = "Kansdichtheid", x = xlabel) +
    theme_minimal()
}
# ---------------------------------------------------------------------------
# Histogram-hulpfuncties
# ---------------------------------------------------------------------------

# Basis histogram: data frame + kolomnaam + labels + breedte + optioneel bereik.
# Retourneert een ggplot waaraan extra lagen toegevoegd kunnen worden.
maakHistogram <- function(data, xvar, xlabel, binbreedte, bereik = NULL,
                           kleur = opvulkleur, alpha = 0.7) {
  p <- ggplot(data, aes(.data[[xvar]])) +
    geom_histogram(fill = kleur, alpha = alpha, binwidth = binbreedte,
                   boundary = if (!is.null(bereik)) bereik[1] else 0,
                   color = "black", linewidth = 0.5) +
    labs(x = xlabel, y = "Frequentie") +
    theme_minimal()
  if (!is.null(bereik)) p <- p + scale_x_continuous(limits = bereik)
  p
}

# Voegt een stippellijn + gebogen pijl + label voor het steekproefgemiddelde toe.
# data: numerieke vector (de steekproef); gemiddelde: de te markeren waarde.
gemiddeldeLijn <- function(data, gemiddelde, sigma, binbreedte, bereik) {
  breaks  <- seq(bereik[1], bereik[2], by = binbreedte)
  hoogste <- max(hist(data, breaks = breaks, plot = FALSE)$counts)
  list(
    geom_vline(xintercept = gemiddelde, color = lijnkleur1,
               linetype = "dashed", linewidth = 1),
    geom_curve(aes(x    = gemiddelde + 1.5 * sigma,  y    = 0.85 * hoogste,
                   xend = gemiddelde + sigma / 20,    yend = 0.70 * hoogste),
               curvature = -0.4, arrow = arrow(length = unit(0.2, "cm")),
               color = lijnkleur1, linewidth = 0.7),
    annotate("text", x = gemiddelde + 1.5 * sigma, y = 0.91 * hoogste,
             label = expression(bar(italic(X))),
             color = accentkleur2, hjust = 0.5, size = 4)
  )
}

# Voegt een geschaalde normaalcurve toe aan een frequentiehistogram.
normaalOverlay <- function(data, xvar, binbreedte) {
  mu <- mean(data[[xvar]], na.rm = TRUE)
  s  <- sd(data[[xvar]],  na.rm = TRUE)
  n  <- nrow(data)
  stat_function(fun = function(x) n * binbreedte * dnorm(x, mean = mu, sd = s),
                color = lijnkleur1, linewidth = 1)
}
