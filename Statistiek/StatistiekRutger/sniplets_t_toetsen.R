```{r}
#| code-fold: true
#| warning: false
#| echo: false
#| fig-width: 2.5
#| fig-height: 3
#| out-width: "40%"
#| fig-cap: "Met behulp van een $t$-toets kunnen we onderzoeken of het gemiddelde van een steekproef (bijvoorbeeld n = 10) significant verschilt van een verwachte waarde. De $t$-toets gaat ervan uit dat de data normaal verdeeld zijn."
#| label: fig-t-toets-1-var

library(ggplot2)

set.seed(125) # Voor reproduceerbaarheid

# Trek 10 getallen uit een normaalverdeling met gemiddelde mu en sd sigma
mu <- 6.5 # Gemiddelde van de normaalverdeling
sigma <- 1.5 # Standaarddeviatie van de normaalverdeling
x <- rnorm(10, mu, sigma)
data <- data.frame(y = x,
                   x = seq(10))
m <- mean(x) # Steekproefgemiddelde

ggplot(data, aes(x = x, y = y)) +
  # Verticale lijnen naar het gemiddelde
  geom_segment(
    aes(
      x = x, xend = x, 
      y = y, yend = m
    ),
    color = "SlateGray",
    linetype = "solid",
    size = 1
  ) +
  # Punten
  geom_point(
    shape = 21, 
    fill = "SlateGray", 
    size = 5
  ) +
  # Gemiddelde lijn
  annotate(
    "segment",
    x = 0.5, xend = 10.5, 
    y = m, 
    color = opvulkleur,
    size = 1
  ) +
  # Label bij de gemiddelde lijn
  annotate(
    "text",
    x = 10.2, 
    y = m + 0.2,
    label = expression(bar(italic(x))),
    color = opvulkleur,
    hjust = 0,
    size = 6
  ) +
  # Y-as instellen
  scale_y_continuous(
    breaks = x, 
    limits = c(min(x), max(x)),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  ylab("Tentamencijfers") +
  # Geen x-as, geen gridlijnen
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
```
