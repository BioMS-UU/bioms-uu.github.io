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


::: {.Rexr #exr-zuurgraad}
  Een onderzoeker vermoedt dat onder invloed van zonlicht de zuurgraad van vijvers kan veranderen. Om dit vermoeden te
  toetsen meet hij in negen vijvers zowel ’s ochtends als ’s avonds de pH.
  De verkregen gegevens zijn in de volgende vectoren weergegeven:
    ```{r}
  zuurgraadOchtend <- c(4.89, 5.28, 5.24, 5.27, 4.92, 5.35, 4.76, 5.72, 4.62)
  zuurgraadAvond <- c(5.29, 5.51, 5.75, 5.62, 5.42, 6.09, 4.92, 6.01, 5.34)
  ```
  
  **a.** Welke statistische toets zou hier gepast zijn? Voer deze toets uit in R.  
  
  **b.** Geef een schatting van de effectgrootte. Geef er ook een 95% betrouwbaarheidsinterval bij, aannemend dat aan de aannames is voldaan.
  
  **c.** Formuleer de hypotheses die de onderzoeker wil toetsen. Welke toets
  hoort hierbij? Wat is op grond van de uitvoer je conclusie ten aanzien
  van deze hypotheses? Geef hierbij ook de overschrijdingskans.
  :::

::: {#exr-diastolische-bloeddruk}

**a.** 
  
  Hieronder staan diastolische bloeddrukwaarden die gemeten zijn bij een steekproef van 50 mannen. Hierna is er een $t$-test uitgevoerd. 

``` {r}
bloeddruk <- c(94, 94, 82, 100, 112,
               110, 84, 78, 92, 112,
               94, 92, 86, 84, 90,
               72, 88, 92, 88, 84,
               98, 98, 84, 90, 90,
               70, 80, 90, 80, 106,
               74, 95, 100, 94, 84,
               70, 102, 92, 84, 80,
               84, 86, 98, 82, 80,
               88, 80, 84, 100, 86)

t.test(bloeddruk, mu=90)
```
Bestudeer de uitvoer; zorg dat je de structuur en de inhoud ervan begrijpt. 

**a.** Welke toets wordt precies uitgevoerd? Stel de nulhypothese en de alternatieve hypothese op. Welke aannames moet men maken om deze toets uit te voeren? 
  
  **b.** Lees de toetsingsgrootheid en bijbehorende overschrijdingskans af. Wat is je conclusie ten aanzien van de gestelde hypotheses, aangenomen dat aan de voorwaarden is voldaan? 
  
  **c.** Lees het 95%-betrouwbaarheidsinterval voor het populatiegemiddelde af uit de uitvoer.

**d.** Wat is het verband tussen de antwoorden op onderdelen **b** en **c**?
  :::
  
  ::: {#fig-manakin-distributie layout-ncol="2" out-width="100%"}
    ```{r}
    #| code-fold: true
    #| warning: false
    #| fig.width: 3.5
    #| fig.height: 4
    #| out-width: "100%"
    #| fig-cap: "Histogram van de gemeten gewichten van de Bonte manakins, per geslacht."
    #| label: fig-dist-manakin-1
    
    massa_manacus <- read.csv("https://tbb.bio.uu.nl/bvd/bms/slides/3_Statistiek_data/massa_manacus.csv")
    
    massa_manacus %>%
      filter(Sex != "Undetermined") %>%
      ggplot(aes(x = Sex, y = Weight, fill = Sex)) +  
      geom_boxplot() + 
      theme_minimal() +
      ylab("Gewicht (g)") +
      scale_fill_discrete(name = "Geslacht",
                          labels = c("Vrouwelijk", "Mannelijk")) +
      theme(legend.position = "top",
            axis.title.x = element_blank()) +  
      guides(fill = guide_legend(nrow = 2)) +  
      scale_x_discrete(
        labels = c("Female" = paste("n =", sum(massa_manacus$Sex == "Female")),
                   "Male" = paste("n =", sum(massa_manacus$Sex == "Male"))))  
    
    
    ```
    
    ```{r}
    #| code-fold: true
    #| warning: false
    #| fig.width: 3.5
    #| fig.height: 4
    #| out.width: "100%"
    #| fig-cap: "Histogram van het gewicht van vrouwelijke en mannetlijke Bonte manakins."
    #| label: fig-dist-manakin-2
    
    massa_manacus %>%
      filter(Sex != "Undetermined") %>%
      ggplot(aes(x = Weight, fill = Sex)) +
      geom_histogram(color = "black", 
                     binwidth = 1, 
                     position = "dodge") +  
      theme_minimal() +
      theme(legend.position = "none") +  
      xlab("Gewicht (g)") +
      ylab("Aantal observaties") +
      facet_wrap(vars(Sex), nrow = 2, 
                 labeller = as_labeller(c("Female" = "Vrouwelijk", 
                                          "Male" = "Mannelijk")))  
    
    ```
    
    Verdeling van gemeten gewichten van de Bonte manakin in Colombia.
    :::
