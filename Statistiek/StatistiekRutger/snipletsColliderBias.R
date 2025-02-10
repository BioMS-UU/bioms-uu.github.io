
```{r}
#| code-fold: true
#| warning: false
#| fig.cap: ""
#| label: fig-collider-plot-2
#| fig.width: 4
#| fig.height: 4
#| out.width: "60%"

if (!("MASS" %in% .packages())) {
  library(MASS)
}
if (!("ggplot2" %in% .packages())) {
  library(ggplot2)
}

# Parameters
n_steekproef <- 1000  # Aantal waarnemingen in de populatie
n_deelsteekproef <- 80  # Aantal waarnemingen in de deelsteekproef
lambda <- 2  # Parameter voor selectiekans

# Genereer bivariate normale steekproef zonder correlatie
set.seed(42)
sigma <- matrix(c(1, 0, 0, 1), nrow = 2)  # Covariantiematrix
steekproef <- as.data.frame(
  mvrnorm(n_steekproef, mu = c(0, 0), Sigma = sigma)
)
colnames(steekproef) <- c("X", "Y")

# Selectiekans berekenen op basis van exp(lambda * (X + Y))
steekproef$kans <- exp(lambda * (sign(steekproef$X)*steekproef$X^2 + sign(steekproef$Y)*steekproef$Y^2))
steekproef$kans <- steekproef$kans / sum(steekproef$kans)  # Normalisatie

# Voeg een kleurkolom toe en initialiseer als "black"
steekproef$kleur <- "darkgray"

# Trek de deelsteekproef met gewogen kansen
deelsteekproef_indices <- sample(
  1:n_steekproef, n_deelsteekproef, prob = steekproef$kans, replace = FALSE
)
steekproef$kleur[deelsteekproef_indices] <- "#e8601d"

# Plot de gegevens
steekproef <- steekproef[order(steekproef$kleur, decreasing = TRUE), ]

plot <- ggplot(
  steekproef, aes(x = X, y = Y, color = kleur)
) +
  geom_point(alpha = 1, size = 2) +
  scale_color_identity() +
  theme_minimal() +
  labs(
    title = NULL,
    x = "fysieke aantrekkelijkheid", y = "acteertalent"
  )

print(plot)

```



::: {#fig-collider layout-ncol="2" out.width="100%"}
  ```{r}
  #| code-fold: true
  #| warning: false
  #| fig.cap: "In the gehele populatie is acteertalent onafhankelijk van uiterlijke aantrekkelijkheid. (Fictieve gegevens.)"
  #| label: fig-mozaiek-collider-A
  #| fig.width: 3.5
  #| fig.height: 3.5
  #| out.width: "100%"
  
  aantal_acteurs <- 10000
  number_sampled <- 1000
  
  set.seed(145)
  
  aantrekkelijk <- factor(sample(c("nee", "ja"), aantal_acteurs, replace = TRUE, prob = c(0.9, 0.1)), levels = c("ja", "nee"))
  talent <- factor(sample(c("nee", "ja"), aantal_acteurs, replace = TRUE, prob = c(0.9, 0.1)), c("nee", "ja"))
  gegevens <- data.frame(talent, aantrekkelijk)
  #gegevens_geselecteerd <- gegevens[aantrekkelijk == "ja" | talent == "ja", ]
  prob <- 0.002 + 
    (gegevens$aantrekkelijk == "ja" | gegevens$talent == "ja")*0.75 + 
    (gegevens$aantrekkelijk == "ja" & gegevens$talent == "ja")*0.15 
  
  select <- sample(1:nrow(gegevens), number_sampled, prob = prob)
  gegevens_geselecteerd <- gegevens[select, ]
  
  # Create a mosaic plot using base R
  mosaicplot(
    table(gegevens), 
    color = kleuren <- c("DarkOrchid", "#e8601d"),
    main = "Gehele populatie",
    xlab = "aantrekkelijk uiterlijk",
    ylab = "acteertalent"
  )
  ```
  
  ```{r}
  #| code-fold: true
  #| warning: false
  #| fig.cap: "In de populatie van successvolle acteurs hebben onaantrekkelijke acteurs vaker vaker talent, puur omdat personen die noch *looks* noch talent hebben zelden door de selectie komen."
  #| label: fig-mozaiek-collider-B
  #| fig.width: 3.5
  #| fig.height: 3.5
  #| out.width: "100%"
  
  mosaicplot(
    table(gegevens_geselecteerd),
    main = "Succesvolle acteurs",
    color = kleuren <- c("DarkOrchid", "#e8601d"),
    xlab = "aantrekkelijk uiterlijk",
    ylab = "acteertalent"
  )
  
  ```
  :::