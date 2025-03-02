::: {.hidden}

## Optellen van meerdere kansvariabelen {#sec-optellen}

We zullen in deze cursus regelmatig
    situaties tegenkomen waarbij
    we kansvariabelen bij elkaar optellen.
Bijvoorbeeld, 
    als we met twee dobbelstenen gooien
    en geïnteresseerd zijn in het totaal aantal ogen,
    dan tellen we in feite twee kansvariabelen op:
    het aantal ogen van de eerste dobbelsteen 
    en het aantal ogen van de tweede dobbelsteen.
Als we die twee kansvariabelen $X_1$ en $X_2$ noemen,
    dan gaat onze interesse dus uit naar
$$ Y = X_1 + X_2.$$ {#eq-som-1}
We hebben op deze manier een nieuwe kansvariabele $Y$ gedefinieerd.
De vraag is nu:
    wat kunnen we zeggen over de kansverdeling van $Y$?

Meer in het algemeen kan het zijn dat we niet twee
    maar $n$ kansvariabelen optellen.
We kunnen die variabelen $X_i$ noemen.
In dat algemene geval is $Y$ dus:
$$ Y = \sum_{i=1}^n X_i.$$ {#eq-som-2}
De vraag is weer wat we kunnen zeggen over de kansverdeling van $Y$.

### De verwachtingswaarde van een som {#sec-som-ev}

Om te beginnen kijken we naar 
    de verwachtingswaarde van $Y$, dus $\ev{Y}$.
(Verwachtingswaarde is een ander woord voor het gemiddelde van een kansvariabele,
  weet je nog?)
Hier is de regel heel eenvoudig:
    de verwachtingswaarde van de som
    is de som verwachtingswaarden, dus
$$
\ev{Y} = \sum_{i = 1}^n \ev{X_i}.
$$ {#eq-evsom}
Dit is niet erg verrassend.
Bijvoorbeeld:
Als het aantal ogen bij het gooien van één dobbelsteen 
gemiddeld $3 \frac{1}{2}$ is (zie @fig-uniform-dobbelsteen),
dan is het aantal ogen van twee dobbelstenen gemiddeld
$3 \frac{1}{2} + 3 \frac{1}{2} = 7$
(zoals te zien was in @fig-kansverdeling-twee-dobbelstenen).

### De variantie van de som

En hoe zit het met de variantie van $Y$, oftewel $\var{Y}$?

Het berekenen van de variantie van $Y$ is eenvoudig als de kansvariabelen $X_i$ **onafhankelijk** zijn. In @sec-onafhankelijk hebben we besproken wat het betekent als twee *gebeurtenissen* onafhankelijk zijn. Hier gaat het echter om de onafhankelijkheid van *variabelen*, wat om een specifieke uitleg vraagt.

Twee kansvariabelen zijn onafhankelijk 
  als de kansverdeling van de ene variabele niet afhangt 
  van de uitkomst van de andere. 
Met andere woorden: 
  de kans op een bepaalde uitkomst van $X_1$ verandert niet, 
  ongeacht wat je weet over de uitkomsten van $X_2$. 
Dit betekent dat elke mogelijke gebeurtenis van $X_1$ 
  onafhankelijk is van elke mogelijke gebeurtenis van $X_2$.

::: {#exr-onafhankelijk}
Verwerkingsopgave over onafhankelijkheid.

**a.**

**b.**

:::

Terug naar de variantie van $Y$,
    waarbij $Y$ de som was van $n$ kansvariabelen $X_i$.
Als alle variabelen $X_i$ onafhankelijk zijn van elkaar,
    dan is de variantie van $Y$ simpelweg
    de som van de varianties van de variabelen $X_i$:
$$
\var{Y} = \var{\sum_{i = 1}^n X_i} = \sum_{i = 1}^n \var{X_i}.
$$ {#eq-somvarianties}
Als je meerdere onafhankelijke variabelen optelt
    wordt de variantie dus steeds groter.

Zoals je weet is de standaarddeviatie 
    simpelweg de wortel van de variantie.
De standaarddeviatie van $Y$ is in dit geval dus
$$ 
\sd{Y} = \sqrt{\var{Y}} = \sqrt{\sum_{i = 1}^n \var{X_i}}.
$$ {#eq-somvarianties}
Het is belangrijk te zien dat je eerst de varianties van de $X_i$ optelt
    en daarna pas de wortel neemt.
De standaarddeviatie van $Y$ is dus *niet*
    de som van de standaarddeviaties van de $X_i$!
  
Als de $X_i$ niet onafhankelijk zijn,
kun je niets algemeens zeggen over de variantie van $Y$.

::: {#exr-wortel}
Verwerkingsopgave over $\sqrt{a + b} \neq \sqrt{a} + \sqrt{b}$.
:::


::: {#exr-psychologisch-experiment}
Psychologisch experiment. 
Twee proefpersonen (mensen? zijn er voorbeelden met primaten?). 
Eerste proefpersoon krijgt 10 euro (of iets anders)
    maar krijgt de gelegenheid om dat bedrag te delen
    met de tweede proefpersoon.
$X_1$ is het bedrag dat proefpersoon 1 zelf houdt,
$X_2$ is het bedrag dat deze aan proefpersoon 2 geeft.
Zowel $X_1$ als $X_2$ zijn kansvariabelen,
    want verschillende proefpersonen maken verschillende beslissingen.

**a.** Zijn $X_1$ en $X_2$ onafhankelijk?

**b.** Stel dat de standaarddeviatie van variabele $X_1$ gelijk is aan 5\,€.  Wat is de variantie van variabele $X_2$?
    
**c.** De variabele $Y$ is gedefinieerd als de som van $X_1$ en $X_2$, dus $Y = X_1 + X_2$. Wat is de variantie van $Y$?

**d.** Zie je waarom je de varianties van $X_1$ en $X_2$ in dit geval niet mag optellen?

:::

:::

::: [.hidden]

### Optellen van kansvariabelen

Als $Y$ de optelsom is van $n$ andere variabelen $X_i$, dan geldt:
  $$
  \ev{Y} = \sum_{i = 1}^n \ev{X_i}.
$$
  Oftewel: het gemiddelde van de som is de som van de gemiddelden.

Als de $X_i$ onafhankelijk zijn, geldt ook:
  $$
  \var{Y} = \var{\sum_{i = 1}^n X_i} = \sum_{i = 1}^n \var{X_i}.
$$
  Oftewel: de variantie van de som is de som van de varianties.
Als de $X_i$ niet onafhankelijk zijn, geldt deze regel niet!
  
:::
