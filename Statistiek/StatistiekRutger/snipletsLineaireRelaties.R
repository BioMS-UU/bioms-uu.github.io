::: {.Rexr #exr-loopsnelheid}
  ## Loopsnelheid in verschillende steden (computer practicum 2 opdracht 2)
  
  In Behaviour 123 (1992) pp. 77–83 [^Wirtz, P., & Ries, G. (1992). The pace of life-reanalysed: why does walking speed of pedestrians correlate with city size?. Behaviour, 123(1-2), 77-83.] 
  beschrijven twee onderzoekers de mogelijke relatie tussen het aantal inwoners 
  van grote steden en de gemiddelde loopsnelheid van de inwoners van die steden.
  Een aantal resultaten van hun onderzoek is vastgelegd in het bestand
  “loopsnelheid.csv”.
  
  a. Download het bestand, en lees het in als een dataframe. Bestudeer
  de dataframe.
  b. Visualiseer, beschrijf en bestudeer de gegevens.
  c. Maak daarbij ook een spreidingsdiagram van de gemiddelde loopsnelheid
  (av.wlksp) versus de logaritme van het aantal inwoners
  van de stad (popsize). (Voeg de logaritme van het aantal inwoners
                          als een nieuwe variabele (logpops) aan de dataframe toe.)
  Beschrijf de samenhang tussen deze twee variabelen.
  d. Bereken de correlatiecoëfficiënt bij het spreidingsdiagram uit onderdeel c). 
(Gebruik de functie `cor.test()`.)
e. Welke aannames moet je doen om te toetsen of deze correlatiecoëfficiënt significant van 0 verschilt? Is die aanname hier redelijk?
  f. Aangenomen dat aan de aannames is voldaan: Is de correlatiecoëfficiënt
statistisch significant verschillend van nul?
  g. Stel de vergelijking op voor de regressielijn die de relatie tussen de twee variabelen beschrijft. (Gebruik lm() en summary().) Wat zijn de aannames van de regressie-analyse, en zijn die hier redelijk?
  h. Is er sprake van een significante regressie en waar leid je dit uit af?
  i. Had je de correlatiecoëfficiënt ook uit de uitvoer van de regressieanalyse kunnen aflezen?
  j. Welke methode (correlatie of regressie) heeft jouw voorkeur in het beantwoorden van de onderzoeksvraag? Beargumenteer je antwoord.
:::
