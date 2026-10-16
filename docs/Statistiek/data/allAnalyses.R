fireflies <- read.table("~/Teaching/ExperimentFilosofieEnStatistiek/2019/R/fireflies.csv", quote="\"", comment.char="")
colnames(fireflies) <- "duration"
str(fireflies)
nrow(fireflies)
summary(fireflies$duration)
sd(fireflies$duration)
res <- t.test(fireflies$duration)
sqrt(var(fireflies$duration))
sd(fireflies$duration)
t.test(fireflies$duration)$conf.int


cholesterol <- c(1.4, 1.8, 2.1, 2.7, 3.0 )
mean(cholesterol)
var(cholesterol)
sd(cholesterol)
sd(cholesterol)/sqrt(5)
t.test(cholesterol)

bloeddruk<-c(94,94,82,100,112,110,84,78,92,112,94,92,86,84,90,72,88,92,88,84,98,98,84,90,90,70,80,90,80,106,74,95,100,94,84,70,102,92,84,80,84,86,98,82,80,88,80,84,100,86)
t.test(bloeddruk)


set.seed(1)
gewichtControle <- rnorm(20, mean = 67,  sd = 2.695)
gewichtLevertraan <- rnorm(10, mean = 70, sd = 2.357)
kuikens <- data.frame(gewicht = c(gewichtControle, gewichtLevertraan), olie = factor(c(rep("controle", 20), rep("levertraan", 10))))
library("car")
str(kuikens)
t.test(gewicht ~ olie, data = kuikens, var.equal = TRUE)
by(kuikens$gewicht, kuikens$olie, sd)
sd(kuikens$gewicht[kuikens$olie == "controle"])
sd(kuikens$gewicht[kuikens$olie == "levertraan"])
leveneTest(gewicht ~ olie, data = kuikens)
t.test(gewicht ~ olie, data = kuikens, var.equal = FALSE)

randO <- rnorm(9, mean = 5.3622, sd = .35330)
randA <- rnorm(9, mean = 5.5522, sd = .35818)
zuurgraadOchtend <- c(4.89, 5.28, 5.24, 5.27, 4.92, 5.35, 4.76, 5.72, 4.62)
zuurgraadAvond <- c(5.29, 5.51, 5.75, 5.62, 5.42, 6.09, 4.92, 6.01, 5.34)
t.test(zuurgraadOchtend, zuurgraadAvond, paired = TRUE)

set.seed(3)
DDTresistent <- rnorm(25, mean = 32.68, sd = 8.72124)
DDTgevoelig <-  rnorm(25, mean = 25.32, sd = 8.5)#6.92050)
controle <- rnorm(25, mean = 23.00, sd = 9.53502)


drosophila <- data.frame(
  eitjes = c(DDTresistent, DDTgevoelig, controle),
  groep = factor(c(rep("resistent", 25), rep("gevoelig", 25), rep("controle", 25)))
)
str(drosophila)
by(drosophila$eitjes, drosophila$groep, summary)
by(drosophila$eitjes, drosophila$groep, sd)
pdf("dros_boxplots.pdf",width=4,height=4,paper='special')
boxplot(eitjes ~ groep, data = drosophila)
dev.off()
library("car")
leveneTest(eitjes ~ groep, data = drosophila)

qqnorm(resultaat$residuals, main = "QQ-plot van de residuen"); qqline(resultaat$residuals)
resultaat <- lm(eitjes ~ groep, data = drosophila)
pdf("dros_qqPlot_residuals.pdf",width=4,height=4,paper='special')
qqnorm(resultaat$residuals, main = "QQ-plot van de residuen"); qqline(resultaat$residuals)
dev.off()
anova(resultaat)
summary(resultaat)


# NEMATODES

set.seed(1)
nematode <- rnorm(12, mean = 11.583, sd = 3.4696)
parasitaire.wespen <- rnorm(12, mean = 9.625, sd = 2.4227)
nematoden.en.parasitaire.wespen <- rnorm(12, mean = 10.333, sd = 1.9579)
bacterien <- rnorm(12, mean = 11.125, sd = 3.1198)
controle <- rnorm(12, mean = 12.292, sd = 2.8720)

opbrengst <- c(
  nematode,
  parasitaire.wespen,
  nematoden.en.parasitaire.wespen,
  bacterien,
  controle
)

behandeling <- factor(c(
  rep("nem", 12),
  rep("wesp", 12),
  rep("nem en wesp", 12),
  rep("bacterien", 12),
  rep("controle", 12)
), levels = c("controle", "nem", "wesp", "nem en wesp", "bacterien"))

mais <- data.frame(
  opbrengst,
  behandeling
)
str(mais)
by(mais$opbrengst, mais$behandeling, mean)
by(mais$opbrengst, mais$behandeling, sd)
pdf("mais_boxplots.pdf",width=8,height=4,paper='special')
boxplot(opbrengst ~ behandeling, data = mais)
dev.off()
resultaat <- lm(opbrengst ~ behandeling, data = mais)
pdf("mais_QQnormOpbrengst.pdf",width=4,height=4,paper='special')
qqnorm(mais$opbrengst); qqline(mais$opbrengst)
dev.off()

pdf("mais_QQnormResiduals.pdf",width=4,height=4,paper='special')
qqnorm(resultaat$residuals, main = "QQ-plot van de residuen"); qqline(resultaat$residuals)
dev.off()

pdf("mais_diagnostiek.pdf",width=4,height=4,paper='special')
plot(resultaat)
dev.off()

library("car")
leveneTest(opbrengst ~ behandeling, data = mais)

anova(resultaat)
summary(resultaat)

# Start of schildpadden

man <- c(220.1, 218.6, 229.6, 228.8, 222.0, 224.1, 226.5)
vrouw <- c(223.4, 221.5, 230.2, 224.3, 223.8, 230.8)

nman <- length(man)
nvrouw <- length(vrouw)

schildpadden <- data.frame(
  cholesterol = c(man, vrouw),
  sex = c(
    rep("man", nman),
    rep("vrouw", nvrouw)
  )
)

str(schildpadden)

by(schildpadden$cholesterol, schildpadden$sex, summary)
by(schildpadden$cholesterol, schildpadden$sex, sd)

pdf("schildpadden_boxplots.pdf",width=4,height=4,paper='special')
boxplot(cholesterol ~ sex, data = schildpadden)
dev.off()

t.test(cholesterol ~ sex, data = schildpadden, var.equal = TRUE)

wilcox.test(cholesterol ~ sex, data = schildpadden)

# haften

oever <- c(8, 0, 3, 23, 19, 2, 48, 7, 20, 15)
midden <- c(0, 6, 8, 4, 5, 1, 23, 11, 8, 6)
rivier <- factor(c(seq(1, 10,1), seq(1, 10, 1)))
positie <- factor(c(rep("oever", 10), rep("midden", 10)))

nimfen <- data.frame(
  aantal = c(oever, midden),
  rivier = rivier,
  positie = positie
)

str(nimfen)

by(nimfen$aantal, nimfen$positie, summary)

pdf("nimfen_boxplots_posities.pdf", width = 4, height = 4, paper = 'special')
boxplot(aantal ~ positie, data = nimfen)
dev.off()

verschillen <- nimfen$aantal[nimfen$positie == "oever"] - nimfen$aantal[nimfen$positie == "midden"]
pdf("nimfen_boxplot_verschillen.pdf", width=4,height=4,paper='special')
boxplot(
  verschillen, main = "Boxplot van verschillen")
dev.off()

pdf("nimfen_qqPlot_verschillen.pdf", width=4,height=4,paper='special')
qqnorm(verschillen, main = "QQ-plot van de verschillen"); qqline(verschillen)
dev.off()

summary(verschillen)
sd(verschillen)

t.test(aantal ~ positie, data = nimfen, var.equal = TRUE)
t.test(
  nimfen$aantal[nimfen$positie == "oever"], nimfen$aantal[nimfen$positie == "midden"],
  paired = TRUE
  )

wilcox.test(aantal ~ positie, data = nimfen)


t.test(aantal ~ positie, data = nimfen, paired = TRUE)

wilcox.test(aantal ~ positie, data = nimfen, paired = TRUE)

(n <- length(verschillen))
(x <- sum(verschillen > 0))
binom.test(x, n)

# ecotypes

set.seed(1)
loc1 <- exp(rnorm(10, mean = 0.2, sd = .4))
loc2 <- exp(rnorm(10, mean = .6, sd = .4))
loc3 <- exp(rnorm(10, mean = 0, sd = .4))
loc4 <- exp(rnorm(10, mean = -0.4, sd = .4))

ratio <- c(loc1, loc2, loc3, loc4)
locatie <- factor(
  c(
  rep(1, 10),
  rep(2, 10),
  rep(3, 10),
  rep(4, 10)
  )
)

ecotypes <- data.frame(
  ratio,
  locatie
)

str(ecotypes)

by(ecotypes$ratio, ecotypes$locatie, summary)

by(ecotypes$ratio, ecotypes$locatie, sd)

pdf("ecotypes_boxplots.pdf", width=4,height=4,paper='special')
boxplot(ratio ~ locatie, data = ecotypes)
dev.off()

result <- lm(ratio ~ locatie, data = ecotypes)

qqnorm(result$residuals); qqline(result$residuals)

library("car")
leveneTest(ratio ~locatie, data = ecotypes)

# kieuwen
# invent fake data

lichaamsgewicht <- c(1.8, 2.5, 4.5, 9, 11, 14, 14.5, 15, 15.5, 16,  17.5)
kieuwgewicht <- c(100, 50, 90, 210, 100, 150, 230, 175, 220, 320, 210)
str(lichaamsgewicht)
str(kieuwgewicht)

pdf("kieuwen_scatter.pdf", width=4,height=4,paper='special')
plot(lichaamsgewicht, kieuwgewicht)
dev.off()

cor.test(lichaamsgewicht, kieuwgewicht)

krabben <- data.frame(lichaamsgewicht, kieuwgewicht)
cor.test( ~ kieuwgewicht + lichaamsgewicht, data = krabben)

# vitamine

vitamineniveau <- c(3, 5, 6, 8, 11, 16, 20)
toenamegewicht <- c(13, 25, 25, 30, 40, 35, 40)
vitamines <- data.frame(vitamineniveau, toenamegewicht)
str(vitamines)

pdf("vitamines_scatter.pdf",width=4,height=4,paper='special')
plot(vitamines)
dev.off()

resultaat <- lm(toenamegewicht ~ vitamineniveau, data = vitamines)
anova(resultaat)
summary(resultaat)

# absorptie

set.seed(2)
concentratie <- seq(5, 50, 5)
absorpsie <- 0.015*concentratie + rnorm(10, mean = 0, sd = sqrt(.0034/8))
UV <- data.frame(concentratie, absorpsie)
str(UV)

pdf("absorbtie_scatter.pdf",width=4,height=4,paper='special')
plot(UV)
dev.off()
resultaat <- lm(absorpsie ~ concentratie, data = UV)

pdf("absorbtie_QQresiduals.pdf",width=4,height=4,paper='special')
qqnorm(resultaat$residuals); qqline(resultaat$residuals)
dev.off()

anova(resultaat)
summary(resultaat)
pdf("absorbtie_diagnostics.pdf",width=3,height=3,paper='special')
plot(resultaat)
dev.off()


# muizen

set.seed(1)
voor <- rnorm(25, mean = 37, sd = 1)
na <- voor + rnorm(25, mean = 0.3, sd = 1)
temperatuur <- c(voor, na)
moment <- factor(c(rep("voor", 25), rep("na", 25)), levels = c("voor", "na"))
muizen <- data.frame(temperatuur, moment)
str(muizen)
by(muizen$temperatuur, muizen$moment, summary)
by(muizen$temperatuur, muizen$moment, sd)
t.test(temperatuur ~ moment, data = muizen, paired = TRUE)
t.test(temperatuur ~ moment, data = muizen, var.equal = TRUE)

# protoplasten

tijd0 <- c(3.0, 3.6, 2.5, 2.0, 2.6, 2.8)
tijd1 <- c(3.3, 3.1, 2.7, 2.6, 3.9, 2.8)
tijd2 <- c(2.9, 3.5, 4.1, 3.1, 3.0, 3.2)
tijd4 <- c(4.0, 4.4, 3.4, 3.8, 4.6, 3.9)
tijd8 <- c(4.5, 4.8, 4.7, 4.2, 3.8, 4.3)

activiteit <- c(tijd0, tijd1, tijd2, tijd4, tijd8)
tijdstip <- c(rep(0, 6), rep(1, 6), rep(2, 6), rep(4, 6), rep(8, 6))

protoplasten <- data.frame(tijdstip, activiteit)

resultaat <- lm(activiteit ~ tijdstip, data = protoplasten)
anova(resultaat)
summary(resultaat)

pdf("protoplasten_scatter.pdf",width=3,height=3,paper='special')
plot(protoplasten)
dev.off()
summary(protoplasten)
by(protoplasten$activiteit, protoplasten$tijd, sd)
sd(protoplasten$activiteit)
sd(protoplasten$tijd)
pdf("protoplasten_boxplot_residuals.pdf",width=3,height=3,paper='special')
boxplot(resultaat$residuals, main = "Boxplot van de residuen")
dev.off()
pdf("protoplasten_QQnorm_tijdstip.pdf",width=3,height=3,paper='special')
qqnorm(protoplasten$tijdstip); qqline(protoplasten$tijdstip)
dev.off()
pdf("protoplasten_QQnorm_activiteit.pdf",width=3,height=3,paper='special')
qqnorm(protoplasten$activiteit); qqline(protoplasten$activiteit)
dev.off()
pdf("protoplasten_diagnostics.pdf",width=3,height=3,paper='special')
plot(resultaat)
dev.off()
leveneTest(activiteit ~ factor(tijdstip), data = protoplasten)

# bloeddruk
library("readr")
bloeddruk <- read.csv("../R/bloeddruk.csv")

str(bloeddruk)
summary(bloeddruk)
sd(bloeddruk$"meting 1")
sd(bloeddruk$"meting 2", na.rm = TRUE)
sd(bloeddruk$verschil, na.rm = TRUE)
