slaap <- data.frame(ID = as.factor(seq(8)),
                    MWT_d0 = c(20, 13, 17, 14, 20, 17, 20, 14),
                    MWT_d1 = c(3.5, 8.7, 10.8, 13.6, 14, 13, 11.9, 12.2),
                    MWT_d2 = c(5.5, 7.6, 6.4, 10.4, 13, 11, 17.3, 7.5),
                    MWT_d3 = c(5.3, 5.9, 5.8, 12.6, 13.3, 8, 9, 7.1),
                    MWT_d4 = c(4.7, 7.9, 7, 17.8, 8.9, 8.5, 8.5, 7.4),
                    MWT_d5 = c(3.7, 3.2, 5, 12.4, 8, 10.8, 16, 10.8),
                    MWT_d6 = c(4.5, 1.9, 3.7, 6.2, 10.3, 8.2, 5.14, 8.2),
                    MWT_d7 = c(1.7, 5.5, 4.7, 3.6, 10.3, 6.8, 8.5, 5.2))

library("ggplot2")
library("tidyverse")

slaap_lang <- pivot_longer(slaap, 
                           cols = starts_with("MWT"),
                           names_to = "dag",
                           names_prefix = "MWT_d", 
                           values_to = "MWT")

ggplot(slaap_lang, aes(x = dag, y = MWT, color = ID)) +
  geom_point() +
  geom_line(aes(group = ID), lty = 2) +
  theme_minimal()

summary(slaap$MWT_d0)
summary(slaap$MWT_d6)

sd(slaap$MWT_d0)
sd(slaap$MWT_d6)

qqnorm(slaap$MWT_d0 - slaap$MWT_d6)
qqline(slaap$MWT_d0 - slaap$MWT_d6)

t.test(slaap$MWT_d0, slaap$MWT_d7, paired = TRUE)
t.test(slaap$MWT_d0, slaap$MWT_d4, paired = TRUE)

dagen_geselecteerd <- slaap_lang[slaap_lang$dag == 0 | slaap_lang$dag == 6, ]

ggplot(dagen_geselecteerd, aes(x = dag, y = MWT, color = ID)) +
  geom_point() +
  geom_line(aes(group = ID), lt = 2) +
  theme_minimal()

bereken_verschillen <- function(data) {
  data %>%
    pivot_wider(names_from = dag, values_from = MWT) %>%
    mutate(verschil = `6` - `0`) %>%
    select(ID, verschil)
}
verschillen <- bereken_verschillen(dagen_geselecteerd)
t.test(verschillen$verschil)

qqnorm(verschillen$verschil)
qqline(verschillen$verschil)
#vervelende outlier... in het verschil tussen dag 0 en 7; misschien 0 en 6 gebruiken?

t.test(slaap$MWT_d6[slaap$groep == 1], slaap$MWT_d6[slaap$groep == 2], var.equal = T)
sd(slaap$MWT_d6[slaap$groep == 1])
sd(slaap$MWT_d6[slaap$groep == 2])
