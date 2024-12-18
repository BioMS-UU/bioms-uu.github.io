# Laad de benodigde bibliotheken
library(ggplot2)
library(ggpubr)

# Voorbeelddata
set.seed(123)
data <- data.frame(
  Group = rep(c("Bruin", "Wit"), each = 30),
  Predatie = c(rnorm(30, mean = 40, sd = 10), rnorm(30, mean = 50, sd = 10))
)

# T-test voor significantie
t_test_result <- t.test(Predatie ~ Group, data = data)

# Maak de boxplot
plot <- ggplot(data, aes(x = Group, y = Predatie, fill = Group)) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +  # Boxplot zonder outliers
  #geom_jitter(width = 0.2, alpha = 0.4, color = "black") +  # Toevoegen van data points
  scale_fill_manual(values = c("Bruin" = "#ad734c", "Wit" = "white")) +  # Aangepaste kleuren
  labs(title = "",
       x = "Kleur konijn",
       y = "Predatie") +
  ylim(0,NA)+
  theme_minimal(base_size=15) +
  theme(legend.position = "none") +
  annotate("text", x = 1.5, y = max(data$Predatie) + 7, label = "p<0.001", size = 3.5) +
  
  geom_segment(aes(x = 1, xend = 2, y = max(data$Predatie) + 2, yend = max(data$Predatie) + 2), size = 0.5)
  #stat_compare_means(method = "t.test", label = "p.format")  # Toevoegen van significantie

# Toon de plot
print(plot)

# Toon de p-waarde
cat("P-waarde van de t-test:", t_test_result$p.value, "\n")
