
library(ggpubr)
library(patchwork)
library(NHANES)
data(NHANES)

n <- 10
#gemeten <- rnorm(n, mean = 37, sd = 0.1)
gemeten <- NHANES$Length
gemeten <- gemeten[order(gemeten)]
q_gemeten <- quantile(gemeten, probs = seq(0, 1, length.out = n), na.rm = T)
q_verwacht <- qnorm(seq(0, 1, length.out = n))
q_verwacht <- verwacht[order(verwacht)]

dat <- data.frame(q_gemeten, q_verwacht)

d_verwacht <- qnorm(seq(0, 1, length.out = 1000))

plot1 <- ggplot(dat, aes(x = q_verwacht, y = q_gemeten, sample = q_gemeten)) +
  geom_point() +
  geom_rug() +
  theme_pubr() +
  geom_segment(aes(x = q_verwacht, y = 115, yend = q_gemeten), linetype = 2) +
  geom_segment(aes(y = q_gemeten, x = q_verwacht, xend = 2.5), linetype = 2) +
  scale_x_continuous(name = "Verwachte kwantielen uit standaardnormale verdeling",
                     limits = c(-2.5, 2.5), expand = c(0, 0)) +
  scale_y_continuous(name = "Gemeten kwantielen uit data",
                     limits = c(45, 115), expand = c(0, 0))

dens_verwacht <- ggplot(as.data.frame(d_verwacht), aes(d_verwacht)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none") +
  geom_vline(data = dat, aes(xintercept = q_verwacht)) +
  scale_x_continuous(limits = c(-2.5, 2.5), expand = c(0, 0))

dens_gemeten <- ggplot(as.data.frame(gemeten), aes(x = gemeten)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none") +
  geom_vline(data = dat, aes(xintercept = q_gemeten)) +
  scale_x_continuous(limits = c(45, 115), expand = c(0, 0)) +
  coord_flip() 

dens_verwacht + plot_spacer() + plot1 + dens_gemeten + 
  plot_layout(ncol = 2, nrow = 2, widths = c(6, 1), heights = c(1, 6))


