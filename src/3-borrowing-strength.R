
# Imports ---------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(gridExtra)
theme_set(theme_classic() + theme(text=element_text(size=14)))
high_red <- "#DE490B"
uc_blue  <- "#003262"


# Plots -----------------------------------------------------------------------------
a <- seq(-150, 150, len = 150)
mu_c <- 10;  mu_s <- 20; mu_r <- 30; mu_g <- 50
sd_c <- 25; sd_s <- 20; sd_r <- 10; sd_g <- 5

d1 <- data.frame(a=a, den=dnorm(x=a, mean=mu_c, sd=sd_c))
d2 <- data.frame(a=a, den=dnorm(x=a, mean=mu_s, sd=sd_s))
d3 <- data.frame(a=a, den=dnorm(x=a, mean=mu_r, sd=sd_r))
d4 <- data.frame(a=a, den=dnorm(x=a, mean=mu_g, sd=sd_g))
d5 <- data.frame(sam= rnorm(n=2000, mean=mu_c, sd=sd_c) + # cheating a bit here
                      rnorm(n=2000, mean=mu_s, sd=sd_s) +
                      rnorm(n=2000, mean=mu_r, sd=sd_r)+
                      rnorm(n=2000, mean=mu_g, sd=sd_g))
max_den <- max(c(d1$den, d2$den, d3$den, d4$den))
min_blood <- min(c(d1$a, d2$a, d3$a, d4$a))
max_blood <- max(c(d1$a, d2$a, d3$a, d4$a))

b1 <- ggplot(data=d1, aes(x=a, y=den)) +
    geom_line(color=uc_blue) +
    geom_area(alpha=.5,color=uc_blue, fill=uc_blue) +
    labs(y="Density", x=quote(a[Kenya]),
         title=quote("Prior: "*a[Kenya])) +
    scale_x_continuous(expand=c(0,0), limits=c(min_blood, max_blood)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max_den))

b2 <- ggplot(data=d2, aes(x=a, y=den)) +
  geom_line(color=uc_blue) +
  geom_area(alpha=.5,color=uc_blue, fill=uc_blue) +
  labs(y="Density", x=quote(a[WestAfrica]),
       title=quote("Posterior: "*a[WestAfrica])) +
  scale_x_continuous(expand=c(0,0), limits=c(min_blood, max_blood)) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max_den))

b3 <- ggplot(data=d3, aes(x=a, y=den)) +
  geom_line(color=uc_blue) +
  geom_area(alpha=.5,color=uc_blue, fill=uc_blue) +
  labs(y="Density", x=quote(a[Africa]),
       title=quote("Posterior: "*a[Africa])) +
  scale_x_continuous(expand=c(0,0), limits=c(min_blood, max_blood)) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max_den))

b4 <- ggplot(data=d4, aes(x=a, y=den)) +
  geom_line(color=uc_blue) +
  geom_area(alpha=.5,color=uc_blue, fill=uc_blue) +
  labs(y="Density", x=quote(a[global]),
       title=quote("Posterior: "*a[global])) +
  scale_x_continuous(expand=c(0,0), limits=c(min_blood, max_blood)) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max_den))

b5 <- ggplot(data=d5, aes(x=sam)) +
  geom_density(alpha=.5,color=uc_blue, fill=uc_blue) +
    labs(y="Density", x=quote(a[Kenya]),
         title=quote("Posterior: "*a[Kenya])) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max_den))

grid2 <- grid.arrange(b1,b2,b3,b4, b5, layout_matrix=rbind(c(1,2,3,4), c(6,5,6,6)))

ggsave(file="out/figs/hier1.pdf", grid2, width=12)
