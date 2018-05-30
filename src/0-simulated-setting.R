# (C) Harm Schuett 2018, schuett@bwl.lmu.de, see LICENSE file for details
# NOTE: if you don't use a project, you need to setwd() to set the working directory
# Imports ---------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(gridExtra)
theme_set(theme_classic() + theme(text=element_text(size=14)))
high_red <- "#DE490B"
uc_blue  <- "#003262"

# Function for extracting legends from a grob
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


# Simulated data --------------------------------------------------------------------
set.seed(888)
n_samples <- 50
n_obs     <- 50
x         <- rnorm(n=n_obs, 0, 1)
dat1      <- list()
for (i in 1:n_samples){
  u <- rnorm(n=n_obs, 0, 20)
  dat1[[i]] <- data.frame(y=1 + 2 * x + u,
                          x=x,
                          u=u)

}
samples       <- tibble(id=1:n_samples)
samples$data  <- dat1
samples$model <- map(samples$data, function(dat) tidy(lm(y ~ x, data=dat)))
head(samples)

estimates <- samples %>%
  unnest(model) %>%
  mutate(param = if_else(term == "x", "a1", "a0")) %>%
  select(id, param, estimate) %>%
  spread(key=param, value=estimate) %>%
  mutate(plabs=paste0("(", round(a0,1), ",", round(a1,1), ")"),
         first_sample = as.factor(c(1, rep.int(0, times=n_samples-1))))


# save first sample to disk for bayes reg
write.csv(x=samples$data[[1]], file="out/first-sample.csv")



# The two intro regressions ---------------------------------------------------------
i1 <- ggplot(data=samples$data[[1]], aes(x=x, y=y)) +
  geom_point(color=uc_blue) +
  geom_smooth(method="lm", color=high_red) +
  scale_x_continuous(expand=c(0.01, 0)) +
  ylim(-50, 50) +
  labs(title=expression(y[1] == 1 + 2 %*% x + u[1]))

i2 <- ggplot(data=samples$data[[2]], aes(x=x, y=y)) +
  geom_point(color=uc_blue) +
  geom_smooth(method="lm", color=high_red) +
  scale_x_continuous(expand=c(0.01, 0)) +
  ylim(-50, 50) +
  labs(title=expression(y[2] == 1 + 2 %*% x + u[2]))

grid1 <- grid.arrange(i1, i2, nrow=1)
ggsave(filename="out/figs/i1.pdf", plot=grid1, width=12)



# Plots -----------------------------------------------------------------------------
min_x   <- min(estimates$a0) - 0.1
max_x   <- max(estimates$a0) + 0.1
min_y   <- min(estimates$a1) - 0.1
max_y   <- max(estimates$a1) + 0.1
nudge_x <- 0.1
a0 <- estimates$a0[1]
a1 <- estimates$a1[1]

(baseplot1 <- ggplot(estimates %>% slice(1), aes(x=a0, y=a1)) +
  geom_point(color="black", size=2) +
  xlim(min_x, max_x) + ylim(min_y, max_y) +
  labs(x=expression("Intercept estimate " ~ hat(a)[0]),
       y=expression("Slope estimate " ~ hat(a)[1])))
(p1 <- baseplot1 )
(p2 <- baseplot1 +
  geom_point(x=0, y=0, color=high_red, size=2) +
  annotate("text", x=0+nudge_x, y=0, label="H0 (0, 0)", hjust=0, color=high_red))


baseplot2 <- ggplot(estimates, aes(x=a0, y=a1, color=first_sample)) +
  geom_point(size=2) +
  xlim(min_x, max_x) + ylim(min_y, max_y) +
  scale_color_manual(values=c("gray70", "black")) +
  labs(x=expression(" Intercept estimate " ~ hat(a)[0]),
       y=expression(" Slope estimate " ~ hat(a)[1])) +
  theme(legend.position = "none")

(p3 <- baseplot2 +
  geom_point(x=0, y=0, color=high_red, size=2) +
  annotate("text", x=0.07, y=0, label="H0 (0, 0)", hjust=0, color=high_red))

(p4 <- baseplot2 +
  geom_segment(aes(color=first_sample), xend=0, yend=0, linetype=2, alpha=0.5) +
  geom_point(x=0, y=0, color=high_red, size=2) +
  annotate("text", x=0.07, y=0, label="H0 (0, 0)", hjust=0, color=high_red))

xc <- 0
yc <- 0
r <- sd(estimates$a1)
(p5 <- baseplot2 +
    geom_segment(aes(color=first_sample), xend=0, yend=0, linetype=2, alpha=0.5) +
    geom_point(x=0, y=0, color="red", size=2) +
    annotate("text", x=0.07, y=0, label="H0 (0, 0)", hjust=0, color=high_red) +
    geom_hline(yintercept = c(r, -r), color=high_red) +
    geom_segment(x=-4, xend=-4, y=-r, yend=r, color=high_red,
                 arrow=arrow(length=unit(0.15, "inches"), ends="both", type="closed")) +
    annotate("text", x=-3.4, y=-2.2,
             size=5, hjust=0, vjust=0, color=high_red,
             label="68% of a normal distribution within one standard deviation radius")
  )


t_dist <- data.frame(vals = seq(-5, 5, len = 100),
                     ps   = dt(seq(-5, 5, len = 100), df=n_obs-2))
t_dist <- data.frame(ps = rt(n=10000, df=n_obs-2))
test_stat <- 1.65
quant_test <- round(1 - pt(test_stat, df=n_obs-2), 2)
(p6 <- ggplot(data=t_dist, aes(x=ps)) +
    geom_histogram(bins=50, color="white", fill=uc_blue) +
    geom_segment(color=high_red,
                 x=test_stat, y=0,
                 xend=test_stat, yend=200) +
    geom_segment(color=high_red,
                 x=-test_stat, y=0,
                 xend=-test_stat, yend=200) +
    # geom_area(color=high_red, fill=high_red, alpha=0.5) +
    annotate("text", x=test_stat, y=200,
             size=5, hjust=0, vjust=0,
             label=paste0("P(t >= ", test_stat, " | H0) = ", quant_test)) +
    annotate("text", x=-test_stat, y=200,
             size=5, hjust=1, vjust=0,
             label=paste0("P(t <= ", test_stat, " | H0) = ", quant_test)) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x=expression((hat(a)-H0)/SE),
         y="Density of occurrence in repeated sampling") +
    theme(legend.position = "none"))


(p7 <- baseplot2 +
  geom_segment(aes(color=first_sample), xend=1, yend=2, linetype=2, alpha=0.5) +
  geom_point(x=1, y=2, color=uc_blue, size=2) +
  annotate("text", x=1.07, y=2, label="Truth (1, 2)", hjust=0, color=uc_blue) +
  geom_point(x=0, y=0, color=high_red, size=2) +
  annotate("text", x=0.07, y=0, label="H0 (0, 0)", hjust=0, color=high_red) +
  theme(legend.position = "none")
  )


# Saving plots
ggsave(filename="out/figs/p1.pdf", plot=p1)
ggsave(filename="out/figs/p2.pdf", plot=p2)
ggsave(filename="out/figs/p3.pdf", plot=p3)
ggsave(filename="out/figs/p4.pdf", plot=p4)
ggsave(filename="out/figs/p5.pdf", plot=p5)
ggsave(filename="out/figs/p6.pdf", plot=p6)
ggsave(filename="out/figs/p7.pdf", plot=p7)



# Visualizing Posterior -------------------------------------------------------------
beta <- seq(-15, 15, len = 100)
den_beta <- dnorm(x=beta, mean=2, sd=3)
(b1 <- ggplot() +
    geom_line(aes(x=beta, y=den_beta), color=uc_blue) +
    geom_area(aes(x=beta, y=den_beta), alpha=.5,color=uc_blue, fill=uc_blue) +
    geom_vline(xintercept = 2, color=high_red) +
    labs(y="Density",
         x=quote(beta),
         title=quote("Posterior: P("*beta*"|y, x, "*sigma*")")) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)))

ggsave(filename="out/figs/b1.pdf", plot=b1)



# Bayesian Updating Figure ----------------------------------------------------------
n <- 15
y<- 8
theta <- seq(0, 1, len = 100)
likelihood <- dbinom(y, n, theta)
# first example: uniform prior
alpha <- 1; beta <- 1
pri1<- dbeta(theta, alpha, beta)
post1 <- dbeta(theta, alpha+y, beta+n-y)

# second example: informative prior
alpha <- .2; beta <- 5
pri2 <- dbeta(theta, alpha, beta)
post2 <- dbeta(theta, alpha+y, beta+n-y)

baydata2 <- rbind(data.frame(Prior="Uninformative (a=1, b=1)", prior=pri1, posterior=post1, theta=theta),
                  data.frame(Prior="Pessimistic (a=.2, b=5)", prior=pri2, posterior=post2, theta=theta)) %>%
  arrange(Prior, theta)


# fill_colors <- c("#EB811B", "#393939")
fill_colors <- c("grey60", "grey30")
p7 <- ggplot(data=baydata2) +
  geom_area(aes(x=theta, y=prior, fill=Prior, color=Prior, group=Prior), alpha=.5, position = "identity") +
  labs(y="Density priors",
       x=quote(theta),
       title=quote("Prior: Beta("*theta*"|a, b)")) +
  scale_fill_manual(values=fill_colors) +
  scale_color_manual(values=fill_colors) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  guides(fill=guide_legend(ncol=2))
p8 <- ggplot() +
  geom_line(aes(x=theta, y=likelihood), color=uc_blue) +
  geom_area(aes(x=theta, y=likelihood), alpha=.5,color=uc_blue, fill=uc_blue) +
  labs(y="Likelihood (Info in the data)",
       x=quote(theta),
       title=quote("Likel.: Binom(y=8|n=15, "*theta*")")) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))
p9 <- ggplot(data=baydata2) +
  geom_area(aes(x=theta, y=posterior, fill=Prior, color=Prior, group=Prior), alpha=.5, position = "identity") +
  labs(y="Density posterior",
       x=quote(theta),
       title=quote("Posterior: Beta("*theta*"|a+y, b+n-y)")) +
  scale_fill_manual(values=fill_colors) +
  scale_color_manual(values=fill_colors) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))
mylegend <- g_legend(p7)
grid2 <- grid.arrange(arrangeGrob(p7 + theme(legend.position="none"),
                                  p8 + theme(legend.position="none"),
                                  p9 + theme(legend.position="none"), nrow=1),
                      mylegend, nrow=2, heights=c(10, 1))

ggsave(filename="out/figs/b2.pdf", plot=grid2, width=12)
