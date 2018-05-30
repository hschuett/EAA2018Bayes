# (C) Harm Schuett 2018, schuett@bwl.lmu.de, see LICENSE file for details
# NOTE: if you don't use a project, you need to setwd() to set the working directory
# Imports ---------------------------------------------------------------------------
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
theme_set(theme_classic())
nudge_x <- 0.3
high_red <- "#DE490B"
uc_blue <- "#003262"


# Load Data -------------------------------------------------------------------------
sample1 <- read_csv("out/first-sample.csv")

input_data <- list(y=sample1$y,
                   x=sample1$x,
                   N=nrow(sample1))

# Very weak Regularization ----------------------------------------------------------
very_weak_regul<- "
data{
  int<lower=1> N;
  real y[N];
  real x[N];
}
parameters{
  real a0;
  real a1;
  real<lower=0> sigma;
}
model{
  vector[N] mu;
  sigma ~ cauchy( 0 , 2 );
  a0 ~ normal( 0 , 10 );
  a1 ~ normal( 0 , 10 );
  for ( i in 1:N ) {
    mu[i] = a0 + a1 * x[i];
  }
  y ~ normal( mu , sigma );
}
"
fit <- stan(model_code=very_weak_regul, data=input_data,
             iter=5000, warmup=2000, chains=2,
             verbose=FALSE)

pars <- c("sigma", "a0", "a1")
print(fit, pars=pars)
traceplot(fit, pars=pars)
pairs(fit, pars=pars)

posterior_draws <- as_data_frame(as.matrix(fit))

x_length = seq(-50, 50, 1)
x_length2 = seq(0, 100, 1)
priors = data.frame(a = dnorm(x=x_length, mean=0, sd=10),
                    sigma = dcauchy(x=x_length2), location=0, scale=20,
                    x1 = x_length,
                    x2 = x_length2)

br1 <- ggplot(data=posterior_draws, aes(x=a0, y=a1)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") +
  geom_point(x=1, y=2, color=high_red, size=2) +
  annotate("text", x=1+nudge_x, y=2, label="(1, 2)", hjust=0, color=high_red) +
  geom_point(x=0.62, y=4.16, color=high_red, size=2) +
  annotate("text", x=0.62+nudge_x, y=4.16, label="Posterior Mode (0.62, 4.16)", hjust=0, color=high_red) +
  scale_x_continuous(expand=c(0, 0), limits=c(-10, 15)) +
  scale_y_continuous(expand=c(0, 0), limits=c(-6, 15)) +
  theme(legend.position='none') +
  labs(x=expression(a[0]),
       y=expression(a[1]),
       title=quote("Posterior: P("*a[0]*","*a[1]*" |y, x)"))
br2 <- ggplot(data=priors, aes(x=x1, y=a)) +
  geom_area(color=uc_blue, fill=uc_blue, alpha=0.5) +
  scale_y_continuous(expand=c(0, 0)) +
  labs(y="Density",
       x=expression(a[k]),
       title=quote("Prior: P("*a[k]*") = N(0, 10"))
br3 <- ggplot(data=priors, aes(x=x2, y=sigma)) +
  geom_area(color=uc_blue, fill=uc_blue, alpha=0.5) +
  scale_y_continuous(expand=c(0, 0)) +
  labs(y="Density",
       x=expression(sigma),
       title=quote("Prior: P("*sigma*") = Chauchy(0, 20)"))

br_grid1 <- gridExtra::grid.arrange(br1, br2, br3, layout_matrix=rbind(c(1,1,2), c(1,1,3)))
ggsave(plot=br_grid1, file="out/figs/br1.pdf", width=12)

# Weak Regularization ---------------------------------------------------------------
weak_regul<- "
data{
int<lower=1> N;
real y[N];
real x[N];
}
parameters{
real a0;
real a1;
real<lower=0> sigma;
}
model{
vector[N] mu;
sigma ~ cauchy( 0 , 20 );
a0 ~ normal( 0 , 2 );
a1 ~ normal( 0 , 2 );
for ( i in 1:N ) {
mu[i] = a0 + a1 * x[i];
}
y ~ normal( mu , sigma );
}
"
fit2 <- stan(model_code=weak_regul, data=input_data,
            iter=5000, warmup=2000, chains=2,
            verbose=FALSE)

pars <- c("sigma", "a0", "a1")
print(fit2, pars=pars)
traceplot(fit2, pars=pars)
pairs(fit2, pars=pars)

posterior_draws2 <- as_data_frame(as.matrix(fit2))

x_length = seq(-50, 50, 1)
x_length2 = seq(0, 100, 1)
priors2 = data.frame(a = dnorm(x=x_length, mean=0, sd=2),
                    sigma = dcauchy(x=x_length2), location=0, scale=20,
                    x1 = x_length,
                    x2 = x_length2)

br4 <- ggplot(data=posterior_draws2, aes(x=a0, y=a1)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") +
  geom_point(x=1, y=2, color=high_red, size=2) +
  annotate("text", x=1+nudge_x, y=2, label="(1, 2)", hjust=0, color=high_red) +
  geom_point(x=0.22, y=1.59 , color=high_red, size=2) +
  annotate("text", x=0.22+nudge_x, y=1.59 , label="Posterior Mode (0.22, 1.59 )", hjust=0, color=high_red) +
  scale_x_continuous(expand=c(0, 0), limits=c(-10, 15)) +
  scale_y_continuous(expand=c(0, 0), limits=c(-6, 15)) +
  theme(legend.position='none') +
  labs(x=expression(a[0]),
       y=expression(a[1]),
       title=quote("Posterior: P("*a[0]*","*a[1]*" |y, x)"))
br5 <- ggplot(data=priors2, aes(x=x1, y=a)) +
  geom_area(color=uc_blue, fill=uc_blue, alpha=0.5) +
  scale_y_continuous(expand=c(0, 0)) +
  labs(y="Density",
       x=expression(a[k]),
       title=quote("Prior: P("*a[k]*") = N(0, 2)"))
br6 <- ggplot(data=priors2, aes(x=x2, y=sigma)) +
  geom_area(color=uc_blue, fill=uc_blue, alpha=0.5) +
  scale_y_continuous(expand=c(0, 0)) +
  labs(y="Density",
       x=expression(sigma),
       title=quote("Prior: P("*sigma*") = Chauchy(0, 20)" ))

br_grid2 <- gridExtra::grid.arrange(br4, br5, br6, layout_matrix=rbind(c(1,1,2), c(1,1,3)))
ggsave(plot=br_grid2, file="out/figs/br2.pdf", width=12)



#----------
# library(ggplot2)
# df <- data.frame(x = rnorm(1000, 50, 10), y = rnorm(1000, 50, 10))
# p <- ggplot(df, aes(x, y)) + geom_point() + theme_classic()
# ggExtra::ggMarginal(p, type = "histogram")
# geom_area(aes(x=theta, y=prior, fill=Prior, color=Prior, group=Prior), alpha=.5, position = "identity")
