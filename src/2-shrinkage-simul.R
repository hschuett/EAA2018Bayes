# (C) Harm Schuett 2018, schuett@bwl.lmu.de, see LICENSE file for details
# NOTE: if you don't use a project, you need to setwd() to set the working directory
# Imports ---------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(stringr)
library(rstan)
rownames_to_column <- tibble::rownames_to_column
grid.arrange <-  gridExtra::grid.arrange
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
theme_set(theme_classic())
high_red <- "#DE490B"
uc_blue <- "#003262"


# Simulate data ---------------------------------------------------------------------
set.seed(1234)
n_groups <- 30
sd_i <- 15
sample_probs <- c(rep(0.02666, 25), rep(0.01333, 25))
obs_per_group <- sample(1:50, size=n_groups, replace=TRUE, prob=sample_probs)
group_means <- rnorm(n=obs_per_group, mean=2, sd=3)
simed_sample <- list()
groupid_sample <- list()
for (i in 1:n_groups) {
  simed_sample[[i]] <- rnorm(n=obs_per_group[i], mean=group_means[i], sd=0)
  groupid_sample[[i]] <- rep.int(i, obs_per_group[i])
}
simed_data <- data.frame(effs = unlist(simed_sample),
                         y = unlist(simed_sample)  + rnorm(n=sum(obs_per_group),
                                                           mean=0,
                                                           sd=sd_i),
                         group_id = as.factor(unlist(groupid_sample)))


# Estimate models -------------------------------------------------------------------
fe.mod   <- lm(y~group_id-1, data=simed_data)

input_data <- list(y=simed_data$y,
                   group=as.integer(simed_data$group_id),
                   I = n_groups,
                   N = nrow(simed_data))

very_weak_regul<- "
data{
  int<lower=1> N;
  int<lower=1> I;
  real y[N];
  int<lower=1> group[N];
}
parameters{
  real mu_a;
  real<lower=0> sigma;
  real<lower=0> sig_a;
  real un_a[I];
}
transformed parameters{
  real a[I];
  for ( i in 1:I) {
    a[i] = mu_a + un_a[i] * sig_a;
  }
}
model{
  vector[N] mu;
  un_a ~ normal(0, 1);
  mu_a ~ normal(0, 100);
  sig_a ~ normal( 0 , 100 );
  sigma ~ normal( 0 , 100 );
  for ( n in 1:N ) {
    mu[n] = a[group[n]];
  }
  y ~ normal( mu , sigma );
}
"
fit <- stan(model_code=very_weak_regul, data=input_data,
            iter=4000, warmup=3000, chains=2,
            verbose=FALSE)

print(fit, pars=c("mu_a", "sig_a", "sigma"))
traceplot(fit, pars=c("mu_a", "sig_a", "sigma"))
pairs(fit, pars=c("mu_a", "sig_a", "sigma"))

resultsBA <- summary(fit, pars=c("a"))$summary %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  select(coefs=rowname, ba_coefs=mean, ba_sds=sd) %>%
  mutate(coefs = str_extract(coefs, "\\d+"))

resultsFE <- data.frame(fe_coefs = fe.mod$coefficients,
                        fe_sds = summary(fe.mod)$coef[,2],
                        coefs = names(fe.mod$coefficients)) %>%
  mutate(coefs = as.character(coefs)) %>%
  mutate(coefs = str_extract(coefs, "\\d+"))

i_effects_results <- filter(resultsFE, coefs != "x") %>%
  left_join(filter(resultsBA, coefs != "b"), by="coefs") %>%
  mutate(group_means = group_means,
         group_n = obs_per_group)


# Plot ------------------------------------------------------------------------------
highest_sd <- max(c(i_effects_results$fe_sds, i_effects_results$ba_sds))
ymax <- max(c(i_effects_results$fe_coefs, i_effects_results$ba_coefs)) + 0.5*highest_sd
ymin <- min(c(i_effects_results$fe_coefs, i_effects_results$ba_coefs)) - 0.5*highest_sd
base_plot <- ggplot(data=i_effects_results, aes(x=group_n)) +
  labs(x="Group size",
       y=expression(hat(a)[i] ~ " vs. true " ~ a[i])) +
  geom_hline(yintercept=2, color="grey70") +
  scale_y_continuous(limits = c(ymin, ymax)) +
  geom_point(aes(y=group_means), alpha=0.5, color=high_red)
p1 <- base_plot +
  geom_linerange(aes(ymin=fe_coefs - 0.5*fe_sds,
                     ymax=fe_coefs + 0.5*fe_sds),
                 color=uc_blue) +
  geom_point(aes(y=fe_coefs)) +
  labs(subtitle="Fixed effects specification")
p2 <- base_plot +
  geom_linerange(aes(ymin=ba_coefs - 0.5*ba_sds,
                     ymax=ba_coefs + 0.5*ba_sds),
                 color=uc_blue) +
  geom_point(aes(y=ba_coefs))+
  labs(subtitle="Bayesian prior regularization")
fig1 <- grid.arrange(p1, p2, nrow=2)

ggsave("out/figs/ppt-shrinkage-example2.pdf", fig1, width=5.5, height=6, dpi=150)




# Laplace Prior -----------------------------------------------------------

x_length <- seq(-20, 20, .25)
dexp <- function(x, mu=0, b=1) {
  return((1/(2 * b)) * exp(-1 * abs(x - mu)/b))
}

lasso_priors <- data.frame(x = c(x_length, x_length),
                           pdfs = c(dexp(x=x_length, mu=0, b = 1),
                                    dexp(x=x_length, mu=0, b = 4)),
                           prior = c(rep.int("tau = 1.00", length(x_length)),
                                     rep.int("tau = 0.25", length(x_length)))
                           )

(Lass1 <- ggplot(data=lasso_priors, aes(x=x, y=pdfs, fill=prior)) +
  geom_area(color=uc_blue, alpha=0.5) +
  scale_y_continuous(expand=c(0, 0)) +
  labs(y="Density",
       x=expression(beta[k]),
       title=quote("Prior: P("*beta[k]*") for different scales")))
