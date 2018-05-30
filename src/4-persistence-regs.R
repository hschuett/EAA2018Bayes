# (C) Harm Schuett 2018, schuett@bwl.lmu.de, see LICENSE file for details
# NOTE: if you don't use a project, you need to setwd() to set the working directory
# Imports ---------------------------------------------------------------------------
library(tidyverse)
library(rstan)
library(gridExtra)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
theme_set(theme_classic() + theme(text=element_text(size=14)))
high_red <- "#DE490B"
uc_blue <- "#003262"



# Load Data -------------------------------------------------------------------------
dat1 <- read.csv("data/random-persistence-sample.csv") %>%
  mutate(id = as.factor(id)) %>%
  filter(abs(LeadEarn) < 2 & abs(Earn) < 2) %>%
  mutate(id2 = as.integer(factor(id)))

input_data <- list(N        = nrow(dat1),
                   N_firm   = max(dat1$id2),
                   LeadEarn = dat1$LeadEarn,
                   Earn     = dat1$Earn,
                   firm     = dat1$id2
                   )



# Simple Regression -----------------------------------------------------------------
summary(lm(LeadEarn ~ Earn -1, data=dat1))

mod1 <- lm(LeadEarn ~ id:Earn -1, data=dat1)
ols_coefs <-coef(mod1) %>%
  as.data.frame() %>%
  rownames_to_column()
colnames(ols_coefs) <- c("coef", "value")
ols_coefs <- ols_coefs %>%
  mutate(id = str_extract(coef, "id\\d*"),
         coef2 = if_else(str_detect(coef, ":Earn"), "Slope", "Inter"))
ols_slopes <- ols_coefs %>%
  select(-coef) %>%
  spread(key=coef2, value=value)

(b1 <- ggplot(data=dat1, aes(x=Earn, y=LeadEarn)) +
  geom_point(color=uc_blue) +
  geom_smooth(method="lm", se=F,
              size=0.5, alpha=0.2, color=high_red) +
  labs(y=quote(E[t+1]),
       x=quote(E[t]))
  )
(b2 <- ggplot(data=dat1, aes(x=Earn, y=LeadEarn)) +
  geom_point(color=uc_blue) +
  geom_smooth(method="lm", aes(group=id), se=F,
              size=0.5, alpha=0.2, color=high_red) +
    labs(y=quote(E[t+1]),
         x=quote(E[t]))
  )

grid1 <- grid.arrange(b1, b2, nrow=1)
ggsave(file="out/figs/slopes1.pdf", grid1, width=12)

# Varying Intercepts Regularized ----------------------------------------------------
var_slopes_code<- "
data{
  int<lower=1> N;
  int<lower=1> N_firm;
  real LeadEarn[N];
  real Earn[N];
  int firm[N];
}
parameters{
  vector[N_firm] un_firm;
  real<lower=0> sigma;
  real<lower=0> mu_firm;
  real<lower=0> sigma_firm;
}
transformed parameters{
  vector[N_firm] b_firm;
  for ( j in 1:N_firm ) {
    b_firm[j] = mu_firm + un_firm[j]*sigma_firm;
  }
}
model{
  vector[N] mu;
  un_firm    ~ normal( 0 , 10 );
  sigma      ~ normal( 0 , 10 );
  sigma_firm ~ normal( 0 , 10 );
  mu_firm    ~ normal( 0 , 10 );
  for ( i in 1:N ) {
    mu[i] = b_firm[firm[i]] * Earn[i];
  }
  LeadEarn ~ normal( mu , sigma );
}
"

fit <- stan(model_code=var_slopes_code, data=input_data,
            iter=2000, warmup=1000, chains=2,
            verbose=FALSE)


pars <- c("sigma", "sigma_firm", "mu_firm")
print(fit, pars=pars)
traceplot(fit, pars=pars)
par(mar=c(.1, .1,.1,.1))
pairs(fit, pars=pars)

b_firms <-  as.matrix(fit, par="b_firm")

slopes <- ols_slopes %>%
  rename(OLS=Slope) %>%
  mutate(Bayes = colMeans(b_firms)) %>%
  gather(key="Method", value="Slope", -id)

(b3 <-  ggplot(data=slopes %>% filter(Slope > -5), aes(x=Slope, fill=Method)) +
  geom_histogram(binwidth=0.1, position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c(uc_blue, high_red)) +
  theme(legend.position = "bottom") +
  labs(y="Count", x="Firm slope coefficient"))

ggsave("out/figs/slopes2.pdf", b3)

summary(ols_slopes$Slope)
summary(colMeans(b_firms))
