setwd("~/Documents/UH/data_science_group") # change to your wd

## if you have problems installing rstanarm, uncomment these lines below:
#if (!require(devtools)) {
#  install.packages("devtools")
#  library(devtools)
#}
# install_github("stan-dev/rstanarm", args = "--preclean", build_vignettes = FALSE)
# or try
# install.packages('rstanarm',type="mac.binary") # change type to "win.binary" for windows

library("rstanarm")
library("bayesplot")
library("lmerTest")
library("loo")
library("tidyverse")
options(mc.cores=parallel::detectCores ()) # Run on multiple cores for Bayesian regressions
 

## Regressions
data_vot <- read.csv("t_d_vot.csv") # import data

data_vot$stop <- ifelse(data_vot$consonant==-0.5,"apical","laminal")
data_vot$stop <- as.factor(data_vot$stop)
# strong priors
model1 <-stan_lmer(vot*1000 ~ stop + 
                    (1+stop|speaker_id) + 
                    (1|word),data=data_vot, 
                  prior_intercept = normal(0, 5,autoscale=F),
                  prior = normal(0, 5,autoscale = F), 
                  prior_covariance = decov(regularization = 2), 
                  chains = 4, 
                  iter = 2000, 
                  adapt_delta=0.9999,
                  warmup=1000)
prior_summary(model1)
posterior_vs_prior(model1,group_by_parameter = TRUE,facet_args = list(scales = "free_y"))
# weaker priors
model2 <-stan_lmer(vot*1000 ~ consonant2 + (1+consonant2|speaker_id)+ 
                     (1|word),
                   data=data_vot,
                   prior_intercept = normal(0, 50,autoscale=F),
                   prior = normal(0, 50,autoscale=F), 
                   prior_covariance = decov(regularization = 2),
                   chains = 4,
                   iter = 2000, adapt_delta=0.99999,warmup=1000)
posterior_vs_prior(model2,group_by_parameter = TRUE,facet_args = list(scales = "free_y")) 
prior_summary(model2)
# very weak priors
model3 <-stan_lmer(vot*1000 ~ consonant2 + (1+consonant2|speaker_id)+ 
                     (1|word),
                   data=data_vot,
                   prior_intercept = normal(0, 500,autoscale=F),
                   prior = normal(0, 500,autoscale=F), 
                   prior_covariance = decov(regularization = 2),
                   chains = 4,
                   iter = 2000, adapt_delta=0.999,warmup=1000)
posterior_vs_prior(model3,group_by_parameter = TRUE,facet_args = list(scales = "free_y"))
prior_summary(model2)

# frequentist model
model_freq <- lmer(vot*1000 ~ consonant2 + (1+consonant2|speaker_id)+ 
                     (1|word),data=data_vot)

## model summaries

# summary gives us results from an entire distribution (n=4000)
summary(model1) # strong priors
summary(model2) # weaker priors
summary(model3) # weak priors
summary(model_freq) # frequentist model

## Plotting posterior distributions of model1

posterior <- as.matrix(model1) # save the entire posterior distribution

# add title
plot_title <- ggtitle("Posterior dist for model1",
                      "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars=c("(Intercept)","stoplaminal"), 
           prob=0.95) + plot_title + ggplot2::xlab("VOT (ms)")

## ploting model2
posterior2 <- as.matrix(model2)

plot_title <- ggtitle("Posterior dist for model2",
                      "with medians and 95% intervals")
mcmc_areas(posterior2, 
           pars=c("(Intercept)","consonant2t̻"), 
           prob=0.95) + plot_title + ggplot2::xlab("VOT (ms)")


## plotting model3
posterior3 <- as.matrix(model3)

plot_title <- ggtitle("Posterior dist for model3",
                      "with medians and 95% intervals")
mcmc_areas(posterior3, 
           pars=c("(Intercept)","consonant2t̻"), 
           prob=0.95) + plot_title + ggplot2::xlab("VOT (ms)")



## Model Diagnostics for model1

launch_shinystan(model1) # opens interactive page in your web browser

pp_check(model1) # shows sampling of data by model

## Comparing models

# run a new version of model1 without random effect of word to see if is meaningful
model1a <-stan_lmer(vot*1000 ~ consonant2 + (1+consonant2|speaker_id),
                    data=data_vot,
                    prior_intercept = normal(0, 5,autoscale=F),
                    prior = normal(0, 5,autoscale=F), 
                    prior_covariance = decov(regularization = 2),
                    chains = 4,
                    iter = 2000, adapt_delta=0.999,warmup=1000)



# do leave one out cross-validation for each model
loo1 <- loo(model) 
loo1a <- loo(model1a,k_threshold=0.7)

# compare the two
compare_models(loo1,loo1a)
# if ELPD is negative, first model is better. If positive second model is better. If 0, then no difference











