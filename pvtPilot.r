library(tidyverse)
library(brms)

# Set N(cores) = detected no. of cores
options(mc.cores = parallel::detectCores())
# To see: getOption("mc.cores", 1L)

setwd("T:/TARG_data/Current project folders (unrestricted)/2020_PVT_JF_MM/Internal pilot/Trial by trial data/full, cleaned and manipulated databases/Aggregated")
df <- read.csv('MeansandQuestionnaire.csv')
head(df)

### Random Intercept for the mean, random effect for individual variability.
## y1~ gives fixed part for mean of y1
## sigma~ gives fixed part for within 

brms_fit1 <- brm( data = df,
                  family = gaussian(),
                  seed = 1337,
                  bf(RTs50 ~ 1 + kssScore + (1 |s| Subject.ID))
                )

## Raw summary stats
summary(brms_fit1)

## abridged and specified summary
round(head(posterior_summary(brms_fit1), n=4), digits = 2)

# Calculate ICC for single level model with random intercepts
pfit <- posterior_summary(brms_fit1)
ICC <- pfit['sd_Subject.ID__Intercept', 'Estimate']/(pfit['sd_Subject.ID__Intercept', 'Estimate']+pfit['sigma', 'Estimate'])
# Print out ICC (2 dp) = 0.66
print(round(ICC, digits=2))

## Plot posterior distributions of parameter estimates: 
plot(brms_fit1)

#################################################################

# Allow within individual variance to be a function of kssScore
# Likely very unstable due to small N

brms_fit2 <- brm(data = df,
                    family = gaussian(),
                    seed = 1337,
                    bf(RTs50 ~ 1 + kssScore + (1 |s| Subject.ID),
                       sigma ~ 1 + kssScore + (1 |s| Subject.ID))
)

#######################################
## Inspecting the results
summary(brms_fit2)
# As suspected- divergent transitions after warmup - suggesting parameter space is not adequately explored by warmup. 
# Try pairs() diagnostic to inspect parameter covariance plots
pairs(brms_fit2)

###########################################################################

# Inntroduce covariates - AM/PM as fixed effect

brms_fit3 <- brm(data=df, 
                 family=gaussian(),
                 seed=1337,
                 bf(RTs50 ~ 1 + kssScore + AMPM + (1|Subject.ID)
                    ))
summary(brms_fit3)

# Introduce AM/PM as random effect

brms_fit4 <- brm(data=df, 
                 family=gaussian(),
                 seed=1337,
                 bf(RTs50 ~ 1 + kssScore + AMPM + (1 + AMPM |Subject.ID)
                 ))
summary(brms_fit4)

########################################################################

# Want to compare substantive model fit, so compare WAIC across models

# Principally want to compare fit0(fixed effects model), fit1(fixed effects with AMPM), fit2(fixed effect for AMPM random intercepts at 
# level 1 (subject_id)), and fit3 (random intercept and random slope for AMPM at subject_id level) and fit4 (random intercept, and random 
# effect for within-individual change)
### Refit models consistent with this order.
fit0 <- brm(data=df, 
            family=gaussian(),
            seed=1337,
            bf(RTs50 ~ 1 +kssScore)
            )
#####
fit1 <- brm(data=df, 
            family=gaussian(),
            seed=1337,
            bf(RTs50 ~ 1 + kssScore + AMPM)
            )
#####
fit2 <- brm(data=df, 
            family=gaussian(),
            seed=1337,
            bf(RTs50 ~ 1 + kssScore + AMPM + (1 | Subject.ID)
               )
            )
#####
fit3 <- brm(data=df, 
            family=gaussian(),
            seed=1337,
            bf(RTs50 ~ 1 + kssScore + AMPM + (1 + AMPM | Subject.ID)
               )
            )
#####
fit4 <- brm(data=df,
            family=gaussian(),
            seed=1337,
            bf(RTs50 ~ 1 + kssScore + AMPM + (1 |s| Subject.ID),
               sigma ~ 1 + kssScore + (1 |s| Subject.ID))
            )
# |s| notation implies that we want to specify that these effects are modelled as correlated. 

model_list <- c(fit0, fit1, fit2, fit3, fit4)

for (i in model_list) {
  i <- add_criterion(i, "waic")
}
