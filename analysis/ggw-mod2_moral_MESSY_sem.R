library(lavaan)
library(semPlot)
library(semTools)
library(ggplot2)
# library(slfm)
# library(lpSolve)
# library(psych)
library(corrplot)

plot_matrix <- function(matrix_toplot){
  corrplot(matrix_toplot, is.corr = FALSE, 
           type = 'lower', 
           order = "original", 
           tl.col='black', tl.cex=.75)
}

# cfa_model <- '
# PHYSIO =~ hungry + pain + tired + fear + pleasure + conscious + free_will + safe + desires
# SOCEMO =~ embarrassed + pride + love + guilt + depressed + disrespected + beliefs + emo_recog + joy
# PERCOG =~ remembering + recognizing + temperature + communicating + goal + depth + sounds + seeing + choices
# '
# 
# cfa_fit <- cfa(model = cfa_model,
#                data = d1)
# 
# summary(cfa_fit)
# 
# semPaths(cfa_fit, what = 'std',
#          label.prop = 1,
#          edge.label.cex = 0.75,
#          curvePivot = T,
#          fade = F)

cfa_model3 <- '
PHYSIO =~ A*hungry + pain + tired + fear + pleasure
SOCEMO =~ B*embarrassed + pride + love + guilt + depressed
PERCOG =~ C*remembering + recognizing + temperature + communicating + goal
'

cfa_fit3 <- cfa(model = cfa_model3, data = d1)

summary(cfa_fit3, fit.measures = T)

semPaths(cfa_fit3, what = 'std',
         label.prop = 1,
         edge.label.cex = 0.75,
         curvePivot = T,
         fade = F)

plot_matrix(resid(cfa_fit3)$cov)

cfa_model2 <- '
PHYSIO =~ A*hungry + pain + tired + fear + pleasure + conscious + free_will + safe + desires + calm + nauseated + angry + intentions + self_aware + odors
SOCEMO =~ B*embarrassed + pride + love + guilt + depressed + disrespected + beliefs + emo_recog + joy + personality + happy + morality + thoughts + self_restraint
PERCOG =~ C*remembering + recognizing + temperature + communicating + goal + depth + sounds + seeing + choices + reasoning
'

cfa_fit2 <- cfa(model = cfa_model2, data = d1)

summary(cfa_fit2, fit.measures = T)

semPaths(cfa_fit2, what = 'std',
         label.prop = 1,
         edge.label.cex = 0.75,
         curvePivot = T,
         fade = F)

plot_matrix(resid(cfa_fit2)$cov)


cfa_model1 <- '
PHYSIO =~ A*hungry + pain + tired + fear + pleasure + conscious + free_will + safe + desires + calm + nauseated + angry + happy + intentions + self_aware + joy + thoughts + odors + computations
SOCEMO =~ B*embarrassed + pride + love + guilt + depressed + disrespected + beliefs + emo_recog + joy + personality + happy + morality + angry + thoughts + self_restraint + nauseated + self_aware + reasoning + pleasure + calm + desires
PERCOG =~ C*remembering + recognizing + temperature + communicating + goal + depth + sounds + seeing + choices + reasoning + computations + odors
'

cfa_fit1 <- cfa(model = cfa_model1, data = d1)

summary(cfa_fit1, fit.measures = T)

semPaths(cfa_fit1, what = 'std',
         label.prop = 1,
         edge.label.cex = 0.75,
         curvePivot = T,
         fade = F)

residuals(cfa_fit1)
plot_matrix(resid(cfa_fit1)$cov)


# cfa_model0 <- '
# EXP =~ happy + depressed + fear + angry + calm + sounds + seeing + temperature + odors + depth + computations + thoughts + reasoning + remembering + beliefs + hungry + tired + pain + nauseated + safe + love + recognizing + communicating + guilt + disrespected + free_will + choices + self_restraint + intentions + goal + conscious + self_aware + desires + embarrassed + emo_recog + joy + morality + personality + pleasure + pride
# '
# 
# cfa_fit0 <- cfa(model = cfa_model0,
#                 data = d1)
# 
# summary(cfa_fit0)
# summary(cfa_fit0, fit.measures = T)
# 
# semPaths(cfa_fit0, what = 'std',
#          label.prop = 1,
#          edge.label.cex = 0.75,
#          curvePivot = T,
#          fade = F)
# 
# residuals(cfa_fit0)
# plot_matrix(resid(cfa_fit0)$cov)


# SEM ---------

causal_model1 <- '
# latent variables
PHYSIO =~ hungry + pain + tired + fear + pleasure + conscious + free_will + safe + desires + calm + nauseated + angry + intentions + self_aware + odors
SOCEMO =~ embarrassed + pride + love + guilt + depressed + disrespected + beliefs + emo_recog + joy + personality + happy + morality + thoughts + self_restraint
PERCOG =~ remembering + recognizing + temperature + communicating + goal + depth + sounds + seeing + choices + reasoning

# regressions
concern_general ~ PHYSIO + SOCEMO + PERCOG

# covariances
PHYSIO ~~ A*SOCEMO + B*PERCOG
SOCEMO ~~ C*PERCOG
'

# '
# hungry ~~ 0*pain + 0*tired + 0*fear + 0*pleasure + 0*conscious + 0*free_will + 0*safe + 0*desires + 0*calm + 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# pain ~~ 0*tired + 0*fear + 0*pleasure + 0*conscious + 0*free_will + 0*safe + 0*desires + 0*calm + 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# tired ~~ 0*fear + 0*pleasure + 0*conscious + 0*free_will + 0*safe + 0*desires + 0*calm + 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# fear ~~ 0*pleasure + 0*conscious + 0*free_will + 0*safe + 0*desires + 0*calm + 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# pleasure ~~ 0*conscious + 0*free_will + 0*safe + 0*desires + 0*calm + 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# conscious ~~ 0*free_will + 0*safe + 0*desires + 0*calm + 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# free_will ~~ 0*safe + 0*desires + 0*calm + 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# safe ~~ 0*desires + 0*calm + 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# desires ~~ 0*calm + 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# calm ~~ 0*nauseated + 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# nauseated ~~ 0*angry + 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# angry ~~ 0*intentions + 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# intentions ~~ 0*self_aware + 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# self_aware ~~ 0*odors + 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# odors ~~ 0*embarrassed + 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# embarrassed ~~ 0*pride + 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# pride ~~ 0*love + 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# love ~~ 0*guilt + 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# guilt ~~ 0*depressed + 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# depressed ~~ 0*disrespected + 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# disrespected ~~ 0*beliefs + 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# beliefs ~~ 0*emo_recog + 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# emo_recog ~~ 0*joy + 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# joy ~~ 0*personality + 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# personality ~~ 0*happy + 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# happy ~~ 0*morality + 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# morality ~~ 0*thoughts + 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# thoughts ~~ 0*self_restraint + 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# self_restraint ~~ 0*remembering + 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# remembering ~~ 0*recognizing + 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# recognizing ~~ 0*temperature + 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# temperature ~~ 0*communicating + 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# communicating ~~ 0*goal + 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# goal ~~ 0*depth + 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# depth ~~ 0*sounds + 0*seeing + 0*choices + 0*reasoning
# 
# sounds ~~ 0*seeing + 0*choices + 0*reasoning
# 
# seeing ~~ 0*choices + 0*reasoning
# 
# choices ~~ 0*reasoning
# '

causal_fit1 <- lavaan::sem(model = causal_model1, data = d1_moral_merged, fixed.x = F)

causal_fit1
summary(causal_fit1, fit.measures = T)
show(causal_fit1)

semPaths(causal_fit1, what = 'std')

# residuals(causal_fit1)
# plot_matrix(resid(causal_fit1)$cov)

causal_model2 <- '
# latent variables
PHYSIO =~ A*hungry + pain + tired + fear + pleasure + conscious + free_will + safe + desires + calm + nauseated + angry + happy + intentions + self_aware + joy + thoughts + odors + computations
SOCEMO =~ B*embarrassed + pride + love + guilt + depressed + disrespected + beliefs + emo_recog + joy + personality + happy + morality + angry + thoughts + self_restraint + nauseated + self_aware + reasoning + pleasure + calm + desires
PERCOG =~ C*remembering + recognizing + temperature + communicating + goal + depth + sounds + seeing + choices + reasoning + computations + odors
# INT_PHSE =~ PHYSIO * SOCEMO
# INT_PHPC =~ PHYSIO * PERCOG
# INT_SEPC =~ SOCEMO * PERCOG

# regressions
# concern_general ~ ph * PHYSIO
# concern_general ~ se * SOCEMO
# concern_general ~ pc * PERCOG
concern_general ~ PHYSIO + SOCEMO + PERCOG
# concern_general ~ PHYSIO + SOCEMO + PERCOG + INT_PHSE + INT_PHPC + INT_SEPC

# interaction
# ph_se := ph * se
# overall := ph + se + pc + ph_se

# covariances
PHYSIO ~~ SOCEMO + PERCOG
SOCEMO ~~ PERCOG
# INT_PHSE ~~ 0 * PHYSIO + 0 * SOCEMO + 0 * PERCOG + 0 * INT_PHPC + 0 * INT_SEPC
# INT_PHPC ~~ 0 * PHYSIO + 0 * SOCEMO + 0 * PERCOG + 0 * INT_PHSE + 0 * INT_SEPC
# INT_SEPC ~~ 0 * PHYSIO + 0 * SOCEMO + 0 * PERCOG + 0 * INT_PHPC + 0 * INT_PHSE
'

# causal_fit2 <- lavaan::sem(model = causal_model2, data = d1_moral_merged, se = "bootstrap")
causal_fit2 <- lavaan::sem(model = causal_model2, data = d1_moral_merged)

causal_fit2
summary(causal_fit2, fit.measures = T)
# show(causal_fit2)

semPaths(causal_fit2, what = 'std')

# residuals(causal_fit2)
# plot_matrix(resid(causal_fit2)$cov)




# causal_model2 <- '
# # latent variables
# PHYSIO =~ A*hungry + pain + tired + fear + pleasure + conscious + free_will + safe + desires + calm + nauseated + angry + happy + intentions + self_aware + joy + thoughts + odors + computations
# SOCEMO =~ B*embarrassed + pride + love + guilt + depressed + disrespected + beliefs + emo_recog + joy + personality + happy + morality + angry + thoughts + self_restraint + nauseated + self_aware + reasoning + pleasure + calm + desires
# PERCOG =~ C*remembering + recognizing + temperature + communicating + goal + depth + sounds + seeing + choices + reasoning + computations + odors
# 
# # regressions
# concern_general ~ PHYSIO + SOCEMO + PERCOG
# SOCEMO + PERCOG ~ PHYSIO
# 
# # covariances
# # PHYSIO ~~ SOCEMO + PERCOG
# SOCEMO ~~ PERCOG
# '
# 
# causal_fit2 <- lavaan::sem(model = causal_model2, data = d1_moral_merged)
# 
# summary(causal_fit2)
# summary(causal_fit2, fit.measures = T)
# 
# semPaths(causal_fit2, what = 'std')
# 
# # residuals(causal_fit2)
# 
# plot_matrix(resid(causal_fit2)$cov)
# 
# cbind(model1 = AIC(causal_fit1), model2 = AIC(causal_fit2))


temp <- d1_moral_merged %>%
  mutate(condition_fac = factor(condition, levels = c("beetle", "robot")),
         gender_fac = factor(ifelse(gender == "other", NA, as.character(gender))))

causal_model3 <- causal_model1

causal_fit3 <- lavaan::sem(model = causal_model3, data = temp,
                           group = "gender_fac")

summary(causal_fit3, fit.measures = T)

semPaths(causal_fit3, what = 'std')

anova(causal_fit3, causal_fit1)

# residuals(causal_fit3)
# plot_matrix(resid(causal_fit3)$cov)
