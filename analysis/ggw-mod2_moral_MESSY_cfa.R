library(lavan)

cfa_model <- 'PHYSIO =~ hungry + pain + tired + fear + pleasure + conscious + free_will + safe + desires
SOCEMO =~ embarrassed + pride + love + guilt + depressed + disrespected + beliefs + emo_recog + joy
PERCOG =~ remembering + recognizing + temperature + communicating + goal + depth + sounds + seeing + choices'

cfa_fit <- cfa(model = cfa_model,
           data = d1)

summary(cfa_fit)

library(semPlot)
library(semTools)
semPaths(cfa_fit, what = 'std',
         label.prop = 1,
         edge.label.cex = 0.75,
         curvePivot = T,
         fade = F)

# reliability of factors
cfa_reliability <- reliability(cfa_fit)

# factor scores
cfa_scores <- data.frame(predict(cfa_fit))

# merge new factor scores with moral data
temp1 <- d1 %>%
  bind_cols(cfa_scores) %>%
  left_join(d1_workers) 

temp2 <- d1_moral %>%
  left_join(d1_moral_workers)
  
d1_moral_merged_cfa <- full_join(temp1, temp2, by = "workerID") %>%
  filter(is.na(concern_general) == F)
d1_moral_merged_cfa_n <- length(levels(factor(d1_moral_merged_cfa$workerID)))


## Correlations with general moral concern

# ALL participants
rp_PHYSIO.gen <- with(d1_moral_merged_cfa, 
                      cor.test(PHYSIO, concern_general, # physiological
                               method = "pearson")); rp_PHYSIO.gen 
rp_SOCEMO.gen <- with(d1_moral_merged_cfa, 
                      cor.test(SOCEMO, concern_general, # social-emotional
                               method = "pearson")); rp_SOCEMO.gen
rp_PERCOG.gen <- with(d1_moral_merged_cfa, 
                      cor.test(PERCOG, concern_general, # perceptual-cognitive
                               method = "pearson")); rp_PERCOG.gen
rp_PHYSIO.SOCEMO <- with(d1_moral_merged_cfa, cor.test(PHYSIO, SOCEMO, method = "pearson")); rp_PHYSIO.SOCEMO
rp_PHYSIO.PERCOG <- with(d1_moral_merged_cfa, cor.test(PHYSIO, PERCOG, method = "pearson")); rp_PHYSIO.PERCOG
rp_SOCEMO.PERCOG <- with(d1_moral_merged_cfa, cor.test(SOCEMO, PERCOG, method = "pearson")); rp_SOCEMO.PERCOG

# compare phy to soc-emo
paired.r(xy = rp_PHYSIO.gen$estimate,
         xz = rp_SOCEMO.gen$estimate,
         yz = rp_PHYSIO.SOCEMO$estimate,
         n = d1_moral_merged_cfa_n)

# compare phy to per-cog
paired.r(xy = rp_PHYSIO.gen$estimate,
         xz = rp_PERCOG.gen$estimate,
         yz = rp_PHYSIO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# compare soc-emo to per-cog
paired.r(xy = rp_SOCEMO.gen$estimate,
         xz = rp_PERCOG.gen$estimate,
         yz = rp_SOCEMO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# spearman correlations with rankings of general concern
rs_PHYSIO.genrank <- with(d1_moral_merged_cfa, 
                          cor.test(PHYSIO, concern_general, # physiological
                                   method = "spearman")); rs_PHYSIO.genrank 
rs_SOCEMO.genrank <- with(d1_moral_merged_cfa, 
                          cor.test(SOCEMO, concern_general, # social-emotional
                                   method = "spearman")); rs_SOCEMO.genrank
rs_PERCOG.genrank <- with(d1_moral_merged_cfa, 
                          cor.test(PERCOG, concern_general, # perceptual-cognitive
                                   method = "spearman")); rs_PERCOG.genrank
rs_PHYSIO.SOCEMO <- with(d1_moral_merged_cfa, cor.test(PHYSIO, SOCEMO, method = "spearman")); rs_PHYSIO.SOCEMO
rs_PHYSIO.PERCOG <- with(d1_moral_merged_cfa, cor.test(PHYSIO, PERCOG, method = "spearman")); rs_PHYSIO.PERCOG
rs_SOCEMO.PERCOG <- with(d1_moral_merged_cfa, cor.test(SOCEMO, PERCOG, method = "spearman")); rs_SOCEMO.PERCOG

# compare phy to soc-emo
paired.r(xy = rs_PHYSIO.genrank$estimate,
         xz = rs_SOCEMO.genrank$estimate,
         yz = rs_PHYSIO.SOCEMO$estimate,
         n = d1_moral_merged_cfa_n)

# compare phy to per-cog
paired.r(xy = rs_PHYSIO.genrank$estimate,
         xz = rs_PERCOG.genrank$estimate,
         yz = rs_PHYSIO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# compare soc-emo to per-cog
paired.r(xy = rs_SOCEMO.genrank$estimate,
         xz = rs_PERCOG.genrank$estimate,
         yz = rs_SOCEMO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# ROBOT participants
rp_ROBOT_PHYSIO.gen <- with(subset(d1_moral_merged_cfa, condition.x == "robot"),
                            cor.test(PHYSIO, concern_general, # physiological
                                     method = "pearson")); rp_ROBOT_PHYSIO.gen 
rp_ROBOT_SOCEMO.gen <- with(subset(d1_moral_merged_cfa, condition.x == "robot"), 
                         cor.test(SOCEMO, concern_general, # social-emotional
                                  method = "pearson")); rp_ROBOT_SOCEMO.gen
rp_ROBOT_PERCOG.gen <- with(subset(d1_moral_merged_cfa, condition.x == "robot"), 
                         cor.test(PERCOG, concern_general, # perceptual-cognitive
                                  method = "pearson")); rp_ROBOT_PERCOG.gen
rp_ROBOT_PHYSIO.SOCEMO <- with(subset(d1_moral_merged_cfa, condition.x == "robot"), 
                         cor.test(PHYSIO, SOCEMO, method = "pearson")); rp_ROBOT_PHYSIO.SOCEMO
rp_ROBOT_PHYSIO.PERCOG <- with(subset(d1_moral_merged_cfa, condition.x == "robot"), 
                         cor.test(PHYSIO, PERCOG, method = "pearson")); rp_ROBOT_PHYSIO.PERCOG
rp_ROBOT_SOCEMO.PERCOG <- with(subset(d1_moral_merged_cfa, condition.x == "robot"), 
                         cor.test(SOCEMO, PERCOG, method = "pearson")); rp_ROBOT_SOCEMO.PERCOG

# compare phy to soc-emo
paired.r(xy = rp_ROBOT_PHYSIO.gen$estimate,
         xz = rp_ROBOT_SOCEMO.gen$estimate,
         yz = rp_ROBOT_PHYSIO.SOCEMO$estimate,
         n = d1_moral_merged_cfa_n)

# compare phy to per-cog
paired.r(xy = rp_ROBOT_PHYSIO.gen$estimate,
         xz = rp_ROBOT_PERCOG.gen$estimate,
         yz = rp_ROBOT_PHYSIO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# compare soc-emo to per-cog
paired.r(xy = rp_ROBOT_SOCEMO.gen$estimate,
         xz = rp_ROBOT_PERCOG.gen$estimate,
         yz = rp_ROBOT_SOCEMO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# spearman correlations with rankings of general concern
rs_ROBOT_PHYSIO.genrank <- with(subset(d1_moral_merged_cfa, condition.x == "robot"),
                             cor.test(PHYSIO, concern_general, # physiological
                                      method = "spearman")); rs_ROBOT_PHYSIO.genrank 
rs_ROBOT_SOCEMO.genrank <- with(subset(d1_moral_merged_cfa, condition.x == "robot"),
                             cor.test(SOCEMO, concern_general, # social-emotional
                                      method = "spearman")); rs_ROBOT_SOCEMO.genrank
rs_ROBOT_PERCOG.genrank <- with(subset(d1_moral_merged_cfa, condition.x == "robot"),
                             cor.test(PERCOG, concern_general, # perceptual-cognitive
                                      method = "spearman")); rs_ROBOT_PERCOG.genrank
rs_ROBOT_PHYSIO.SOCEMO <- with(subset(d1_moral_merged_cfa, condition.x == "robot"),
                         cor.test(PHYSIO, SOCEMO, method = "spearman")); rs_ROBOT_PHYSIO.SOCEMO
rs_ROBOT_PHYSIO.PERCOG <- with(subset(d1_moral_merged_cfa, condition.x == "robot"),
                         cor.test(PHYSIO, PERCOG, method = "spearman")); rs_ROBOT_PHYSIO.PERCOG
rs_ROBOT_SOCEMO.PERCOG <- with(subset(d1_moral_merged_cfa, condition.x == "robot"),
                         cor.test(SOCEMO, PERCOG, method = "spearman")); rs_ROBOT_SOCEMO.PERCOG

# compare phy to soc-emo
paired.r(xy = rs_ROBOT_PHYSIO.genrank$estimate,
         xz = rs_ROBOT_SOCEMO.genrank$estimate,
         yz = rs_ROBOT_PHYSIO.SOCEMO$estimate,
         n = d1_moral_merged_cfa_n)

# compare phy to per-cog
paired.r(xy = rs_ROBOT_PHYSIO.genrank$estimate,
         xz = rs_ROBOT_PERCOG.genrank$estimate,
         yz = rs_ROBOT_PHYSIO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# compare soc-emo to per-cog
paired.r(xy = rs_ROBOT_SOCEMO.genrank$estimate,
         xz = rs_ROBOT_PERCOG.genrank$estimate,
         yz = rs_ROBOT_SOCEMO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# BEETLE participants
rp_BEETLE_PHYSIO.gen <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"),
                          cor.test(PHYSIO, concern_general, # physiological
                                   method = "pearson")); rp_BEETLE_PHYSIO.gen 
rp_BEETLE_SOCEMO.gen <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"), 
                          cor.test(SOCEMO, concern_general, # social-emotional
                                   method = "pearson")); rp_BEETLE_SOCEMO.gen
rp_BEETLE_PERCOG.gen <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"), 
                          cor.test(PERCOG, concern_general, # perceptual-cognitive
                                   method = "pearson")); rp_BEETLE_PERCOG.gen
rp_BEETLE_PHYSIO.SOCEMO <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"), 
                          cor.test(PHYSIO, SOCEMO, method = "pearson")); rp_BEETLE_PHYSIO.SOCEMO
rp_BEETLE_PHYSIO.PERCOG <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"), 
                          cor.test(PHYSIO, PERCOG, method = "pearson")); rp_BEETLE_PHYSIO.PERCOG
rp_BEETLE_SOCEMO.PERCOG <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"), 
                          cor.test(SOCEMO, PERCOG, method = "pearson")); rp_BEETLE_SOCEMO.PERCOG

# compare phy to soc-emo
paired.r(xy = rp_BEETLE_PHYSIO.gen$estimate,
         xz = rp_BEETLE_SOCEMO.gen$estimate,
         yz = rp_BEETLE_PHYSIO.SOCEMO$estimate,
         n = d1_moral_merged_cfa_n)

# compare phy to per-cog
paired.r(xy = rp_BEETLE_PHYSIO.gen$estimate,
         xz = rp_BEETLE_PERCOG.gen$estimate,
         yz = rp_BEETLE_PHYSIO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# compare soc-emo to per-cog
paired.r(xy = rp_BEETLE_SOCEMO.gen$estimate,
         xz = rp_BEETLE_PERCOG.gen$estimate,
         yz = rp_BEETLE_SOCEMO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# spearman correlations with rankings of general concern
rs_BEETLE_PHYSIO.genrank <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"),
                              cor.test(PHYSIO, concern_general, # physiological
                                       method = "spearman")); rs_BEETLE_PHYSIO.genrank 
rs_BEETLE_SOCEMO.genrank <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"),
                              cor.test(SOCEMO, concern_general, # social-emotional
                                       method = "spearman")); rs_BEETLE_SOCEMO.genrank
rs_BEETLE_PERCOG.genrank <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"),
                              cor.test(PERCOG, concern_general, # perceptual-cognitive
                                       method = "spearman")); rs_BEETLE_PERCOG.genrank
rs_BEETLE_PHYSIO.SOCEMO <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"),
                          cor.test(PHYSIO, SOCEMO, method = "spearman")); rs_BEETLE_PHYSIO.SOCEMO
rs_BEETLE_PHYSIO.PERCOG <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"),
                          cor.test(PHYSIO, PERCOG, method = "spearman")); rs_BEETLE_PHYSIO.PERCOG
rs_BEETLE_SOCEMO.PERCOG <- with(subset(d1_moral_merged_cfa, condition.x == "beetle"),
                          cor.test(SOCEMO, PERCOG, method = "spearman")); rs_BEETLE_SOCEMO.PERCOG

# compare phy to soc-emo
paired.r(xy = rs_BEETLE_PHYSIO.genrank$estimate,
         xz = rs_BEETLE_SOCEMO.genrank$estimate,
         yz = rs_BEETLE_PHYSIO.SOCEMO$estimate,
         n = d1_moral_merged_cfa_n)

# compare phy to per-cog
paired.r(xy = rs_BEETLE_PHYSIO.genrank$estimate,
         xz = rs_BEETLE_PERCOG.genrank$estimate,
         yz = rs_BEETLE_PHYSIO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

# compare soc-emo to per-cog
paired.r(xy = rs_BEETLE_SOCEMO.genrank$estimate,
         xz = rs_BEETLE_PERCOG.genrank$estimate,
         yz = rs_BEETLE_SOCEMO.PERCOG$estimate,
         n = d1_moral_merged_cfa_n)

## Exploratory plots 

# condition
ggplot(aes(x = condition.x, y = concern_general, 
           fill = condition.x, group = condition.x), data = d1_moral_merged_cfa) +
  geom_boxplot(fill = "white", alpha = 0.5, width = 0.9) +
  geom_jitter(shape = 21, colour = "black", width = 0.7,
              alpha = 0.5, size = 3, stroke = 1) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "General Moral Concern by Condition (RAW SCORES)\n",
       x = "\nCondition",
       y = "General moral concern\n")


## Plots with general moral concern

# raw scores
ggplot(aes(x = PHYSIO, y = concern_general, fill = condition.x), data = d1_moral_merged_cfa) +
  geom_smooth(method = "lm", alpha = 0.1, aes(colour = condition.x)) +
  geom_point(shape = 21, colour = "black", 
             alpha = 0.5, size = 3, stroke = 1) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(title = "Physiological Experience vs. General Moral Concern: RAW SCORES\n",
       fill = "Condition",
       colour = "Condition",
       x = "\nPHYSIO factor score (physiological)",
       y = "General moral concern\n")
#   annotate("text", x = 0, y = d1_moral_merged_cfa_n, size = 5, hjust = 0.5,
#        label = paste0("Pearson's r = ", round(rp_PHYSIO.gen$estimate, 2),
#                       ", p = ", round(rp_PHYSIO.gen$p.value, 3)))

ggplot(aes(x = SOCEMO, y = concern_general, fill = condition.x), data = d1_moral_merged_cfa) +
  geom_smooth(method = "lm", alpha = 0.1, aes(colour = condition.x)) +
  geom_point(shape = 21, colour = "black", 
             alpha = 0.5, size = 3, stroke = 1) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(title = "Social-emotional Experience vs. General Moral Concern: RAW SCORES\n",
       fill = "Condition",
       colour = "Condition",
       x = "\nSOCEMO factor score (social-emotional)",
       y = "General moral concern\n")
#   annotate("text", x = 0, y = d1_moral_merged_cfa_n, size = 5, hjust = 0.5,
#        label = paste0("Pearson's r = ", round(rp_SOCEMO.gen$estimate, 2),
#                       ", p = ", round(rp_SOCEMO.gen$p.value, 3)))

ggplot(aes(x = PERCOG, y = concern_general, fill = condition.x), data = d1_moral_merged_cfa) +
  geom_smooth(method = "lm", alpha = 0.1, aes(colour = condition.x)) +
  geom_point(shape = 21, colour = "black", 
             alpha = 0.5, size = 3, stroke = 1) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(title = "Perceptual-cognitive Experience vs. General Moral Concern: RAW SCORES\n",
       fill = "Condition",
       colour = "Condition",
       x = "\nPERCOG factor score (perceptual-cognitive)",
       y = "General moral concern\n")
#   annotate("text", x = 0, y = d1_moral_merged_cfa_n, size = 5, hjust = 0.5,
#        label = paste0("Pearson's r = ", round(rp_PERCOG.gen$estimate, 2),
#                       ", p = ", round(rp_PERCOG.gen$p.value, 3)))

# ranks
ggplot(aes(x = rank(PHYSIO), y = rank(concern_general), 
           fill = condition.x, group = condition.x), data = d1_moral_merged_cfa) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_point(shape = 21, colour = "black", 
             alpha = 0.5, size = 3, stroke = 1) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(title = "Physiological Experience vs. General Moral Concern: RANKS\n",
       x = "\nPHYSIO factor score ranking (physiological)",
       y = "General moral concern ranking\n")
#   annotate("text", x = d1_moral_merged_cfa_n, y = 18, size = 5, hjust = 1,
#        label = paste0("Spearman's rho = ", round(rs_PHYSIO.genrank$estimate, 2),
#                       ", p = ", round(rs_PHYSIO.genrank$p.value, 3)))

ggplot(aes(x = rank(SOCEMO), y = rank(concern_general), 
           fill = condition.x, group = condition.x), data = d1_moral_merged_cfa) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_point(shape = 21, colour = "black", 
             alpha = 0.5, size = 3, stroke = 1) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(title = "Social-emotional Experience vs. General Moral Concern: RANKS\n",
       x = "\nSOCEMO factor score ranking (social-emotional)",
       y = "General moral concern ranking\n")
#   annotate("text", x = d1_moral_merged_cfa_n, y = 18, size = 5, hjust = 1,
#        label = paste0("Spearman's rho = ", round(rs_SOCEMO.genrank$estimate, 2),
#                       ", p = ", round(rs_SOCEMO.genrank$p.value, 3)))

ggplot(aes(x = rank(PERCOG), y = rank(concern_general),
           fill = condition.x, group = condition.x), data = d1_moral_merged_cfa) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_point(shape = 21, colour = "black", 
             alpha = 0.5, size = 3, stroke = 1) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(title = "Perceptual-cognitive Experience vs. General Moral Concern: RANKS\n",
       x = "\nPERCOG factor score ranking (perceptual-cognitive)",
       y = "General moral concern ranking\n")
#   annotate("text", x = d1_moral_merged_cfa_n, y = 18, size = 5, hjust = 1,
#          label = paste0("Spearman's rho = ", round(rs_PERCOG.genrank$estimate, 2),
#                         ", p = ", round(rs_PERCOG.genrank$p.value, 3)))