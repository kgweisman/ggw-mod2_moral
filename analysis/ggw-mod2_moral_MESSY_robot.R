# limit only to ROBOT condition (even FA)

# run EFA with rotation with N factors
efa_d1_robot_rotatedN <- fa(d1_robot, 2, rotate = "varimax", 
                          scores = "regression", # need to be clear about choice!
                          cor = chosenCorType, fm = "minres")
print(efa_d1_robot_rotatedN)

# get loadings for each factor
efa_d1_robot_rotatedN_loadings <- loadings(efa_d1_robot_rotatedN)[] %>%
  data.frame() %>% 
  add_rownames(var = "mc")

# add factor scores for each participant
d1_factor_scores <- efa_d1_robot_rotatedN$scores %>%
  data.frame() %>%
  add_rownames(var = "subid") %>%
  full_join(d1 %>% select(subid, mturkcode)) %>%
  full_join(d1_workers) %>%
  select(-subid, -mturkcode)

# clean up datasets
d1_d_moral_merged <- left_join(d1, d1_workers)
d1_moral_d_moral_merged <- full_join(d1_moral, d1_moral_workers)

d1_moral_merged <- left_join(d1_moral_d_moral_merged, d1_d_moral_merged, by = "workerID") %>%
  select(study.x:workerID, happy:religion_cat) %>%
  rename(study = study.x,
         subid = subid.x,
         date = date.x,
         start_time = start_time.x,
         end_time = end_time.x,
         duration = duration.x,
         finished_mod = finished_mod.x,
         mturkcode = mturkcode.x,
         condition = condition.x,
         CATCH = CATCH.x,
         feedback = feedback.x) %>%
  select(-CATCH.y, -feedback.y, -finished_mod.y) %>%
  left_join(d1_factor_scores) %>%
  filter(condition == "robot")

##

# make dataset
d_moral_merged <- d1_moral_merged %>%
  filter(is.na(subid) == F) %>%
  mutate(condition = factor(condition),
         religion_cat2 = factor(
           ifelse(religion_cat %in% c("christianity", "judaism"), "judeo-christian",
                  ifelse(religion_cat == "none", "not religious",
                         "other (religious)"))),
         race_cat2 = factor(ifelse(race_cat == "white", "white", "non-white")),
         median_MR1 = ifelse(MR1 < median(MR1), "low", "high"),
         median_MR2 = ifelse(MR2 < median(MR2), "low", "high"),
#          median_MR3 = ifelse(MR3 < median(MR3), "low", "high"),
         mean_MR1 = ifelse(MR1 < mean(MR1), "low", "high"),
         mean_MR2 = ifelse(MR2 < mean(MR2), "low", "high")
         # mean_MR3 = ifelse(MR3 < mean(MR3), "low", "high")
)

d_moral_merged_n <- length(levels(factor(d_moral_merged$subid)))

##

# ALL participants
rp_MR1.gen <- with(d_moral_merged, cor.test(MR1, concern_general, # emotional
                                            method = "pearson")); rp_MR1.gen 
rp_MR2.gen <- with(d_moral_merged, cor.test(MR2, concern_general, # perceptual
                                            method = "pearson")); rp_MR2.gen
rp_MR1.MR2 <- with(d_moral_merged, cor.test(MR1, MR2, method = "pearson")); rp_MR1.MR2

# compare emo to per
paired.r(xy = rp_MR1.gen$estimate,
         xz = rp_MR2.gen$estimate,
         yz = rp_MR1.MR2$estimate,
         n = d_moral_merged_n)

# spearman correlations with rankings of general concern
rs_MR1.genrank <- with(d_moral_merged, cor.test(MR1, concern_general, # emotional
                                                method = "spearman")); rs_MR1.genrank 
rs_MR2.genrank <- with(d_moral_merged, cor.test(MR2, concern_general, # perceptual
                                                method = "spearman")); rs_MR2.genrank
rs_MR1.MR2 <- with(d_moral_merged, cor.test(MR1, MR2, method = "spearman")); rs_MR1.MR2

# compare emo to per
paired.r(xy = rs_MR1.genrank$estimate,
         xz = rs_MR2.genrank$estimate,
         yz = rs_MR1.MR2$estimate,
         n = d_moral_merged_n)