### READ THIS ###
#To work, this script must be placed in the same folder with: Dataset_Hcanariensis.xlsx



# 0 - PREPARE THE ENVIRONMENT ----
#You can collapse and run this whole section as its only purpose is to
#prepare the R-environment

.start_time <- Sys.time() #Set the clock!

packages <- c(
  # Operate with data
  "openxlsx", "readxl", "writexl", "dplyr", "tidyr", "purrr", "tibble", "scales",
  "broom.mixed", "stats",
  
  # Models
  "glmmTMB", "vegan", "spaa", "bipartite", "funspace", "car", "performace",
  
  # Plots
  "ggplot2", "viridis",
  
  # Wake up!
  "beepr", "here" 
)

# Install and load missing packages
install.packages(setdiff(packages, rownames(installed.packages())))
invisible(lapply(packages, library, character.only = T))


#Create folders to save results and figures
directory <- c("figures", "results")
for(name in directory) {
  if (!dir.exists(name)){
    dir.create(name)
  }else{
    print("dir already exists")
  }
}


#Clean the environment
rm(list = ls())


#Is the directory correct?
#The folder in which BOTH the .R and .xlsx are stored
here()


### YOU SHOULD NOW BE ABLE TO RUN THE SCRIPT ###
beep()
cat("Required packages installed, now the script should run.\n\n\n")



#---------------------------------------------------------------------------#



#LOAD DATASETS
metad <- read.xlsx("Dataset_Hcanariensis.xlsx", sheet = "Metadata")
honeyb <- read.xlsx("Dataset_Hcanariensis.xlsx", sheet = "Hives")
evol <- read.xlsx("Dataset_Hcanariensis.xlsx", sheet = "Emergence")
produc <- read.xlsx("Dataset_Hcanariensis.xlsx", sheet = "Productivity")
morpho <- read.xlsx("Dataset_Hcanariensis.xlsx", sheet = "Morphology")
chc <- read.xlsx("Dataset_Hcanariensis.xlsx", sheet = "CHC")
floral <- read.xlsx("Dataset_Hcanariensis.xlsx", sheet = "Network")


#Refine the morphology dataset
morpho <- morpho[,c(1:6, 8:11, 14,17:19)]
w_morpho <- morpho
w_morpho$MOC <- w_morpho$MOC / w_morpho$ITD
w_morpho$Eye_A <- w_morpho$Eye_A / (w_morpho$ITD * w_morpho$ITD)
w_morpho$WA <- w_morpho$WA / (w_morpho$ITD * w_morpho$ITD)


#In each dataset, associate the number of apiaries for the closest day recordered
chc$Apiaries <- honeyb$Hives[
  sapply(chc$Day, function(x) which.min(abs(honeyb$Day - x)))]

w_morpho$Apiaries <- honeyb$Hives[
  sapply(w_morpho$Day, function(x) which.min(abs(honeyb$Day - x)))]

produc$Apiaries <- honeyb$Hives[
  sapply(produc$Day, function(x) which.min(abs(honeyb$Day - x)))]



#Create palettes
sex_p <- c(
  "M" = "skyblue",
  "F" = "purple3"
)

sex_shape <- c(
  "M" = 21,  #Square
  "F" = 24   #Triangle
)

distance_p <- c(
  "200"  = "#4B0055",
  "400"  = "#43296E",
  "700"  = "#234B84",
  "1000" = "#006992",
  "1200" = "#007395",
  "1300" = "#009796",
  "1500" = "#00AF8C",
  "1600" = "#00C377",
  "1700" = "#79D359",
  "1800" = "#C0DE35",
  "2100" = "#FDE333"
)

#Finish
beep()
cat("All the DFs have been prepared.\n\n\n")





#---------------------------------------------------------------------------#





#1 - PRODUCTIVITY ----

years <- c(2020, 2021)
vars <- c("M", "F", "TOT")

sink("results/GLM-Productivity.txt")

for (y in years) {
  
  cat("\n########################################\n")
  cat("YEAR:", y, "\n")
  cat("########################################\n\n")
  
  #For each year
  produc_sum <- produc %>%
    dplyr::filter(Year == y) %>%
    group_by(Site) %>%
    summarise(
      M = sum(Sex == "M", na.rm = T),
      F = sum(Sex == "F", na.rm = T),
      Day_mean = mean(Day, na.rm = T),
      Apiaries = first(Apiaries),
      Tube = mean(Tube_size, na.rm = T),
      .groups = "drop"
    ) %>%
    left_join(metad %>% dplyr::select(Site, Distance), by = "Site") %>%
    dplyr::mutate(TOT = M + F)
  
  #POISSON
  for (v in vars) {
    
    cat("\n------------------------------\n")
    cat(v, "\n")
    cat("------------------------------\n\n")
    
    formula <- as.formula(
      paste(v, "~ Distance")
    )
    
    model <- glm(
      formula,
      family = poisson(link = "log"),
      data = produc_sum
    )
    
    print(summary(model))
  }
  
  #SEX RATIO (binomial)
  cat("\n------------------------------\n")
  cat("Sex_R (binomial)\n")
  cat("------------------------------\n\n")
  
  model_sex <- glm(
    cbind(M, F) ~ Distance,
    family = binomial,
    data = produc_sum
  )
  
  print(summary(model_sex))
}

sink()



#Plots
produc_sum <- produc %>%
  group_by(Site, Year) %>%
  summarise(
    M = sum(Sex == "M", na.rm = T),
    F = sum(Sex == "F", na.rm = T),
    Day_mean = mean(Day, na.rm = T),
    Apiaries = first(Apiaries),
    Tube = mean(Tube_size, na.rm = T),
    .groups = "drop"
  ) %>%
  left_join(metad %>% dplyr::select(Site, Distance), by = "Site") %>%
  mutate(TOT = M + F)

produc_long <- produc_sum %>%
  pivot_longer(
    cols = c(M, F),
    names_to = "Sex",
    values_to = "Count"
  )


ggplot(produc_long, aes(x = Distance, y = Count, fill = Sex, shape = Sex)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(aes(color = Sex, linetype = factor(Year)),
              method = "lm", se = TRUE, level = 0.90,
              linewidth = 1, alpha = 0.1) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "Number of individuals per sex") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")
ggsave("figures/MF_Distance.pdf", height = 15, width = 15, units = "cm")  


ggplot(produc_long, aes(x = Distance, y = TOT)) +
  geom_point(size = 1.5, shape = 21, alpha = 0.5, fill = "grey") +
  geom_smooth(aes(linetype = factor(Year)), colour = "darkblue", method = "lm", se = T,
              linewidth = 1, alpha = 0.1) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "Total number of individuals") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")

ggsave("figures/TOT_Distance.pdf", height = 15, width = 15, units = "cm") 



#Finish
rm(produc_sum, produc_long, y, years, v, vars, model, model_sex, formula)
dev.off()
beep()
cat("Models on productivity done.\n\n\n")




#---------------------------------------------------------------------------#





#2 - EVOLUTION ----

#Prepare the dataset for the analysis
evol <- evol %>%
  left_join(metad %>% dplyr::select(Site, Distance), by = "Site")

evol <- evol[,-c(8:9)]

evol_long <- evol %>%
  pivot_longer(
    cols = c(`154`, `160`, `177`, `182`),
    names_to = "Day",
    values_to = "Completed"
  ) %>%
  mutate(Day = as.numeric(Day)) %>%
  arrange(Site, Nest, Side, Day) %>%
  group_by(Site, Nest, Side) %>%
  mutate(
    NewCompleted = pmax(Completed - lag(Completed, default = 0), 0)
  )

# Group by Side-nest-Day
evol_long <- evol_long %>%
  group_by(Site, Nest, Day) %>%
  summarize(
    Completed = sum(Completed, na.rm = T),
    NewCompleted = sum(NewCompleted, na.rm = T),
    .groups = "drop",
    Distance = first(Distance)
  )

#Associate apiaries
evol_long <- evol_long %>%
  rowwise() %>%
  mutate(
    Apiaries = honeyb$Hives[
      which.min(abs(honeyb$Day - Day))
    ]
  ) %>%
  ungroup()


#Model
tmp <- glmmTMB(Completed ~ Distance + Apiaries + (1 | Site/Nest),
                ziformula = ~1,
                family = nbinom2,
                data = evol_long)

sink("results/GLMM-Evolution.txt")
summary(tmp)
sink()


#Plot
ggplot(evol_long, aes(x = Apiaries, y = Completed, color = factor(Distance))) +
  geom_point(size = 2, shape = 21, alpha = 1, col = "darkgrey") +
  geom_smooth(method = "lm", se = F,
              aes(group = factor(Distance), color = factor(Distance)),
              linewidth = 1) +
  theme_bw() +
  labs(x = "Active beehives on the survey day",
       y = "Brood cells") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none") +
  scale_color_manual(values = distance_p)

ggsave("figures/Evolution.pdf", height = 15, width = 15, units = "cm")


#Finish
rm(evol_long, tmp)
dev.off()
beep()
cat("Models on evolution done.\n\n\n")





#---------------------------------------------------------------------------#





#3 - MORPHOLOGY ----

#Add distance
w_morpho <- w_morpho %>% 
  left_join(metad %>% select(Site, Distance), by = "Site")


##3.1 - Linear models ----
vars <- c("ITD", "Head", "MOC", "Eye_A", "WA", "WAR")

morpho_results <- lapply(vars, function(v){
  f <- as.formula(paste0("`", v, "` ~ Distance * Sex + (1|Site)"))
  mod <- glmmTMB(f, data = w_morpho, family = gaussian())
  
  an <- Anova(mod, type = 2)
  
  tidy(an) %>%
    transmute(
      variable = v,
      term     = term,
      statistic= statistic,
      df       = df,
      p.value  = p.value
    )
}) %>% bind_rows()


#For FA we have few observations
df <- na.omit(w_morpho[, c("FA","Distance","Sex","Site")])
lm1 <- lm(FA ~ Distance * Sex, data = df)


#Export results
write.xlsx(morpho_results, "results/LMM-Morphology.xlsx")
sink("results/FA.txt")
Anova(lm1, type = 2)
sink()

#Export summary statistics
tmp <- morpho[,c(6, 8:14)]

table <- tmp %>%
  group_by(Sex) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    -Sex,
    names_to = c("Variable", ".value"),
    names_pattern = "(.+)_(mean|se)$"
  ) %>%
  mutate(mean_se = sprintf("%.3f ± %.3f", mean, se)) %>%
  select(Sex, Variable, mean_se) %>%
  pivot_wider(names_from = Variable, values_from = mean_se)

write.xlsx(table, "results/Summary-Morphology.xlsx")




#Plots
ggplot(w_morpho, aes(x = Distance, y = Eye_A, fill = Sex, shape = Sex)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(aes(color = Sex), method = "lm", se = T, linewidth = 1, alpha = 0.1) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "Weighted compound eye area") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")

ggsave("figures/EyeA_Distance.pdf", height = 15, width = 15, units = "cm")


ggplot(w_morpho, aes(x = Distance, y = WAR, fill = Sex, shape = Sex)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(aes(color = Sex), method = "lm", se = T, linewidth = 1, alpha = 0.1) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "Weighted wing aspect ratio") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")
ggsave("figures/WAR_Distance.pdf", height = 15, width = 15, units = "cm")



##3.2 - NMDS ----

#Create the matrix
w_morpho2 <- w_morpho[,-14] #Exclude FA
w_morpho2 <- w_morpho2[complete.cases(w_morpho2),]

w_morpho_male <- w_morpho2 %>% 
  filter(Sex == "M") %>%
  dplyr::select(all_of(vars))

w_morpho_male <- w_morpho_male[, colSums(w_morpho_male, na.rm = T) != 0]
w_morpho_matrix_male_hel <- decostand(w_morpho_male, method = "hellinger")

w_morpho_female <- w_morpho2 %>% 
  filter(Sex == "F") %>%
  dplyr::select(all_of(vars))

w_morpho_female <- w_morpho_female[, colSums(w_morpho_female, na.rm = T) != 0]
w_morpho_matrix_female_hel <- decostand(w_morpho_female, method = "hellinger")




#Run NMDS
set.seed(123)

nmds_male <- metaMDS(w_morpho_matrix_male_hel, distance = "bray", k = 2, try = 200, trymax = 999)
nmds_male$stress

nmds_female <- metaMDS(w_morpho_matrix_female_hel, distance = "bray", k = 2, try = 200, trymax = 999)
nmds_female$stress

nmds_scores_male <- as.data.frame(scores(nmds_male, display = "sites"))
nmds_scores_male$Distance <- w_morpho2 %>% 
  filter(Sex == "M") %>% 
  pull(Distance)
nmds_scores_male$Distance_f <- factor(nmds_scores_male$Distance, levels = as.numeric(names(distance_p)))

nmds_scores_female <- as.data.frame(scores(nmds_female, display = "sites"))
nmds_scores_female$Distance <- w_morpho2 %>% 
  filter(Sex == "F") %>% 
  pull(Distance)
nmds_scores_female$Distance_f <- factor(nmds_scores_female$Distance, levels = as.numeric(names(distance_p)))

#Hull polygons
get_hull <- function(df, group_var){
  df[chull(df$NMDS1, df$NMDS2), ]
}

hull_male <- nmds_scores_male %>%
  group_by(Distance_f) %>%
  group_modify(~ get_hull(.x, "Distance_f"))

hull_female <- nmds_scores_female %>%
  group_by(Distance_f) %>%
  group_modify(~ get_hull(.x, "Distance_f"))


#Plot
ggplot(nmds_scores_male, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(fill = Distance_f), shape = 21, size = 2, alpha = 0.5) +
  geom_polygon(data = hull_male, aes(x = NMDS1, y = NMDS2, color = Distance_f),
               alpha = 0) +
  theme_bw() +
  labs(x = "NMDS1",
       y = "NMDS2") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none") +
  scale_fill_manual(values = distance_p) +
  scale_colour_manual(values = distance_p)

ggsave("figures/NMDS_Morphology_M.pdf", height = 15, width = 15, units = "cm")


ggplot(nmds_scores_female, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(fill = Distance_f), shape = 24, size = 2, alpha = 0.5) +
  geom_polygon(data = hull_female, aes(x = NMDS1, y = NMDS2, color = Distance_f),
               alpha = 0) +
  theme_bw() +
  labs(x = "NMDS1",
       y = "NMDS2") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none") +
  scale_fill_manual(values = distance_p) +
  scale_colour_manual(values = distance_p)

ggsave("figures/NMDS_Morphology_F.pdf", height = 15, width = 15, units = "cm")



##3.3 - Permanova ----
permanova_male <- adonis2(w_morpho_matrix_male_hel ~ Distance, data = nmds_scores_male ,
                        method = "euclidean", permutations = 9999)

disp <- betadisper(vegdist(w_morpho_matrix_male_hel, method = "euclidean"), nmds_scores_male$Distance)

sink("results/PERMANOVA_Morphology_M.txt")
anova(disp)
permutest(disp)
TukeyHSD(disp)
tapply(disp$distances, nmds_scores_male$Distance, mean)
permanova_male #P = 0.002
sink()



permanova_female <- adonis2(w_morpho_matrix_female_hel ~ Distance, data = nmds_scores_female ,
                          method = "euclidean", permutations = 9999)

disp <- betadisper(vegdist(w_morpho_matrix_female_hel, method = "euclidean"), nmds_scores_female$Distance)

sink("results/PERMANOVA_Morphology_F.txt")
anova(disp)
permutest(disp)
TukeyHSD(disp)
tapply(disp$distances, nmds_scores_female$Distance, mean)
permanova_female #P = 0.058
sink()


##3.4 - Functional tratis spaces ----

#Create subsets for each species-distance
w_morpho2$DistClass <- cut(
  w_morpho2$Distance,
  breaks = c(-Inf, 1000, 1600, Inf),
  labels = c("Close", "Medium", "Far")
)


#Apply FT spaces
results <- list()

for(sex in c("M", "F")) {
  
  #Subset
  dat <- subset(w_morpho2, Sex == sex)
  
  #PCA
  pca <- princomp(dat[, vars], cor = T)
  
  #Functional space
  fts <- funspace(
    x = pca,
    PCs = c(1,2),
    group.vec = dat$DistClass,
    n_divisions = 500
  )
  
  results[[sex]] <- fts
  
  #Save results
  sink(paste0("results/Funspace_global_", sex, ".txt"))
  print(summary(fts))
  sink()
}


#Store the results
df_FRich <- data.frame()

for(sex in names(results)) {
  
  fts <- results[[sex]]
  
  frich_vals <- fts$FD$group$FRich

  for(cat in names(frich_vals)) {
    
    df_FRich <- rbind(df_FRich, data.frame(
      Sex = sex,
      Category = cat,
      FRich = frich_vals[[cat]]
    ))
  }
}

df_FRich$Category <- factor(df_FRich$Category, 
                            levels = c("Close", "Medium", "Far"))
df_FRich$Category <- dplyr::recode(df_FRich$Category,
                            "Close" = "Close (<1000m)",
                            "Medium" = "Medium (1000-1600m)",
                            "Far" = "Far (>1600m)")

#Plot
ggplot(df_FRich, aes(x = Category, y = FRich, group = Sex)) +
  geom_line(aes(color = Sex), linewidth = 1, linetype = 2) +
  geom_point(aes(fill = Sex, shape = Sex), size = 6, color = "black", alpha = 0.6) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries",
       y = "Functional Richness") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 10, color = "black")
        , legend.position = "none")

ggsave("figures/FTS_Morphology.pdf", height = 15, width = 15, units = "cm")

#Finish
rm(disp, hull_female, hull_male, mod, nmds_female, nmds_male, nmds_scores_female,
   nmds_scores_male, permanova_female, permanova_male, w_morpho_female, w_morpho_male,
   w_morpho2, w_morpho_matrix_female_hel, w_morpho_matrix_male_hel, f, vars,
   results, dat, pca, fts, df_FRich, cat, frich_vals, sex, morpho_results, lm1, df,
   tmp)
dev.off()
beep()
cat("Models on morphology done.\n\n\n")





#---------------------------------------------------------------------------#





#4 - CHC ----

#Add distance
chc <- chc %>% 
  left_join(metad %>% select(Site, Distance), by = "Site")


##4.1 - Linear models ----
vars <- c("Shannon", "Richness", "Saturation", "MCL", "Linear_alk", "Met_alk")

chc_results <- lapply(vars, function(v){
  f <- as.formula(paste0("`", v, "` ~ Distance * Sex + (1|Site)"))
  mod <- glmmTMB(f, data = chc, family = gaussian())
  
  an <- Anova(mod, type = 2)
  
  tidy(an) %>%
    transmute(
      variable = v,
      term     = term,
      statistic= statistic,
      df       = df,
      p.value  = p.value
    )
}) %>% bind_rows()



#Export results
write.xlsx(chc_results, "results/LMM-CHC.xlsx")

#Export summary statistics
tmp <- chc[,c(6, 8:82)]

table <- tmp %>%
  group_by(Sex) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    -Sex,
    names_to = c("Variable", ".value"),
    names_pattern = "(.+)_(mean|se)$"
  ) %>%
  mutate(mean_se = sprintf("%.3f ± %.3f", mean, se)) %>%
  select(Sex, Variable, mean_se) %>%
  pivot_wider(names_from = Variable, values_from = mean_se)

write.xlsx(table, "results/Summary-CHC.xlsx")




#Plots
ggplot(chc, aes(x = Distance, y = Shannon, fill = Sex, shape = Sex)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(aes(color = Sex), method = "lm", se = T, linewidth = 1, alpha = 0.1) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "Shannon of CHC profile") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")
ggsave("figures/Shannon_CHC_Distance.pdf", height = 15, width = 15, units = "cm")


ggplot(chc, aes(x = Distance, y = MCL, fill = Sex, shape = Sex)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(aes(color = Sex), method = "lm", se = T, linewidth = 1, alpha = 0.1) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "Mean Chain Length") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")
ggsave("figures/MCL_CHC_Distance.pdf", height = 15, width = 15, units = "cm")


ggplot(chc, aes(x = Distance, y = Linear_alk, fill = Sex, shape = Sex)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(aes(color = Sex), method = "lm", se = T, linewidth = 1, alpha = 0.1) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "Linear alkanes %") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")
ggsave("figures/LinAlk_CHC_Distance.pdf", height = 15, width = 15, units = "cm")


ggplot(chc, aes(x = Distance, y = Met_alk, fill = Sex, shape = Sex)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(aes(color = Sex), method = "lm", se = T, linewidth = 1, alpha = 0.1) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "Methyl-branched alkanes %") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")
ggsave("figures/MetAlk_CHC_Distance.pdf", height = 15, width = 15, units = "cm")




##4.2 - NMDS ----

#Create the matrix
chc_male <- chc %>% 
  filter(Sex == "M") %>%
  select(14:82)
chc_male <- chc_male[, colSums(chc_male, na.rm = T) != 0]
chc_matrix_male_hel <- decostand(chc_male, method = "hellinger")


chc_female <- chc %>% 
  filter(Sex == "F") %>%
  select(14:82)
chc_female <- chc_female[, colSums(chc_female, na.rm = T) != 0]
chc_matrix_female_hel <- decostand(chc_female, method = "hellinger")



#Run NMDS
set.seed(123)

nmds_male <- metaMDS(chc_matrix_male_hel, distance = "bray", k = 2, try = 200, trymax = 999)
nmds_male$stress

nmds_female <- metaMDS(chc_matrix_female_hel, distance = "bray", k = 2, try = 200, trymax = 999)
nmds_female$stress

nmds_scores_male <- as.data.frame(scores(nmds_male, display = "sites"))
nmds_scores_male$Distance <- chc %>%
  filter(Sex == "M") %>%
  pull(Distance)
nmds_scores_male$Distance_f <- factor(nmds_scores_male$Distance, levels = as.numeric(names(distance_p)))

nmds_scores_female <- as.data.frame(scores(nmds_female, display = "sites"))
nmds_scores_female$Distance <- chc %>%
  filter(Sex == "F") %>%
  pull(Distance)
nmds_scores_female$Distance_f <- factor(nmds_scores_female$Distance, levels = as.numeric(names(distance_p)))

#Hull polygons
get_hull <- function(df, group_var){
  df[chull(df$NMDS1, df$NMDS2), ]
}

hull_male <- nmds_scores_male %>%
  group_by(Distance_f) %>%
  group_modify(~ get_hull(.x, "Distance_f"))

hull_female <- nmds_scores_female %>%
  group_by(Distance_f) %>%
  group_modify(~ get_hull(.x, "Distance_f"))


#Plot
ggplot(nmds_scores_male, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(fill = Distance_f), shape = 21, size = 2, alpha = 0.5) +
  geom_polygon(data = hull_male, aes(x = NMDS1, y = NMDS2, color = Distance_f),
               alpha = 0) +
  theme_bw() +
  labs(x = "NMDS1",
       y = "NMDS2") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none") +
  scale_fill_manual(values = distance_p) +
  scale_colour_manual(values = distance_p)

ggsave("figures/NMDS_CHC_M.pdf", height = 15, width = 15, units = "cm")


ggplot(nmds_scores_female, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(fill = Distance_f), shape = 24, size = 2, alpha = 0.5) +
  geom_polygon(data = hull_female, aes(x = NMDS1, y = NMDS2, color = Distance_f),
               alpha = 0) +
  theme_bw() +
  labs(x = "NMDS1",
       y = "NMDS2") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none") +
  scale_fill_manual(values = distance_p) +
  scale_colour_manual(values = distance_p)

ggsave("figures/NMDS_CHC_F.pdf", height = 15, width = 15, units = "cm")



##4.3 - Permanova ----
permanova_male <- adonis2(chc_matrix_male_hel ~ Distance, data = nmds_scores_male ,
                          method = "euclidean", permutations = 9999)

disp <- betadisper(vegdist(chc_matrix_male_hel, method = "euclidean"), nmds_scores_male$Distance)

sink("results/PERMANOVA_CHC_M.txt")
anova(disp)
permutest(disp)
TukeyHSD(disp)
tapply(disp$distances, nmds_scores_male$Distance, mean)
permanova_male #P = 0.007
sink()



permanova_female <- adonis2(chc_matrix_female_hel ~ Distance, data = nmds_scores_female ,
                            method = "euclidean", permutations = 9999)

disp <- betadisper(vegdist(chc_matrix_female_hel, method = "euclidean"), nmds_scores_female$Distance)

sink("results/PERMANOVA_CHC_F.txt")
anova(disp)
permutest(disp)
TukeyHSD(disp)
tapply(disp$distances, nmds_scores_female$Distance, mean)
permanova_female #P = 0.004
sink()


##4.4 - Simper ----
dist_M <- chc %>% filter(Sex == "M") %>% pull(Distance)
sim_M <- simper(chc_male, permutations = 9999)
list <- summary(sim_M, ordered = T)
res_M <- map_df(names(list), function(comparison) {
  df <- as.data.frame(list[[comparison]])
  df$Comparison <- comparison
  df$Compound <- rownames(df)
  return(df)
}) %>%
  filter(average != 0) %>%
  group_by(Comparison) %>%
  mutate(perc_contrib = 100 * average / sum(average)) %>%
  ungroup()

res_M <- as.data.frame(res_M)



dist_F <- chc %>% filter(Sex == "F") %>% pull(Distance)
sim_F <- simper(chc_female, permutations = 9999)
list <- summary(sim_F, ordered = T)
res_F <- map_df(names(list), function(comparison) {
  df <- as.data.frame(list[[comparison]])
  df$Comparison <- comparison
  df$Compound <- rownames(df)
  return(df)
}) %>%
  filter(average != 0) %>%
  group_by(Comparison) %>%
  mutate(perc_contrib = 100 * average / sum(average)) %>%
  ungroup()

res_F <- as.data.frame(res_F)


#Export results
write.xlsx(res_M, "results/SIMPER_M.xlsx", row.names = F)
write.xlsx(res_F, "results/SIMPER_F.xlsx", row.names = F)


##4.5 - Linear models on most important compounds ----

#Males
vars <- c("C27ene_01", "C25", "C27", "C23", "C29ene_01") #Top-5 for males

chc_m <- lapply(vars, function(v){
  f <- as.formula(paste0("`", v, "` ~ Distance + (1|Site)"))
  mod <- glmmTMB(f, data = subset(chc, Sex == "M"), family = gaussian())
  
  tidy(mod, effects = "fixed") %>%
    filter(term == "Distance") %>%
    transmute(
      variable = v,
      term     = term,
      estimate = estimate,
      std.error= std.error,
      z.value  = statistic,
      p.value  = p.value
    )
}) %>% bind_rows()


#Females
vars <- c("C25", "C27", "3-MeC25", "13-MeC29", "C26") #Top-5 for females

chc_f <- lapply(vars, function(v){
  f <- as.formula(paste0("`", v, "` ~ Distance + (1|Site)"))
  mod <- glmmTMB(f, data = subset(chc, Sex == "F"), family = gaussian())
  
  tidy(mod, effects = "fixed") %>%
    filter(term == "Distance") %>%
    transmute(
      variable = v,
      term     = term,
      estimate = estimate,
      std.error= std.error,
      z.value  = statistic,
      p.value  = p.value
    )
}) %>% bind_rows()



#Export results
write.xlsx(chc_m, "results/LMM-SIMPER-M.xlsx")
write.xlsx(chc_f, "results/LMM-SIMPER-F.xlsx")



#Plots
ggplot(chc, aes(x = Distance, y = C25, fill = Sex, shape = Sex)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(aes(color = Sex), method = "lm", se = T, linewidth = 1, alpha = 0.1) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "C25 %") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")
ggsave("figures/C25_Distance.pdf", height = 15, width = 15, units = "cm")


ggplot(chc, aes(x = Distance, y = C27, fill = Sex, shape = Sex)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_smooth(aes(color = Sex), method = "lm", se = T, linewidth = 1, alpha = 0.1) +
  scale_fill_manual(values = sex_p) +
  scale_color_manual(values = sex_p) +
  scale_shape_manual(values = sex_shape) +
  theme_bw() +
  labs(x = "Distance to apiaries (m)",
       y = "C27 %") +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")
ggsave("figures/C27_Distance.pdf", height = 15, width = 15, units = "cm")






#Finish
rm(disp, hull_female, hull_male, nmds_female, nmds_male, nmds_scores_female, nmds_scores_male,
   permanova_female, permanova_male, chc_female, chc_male, chc_matrix_female_hel, list,
   chc_matrix_male_hel, f, vars, res_F, res_M, sim_F, sim_M, dist_F, dist_M, chc_f,
   chc_m, chc_results)
dev.off()
beep()
cat("Models on CHC done.\n\n\n")





#---------------------------------------------------------------------------#





#5 - FLORAL COMPETITION ----

floral$Species <- as.factor(floral$Species)

#Remove bees not collected on flowers
floral_clean <- floral %>% filter(Plant != "Vegetation")
floral_clean <- floral_clean %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))

#Bipartite networks
web2019 <- floral_clean %>%
  filter(Year == 2019) %>%
  with(table(Species, Plant)) %>%
  as.matrix()

web2020 <- floral_clean %>%
  filter(Year == 2020) %>%
  with(table(Species, Plant)) %>%
  as.matrix()

pdf("figures/network2019.pdf", width = 11, height = 3)
plotweb(web2019,
        sorting = "normal",
        higher_color = "lightgreen",
        lower_color = "lightblue",
        higher_border = "black",
        lower_border = "black",
        link_color = "grey70",
        text_size = 0.7,
        srt = -90)
dev.off()

pdf("figures/network2020.pdf", width = 11, height = 3)
plotweb(web2020,
        sorting = "normal",
        higher_color = "lightgreen",
        lower_color = "lightblue",
        higher_border = "black",
        lower_border = "black",
        link_color = "grey70",
        text_size = 0.7,
        srt = -90)
dev.off()

#Resource overlap
mat2019 <- t(web2019)
mat2020 <- t(web2020)

#Niche overlap
niche.overlap(mat2019, method = "pianka")
niche.overlap(mat2020, method = "pianka")


#Overlap site x year
overlap_df <- floral_clean %>%
  count(Year, Site, Date, Distance, Hives, Species, Plant) %>%
  group_split(Year, Site, Date) %>%
  map_dfr(function(x){
    
    if(n_distinct(x$Species) < 2) return(NULL)
    
    mat <- x %>%
      select(Species, Plant, n) %>%
      pivot_wider(
        names_from = Plant,
        values_from = n,
        values_fn = sum,
        values_fill = 0
      )
    
    spp <- mat$Species
    mat <- as.matrix(mat[, -1, drop = F])
    rownames(mat) <- spp
    
    if(ncol(mat) < 2) return(NULL)
    
    pianka_val <- tryCatch(
      as.matrix(niche.overlap(mat, method = "pianka"))[1, 2],
      error = function(e) NA_real_
    )
    
    data.frame(
      Year = unique(x$Year),
      Site = unique(x$Site),
      Date = unique(x$Date),
      Distance = unique(x$Distance),
      Hives = unique(x$Hives),
      Pianka = pianka_val
    )
  })

#Overlap vs. Distance
cor.test(overlap_df$Pianka, overlap_df$Distance, method = "spearman")

#Overlap vs. Active beehives
cor.test(overlap_df$Pianka, overlap_df$Hives, method = "spearman")




#---------------------------------------------------------------------------#





#FINISH
gc()
beep(3)
.end_time <- Sys.time()
elapsed <- difftime(.end_time, .start_time, units = "mins")
cat("Script run in", round(elapsed, 3), "minutes \n\n\n")
rm(list = ls())