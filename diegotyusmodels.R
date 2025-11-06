# Exploration September 16th 2025

# Today we tested biology (spring migraiton and functional diversity) as well as the digital lxuury effect (number of obs and number of checklists)

# Next steps from Tyus ends:
# Make a water percent species association
# Get NDVI year round
# Diego in late november gets distance2coastline






# --- --- --- --- --- --- --- --- --- --- --- ---
# [1] Testing spring migration
# --- --- --- --- --- --- --- --- --- --- --- ---



#  Spring migraiton model ####

# Spring migration is negatively associated

# Next steps: Show savannah and turn into a mixed effect model? What is the random effect?



spring_migration_model =  lm(sp_migrate_mean ~imp_surf + medincome_mean+
                               pcnt_understory+pcnt_forest, # +nitelite_mean,
                             data = ebird_scaled)


library(corrplot)

predictors <- ebird_scaled[, c("sp_migrate_mean", "imp_surf", "medincome_mean", 
                               "pcnt_understory", "pcnt_forest", "pcnt_urban")]

# Compute the correlation matrix
cor_matrix <- cor(predictors, use = "complete.obs")

# Make a corrplot
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", # adds correlation values
         diag = FALSE)


tab_model(spring_migration_model)
# Interpretation:
# Spring migraiton abundance is positively significantly associated with areas with higher amounts of understory and forest specialists
# Negatively associated with impervious surface
plot_model(spring_migration_model, 'pred')

# [1] Focus on the spring migration model ####



# Next steps we look a step within the fucntional diversity of these birds

# --- --- --- --- --- --- --- --- --- --- --- ---
# [2] Testing functional diversity
# --- --- --- --- --- --- --- --- --- --- --- ---


# We expect urban birds to not be significantly associated with the luxury effect, but positively respond to impervious surface
urban_bird_model =  lm(pcnt_urban ~imp_surf + medincome_mean * ndviwet_mean+elev_mean
                               , # +nitelite_mean,
                             data = ebird_scaled)
tab_model(urban_bird_model)

forest_birds_model =  lm(pcnt_forest ~imp_surf + medincome_mean * ndviwet_mean+elev_mean, # +nitelite_mean,
                       data = ebird_scaled)
tab_model(forest_birds_model)


understory_birds_model =  lm(pcnt_understory ~imp_surf + medincome_mean * ndviwet_mean+elev_mean, # +nitelite_mean,
                         data = ebird_scaled)
tab_model(understory_birds_model)

names(ebird_scaled)

# [2] Provide interpretation to the functional traits urban-forest-understory-water ####


# --- --- --- --- --- --- --- --- --- --- --- ---
# testing the digital luxury effect
# --- --- --- --- --- --- --- --- --- --- --- ---

# [3] Provide interpretation on the digital luxury effect being super complicated in a urban biodiversity hotspot thats not exclusively water limited ####

require(sjPlot)
require(lme4)
require(corrplot)

# --- --- --- --- --- --- --- --- --- --- --- ---
# All observations
# --- --- --- --- --- --- --- --- --- --- --- ---

# Lets model patterns of data, little to do with biology
# Testing the luxury effect in presence only data
# testing_luxury_eff = glm(num_bird_obvs ~elev_mean +  imp_surf + medincome_mean + pop_density_mean,
#                          data = ebird_scaled)

testing_luxury_eff = gam(num_bird_obvs ~ s(elev_mean) +  s(imp_surf) + s(medincome_mean) + s(ndvidry_mean),
                        data = ebird_scaled)


testing_luxury_eff = lm(num_bird_obvs ~elev_mean +  imp_surf + medincome_mean * ndvidry_mean,
                         data = ebird_scaled)
tab_model(testing_luxury_eff)
plot_model(testing_luxury_eff, type = "pred")
# testing_luxury_eff = gam(num_bird_obvs ~elev_mean +  imp_surf + medincome_mean + pop_density_mean,
#                          data = ebird_scaled)


ggplot(ebird_eastbay_tests,aes(x=medincome_mean,y=num_records))+geom_smooth()


# Number of checklists
require(mgcv)

# --- --- --- --- --- --- --- --- --- --- --- ---
# Complete checklists
# --- --- --- --- --- --- --- --- --- --- --- ---


test_luxur_eff_complete_checklists = gam(num_records ~elev_mean +  imp_surf + pop_density_mean
                                         + ndviwet_mean + medincome_mean * nitelite_mean + bio1_temp_mean + bio12_rain_mean,
                                         data = ebird_scaled)


tab_model(test_luxur_eff_complete_checklists)



# --- --- --- --- --- --- --- ---
# This is a solid chunck = End here
# --- --- --- --- --- --- --- ---









# Extra exploration graveyard #####





lm_model_luxury <- lm(num_records ~ ndviwet_mean + nitelite_mean + 
                  housing_density_mean + bio1_temp_mean + nitelite_mean * medincome_mean, data = ebird_scaled)

AIC(test_luxur_eff_complete_checklists, lm_model_luxury)

tab_model(lm_model_luxury)



# Lets model biology
# Number of total species
num_species_model =  lm(num_species ~elev_mean +  imp_surf * medincome_mean + pop_density_mean,
     data = ebird_scaled)

tab_model(num_species_model)

# Number of species / species richness
num_species_model =  lm(num_species ~elev_mean +  imp_surf + pop_density_mean+
                          ndviwet_mean * medincome_mean +
                        nitelite_mean + bio1_temp_mean + bio12_rain_mean,
                        data = ebird_scaled)

tab_model(num_species_model)


# Migration  Full model
num_migration_model =  lm(num_migr_sp ~elev_mean +  imp_surf + pop_density_mean+
                          ndviwet_mean * medincome_mean +
                          nitelite_mean + bio1_temp_mean + bio12_rain_mean,
                        data = ebird_scaled)



# Migration  Full model
num_migration_model =  lm(num_migr_sp ~elev_mean +  imp_surf + 
                            ndviwet_mean * medincome_mean +
                            nitelite_mean + bio1_temp_mean + bio12_rain_mean,
                          data = ebird_scaled)
tab_model(num_migration_model)


# For now lets stick to bird migraiton intensity/biomass per Horton et al.
# Migration is a functional trait
num_migration_model_spring =  lm(sp_migrate_mean ~elev_mean + imp_surf+
                            ndviwet_mean + medincome_mean + pcnt_understory + pcnt_forest +
                            nitelite_mean,
                          data = ebird_scaled)
tab_model(num_migration_model_spring)
plot_model(num_migration_model_spring, 'pred')





number_migratory_species =  lm(num_migr_sp ~imp_surf+pcnt_understory+pcnt_forest+nitelite_mean
                               + medincome_mean,
                                 data = ebird_scaled)

tab_model(number_migratory_species)
plot_model(num_migration_model_spring, 'pred')

# medincome_mean


lm_model2 <- lm(num_migr_sp ~ ndviwet_mean + nitelite_mean + 
                  housing_density_mean + bio1_temp_mean + sp_migrate_mean + nitelite_mean * medincome_mean, data = ebird_scaled)

AIC(num_migration_model,lm_model2 ) # compete against each other suckers

summary(lm_model2)
tab_model(lm_model2)
plot_model(lm_model2, 'pred')

# number of resident species full model
resident_birds <- gam(num_resid_sp ~ s(ndviwet_mean) + s(nitelite_mean) + s(medincome_mean) +
     s(housing_density_mean) + s(bio1_temp_mean) + s(sp_migrate_mean), data = ebird_scaled)
summary(resident_birds)
tab_model(resident_birds)
plot_model(resident_birds, 'pred')



# make sure to scale the vars to reduce error #
scaled.vars = as.data.frame(scale(uwin_vars_anno[9:21], scale = TRUE)) # looks good

cor.matrix <- cor(ebird_scaled[,c(7:23)], use = "complete.obs", method = "pearson") # look at the correlation estimate

print(cor.matrix) # this looks much better, now in matrix format

# make the matrix plot
corrplot::corrplot(cor.matrix, method = "square", 
                   type = "full",
                   addCoef.col = "black",
                   addCoefasPercent = FALSE,
                   outline = TRUE,
                   insig = "pch",
                   p.mat = NULL,
                   tl.col = "black",
                   tl.srt = 45) # this looks fine will improve later


names(ebird_scaled)