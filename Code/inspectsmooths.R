library(mgcv)
library(ggplot2)

#Some fake data
n_observations <- 100

covs <- data.frame(
  a = runif(n_observations, 0, 10),
  b = rnorm(n_observations, 0, 5),
  c = runif(n_observations, -10,10),
  d = rnorm(n_observations, 3, 1),
  e = sample(c(0,1), n_observations, replace = T)
)

#We need to generate some observations of data that are a funciton
#of some subset of the covariates just so this works for the example.
# In reality, you probably won't know the functional form of the data
#generating process.
y <- (
  covs$a * 5 + covs$a^2*2 - covs$a^3 +
  covs$b*4 +
  covs$d^4 * covs$e + -covs$d^2 * (1-covs$e) 
) 

my_data <- cbind(covs, y)

# Fit with a GAM
#ensure all covariates are scaled (use ebird_scaled file)
my_data$a_scaled <- scale(my_data$a)
my_data$b_scaled <- scale(my_data$b)
my_data$c_scaled <- scale(my_data$c)
my_data$d_scaled <- scale(my_data$d)
my_data$e_scaled <- scale(my_data$e)

gam_mod <- gam(num_migr_sp ~ s(medincome_mean) +
                 s(housing_density_mean) + 
                 s(nitelite_mean) + 
                 s(ndviwet_mean) + s(bio1_temp_mean), 
               data = ebird_scaled)
#note the interaction between d and e includes a baseline term for d
# AND an interaction term that predicts the additional deviations from the
#baseline smooth for d caused by e

#scatter plots (data only)
ggplot() + 
  geom_point(data = ebird_scaled, mapping = aes(x = medincome_mean, y = num_migr_sp)) +
  theme_bw()

ggplot() + 
  geom_point(data = ebird_scaled, mapping = aes(x = housing_density_mean, y = num_migr_sp)) +
  theme_bw() # four outlying points of data, why is this the case? 12,088 houses km^2 is a lot and it could be because of intersecting housing block groups

ggplot() + 
  geom_point(data = ebird_scaled, mapping = aes(x = nitelite_mean, y = num_migr_sp)) +
  theme_bw()

ggplot() + 
  geom_point(data = ebird_scaled, mapping = aes(x = ndviwet_mean, y = num_migr_sp)) +
  theme_bw()

# Create predictions from gam
# (note we just want to span all of the covariate space)
span_covariates <- data.frame(
  a = seq(min(ebird_scaled$medincome_mean, na.rm = TRUE), max(ebird_scaled$medincome_mean, na.rm = TRUE), length.out = 1132),
  b = seq(min(ebird_scaled$housing_density_mean, na.rm = TRUE), max(ebird_scaled$housing_density_mean, na.rm = TRUE), length.out = 1132),
  c = seq(min(ebird_scaled$ndviwet_mean, na.rm = TRUE), max(ebird_scaled$ndviwet_mean, na.rm = TRUE), length.out = 1132),
  d = seq(min(ebird_scaled$bio1_temp_mean, na.rm = TRUE), max(ebird_scaled$bio1_temp_mean, na.rm = TRUE), length.out = 1132),
  e = seq(min(ebird_scaled$nitelite_mean, na.rm = TRUE), max(ebird_scaled$nitelite_mean, na.rm = TRUE), length.out = 1132)
)

ggplot() +
  geom_point(data = ebird_scaled, mapping = aes(x = medincome_mean, y = num_migr_sp), color = "forestgreen") +
  geom_line(data = data.frame(a = span_covariates$a,
                              y = predict(gam_mod, #we just want to predict
                                          #values of y for the whole space of a,
                                          #so we use the predict function for a data
                                          #frame where only a has values that span
                                          # its space and everything else is 0 (the mean
                                          #of the scaled covariates)
                                          newdata = data.frame(medincome_mean = span_covariates$a,
                                                               housing_density_mean = 0,
                                                               nitelite_mean = 0, 
                                                               ndviwet_mean = 0, 
                                                               bio1_temp_mean = 0),
                                          type = "response"
                              )), 
            mapping = aes(x = a, y = y), color = "red") # not a noticeable pattern

ggplot() +
  geom_point(data = ebird_scaled, mapping = aes(x = housing_density_mean, y = num_migr_sp)) +
  geom_line(data = data.frame(b = span_covariates$b,
                              y = predict(gam_mod, 
                                          newdata = data.frame(housing_density_mean = span_covariates$b,
                                                               medincome_mean = 0,
                                                               nitelite_mean = 0, 
                                                               ndviwet_mean = 0, 
                                                               bio1_temp_mean = 0),
                                          type = "response"
                              )), 
            mapping = aes(x = b, y = y), color = "red") # might omit outliers

ggplot() +
  geom_point(data = ebird_scaled, mapping = aes(x = ndviwet_mean, y = num_migr_sp)) +
  geom_line(data = data.frame(c = span_covariates$c,
                              y = predict(gam_mod, 
                                          newdata = data.frame(ndviwet_mean = span_covariates$c,
                                                               nitelite_mean = 0,
                                                               medincome_mean = 0, 
                                                               bio1_temp_mean = 0, 
                                                               housing_density_mean = 0),
                                          type = "response"
                              )), 
            mapping = aes(x = c, y = y), color = "red")

ggplot() +
  geom_point(data = ebird_scaled, mapping = aes(x = bio1_temp_mean, y = num_migr_sp)) +
  geom_line(data = data.frame(d = span_covariates$d,
                              y = predict(gam_mod, 
                                          newdata = data.frame(bio1_temp_mean = span_covariates$d,
                                                               nitelite_mean = 0,
                                                               medincome_mean = 0, 
                                                               ndviwet_mean = 0, 
                                                               housing_density_mean = 0),
                                          type = "response"
                              )), 
            mapping = aes(x = d, y = y), color = "red") # 95% CI probably high

ggplot() +
  geom_point(data = ebird_scaled, mapping = aes(x = nitelite_mean, y = num_migr_sp)) +
  geom_line(data = data.frame(e = span_covariates$e,
                              y = predict(gam_mod, 
                                          newdata = data.frame(nitelite_mean = span_covariates$e,
                                                               ndviwet_mean = 0,
                                                               medincome_mean = 0, 
                                                               bio1_temp_mean = 0, 
                                                               housing_density_mean = 0),
                                          type = "response"
                              )), 
            mapping = aes(x = e, y = y), color = "red")

#d and e interact, so we create predicted values for a dataframe
#that has combinations of both
newdata_de <- expand.grid(a_scaled = 0,
                          b_scaled = 0,
                          c_scaled = 0, 
                          d_scaled = span_covariates$d, #span of d space
                          e_scaled = c(min(my_data$e_scaled),max(my_data$e_scaled))) #possible values of e
newdata_de$y = predict(gam_mod, 
                       newdata = newdata_de,
                       type = "response"
)

ggplot() +
  geom_point(data = my_data, mapping = aes(x = d_scaled, y = y, 
                                           color = factor(e_scaled), group = factor(e_scaled))) +
  geom_line(data = newdata_de, 
            mapping = aes(x = d_scaled, y = y, color = factor(e_scaled), group = factor(e_scaled)))

