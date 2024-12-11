install.packages("haven")
library(haven)

data <- read_dta("guns.dta")

library(plm)
library(lmtest)

#log-transform
data$lvio <- log(data$vio)
data$lmur <- log(data$mur)
data$lrob <- log(data$rob)
data$lincarc_rate <- log(data$incarc_rate)
pdata <- pdata.frame(data, index = c("stateid", "year"))

### VIOLENT CRIME AS DEPENDENT

# Pooled OLS
pooled_ols <- plm(lvio ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064,
                  data = pdata,
                  model = "pooling")

# Summary of the model
summary(pooled_ols)

robust_se <- coeftest(pooled_ols, vcov = vcovHC(pooled_ols, method = "arellano"))

# Display the results
print(robust_se)

# FIXED EFFECTS

fixed_effects <- plm(lvio ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064,
                     data = pdata, model = "within")  

# Summary of the Fixed Effects Model
summary(fixed_effects)

# Fixed Effects Model with Robust Standard Errors
robust_se_fe <- coeftest(fixed_effects, vcov = vcovHC(fixed_effects, method = "arellano"))

# Display the results
print(robust_se_fe)

##F test
pFtest(fixed_effects, pooled_ols)

## Time Fixed Effects

fixed_effects_time <- plm(lvio ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064 + factor(year),
                          data = pdata,
                          model = "within")
summary(fixed_effects_time)

library(sandwich)


# Summary with Robust Standard Errors
robust_se_summary <- coeftest(fixed_effects_time, vcov = vcovHC(fixed_effects_time, type = "HC1"))

# Print robust summary
print(robust_se_summary)


## F test for time dummies sig

# Calculate Residual Sum of Squares for both models
RSS_restricted <- sum(resid(fixed_effects)^2) # Restricted model
RSS_unrestricted <- sum(resid(fixed_effects_time)^2) # Unrestricted model

# Degrees of freedom for the F-test
q <- length(unique(pdata$year)) - 1  # Number of time dummies (excluding reference year)
n <- nrow(pdata)  # Number of observations
k <- length(coef(fixed_effects_time))  # Number of coefficients in the unrestricted model

# F-statistic for the test
F_stat <- ((RSS_restricted - RSS_unrestricted) / q) / (RSS_unrestricted / (n - k))

# Calculate p-value from the F-statistic
df1 <- q  # The difference in the number of parameters (time dummies)
df2 <- n - k  # The residual degrees of freedom in the unrestricted model
p_value <- pf(F_stat, df1, df2, lower.tail = FALSE)

# Output the results
cat("F-statistic:", F_stat, "\n")
cat("p-value:", p_value, "\n")

##Random Effects

random_effects_model <- plm(lvio ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064 + factor(year),
                            data = pdata,  
                            model = "random")

# Summarize the model
summary(random_effects_model)

# Calculate robust standard errors
robust_se_time <- vcovHC(random_effects_model, method = "arellano", type = "HC1")

# Summarize the model with robust standard errors
coeftest(random_effects_model, vcov = robust_se_time)

hausman_test <- phtest(fixed_effects_time, random_effects_model)
print(hausman_test)

######## Murder AS DEPENDENT

# Pooled OLS
pooled_ols2 <- plm(lmur ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064,
                  data = pdata,
                  model = "pooling")

# Summary of the model
summary(pooled_ols2)

robust_se2 <- coeftest(pooled_ols2, vcov = vcovHC(pooled_ols2, method = "arellano"))

# Display the results
print(robust_se2)

# FIXED EFFECTS

fixed_effects2 <- plm(lmur ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064,
                     data = pdata, model = "within")  

# Summary of the Fixed Effects Model
summary(fixed_effects2)

# Fixed Effects Model with Robust Standard Errors
robust_se_fe2 <- coeftest(fixed_effects2, vcov = vcovHC(fixed_effects2, method = "arellano"))

# Display the results
print(robust_se_fe2)

##F test pooled OLS and FE

pFtest(fixed_effects2, pooled_ols2)

## Time Fixed Effects

fixed_effects_time2 <- plm(lmur ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064 + factor(year),
                          data = pdata,
                          model = "within")
summary(fixed_effects_time2)

library(sandwich)


# Summary with Robust Standard Errors
robust_se_summary <- coeftest(fixed_effects_time2, vcov = vcovHC(fixed_effects_time2, type = "HC1"))

# Print robust summary
print(robust_se_summary)


## F test for time dummies sig

# Calculate Residual Sum of Squares for both models
RSS_restricted2 <- sum(resid(fixed_effects2)^2) # Restricted model
RSS_unrestricted2 <- sum(resid(fixed_effects_time2)^2) # Unrestricted model

# Degrees of freedom for the F-test
q2 <- length(unique(pdata$year)) - 1  
n2 <- nrow(pdata)  
k2 <- length(coef(fixed_effects_time2))  

# F-statistic for the test
F_stat2 <- ((RSS_restricted2 - RSS_unrestricted2) / q2) / (RSS_unrestricted2 / (n2 - k2))

# Calculate p-value from the F-statistic
df1 <- q2  # The difference in the number of parameters (time dummies)
df2 <- n2 - k2  # The residual degrees of freedom in the unrestricted model
p_value <- pf(F_stat2, df1, df2, lower.tail = FALSE)

# Output the results
cat("F-statistic:", F_stat2, "\n")
cat("p-value:", p_value, "\n")

##Random Effects

random_effects_model2 <- plm(lmur ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064 + factor(year),
                            data = pdata,  
                            model = "random")

# Summarize the model
summary(random_effects_model2)

# Calculate robust standard errors
robust_se_time <- vcovHC(random_effects_model2, method = "arellano", type = "HC1")

# Summarize the model with robust standard errors
coeftest(random_effects_model2, vcov = robust_se_time)

hausman_test <- phtest(fixed_effects_time2, random_effects_model2)
print(hausman_test)



######## Robbery AS DEPENDENT

# Pooled OLS
pooled_ols3 <- plm(lrob ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064,
                   data = pdata,
                   model = "pooling")

# Summary of the model
summary(pooled_ols3)

robust_se3 <- coeftest(pooled_ols3, vcov = vcovHC(pooled_ols3, method = "arellano"))

# Display the results
print(robust_se3)

# FIXED EFFECTS

fixed_effects3 <- plm(lrob ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064,
                      data = pdata, model = "within")  

# Summary of the Fixed Effects Model
summary(fixed_effects3)

# Fixed Effects Model with Robust Standard Errors
robust_se_fe3 <- coeftest(fixed_effects3, vcov = vcovHC(fixed_effects3, method = "arellano"))

# Display the results
print(robust_se_fe3)

##F test pooled OLS and FE

pFtest(fixed_effects3, pooled_ols3)

## Time Fixed Effects

fixed_effects_time3 <- plm(lrob ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064 + factor(year),
                           data = pdata,
                           model = "within")
summary(fixed_effects_time3)


# Summary with Robust Standard Errors
robust_se_summary <- coeftest(fixed_effects_time3, vcov = vcovHC(fixed_effects_time3, type = "HC1"))

# Print robust summary
print(robust_se_summary)


## F test for time dummies sig

# Calculate Residual Sum of Squares for both models
RSS_restricted3 <- sum(resid(fixed_effects3)^2) # Restricted model
RSS_unrestricted3 <- sum(resid(fixed_effects_time3)^2) # Unrestricted model

# Degrees of freedom for the F-test
q3 <- length(unique(pdata$year)) - 1  
n3 <- nrow(pdata)  
k3 <- length(coef(fixed_effects_time3))  

# F-statistic for the test
F_stat3 <- ((RSS_restricted3 - RSS_unrestricted3) / q3) / (RSS_unrestricted3 / (n3 - k3))

# Calculate p-value from the F-statistic
df1 <- q3  # The difference in the number of parameters (time dummies)
df2 <- n3 - k3  # The residual degrees of freedom in the unrestricted model
p_value <- pf(F_stat3, df1, df2, lower.tail = FALSE)

# Output the results
cat("F-statistic:", F_stat3, "\n")
cat("p-value:", p_value, "\n")

##Random Effects

random_effects_model3 <- plm(lrob ~ shall + lincarc_rate + density + avginc + pm1029 + pw1064 + pb1064 + factor(year),
                             data = pdata,  
                             model = "random")

# Summarize the model
summary(random_effects_model3)

# Calculate robust standard errors
robust_se_time <- vcovHC(random_effects_model3, method = "arellano", type = "HC1")

# Summarize the model with robust standard errors
coeftest(random_effects_model3, vcov = robust_se_time)

hausman_test <- phtest(fixed_effects_time3, random_effects_model3)
print(hausman_test)









