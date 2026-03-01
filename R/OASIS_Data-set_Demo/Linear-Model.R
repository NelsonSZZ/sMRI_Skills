# Linear Model

# Creating a subset with no NAs in any variable that we are planning to see

oasis_complete <- oasis_clean %>%
  filter(!is.na(Educ) & !is.na(gender) & !is.na(nWBV) & !is.na(Age))

# Checking how many rows remains after excluding NA's for comparison

nrow(oasis_complete)

"Fit the model:
'lm()' can be used when the outcome is a continuous,
if the outcome is binary use 'glm()'.
If you have repeated measures we would need 'lmer()'"

"Coefficients (how big is the effect), standard error (how certain),
t-value (how many standard errors away from zero),
p-value (how likely is this by chance, 
R^2 (how much of the ourcome's variance does our model explain)"

model_age  <- lm(nWBV ~ Age, data = oasis_complete)
model_full <- lm(nWBV ~ Age + gender + Educ, data = oasis_complete)

# Plotting the new graph

ggplot(oasis_complete, aes(x = Age, y = nWBV)) +
  geom_point(alpha = 0.4,color = 'blue') +
  geom_smooth(method = 'lm', color = 'red', se = TRUE ) +
  labs(title = 'Brain Volume Declined With Age',
       x = 'Age (Years)', y = 'Normalised Whole Brain Volume') +
  theme_minimal()

nobs(model_age)   # should be identical
nobs(model_full)  # should be identical

summary(model_age)
summary(model_full)


"We compare the Age coefficient before and after adding the covariates -
such as gender, education. 

If the coefficient barely changes, then it means Age is a clean predictor to begin with - 
this process is called 'checking for confounding'"

coef(model_age)['Age']
coef(model_full)['Age']
