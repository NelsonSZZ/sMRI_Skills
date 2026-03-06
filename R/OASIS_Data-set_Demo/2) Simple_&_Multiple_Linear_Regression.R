# Running Linear Regression
# Creating a subset of data without any NA's

oasis_complete <- oasis_clean %>%
  filter(!is.na(Educ) & !is.na(gender) & !is.na(nWBV) & !is.na(Age))

# Checking how many rows remains after excluding NA's for comparison
nrow(oasis_complete)

# Continuous outcome, independent observations → lm()
# Binary outcome → glm(..., family = binomial)
# Repeated measures / clustered data → mixed model:
  # continuous outcome → lmer()
  # binary outcome → glmer(..., family = binomial)

# Coefficients: expected change in outcome per 1-unit predictor increase (or vs reference group).
# Std. Error: precision of the coefficient estimate (smaller = more precise).
# t-value: Estimate / SE (how many SEs from 0).
# p-value: P(|t| as extreme) assuming H0: coefficient = 0 (two-sided).
# R^2: proportion of outcome variance explained by the model (in-sample).
# Adj R^2: R^2 adjusted for number of predictors (better for model comparison).
# F-test: overall test that all slopes = 0 (predictors add no explanatory power).

# Linear Regression

model_age  <- lm(nWBV ~ Age, data = oasis_complete)
model_full <- lm(nWBV ~ Age + gender + Educ, data = oasis_complete)

# Checking how many number of rows actually was used
# Needs to make sure they are identical, as there are many NA's data points (from younger participants)

nobs(model_age)
nobs(model_full)

summary(model_age)
summary(model_full)

# We compare the Age coefficient before and after adding the covariates - such as gender, education. 
# If the coefficient barely changes, then it means Age is a clean predictor to begin with - this process is called 'checking for confounding'"

coef(model_age)['Age']
coef(model_full)['Age']

# Plotting the graph

ggplot(oasis_complete, aes(x = Age, y = nWBV)) +
  geom_point(alpha = 0.4,color = 'blue') +
  geom_smooth(method = 'lm', color = 'red', se = TRUE ) +
  labs(title = 'Brain Volume Declined With Age',
       x = 'Age (Years)', y = 'Normalised Whole Brain Volume') +
  theme_minimal()

# We can therefore conclude - 
"Using oasis_complete (n = 235), two linear models were fitted predicting normalised whole brain
volume (nWBV). The Age-only model (model_age) explained 51.9% of variance (R² = 0.519).
Adding gender and education (model_full) improved this slightly (R² = 0.540). The Age
coefficient barely changed (-0.002854 vs -0.002821), confirming Age is a clean predictor
not confounded by gender or education. In model_full, Age (b = -0.00282, p < 0.001) and
gender (b = -0.01411, p = 0.002) were both significant — males show lower nWBV than females
independent of age. Education was not significant (p = 0.214). model_full is preferred as it
is more rigorous and accounts for known biological differences."


