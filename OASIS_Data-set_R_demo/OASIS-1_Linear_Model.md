# Intro

As a general note, every statistical test is a linear model asking: "does knowing X help me predict Y, beyond chance?" With this, all statistical test answers the same question, just different data shapes.

Instead of just a general correlational test, that tells you the strength of the relationship,
models shows you the size of that relationship.

# Scientific Question: Does Brain Volume Decline with Age? 

nWBV (normalised whole brain volume) as our outcome and Age as our predictor

```R
# Linear Model

# Creating a subset with no NAs in any variable that we are planning to see

oasis_complete <- oasis_clean %>%
  filter(!is.na(Educ) & !is.na(gender) & !is.na(nWBV) & !is.na(Age))

# Checking how many rows remains after excluding NA's for comparison

nrow(oasis_complete)

# After exclusion, we fit model

model_age2  <- lm(nWBV ~ Age, data = oasis_complete)
model_full2 <- lm(nWBV ~ Age + gender + Educ, data = oasis_complete)

# Plotting the new graph

ggplot(oasis_complete, aes(x = Age, y = nWBV)) +
  geom_point(alpha = 0.4,color = 'blue') +
  geom_smooth(method = 'lm', color = 'red', se = TRUE ) +
  labs(title = 'Brain Volume Declined With Age',
       x = 'Age (Years)', y = 'Normalised Whole Brain Volume') +
  theme_minimal()

nobs(model_age2)   # should be identical
nobs(model_full2)  # should be identical


summary(model_age2)
summary(model_full2)

# Now this comparison is valid
coef(model_age2)['Age']
coef(model_full2)['Age']

```

