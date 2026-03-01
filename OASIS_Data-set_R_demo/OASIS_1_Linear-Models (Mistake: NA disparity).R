# Linear Model

# We start off by sub-setting out healthy subjects

healthy <- oasis_clean %>% filter(CDR == 0 | is.na(CDR))

"Fit the model:
'lm()' can be used when the outcome is a continuous,
if the outcome is binary use 'glm()'.
If you have repeated measures we would need 'lmer()'"

"Coefficients (how big is the effect), standard error (how certain),
t-value (how many standard errors away from zero),
p-value (how likely is this by chance, 
R^2 (how much of the ourcome's variance does our model explain)"

model_age <- lm(nWBV ~ Age, data = healthy)
summary(model_age)


# Visualization (Important)
ggplot(healthy, aes(x =Age, y = nWBV)) +
  geom_point(alpha = 0.4,color = 'blue') +
  geom_smooth(method = 'lm', color = 'red', se = TRUE ) +
  labs(title = 'Brain Volume Declined With Age',
       x = 'Age (Years)', y = 'Normalised Whole Brain Volume') +
  theme_minimal()

# Adding in Covariables

model_full <- lm(nWBV ~ Age + gender + Educ, data = healthy)
summary(model_full)

"We compare the Age coefficient before and after adding the covariates -
such as gender, education. 

If the coefficient barely changes, then it means Age is a clean predictor to begin with - 
this process is called 'checking for confounding'"

coef(model_age)['Age'] # From simple model
coef(model_full)['Age'] # From adjusted model 

# The disparity in the 

nobs(model_age) # Shows 336 - Certainly age ranges from 18-96 (full lifespan)
nobs(model_full) # Shows 135 - Certainly older adults

"The comparison likely arised from the effect of accidentally switching from young + old sample
to old sample. Therefore we need to siphon out the 'NA' from any variable we plan on using and rerun the lm()"

