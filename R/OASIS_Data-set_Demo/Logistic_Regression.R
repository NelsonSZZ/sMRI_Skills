# Logistic Regression

table(clinical$demented) # This is to check the balance of the data before modeling
model_logit <- glm(demented ~ nWBV + Age + gender,
                   data = clinical,
                   family = binomial)

summary(model_logit)

"It is imortant to note that the coefficient here means nothing intuitively,
always exponentiate it to get odds ratio before intepreting"

exp(cbind(OR = coef(model_logit), confint(model_logit)))


# Overall Model Fit

pchisq(model_logit$null.deviance - model_logit$deviance,
       df = model_logit$df.null - model_logit$df.residual,
       lower.tail = FALSE) # p < 0.001 means model significantly better than the mean

# Model Comparison

m1 <- glm(demented ~ nWBV + Age, 
          data = clinical, family = binomial)
m2 <- glm(demented ~ nWBV + Age + gender,
          data = clinical, family = binomial)

anova (m1, m2,  test = "Chisq") # Must specify
AIC(m1, m2)
 



