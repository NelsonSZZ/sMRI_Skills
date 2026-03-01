# Does Dementia Severity Predict Brain Volume Loss?

"Clinical Question: 
Above and beyond normal aging, does dementia severity predicts Brain Volume Loss (continuous outcome)?"

clinical <- oasis %>% filter (!is.na(CDR))
nrow(clinical)
table(clinical$CDR_f)


model_cdr <- lm(nWBV ~ CDR_f + Age + gender, data = clinical)
summary(model_cdr)

# Checking assumptions and multi-collinearity

par(mfrow = c(2,2))
plot(model_cdr)
par(mfrow = c(1,1))

vif(model_cdr)


"Model comparison - Is CDR worth including into the linear model"

model_no_cdr <- lm(nWBV ~ Age + gender, data = clinical)
model_with_cdr <- lm(nWBV ~ Age + gender + CDR_f, data = clinical)

# Always verify same rows (Important)
 
nobs(model_no_cdr)
nobs(model_with_cdr)

anova (model_no_cdr, model_with_cdr)
AIC(model_no_cdr, model_with_cdr)
