install.packages(c("tidyverse", "lme4", "lmerTest", "car", "readxl"))

# Load packages

library(tidyverse) 
library(readxl)
library(lme4)
library(lmerTest)
library(car)
library(readxl)

# Load dataset
oasis <- read_excel("~/R/OASIS_R_practice/OASIS_Resources/oasis_cross_sectional.xlsx") # Read the data

# First look - Make sure to do this everytime before anything else

str(oasis)
summary(oasis) 
head(oasis, 11) # Returns the first 11 rows 

# Data preparation
# Rename gender column (2nd column) as the slash (M/F) can be awkward
oasis <- oasis %>% rename(gender = 2)


# We first need to convert categorical columns to factors, this makes sure that statistical models-
# interpret the variables as distinct groups,rather than a ranking (e.g., 0.5 -> 1 -> 2)

oasis <- oasis %>%
  mutate(
    gender = as.factor(gender),
    CDR_f = factor(CDR, levels = c(0, 0.5, 1, 2)), 
    Educ_f = factor(Educ, levels = 1:5),
    demented = ifelse(CDR > 0, 1, 0)  # We need to create a binary dementia variable for logistic regression later
  )

# We check how many demented vs.  non-demented to avoid misfitting out logistic regression to mislabeled target
table(oasis$demented, useNA = 'always') 

oasis_clean <- oasis # Creating a new copy of our data
dim(oasis_clean) # Double check how many rows and columns we have
colSums(is.na(oasis_clean)) # Double check the NA data to make sure it is by research design logistics

# Summary to check to see any values that is outside realistic bounds
summary(oasis_clean[, c ('Age', 'MMSE', 'nWBV', 'eTIV')])

# The next step is the proof that missingness is age-structured (clinical testing mostly in older adults)
# That directly justifies why later you create different analytic subsets (“healthy” vs “clinical”)

oasis_clean %>%
  mutate(has_clinical = !is.na(MMSE)) %>%
  group_by(has_clinical) %>%
  summarise(mean_age = mean(Age), min_age = min(Age), max_age = max(Age))

# A reliablility check is warrented as 20 subjects were brought back for a rescan (within 90 days of their first scan). 
# Therefore, if the nWBV data is reliable, it should produce nearly identical numbers both times (this is known as reliability check).
# Before we use any biomarker in clinical study, we need to make sure that the measurement (or any variable) is reliable/stable and not noise

reliability <- read_excel("~/R/OASIS_R_practice/OASIS_Resources/oasis_cross_sectional.xlsx")

# Because the main dataset and reliability dataset has different labels, we need create a new column (base_ID)
# We then replace '_MR2' to '_MR1', giving us the ID that does exist and comparable to the main data set (as the main data set does not have _MR2)

reliability <- reliability %>%
  mutate(base_ID = str_replace(ID, "_MR2", "_MR1")) 

# The intent here is to join the reliability dataset together with the main dataset.
# The reason we use 'left_join()' is because we don't want to accidentally lose any reliability subjects due to a mismatch or an inability to match
# The 'select()' helps us merge table cleanly without ambiguity, and it also displays the intent (that we care about Age, gender, and nWBV from the main file)

rel_merged <- reliability %>%
  left_join(oasis_clean %>% select(ID, Age, gender, nWBV),
            by = c("base_ID" = "ID"))

# Instead of looking at 20 pairs of data side-by-side, which can be difficult to summarise. 
# We can use correlation coefficient that gives me a number that captures the consistency across all 20 pairs simultaneously
# A measurement of 0.999 tells you that the measurement is extremely reliable (not guaranteed), 
# If noisy otherwise, you would get 0.7 or 0.8 (much more cautious interpreting small difference between subjects)"
# This renames both the x and y in the nWBV for better reference when comparing the reliability with the original

rel_merged <- rel_merged %>% 
  rename(nWBV.x = nWBV_rescan,
         nWBV.y = nWBV_original) # This rename step only needs to be runned once

cor(rel_merged$nWBV.x, rel_merged$nWBV.y, use = 'complete.obs') # Should give r = 0.999818

# With this, we can now move onto the next step, running linear regression, multiple regression, t-test and ANOVA. 

