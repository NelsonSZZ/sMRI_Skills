install.packages(c("tidyverse", "lme4", "lmerTest", "car", "readxl"))

# load packages

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(car)
library(readxl)

oasis <- read_excel("~/R/OASIS_R_practice/OASIS_Resources/oasis_cross_sectional.xlsx")

# First look - Make sure to do this everytime before anything else

str(oasis) 
summary(oasis) 
head(oasis, 11) # Returns the first 11 rows 

# Prepare data 

oasis <- oasis %>% rename(gender = 2) #Rename gender column (2nd column) as the slash can be awkward

# Convert categorical columns to factors, this makes sure that statistical models interpret the variables as distinct groups, rather than a ranking

oasis <- oasis %>%
  mutate(
    gender = as.factor(gender),
    CDR_f = factor(CDR, levels = c(0, 0.5, 1, 2)), 
    Educ_f = factor(Educ, levels = 1:5),
    # Create binary dementia variable for logistic regression later
    demented = ifelse(CDR > 0, 1, 0)
  )

# To check how many demented vs not-demented (excluding NA's) - We do this to avoid misfitting our logistic regression to a mislabeled target

table(oasis$demented, useNA = 'always')

# New copy with newly added, so our data is organized

oasis_clean <- oasis 

# Double check how many rows and columns we have

dim(oasis_clean) 

# Checking NA's per column - to make sure data NA is by research design (In this case it is)

colSums(is.na(oasis_clean))

# Summary to check to see any values that is outside realistic bounds

summary(oasis_clean[, c ('Age', 'MMSE', 'nWBV', 'eTIV')])

# The next step is the proof that missingness is age-structured (clinical testing mostly in older adults). That directly justifies why later you create different analytic subsets (“healthy” vs “clinical”)

oasis_clean %>%
  mutate(has_clinical = !is.na(MMSE)) %>%
  group_by(has_clinical) %>%
  summarise(mean_age = mean(Age), min_age = min(Age), max_age = max(Age))

" A reliable check is warranted as 20 subjects were brought back for a rescan (within 90 days of thier first scan). 
Therefore, if the nWBV is any good, it should produce nearly identical numbers both times (known as reliability check).
Before we use any biomarker in clinical study, we need to make sure that the measurement (considering the context) is real/stable and not noise"

reliability <- read_excel("~/R/OASIS_R_practice/OASIS_Resources/oasis_cross_sectional.xlsx")


" Because the main dataset and reliability dataset has different labels, we therefore create a new column (base_ID)
We then replace '_MR2' to '_MR1', giving us the ID that does exist and comparable to the main dataset " 

reliability <- reliability %>%
  mutate(base_ID = str_replace(ID, "_MR2", "_MR1")) 


" The intent here is to join the reliability section together with the main dataset.
The reason we use 'left_join()' is because we don't want to accidentally lose any reliability subjects due to a unmatch
The 'select()' helps us merge table cleanly without ambiguity, and it also displays the intent (that we care about Age, gender, and nWBV from the main file)"

rel_merged <- reliability %>%
  left_join(oasis_clean %>% select(ID, Age, gender, nWBV),
            by = c("base_ID" = "ID"))
 

" Instead of looking at 20 pairs of data side-by-side, which can be difficult to summarise. 
We can use correlation coefficient that gives me a number that captures the consistency across all 20 pairs simultaneously
A measurement of 0.9999 tells you that the measurement is extremely reliable (not guaranteed), 
if noisy otherwise, you would get 0.7 or 0.8 (much more cautious interpreting small difference between subjects)"

rel_merged <- rel_merged %>% # This renames both the x and y in the nWBV for better reference when comparing the reliability with the original
  rename(nWBV_rescan = nWBV.x,
         nWBV_original = nWBV.y) 


cor(rel_merged$nWBV_rescan, rel_merged$nWBV_original, use = 'complete.obs')