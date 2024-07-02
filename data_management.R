# Purpose: Prep statewide data for data analysis and tables/graphs
# Contact: Shady Shamy

# Load data and packages:

library(dplyr)
bns <- readRDS(here::here("../BNS3-statewide/data/bns3_statewide_clean.rds"))




# Recode to bivariate responses, agree and not agree


bns$course_activities <- recode(bns$q106_3, "Strongly disagree" = "0",
                           "Disagree" = "0",
                           "Neither agree nor disagree" = "0",
                           "Agree" = "1",
                           "Strongly agree" = "1")


bns$office_hours <- recode(bns$q106_4, "Strongly disagree" = "0",
                           "Disagree" = "0",
                           "Neither agree nor disagree" = "0",
                           "Agree" = "1",
                           "Strongly agree" = "1")

bns$attend_class <- recode(bns$q106_5, "Strongly disagree" = "0",
                           "Disagree" = "0",
                           "Neither agree nor disagree" = "0",
                           "Agree" = "1",
                           "Strongly agree" = "1")


bns$SNAP_course_activities <- recode(bns$q52_17, "Strongly disagree" = "Not Agree",
                                     "Disagree" = "Not Agree",
                                     "Neither agree nor disagree" = "Not Agree",
                                     "Agree" = "Agree",
                                     "Strongly agree" = "Agree")


bns$SNAP_office_hours <- recode(bns$q52_18, "Strongly disagree" = "Not Agree",
                                "Disagree" = "Not Agree",
                                "Neither agree nor disagree" = "Not Agree",
                                "Agree" = "Agree",
                                "Strongly agree" = "Agree")


bns$SNAP_attend_class <- recode(bns$q52_19, "Strongly disagree" = "Not Agree",
                                "Disagree" = "Not Agree",
                                "Neither agree nor disagree" = "Not Agree",
                                "Agree" = "Agree",
                                "Strongly agree" = "Agree")


bns$SNAP_not_worry_abt_food <- recode(bns$q52_3, "Strongly disagree" = "Not Agree",
                                "Disagree" = "Not Agree",
                                "Neither agree nor disagree" = "Not Agree",
                                "Agree" = "Agree",
                                "Strongly agree" = "Agree")




# Recode to Men, Women, and Other

bns$gender <- case_when(bns$q7 == "Man" ~ "Men", bns$q7 == "Woman" ~ "Women",
                        bns$q7 == "Gender Queer or Gender-Nonconforming" ~ "Other",
                        bns$q7 == "Gender non-binary" ~ "Other",
                        bns$q7 == "Gender Unlisted:" ~ "Other") %>% factor(order = FALSE) %>% 
  relevel(ref = "Women")


# Rename to units
bns <- rename(bns, units = q18)


# Full Time
bns$full_time <- ifelse(bns$units >= 12, "Yes", "No")


# Count CalFresh as current users
bns <- bns %>% mutate(calfresh = case_when(
  grepl("Currently use as a college student", q46_1) ~ "CalFresh",
  grepl("Used it in the past as a college student", q46_1) ~ "No CalFresh",
  grepl("Heard of it, but never used it", q46_1) ~ "No CalFresh",
  grepl("Never heard of it", q46_1) ~ "No CalFresh",
  grepl("Used it before attending college", q46_1) ~ "No CalFresh"))



# Remove NAs from citizenship questions from skip logic (to use in imputations)

# If q38 (citizenship) is Yes, set to Citizen, otherwise, set to q39 (green card)
bns$citizenship <- ifelse(bns$q38 == "Yes", "Citizen", as.factor(bns$q39))
bns$citizenship <- recode(bns$citizenship, "1" = "Yes", "2" = "No") %>% factor(order = FALSE)



# Create HSI Variable. Assignments based on this list 2021 - 2022: https://www.hacu.net/hacu/HSIs.asp # Accessed 1/27/24
# https://www.nifa.usda.gov/about-nifa/how-we-work/partnerships/hispanic-serving-institutions-hsis
# The second list from the USDA did not count Butte and Clovis as an HSI,
#  but the first list from the HISPANIC ASSOCIATION OF COLLEGES & UNIVERSITIES lists Butte and Clovis as an HSI and I trust this source more as they display Hispanic percentages
# Only UC Berkeley was not an HSI

bns$HSI <- ifelse(bns$school == "UC Berkeley", "Not HSI", "HSI")

# Create Rural College Indicator
# If a School is in a county indicated as Rural by the Rural County Representatives of California, it was counted as Rural
# https://www.rcrcnet.org/counties Accessed 1/27/2024

bns$rural <- case_when(bns$school == "Butte CC" ~ "Rural",
                       bns$school == "Allan Hancock Community College" ~ "Rural",
                       bns$school == "Cal State LA" ~ "Urban",
                       bns$school == "Cal State San Bernardino" ~ "Urban",
                       bns$school == "Clovis CC" ~ "Urban",
                       bns$school == "CSU Bakersfield" ~ "Urban",
                       bns$school == "CSU Dominguez Hills" ~ "Urban",
                       bns$school == "Mt. SAC CC" ~ "Urban",
                       bns$school == "Palo Verde CC" ~ "Urban",
                       bns$school == "Sacramento State" ~ "Urban",
                       bns$school == "San Francisco State" ~ "Urban",
                       bns$school == "UC Berkeley" ~ "Urban")


# Set variables as factor
bns <- bns %>% 
  mutate_at(c("eth_hispanic", "firstgen", "food_insecure_2item", 
              "calfresh", "calfresh_eligible", "q64", "q65", "school"), 
            as.factor)


bns_unfiltered <- bns

# Filter data

# Keep Only full time students, calfresh eligible or current calfresh users, and men/women
bns <- bns %>% filter(units >= 12,
                      (calfresh_eligible == 1 | calfresh == "CalFresh"),
                      (gender== "Men" | gender == "Women")) %>% droplevels()





