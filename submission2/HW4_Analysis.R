if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

# question 1 
full_df <- read_rds("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW4/data/output/final_ma_data.rds")

table(full_df$plan_type)
table(full_df$org_type)


# question 2

# Load required library
library(tidyverse)

# Filter for relevant years and non-missing star ratings
star_data <- final.data %>%
  filter(year %in% c(2010, 2012, 2015), !is.na(Star_Rating))

# Create the combined bar graph
ggplot(star_data, aes(x = as.factor(Star_Rating), fill = as.factor(year))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Star Ratings by Year",
    x = "Star Rating",
    y = "Count of Plans",
    fill = "Year"
  ) +
  theme_bw()

# question 3

q3 <- full_df %>% 
filter(year >= 2010 & year <= 2015) %>%
group_by(year) %>%
summarise(avg_rate = mean(ma_rate, na.rm = TRUE))

ggplot(q3, aes(x = year, y = avg_rate)) +
    geom_line() +
    geom_point() +
    labs(
        title = "Average MA Rate Over Time",
        x = "Year",
        y = "Average MA Rate"
    ) +
    theme_minimal()


# question 4
q4 <- full_df %>% 
    filter(year >= 2010 & year <= 2015) %>%
    mutate(avg_share = avg_enrollment / avg_eligibles)

q4_b <- q4 %>%
    group_by(year) %>%
    summarise(avg = mean(avg_share, na.rm = TRUE))


ggplot(q4_b, aes(x = year, y = avg)) +
    geom_line() +
    geom_point() +
    labs(
        title = "Average Share of Enrollment to Eligible Population Over Time",
        x = "Year",
        y = "Average Share"
    ) +
    theme_minimal()

# question 5

library(tidyverse)

# Load and filter data for 2010 only
full_df10 <- full_df %>% 
  filter(year == 2010) %>%
  filter(!is.na(avg_enrollment), !is.na(partc_score)) %>%
  mutate(raw_rating = rowMeans(
    cbind(
      breastcancer_screen, rectalcancer_screen, cv_cholscreen, diabetes_cholscreen,
      glaucoma_test, monitoring, flu_vaccine, pn_vaccine, physical_health,
      mental_health, osteo_test, physical_monitor, primaryaccess,
      hospital_followup, depression_followup, nodelays, carequickly,
      overallrating_care, overallrating_plan, calltime, doctor_communicate,
      customer_service, osteo_manage, diabetes_eye, diabetes_kidney,
      diabetes_bloodsugar, diabetes_chol, antidepressant, bloodpressure,
      ra_manage, copd_test, betablocker, bladder, falling, appeals_timely,
      appeals_review
    ), na.rm = TRUE
  )) %>%
  mutate(mktshare = avg_enrolled / avg_eligibles)

# Create summarized table
star_rating_summary <- full_df10 %>%
  filter(partc_score >= 2.5) %>%
  group_by(partc_score) %>%
  summarize(count = n())

star_rating_summary

# Question 6

# 2.75 cutoff (3 vs 2.5)
df_10a <- full_df10 %>%
  filter((2.75 - 0.125) <= raw_rating & raw_rating <= (2.75 + 0.125)) %>%
  mutate(raw_rating_dev = raw_rating - 2.75)

rd.est3 <- lm(avg_enrollment ~ raw_rating_dev, data = df_10a)

# 3.25 cutoff (3.5 vs 3)
df_10b <- full_df10 %>%
  filter((3.25 - 0.125) <= raw_rating & raw_rating <= (3.25 + 0.125)) %>%
  mutate(raw_rating_dev = raw_rating - 3.25)

rd.est35 <- lm(avg_enrollment ~ raw_rating_dev, data = df_10b)

# Format results
library(broom)

coef_rd3 <- tidy(rd.est3) %>% 
  select(term, estimate) %>% 
  mutate(model = "Cutoff at 2.75")

coef_rd35 <- tidy(rd.est35) %>% 
  select(term, estimate) %>% 
  mutate(model = "Cutoff at 3.25")

results_table <- bind_rows(coef_rd3, coef_rd35)

results_table