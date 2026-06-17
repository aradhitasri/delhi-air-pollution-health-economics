library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 12))

citizens <- read_csv("citizens.csv")
doctors  <- read_csv("doctors.csv")

citizens <- citizens %>%
  mutate(across(everything(), ~ str_replace_all(., "â€“", "-")))

doctors <- doctors %>%
  mutate(across(everything(), ~ str_replace_all(., "â€“", "-")))

citizen_col  <- "#1f78b4"   
 doctor_col   <- "#e31a1c"   
 neutral_col  <- "#6a6a6a"
 
 
 ggplot(citizens, aes(age_group)) +
   geom_bar(fill = citizen_col) +
   labs(
     title = "Age Distribution of Citizen Survey Respondents",
     x = "Age Group",
     y = "Number of Respondents"
   )
 
 citizen_health <- citizens %>%
   separate_rows(healthissues, sep = ", ") %>%
   count(healthissues)
 
 ggplot(citizen_health,
        aes(reorder(healthissues, n), n)) +
   geom_col(fill = citizen_col) +
   coord_flip() +
   labs(
     title = "Health Issues Experienced Due to Air Pollution (Citizens)",
     x = "Health Condition",
     y = "Number of Mentions"
   )
 
 ggplot(citizens, aes(money)) +
   geom_bar(fill = citizen_col) +
   labs(
     title = "Monthly Household Expenditure on Pollution-Related Treatment",
     x = "Monthly Cost Bracket (INR)",
     y = "Number of Households"
   )
 
 citizen_months_pct <- citizens %>%
   separate_rows(monthsworse, sep = ", ") %>%
   filter(monthsworse != "No noticeable pattern") %>%
   count(monthsworse) %>%
   mutate(
     group = "Citizens",
     percent = n / sum(n) * 100,
     month = monthsworse
   )
 
 doctor_months_pct <- doctors %>%
   separate_rows(monthsshowing, sep = ", ") %>%
   filter(monthsshowing != "No noticeable pattern") %>%
   count(monthsshowing) %>%
   mutate(
     group = "Doctors",
     percent = n / sum(n) * 100,
     month = monthsshowing
   )
 
 combined_months_pct <- bind_rows(citizen_months_pct, doctor_months_pct)
 
 cit_cond_pct <- citizens %>%
   separate_rows(healthissues, sep = ", ") %>%
   count(healthissues) %>%
   mutate(
     group = "Citizens",
     percent = n / sum(n) * 100,
     condition = healthissues
   )
 
 doc_cond_pct <- doctors %>%
   separate_rows(healthconditions, sep = ", ") %>%
   count(healthconditions) %>%
   mutate(
     group = "Doctors",
     percent = n / sum(n) * 100,
     condition = healthconditions
   )
 
 combined_conditions_pct <- bind_rows(cit_cond_pct, doc_cond_pct)
 
 cit_cost_pct <- citizens %>%
   count(money) %>%
   mutate(
     group = "Households",
     percent = n / sum(n) * 100,
     category = money
   )
 
 doc_cost_pct <- doctors %>%
   count(cost) %>%
   mutate(
     group = "Hospitals",
     percent = n / sum(n) * 100,
     category = cost
   )
 
 combined_cost_pct <- bind_rows(cit_cost_pct, doc_cost_pct)
 
 cit_cost_pct <- citizens %>%
   mutate(category_group = case_when(
     money %in% c("< ₹500", "₹500–1,000") ~ "Low",
     money %in% c("₹1,000–2,500", "₹2,500–5,000") ~ "Medium",
     money %in% c("> ₹5,000") ~ "High"
   )) %>%
   count(category_group) %>%
   mutate(
     group = "Households",
     percent = n / sum(n) * 100
   )
 

 doc_cost_pct <- doctors %>%
   mutate(category_group = case_when(
     cost %in% c("Less than ₹10,000 per case") ~ "Low",
     cost %in% c("₹10,000–₹15,000 per case") ~ "Medium",
     cost %in% c("₹15,000–₹20,000 per case", "More than ₹20,000 per case") ~ "High"
   )) %>%
   count(category_group) %>%
   mutate(
     group = "Hospitals",
     percent = n / sum(n) * 100
   )
 

 combined_cost_pct <- bind_rows(cit_cost_pct, doc_cost_pct)
 
 cit_cost <- citizens %>%
   count(money) %>%
   mutate(
     group = "Households",
     category = money
   )
 
 
 doc_cost <- doctors %>%
   count(cost) %>%
   mutate(
     group = "Hospitals",
     category = cost
   ) %>%
   rename(money = cost) 
 

 combined_cost <- bind_rows(
   cit_cost %>% select(category, n, group),
   doc_cost %>% select(category, n, group)
 )
 

 combined_cost$category <- factor(
   combined_cost$category,
   levels = c(
     "< ₹500", "₹500–1,000", "₹1,000–2,500", "₹2,500–5,000", "> ₹5,000",
     "Less than ₹10,000 per case", "₹10,000–₹15,000 per case", 
     "₹15,000–₹20,000 per case", "More than ₹20,000 per case"
   )
 )
 citizens_numeric <- citizens %>%
   mutate(cost_numeric = case_when(
     money == "< ₹500" ~ 250,
     money == "₹500–1,000" ~ 750,
     money == "₹1,000–2,500" ~ 1750,
     money == "₹2,500–5,000" ~ 3750,
     money == "> ₹5,000" ~ 6000
   )) %>%
   mutate(group = "Households") %>%
   select(cost_numeric, group)
 doctors_numeric <- doctors %>%
   mutate(cost_numeric = case_when(
     cost == "Less than ₹10,000 per case" ~ 5000,
     cost == "₹10,000–₹15,000 per case" ~ 12500,
     cost == "₹15,000–₹20,000 per case" ~ 17500,
     cost == "More than ₹20,000 per case" ~ 22500
   )) %>%
   mutate(group = "Hospitals") %>%
   select(cost_numeric, group)
 
 combined_numeric <- bind_rows(citizens_numeric, doctors_numeric)
 
 
 citizens_numeric <- citizens %>%
   mutate(cost_numeric = case_when(
     money == "< ₹500" ~ 250,
     money == "₹500–1,000" ~ 750,
     money == "₹1,000–2,500" ~ 1750,
     money == "₹2,500–5,000" ~ 3750,
     money == "> ₹5,000" ~ 6000
   )) %>%
   mutate(group = "Households") %>%
   select(cost_numeric, group)
 
 doctors_numeric <- doctors %>%
   mutate(cost_numeric = case_when(
     cost == "Less than ₹10,000 per case" ~ 5000,
     cost == "₹10,000–₹15,000 per case" ~ 12500,
     cost == "₹15,000–₹20,000 per case" ~ 17500,
     cost == "More than ₹20,000 per case" ~ 22500
   )) %>%
   mutate(group = "Hospitals") %>%
   select(cost_numeric, group)
 
 combined_numeric <- bind_rows(citizens_numeric, doctors_numeric)
 
 cit_counts <- citizens %>%
   group_by(money) %>%
   summarise(n = n(), .groups = 'drop') %>%
   mutate(prop = n / sum(n))
 
 doc_counts <- doctors %>%
   group_by(cost) %>%
   summarise(n = n(), .groups = 'drop') %>%
   mutate(prop = n / sum(n))
 
 ggplot(cit_counts, aes(x = "", y = prop, fill = money)) +
   geom_col(width = 1) +
   coord_polar(theta = "y") +
   geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) +
   labs(title = "Household Economic Burden of Air Pollution", fill = "Cost Bracket") +
   theme_minimal() +
   theme(axis.text = element_blank(),
         axis.title = element_blank(),
         panel.grid = element_blank())
 ggplot(doc_counts, aes(x = "", y = prop, fill = cost)) +
   geom_col(width = 1) +
   coord_polar(theta = "y") +
   geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) +
   labs(title = "Hospital Economic Burden of Air Pollution", fill = "Cost per Case") +
   theme_minimal() +
   theme(axis.text = element_blank(),
         axis.title = element_blank(),
         panel.grid = element_blank())
 citizens_long <- citizens %>%
   filter(!is.na(monthsworse)) %>%
   separate_rows(monthsworse, sep = ",\\s*") %>%  # split multiple months
   mutate(group = "Citizens")
 

 citizens_long$monthsworse <- str_to_title(citizens_long$monthsworse)
 doctors_long <- doctors %>%
   filter(!is.na(monthsshowing)) %>%
   separate_rows(monthsshowing, sep = ",\\s*") %>%
   mutate(group = "Doctors")
 

 doctors_long$monthsshowing <- str_to_title(doctors_long$monthsshowing)
 cit_counts <- citizens_long %>%
   group_by(month = monthsworse) %>%
   summarise(count = n(), .groups = 'drop') %>%
   mutate(group = "Citizens")
 

 doc_counts <- doctors_long %>%
   group_by(month = monthsshowing) %>%
   summarise(count = n(), .groups = 'drop') %>%
   mutate(group = "Doctors")
 

 combined_counts <- bind_rows(cit_counts, doc_counts)
 

 month_levels <- c("January","February","March","April","May","June","July","August","September","October","November","December")
 combined_counts$month <- factor(combined_counts$month, levels = month_levels)
 cit_percent <- citizens_long %>%
   group_by(group = "Citizens", month = monthsworse) %>%
   summarise(count = n(), .groups = 'drop') %>%
   mutate(percent = count / sum(count) * 100)
 

 doc_percent <- doctors_long %>%
   group_by(group = "Doctors", month = monthsshowing) %>%
   summarise(count = n(), .groups = 'drop') %>%
   mutate(percent = count / sum(count) * 100)
 

 combined_percent <- bind_rows(cit_percent, doc_percent)
 

 month_levels <- c("January","February","March","April","May","June","July","August","September","October","November","December")
 combined_percent$month <- factor(combined_percent$month, levels = month_levels)
 
 ggplot(combined_percent, aes(x = month, y = percent, color = group, group = group)) +
   geom_line(size = 1.2) +
   geom_point(size = 3) +
   labs(
     title = "Seasonal Trend of Pollution-Related Health Issues (Percentage)",
     x = "Month",
     y = "Percentage of Respondents",
     color = "Survey Group"
   ) +
   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
   theme_minimal(base_size = 12) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 library(tidyverse)
 
 citizens_clean <- citizens %>%
   mutate(
     money_clean = str_trim(money),          # remove extra spaces
     money_clean = str_replace_all(money_clean, "â€“", "–")  # fix any encoding issues
   )
 citizens_clean <- citizens %>%

   filter(!is.na(money) & money != "") %>%
   
   mutate(money_clean = str_trim(money)) %>%
   
   mutate(money_clean = case_when(
     str_detect(money_clean, regex("less than 500", ignore_case = TRUE)) ~ "< 500",
     str_detect(money_clean, regex("500-1000", ignore_case = TRUE)) ~ "500-1000",
     str_detect(money_clean, regex("1000-2500", ignore_case = TRUE)) ~ "1000-2500",
     str_detect(money_clean, regex("2500-5000", ignore_case = TRUE)) ~ "2500-5000",
     str_detect(money_clean, regex("more than 5000", ignore_case = TRUE)) ~ "> 5000",
     TRUE ~ NA_character_
   )) %>%
   
   filter(!is.na(money_clean))
 citizens_numeric <- citizens_clean %>%
   mutate(cost_inr = case_when(
     money_clean == "< 500" ~ 250,
     money_clean == "500-1000" ~ 750,
     money_clean == "1000-2500" ~ 1750,
     money_clean == "2500-5000" ~ 3750,
     money_clean == "> 5000" ~ 6000
   ))
 avg_cost <- mean(citizens_numeric$cost_inr)
 median_cost <- median(citizens_numeric$cost_inr)
 
 cat("Average monthly expenditure:", avg_cost, "INR\n")
 cat("Median monthly expenditure:", median_cost, "INR\n")
 avg_by_occupation <- citizens_numeric %>%
   group_by(occupation) %>%
   summarise(
     avg_cost = mean(cost_inr),
     median_cost = median(cost_inr),
     n = n()
   )
 
 avg_by_occupation
 doctors_clean <- doctors %>%
   filter(!is.na(cost) & cost != "") %>%  
   mutate(cost_clean = str_trim(cost)) %>% 
   mutate(cost_clean = str_replace_all(cost_clean, "â‚¹", "₹")) %>%  
   mutate(cost_clean = str_replace_all(cost_clean, "â€“", "-")) 
 doctors_clean <- doctors_clean %>%
   mutate(cost_category = case_when(
     str_detect(cost_clean, regex("Cannot estimate|Don’t know", ignore_case = TRUE)) ~ NA_character_,
     str_detect(cost_clean, regex("Less than ₹10000", ignore_case = TRUE)) ~ "< 10000",
     str_detect(cost_clean, regex("₹10000-₹15000", ignore_case = TRUE)) ~ "10000-15000",
     str_detect(cost_clean, regex("₹15000-₹20000", ignore_case = TRUE)) ~ "15000-20000",
     str_detect(cost_clean, regex("More than ₹20000", ignore_case = TRUE)) ~ "> 20000",
     TRUE ~ NA_character_
   )) %>%
   filter(!is.na(cost_category))
 
 citizens_numeric <- citizens %>%
   filter(!is.na(money)) %>%
   mutate(money_clean = str_trim(money),
          cost_inr = case_when(
            money_clean %in% c("less than 500", "<500") ~ 250,
            money_clean == "500-1000" ~ 750,
            money_clean == "1000-2500" ~ 1750,
            money_clean == "2500-5000" ~ 3750,
            money_clean %in% c("more than 5000", ">5000") ~ 6000
          )) %>%
   filter(!is.na(cost_inr))
 ggplot(citizens_numeric, aes(x = occupation, y = cost_inr, fill = occupation)) +
   geom_boxplot() +
   labs(title = "Household Pollution-Related Expenditure by Occupation",
        x = "Occupation", y = "Monthly Expenditure (INR)") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 citizens_steps <- citizens %>%
   separate_rows(stepstaken, sep = ",") %>%
   mutate(stepstaken = str_trim(stepstaken))
 
 # Count steps
 citizens_steps %>%
   count(stepstaken) %>%
   ggplot(aes(x = reorder(stepstaken, n), y = n, fill = stepstaken)) +
   geom_col() +
   coord_flip() +
   labs(title = "Precautionary Steps Taken by Citizens",
        x = "Step", y = "Number of Respondents") +
   theme_minimal()
 citizens_severity <- citizens %>%
   mutate(
     severity = case_when(
       affected %in% c("It has seriously affected my health and finances") ~ "Severe",
       affected %in% c("It has caused minor issues, manageable") ~ "Moderate",
       affected %in% c("I haven’t noticed any effect") ~ "Low",
       TRUE ~ NA_character_
     ),
     group = "Citizens"
   )
 citizens_overview <- citizens %>%
   mutate(
     impact_level = case_when(
       affected %in% c("It has seriously affected my health and finances",
                       "Severely affected") ~ "Severe impact",
       affected %in% c("It has caused some health issues",
                       "Moderately affected") ~ "Moderate impact",
       affected %in% c("No noticeable impact",
                       "Not affected") ~ "Low / No impact",
       TRUE ~ NA_character_
     )
   ) %>%
   filter(!is.na(impact_level))
 citizen_impact_summary <- citizens_overview %>%
   count(impact_level) %>%
   mutate(
     perc = n / sum(n) * 100,
     impact_level = factor(
       impact_level,
       levels = c("Low / No impact", "Moderate impact", "Severe impact")
     )
   )
 cit_age <- citizens %>%
   mutate(
     age_group_clean = case_when(
       age_group %in% c("0-14", "Children (0–14 years)") ~ "0–14",
       age_group %in% c("15-24", "Youth (15–24 years)") ~ "15–24",
       age_group %in% c("18-29") ~ "18–29",
       age_group %in% c("30–44", "25–59", "Adults (25–59 years)") ~ "25–59",
       age_group %in% c("60+", "Elderly (60+ years)") ~ "60+",
       TRUE ~ NA_character_
     )
   ) %>%
   filter(!is.na(age_group_clean)) %>%
   count(age_group_clean) %>%
   mutate(
     dataset = "Citizens",
     perc = n / sum(n) * 100
   )
 doc_age <- doctors %>%
   separate_rows(agegroup, sep = ",") %>%
   mutate(agegroup = trimws(agegroup)) %>%
   mutate(
     age_group_clean = case_when(
       grepl("0", agegroup) ~ "0–14",
       grepl("15", agegroup) ~ "15–24",
       grepl("25", agegroup) ~ "25–59",
       grepl("60", agegroup) ~ "60+",
       grepl("All", agegroup) ~ "All ages",
       TRUE ~ NA_character_
     )
   ) %>%
   filter(!is.na(age_group_clean), age_group_clean != "All ages") %>%
   count(age_group_clean) %>%
   mutate(
     dataset = "Doctors",
     perc = n / sum(n) * 100
   )
 age_combined <- bind_rows(cit_age, doc_age) %>%
   mutate(
     age_group_clean = factor(
       age_group_clean,
       levels = c("0–14", "15–24", "18–29", "25–59", "60+")
     )
   )
 cit_impact <- citizens %>%
   mutate(
     impact_score = case_when(
       affected == "I haven’t noticed any effect" ~ 0,
       affected == "It has caused minor issues, manageable" ~ 1,
       affected == "It has seriously affected my health and finances" ~ 2,
       TRUE ~ NA_real_
     )
   ) %>%
   filter(!is.na(impact_score)) %>%
   count(impact_score) %>%
   mutate(
     dataset = "Citizens",
     share = n / sum(n)
   )
 doc_impact <- doctors %>%
   mutate(
     impact_score = case_when(
       effectonhospitals == "No major change" ~ 0,
       effectonhospitals == "Some increase, but manageable" ~ 1,
       effectonhospitals == "Yes, admissions have increased significantly" ~ 2,
       TRUE ~ NA_real_
     )
   ) %>%
   filter(!is.na(impact_score)) %>%
   count(impact_score) %>%
   mutate(
     dataset = "Doctors",
     share = n / sum(n)
   )
 impact_combined <- bind_rows(cit_impact, doc_impact)
 ggplot(impact_combined,
        aes(x = impact_score,
            y = share,
            group = dataset,
            color = dataset)) +
   
   geom_line(linewidth = 1.3) +
   geom_point(size = 4) +
   
   scale_x_continuous(
     breaks = c(0, 1, 2),
     labels = c("No impact", "Moderate impact", "Severe impact")
   ) +
   
   scale_y_continuous(labels = scales::percent_format()) +
   
   labs(
     title = "Severity of Air Pollution Impacts: Citizens vs Hospitals",
     subtitle = "Comparison of perceived household impact and hospital burden",
     x = "Impact severity",
     y = "Share of responses",
     color = ""
   ) +
   
   theme_minimal(base_size = 14)
 cit_conditions <- citizens %>%
   separate_rows(healthissues, sep = ",") %>%
   mutate(healthissues = trimws(healthissues)) %>%
   mutate(
     condition = case_when(
       grepl("Asthma", healthissues, ignore.case = TRUE) ~ "Asthma",
       grepl("Breath", healthissues, ignore.case = TRUE) ~ "Breathlessness",
       grepl("Cough", healthissues, ignore.case = TRUE) ~ "Cough/Cold",
       grepl("Eye|throat", healthissues, ignore.case = TRUE) ~ "Eye/Throat irritation",
       grepl("Chest|fatigue", healthissues, ignore.case = TRUE) ~ "Chest pain/Fatigue",
       TRUE ~ NA_character_
     )
   ) %>%
   filter(!is.na(condition)) %>%
   count(condition) %>%
   mutate(
     dataset = "Citizens",
     perc = n / sum(n) * 100
   )
 doc_conditions <- doctors %>%
   separate_rows(healthconditions, sep = ",") %>%
   mutate(healthconditions = trimws(healthconditions)) %>%
   mutate(
     condition = case_when(
       grepl("Asthma", healthconditions, ignore.case = TRUE) ~ "Asthma",
       grepl("COPD", healthconditions, ignore.case = TRUE) ~ "COPD",
       grepl("Cardio", healthconditions, ignore.case = TRUE) ~ "Cardiovascular disease",
       grepl("Stroke", healthconditions, ignore.case = TRUE) ~ "Stroke",
       grepl("Cancer", healthconditions, ignore.case = TRUE) ~ "Lung cancer",
       grepl("Allerg", healthconditions, ignore.case = TRUE) ~ "Allergies",
       TRUE ~ NA_character_
     )
   ) %>%
   filter(!is.na(condition)) %>%
   count(condition) %>%
   mutate(
     dataset = "Doctors",
     perc = n / sum(n) * 100
   )
 conditions_combined <- bind_rows(cit_conditions, doc_conditions)
 
 condition_order <- conditions_combined %>%
   group_by(condition) %>%
   summarise(mean_perc = mean(perc)) %>%
   arrange(mean_perc) %>%
   pull(condition)
 
 conditions_combined$condition <- factor(
   conditions_combined$condition,
   levels = condition_order
 )
 cit_loc <- citizens %>%
   mutate(
     impact_score = case_when(
       affected == "I haven’t noticed any effect" ~ 0,
       affected == "It has caused minor issues, manageable" ~ 1,
       affected == "It has seriously affected my health and finances" ~ 2,
       TRUE ~ NA_real_
     )
   ) %>%
   filter(!is.na(impact_score))
 cit_loc$location <- factor(
   cit_loc$location,
   levels = c(
     "Central Delhi",
     "North Delhi",
     "South Delhi",
     "East Delhi",
     "West Delhi",
     "Outside Delhi but nearby NCR"
   )
 )
 loc_summary <- cit_loc %>%
   group_by(location) %>%
   summarise(
     mean_impact = mean(impact_score),
     .groups = "drop"
   )
 ggplot(loc_summary,
        aes(x = location, y = mean_impact, group = 1)) +
   
   geom_line(linewidth = 1.4, color = "#2C7FB8") +
   geom_point(size = 4, color = "#2C7FB8") +
   
   scale_y_continuous(
     breaks = c(0, 1, 2),
     labels = c("No impact", "Moderate impact", "Severe impact"),
     limits = c(0, 2)
   ) +
   
   labs(
     title = "Perceived Severity of Air Pollution Impacts by Location",
     subtitle = "Average citizen-reported impact across Delhi NCR",
     x = "Location",
     y = "Average impact severity"
   ) +
   
   theme_minimal(base_size = 14)
 citizens <- citizens %>%
   mutate(
     money_mid = case_when(
       money == "less than 500" ~ 250,
       money == "500-1000" ~ 750,
       money == "1000-2500" ~ 1750,
       money == "2500-5000" ~ 3750,
       money == "more than 5000" ~ 5000,
       TRUE ~ NA_real_
     )
   )
 citizens$occupation <- factor(citizens$occupation,
                               levels = c("Daily wage laborer",
                                          "Homemaker",
                                          "Shop/business owner",
                                          "Private-sector employee",
                                          "Government employee",
                                          "Student",
                                          "Other"))
 
 citizens$age_group <- factor(citizens$age_group,
                              levels = c("18-29", "30-44", "45-59", "60 or above"))
 cost_summary <- citizens %>%
   group_by(age_group, occupation) %>%
   summarise(mean_cost = mean(money_mid, na.rm = TRUE), .groups = "drop")
 ggplot(cost_summary,
        aes(x = age_group, y = mean_cost, group = occupation, color = occupation)) +
   
   geom_line(linewidth = 1.3) +
   geom_point(size = 3) +
   
   labs(
     title = "Average Pollution-Related Healthcare Costs by Age and Occupation",
     x = "Age group",
     y = "Average monthly expenditure (₹)",
     color = "Occupation"
   ) +
   
   theme_minimal(base_size = 14) +
   scale_y_continuous(labels = scales::comma_format())
 
 