

library(haven) # for reading in .sav data
library(labelled) # for manipulating metadata as variable labels
library(ggthemes)
library(showtext) # for customizing plots
library(tidyverse) # for data wrangling




################################################################################
##### I. Read and clean the dataset ############################################
################################################################################


### 1. Read in the raw data
climate <- read_sav("./problem-set-1/Data_Climate_Change_American_Mind_2008_2022.sav")



##### A. Dataset for the dot plot ==============================================
# ==============================================================================

### 2. Define a function that calculates the (unweighted) number & % of respondents
### who support a given environmental policy

summarize_num_policy_support <- function(variable, policy){
  
  ### a) Store the name of the new variable (aggregated)
  # num_supportive = paste0("support_", aggregated_variable)
  
  
  ### b) Create a dataframe that stores the *total* number of survey respondents,
  ### by demographic characteristics
  total_num_respondents <- 
    # Subset the data
    # Keep only: support for a given policy, education, political ideology
    climate %>%
    select({{variable}}, educ_category, ideology) %>%
    # Remove NAs
    filter(!is.na({{variable}})) %>%
    # For every combination of political ideology & education level,
    # calculate the number of respondents (unweighted count)
    group_by(ideology, educ_category) %>% 
    summarize(total_respondents = n(), .groups = 'drop_last')
  
  
  ### c) Create a dataframe that stores the number of survey respondents who
  ### *support* the given policy
  num_respondents_support <- 
    climate %>%
    # Keep only the variables of interest (same as above)
    select({{variable}}, educ_category, ideology) %>%
    # Subset to respondents who support the given policy
    filter({{variable}} %in% c(3, 4)) %>% 
    # For every combination of political ideology & education level,
    # calculate the number of respondents who support the policy (unweighted count)  
    group_by(ideology, educ_category) %>% 
    summarize(num_supportive = n(), .groups = 'drop_last')
    
  
  ### d) Perform an innerjoin between the two, in order to summarize:
  # a) the total number of respondents; and
  # b) the number of respondents who support the given policy;
  # c) by political ideology and education level 
  summary <- 
    merge(num_respondents_support, total_num_respondents,
          by = c("ideology", "educ_category")) %>%
    ### e) Calculate the % of respondents who support the climate-friendly policy
    ### Round to the nearest integer
    mutate(
      percentage = round((num_supportive/total_respondents)*100),
      # Overwrite *text* labels on the *numeric-coded* values
      ideology = to_character(ideology),
      # Shorten "Moderate, middle of the road" (political ideology) to "Moderate"
      ideology = ifelse(ideology == "Moderate, middle of the road", "Moderate", ideology),
      educ_category = to_character(educ_category),
      policy = policy) %>%
    # Bring the policy column to the very front
    select(policy, everything()) %>% 
    
    # Remove respondents who refused to disclose political ideology (i.e. NAs)
    slice(5:n())
  
  
  return(summary)
}



### 3. Define a function that calculates average levels of support for a given policy
### (% of respondents who support the policy (out of ALL respondents))
### -> to determine where to draw a vertical line on a dot plot

calculate_average_policy_support <- function(variable) {
  
  main_variable <- 
    climate %>% 
    select({{variable}})
  
  average_pct <-
    climate %>% 
    filter({{variable}} %in% c(3, 4)) %>%
    nrow()/sum(!is.na(main_variable))*100
  
  return(average_pct)
  
}


### 4. Store the summary data as a single csv file 

rbind(
  summarize_num_policy_support(reg_CO2_pollutant, "CO2_pollutant"),
  summarize_num_policy_support(reg_utilities, "RES")) %>%
  write.csv(., paste0("climate-change-in-the-american-mind/Data/policy_support.csv"), row.names = FALSE)


### 5. Calculate the average levels of policy support (all respondents)
# avg_support_CO2_pollutant = 73.14084;
# avg_support_RES = 62.28049

avg_support_CO2_pollutant <- calculate_average_policy_support(reg_CO2_pollutant)
avg_support_RES <- calculate_average_policy_support(reg_utilities)





##### B. Dataset for the line plot =============================================
# ==============================================================================



### 1. Define a function that collapses 6 climate risk perception categories into 4

recode_climate_risk <- function(response) {
  case_when(
    # Global warming will harm a great deal/moderate amount -> 'causes harm'
    response %in% c(3, 4) ~ "Climate change causes harm",
    # Global warming will harm: not at all/only a little -> 'not cause harm'
    response %in% c(1, 2) ~ "Climate change does not cause harm",
    # For all other responses, assign text labels instead of numeric values
    response == -1 ~ "Refused",
    TRUE ~ "Don't know")
}



### 2. Define a function that calculates the total number of survey respondents for each year

calculate_total_respondents <- function(data, group_by_column) {
  data %>%
    summarize(
      total_respondents = n(),
      .by = {{group_by_column}})
}



# 3. Define a function that calculates the number of respondents who believe that
# climate change causes harm: 1) personally 2) on the U.S. 3) developing countries and 4) future generations

calculate_num_concerned <- function(target_gp) {
  
  # Assign individualized column names
  # (= number of respondents who perceive that climate change causes harm) for 1) ~ 4)
  new_column = paste0("n_", target_gp)
  
  # For each year (2008~2022),
  # count the number of respondents who believe climate change is harmful
  climate_risk_perceptions %>%
    filter(.data[[target_gp]] == "Climate change causes harm") %>%
    group_by(year, .data[[target_gp]]) %>% 
    summarize(!!new_column := n(), .groups = 'drop') %>%
    select(-starts_with("harm_"))
}



### 4. Clean the data. Generate a yearly summary

# a) Reduce the number of climate risk perception categories from 6 -> 4

climate_risk_perceptions <-
  # Select variables that measure climate risk perceptions, along with the survey year
  climate %>%
  select(year, starts_with("harm_") & !ends_with("plants_animals")) %>%
  # Recode responses for all columns except the year column
  mutate(
    across(-year,
           ~ map(
             .x,
             recode_climate_risk)))


# b) Calculate the total number of survey respondents for each year

num_respondents <- calculate_total_respondents(climate_risk_perceptions, year)


# c) Store as a dataframe:
# 1) year, 2) number of respondents who perceive climate risks across each of the 4 target groups,
# 3) total number of respondents 4) % of respondents for each of the 4 targets
risk_perception <-
  # Select columns on which to apply the function (i.e. columns that measure
  # risk perceptions on 1) individuals 2) U.S. 3) developing countries 4) future generation
  climate_risk_perceptions %>%
  select(-year) %>% 
  colnames() %>% 
  # Calculate the number of respondents who believe that climate change
  # causes harm, on each of the 4 target groups
  map(
    ~ calculate_num_concerned(.x)) %>%
  # Because it results in a list of 4 dataframes, merge them as one
  reduce(
    left_join,
    by = "year") %>% 
  # Append the total number of respondents in each wave of the survey
  left_join(
    num_respondents,
    by = "year") %>% 
  # Create a column that indicates the number of respondents in %s
  mutate(
    across(
      # Don't calculate %s on the 'year' and 'total_respondents' column
      starts_with("n_harm"),
      ~ round((. / total_respondents)*100),
      # Indicate that it's in %s, in the column name
      .names = "pct_{.col}"),
    # Instead of numeric coded values, use the actual year.
    # Apply the value labels. Then convert it from character -> double.
    year = as.numeric(to_character(year))) %>%
  
  # Transform the dataset into long format, to create multiple line graphs on a single plot
  # For all variables that denote the percentage of respondents, display the %s in a single column
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "target",
    values_to = "percentage")  %>% 
  mutate(
    # Rename the values of the legend categories (targets)
    # For categories that end with OO -> change to XX
    target = case_when(
      str_detect(target, "future_gen$") ~ "Future generation",
      str_detect(target, "dev_countries$") ~ "Developing countries",
      str_detect(target, "US$") ~ "U.S.",
      str_detect(target, "personally$") ~ "Respondent him/herself"),
    # Rearrange the legend categories (targets) in order of % values
    target = fct_reorder(target, percentage))



### c) Save the yearly data on climate risk perceptions

write.csv(risk_perception, "climate-change-in-the-american-mind/Data/risk_perception.csv", row.names = FALSE)



### Summary statistics ###

risk_perception %>% 
  select(year, total_respondents) %>% 
  distinct()
