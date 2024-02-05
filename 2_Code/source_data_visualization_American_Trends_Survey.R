
library(haven)
library(forcats)
library(showtext) # for customizing plots
library(ggthemes)
library(waffle) # for waffle charts
library(tidyverse)


################################################################################
##### I. Read in predefined functions and dataset ##############################
################################################################################


### 1. Use the pewmethods package developed by the Pew Research Center, to calculate *weighted* percentages:
# -> Instead of loading their package into R,
# I copy-and-pasted their predefined functions into an R script, due to installation issues.
# The following source file contains 
# 1) get_totals() & 2) replace_if() functions from the pewmethods package:

# source('climate-change-in-the-american-mind/source_pewmethods_survey_weights.R')
source('source_pewmethods_survey_weights.R')


### 2. Read in & clean the survey data (American Trends Panel, Waves 106 & 108)
# Assign the output as 'energy_conservation' & 'environment_policy', respectively

energy_conservation <-
  # read_sav("climate-change-in-the-american-mind/Data/ATP W106.sav") %>%
  read_sav("Data/ATP W106.sav") %>% 
  select(
    # Climate-related survey items
    starts_with("GOVWRRY_"), starts_with("ENV27MOD_"), -ENV27MOD_e_W106,
    SVENERGY_W106, SVREASON_W106,
    # Demographic variables & survey weights
    F_IDEO, WEIGHT_W106) %>% 
  rename(
    # [concerns] Result of environmental regulations, 30 years later
    "RESULT_OF_ENV_REG_HIGHER_FUEL_PRICES" = "GOVWRRY_a_W106",
    "RESULT_OF_ENV_REG_NO_FREEDOM" = "GOVWRRY_b_W106",
    "RESULT_OF_ENV_REG_JOB_LOSS" = "GOVWRRY_c_W106",
    # Trust in various sources of climate info
    "TRUST_POLITICIANS" = "ENV27MOD_a_W106",
    "TRUST_SCIENTISTS" = "ENV27MOD_b_W106",
    "TRUST_NEWS" = "ENV27MOD_d_W106",
    # d) Energy conservation
    "IMP_ENERGY_SAVING" = "SVENERGY_W106",
    "REASON_ENERGY_SAVING" = "SVREASON_W106",
    # Demographic variables
    "IDEOLOGY" = "F_IDEO",
    # Sampling weights
    "WEIGHT" = "WEIGHT_W106") %>% 
  mutate(
    # Replace numeric labels with their original, descriptive labels
    across(-WEIGHT, haven::as_factor),
    # Collapse the 5-category political ideology into 3 levels
    IDEOLOGY = fct_collapse(
      IDEOLOGY,
      Conservative = c("Very conservative", "Conservative"),
      Liberal = c("Very liberal", "Liberal")),
    # Collapse the 5-category likelihood beliefs binary response (very likely/other)
    across(
      starts_with("RESULT_OF_ENV_REG_") | starts_with("RESULT_OF_CC_"),
      ~ fct_collapse(
        .,
        Likely = c("Extremely likely", "Very likely"),
        Other = c("Somewhat likely", "Not too likely", "Not at all likely"))),
    across(
      starts_with("TRUST_"),
      ~ fct_collapse(
        .,
        Strongly_trust = c("Quite a bit", "A great deal"),
        Other = c("Some", "A little", "None"))),
    IMP_ENERGY_SAVING = case_when(
      IMP_ENERGY_SAVING == "Refused" ~ NA_character_,
      TRUE ~ fct_collapse(
        IMP_ENERGY_SAVING,
        Important = c("Extremely important", "Very important", "Somewhat important"),
        Unimportant = c("Not too important", "Not at all important")))) 



environment_policy <- 
  # read_sav("climate-change-in-the-american-mind/Data/ATP W108.sav") %>%
  read_sav("Data/ATP W108.sav") %>%
  select(
    # Climate-related survey items
    starts_with("ENV2_"), EVCAR3_W108, starts_with("CCPOLICY_"), -CCPOLICY_d_W108,
    # Demographic variables & survey weights
    F_IDEO, WEIGHT_W108) %>% 
  rename(
    # Expansion of various sources of energy
    "EXPAND_OFFSHORE_DRILLING" = "ENV2_a_W108",
    "EXPAND_NUCLEAR" = "ENV2_b_W108",
    "EXPAND_COAL" = "ENV2_c_W108",
    "EXPAND_SOLAR" = "ENV2_d_W108",
    "EXPAND_HYDROPOWER" = "ENV2_e_W108",
    "EXPAND_WIND" = "ENV2_f_W108",
    # Opinion on phasing out the production of new gasoline vehicles by 2035
    "POLICY_PHASE_OUT_GASOLINE_VEHICLES" = "EVCAR3_W108",
    # Opinion on various climate adaptation policies
    "POLICY_REFORESTATION" = "CCPOLICY_a_W108",
    "POLICY_CARBON_TAX" = "CCPOLICY_b_W108",
    "POLICY_TAX_CREDIT" = "CCPOLICY_c_W108",
    "POLICY_EV_INCENTIVES" = "CCPOLICY_e_W108",
    # Demographic variables
    "IDEOLOGY" = "F_IDEO",
    # Sampling weights
    "WEIGHT" = "WEIGHT_W108") %>% 
  mutate(
    # Replace numeric labels with their original, descriptive labels
    across(-WEIGHT, haven::as_factor),
    # Collapse the 5-category political ideology into 3 levels
    IDEOLOGY = fct_collapse(
      IDEOLOGY,
      Conservative = c("Very conservative", "Conservative"),
      Liberal = c("Very liberal", "Liberal")))





################################################################################
##### II. Generate summaries from the survey data ##############################
################################################################################



################################################################################
##### II-A. For the pyramid plot ###############################################
################################################################################

### 1. Define a function that creates a summary table of the
### a) *weighted* number of respondents & b) their *weighted* percentages,
### for the survey item of our interest


generate_weighted_summary <- function(variable, data) {
  
  
  ### 1) Find the *weighted percentages*
  weighted_pct <- 
    get_totals(
      # Specify the (dummy) variable that you want to apply the weights to
      variable,
      # Specify the dataframe that this (dummy) variable is located
      data,
      # Specify the sampling weights
      wt = "WEIGHT",
      # Specify the demographic variables to group the respondents by
      by = "IDEOLOGY",
      # Round the percentages to the nearest whole number
      digits = 0) %>% 
    select(!weight_name) %>% 
    
    # Convert to long format -> for subsetting and interactive plots
    pivot_longer(
      cols = !{{variable}},
      names_to = "POLITICAL_IDEOLOGY",
      values_to = "PERCENTAGE")
  
  
  ### 2) Find the *weighted totals*
  weighted_num <- 
    get_totals(
      variable,
      data,
      wt = "WEIGHT",
      by = "IDEOLOGY",
      digits = 0,
      # Find the number of respondents (weighted), NOT percentages
      percent = FALSE) %>%
    # Add a column that indicates row-wise total
    # by_total = TRUE) %>% 
    # Remove the redundant columns (from both the weighted total & percentage data)
    select(!weight_name) %>% 
    
    # Convert to long format -> for subsetting and interactive plots
    pivot_longer(
      cols = !{{variable}},
      names_to = "POLITICAL_IDEOLOGY",
      values_to = "NUM_AMERICANS")
  
  
  
  ### 3) Merge the weighted a) percentages & b) totals, in a column-wise fashion
  
  weighted_summary <- 
    merge(weighted_pct, weighted_num, by = c(variable, "POLITICAL_IDEOLOGY")) %>%
    # i. Keep only summaries for Conservatives & Liberals
    # (i.e. Remove rows for Moderates & Refused)
    filter(POLITICAL_IDEOLOGY %in% c("Conservative", "Liberal")) 
  
  
  
  ### 4) Indicate the survey question in the response categories
  weighted_summary[, 1] <- paste0(as.character(variable), "_", weighted_summary[, 1])
  
  ### 5) Change the name of the variable of interest
  ### (for unification, to merge by rows in subsequent sections)
  colnames(weighted_summary)[1] = "POLICY_STANCE"
  
  
  ### 6) Convert policy stance & political ideology as factors (NOT characters)
  return(as_tibble(weighted_summary) %>%
           mutate_if(is.character, as.factor) %>% 
           ### 7) Remove rows with missing values
           filter(!str_detect(POLICY_STANCE, "_Refused$")))
  
}




### 2. Define a function that combines the summary tables of related policies

merge_summary_tables <- function(policies, data) {
  
  # Iterate through each policy area.
  policies %>% 
    map_dfr(
      # Generate a summary table and perform a row-wise merge
      ~ generate_weighted_summary(
        .x, 
        data))
}



### 3. Define a function that partitions the data by political ideology
### -> For plotting charts on the right and left hand side of the 0% line

filter_by_political_ideology <- function(data, political_leaning) {
  
  
  filtered_data <- data %>%
    filter(POLITICAL_IDEOLOGY == political_leaning) %>%
    
    # To locate the 'Liberals' on the *left-hand* side of the plot,
    # multiply the percentages by -1.
    # For 'Conservatives', keep the percentage values as-is,
    # because 'Conservatives' will be plotted on the *right-hand* side of the plot
    mutate(PERCENTAGE = if (political_leaning == "Liberal") -PERCENTAGE else PERCENTAGE)
  
  return(filtered_data)
}



### 4. Define a function that produces summary data
### (weighted % & weighted number of survey respondents) & plots a pyramid chart


plot_pyramid_chart <- function(
    variables, data, response, reordered_categories,
    y_tick_labels, title, subtitle, y_axis_label, wave, legend_x, legend_y) {
  
  
  ### a) Generate a summary table of weighted %s and count of respondents
  summary <-
    merge_summary_tables(variables, data) %>% 
    # Only examine favorable responses
    filter(str_detect(POLICY_STANCE, paste0("_", response, "$"))) %>%
    # Rearrange the levels of the policy stance category
    mutate(POLICY_STANCE = fct_relevel(POLICY_STANCE, reordered_categories))
  
  
  ### b) Partition the summary table by political ideology
  conservative <- filter_by_political_ideology(summary, "Conservative")
  liberal <- filter_by_political_ideology(summary, "Liberal")
  
  
  ### c) Specify the font
  font_add_google("Oswald", "oswald")
  showtext_auto() 
  
  ### d) Generate a pyramid chart
  pyramid <- 
    ggplot() +
    # (1) Plot bar chart for conservative respondents
    geom_col(aes(
      y = conservative$POLICY_STANCE,
      x = conservative$PERCENTAGE,
      fill = conservative$POLITICAL_IDEOLOGY)) +
    # (2) Plot bar chart for liberal respondents
    geom_col(aes(
      y = liberal$POLICY_STANCE,
      x = liberal$PERCENTAGE,
      fill = liberal$POLITICAL_IDEOLOGY)) +
    # (3) Remove - signs in the percentages & specify the range as 0-100%
    scale_x_continuous(
      breaks = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100), 
      labels = c("100", "80", "60", "40", "20", "0", "20", "40", "60", "80", "100"),
      limits = c(-100, 100)) +
    # (4) Use abbreviated labels for the sources of energy
    scale_y_discrete(labels = y_tick_labels) +
    # (5) Add a vertical line that separates the 2 political groups
    geom_vline(xintercept = 0, color = "black", linewidth = 2.2) +  
    theme_bw() +
    # (6) Specify axis & legend labels, titles, annotation
    labs(
      title = title,
      subtitle = subtitle,
      x = "Percentage of Americans (%)",
      y = y_axis_label,
      fill = "Political ideology",
      caption = paste0(
        "Source: Pew Research Center, American Trends Panel Wave ", wave,
        " (2022)\nNote: The percentages were weighted.")) +
    theme(
      text = element_text(family = "oswald"),
      plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 22, hjust = 0.5),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.y = element_text(size = 17),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 17),
      plot.caption = element_text(size = 16),
      # Place the legend inside the plot
      legend.position = c(legend_x, legend_y),
      legend.justification = c("right", "bottom"),
      legend.title = element_text(size = 17, face = "bold"),
      legend.text = element_text(size = 17)) +
    # (7) Apply red -> Conservative; blue -> Liberal
    scale_fill_brewer(palette = "Set1") 
  
  return(pyramid)
} 



### 5. Define a function that generates a list of variables to be summarized
create_summary_variables <- function(data, string) {
  
  data %>%
    select(starts_with(string)) %>%
    colnames()
  
}



### 6. Define a function that outputs the list of variables in newly classified order

rearrange_categories <- function(prefix, suffix, order) {
  
  # Concatenate the prefix, variable name, and suffix
  paste(prefix, order, suffix, sep = "_")
  
}



################################################################################
##### II-B. For the waffle chart ###############################################
################################################################################


### 1. Define a function that removes NAs from the variable of our interest

subset_energy_conservation_data <- function(variable) {
  
  subset_df <-
    energy_conservation %>% 
    select({{variable}}, IDEOLOGY, WEIGHT) %>% 
    filter(!is.na({{variable}}))
  
  return(subset_df)
  
}



### 2. Create a list of variables to summarize & their reordered response categories
### -> For plotting the waffle plot in the desired order

reason_energy_saving_order <-
  rearrange_categories(
    "REASON_ENERGY_SAVING", "", # suffix as blank
    order = c("Protect the environment", "Save money", "Both", "Neither")) %>%
  # Remove the extraneous underscore at the end of the strings
  str_remove('_$')


### 3. Reorder the categories of the political ideology & policy stance (optional)

rearrange_summary_table <- function(variable, summary_df, order = NULL) {
  
  summary_table <-
    # Generate a summary table (weighted % & weighted number of respondents)
    merge_summary_tables(variable, summary_df) %>%
    mutate(
      # Rearrange the order of the political ideology, so that
      # 'Liberals' are plotted on the 'Left' and 'Conservatives' on the 'Right'
      POLITICAL_IDEOLOGY = fct_relevel(
        POLITICAL_IDEOLOGY,
        c("Liberal", "Conservative")))
  
  # If the categories for policy stance need to be rearranged,
  # perform additional round of cleaning
  if (!is.null(order)) {
    
    summary_table <-
      summary_table %>%
      mutate(POLICY_STANCE = fct_relevel(POLICY_STANCE, order))
  }
  
  return(summary_table)
}



### 4. Define a function that plots a waffle chart

plot_waffle_chart <- function(
    data, variable, title, subtitle, legend_title, legend_labels, colors) {
  
  waffle_chart <- 
    data %>% 
    ggplot(aes(fill = POLICY_STANCE, values = PERCENTAGE)) +
    geom_waffle(
      color = "white",
      size = 0.25,
      flip = TRUE,
      # Add rounded edges to the square tiles
      radius = unit(1, units = "mm")) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = "Source: Pew Research Center, American Trends Panel Wave 106 (2022)\nNote: The percentages were weighted. Each block represents 10%."
    ) +
    theme_few() +
    theme(
      text = element_text(family = "oswald"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_text(size = 16.5, face = "bold"),
      axis.ticks.y = element_blank(),
      plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 22, hjust = 0.5),
      plot.caption = element_text(size = 17),
      # Enlarge the titles of the faceted grids
      strip.text.x = element_text(size = 20, face = "bold"),
      # Change the legend position, enlarge the font, and assign descriptive labels
      legend.position = "bottom",
      legend.title = element_text(size = 19.5, face = "bold"),
      legend.text = element_text(size = 19.5)) +
    scale_fill_manual(
      name = legend_title,
      values = colors, # color palette
      labels = legend_labels) +
    # Plot waffle charts for Conservative & Liberal, respectively 
    facet_wrap(
      ~POLITICAL_IDEOLOGY,
      nrow = 1)
  
  return(waffle_chart)
}



################################################################################
##### III. Plot the data #######################################################
################################################################################



################################################################################
##### III-A. For the pyramid plot ##############################################
################################################################################


### 1. Create a list of variables to summarize & their reordered response categories
### -> For plotting a pyramid plot in the desired order

energy <- create_summary_variables(environment_policy, "EXPAND_")
mitigation <- create_summary_variables(environment_policy, "POLICY_")
concern <- create_summary_variables(energy_conservation, "RESULT_OF_ENV_REG_")
info_source <- create_summary_variables(energy_conservation, "TRUST_")


energy_order <-
  rearrange_categories(
    "EXPAND", "Favor",
    order = c("COAL", "OFFSHORE_DRILLING", "NUCLEAR", "HYDROPOWER", "WIND", "SOLAR"))

mitigation_order <-
  rearrange_categories(
    "POLICY", "Favor",
    order = c("PHASE_OUT_GASOLINE_VEHICLES", "EV_INCENTIVES", "CARBON_TAX", "TAX_CREDIT", "REFORESTATION"))

concern_order <-
  rearrange_categories(
    "RESULT_OF_ENV_REG", "Likely",
    order = c("NO_FREEDOM", "JOB_LOSS", "HIGHER_FUEL_PRICES"))

info_source_order <- 
  rearrange_categories(
    "TRUST", "Strongly_trust",
    order = c("POLITICIANS", "NEWS", "SCIENTISTS"))



### 2. Visualize summary data: 

### a) Plot summary data (weighted % and weighted number of survey respondents)
### for public opinion on Energy policies (whether one supports the expansion of each type of energy)

# Renewable energy: solar/wind/hydropower
# Non-renewable energy: gas/coal/nuclear

expansion_energy_plot <- 
  plot_pyramid_chart(
    variables = energy,
    data = environment_policy,
    response = "Favor",
    reordered_categories = energy_order,
    y_tick_labels = c(
      "Coal", "Offshore \noil and gas", "Nuclear \nenergy", "Hydro \npower",
      "Wind \nenergy", "Solar \nenergy"),
    title = "Americans' support for expanding various sources of energy:",
    subtitle = "Regardless of political ideology, majority of Americans agree with U.S. expansion of solar and wind, although Conservatives tend to show greater support for the expansion of non-renewables",
    y_axis_label = "Types of energy",
    wave = "108", legend_x = 0.997, legend_y = 0.2) 


### b) Plot summary data for public opinion on climate mitigation policies

climate_mitigation_plot <- 
  plot_pyramid_chart(
    variables = mitigation,
    data = environment_policy,
    response = "Favor",
    reordered_categories = mitigation_order,
    y_tick_labels = c(
      "Phasing out the \nproduction of \ngasoline vehicles", "Incentives for \nhybrid/electric \nvehicles",
      "Carbon tax", "Tax credit for \nCarbon capture \ntechnology", "Reforestation"),
    title = "Americans' support for various climate mitigation policies:",
    subtitle = "Conservatives demonstrate varying levels of support, but reforestation and corporate tax credit garner support from both the Left and Right",
    y_axis_label = "Policies to reduce greenhouse gas emissions",
    wave = "108", legend_x = 0.95, legend_y = 0.3)


### c) Plot summary data on public concern about environmental regulations

concern_plot <- 
  plot_pyramid_chart(
    variables = concern,
    data = energy_conservation,
    response = "Likely",
    reordered_categories = concern_order,
    y_tick_labels = c("No freedom", "Job loss", "Higher \nfuel prices"),
    title = "Consequences of environmental regulation: Americans who believe that a given scenario is 'very likely'",
    subtitle = "Conservatives are more concerned about lack of freedom than Liberals, but job loss and increased fuel prices are of mutual concern",
    y_axis_label = "Areas of concern",
    wave = "106", legend_x = 0.98, legend_y = 0.3)


### d) Plot summary data on trusted sources of climate info

info_source_plot <- 
  plot_pyramid_chart(
    variables = info_source,
    data = energy_conservation,
    response = "Strongly_trust",
    reordered_categories = info_source_order,
    y_tick_labels = c("Politicians", "News/\nMedia", "Climate \nscientists"),
    title = "Trusted source of climate-related information: Americans who believe that a given source is at least 'moderately trustworthy'",
    subtitle = "Irrespective of political lines, politicians are the least trusted while scientists are considered the most credible",
    y_axis_label = "Source of information",
    wave = "106", legend_x = 0.95, legend_y = 0.35)




################################################################################
##### III-B. For the waffle chart ##############################################
################################################################################


### 1. Select the variable of interest and store rows without any missing values

importance_energy_saving <- subset_energy_conservation_data(IMP_ENERGY_SAVING)
reason_energy_saving <- subset_energy_conservation_data(REASON_ENERGY_SAVING)


### 2. Visualize summary data: 

# a) Whether respondents believe that energy conservation is important or not:

importance_conservation_plot <- 
  plot_waffle_chart(
    # Generate summary data
    data = merge_summary_tables("IMP_ENERGY_SAVING", importance_energy_saving),
    # Generate a waffle chart
    variable = "IMP_ENERGY_SAVING",
    title = "Percentage of Americans who believe that energy conservation is important",
    subtitle = "Both Liberals and Conservatives recognize the importance of energy savings, although Liberals do so to a larger extent",
    legend_title = "Percentage of Americans who believe that energy savings is:",
    legend_labels = c("Important", "Unimportant"),
    colors = c("#009E73", "#d1495b"))


# b) If the respondents believe that energy conservation is *important*,
# motivations for conserving energy

reason_conservation_plot <- 
  plot_waffle_chart(
    # Generate summary data
    data = merge_summary_tables("REASON_ENERGY_SAVING", reason_energy_saving) %>%
      # Rearrange the categories
      # (so that reasons for conserving energy is plotted in desired order)
      mutate(POLICY_STANCE = fct_relevel(POLICY_STANCE, reason_energy_saving_order)),
    # Generate a waffle chart
    variable = "REASON_ENERGY_SAVING",
    title = "Reasons for conserving energy",
    subtitle = "Conservatives tend to reduce their energy consumption mainly to cut down costs, rather than to protect the environment",
    legend_title = "Percentage of Americans, by reason:",
    legend_labels = c("Environmental protection", "Cost savings", "Both", "Neither"),
    colors = c("#009E73", "#FBB117", "#3C5488FF", "#d1495b"))




### Summary statistics: Calculate number of respondents by political ideology, for each wave ###

list(energy_conservation, environment_policy) %>% 
  map(
    ~ .x %>%
      summarize(
        num_respondents = n(),
        .by = IDEOLOGY))
  

