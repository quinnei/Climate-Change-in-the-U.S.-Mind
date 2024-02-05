library(ggthemes)
library(showtext) # for customizing plots
library(tidyverse)


################################################################################
##### II. Define functions for data visualization ##############################
################################################################################


### 1. Create a function that reads in the summary csv file (ready for plotting)

read_data <- function(file) {
  data <- read_csv(paste0("Data/", file, ".csv"), show_col_types = FALSE)
  # data <- read_csv(paste0("climate-change-in-the-american-mind/Data/", file, ".csv"), show_col_types = FALSE)
  return(data)
}



### 2. Create a function that generates a dot plot (for policy_support data)

create_dotplot <-
  function(env_policy, heading, avg_pct_support, text_location) {
    
    
    # Specify the font for the plot
    font_add_google("Oswald", "oswald")
    showtext_auto() 
    
    data <- 
      read_data("policy_support") %>% 
      filter(policy == env_policy) 
    
    # Create a dot plot
    dot_plot <-
      data %>% 
      mutate(
        # a) Instead of listing the legend categories (education) in alphabetical order,
        # rearrange them according to the level of educational attainment
        educ_category = fct_relevel(
          educ_category,
          "Less than high school",
          "High school",
          "Some college",
          "Bachelor's degree or higher"),
        # b) Instead of listing the y-axis categories (ideology) in alphabetical order,
        # rearrange them according to left-right spectrum
        ideology = fct_relevel(
          ideology,
          "Very conservative",
          "Somewhat conservative",
          "Moderate",
          "Somewhat liberal",
          "Very liberal")) %>%
      
      # Plot the degree of policy support on x & political ideology on y-axis,
      # Indicate education level as different colored dots 
      ggplot(aes(
        x = percentage,
        y = ideology,
        color = educ_category)) +
      geom_point(size = 6) +
      # Add a dashed vertical line to denote average % of those who support the policy
      geom_vline(
        xintercept = round(avg_pct_support),
        linetype = "dashed",
        color = "slategray4",
        linewidth = 0.6) +
      # Remove the grid in the background; Give it a white background color;
      # Use dark colored dots
      theme_few() +
      scale_color_few("Dark") +
      # Assign descriptive labels to the titles, axes, legend, source annotation
      labs(
        title = heading,
        subtitle = "Among Conservatives, those with higher education are the least supportive of climate mitigation policies, while the reverse is true among Liberals",
        x = "Americans in favor of the policy (%)",
        y = "Political ideology",
        color = "Level of education",
        caption = "Source: Yale Program on Change Communication & George Mason University Center for Climate Change Communication (2022)\nNote: The percentages were calculated based on unweighted survey responses.") +
      # Specify the range of the x-axis, tick marks, and append % to the end of the values
      scale_x_continuous(
        breaks = seq(0, 100, by = 20),
        limits = c(0, 100),
        labels = scales::label_percent(scale = 1)) +
      # Enlarge font sizes and apply custom font
      theme(
        text = element_text(family = "oswald"),
        plot.title = element_text(size = 26, face = "bold"),
        plot.subtitle = element_text(size = 22),
        axis.title.y = element_text(size = 21.5, face = "bold"),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 21.5, face = "bold"),
        axis.text.x = element_text(size = 17),
        legend.title = element_text(size = 17, face = "bold"),
        legend.text = element_text(size = 17),
        plot.caption = element_text(size = 16)) +
      # Add annotation to the dashed vertical line
      annotate(
        "text",
        x = text_location * max(data$percentage),
        y = 3.5,
        label = paste("U.S. average, \nacross 2008 - 2022"),
        hjust = 0,
        vjust = 1,
        color = "slategray4",
        size = 6.5,
        fontface = "bold")
    
    return(dot_plot)
  }



### 3. Create a function that generates a line graph (for risk_perception data)

plot_line_graph <- function(data) {
  
  # Specify the font for the plot
  font_add_google("Oswald", "oswald")
  showtext_auto()
  
  
  line_graph <-
    data %>% 
    ggplot(aes(
      x = year, 
      y = percentage,
      color = target)) +
    # Indicate every data point with dots, along each line
    geom_line(linewidth = 1) +
    geom_point(size = 2.4) +
    # Assign descriptive labels to axes, titles, annotations
    labs(
      title = "Americans' perceptions of the impacts of climate change (2008 - 2022)",
      subtitle = "Although increasing number of Americans see climate change as a threat, Americans tend to underestimate the impacts on their immediate surroundings",
      y = "Percentage of Americans who believe \nclimate change will cause harm (%)",
      x = "Year",
      color = "Targets of climate change",
      caption = "Source: Yale Program on Climate Change Communication & George Mason University Center for Climate Change Communication (2022)\nNote: The percentages were calculated based on unweighted survey responses. \nQuestions on climate risk perceptions were omitted from the 2009 iteration.") +
    # Indicate each year on the x-axis 
    scale_x_continuous(
      breaks = seq(2008, 2022, by = 1)) +
    # Set y-axis from 0 ~ 100, in increments of 20%
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 20),
      # Append '%" sign to the y-axis labels
      labels = scales::percent_format(scale = 1)) +
    # Apply a minimalistic theme
    theme_few() +
    scale_color_gdocs() +
    # Enlarge font sizes and apply custom font
    theme(
      text = element_text(family = "oswald"),
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 24, face = "italic", hjust = 0.5),
      axis.text.y = element_text(size = 15),
      axis.title.y = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(size = 15),
      legend.title = element_text(size = 17, face = "bold"),
      legend.text = element_text(size = 17),
      plot.caption = element_text(size = 14))
  
  
  return(line_graph)
}





################################################################################
##### III. Plot the data #######################################################
################################################################################

# Average percentage of respondents who support each policy (regardless of political ideology & education):
# avg_support_CO2_pollutant = 73.14084;
# avg_support_RES = 62.28049


### 1. Dotplot that shows % of Americans who support regulating CO2 as a pollutant

CO2_regulation_plot <-
  create_dotplot(
    env_policy = "CO2_pollutant",
    heading = "Americans' support for the regulation of Carbon Dioxide (CO2) as a pollutant",
    avg_pct_support = 73.14084,
    text_location = 0.55)



### 2. Dotplot that shows % of Americans who support stricter Renewable Electricity Standards

RES_plot <-
  create_dotplot(
    env_policy = "RES",
    heading = "Americans' support for mandating electricity providers to produce at least 20% of their energy from renewables",
    avg_pct_support = 62.28049,
    text_location = 0.45)