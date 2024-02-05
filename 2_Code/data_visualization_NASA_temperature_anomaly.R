

# I. Read in data ---------------------------------------------------------

library(tidyverse)
library(gganimate)
library(ggthemes)
library(showtext)



temperature <-
  read_csv(
    "./gganimate/NASA_Global surface temperature change estimates.csv",
    # Read in from the 2nd row (2nd row = column names)
    skip = 1,
    # Replace '***' as NA
    na = "***",
    # Read in the year & month (Jan ~ Dec) columns
    col_select = c("Year", "Apr", "Oct")) %>%
  # Remove records for 2023 (because we don't have year-long data for the current year)
  # i.e. Remove the last row from the dataset
  slice(-(n())) %>% 
  
  mutate(
    # Convert the Year column from double -> integer (for display on the animated plot)
    Year = as.integer(Year),
    # Create a column that calculates the average temperature difference of April & October
    Avg_temperature_change = rowMeans(select(., -Year)))





# II. Plot the data (static & animated) -----------------------------------




### Store data for the 1) year & 2) temperature labels, as a dataframe
# Create year indicators for: 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2022
# From 1880 ~ 2000, use increments of 20. Year 2022 is to denote the final data point
year_labels <-
  tibble(
    years = c(
      seq(1880, 2000, by = 20),
      2022))




# Create celsius indicators that denote the extent of temperature anomaly
# Create a dataset in a row-wise fashion, for readability
celsius_labels <-
  tribble(
    
    ~x_position, ~celsius,
    -5,           "+1°C",  
    -4,           "0°C", 
    0,           "0°C", 
    1,           "+1°C")






# Specify the font for the plot
font_add_google("Oswald", "oswald")
showtext_auto()


anomaly <-
  
  temperature %>%
  ggplot(aes(
    # Plot the monthly temperature anomaly on the x-axis
    # Subtract -4°C from the raw temperature values to ensure that
    # the curves stretch out from left to right
    x = -4 - Oct,
    # Plot the corresponding year (1880 ~ 2022) on the y-axis
    y = Year,
    # For connecting the two points (x, y) & (xend, yend) with a (curved) line
    xend = Apr,
    yend = Year,
    # Indicate average changes in global surface temperatures in different shades of colors
    color = Avg_temperature_change)) +
  
  
  
  ### Create a tornado plot
  
  # a) Add curvature to the lines
  geom_curve(
    curvature = 0.03,
    lineend = "round", # The line should have round tips at each end
    linewidth = 2.3) + # Increase the thickness of the lines
  
  # b) Indicate the year to which the data point corresponds.
  geom_text(
    # b-i) Do so for every 2 decades & for 2022 (i.e. the latest record we have)
    data = year_labels,
    aes(
      # b-ii) Place the text labels in the middle of the tornado plot
      x = -2, # Mid-point along the x-axis
      # b-iii) Position the 'year' labels so that they align with the y-axis of the main plot (Year)
      y = years,
      label = years),
    # Remove the default aesthetics
    inherit.aes = FALSE,
    size = 7,
    fontface = "bold") +
  
  
  
  ### Emphasize the degree of global warming compared to pre-Industrial levels
  
  # a) Add dashed vertical lines to indicate temperature rise of 0°C and +1°C 
  geom_vline(
    xintercept = celsius_labels$x_position,
    color = "gray3",
    linetype = "dashed",
    linewidth = 0.5) +
  # b) Indicate the 0°C and +1°C mark
  geom_label(
    data = celsius_labels,
    aes(
      # Place the celsius labels where x-axis is -5, -4, 0, 1; and
      x = x_position,
      # where year (y-axis) is 2030
      # (so that it's located above the '2022' label)
      y = 2030,
      label = celsius),
    inherit.aes = FALSE, # Override the default aesthetics
    size = 6.5) +
  
  # Use the colorspace package to denote the extent of global temperature change
  colorspace::scale_colour_continuous_sequential(
    # Apply red gradation of colors to the line
    # Alternative palettes to choose from: OrRd/Oranges
    palette = "Reds",
    # Remove the legend
    guide = "none") +
  
  
  
  # Add descriptive titles and subtitles
  labs(
    ## Interactive component: Displays the year of the corresponding data point
    title = "Average annual temperature deviations in {frame_time}, relative to 1850 - 1900",
    caption = "Source: NASA (2023). The darker the color, the warmer the global average temperatures relative to the baseline.") +

  
  
  # Apply a minimalist theme (a plain white background) that doesn't have any grids at every tick
  theme_few() +
  theme(
    # Apply a custom font (defined in the previous step)
    text = element_text(family = "oswald"),
    plot.title = element_text(
      # Add margins/space to the top and bottom of the main title
      # Place the title in the center and adjust font
      hjust = 0.5,
      size = 34,
      face = "bold"),
    plot.caption = element_text(size = 25),
    # Remove all text for the x- and y- axis
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # Apply a pastel-tone background to the plot 
    panel.background = element_rect(fill = "linen"),
    # Add a gray frame around the whole image
    plot.background = element_rect(
      colour = "gray19",
      linewidth = 1.5,
      # Apply a darker pastel-tone background to the outer borders of the plot
      fill = "bisque1")) +
  
  
  
  
  ### Add the interactive component
  transition_time(Year) +
  shadow_trail(distance = 0.01)





### Render the animated image
animate(
  anomaly,
  # Define the number of frames to generate (default = 100)
  # The total number of frames in the animation
  nframes = 120,
  # Add pauses, before we replay the animation
  # Note: The units are *NOT* in seconds, but the number of frames! (default: 10)
  end_pause = 30,
  # Specify the dimensions of the animated plot
  height = 4.2,
  width = 3,
  units = "in",
  # Specify its resolution
  res = 500)

### Save the animation to a file
anim_save("./climate-change-in-the-american-mind/temperature_anomaly.gif")


### Summary statistics: average temperature anomaly: 0.06846154

temperature %>% 
  select(Avg_temperature_change) %>% 
  pull() %>% 
  mean()
