
library(shiny)
library(bslib)
library(ggthemes)
library(showtext) # for customizing plots
library(tidyverse)


################################################################################
########################## A. SET UP, GETTING STARTED ##########################
################################################################################

### Import the predefined functions for plotting charts:

source("source_data_visualization_Climate_Change_in_US_Mind.R")
source("source_data_visualization_American_Trends_Survey.R")
source("source_pewmethods_survey_weights.R")


### Read in the cleaned dataset (ready for plotting)

# policy_support <- read_data("policy_support")
risk_perception <- read_data("risk_perception")



################################################################################
#### B. CREATE A USER INTERFACE THAT DEFINES THE VISUAL ELEMENTS OF THE APP ####
################################################################################


ui <- fluidPage(
  
  
  verticalLayout(
    navbarPage(
      
      strong("CLIMATE CHANGE IN THE AMERICAN MIND",
             style = "font-size: 32px;"),
      
      theme = bs_theme(
        "navbar-bg" = "#ede8ed", # Navigation bar background
        bg = "white", fg = "#333333", # Main background and foreground
        primary = "#0b3d91", # Font colors
        base_font = font_google("Staatliches"),
        code_font = font_google("Staatliches"),
        heading_font = font_google("Staatliches")
      ),
      
      
      
################################################################################
### I. NAVIGATION BAR TAB 1 (= BACKGROUND) #####################################
################################################################################      
      
      tabPanel(
        strong("BACKGROUND", style = "font-size: 20px;"),
        
        tabsetPanel(
          
          ### I-a. TAB 1 FOR THE 'BACKGROUND' tab 
          tabPanel(
            strong("EVIDENCE OF CLIMATE CHANGE", style = "font-size: 18px;"),
            br(),
            strong(
              "There is unequivocal evidence of a rapidly warming planet. Evidently, the last four decades has been the warmest on record!!!",
              style = "font-size: 23px;"),
            br(),
            strong(
              "But in light of such scientific evidence, what do Americans really think about climate change? Let's take a deep dive.",
              style = "font-size: 23px;"),
            br(),
            br(),
            ## 1) SHOW ANIMATED TORNADO PLOT (RISING TEMPERATURES) 
            imageOutput(
              "tornado_plot",
              width = "120%")
          ),
          
          
          ### I-b. TAB 2 FOR THE 'BACKGROUND' tab 
          tabPanel(
            strong("CLIMATE RISK PERCEPTIONS OVER TIME", style = "font-size: 18px;"),
            
            verticalLayout(
              br(),
              includeCSS("styles.css"),
              
              
              ### 1) DROPDOWN MENU FOR THE LINE GRAPH (CLIMATE RISK PERCEPTIONS) 
              
              selectInput(
                inputId = "climate_risk",
                # Text displayed above the dropdown menu
                label = tags$label("Impacts of climate change on:", class = "dropdown-label"),
                # Dropdown options
                choices = c(
                  "Our personal lives" = "Respondent him/herself",
                  "The U.S." = "U.S.",
                  "Developing countries" = "Developing countries",
                  "The future generation" = "Future generation"),
                # Select "Respondent him/herself" by default
                selected = "Respondent him/herself",
                # Enable selection of multiple options
                multiple = TRUE,
                # Adjust the width of the dropdown menu
                width = "40%"),
              
              
              ### 2) PLOT: LINE GRAPH (CLIMATE RISK PERCEPTIONS)         
              
              plotOutput(
                outputId = "risk_perception_plot",
                width = "80%", 
                height = "855px"),
              
              
              ### 3) SUMMARY OF THE TAKEAWAYS 
              
              br(),
              h4("TAKEAWAYS:"),
              tags$ul(
                tags$li("Despite evidence of global warming, the U.S. has witnessed only a modest increase in climate risk perceptions over the last 15 years"),
                tags$li("Interestingly, Americans tend to perceive climate change as an 'other people's problem', in that they underestimate the impacts of climate change on themselves than on the country as a whole/developing countries/future generations")
              )
            )
          )
        )
      ),
      
      

################################################################################
### II. NAVIGATION BAR TAB 2 (= ANALYSIS) ######################################
################################################################################
      
      
      tabPanel(
        strong("ANALYSIS", style = "font-size: 20px;"),
        
        tabsetPanel(
          
          ### II-a. TAB 1 FOR THE 'ANALYSIS' tab 
          
          tabPanel(
            strong("SUPPORT FOR ENVIRONMENTAL POLICY", style = "font-size: 18px;"),
            verticalLayout(
              br(),
              includeCSS("styles.css"),
              
              
              ### 1) DROPDOWN MENU FOR THE PYRAMID GRAPH (CLIMATE BELIEFS)
              
              selectInput(
                inputId = "policy_opinion",
                label = tags$label("Support for:", class = "dropdown-label"),
                choices = c(
                  "Regulation of Carbon Dioxide (CO2) as a pollutant" = "CO2_pollutant",
                  "Stringent Renewable Electricity Standards (RES)" = "RES",
                  "Expansion of renewable & non-renewable energy",
                  "Climate mitigation policies"),
                # Adjust the width of the dropdown menu
                width = "40%"),
              
              
              ### 2) PLOT: PYRAMID CHART (CONCERNS & TRUSTED SOURCE OF INFO) 
              
              plotOutput(
                outputId = "policy_opinion_plot",
                width = "80%", 
                height = "850px"
              ),
              
              
              ### 3) SUMMARY OF THE TAKEAWAYS 
              
              br(),
              h4("TAKEAWAYS:"),
              tags$ul(
                tags$li("The more conservative one's political leaning, the less supportive one is of environmental regulations that curb greenhouse gas emissions"),
                tags$li("Although a majority of Conservative Americans favor the expansion of renewable energy such as solar, wind, and hydrogen, they favor the expansion of non-renewables (i.e. offshore oil and gas, coal) just as much, if not more"),
                tags$li("That being said - among college-educated Americans - support for climate mitigation policy wanes considerably, as we move from Left to the Right-end of the political spectrum"),
                tags$li("In fact, Conservative Americans without a college degree are more supportive of climate-friendly policies than their counterparts with higher educational attainment"),
                tags$li("Such findings may be attributable to the disproportionate impacts of climate change on underserved communities. Since Conservatives with lower educational attainment are at greater risk of climate extremes and climate-induced public health hazards, they may recognize the importance of stringent environmental standards and transforming the U.S. into a low-carbon economy"),
                tags$li("Certain climate migitation policies draw support from both the Left and Right, namely reforestation and tax credit for Carbon capture/sequestration"),
                tags$li("Conservatives are generally lukewarm about switching to 'clean' vehicles and are averse to the idea of abandoning gasoline vehicles by 2035")
              )
            )
          ),
          
          
          ### II-b. TAB 2 FOR THE 'ANALYSIS' tab 
          
          tabPanel(
            strong("CLIMATE BELIEFS", style = "font-size: 18px;"),
            verticalLayout(
              br(),
              includeCSS("styles.css"),
              
              
              ### 1) DROPDOWN MENU FOR THE PYRAMID GRAPH (CLIMATE BELIEFS) 
              
              selectInput(
                inputId = "climate_beliefs",
                label = tags$label("Select option:", class = "dropdown-label"),
                choices = c(
                  "Concerns about enforcing environmental regulations",
                  "Trusted source of climate-related information"),
                # Adjust the width of the dropdown menu
                width = "40%"),
              
              
              ### 2) PLOT: PYRAMID CHART (CONCERNS & TRUSTED SOURCE OF INFO)
              
              plotOutput(
                outputId = "climate_beliefs_plot",
                width = "80%", 
                height = "850px"
              ),
              
              
              ### 3) SUMMARY OF THE TAKEAWAYS
              
              br(),
              h4("TAKEAWAYS:"),
              tags$ul(
                tags$li("Conservatives are more sensitive to the potential costs associated with environmental regulations than are Liberals. Loss of freedom is a big concern that matters to Conservatives, but not so much for the Liberals"),
                tags$li("Americans are more worried about the 'quotidian' aspects of their lives, such as unemployment and inflation, than they are about 'conceptual', less tangible elements, such as lack of freedom"),
                tags$li("Only a small percentage of Conservatives gives credence to the sources of climate change related information"),
                tags$li("However, across political ideology, Americans believe that scientists are the most trustworthy"),
                tags$li("Politicians are the least trusted source of information"),
                tags$li("Less than 30% of Liberals trust the media, which is somewhat surprising"),
              )
            )
          ),
          
          
          ### II-c. TAB 3 FOR THE 'ANALYSIS' tab 
          
          tabPanel(
            strong("CLIMATE ATTITUDES", style = "font-size: 18px;"),
            verticalLayout(
              br(),
              includeCSS("styles.css"),
              
              
              ### 1) DROPDOWN MENU FOR THE WAFFLE CHART (CLIMATE ATTITUDES) 
              
              selectInput(
                inputId = "climate_attitudes",
                label = tags$label("Select option:", class = "dropdown-label"),
                choices = c(
                  "Importance of conserving energy",
                  "Reasons for conserving energy"),
                # Adjust the width of the dropdown menu
                width = "40%"),
              
              
              ### 2) PLOT: WAFFLE CHART (IMPORTANCE OF & REASONS FOR ENERGY CONSERVATION)
              
              plotOutput(
                outputId = "climate_attitudes_plot",
                width = "75%", 
                height = "840px"
              ),
              
              
              ### 3) SUMMARY OF THE TAKEAWAYS
              
              br(),
              h4("TAKEAWAYS:"),
              tags$ul(
                tags$li("Most Americans - regardless of political ideology - agree that cutting down unnecessary consumption of energy is important"),
                tags$li("Among those who believe that energy saving is important, more than half of both Liberals and Conservatives use less energy, for both environmental and economical reasons"),
                tags$li("But for Conservatives, motivations for energy conservation tend to be driven more by cost than by environmental concern")
              )
            )
          )
        ) 
      ), 
      
      
      
################################################################################
### III. NAVIGATION BAR TAB 3 ##################################################
################################################################################      
      
      tabPanel(
        strong("RECOMMENDATION", style = "font-size: 20px;"),
        
        br(),
        h2("POLICY IMPLICATIONS:"),
        
        br(),
        h4("1. ISSUE FRAMING"),
        tags$ul(
          tags$li("Given that Americans are more sensitive to the detrimental impacts of climate change on future generations than on oneself, policy makers should emphasize the costs that future generations would have to bear, in order to prompt climate action"),
          tags$li("Not all Conservative Americans are indifferent to climate change. To reduce partisan polarization and nudge more Conservatives toward climate-friendly attitudes/behavior, policy makers should target underserved communities who are most likely to grapple with and therefore understand the impacts of climate change"),
          tags$li("Climate scientists are the most well-equipped for addressing disinformation and misinformation about climate change. Moreover, policy solutions that are initiated by domain experts will be more well-received and hence have greater potential for momentum. Information communicated via politicians will carry less gravity"),
          tags$li("To be able to elicit support all-across-the-board, policy makers should devise climate policies that take into account increased costs and potential job loss, since they have the most profound and tangible effects on our everyday lives")
        ),
        
        br(),
        h4("2. POLICY AREAS THAT ARE LIKELY TO GARNER BIPARTISAN SUPPORT"),
        tags$ul(
          tags$li("Overall, Americans are open to climate mitigation policies that do not require significant reduction in one's own carbon footprint. Methods that offset (rather than reduce) carbon emissions, such as carbon sink, carbon capture, and carbon sequestration, draw the least public resistance. Policy makers should thus begin with reforestation and tax incentives for carbon capture technology"),
          tags$li("U.S. should increase its reliance on solar and wind energy (renewable), although careful consideration is required with regards to reducing reliance on fossil fuels (non-renewable)"),
          tags$li("Energy conservation is of interest to most Americans. Behavioral interventions for optimizing energy consumption would be most effective if policy makers emphasized the cost-cutting aspects of energy efficiency upgrades and retrofits")
        ),
        
        
        br(),
        h4("3. ROADBLOCKS"),
        tags$ul(
          tags$li("Significant challenges lie ahead in transitioning from gasoline-powered to electric vehicles. Incentives for adopting electric vehicles will most likely work for Liberals, but fail to gain traction among Conservatives"),
          tags$li("Likewise, phasing out the production of gasoline/diesel-powered cars within the next decade may be met with some backlash, particularly among Conservatives"),
          tags$li("Loss of individual freedom acts as a deterrent for endorsing environmental regulations for many Conservatives")
        )
      )
    ) 
  ) 
) 



################################################################################
######## C. CREATE A SERVER, WHICH CONSISTS OF AN 1) INPUT & 2) OUTPUT ######### 
################################################################################ 

server <- function(input, output) {
  
  
  ## 1) ANIMATED TORNADO PLOT (RISING TEMPERATURES)
  
  output$tornado_plot <- renderImage({
    
    list(src = "temperature_anomaly.gif",
         contentType = "image/gif",
         width = 650, height = 850, alt = "temperature_anomaly.gif")
  }, deleteFile = FALSE)
  
  
  ## 2) LINE GRAPH (CLIMATE RISK PERCEPTIONS)
  
  # a) Filter the data based on the dropdown option
  
  filtered_risk_perception <- reactive({
    
    risk_perception %>%
      filter(target %in% input$climate_risk)
  })
  
  
  # b) Render the risk_perception_plot based on the filtered data
  
  output$risk_perception_plot <- renderPlot({
    
    plot_line_graph(filtered_risk_perception())
  })
  
  
  
  ## 3) DOT PLOT (SUPPORT FOR ENVIRONMENTAL POLICY) 
  
  
  # Average percentage of respondents who support each policy (regardless of political ideology & education):
  # avg_support_CO2_pollutant = 73.14084;
  # avg_support_RES = 62.28049
  
  output$policy_opinion_plot <- renderPlot({
    
    # Check the selected option in the dropdown. Display the plot accordingly
    # Import the plots from the source file: source_data_visualization_Climate_Change_in_US_Mind.R
    
    if (input$policy_opinion == "CO2_pollutant") {
      
      # Dotplot that shows % of Americans who support regulating CO2 as a pollutant
      CO2_regulation_plot
      
      
    } else if (input$policy_opinion == "RES") {
      
      # Dotplot that shows % of Americans who support stricter Renewable Electricity Standards
      RES_plot
      
    } else if (input$policy_opinion == "Expansion of renewable & non-renewable energy") {
      
      # Display the expansion of energy plot
      expansion_energy_plot
      
    } else if (input$policy_opinion == "Climate mitigation policies") {
      
      # Display the climate mitigation plot
      climate_mitigation_plot
    }
  })
  
  
  
  ## 4) PYRAMID CHART (CLIMATE BELIEFS) 
  
  output$climate_beliefs_plot <- renderPlot({
    
    # Check the selected option in the dropdown. Display the plot accordingly
    # Import the plots from the source file: source_data_visualization_American_Trends_Survey.R
    
    
    if (input$climate_beliefs == "Concerns about enforcing environmental regulations") {
      
      # Display a pyramid plot that summarizes Americans' concerns about enforcing environmental regulations
      concern_plot
      
      
      
    } else if (input$climate_beliefs == "Trusted source of climate-related information") {
      
      # Display a pyramid plot that summarizes Americans' trusted source of climate info
      info_source_plot
      
    }
  })
  
  
  
  
  ## 5) WAFFLE CHART (CLIMATE ATTITUDES) 
  
  output$climate_attitudes_plot <- renderPlot({
    
    # Check the selected option in the dropdown. Display the plot accordingly
    # Import the plots from the source file: source_data_visualization_American_Trends_Survey.R
    
    
    
    if (input$climate_attitudes == "Importance of conserving energy") {
      
      # Waffle chart that shows % of Americans who believe that energy conservation is important
      importance_conservation_plot
      
      
      
    } else if (input$climate_attitudes == "Reasons for conserving energy") {
      
      # Waffle chart that shows reasons for conserving energy, as % of Americans 
      reason_conservation_plot
      
    }
  })  
  
}



################################################################################
############################# D. RUN THE SHINY APP #############################
################################################################################ 


shinyApp(ui = ui, server = server)
