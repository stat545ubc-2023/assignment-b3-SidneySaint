#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(tidyverse)
library(ggplot2)
data("USArrests") #[2] (numbers in square brackets are references - full references below)

# define the Shiny App UI and components:
ui <- fluidPage(
  # Add a theme (not especially useful although I think it makes the app look nicer.)
  theme = shinytheme("journal"), #[1]
  #Add a title:
    titlePanel("Major Crime in US States by Urban Population "),
  # Add a subheading to explain the purpose of the app:
  h5("Explore how the association between murders and assualts in US states might change as a function of the percent of state population living in urban areas:"),
  
    # Select the attributes to be displayed in the main panel of the user interface (listed in the order I want them to appear)
    mainPanel( #[7]
     # FEATURE 1: a plot (reactive/ display varies with slider input. Described below where plot is defined)
      plotOutput("MurderbyAssaultPlot", height = "900px", width = "950px"),
      # FEATURE 2: a slider (described below where input is defined)
      sliderInput("id_slider_percUrban",
                  "Percentage of Urban Population in State",
                  min = 0,
                  max = 100,
                  value = c(0, 100)),
      # FEATURE 3: reactive text output (described below where it is defined).
      textOutput("DescribeStatesFound"), 
      # FEATURE 4: Image (Static; described below where image is called)
      imageOutput("redditUSApopDens"), #[3],[4]
      # more text output (non-reactive)
      textOutput("Textbelowimage"),
      # FEATURE 5: interactive data table (described where table defined below)
      DT::dataTableOutput("CrimeData"), #[6]
      # more text output (notes on data table and references for data and image)
      textOutput("References")
      
    )
)

server <- function(input, output) {
  
  ## FEATURE 2. an interactive slider that will restrict the observations the dataset 'USArrests' to those with 'UrbanPop' values within the range bounded by the 2 values in the slider (id_slider_percUrban[1] & id_slider_percUrban[2]), set by the user on UI, yielding the filtered df 'ArrestsFiltered' (which is the input for scatter plot and data table); Useful to view crime statistics only from the most/least urban states.  
  ArrestsFiltered <- reactive({ #[8]
    USArrests %>% 
      filter(UrbanPop >= input$id_slider_percUrban[1], 
             UrbanPop <= input$id_slider_percUrban[2])
  })

  ## FEATURE 1: A plot depicting how murder rates as a function of assaults across states; based on filtered dataset and therefore reactive to UI filtering of data based on urban population of state. Useful to for visualizing the Murder ~ Assualt realtionship and how it might change based on % urbanization of state.
  output$MurderbyAssaultPlot <- renderPlot({
    ArrestsFiltered() %>%
    ggplot(aes(x=Assault, y=Murder, colour=UrbanPop)) +
      geom_point(alpha = 0.5, size = 2) +
      # below I label data points with their State. Useful to see which US State's data remain if user filters data.
      geom_text(aes(label = rownames(ArrestsFiltered())), size = 3.5, hjust = 0, vjust = 1, angle = 10) + #[9]
      scale_color_gradient(low="blue", high="darkred") +
      xlab("Assault Arrest Rate") +
      ylab("Murder Arrest Rate") +
      ggtitle("Murder Incidence by Assault Incidence in the US States") +
      xlim(0, 400) +
      labs(caption = "Rates are arrests for crime per 100,000.") +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
            axis.title.x =element_text(size=15), axis.title.y = element_text(size=15), plot.title = element_text(size=17)) #[5]
    
  })
  
  ## FEATURE 3. Text output that tells the user the number of  and names of the US states that have an urban population within the range they have selected on the slider (names and number of rows in the filtered dataframe, respectively). This is useful for the user to gauge what subset of the overall data (e.g. 1/3 of States) they are looking at, and where the data are coming from (which States).
  output$DescribeStatesFound <- renderText({
    paste("There are", nrow(ArrestsFiltered()), "states that have a percentage of their population in urban areas between", input$id_slider_percUrban[1], "and", input$id_slider_percUrban[2], 
          ":", paste(rownames(ArrestsFiltered()), collapse = ", "))
  }) #[6]
  
  # FEATURE 4. An image I found on reddit (reference below), that depicts the population desnisty in the USA (useful as a visual indicator of which states are likely to have a higher urban population.)
  output$redditUSApopDens <- renderImage({
    list(src = "www/ScreenShot.png",
         width = "100%",
         height = 300)
  }, deleteFile = F)
  
  output$Textbelowimage <- renderText({
    "See the crime data from those states below:"
  }) # text that goes above the data tabel / below the .png map of the states; just to indicate that the data in the table below are also filtered according to their selected urban population range and orient the user to the interactive data table.
  
  # FEATURE 5. An interactive data table (by DT, calling the filterred data table.) I think the biggest utility this feature adds is that is allows users to search for a specific US state's crime data in the search bar.
  output$CrimeData <- DT::renderDataTable({
    ArrestsFiltered()
  })
  
  # Text to go at the bottom of the app: explains the units of crime statistics displayed in the interactive data table. References the data source and image source.
  output$References <- renderText({
    "Notes. Statistics are arrest numbers for the indicated crime per 100,000 US state residents. UrbanPop is the percent of State population
    in an urban area.  References:
    image from dataisbeautiful [reddit, c. 2019].
    'Population density of the United States made to look like a night-time satellite image [OC]', https://www.reddit.com/r/dataisbeautiful/comments/djvdxn/population_density_of_the_united_states_made_to/.
    Data are c. 1973 from: Rdatasets, USArrests, https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/USArrests.
    This Shiny app is Sidney's Assignment 3B submission for STAT 545 F23. Instructions: https://stat545.stat.ubc.ca/assignments/assignment-b3/."
  })
  
  
}
# Call the shiny app 
shinyApp(ui = ui, server = server)

# References (for code, implementation of widgets, generation, image and data used in) my Shiny App
# [1] Shiny Themes. (n.d.) https://rstudio.github.io/shinythemes/#:~:text=If%20you%20want%20to%20quickly,reload%20or%20restart%20your%20app.
# [2] DataSets (R) for USArrests data and explanation of variable units. 
# [3] dataisbeautiful [reddit, c. 2019].   'Population density of the United States made to look like a night-time satellite image [OC]', https://www.reddit.com/r/dataisbeautiful/comments/djvdxn/population_density_of_the_united_states_made_to/.(.Png)
# [4] Ardini (n.d.). Advancing Your Shiny App https://algoritmaonline.com/advancing-your-shinyapp/#:~:text=To%20add%20an%20image%20in,you%20understand%20how%20it%20works.
# [5] Elferts (2013). https://stackoverflow.com/questions/14942681/change-size-of-axes-title-and-labels-in-ggplot2.
# [6] Gao et al. (2023) https://stat545.stat.ubc.ca/assignments/assignment-b3/ (Assignment instructions)
# [7] Statistical Learning (2020). R. Shiny Basics and User Interface. [Video]. https://www.youtube.com/watch?v=6mJaw5pLtso&t=1009s 
# [8] Coia et al. (2020). How to make a SHiny App in R (Stat 545 Episode 4-B). [Video]. https://www.youtube.com/watch?v=rB4SJGyaGls. 
# [9] ggplot2. "text". (n.d.) https://ggplot2.tidyverse.org/reference/geom_text.html
