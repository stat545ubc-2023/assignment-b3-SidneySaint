#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# ### SIDNEY S. ASST 4B STAT 545
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(maps) # for map_data
library(viridis)# colours and plots
library(usmap) 
library(crimedata) # recent crime data
library(tools) 



#######///##### START OF DATA MANIPULATION CODE #######///#####

# Below is data manipulation (merging data sets, largely; clarifying variables etc).
# Chosen to include here so that users can run their app on local if they would like; without importing any
# data files (i.e., data can be grabbed and datasets created straight from relevant R packages, once installed on
# user's local device, if desired.)
# For the purposes of the assignment, I think the data manipulation code is ignore-able; although it is sparsely annotated (if interest).
#######///#####

data("USArrests") #[2] (Numbers in square brackets are references - full references below) # crime and population data by state for 1973.
## EDIT 4B 
USAmap <- map_data("state") #US state coordinate data (longitude and latitude)
USAmap$state <- USAmap$region
USArrests2 <- USArrests %>% rownames_to_column(var = "state")
USArrests2$state <- tolower(USArrests2$state)
Statelonglat_crime <- inner_join(USAmap, USArrests2, by ="state") # < join State coordinate data and by-state population and crime rate data 
# (for downstream reactive maps on tab 1; "MAP_PlotUrbanPop", "MAP_PlotMurder", and "MAP_PlotAssault". 

# filter out 'negligible manslaughter' records in crime data frame - we are interested primarily in murder rates:
crimedat <- homicides15 %>% filter(offense_type=="murder and nonnegligent manslaughter") %>% 
  mutate(region = case_when(
    fips_state == 17 ~ "illinois",
    fips_state == 26 ~ "michigan",
    fips_state == 48 ~ "texas",
    fips_state == 29 ~ "missouri",
    fips_state == 6 ~ "california",
    fips_state == 21 ~ "kentucky",
    fips_state == 36 ~ "new york",
    fips_state == 4 ~ "arizona",
    fips_state == 51 ~ "virginia",
    TRUE ~ NA_character_
  ), 
 State = case_when(
    fips_state == 17 ~ "Illinois",
    fips_state == 26 ~ "Michigan",
    fips_state == 48 ~ "Texas",
    fips_state == 29 ~ "Missouri",
    fips_state == 6 ~ "California",
    fips_state == 21 ~ "Kentucky",
    fips_state == 36 ~ "New York",
    fips_state == 4 ~ "Arizona",
    fips_state == 51 ~ "Virginia",
    TRUE ~ NA_character_
  ) ) # Here I create almost 2 identical Variables with repeated code - 'region' with lower case state name is to merge these data with current 
# population density data (within that df the 'state' variable is entitled region  and states are lower case ) 'State', with state names in capitals, is for the data table 
# of crime data that users will interact with (both case_when calls decode the fips_state to it's respective US State name).

# Select desired 2015 crime variables for interactive DT (defined downstream & included in UI).
crimeData2015_forUI_desiredVars <- crimedat[, c("uid", "State", "city_name",  "offense_type",  "date_single", "address", "location_type", "longitude", "latitude")]   

crimedat <- left_join(crimedat, USAmap, by ="region") # region is the name of state variable in the USAmap df 
# necessary for to bind this for the spatial overlays downstream of crime data "hotspots" within major urban areas 
# on to the USA map (feature: "PlotUSA_wMurders"). The ggplot overlay of crimes (red dots on the UI on the 2015 murder map; tab 2)
# doesn't work if the crime data don't contain the 'group' / state variable used in mapping as the USAmap data have (so the data frames need to be joined as above).


#######///##### END OF DATA MANIPULATION CODE #######///#####

# Shiny App definition:
# This app is intended to allow users to explore the association between violent crime and population density over time
# in the different US states. One tab displays crime and population density from 1973. The second shows updated data from 2015. 
#Tabs and/or sidebars were requested as TA feedback for my first submission of this app (for assignment B3) and have been added.

# Define UI
ui <- navbarPage("Violent Crimes and Population Density by Time in the USA", theme = shinytheme("journal"), #[17]
                 # Add a tab for older 1973 data (the only data used for Assignment 3B submission)
                 tabPanel("1973",
                          # Add a sidebar panel to the tabs for graphs 
                          sidebarLayout(
                            sidebarPanel(
                              #4B ADDITIONS: 'MAP_Plot*' Features: Reactive Maps of US States, with urban populations (a), Murders (b) , and assualt rates (c) c. 1973 overlayed
                              # These plots replaces what was previously (for asst 3B) a static image of population density in the USA. 
                              # These plots defined in 'server'. They use data spatial data from 'maps' package (df here USAmap) and crime/population data
                              # from USArrests. 
                              plotOutput("MAP_PlotUrbanPop", height = "400px", width = "400px"), # (a)
                              # Retained 3B FEATURE: a slider (described below where input is defined)
                              sliderInput("id_slider_percUrban",
                                          "Percentage of Urban Population in State",
                                          min = 0,
                                          max = 100,
                                          value = c(0, 100)),
                              plotOutput("MAP_PlotMurder", height = "400px", width = "400px"), # (b)
                              plotOutput("MAP_PlotAssault", height = "400px", width = "400px"), #(c)
                              width = 5
                            ),
                            mainPanel(
                              # headers: 
                              h3("Explore the relationship between violent crime and population density over time in the US states."),
                              h5("This tab shows crime and population data from 1973."),
                              # Retained 3B FEATURE: a plot (reactive/ display varies with slider input. Described below where plot is defined)
                              plotOutput("MurderbyAssaultPlot", height = "700px", width = "700px"),
                              # Retained 3B FEATURE: reactive text output (described below where it is defined).
                              textOutput("DescribeStatesFound"), 
                              # Retained 3B FEATURE: Image (Static; described below where image is called)
                              #imageOutput("redditUSApopDens"), #[3],[4] # EDIT removing image for 4B - updating to reactive maps
                              # more text output (non-reactive)
                              textOutput("Textbelowimage"),
                              # Retained 3B FEATURE: interactive data table (described where table defined below)
                              DT::dataTableOutput("CrimeData"), #[6]
                              # more text output (notes on data table and references for data and image)
                              textOutput("ReferencesTAB1_1973"),
                              width = 7
                            )
                          )
                 ),
                 
                 
                 # New tab for updated crime data (2015)
                 tabPanel("2015",
                          sidebarLayout(
                            sidebarPanel(
                              # Map of USA with population density overlayed to visualize between-state variation in population # defined below
                              plotOutput("Plot_2015POPUSstates", height = "400px", width = "400px"),
                              # Map of US states with locations of murder incidents from select major urban cities in the US overlayed (murder incident data from the states that is overlayed on this map is rendered in main panel) # defined below
                              plotOutput("PlotUSA_wMurders", height = "400px", width = "400px"),
                              width = 5
                              
                            ),
                            mainPanel(
                              # headers
                              h3("Population Density and Violent Crime Data from 2015"),
                              h5(" Table: 2015 Crime Open Database (CODE) Reports of Murders in Major Urban Cities"),
                              # this text explains the data in the table and how it can be filtered. Defined below
                              textOutput("NoteOnUsage_TAB2_2015"),
                              # Render the Murder incident data from 2015 to an interactive, searchable data table so users can 
                              # look up specific recorded murders in 2015 from Chicago, Detroit, Fort Worth, Kansas City, Los Angeles, Louisville, New York, Tucson, and Virginia Beach
                              # table defined below
                              DT::dataTableOutput("Crimedat_2015"),
                              # references for data
                              textOutput("ReferencesTAB2_2015"),
                              width = 7
                              
                            )
                          )
                 )
)

# Define Server
server <- function(input, output) {
  
  ## Retained 3B Feature (slider) an interactive slider that will restrict the observations the dataset 'USArrests' (c. 1973) to those with 'UrbanPop' values within the range bounded by the 2 values in the slider (id_slider_percUrban[1] & id_slider_percUrban[2]), set by the user on UI, yielding the filtered df 'ArrestsFiltered' (which is the input for scatter plot and data table); Useful to view crime statistics only from the most/least urban states.  
  ArrestsFiltered <- reactive({ #[8]
    USArrests %>% 
      filter(UrbanPop >= input$id_slider_percUrban[1], 
             UrbanPop <= input$id_slider_percUrban[2])
  })
  
  ## 4B ADDITION: filter the c. 1973 data + USA geo-spatial data - filtered by user input on the single slider included on UI. 
  geographic_data_filtered <- reactive({
    Statelonglat_crime  %>% 
      filter(UrbanPop >= input$id_slider_percUrban[1], 
             UrbanPop <= input$id_slider_percUrban[2])
  })
  ## 4B ADDITION: c. 2015 crime data from package crimedata; wrapped in reactive text so it can be rendered on UI as an interactive data table 
  # for users to search specific crime incidences. 
  crimedat_reactive <- reactive({ 
    crimeData2015_forUI_desiredVars 
  })

  
  ## Retained 3B feature: A plot depicting how 1973 murder rates as a function of assaults across states; based on filtered dataset and therefore reactive to UI filtering of data based on urban population of state. Useful to for visualizing the Murder ~ Assualt realtionship and how it might change based on % urbanization of state.
  output$MurderbyAssaultPlot <- renderPlot({
    ArrestsFiltered() %>%
      ggplot(aes(x=Assault, y=Murder, colour=UrbanPop)) +
      geom_point(alpha = 0.5, size = 2) +
      # below I label data points with their State. Useful to see which US State's data remain if user filters data.
      geom_text(aes(label = rownames(ArrestsFiltered())), size = 3.5, hjust = 0, vjust = 1, angle = 10) + #[9]
      scale_color_gradient(low="darkgrey", high="black") + #EDIT 4B colour of graphs
      xlab("Assault Arrest Rate") +
      ylab("Murder Arrest Rate") +
      ggtitle("Murder Incidence by Assault Incidence in the US States") +
      xlim(0, 400) +
      labs(caption = "Rates are arrests for crime per 100,000.") +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
            axis.title.x =element_text(size=15), axis.title.y = element_text(size=15), plot.title = element_text(size=17))  #[5]
    
  })
  
  ## Retained 3B feature:  Text output that tells the user the number of  and names of the US states that have an urban population (in 1973) within the range they have selected on the slider (names and number of rows in the filtered dataframe, respectively). This is useful for the user to gauge what subset of the overall data (e.g. 1/3 of States) they are looking at, and where the data are coming from (which States).
  output$DescribeStatesFound <- renderText({
    paste("There are", nrow(ArrestsFiltered()), "states that have a percentage of their population in urban areas between", input$id_slider_percUrban[1], "and", input$id_slider_percUrban[2], 
          ":", paste(rownames(ArrestsFiltered()), collapse = ", "))
  }) #[6]
  
  # Retained 3B text feature:
  output$Textbelowimage <- renderText({
    "See the crime data from those states below:"
  }) # text that goes above the data tabel / below the .png map of the states; just to indicate that the data in the table below are also filtered according to their selected urban population range and orient the user to the interactive data table.
  
  # Retained 3B data table feature. An interactive data table (by DT, calling the filtered 1973 data table.) I think the biggest utility this feature adds is that is allows users to search for a specific US state's crime data in the search bar.
  output$CrimeData <- DT::renderDataTable({
    ArrestsFiltered()
  })
  
  #  Retained 3B feature (text updated to reflect 4B submission); Text to go at the bottom of the app: explains the units of 1973 crime statistics displayed in the interactive data table. References the data source and image source.
  output$ReferencesTAB1_1973 <- renderText({
    "Notes. Statistics are arrest numbers for the indicated crime per 100,000 US state residents. UrbanPop is the percent of State population
    in an urban area.  References:
    Crime and Population Data are c. 1973 from: Rdatasets, USArrests, https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/USArrests.
    For geospatial data of US state coordinates (used for maps): Becker, R., Wilks, A., Brownrigg, R., Mika, T., Deckmyn, A., CRAN. 'maps' R package. (2023) ['map_data' with ggplot2] https://cran.r-project.org/web/packages/maps/index.html (https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_map.html). 
    To view more recent violent crime data and population density data from the US states click the second tab '2015'."
  })
  
  ## 4B ADDITIONS: Reactive *maps* of 1973 crime and population density data:
  
  # below for: adding a state label in each graph  / get mean coordinates per state to include as labels on MAP_Plot* reactive map features (Tab1; 1973 data)
  state_labels <- Statelonglat_crime %>%
    group_by(state) %>%
    summarise(long = mean(long),
              lat = mean(lat),
              group = mean(group),
              UrbanPop = mean(UrbanPop),
              Murder =mean(Murder),
              Assault=mean(Assault))
  state_labels$state <- toTitleCase(state_labels$state)
  ###
  
  ## (4B ADDITION): define map of US states (spatial coordinates are x and y; shape is polygon) with urban population c 1973 
  # indicated by the colour of state. Reactive to slider as input; states whose urban population is not within user-defined range 
  # will not be filled in on the graph displayed on UI. 
  output$MAP_PlotUrbanPop <-renderPlot({
    geographic_data_filtered() %>%
      # fill = urban pop - indicate relative urban population by state on graoh
      ggplot(aes(x=long, y=lat, fill=UrbanPop, group=group)) + 
      geom_polygon(color = "white") + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      theme_void() +
      ggtitle('Urban Population of the US States c. 1973') + 
      coord_fixed(1.3) +
      # add state labels defined above on the mean coordinates of their states
      geom_text(data = state_labels, aes(x = long, y = lat, group= group, label = state), colour = "black", size = 2, check_overlap = TRUE) +
      scale_fill_viridis_c(option = "cividis") # defines colour scheme for Urban pop. [13] (Gu & Wu - Mapping instructions and reference code)
    
  })
  
  ## (4B ADDITION): define map of US states with Murder rate c 1973 indicated by colour of state. 
  # syntax parallels map defined above(MAP_PlotUrbanPop), except we fill by the state's murder rate instead rather than population density to 
  # indicate by-state murder rate 
  output$MAP_PlotMurder <-renderPlot({
    geographic_data_filtered() %>%
      ggplot(aes(x=long, y=lat, fill=Murder, group=group)) + 
      geom_polygon(color = "white") + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      theme_void() +
      ggtitle('Murder Rate in the US States c. 1973') + 
      coord_fixed(1.3) +
      geom_text(data = state_labels, aes(x = long, y = lat, group= group, label = state), colour = "black", size = 2, check_overlap = TRUE) +
      scale_fill_viridis_c(option = "rocket")    # colour scheme: red # [13] (Gu & Wu - Mapping instructions and reference code)
     
  })
  
  ## (4B ADDITION): define map of US states with assault rate c 1973 indicated by colour of state. 
  # syntax parallels maps defined above except we fill by assualt rate to indicate by-state assault rate 
  output$MAP_PlotAssault <-renderPlot({
    geographic_data_filtered() %>%
      ggplot(aes(x=long, y=lat, fill=Assault, group=group)) + 
      geom_polygon(color = "white") + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      theme_void() +
      ggtitle('Assualt Rate in the US States c. 1973') + 
      coord_fixed(1.3) +
      geom_text(data = state_labels, aes(x = long, y = lat, group= group, label = state), colour = "black", size = 2, check_overlap = TRUE) +
      scale_fill_viridis_c(option = "rocket") # [13]
    
  })
  
  ## 4B ADDITION: (tab 2/ 2015 data): a plot of US states overlayed with by-county population estimates from US Census 2015. 
  ## Population and geospatial data are US Census data imported from Di Lorenzo (2023), package 'usmap', https://cran.r-project.org/web/packages/usmap/usmap.pdf.)
  output$Plot_2015POPUSstates <- renderPlot({plot_usmap(data=countypop, values="pop_2015", colour="lightblue", labels =TRUE) +
    scale_fill_continuous(low="white", high ="blue") + 
      ggtitle("2015 Population Across US States (Census data") # [13] for explanation and code reference to plot usmap data
    
  })
  
  ## 4B ADDITION: (Tab 2/ 2015 data): a plot of US states (geospatial data from maps package) overlayed with locations of murder incidents in 2015 from  Chicago, Detroit, Fort Worth, Kansas City, Los Angeles, Louisville, New York, Tucson and Virginia Beach.
  #From 2015 USA 'CODE' crime data from crimedata R package. 
  output$PlotUSA_wMurders <- renderPlot({ggplot(data=USAmap, aes(x=long, y=lat, group=group)) + 
    geom_polygon(color = "white") + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    geom_polygon(fill = "white", color = "blue") +
    ggtitle('2015 Murders in US Major Cities (CODE data)') + 
    coord_fixed(1.3) +
    theme_void() +
    geom_point(data=crimedat, aes(x=longitude, y=latitude), alpha = 0.5, colour = "red", fill = "red") # [14]
  })
  
  # 4B ADDITION:(Tab 2/ 2015 data): Include a table of crime reports in Major Urban US cities C. 2015. Crime data are 2015 USA 'CODE' crime data from crimedata R package. 
  output$Crimedat_2015 <- DT::renderDataTable({
    crimedat_reactive()
  })
  
  # 4B ADDITION: (Tab 2/ 2015 Data) Write a note to exlin the usage of data table of 2015 US crimes.
  output$NoteOnUsage_TAB2_2015 <- renderText({
    "Filter by US State, city, date, or address to search for a specific recorded crime incident. 
    Crime data are for the following major urban US cities: Chicago, Detroit, Fort Worth, Kansas City,  Los Angeles, Louisville, New York, Tucson, Virginia Beach."
  })
  
  # 4B ADDITION: (Tab 2/ 2015 Data) References for geospatial data and 2015 crime and population data
  output$ReferencesTAB2_2015 <- renderText({
    "Crime and population data are 2015. 
    References:
    Population and Geospatial State dataL Di Lorenzo (2023), package 'usmap', https://cran.r-project.org/web/packages/usmap/usmap.pdf. 
    Population data  are county-level population estimates from the US census. 
    More details on the original source (https://www.census.gov/programs-surveys/popest.html) found on R package documentation (p. 10).
    Murder data: Ashby, M. (2023). 'Accessing open crime data in R'. https://cran.r-project.org/web/packages/crimedata/vignettes/introduction.html. [via R package 'crimedata'].
    More details on the original source (Crime Open Database (CODE; https://osf.io/zyaqn/)) are detailed on crimedata package RDocumentation. 
    More Geospatial data for US Maps: Becker, R., Wilks, A., Brownrigg, R., Mika, T., Deckmyn, A., CRAN. 'maps' R package. (2023) ['map_data' with ggplot2] https://cran.r-project.org/web/packages/maps/index.html (https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_map.html). 
   Oft-consulted resources for mapping these data: Guo and Wu (n.d.). 'Chapter 41 Different Ways of Plotting U.S. Map in R' https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html.
   Moreno, M. & Basille, M. (2018). 'Drawing beautiful maps programmatically with R, sf and ggplot2 — Part 2: Layers'. https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html.
    This app is Sidney's 4B submission for STAT 545 at UBC."
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
# [10] Di Lorenzo (2023), package 'usmap', https://cran.r-project.org/web/packages/usmap/usmap.pdf. 
# For the 2015 Population data: 'countypop'. These are county-level population estimates from the US census. More details on the original source (https://www.census.gov/programs-surveys/popest.html) found on R package documentation (p. 10).
# [11] Ashby, M. (2023). 'Accessing open crime data in R'. https://cran.r-project.org/web/packages/crimedata/vignettes/introduction.html. [via R package 'crimedata'].
# For the 2015 Crime data. More details on the original source (Crime Open Database (CODE; https://osf.io/zyaqn/)) are detailed on crimedata package RDocumentation. 
# [12] Becker, R., Wilks, A., Brownrigg, R., Mika, T., Deckmyn, A., CRAN. 'maps' R package. (2023) ['map_data' with ggplot2] https://cran.r-project.org/web/packages/maps/index.html (https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_map.html). 
#For the longitude and latitude coordinates of US states used for the interactive maps. 
#[13] Guo and Wu (n.d.). 'Chapter 41 Different Ways of Plotting U.S. Map in R' https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html.
# For introducing me to the 'usmap' package and how to plot that data and US data from map_data. (Used for reactive maps c. 1973 data and 2015 data maps)
#[14] Moreno, M. & Basille, M. (2018). 'Drawing beautiful maps programmatically with R, sf and ggplot2 — Part 2: Layers'. https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
#Used for overlaying the 2015 crime data onto the map of the US states. 
#[15] Gi, X., Qi, J., & Fan,, R. (2022). "The state dataset". https://gexijin.github.io/learnR/the-state-dataset.html#reading-in-and-manipulating-data.
#[16] Christiansen, P. [2021] 'R tutorial: Creating Maps and mapping data with ggplot2'. https://www.youtube.com/watch?v=AgWgPSZ7Gp0&t=314s [YouTube Video]. 
#[17] Jcblum (c. 2015) re: 'How do I add tabs to the dashboardHeader?'. https://community.rstudio.com/t/how-do-i-add-tabs-to-the-dashboardheader/39674 # for adding tabs to my Shiny dashboard



