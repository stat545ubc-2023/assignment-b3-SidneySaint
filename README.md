## Welcome to my first Shiny app!
### This is a project assigned by STAT 545B, wherein students are asked to develop a simple, interactive Shiny app. Mine was originally developed to facilitate the examination of violent crime data from USA in the 70s, and how trends in this violent crime might relate to population density. I have since (as in, for the last assignment of STAT 545, wherein students may submit an updated version of their earlier Shiny app as their project) updated the app; it is now intended to facilitate the comparison/exploration of trends in violent crime and population density in the US between the 1970s and 2015. 
To this end I have added a second tab to my app which displays population density and murder incident data in the US states from 2015. 

On Tab 1, containing older (c. 1973) crime and population-density data, a plot of the murder incidence, by the assault incidence in each US state in 1973. Users can toggle a slider on the UI (user interface) to filter the states whose data appear in that plot based on what percent of their population lives in urban areas. The same is true for a data table of the violent crime data. Updates: 3 new reactive maps in a side panel. They show the 1973 murder rate, assault rate, and population density overlayed on each state on the map; reactive in that these metrics will not be indicated on any given state on the maps if their population density isn't within the user-defined range (via slider). 

Tab 2 indicates 2015 violent crime and population density data: a side panel displays maps of (i) county-level population estimates from 2015 US census data and (ii) concentrations of murder incidents ('hotspots') in select major urban US cities. The incidents whose locations are plotted in the latter are searchable in a data table in the main frame (it contains the individual records from 2015 of murders in Chicago, Detroit, Fort Worth, Kansas City, Los Angeles, Louisville, New York, Tucson, and Virginia Beach from the Crime Open Database).
 

 :exclamation: 
 Please be patient with this tab (#2; on line or local). On my side, it *takes around 1-2 minutes for the UI to load* (the reactive table contains ~ 2000 rows and I think it slows things down).

**Data Sources**

1973 crime and population density data: from R Datasets "USArrests" (https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/USArrests). More details on this data set, including its original source: **McNeil, D. R.** (1977) *Interactive Data Analysis*. New York: Wiley, are available in other corresponding R Documentation (https://rdrr.io/r/datasets/USArrests.html).

Geospatial (map coordinates of the US states) and 2015 population data: Di Lorenzo (2023), R package 'usmap', https://cran.r-project.org/web/packages/usmap/usmap.pdf. 
Population data are county-level population estimates from the US census. More details on the original source (https://www.census.gov/programs-surveys/popest.html) found on R package documentation (p. 10).

2015 Crime (murder) data from: Ashby, M. (2023). From data set 'homicides15' in R package 'crimedata' 'Accessing open crime data in R'. https://cran.r-project.org/web/packages/crimedata/vignettes/introduction.html.
More details on the original source (Crime Open Database (CODE; https://osf.io/zyaqn/)) are detailed on crimedata package RDocumentation. 

Geospatial (map coordinates of the US states; additional; used for seperate maps): Becker, R., Wilks, A., Brownrigg, R., Mika, T., Deckmyn, A., CRAN. 'maps' R package. (2023) ['map_data' with ggplot2] https://cran.r-project.org/web/packages/maps/index.html (https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_map.html)

## This repository is structured as follows: 

## assignment-b3-SidneySaint/
*   **app.R** 
  This is the essence of the original version of the app. It contains the code that defines the input and output of what appears on its user interface.

*   **updated_app.R** :sparkles:
  This is the essence of the updated version of the app. It contains the code that defines the input and output of what appears on its user interface.

*  **www /** 
  1. ScreenShot.png: Illustrative overlay of population density onto a map of the USA; displayed in the earlier version of the app (c. 2019 by Reddit user 'dataisbeautiful'). (The updated version has reactive plots illustating US state population density in lieu of this image.)

:sparkles: :sparkles:
## Link to a running version of this app with updates for assignment 4B: https://s-idneys.shinyapps.io/sidneysasst4b_stat545/
(  :exclamation:  please have patience waiting for tab 2 to load)

### Link to the running older version of this app: https://s-idneys.shinyapps.io/sidneysasst3b/
