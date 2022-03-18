setwd("~/Desktop/Vizathon")
## PACKAGES
library(tm)
library(wordcloud)
library(shiny)
library(dplyr)
library(highcharter)
library(readxl)
library(stringr)
library(plotly)
library(gganimate)
library(lubridate)
library(countrycode)
library(dplyr)
library(openintro)
library(shinyWidgets)

## DATASETS

## SERVER
server <- function(input, output) {
	output$worldChildFoodInsecurity <- renderHighchart(
		highchart(type="map") %>%
			hc_exporting(enabled= T) %>%
			hc_title(text = "What percentage of children lack consistent access to enough food to support a healthy life?") %>%
			hc_add_series_map(map = usgeojson, Viz.a.thon.database...Sheet8,
												name = "State",
												value = "Child.Food.Insecurity.Rate",
												joinBy = c("woename", "State")) %>%
			hc_mapNavigation(enabled = T) %>% 
			hc_colorAxis(
				maxColor="#01121d",
				stops = list_parse2(data.frame(
					q = 0:3, 
					c = c("#ccdae3","#356d8f","#01243a", "#01121d"),
					stringsAsFactors= T
					))))
	
	output$worldChildObesity2018 <- renderHighchart(
		highchart(type="map") %>%
			hc_exporting(enabled= T) %>%
			hc_title(text = "Obesity rates, children ages 10 to 17") %>%
			hc_add_series_map(map = usgeojson, Viz.a.thon.database...Sheet8,
												name = "State",
												value = "X2018",
												joinBy = c("woename", "State")) %>%
			hc_mapNavigation(enabled = T) %>% 
			hc_colorAxis(
				max = 0.25, maxColor="#01121d",
				stops = list_parse2(data.frame(
					q = 0:3, 
					c = c("#ccdae3","#356d8f","#01243a", "#01121d"),
					stringsAsFactors= T
				))))
}

ui <- fluidPage(
	
	highchartOutput("worldChildFoodInsecurity"),
	
	highchartOutput("worldChildObesity2018")
	
)



## CONNECTION
shinyApp(ui = ui, server = server)
