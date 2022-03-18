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
text <- read.delim("txt mining.txt")
scaled_num_restaurants <- readRDS("~/Desktop/Vizathon/scaled_num_restaurants.rds")
mapdata <- get_data_from_map(download_map_data("custom/world.js"))
children_overweight <- readRDS("~/Desktop/Vizathon/children_overweight.rds")
StateStats <- read.csv("~/Desktop/Vizathon/OneDrive_2021-01-12/csv sheets and R file/StateStats.csv")

## CLEANING DATA

## HELPER FUNCTIONS

## SERVER
server <- function(input, output) {
	
	## What the media is talking about beauty standards----------------------------------------------------
	wordcld <- function(x){
		text.corpus <- Corpus(VectorSource(x))
		words_to_remove <- c("ideal", "women", "beauty", "media", "body")
		term.matrix <- TermDocumentMatrix(text.corpus, 
																			control=list(removePunctuation=T,
																									 stopwords=c(words_to_remove, stopwords('english')),
																									 removeNumbers=T,
																									 tolower=T))
		term.matrix <- as.matrix(term.matrix)
		word.freq <- sort(rowSums(term.matrix), decreasing=T)
		df <- data.frame(word=names(word.freq), freq=word.freq) 
		wordcloud(df$word, df$freq,scale=c(3, 0.5),colors=brewer.pal(8, "Dark2"), max.words=150)
	}
	
	output$mediaBeautyStandards <- renderPlot({
		wordcld(text)
	}, height = 600, width = 600)
	
	
	## Number of restaurants by state -----------------------------------------------------------------
	# per 100000 people
	output$num_ffres <- renderHighchart(
		if (input$fastfood == "Per 100000 people"){
			highchart(type="map") %>%
				hc_exporting(enabled= T) %>%
				hc_add_series_map(map = usgeojson,  scaled_num_restaurants,
													name = "state",
													value = "num_res_per_100000",
													joinBy = c("woename", "state")) %>%
				hc_mapNavigation(enabled = T) %>% 
				hc_colorAxis(
					stops = list_parse2(data.frame(
						q = 0:5,
						c = c("#ccdae3","#356d8f","#01243a"),
						stringsAsFactors= T
					)))
		}
		# total
		else{
			highchart(type="map") %>%
				hc_exporting(enabled= T) %>%
				hc_add_series_map(map = usgeojson,  num_restaurants_state,
													name = "state",
													value = "n",
													joinBy = c("woename", "state")) %>%
				hc_mapNavigation(enabled = T) %>% 
				hc_colorAxis(
					stops = list_parse2(data.frame(
						q = 0:5,
						c = c("#ccdae3","#356d8f","#01243a"),
						stringsAsFactors= T
					)))
		}
	)
	
	
	## Obesity in children by state----------------------------------------------------------
	filter_continent <- reactive({
		if (input$worldreg == "World"){
			children_overweight %>% filter(Year == as.integer(input$worldYear))
		} else {
			children_overweight %>%
				filter(continent == input$worldreg, Year == as.integer(input$worldYear))
		}
		children_overweight <- children_overweight %>% rename(code = "iso-a3")
		})
	
	get_map <- reactive({
		if(input$worldreg == "Asia"){
			download_map_data("custom/asia.js")
		} else if (input$worldreg == "Europe"){
			download_map_data("custom/europe.js")
		} else if (input$worldreg == "Africa"){
			download_map_data("custom/africa.js")
		} else if (input$worldreg == "Oceania"){
			download_map_data("custom/oceania.js")
		} else if (input$worldreg == "North America"){
			download_map_data("custom/north-america.js")
		} else if (input$worldreg == "South America"){
			download_map_data("custom/south-america.js")
		} else if (input$worldreg == "World"){
			download_map_data("custom/world.js")
		}
	})
	
	output$worldChildOverweight <- renderHighchart(
		if (input$worldreg == "World"){
			highchart(type="map") %>%
				hc_exporting(enabled= T) %>%
				hc_title(text = "Share of children aged 5-19 who are overweight") %>%
				hc_add_series_map(map = get_map(), filter_continent(),
													name = "Country",
													value = "Indicator.2.2.2b..Prevalence.of.overweight.in.children.aged.2.4.......Past...Unscaled",
													joinBy = c("iso-a3", "code")) %>%
				hc_mapNavigation(enabled = T) %>% 
				hc_colorAxis(
					max = 60, min=0, tickInterval = 10, maxColor="#01121d",
					stops = list_parse2(data.frame(
						q = 0:3, 
						c = c("#ccdae3","#356d8f","#01243a", "#01121d"),
						stringsAsFactors= T
					)))
		} else {
			highchart(type="map") %>%
				hc_exporting(enabled= T) %>%
				hc_title(text = "Share of children aged 5-19 who are overweight") %>%
				hc_add_series_map(map = get_map(), filter_continent(),
													name = "Country",
													value = "Indicator.2.2.2b..Prevalence.of.overweight.in.children.aged.2.4.......Past...Unscaled",
													joinBy = c("iso-a3", "code")) %>%
				hc_mapNavigation(enabled = T) %>% 
				hc_colorAxis(
					max = 60, min=0, tickInterval = 10, maxColor="#01121d",
					stops = list_parse2(data.frame(
						q = 0:3,
						c = c("#ccdae3","#356d8f","#01243a", "#01121d"),
						stringsAsFactors= T
					)))
		}
		)
	
	## Poverty by state--------------------------------------------------------------
	output$distPovertyUS <- renderHighchart(
		highchart(type="map") %>%
			hc_exporting(enabled= T) %>%
			hc_add_series_map(map = usgeojson,  StateStats,
												name = "States",
												value = "X.Poverty",
												joinBy = c("woename", "States")) %>%
			hc_mapNavigation(enabled = T) %>% 
			hc_colorAxis(
				stops = list_parse2(data.frame(
					q = 0:5,
					c = c("#ccdae3","#356d8f","#01243a"),
					stringsAsFactors= T
				))))
	
	
}

## UI
ui <- fluidPage(
	
	includeCSS("viz-a-thon21-2.css"),
	
	h3("Beauty standards in the media"),
	plotOutput("mediaBeautyStandards", width="80%", height="80%"),
	
	h3("Number of fast food restaurants in each state"),
	selectInput("fastfood", "Select: ", choices=c("Total", "Per 100000 people"), selected="Total"),
	highchartOutput("num_ffres"),
	
	h3("Poverty rate in the US"),
	highchartOutput("distPovertyUS"),
	
	selectInput("worldreg", "Select a region: ", choices=c(unique(mapdata$continent)[-4], "World"), selected="World"),
	sliderTextInput("worldYear", "Select a year: ", choices=c("1990", "1995", "2000", "2005", "2010", "2016"), selected="2016", animate = T),
	highchartOutput("worldChildOverweight")
	#fluidRow( column( width = 6, plotOutput("mediaBeautyStandards", width="80%", height = "80%")),
	#					column( width = 6, highchartOutput("charts")))
	
)



## CONNECTION
shinyApp(ui = ui, server = server)
