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

## DATASETS
insecurity <- readRDS('insecurity.rds')
insecurity.education <- read.csv('Food insecurity linked to obesity - Education.csv', stringsAsFactors=T)
insecurity.income <- read.csv('Food insecurity linked to obesity - Income.csv', stringsAsFactors = T)
policy <- read.csv('CDC_Nutrition__Physical_Activity__and_Obesity_-_Legislation.csv')
time_obese_data <- readRDS("~/Desktop/Vizathon/time_obese_data.rds")
obesity.rate <- data.frame(
	Date = paste(
		"01", "01",
		seq(from=2002, to = 2018, by = 2),
		sep="-"
	),
	Rate = c(15.4, 17.1, 15.4, 16.8, 16.9, 16.9, 17.2, 18.5, 19.3),
	boys = c(16.4, 18.2, 15.9, 17.7, 18.6, 16.7, 17.2, 19.1, 20.5),
	girls= c(14.3, 16.0, 14.9, 15.9, 15.0, 17.2, 17.1, 17.8, 18.0)
)
text <- read.delim("txt mining.txt")
geomap <- readRDS("num_restaurants_state.rds")

## CLEANING DATA
policy$Setting <- as.factor(policy$Setting)
policy$HealthTopic <- as.factor(policy$HealthTopic)
policy$PolicyTopic <- as.factor(policy$PolicyTopic)
obesity.rate$Date <- as.Date(obesity.rate$Date, format = "%d-%m-%Y")
obesity.rate$Year <- year(obesity.rate$Date)

## HELPER FUNCTIONS

## SERVER
server <- function(input, output) {
	## Obesity rate vs food insecurity of different demographics-------------------------------------------------
	# -> obesity rate differs between demographic groups
	output$insecurityEdu <-renderHighchart({
		highchart() %>%
			hc_exporting(enabled=T) %>%
			hc_chart(type = 'bar') %>%
			hc_series( list(name = "Food secure", data = insecurity.education[insecurity.education$X.status=="secure","Percentage"]),
								 list(name = "Food insecure", data = insecurity.education[insecurity.education$X.status=="insecure","Percentage"])) %>%
			hc_xAxis( categories = unique(insecurity.education$Education.level)) %>%
			hc_yAxis( max = 45, title = list(text = "Prevalence of obesity (%)", 
														 align= "high"),
								labels = list(overflow = 'justify') )%>%
			hc_title( text = "Difference between education levels") %>%
			hc_plotOptions(column = list(
				dataLabels = list(enabled = F),
				enableMouseTracking = T ) ) %>%
			hc_legend( 
				layout= 'vertical',
				align= 'right',
				verticalAlign= 'top',
				x= 0,
				y= 250,
				floating= T,
				borderWidth= 1) %>%
			hc_tooltip(
				valueSuffix= ' %'
			)
	})
	
	output$insecurityInc <- renderHighchart({
		highchart() %>% highchart() %>%
			hc_chart(type = 'bar') %>%
			hc_series( list(name = "Food secure", data = insecurity.income[insecurity.income$X.status=="secure","Percentage"]),
								 list(name = "Food insecure", data = insecurity.income[insecurity.income$X.status=="insecure","Percentage"])) %>%
			hc_xAxis( categories = unique(insecurity.income$X.Income.level)) %>%
			hc_yAxis( max = 45, title = list(text = "Prevalence of obesity (%)", 
																			 align= "high"),
								labels = list(overflow = 'justify') )%>%
			hc_title( text = "Difference between income levels") %>%
			hc_plotOptions(column = list(
				dataLabels = list(enabled = F),
				enableMouseTracking = T ) ) %>%
			hc_legend( 
				layout= 'vertical',
				align= 'right',
				verticalAlign= 'top',
				x= 0,
				y= 250,
				floating= T,
				borderWidth= 1) %>%
			hc_tooltip(
				valueSuffix= ' %'
			)
	})
	
	# -> food insecurity affects children
	output$securityChild <- renderHighchart({
		highchart() %>%
			hc_chart(type = 'solidgauge') %>%
			hc_title(text = "Food Secure") %>%
			hc_tooltip( enabled = T,
									pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
									positioner = JS("function (labelWidth, labelHeight) {return {x: 500 - labelWidth / 2,y: 180};}")) %>%
			hc_add_series( name = "Obese mother", data = list(list(color= "#f2c12e", 
																									 radius= '112%',
																									 innerRadius= '88%',
																									 y=83.7))) %>%
			hc_add_series( name = "Obese children", data = list(list(color= "#024873", 
								 																		radius= '87%',
								 																		innerRadius= '63%',
								 																		y=40.5))) %>%
			hc_plotOptions(solidgauge = list(borderWidth = '34px',
																			 dataLabels = list(enabled = F),
																			 enableMouseTracking = T ,
																			 linecap= "round",
																			 stickyTracking= F
			)) %>%
			hc_yAxis(min= 0, max= 100, lineWidth= 0, tickPositions = list()) %>%
			hc_pane(
				startAngle= 0, endAngle= 360,
				background= list(
					list(outerRadius= "112%", innerRadius= "88%", 
							 backgroundColor= JS("Highcharts.Color('#f2c12e').setOpacity(0.1).get()"), 
							 borderWidth= 0),
					list(outerRadius= '87%', innerRadius= '63%',
							 backgroundColor= JS("Highcharts.Color('#024873').setOpacity(0.1).get()"), 
							 borderWidth= 0))
			)
	})
	
	output$insecurityChild <- renderHighchart({
		highchart() %>%
			hc_chart(type = 'solidgauge') %>%
			hc_title(text = "Food Insecure") %>%
			hc_tooltip( enabled = T,
									pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
									positioner = JS("function (labelWidth, labelHeight) {return {x: 500 - labelWidth / 2,y: 180};}")) %>%
			hc_add_series( name = "Obese mother", data = list(list(color= "#f2c12e", 
																														 radius= '112%',
																														 innerRadius= '88%',
																														 y=92.3))) %>%
			hc_add_series( name = "Obese children", data = list(list(color= "#024873", 
																															 radius= '87%',
																															 innerRadius= '63%',
																															 y=76.9))) %>%
			hc_plotOptions(solidgauge = list(borderWidth = '34px',
				dataLabels = list(enabled = F),
				enableMouseTracking = T ,
				stickyTracking= F
			)) %>%
			hc_yAxis(min= 0, max= 100, lineWidth= 0, tickPositions = list()) %>%
			hc_pane(
				startAngle= 0, endAngle= 360,
				background= list(
					list(outerRadius= "112%", innerRadius= "88%", 
							 backgroundColor= JS("Highcharts.Color('#f2c12e').setOpacity(0.1).get()"), 
							 borderWidth= 0),
					list(outerRadius= '87%', innerRadius= '63%',
							 backgroundColor= JS("Highcharts.Color('#024873').setOpacity(0.1).get()"), 
							 borderWidth= 0))
			)
	})
		
		
		
	## Number of health policies over year--------------------------------------------------------------------------
	output$changeOverTime <-renderHighchart({
		highchart() %>%
			hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
			hc_chart(type = 'line') %>%
			hc_series( list(name = 'Nutrition-School', data =time_obese_data$n[time_obese_data$HealthTopic=='Nutrition' &time_obese_data$type == "School"], color='brown' , marker = list(enabled = F), lineWidth = 3 ),
								 list(name = 'Nutrition-Total', data =time_obese_data$n[time_obese_data$HealthTopic=='Nutrition' & time_obese_data$type == "Total"], color='brown' , marker = list(enabled = F), lineWidth = 3 ),
								 list(name = 'Obesity-School', data =time_obese_data$n[time_obese_data$HealthTopic=='Obesity'&time_obese_data$type == "School"], color = 'darkgreen', dashStyle = 'shortDot', marker = list(symbol = 'circle') ),
								 list(name = 'Obesity-Total', data =time_obese_data$n[time_obese_data$HealthTopic=='Obesity'&time_obese_data$type == "Total"], color = 'darkgreen', dashStyle = 'shortDot', marker = list(symbol = 'circle') ),
								 list(name = 'Physical Activity-School', data =time_obese_data$n[time_obese_data$HealthTopic=='Physical Activity'&time_obese_data$type == "School"], color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') ),
								 list(name = 'Physical Activity-Total', data =time_obese_data$n[time_obese_data$HealthTopic=='Physical Activity'&time_obese_data$type == "Total"], color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') )
			)%>%
			hc_xAxis( categories = unique(paste(time_obese_data$Year, paste("Q", time_obese_data$Quarter, sep=""))) ) %>%
			hc_yAxis( title = list(text = "Number of policies enacted"),
								plotLines = list(
									list(
										color = "#ff0000",
										width = 2,
										value = 0 ) )
			) %>%
			hc_plotOptions(column = list(
				dataLabels = list(enabled = F),
				enableMouseTracking = T ) 
			)%>%
			hc_tooltip(table = TRUE,
								 sort = TRUE,
								 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
								 											" {series.name}: {point.y}"),
								 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
			) %>%
			hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
	})
	
	# number of health policies (total and obesity) and rate of obesity in children 2-19 years old (by gender and total over time)
	output$policiesVsObesity <- renderPlot({
		coeff <- 500
		ggplot(policy,aes(Year)) + geom_bar(fill="#f2c12e", color="black")+
			geom_point(data=obesity.year, aes(x=Year, y=n), color="#024873")+
			geom_line(data=obesity.year, aes(x=Year, y=n), color="#024873")+
			theme_classic()+
			geom_line(data = obesity.rate, aes(x = Year, y=Rate*coeff), color="#024873", size=1.5)+
			geom_line(data = obesity.rate, aes(x = Year, y=boys*coeff), color="#024873",linetype="dashed")+
			geom_line(data = obesity.rate, aes(x = Year, y=girls*coeff), color="#024873",linetype="dotted")+
			scale_y_continuous(
				name = "Number of policies",
				sec.axis = sec_axis(~./coeff, name="Obesity Rate")
			)
	})
	
	
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
		wordcloud(df$word, df$freq,scale=c(4, 0.5),colors=brewer.pal(8, "Dark2"), max.words=150)
	}
	
	output$mediaBeautyStandards <- renderPlot({
		wordcld(text)
	}, height = 800, width = 800)
	
	
	## Map data 
	output$charts <- renderHighchart(highchart() %>%
																	 	#hc_title(text = "Sales in US $") %>%
																	 	#hc_subtitle(text = paste0("Product Class: ",input$product)) %>%
																	 	hc_add_series_map(map = usgeojson,  geomap,
																	 										name = "province",
																	 										value = "n",
																	 										joinBy = c("woename", "province")) %>%
																	 	hc_mapNavigation(enabled = T) %>% 
																	 	hc_colorAxis(stops = color_stops()) )
	
}

## UI
ui <- fluidPage(
	
	includeCSS("viz-a-thon21-2.css"),
	
	h3("Prevalence of obesity among adults in 12 states, by food security status and 
			selected sociodemographic characteristics, Behavioral Risk Factor Surveillance System, 2009"),
	fluidRow( column( width = 6, highchartOutput("insecurityEdu") ),
						column( width = 6, highchartOutput('insecurityInc') )
	),
	
	h4("Mother and children's obesity status in food secure and food insecure families"),
	fluidRow( column( width = 6, highchartOutput("insecurityChild") ),
						column( width = 6, highchartOutput("securityChild") )
	),
	
	h3("Health policies over time"),
	fluidRow( column( width = 6,h4("Number of health policies in school and the total number of policies in different health topics over time", align = 'center'), highchartOutput("changeOverTime") ),
						column( width = 6, h4("Relationship between obesity rate in children by gender and number of policies over time", align= "center"),plotOutput("policiesVsObesity"))
						),
	
	h3("Beauty standards in the media"),
	plotOutput("mediaBeautyStandards", width="80%", height="80%"),
	
	highchartOutput("charts")
	)



## CONNECTION
shinyApp(ui = ui, server = server)
