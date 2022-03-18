setwd("~/Desktop/Vizathon")
library(tm)
library(wordcloud)
library(shiny)
library(dplyr)
library(highcharter)

policy <- read.csv('CDC_Nutrition__Physical_Activity__and_Obesity_-_Legislation.csv')
policy$Setting <- as.factor(policy$Setting)
policy$HealthTopic <- as.factor(policy$HealthTopic)
policy$PolicyTopic <- as.factor(policy$PolicyTopic)

time_obese_data <- readRDS("~/Desktop/Vizathon/time_obese_data.rds")

server <- function(input, output) {
	
	#building wordcloud
	wordcld <- function(data){
		words_to_remove <- c("committee", "billhistory", "abstract","introduced", "adopted", "passed", "assembly", 
												 "senate", "house", "amendment", "amended", "appropriations")
		text <- paste(data$Comments, collapse=" ")
		text.corpus <- Corpus(VectorSource(text))
		term.matrix <- TermDocumentMatrix(text.corpus, 
																			control=list(removePunctuation=T,
																									 stopwords=c(words_to_remove, stopwords('english')),
																									 removeNumbers=T,
																									 tolower=T))
		term.matrix <- as.matrix(term.matrix)
		word.freq <- sort(rowSums(term.matrix), decreasing=T)
		df <- data.frame(word=names(word.freq), freq=word.freq) 
		wordcloud(df$word, df$freq,scale=c(2, 0.5),colors=brewer.pal(8, "Dark2"), max.words=100)
	}
	
	filter_data <- reactive({
		if(input$health != "All"){
			data <- policy %>% filter(
				HealthTopic == input$health)
		} else {
			data <- policy
		}
		if(input$policy != "All"){
			data <- data %>% filter(
				PolicyTopic == input$policy)
		}
		if(input$setting != "All"){
			data <- data %>% filter(
				Setting == input$setting)
		} 
		data	
	})
	
	output$wordcloud <- renderPlot({
		data <- filter_data()
		validate(
			need(filter_data() != "", "There are no data of your choice")
		)
		wordcld(data)
	})
	
	
	
	
	
	
	
	
	
	
	#building time series data
	
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
			hc_xAxis( categories = paste(time_obese_data$Year, paste("Q", time_obese_data$Quarter, sep="")) ) %>%
			hc_yAxis( title = list(text = "Number of policies enacted"),
								#labels = list( format = "${value:,.0f} m"),
								plotLines = list(
									list(#label = list(text = "This is a plotLine"),
										color = "#ff0000",
										#dashStyle = 'shortDot',
										width = 2,
										value = 0 ) )
			) %>%
			hc_plotOptions(column = list(
				dataLabels = list(enabled = F),
				#stacking = "normal",
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
	
	
	
	
	
	
	
	
	
	
	
	
	
}










ui <- fluidPage(
	selectInput("health", "Select a health topic:", choices =c(levels(policy$HealthTopic), "All"), selected="All"),
	selectInput("policy", "Select a policy topic:", choices =c(levels(policy$PolicyTopic), "All"), selected="All"),
	selectInput("setting", "Select a setting:", choices =c(levels(policy$Setting), "All"), selected="All"),
	plotOutput("wordcloud"),
	highchartOutput("changeOverTime")
)







shinyApp(ui = ui, server = server)
