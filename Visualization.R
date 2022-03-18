setwd("~/Desktop/Vizathon")
library(tm)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(forcats)
library(highcharter)
library(lubridate)
library(countrycode)
library(dplyr)
library(openintro)

policy <- read.csv('CDC_Nutrition__Physical_Activity__and_Obesity_-_Legislation.csv')
policy_nocmt <- policy %>% select(-Comments)
head(policy)
policy.wc <- policy %>% filter(PolicyTopic=="Appropriations")
words_to_remove <- c("committee", "billhistory", "abstract","introduced", "adopted", "passed", "assembly", 
										 "senate", "house", "amendment", "amended", "appropriations", "ideal", "women", "beauty", "media", "body")

text <- read.delim("txt mining.txt")
wordcld <- function(x){
	#text <- paste(x$Comments, collapse=" ")
	text.corpus <- Corpus(VectorSource(text))
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


policy_nocmt <- policy_nocmt %>% filter(Setting != "Other")
ggplot(policy_nocmt, aes(Setting, fill=HealthTopic)) + 
	geom_bar(position="fill")+
	coord_flip()+
	theme_classic()
# community > school > restaurant

policyType <- policy %>%
	filter(HealthTopic == "Obesity") %>%
	group_by(PolicyTopic) %>%
	summarize(n=n()) 
policyType %>%
	mutate(PolicyTopic = fct_reorder(PolicyTopic, n)) %>%
	head(20) %>%
	ggplot(aes(PolicyTopic, n)) + geom_col()+coord_flip() +
	theme_classic()
# for obesity, the most common type is appropriations, food restrictions, BMI.

word.res <- policy %>% filter(HealthTopic=="Obesity")
wordcld(word.res)
# agriculture, farmers

#distribution of obesity policies

#time series data
time.data <- policy 
time.data$EnactedDate <- as.POSIXct(policy$EnactedDate, format ="%m/%d/%Y %r")
time.data$EffectiveDate <- as.POSIXct(policy$EffectiveDate, format ="%m/%d/%Y %r")
obesity.year <- policy %>% filter(HealthTopic=="Obesity") %>%
	group_by(Year) %>%
	summarize(n = n())
coeff <- 500
ggplot(policy,aes(Year)) + geom_bar(fill="yellow", color="black")+
	geom_point(data=obesity.year, aes(x=Year, y=n))+
	geom_line(data=obesity.year, aes(x=Year, y=n))+
	theme_classic()+
	#geom_line(data = time.data.obese, aes(x= Year, y=n))+
	geom_line(data = obesity.rate, aes(x = Year, y=Rate*coeff), color="black", size=1.5)+
	geom_line(data = obesity.rate, aes(x = Year, y=boys*coeff), color="black",linetype="dashed")+
	geom_line(data = obesity.rate, aes(x = Year, y=girls*coeff), color="black",linetype="dotted")+
	scale_y_continuous(
		# Features of the first axis
		name = "Number of policies",
		# Add a second axis and specify its features
		sec.axis = sec_axis(~./coeff, name="Obesity Rate")
	)

# number of policies of different health topic over time (quarters)
time.data <- time.data %>% mutate(date = case_when(Quarter == 1 ~ paste(policy$Year,"01-01", sep="-"),
																									 Quarter == 2 ~ paste(policy$Year,"04-01", sep="-"),
																									 Quarter == 3 ~ paste(policy$Year,"07-01", sep="-"),
																									 Quarter == 4 ~ paste(policy$Year,"10-01", sep="-")))
time.data$date <- as.Date(time.data$date)
time.data.obese <- time.data %>%
	group_by(date, HealthTopic, Quarter, Year) %>%
	summarize(n = n())

time.school.obese <- time.data %>%
	filter(Setting=="School/After School") %>%
	group_by(date, HealthTopic, Quarter, Year) %>%
	summarize(n = n())

time.data.obese$type <- rep("Total", nrow(time.data.obese))
time.school.obese$type <- rep("School", nrow(time.school.obese))
time.obese.combined <- rbind(time.data.obese, time.school.obese)
write.csv(time.obese.combined, "time_obese_data.csv")

ggplot(time.obese.combined, aes(x=date, y=n, color=HealthTopic))+
	geom_line(aes(linetype=type))+
	geom_point()+
	theme_classic()+
	facet_grid(HealthTopic~.)

## OBESITY RATE DATA
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
obesity.rate$Date <- as.Date(obesity.rate$Date, format = "%d-%m-%Y")
obesity.rate$Year <- year(obesity.rate$Date)
coeff <- 200
p <- ggplot()+
	geom_line(data = time.data.obese, aes(x= date, y=n, color=HealthTopic))+
	geom_line(data = obesity.rate, aes(x = Date, y=Rate*coeff), color="black", size=1.5)+
	geom_line(data = obesity.rate, aes(x = Date, y=boys*coeff), color="black",linetype="dashed")+
	geom_line(data = obesity.rate, aes(x = Date, y=girls*coeff), color="black",linetype="dotted")+
	theme_classic()+
	scale_y_continuous(
		# Features of the first axis
		name = "Number of policies",
		# Add a second axis and specify its features
		sec.axis = sec_axis(~./coeff, name="Obesity Rate")
	)


wordcld(text)



geomap <- read.csv('FastFoodRestaurants.csv')
geomap <- geomap %>% select(-websites, -keys, -postalCode, -address)
geomap$province <- geomap$province %>%
	recode("Co Spgs" = "CO",
				 "DC" = "MD")
num_restaurants_state <- geomap %>%
	group_by(province) %>%
	count()
geomap_full  <- num_restaurants_state 
geomap_full$state <- abbr2state(geomap_full$province)
write.csv(geomap_full, "num_restaurants_state.csv")

library(highcharter)
mapdata <- get_data_from_map(download_map_data("custom/world.js"))
children_who_are_overweight_sdgs <- read.csv("children-who-are-overweight-sdgs.csv")
children_who_are_overweight_sdgs <- mapdata %>% 
	select("iso-a3" , continent) %>%
	inner_join(children_who_are_overweight_sdgs, by = c("iso-a3" = "Code"))
write.csv(children_who_are_overweight_sdgs , "children_overweight.csv")

ffres_per_capita <- num_restaurants_state %>% 
	inner_join(nst_est2019_01_NST01, by  = c("state" = "State")) %>%
	mutate(num_res_per_100000 = 100000 * n/Population)
write.csv(ffres_per_capita, "scaled_num_restaurants.csv")



head(Mental.Health.vs..Obesity...Boys)
head(Mental.Health.vs..Obesity...Girls)

Mental.Health.vs..Obesity...Boys$Gender <- rep("Boys", 5)
Mental.Health.vs..Obesity...Girls$Gender <- rep("Girls", 5)
dt <- bind_rows(Mental.Health.vs..Obesity...Boys, Mental.Health.vs..Obesity...Girls)

pdf(file="saving_plot5.pdf")
ggplot(dt, aes(x = Weight, y = Poor.Psychological.Health, fill= Gender))+
	geom_col(position= "dodge")+
	theme_classic()+
	theme(
		axis.text.x = element_text(angle=45, hjust = 1)
	)
dev.off()
