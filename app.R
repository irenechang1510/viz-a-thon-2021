library(ggplot2, warn.conflicts = FALSE)
library(shinyWidgets, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(tm, warn.conflicts = FALSE)
library(wordcloud2, warn.conflicts = FALSE)
library(highcharter, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(gganimate, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(wordcloud, warn.conflicts = FALSE)
library(countrycode, warn.conflicts = FALSE)
library(openintro, warn.conflicts = FALSE)

## DATASETS
#insecurity <- insecurity
insecurity.education <- read.csv("Food insecurity linked to obesity - Education.csv")
insecurity.income <- read.csv("Food insecurity linked to obesity - Income.csv")
policy <- read.csv("CDC_Nutrition__Physical_Activity__and_Obesity_-_Legislation.csv")
time_obese_data <- read.csv("time_obese_data.csv")
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
scaled_num_restaurants <- read.csv("scaled_num_restaurants.csv")
mapdata <- get_data_from_map(download_map_data("custom/world.js"))
children_overweight <- read.csv("children_overweight.csv")
StateStats <- read.csv("StateStats.csv")

## CLEANING DATA
policy$Setting <- as.factor(policy$Setting)
policy$HealthTopic <- as.factor(policy$HealthTopic)
policy$PolicyTopic <- as.factor(policy$PolicyTopic)
obesity.rate$Date <- as.Date(obesity.rate$Date, format = "%d-%m-%Y")
obesity.rate$Year <- year(obesity.rate$Date)

ui <- fluidPage(
  
  tags$div(id = "title", tags$p("Childhood Obesity: A Cyclical Cycle")),
  tags$style(HTML("#title{background-color: #f2c12e; font-family: Henriette; font-size: 50px; border: 5px solid #024873; text-align: center; }")),
  #tags$style(HTML("#title{background-color: #f2c12e}")),
  
  br(),
  tabsetPanel(type = "tabs", 
              #--------------------------------------------FAMILY--------------------------------------------
              tabPanel("Family",
                       h1(id = "header", "It's in the Family (Genetically and Systematically)"),
                       h4(id = "subtitle", "Obesity often runs in a family. This means that if a child's family has a history of obesity, the child is more likely to experience the affects firsthand."),
                       
                       br(),
                       
                       h3(id = "graphTitle", "Percent of Adult Obesity vs. Childhood Obesity, 2019"),
                       selectInput("reg", "Region:",choices = c("Northeast", "South", "Midwest", "Southwest", "West")),
                       plotOutput("childAdult"),
                       p(id = "description", "This plot shows the correlation between adult obesity and childhood obesity in the United States in different regions. For the Northeast, South, and Southwest, the correlation is postiive, showing that there as the rate of one form of obesity rises, so does the other. For the other two regions, outliers make the correlation weaker, but once outliers are removed, there is a clear connection between adult and childhood obesity as well."),
                       
                       br(),
                       br(),
                       
                       h3(id = "graphTitle","Relative Risk of Overweight and Obesity According to Parental Weight Status"),
                       plotOutput("differentAges"),
                       fluidRow(column(4, offset = 4,
                                       sliderTextInput("ages", "Age:", choices = c("Age 2", "Age 8", "Age 11", "Age 15", "Age 20"), animate = animationOptions(interval = 500, loop = TRUE))
                       )),
                       p(id = "description", "This plot shows the percent of overweight children in a study at different ages based on how many of their parents experience obesity. At each age, the distinction is clear: the children who had two overweight parents were overwhelmingly overweight themselves. The other interesting component is that as the children age, this division continues to grow, showing that childhood obesity has lasting implications.")
                       
                       ),
              
              
              #--------------------------------------------SOCIOECONOMIC--------------------------------------------
              tabPanel("Socioeconomic Status",
                       h1(id = "header", "Socioeconomic Status and Obesity: A Disturbing Connection"),
                       h4(id = "subtitle","With most fast food joints offering $1 menus and most sports programs costing both time and money, there is stark division between childhood obesity in the upper and lower classes."),
                       
                       br(),
                       
                       h3(id = "graphTitle","Prevalence of obesity among adults in 12 states, by food security status and 
                          selected sociodemographic characteristics, Behavioral Risk Factor Surveillance System, 2009"),
                       fluidRow( column( width = 6, highchartOutput("insecurityEdu") ),
                                 column( width = 6, highchartOutput('insecurityInc') )
                       ),
                       p(id = "description", "This series of bar charts shows the difference in obesity rates among the food secure and those who are food insecure for different education levels and income brackets. As can be seen in the graph, the food insecure participants invariably have higher rates of obesity when compared to their food secure counterparts. It is also interesting to note that there is not much distinction here between education levels and income brackets, perhaps because the data is already stratified along food security."),
                       
                       br(),
                       br(),
                       
                       h3(id = "graphTitle","Time Lapse of the Proportion of Physically Inactive Children Based on Income Bracket, 2012-2018"),
                       plotOutput("interactiveChild"),
                       fluidRow(column(4, offset = 4, sliderInput("double", "Year:", min = 2012, max = 2018, value = 2012, step = 1, animate = animationOptions(interval = 500, loop = TRUE)))),
                       p(id = "description", "This time lapse shows the change in the percent of children who are physically inactive between income brackets over a period of 6 years (from 2012 to 2018.) During each year, the percent of children of lower socioeconomic status who were inactive was always greater than those in an income bracket above them. What is most striking is that between 2012 and 2018, this division grows, meaning that in 2018, there were even less opportunities for poorer children to access fitness programs than in 2012."),
                       
                       br(),
                       br(),
                       
                       h3(id = "graphTitle","Mother and children's obesity status in food secure and food insecure families"),
                       fluidRow( column( width = 6, highchartOutput("insecurityChild") ),
                                 column( width = 6, highchartOutput("securityChild") )
                       ),
                       p(id = "description", "The two donut charts depicted above show the difference in weight between families who are food secure and families who aren't. For the food secure families, both the mothers and the children have lower rates of obesity, whereas in the food insecure families, those numbers spike, particuarly when it comes to the children."),
                       
                       br(),
                       br(),
                       
                       h3(id = "graphTitle","Comparison of Weight Gain in Early Infancy for Children of Different Socioeconomic Statuses"),
                       radioButtons("ses", "Socioeconomic Status:", choices = list("Low" = 1, "Intermediate" = 2, "High" = 3, "Comparison" = 4)),
                       plotOutput("infancy"),
                       p(id = "description", "Childhood obesity can often be predicted at a very early age. One such indicator is the amount of weight infants gain within their first three months. The line graph above shows the average weight gained for children of high, intermediate, and low economic standing, with the dotted lines representing the 95% confidence interval. The final graph is a comparison of the three, and it is apparent that the slope for children of the lowest socioeconomic status is the greatest, meaning that in their first three months, those children have gained the most weight and are already at greater risk of childhood obesity.")
                       
                       ),
              
              #--------------------------------------------GEOGRAPHY--------------------------------------------
              tabPanel("Geography",
                       h1(id = "header", "Geography is Everything"),
                       h4(id = "subtitle","Oftentimes, a family's socioeconomic standing influences where they live, and that in turn impacts the family's ability to access resources to combat obesity."),
                       
                       br(),
                       
                       h3(id = "graphTitle","Comparison of the Prevalence of Childhood Obesity, Poverty, and Food Insecurity by State, 2018"),
                       textInput("stateStat", "Enter State Name:", value = "Alabama"),
                       plotOutput("piCharts"),
                       selectInput("choice","Find the Correlation Between:",choices = list("Childhood Obesity and Poverty" = 1, "Food Insecurity and Childhood Obesity" = 2, "Poverty and Food Insecurity" = 3)),
                       fluidRow(column(3,verbatimTextOutput("value"))),
                       p(id = "description", "There is a strong correlation between childhood obesity, poverty, and food insecurity, especially within the United States. As can be seen by typing different state names into the pi charts above, as one of those variables increases or decreases, the others tend to as well. This shows that there is some striking connection between obesity and poverty, and that often one's geography impacts how those variables correlate."),
                       
                       br(),
                       br(),
                       
                       h3(id = "graphTitle","Percent of Public Colleges and Universities With Athletic Programs by State"),
                       textInput("state","Enter State Name: ",value = "Alabama"),
                       fluidRow(column(3, verbatimTextOutput("v"))),
                       plotOutput("inf"),
                       p(id = "description", "Colleges and universities are an incredible asset to a community, particuarly when it comes to sports. When schools around you offer sports programs, they often have available equipment and resources for the community to use. Above is a bar chart of the percentage of public colleges in each state that offer some form of sports programming. While there is no real correlation between this information and childhood obesity, it is something useful to know when choosing where to live as it can often be beneificial to children seeking exercise."),
                       
                       br(),
                       br(),
                       
                       h3(id = "graphTitle", "Number of Fast Food Restaurants in Each State"),
                       selectInput("fastfood", "Select: ", choices=c("Total", "Per 100000 people"), selected="Total"),
                       highchartOutput("num_ffres"),
                       
                       br(),
                       br(),
                       
                       h3(id = "graphTitle", "Poverty Rates in the US by State"),
                       highchartOutput("distPovertyUS"),
                       p(id = "description", "Fast food restaraunts are often the place of choice for those struggling financially as more than a few of them offer $1 or $2 menus that are a cheap alternative to the traditional groceries. However, fast food isn't healthy, and many children who primarily eat fast food struggle with obesity. These two geographs show the correlation between the number of fast food restaraunts in eachs state and their poverty rates.")
                       
                       ),
              
              #--------------------------------------------MENTAL ILLNESS--------------------------------------------
              tabPanel("Mental Health",
                       h1(id = "header", "Mental Health: The Invisible Side Effect"),
                       h4(id = "subtitle","For many suffering with obesity, social beauty standards serve to impede on their mental health. No where is this more apparent than in the case of childhood obesity."),
                       
                       br(),
                       
                       h3(id = "graphTitle", "Words Corresponding with Beauty Standards in the Media"),
                       fluidRow(align="center", plotOutput("mediaBeautyStandards", width="80%", height="80%")),
                       p(id = "description", "The media often influences our perceptions of beauty. Through some data mining of popular publications on beauty and looks, the above word cloud was generated. Among the largest words (showing the most prevalence in the articles) is 'weight,' meaning that the media has defined beauty in the context of size and shape. For many children struggling with obesity, oftentimes the negative body image they gain from consuming media has an impact on their mental health."),
                       
                       br(),
                       br(),
                       
                       h3(id = "graphTitle", "Results of Observational Study on the Impact of Weight on Mental Health, 2012"),
                       plotOutput("MentalHealth"),
                       fluidRow(column(4, offset = 0, selectInput("boyOrGirl", "Gender:", choices=list("Male"=1, "Female"=2))),
                                column(4, offset = 1, selectInput("mentalHealthStatus", "Mental Health Status:", choices = list("Poor Psychological Health"=1, "Suicidal Thoughts in the Past 12 Months"=2, "Lifetime Suicide Attempts"=3)))),
                       p(id = "description", "Because of the negative body image the media often promotes, children who are overweight are more likely than children who are normal weight to struggle with obesity, as shown in the bar graph above. As the severity of the mental illness increases, the disparity between those of normal weight and those who are overweight and obese grows. This represents a clear impact that weight has on self image.")
                       
                       ),
              
              #--------------------------------------------POLICY--------------------------------------------
              tabPanel("Public Policy",
                       h1(id = "header", "Health Public Policy and Finding a Solution"),
                       h4(id = "subtitle","Recently, the government has put a lot of effort into combating the obesity epidemic in America. Through time and experience, certain policies have prevailed while others have fallen to the wayside."),
                       
                       br(),
                       
                       h3(id = "graphTitle", "Public Health Policies Over Time"),
                       fluidRow( column( width = 6,h4("Number of health policies in school and the total number of policies in different health topics over time", align = 'center'), highchartOutput("changeOverTime") ),
                                 column( width = 6, h4("Relationship between obesity rate in children by gender and number of policies over time", align= "center"),
                                         plotOutput("policiesVsObesity"))),
                       p(id = "description", "As the obesity crisis has grown, so has the response. The graphs above represent the different steps public health officials have taken to inform the public. As can be seen in the second of the two graphs, when efforts were made in the early 2000s to sufficiently curb the spread of childhood obesity, the rate of obesity declined in all subsets of Amerian society. However, when some of those policies began to drop, the rate increased sufficiently over time. This shows that obesity is manageable with education and with public health policies; we just need to be brave enough to implement them.")
                       )
              ),
  
  tags$style(HTML("#header{font-family: Henriette; }")),
  tags$style(HTML("#subtitle{font-family: Futura PT; ")),
  tags$style(HTML("#graphTitle{font-family: Henriette; text-align: center; ")),
  tags$style(HTML("#description{font-family: Future PT; }"))
  )

server <- function(input, output, session) {
  #Percent of Adult Obesity vs. Childhood Obesity in Different States
  output$childAdult <- renderPlot ({
    percentData <- read.csv("Percent Adult Obesity vs. Childhood Obesity in Different States - Sheet2.csv")
    if (input$reg == "Northeast") (
      ggplot(data = percentData, aes(x = NortheastChildren, y = NortheastAdults)) +
        geom_point(shape = 18, size = 3, aes(color = "#024873")) +
        geom_smooth(method = "lm", fullrange = TRUE, se = FALSE, aes(alpha = 0.2, color = "#024873")) +
        scale_color_manual(values=c("#024873")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "Northeast", x = "Childhood Obesity", y = "Adult Obesity")
    )
    else if (input$reg == "South") (
      ggplot(data = percentData, aes(x = SouthChildren, y = SouthAdults)) +
        geom_point(shape = 18, size = 3, aes(color = "#b59123")) +
        geom_smooth(method = "lm", fullrange = TRUE, se = FALSE, aes(alpha = 0.2, color = "#b59123")) +
        scale_color_manual(values=c("#b59123")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "South", x = "Childhood Obesity", y = "Adult Obesity")
    )
    else if (input$reg == "Midwest") (
      ggplot(data = percentData, aes(x = MidwestChildren, y = MidwestAdults)) +
        geom_point(shape = 18, size = 3, aes(color = "#f5cd58")) +
        geom_smooth(method = "lm", fullrange = TRUE, se = FALSE, aes(alpha = 0.2, color = "#f5cd58")) +
        scale_color_manual(values=c("#f5cd58")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "Midwest", x = "Childhood Obesity", y = "Adult Obesity")
    )
    else if (input$reg == "Southwest") (
      ggplot(data = percentData, aes(x = SouthwestChildren, y = SouthwestAdults)) +
        geom_point(shape = 18, size = 3, aes(color = "#d94b2b")) +
        geom_smooth(method = "lm", fullrange = TRUE, se = FALSE, aes(alpha = 0.2, color = "#d94b2b")) +
        scale_color_manual(values=c("#d94b2b")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "Southwest", x = "Childhood Obesity", y = "Adult Obesity")
    )
    else if (input$reg == "West") (
      ggplot(data = percentData, aes(x = WestChildren, y = WestAdults)) +
        geom_point(shape = 18, size = 3, aes(color = "#03658c")) +
        geom_smooth(method = "lm", fullrange = TRUE, se = FALSE, aes(alpha = 0.2, color = "#03658c")) +
        scale_color_manual(values=c("#03658c")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "West", x = "Childhood Obesity", y = "Adult Obesity")
    )
  })
  
  #Relative Risk of Overweight and Obesity at Each Age Accordng to Parental Weight Status
  output$differentAges <- renderPlot({
    relativeRisk <- read.csv("Relative risk of overweight and obesity (owt) at each age according to parental weight status - Sheet1.csv")
    relativeRisk$ObeseParents <- factor(relativeRisk$ObeseParents, levels = c("None", "One", "Both"))
    if (input$ages == "Age 2") (
      ggplot(data = relativeRisk, aes(x = ObeseParents, y = Age.2)) +
        geom_bar(position = "dodge", stat = "identity", aes(fill = "#024873", size = 1), color = "#f2c12e") +
        scale_fill_manual(values = c("#024873")) +
        scale_color_manual(values = c("#f2c12e")) + 
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(0,60) +
        labs(x = "Number of Obese Parents", y = "Percent of Overweight Participants in Study", title = "Age 2")
    )
    else if (input$ages == "Age 8") (
      ggplot(data = relativeRisk, aes(x = ObeseParents, y = Age.8)) + 
        geom_bar(position = "dodge", stat = "identity", aes(fill = "#024873", size = 1), color = "#f2c12e") +
        scale_fill_manual(values = c("#024873")) +
        scale_color_manual(values = c("#f2c12e")) +
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(0,60) +
        labs(x = "Number of Obese Parents", y = "Percent of Overweight Participants in Study", title = "Age 8")
    )
    else if (input$ages == "Age 11") (
      ggplot(data = relativeRisk, aes(x = ObeseParents, y = Age.11)) + 
        geom_bar(position = "dodge", stat = "identity", aes(fill = "#024873", size = 1), color = "#f2c12e") +
        scale_fill_manual(values = c("#024873")) +
        scale_color_manual(values = c("#f2c12e")) +
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(0,60) +
        labs(x = "Number of Obese Parents", y = "Percent of Overweight Participants in Study", title = "Age 11")
    )
    else if (input$ages == "Age 15") (
      ggplot(data = relativeRisk, aes(x = ObeseParents, y = Age.15)) + 
        geom_bar(position = "dodge", stat = "identity", aes(fill = "#024873", size = 1), color = "#f2c12e") +
        scale_fill_manual(values = c("#024873")) +
        scale_color_manual(values = c("#f2c12e")) +
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(0,60) +
        labs(x = "Number of Obese Parents", y = "Percent of Overweight Participants in Study", title = "Age 15")
    )
    else if (input$ages == "Age 20") (
      ggplot(data = relativeRisk, aes(x = ObeseParents, y = Age.20)) + 
        geom_bar(position = "dodge", stat = "identity", aes(fill = "#024873", size = 1), color = "#f2c12e") +
        scale_fill_manual(values = c("#024873")) +
        scale_color_manual(values = c("#f2c12e")) +
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(0,60) +
        labs(x = "Number of Obese Parents", y = "Percent of Overweight Participants in Study", title = "Age 20")
    )
  })
  
  ## Obesity rate vs food insecurity of different demographics-------------------------------------------------
  # -> obesity rate differs between demographic groups
  output$insecurityEdu <-renderHighchart({
    highchart() %>%
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
  
  #Physically Inactive Children Based on Income Bracket
  output$interactiveChild <- renderPlot({
    print(input$double)
    physicallyInactive <- read.csv("PhysicallyInactive - Sheet2.csv")
    physicallyInactive$Income <- factor(physicallyInactive$Income, levels = c("Under $25,000", "$25,000-$49,999", "$50,000-$74,999", "$75,000-$99,999", "$100,000+"))
    if (input$double == 2012) (
      ggplot(data = physicallyInactive, aes(x = Income, group = 1, y = Year.2012, color = "#024873")) +
        geom_point(shape = 18, size = 3) +
        geom_line() +
        ylim(0,35) + 
        scale_color_manual(values=c("#024873")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "2012", x = "", y = "Percent of American Children") +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust=1.1))
    )
    else if (input$double == 2013) (
      ggplot(data = physicallyInactive, aes(x = Income, group = 1, y = Year.2013, color = "f6d56d")) +
        geom_point(shape = 18, size = 3) +
        geom_line() +
        ylim(0,35) +
        scale_color_manual(values=c("#024873")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "2013", x = "", y = "Percent of American Children") +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust=1.1))
    )
    else if (input$double == 2014) (
      ggplot(data = physicallyInactive, aes(x = Income, group = 1, y = Year.2014, color = "f6d56d")) +
        geom_point(shape = 18, size = 3) +
        geom_line() +
        ylim(0,35) +
        scale_color_manual(values=c("#024873")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "2014", x = "", y = "Percent of American Children") +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust=1.1))
    )
    else if (input$double == 2015) (
      ggplot(data = physicallyInactive, aes(x = Income, group = 1, y = Year.2015, color = "f6d56d")) +
        geom_point(shape = 18, size = 3) +
        geom_line() +
        ylim(0,35) +
        scale_color_manual(values=c("#024873")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "2015", x = "", y = "Percent of American Children") +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust=1.1))
    )
    else if (input$double == 2016) (
      ggplot(data = physicallyInactive, aes(x = Income, group = 1, y = Year.2016, color = "f6d56d")) +
        geom_point(shape = 18, size = 3) +
        geom_line() +
        ylim(0,35) +
        scale_color_manual(values=c("#024873")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "2016", x = "", y = "Percent of American Children") +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust=1.1))
    )
    else if (input$double == 2017) (
      ggplot(data = physicallyInactive, aes(x = Income, group = 1, y = Year.2017, color = "f6d56d")) +
        geom_point(shape = 18, size = 3) +
        geom_line() +
        ylim(0,35) +
        scale_color_manual(values=c("#024873")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "2017", x = "", y = "Percent of American Children") +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust=1.1))
    )
    else if (input$double == 2018) (
      ggplot(data = physicallyInactive, aes(x = Income, group = 1, y = Year.2018, color = "f6d56d")) +
        geom_point(shape = 18, size = 3) +
        geom_line() +
        ylim(0,35) +
        scale_color_manual(values=c("#024873")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "2018", x = "", y = "Percent of American Children") +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust=1.1))
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
  
  #Socioeconomic Status and Weight Gain in Early Infancy
  output$infancy <- renderPlot ({
    seData <- read.csv("Socioeconomic status and weight gain in early infancy - Sheet1.csv")
    print(input$ses)
    if (input$ses == "1") (
      ggplot(data = seData, aes(x = Month, y = Low)) +
        scale_color_manual(values = c("#f2c12e","#024873")) +
        geom_line(aes(y = Low, color = "Lower Income")) + 
        geom_segment(aes(x = 0, xend = 0, y = -0.65, yend = -0.49, color = "Lower"), linetype = "twodash") + 
        geom_segment(aes(x = 3, xend = 3, y = -0.28, yend = -0.07, color = "Lower"), linetype = "twodash") +
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(-1,0) +
        labs(title = "Low Socioeconomic Status", x = "Months Since Birth", y = "Average Birth Weight SDS")
    )
    else if(input$ses == "2") (
      ggplot(data = seData, aes(x = Month, y = Intermediate)) +
        scale_color_manual(values = c("#f2c12e","#024873")) +
        geom_line(aes(y = Intermediate, color = "Intermediate Income")) + 
        geom_segment(aes(x = 0, xend = 0, y = -0.61, yend = -0.42, color = "Intermediate"), linetype = "twodash") + 
        geom_segment(aes(x = 3, xend = 3, y = -0.27, yend = -0.05, color = "Intermediate"), linetype = "twodash") +
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(-1,0) +
        labs(title = "Intermediate Socioeconomic Status", x = "Months Since Birth", y = "Average Birth Weight SDS")
    )
    else if(input$ses == "3") (
      ggplot(data = seData, aes(x = Month, y = High)) +
        scale_color_manual(values = c("#f2c12e","#024873")) +
        geom_line(aes(y = High, color = "Higher Income")) + 
        geom_segment(aes(x = 0, xend = 0, y = -0.61, yend = -0.51, color = "Higher"), linetype = "twodash") + 
        geom_segment(aes(x = 3, xend = 3, y = -0.39, yend = -0.27, color = "Higher"), linetype = "twodash") +
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(-1,0) +
        labs(title = "High Socioeconomic Status", x = "Months Since Birth", y = "Average Birth Weight SDS")
    )
    else if(input$ses == "4") (
      ggplot(data = seData, aes(x = Month)) +
        scale_color_manual(values = c("#03658c","#b59123","#d94b2b")) +
        geom_line(aes(y = High, color = "Higher Income")) +
        geom_segment(aes(x = 0, xend = 0, y = -0.61, yend = -0.51, color = "Higher Income"), linetype = "twodash") + 
        geom_segment(aes(x = 3, xend = 3, y = -0.39, yend = -0.27, color = "Higher Income"), linetype = "twodash") +
        geom_line(aes(y = Intermediate, color = "Intermediate Income")) + 
        geom_segment(aes(x = 0, xend = 0, y = -0.61, yend = -0.42, color = "Intermediate Income"), linetype = "twodash") + 
        geom_segment(aes(x = 3, xend = 3, y = -0.27, yend = -0.05, color = "Intermediate Income"), linetype = "twodash") +
        geom_line(aes(y = Low, color = "Lower Income")) +
        geom_segment(aes(x = 0, xend = 0, y = -0.65, yend = -0.49, color = "Lower Income"), linetype = "twodash") + 
        geom_segment(aes(x = 3, xend = 3, y = -0.28, yend = -0.07, color = "Lower Income"), linetype = "twodash") +
        theme_minimal() +
        labs(title = "", x = "Months Since Birth", y = "Average Birth Weight SDS")
    )
  })
  
  #State Statistics
  stateInfo <- read.csv("StateStats.csv")
  output$piCharts <- renderPlot({
    validate(
      need(input$stateStat != "", "Not a valid state")
    )
    theState <- stateInfo %>% filter(States == input$stateStat)
    childrenPercent <- theState$X.Children
    povertyPercent <- theState$X.Poverty
    foodinsecurePercent <- theState$X.FoodInsecure
    par(mfrow = c(1,3))
    pie(c(childrenPercent, 1-childrenPercent), labels = c(childrenPercent, 1-childrenPercent), col = c("#024873", "#a3a8a8"), main = "Prevalence of\nChildhood Obesity")
    pie(c(povertyPercent, 1-povertyPercent), labels = c(povertyPercent, 1-povertyPercent), col = c("#f2c12e","#a3a8a8"), main = "Prevalence of\nPoverty")
    pie(c(foodinsecurePercent, 1-foodinsecurePercent), labels = c(foodinsecurePercent, 1-foodinsecurePercent), col = c("#d94b2b","#a3a8a8"), main = "Prevalence of\nFood Insecurity")
  })
  output$value <- renderText({
    if (input$choice == 1) (
      paste("The correlation between childhood obesity and povery in the United States is", 0.65)
    )
    else if (input$choice == 2) (
      paste("The correlation between food insecurity and childhood obesity in the United States is", 0.64)
    )
    else if (input$choice == 3) (
      paste("The correlation between poverty and food insecurity in the United States is", 0.81)
    )
  })
  
  #Percent of Public Sports Teams Offering Sports Programs
  sportsTeams <- read.csv("Public Sports Teams - PublicSportsTeams.csv")
  output$v <- renderText({
    df <- data.frame(ID = sportsTeams$Percent, ITEM = sportsTeams$State)
    theState <- df %>% filter(ITEM == input$state)
    paste("The proportion of public colleges in", input$state, "with sports programs is", theState$ID)
  })
  output$inf <- renderPlot({
    newTeams <- sportsTeams[order(sportsTeams$X.Children),]
    lowestToHighest <- newTeams[1:51,1]
    sportsTeams$State <- factor(sportsTeams$State, levels = lowestToHighest)
    ggplot(data = sportsTeams, aes(x = State, y = Percent, fill = X.Children)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient(low = "#f5cd58", high = "#f27c38") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust=1.1)) +
      labs(title = "", y = "Percent with Athletic Programs", fill = "Childhood Obesity (%)")
  })
  
  ## Number of restaurants by state -----------------------------------------------------------------
  # per 100000 people
  output$num_ffres <- renderHighchart(
    if (input$fastfood == "Per 100000 people"){
      highchart(type="map") %>%
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
      numRest <- read.csv("num_restaurants_state.csv")
      highchart(type="map") %>%
        hc_add_series_map(map = usgeojson,  numRest,
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
  
  ## Poverty by state--------------------------------------------------------------
  output$distPovertyUS <- renderHighchart(highchart(type="map") %>%
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
  
  #Mental Health vs. Obesity
  mentalHealthBoys <- read.csv("Mental Health vs. Obesity - Boys.csv")
  mentalHealthGirls <- read.csv("Mental Health vs. Obesity - Girls.csv")
  output$MentalHealth <- renderPlot({
    mentalHealthBoys$Weight <- factor(mentalHealthBoys$Weight, levels = c("Severe Underweight", "Moderate Underweight", "Normal Weight", "Moderate Overweight", "Obesity"))
    mentalHealthGirls$Weight <- factor(mentalHealthBoys$Weight, levels = c("Severe Underweight", "Moderate Underweight", "Normal Weight", "Moderate Overweight", "Obesity"))
    if (input$boyOrGirl == 1) (
      if (input$mentalHealthStatus == 1) (
        ggplot(data = mentalHealthBoys, aes(x = Weight, y = Poor.Psychological.Health)) +
          geom_bar(stat = "identity", position = "dodge", aes(fill = "#03658c", color = "#b59123", size = 1)) +
          scale_fill_manual(values = c("#03658c")) +
          scale_color_manual(values = c("#b59123")) +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(x = "Weight Status of Patients", y = "Percent of Participants in Study")
      )
      else if (input$mentalHealthStatus == 2) (
        ggplot(data = mentalHealthBoys, aes(x = Weight, y = Suicidal.Thoughts.in.the.Past.12.Months)) +
          geom_bar(stat = "identity", position = "dodge", aes(fill = "#03658c", color = "#b59123", size = 1)) +
          scale_fill_manual(values = c("#03658c")) +
          scale_color_manual(values = c("#b59123")) +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(x = "Weight Status of Patients", y = "Percent of Participants in Study")
      )
      else if (input$mentalHealthStatus == 3) (
        ggplot(data = mentalHealthBoys, aes(x = Weight, y = Lifetime.Suicide.Attempts)) +
          geom_bar(stat = "identity", position = "dodge", aes(fill = "#03658c", color = "#b59123", size = 1)) +
          scale_fill_manual(values = c("#03658c")) +
          scale_color_manual(values = c("#b59123")) +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(x = "Weight Status of Patients", y = "Percent of Participants in Study")
      )
    )
    
    else if (input$boyOrGirl == 2) (
      if (input$mentalHealthStatus == 1) (
        ggplot(data = mentalHealthGirls, aes(x = Weight, y = Poor.Psychological.Health)) +
          geom_bar(stat = "identity", position = "dodge", aes(fill = "#d94b2b", color = "#b59123", size = 1)) +
          scale_fill_manual(values = c("#d94b2b")) +
          scale_color_manual(values = c("#b59123")) +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(x = "Weight Status of Patients", y = "Percent of Participants in Study")
      )
      else if (input$mentalHealthStatus == 2) (
        ggplot(data = mentalHealthGirls, aes(x = Weight, y = Suicidal.Thoughts.in.the.Past.12.Months)) +
          geom_bar(stat = "identity", position = "dodge", aes(fill = "#d94b2b", color = "#b59123", size = 1)) +
          scale_fill_manual(values = c("#d94b2b")) +
          scale_color_manual(values = c("#b59123")) +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(x = "Weight Status of Patients", y = "Percent of Participants in Study")
      )
      else if (input$mentalHealthStatus == 3) (
        ggplot(data = mentalHealthGirls, aes(x = Weight, y = Lifetime.Suicide.Attempts)) +
          geom_bar(stat = "identity", position = "dodge", aes(fill = "#d94b2b", color = "#b59123", size = 1)) +
          scale_fill_manual(values = c("#d94b2b")) +
          scale_color_manual(values = c("#b59123")) +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(x = "Weight Status of Patients", y = "Percent of Participants in Study")
      )
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
  
  #beauty standards word cloud
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
  
  # number of health policies (total and obesity) and rate of obesity in children 2-19 years old (by gender and total over time)
  output$policiesVsObesity <- renderPlot({
    coeff <- 500
    obesity.year <- policy %>% filter(HealthTopic=="Obesity") %>%
      group_by(Year) %>%
      summarize(n = n())
    ggplot(policy,aes(Year)) + geom_bar(aes(fill="Number of Policies Total"), color="black")+
      scale_fill_manual(values = c("#f2c12e"))+
      geom_point(data=obesity.year, aes(x=Year, y=n, color = "Number of Obesity Related Policies"))+
      geom_line(data=obesity.year, aes(x=Year, y=n, color = "Number of Obesity Related Policies"))+
      scale_color_manual(values = c("#024873"))+
      theme_classic()+
      geom_line(data = obesity.rate, aes(x = Year, y=Rate*coeff, linetype = "Proportion of Obese People in the US"), color="#024873", size=1.5) +
      geom_line(data = obesity.rate, aes(x = Year, y=boys*coeff, linetype = "Proportion of Obese Boys"), color="#024873") +
      geom_line(data = obesity.rate, aes(x = Year, y=girls*coeff, linetype = "Proportion of Obese Girls"), color="#024873") +
      labs(linetype = "", fill = "", color = "") +
      scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
      scale_y_continuous(
        name = "Number of policies",
        sec.axis = sec_axis(~./coeff, name="Obesity Rate")
      )
  })
}

shinyApp(ui, server)