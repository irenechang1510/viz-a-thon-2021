filter_continent <- reactive({
	if (input$worldreg == "World"){
		children_overweight %>% filter(Year == as.integer(input$worldYear))
	} else {
		children_overweight %>%
			filter(continent == input$worldreg, Year == as.integer(input$worldYear))
	}
	children_overweight <- children_overweight %>% rename(code = "iso.a3")
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