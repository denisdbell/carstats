#############################################
## Application to load motor verhicle data ##
#############################################


library("shiny")


shinyUI(pageWithSidebar(

	
	headerPanel("Used Car Statistics"),
	
	sidebarPanel(
	
		uiOutput("selectInputMake"),
			
		uiOutput("selectInputModel"),
		
		uiOutput("selectInputYear"),

		uiOutput("sliderInputPricesMin"),

                uiOutput("sliderInputPricesMax")

	),
	mainPanel(
		htmlOutput("aggregations"),
		plotOutput("plot")
	)
))

