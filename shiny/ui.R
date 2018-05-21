#############################################
## Application to load motor verhicle data ##
#############################################


library("shiny")


shinyUI(pageWithSidebar(

	
	headerPanel("Used Car Statistics"),
	
	sidebarPanel(
	
		uiOutput("selectInputMake"),
			
		uiOutput("selectInputModel"),
		
		uiOutput("selectInputYear")

	),
	mainPanel(
		DT::dataTableOutput("motorVehicleTable"),		
		plotOutput("plot"),
		plotOutput("plotAggregated")
	)
))

