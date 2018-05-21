##################################
## Server function for application
##################################

library(shiny)
library(ggplot2)

shinyServer <- function(input, output) {

	
	##Load all data
	data <- read.csv2("all.csv", sep = ",")

	makes <- unique(data$make.1)
	models <- unique(data$make.2)
        years <- unique(data$years)
	#aggre <- aggregate_by_count(data$makes)

	#Count occurences of a specific column
	aggregate_by_count <- function(column) {

		#Aggregate data              
		aggre <- aggregate(data.frame(count = column), list(value = column), length)

		#Order by count
	   	order <- aggre[order(-aggre$count), ]
	}	
	
	filter_data <- function(data,column,value){

        	filtered_data <- data[(tolower(column) == tolower(value)) & !is.na(data$prices),]
	}
	

	motorVehicleData <- reactive({
           
           if(class(input$year)=="character") {
		
		data <- data[data$years == input$year,]

	   }

	 #  if(class(input$model)=="character") {

          #      data <- na.omit(data[data$models %in% input$model,])

          # }

	  # if(class(input$make)=="character") {

           #     data <- na.omit(data[data$makes %in% input$make,])

           #}

	   # if( req(input$facility) ){	
        	#data <- data[data$Facility %in% input$facility,]
            
	   # }else{
	
	   #	data <- data
	   # }
		#data <- data[data$Year %in% req(input$year),]
	    
	    print(head(aggregate(data$prices, list(value = data$prices), length)))
	   
            data
	})

	motorVehicleDataAggregated <- reactive({	

            aggregate(data$prices, list(value = data$prices), length)
 
        })
	
	#SelectInput with distinct facilities 
  	output$selectInputMake <- renderUI({
    		selectInput("make", "Make:", as.list(makes))
	})
	
	output$selectInputModel <- renderUI ({
		selectInput("model",  "Model:", as.list(models))	
	})
	
	output$selectInputYear <- renderUI ({
                selectInput("year",  "Years:", as.list(years))
        })

	output$motorVehicleTable <- DT::renderDT( 
		motorVehicleData()
	)	
	
        output$plot <- renderPlot({
		print(
			ggplot(
				aggregate_by_count(motorVehicleDataAggregated()),
				aes(x = value,x) 
			) + geom_line() 
				
		)
	})
	
	# output$plotAggregated <- renderPlot({
         #       print(
         #               ggplot(
          #                      motorVehicleDataAggregated(),
           #                     aes(x = value,count)
            #            ) + geom_line()

             #   )
       # })
}
