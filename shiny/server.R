##################################
## Server function for application
##################################

library(shiny)
library(ggplot2)

shinyServer <- function(input, output) {

	
	##Load all data
	data <- read.csv2("all.csv", sep = ",")
	
	data$make.1 <- toupper(data$make.1)
        data$make.2 <- toupper(data$make.2)


	makes <- unique(data$make.1)
	#models <- unique(data$make.2)
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

           data[rowSums(is.na(data)) != ncol(data), ]
	})
	
	models <- reactive({
		
	 	 filtered_data <- data[ data$make.1 == input$make, ]		

		 filtered_data$make.2	
	})

	motorVehicleDataAggregated <- reactive({	

            aggregate(data$prices, list(value = data$prices), length)
 
        })
	
	#SelectInput with distinct facilities 
  	output$selectInputMake <- renderUI({
    		selectInput("make", "Make:", as.list(makes))
	})
	
	output$selectInputModel <- renderUI ({
		selectInput("model",  "Model:", as.list(models()))	
	})
	
	output$selectInputYear <- renderUI ({
                selectInput("year",  "Years:", as.list(years))
        })

	output$motorVehicleTable <- DT::renderDT( 
		motorVehicleData()
	)	
	
        output$plot <- renderPlot({
	
		top_models <- head( aggregate_by_count(motorVehicleData()$make.2) )

		print(

			barplot(top_models$count,
			names.arg=top_models$value, main="Top Cars",
   			xlab="Models")  
				
		)
	})
	
}
