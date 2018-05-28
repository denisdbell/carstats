##################################
## Server function for application
##################################

library(shiny)
library(ggplot2)
library(stringi)

shinyServer <- function(input, output) {
	
	#Load all data
	data <- read.csv2("all.csv", sep = ",")
	
	data$make.1 <- toupper(data$make.1)
        data$make.2 <- toupper(data$make.2)
	
	#Filter out all emoty rows
	data <- data[rowSums(is.na(data)) != ncol(data), ]
	
	#Get unique make and years
	makes <- unique(data$make.1)
        years <- unique(data$years)

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
           
           filtered_data <- data[rowSums(is.na(data)) != ncol(data), ]
		
           if(!stri_isempty(input$make)){   
               filtered_data <- filtered_data[filtered_data$make.1 == input$make,]
           
	   }
 
           if(!stri_isempty(input$model)){   
               filtered_data <- filtered_data[filtered_data$make.2 == input$model,]
           }

	   if(!stri_isempty(input$year)){   
               filtered_data <- filtered_data[filtered_data$years == input$year,]
           }	

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
