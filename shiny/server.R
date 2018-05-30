##################################
## Server function for application
##################################

library(shiny)
library(ggplot2)
library(stringi)

shinyServer <- function(input, output) {
	
	#Load all data
	data <- read.csv2("all.csv", sep = ",")
	
	##################################################################
	########		Function Definitions 		##########
	##################################################################
	

	#Function will remove duplicates and NA values. 
        #it will also sort the input and place the text "ALL" 
        #at the beginning of the  list
        sanitize_and_sort_select_input_data <- function(column) {

                c("ALL", sort( na.omit(unique(column) ) ) )
        }


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
	
	#############################################################
	##########             Data preparation 	  ###########
        #############################################################
	
	#convert make and model to uppercase
	data$make.1 <- toupper(data$make.1)
        data$make.2 <- toupper(data$make.2)

        #Filter out all empty rows
        data <- data[rowSums(is.na(data)) != ncol(data), ]	

	
	makes <- sanitize_and_sort_select_input_data(data$make.1)
	years <- sanitize_and_sort_select_input_data(data$years)
	

	############################################################
	######## 	Reactive Data Definitions 	  ##########
        ############################################################

	motorVehicleData <- reactive({
           
           filtered_data <- data[rowSums(is.na(data)) != ncol(data), ]
		
           if(input$make != "ALL"){   
               filtered_data <- filtered_data[filtered_data$make.1 == input$make,] 
	   }
 
           if(input$model != "ALL"){   
               filtered_data <- filtered_data[filtered_data$make.2 == input$model,]
           }

	   if(input$year != "ALL"){   
               filtered_data <- filtered_data[filtered_data$years == input$year,]
           }

	   filtered_data	

	})
	
	models <- reactive({
		
	 	filtered_data <- data[ data$make.1 == input$make, ]		

		sanitize_and_sort_select_input_data(filtered_data$make.2)

	})

	motorVehicleDataAggregated <- reactive({	

            aggregate(data$prices, list(value = data$prices), length)
 
        })
	
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
