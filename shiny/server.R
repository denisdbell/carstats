##################################
## Server function for application
##################################

library(shiny)
library(ggplot2)
library(stringi)
library(dplyr)

shinyServer <- function(input, output) {
	
	#Load all data
	data <- read.csv2("all.csv", sep = ",", stringsAsFactors=FALSE)
	
	##################################################################
	########		Function Definitions 		##########
	##################################################################
	

	#Function will remove duplicates and NA values. 
        #it will also sort the input and place the text "ALL" 
        #at the beginning of the  list
        sanitize_and_sort_select_input_data <- function(column) {

                c("ALL", sort( na.omit(unique(column) ) ) )
        }
	
	#Prices lider min and max variables
        sliderInputPricesMin = 0  
        sliderInputPricesMax = 0  

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

	#Remove NA's and format numbers as integers
	format_prices <- function(column){
		
		na.omit(as.integer(as.character(na.omit(column)))) 
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
	#years <- sanitize_and_sort_select_input_data(data$years)
	

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
		
	  filtered_data <- filtered_data[
	  		    	   filtered_data$prices >= as.numeric(input$prices_min)
			   	 & filtered_data$prices <= as.numeric(input$prices_max)
				,		            ]           

	   filtered_data	

	})
	
	models <- reactive({
		
	 	filtered_data <- data[ data$make.1 == input$make, ]		

		sanitize_and_sort_select_input_data(filtered_data$make.2)

	})

	years <- reactive({
		
		filtered_data <- data[ data$make.1 == input$make, ]

                sanitize_and_sort_select_input_data(filtered_data$years)

	})

	aggregates <- reactive({
		
		average_price <- round(mean(format_prices(motorVehicleData()$prices)))

                max_price <- max(format_prices(motorVehicleData()$prices))
		
		median_price <- median(format_prices(motorVehicleData()$prices))

		min_price <- min(format_prices(motorVehicleData()$prices))
			
		data.frame(average_price, max_price, median_price, min_price)	

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
                selectInput("year",  "Years:", as.list(years()))
        })
		

        output$sliderInputPricesMin <- renderUI ({
                 
                 sliderInputPricesMin = 1#aggregates()$min_price
        	 sliderInputPricesMax = 15000000#aggregates()$max_price/2   
	         		
                 sliderInput("prices_min", "Minimum Price:",
			     min = sliderInputPricesMin, 
			     max = sliderInputPricesMax,
			     value = sliderInputPricesMin)

        })	

	output$sliderInputPricesMax <- renderUI ({

                 sliderInputPricesMin = 15000000#aggregates()$max_price/2
                 sliderInputPricesMax = 35000000#aggregates()$max_price

                 sliderInput("prices_max", "Maximum Price:",
                             min = sliderInputPricesMin,
                             max = sliderInputPricesMax,
                             value = sliderInputPricesMin)

        })


	output$motorVehicleTable <- DT::renderDT( 
		motorVehicleData()
	)	
	
        output$plot <- renderPlot({
	
		top_models <- head( aggregate_by_count(motorVehicleData()$make.2) )
		print(motorVehicleDataAggregated())		

		print(

			barplot(top_models$count,
			names.arg=top_models$value, main="Top Cars",
   			xlab="Models")  
				
		)
	})

        output$aggregations <- renderText({
		paste0( 
		       "<strong>Average Price is :</strong> ", aggregates()$average_price, "</br>",                                              "<strong>Maximum Price is :</strong>", aggregates()$max_price,"</br>",
		       "<strong>Median Price is :</strong>", aggregates()$median_price
  		      )
	})
	
}
