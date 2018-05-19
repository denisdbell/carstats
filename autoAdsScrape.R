# !Author: Denis Bell
# !Date: May 17, 2018
# !Descripition: R script to scrape www.jacars.net web site

#General purpose data wrangling
library(tidyverse)

# Parsing of HTML/XML files  
library(rvest)

# String manipulation
library(stringr)

# Verbose regular expressions
library(rebus)

# Eases DateTime manipulation
library(lubridate)

url <- "https://www.autoadsja.com/search.asp?SearchVehicleMake=&SearchVehicleModel=&SearchMinVehicleYear=&SearchMaxVehiclePrice=&SearchParish=&SearchSB=5"

numextract <- function(string){
        as.numeric( gsub("[$,]","",string) )
}

get_last_page <- function(html){

        pages_data <- html %>%
                      # The '.' indicates the class
                      html_nodes("body > center > div:nth-child(1) > table:nth-child(2) > tr:nth-child(1) > td:nth-child(1) > center > table:nth-child(1) > tr:nth-child(9) > td:nth-child(2)") %>%
                      # Extract the raw text as a list
                      html_text()

        last_page <- strsplit(pages_data," ")
	
	#Divide total pages by total items displayed. 
	#This gives the last page number	
	last_page_number <- round( as.numeric( unlist(last_page)[1] ) / 10 )

	
}

html <- read_html(url)

get_vehicle_info <- function(url) {

        html <- read_html(url)
	
	raw_data <-  html %>% html_nodes("small") %>% html_text() 
      
        car_data <- strsplit(raw_data[2:11]," ")	

	years <- NA
        makes <- NA
        models <- NA
        prices <- NA
        titles <- NA
        processed_data <- NA
	
	for (i in 1:length(car_data)) {
 
   		years[i] <- car_data[[i]][1]
		makes[i] <- car_data[[i]][2]

		model_price_match <- grepl("\\$",car_data[[i]], perl=TRUE) 
		
		if( any(model_price_match) ){
			  model_price <- strsplit ( car_data[[i]][ model_price_match ], "\\$" )
			  models[i] <- model_price[[1]][1]
	                  prices[i] <- numextract(model_price[[1]][2])

		}else{
			models[i] <- car_data[[i]][3]
			prices[i] <- NA
		}	
	
		titles[i] = paste(makes[i],models[i], sep = " ")
        }		

	processed_data <- rbind( processed_data,data.frame(titles,prices,years,NA,makes,models) )
}

data <- NA

last_page_number <- get_last_page(html)

list_of_pages <- str_c(url, '&page=', 1:last_page_number)

for(url in list_of_pages){
        print(url)
        data <- rbind( data, get_vehicle_info(url) )
}

write.csv(data, file = "autoadsdata.csv")

