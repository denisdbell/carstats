# !Author: Denis Bell
# !Date: May 15, 2018
# !Descripition: R script to scrape www.jacars.net web site


# General-purpose data wrangling
library(tidyverse)  

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr)   

# Verbose regular expressions
library(rebus)     

# Eases DateTime manipulation
library(lubridate)

url <- "http://www.jacars.net/"

numextract <- function(string){ 
	as.numeric( gsub("[$,]","",string) )
} 

get_last_page <- function(html){

	pages_data <- html %>% 
                      # The '.' indicates the class
                      html_nodes('.PageNum') %>% 
                      # Extract the raw text as a list
                      html_text()    
  
	last_page <- strsplit(pages_data," ")
 
	last_page
 
	as.numeric( unlist(last_page)[4] )   
}


get_vehicle_info <- function(url) {	
	
	html <- read_html(url)
	
	#last_page_number <- get_last_page(first_page)

	#Generate all pages
	#list_of_pages <- str_c(url, '?page=browse&p=', 1:last_page_number)

	#Get all cars on the page
	#vehicle_tag_list <- read_html(url) %>% html_nodes('.thumbnails-ro')

	#Get all titles
	titles <- html %>% html_nodes('.initInfo') %>%  html_nodes('.results-title') %>% html_text()

	prices <- html%>% html_nodes('span') %>% html_nodes('.results-priceE')  %>% html_text() #%>% numextract

	years <- html %>% html_nodes('span') %>% html_nodes('.results-year')  %>% html_text()
 
	transmissions <-  html %>% html_nodes('span') %>% html_nodes('.results-trans')  %>% html_text()

	cars_table <- data.frame(titles,prices,years,transmissions)

	cars_table
}

data <- ""

html <- read_html(url)

last_page_number <- get_last_page(html)

list_of_pages <- str_c(url, '?page=browse&p=', 1:2)

for(url in list_of_pages){
	print(url)
	data <- rbind( data, get_vehicle_info(url) )
}

data$make <- str_split_fixed(data$titles," ",2)

write.csv(data, file = "jacardata.csv")





