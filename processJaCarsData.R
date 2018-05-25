#Author: Denis Bell
#Date: May 16, 2018
#Description: File to provide reports for jacars data

library(dplyr)


##Load all data
data <- read.csv2("all.csv", sep = ",")

makes <- unique(data$make.1)
models <- unique(data$make.2)

#Count occurences of a specific column
aggregate_by_count <- function(column) {

   #Aggregate data		
   aggre <- aggregate(data.frame(count = column), list(value = column), length)

   #Order by count
   order <- aggre[order(-aggre$count), ]	
}

filter_data <- function(data,column,value){
	#filtered_data <- data[(column == value) && is.na(data$make.2),]
	
	filtered_data <- data[(tolower(column) == tolower(value)) & !is.na(data$prices),]
}

#head( aggregate_by_count(data$prices), 100L )

result <- filter_data(data,data$make.2,"rav 4")

#result <- filter_data(result,result$years,"2002")

result$prices = as.numeric(as.character(result$prices))

print( summary(result) )




