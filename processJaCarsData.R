#Author: Denis Bell
#Date: May 16, 2018
#Description: File to provide reports for jacars data


##Load all data
data <- read.csv2("jacardata.csv", sep = ",")

makes <- unique(data$make.1)
models <- unique(data$make.2)


#Stire make and number of occurences
make_count <- ""

make_count <- aggregate(data.frame(count = data$make.1), list(value = data$make.1), length)

#make_count <- ag[order(-ag$count),]

models_count <- aggregate(data.frame(count = data$make.2), list(value = data$make.2), length)
 
models_count <- models_count[order(-models_count$count),]

year_count <-  aggregate(data.frame(count = data$years), list(value = data$years), length)

year_count <- year_count[order(-year_count$count), ]

head(year_count, n = 100L)

#head(models_count, n = 100L)
