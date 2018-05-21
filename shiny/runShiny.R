###################################
#### Initializer For Shiny App ####
###################################

#Add shiny library
library("shiny")

#Retrieve command line arguments
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
        args[1]="/<default shiny project path>/"
}

#Print working directory(Mostly for debugging)
print(args[1])

#Set the working directory
setwd(args[1])

#Run the shiny application on port 1234
runApp(port = 1234)
