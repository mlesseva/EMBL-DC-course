# talking abt data.frames

#Import some data
download.file(url = "https://ndownloader.figshare.com/files/2292169",destfile = "data_raw/portal_data_joined.csv")


library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")

head(surveys)

str(surveys)

dim(surveys)
nrow(surveys)
ncol(surveys)

tail(surveys)
#names is equivalent
names(surveys)
colnames(surveys)
rownames(surveys)

summary(surveys)

# indexing and subsetting
surveys[1, 6]

surveys[1, ]
surveys[, 1]

surveys[c(1, 2, 3), c(5, 6)]
surveys[c(1, 2, 3),]

surveys[1:3, 5:6]
surveys[, -1]
surveys[, "sex"]
surveys["sex"]
surveys$plot_id

surveys_200 <- surveys[200,]
surveys[nrow(surveys), ]
nrow(surveys)/2
surveys[nrow(surveys)/2, ]

#Amri's attempt at challenge a list is not a dataframe
surveys_200_again <- c(surveys[200, ])






