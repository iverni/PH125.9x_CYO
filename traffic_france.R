#Load packages and Libraries----
# pacman    : This contains tools to more conveniently perform tasks associated with add-on packages.
# snakecase : Collection of miscellaneous utility functions, supporting data transformation
# ggalt     : Support geom_dumbbell()
# devtools  : Collection of R development tools
if(!require(pacman))install.packages("pacman")
if(!require(devtools))install.packages('devtools')
devtools::install_github('bbc/bbplot')    #Load the BBC plots for use with ggplot2
pacman::p_load('devtools',                                # Development 
               'data.table',                              # Data Importation
               'vtreat',                                  #  
               'tidyverse', 'dplyr', 'tidyr', 'stringr',  # Data Manipulation
               'sjmisc', 'snakecase', 'lubridate',        # Data Manipulation
               'ggplot2', 'bbplot', 'ggalt',              # Visualisation 
               'caret', 'randomForest',                   # Classification and Regression 
               'tictoc')                                 # Performance measuring

#Download the files----
##Data Retrieval
tic("Retrieve French Data Gov files")
https://insee.fr/fr/information/2666684
https://www.insee.fr/fr/statistiques/fichier/3720946/commune2019-csv.zip
#2016 Commune
communes_url <- "6d3428b2-3893-45a1-b404-2522a4e77d41"        # Semi colon delimited
datagov_url <- "https://www.data.gouv.fr/en/datasets/r/"
vehicles_url <- "109520e1-47ae-4294-a5b6-6d10c7bae9a6"        # Comma delimited
users_url <- "07bfe612-0ad9-48ef-92d3-f5466f8465fe"           # Comma delimited
places_url <- "9b76a7b6-3eef-4864-b2da-1834417e305c"          # Comma delimited
characteristics_url <- "9a7d408b-dd72-4959-ae7d-c854ec505354" # Comma delimited

#Function module to check if the file has already been downloaded.
download_datefile <- function(url_ref, new_filename) {
  # The French commune codes are csv files with a semi-colon seperator. The file is read using
  if(!file.exists(new_filename)){
    download.file(url_ref, new_filename)
  }else{
    print(new_filename)
    print("File already exist and do not need to be downloaded again")
  }
}

# Code INSEE - Codes for all French Regions, Departments and Communes from the Open platform for French public data provided by the French Government
# The INSEE is The National Institute of Statistics and Economic Studies. It collects, analyses and disseminates information on the French economy and society
# read_csv2 is used instead of read_csv as the files are semi-colon seperated (common approach in the European Union)
# read_csv2 is preferred to read.csv2 because a tibble is created but also due to the intepretation of the commune codes. A code of 08 is treated by default as 8 using read.csv2 which is not desired for this project as the code 08 is the formal code used by the French authorities.
download_datefile(paste0(datagov_url,communes_url),"communes.csv")
communes <- read_csv2("communes.csv")
# The population figure in the dataset is per 1000. 
communes$Population <- as.numeric(communes$Population) * 1000
# Change the name of certain commune features to ensure select statements are not impacted by codepages
setnames(communes, old = c('Code Département','Code Commune'), new = c('DepartCode','CommCode'))

# Download the vehicles file. Ensure that the accident reference is a character so that it is not treated as a double.
# col_types ensures that the accident number 201700000001 is not imported as 2.017e+11.
download_datefile(paste0(datagov_url,vehicles_url),"vehicles.csv")
vehicles <- read_csv("vehicles.csv", col_types = cols(Num_Acc = "c"))

download_datefile(paste0(datagov_url,users_url),"users.csv")
users <- read_csv("users.csv", col_types = cols(Num_Acc = "c"))

download_datefile(paste0(datagov_url,places_url),"places.csv")
places <- read_csv("places.csv", col_types = cols(Num_Acc = "c"))

# Issue of codepage when downloading the data. E.g. "d<e9>partementale" should be "départementale". Therefore the codepage locale is set to Latin 1 to ensure
# all West European characters can be interpreted when loading the file. For a global dataset a more comprehensive locale would be necessary 
download_datefile(paste0(datagov_url,characteristics_url),"characteristics.csv")
characteristics <- read_csv("characteristics.csv", col_types = cols(Num_Acc = "c"), locale = readr::locale(encoding = "latin1"))

toc()

#File cleansing----
#Each file must be checked and cleaned before joining the data together to support exploratory data analysis
tic("Initial Data Cleanse")

##Characteristic File----
###Summary information
summary(characteristics)
#Calculate all the NAs present in each column/feature and then assess if any action is required.
sapply(characteristics, function(x) sum(length(which(is.na(x)))))

#From the analysis of the NAs only the atm (atmospheric conditions) and col (collison) features will be corrected.
#The adr (address) field is the postal address filled in for accidents occurring in built up areas. 
#This is not required (but will be checked by performing a NZV)
#The GPS, lat and long co-ordinate information will only used for graphics
#Only 13 Atmospheric conditions are missing from the dataset
#Replace the atm feature with the median. A histogram analysis of the values shows that the median is "normal" atmos.
hist(characteristics$atm)
characteristics$atm <- ifelse(is.na(characteristics$atm), median(characteristics$atm, na.rm=TRUE), characteristics$atm)
#Repeating the check for Na values shows the issue has been corrected
sapply(characteristics, function(x) sum(length(which(is.na(x)))))

#The median for the collision type is 6 - Other collision. However the value 6 is chosen as the replacement value because
# of "other collision" is the best category to apply for the missing data, irrespective of the median.
characteristics$col <- ifelse(is.na(characteristics$col), median(characteristics$col, na.rm=TRUE), characteristics$col)
sapply(characteristics, function(x) sum(length(which(is.na(x)))))

##Places File----
###Summary information
summary(places)
#Calculate all the NAs present in each column/feature and then assess if any action is required.
sapply(places, function(x) sum(length(which(is.na(x)))))

# circ - Traffic Regime. the missing and 0 value entries in the column denote no recording of the traffic regime
# 1 - One way, 2 - Bidirectional, 3 - Separated carriageways 4 With variable assignment channels
# The missing values are defaulted to category 0 - no indication.
# The same applies to the following 
#   nbv - total number of traffic lanes.
#   vosp - Indicates the existance of a reserved lane. 
#   prof - Profondeur / Terrain type - 1 Flat, 2 Slope, 3 hill top, 4 Hill bottom 
#   surf - Surface condition 9 indicates "other". All 0 values and NAs should be moved to 9
#   infra - Infrastructure e.g. underground, bridge
#   situ - Situation of the accident 
table(places$circ)
places$circ <- ifelse(is.na(places$circ), 0, places$circ)
places$nbv <- ifelse(is.na(places$nbv), 0, places$nbv)
places$vosp <- ifelse(is.na(places$vosp), 0, places$vosp)
places$prof <- ifelse(is.na(places$prof), 0, places$prof)
places$surf <- ifelse(is.na(places$surf), 9, places$surf)
places$surf <- ifelse(places$surf == 0, 9, places$surf)
places$infra <- ifelse(is.na(places$infra), 0, places$infra)
places$situ <- ifelse(is.na(places$situ), 0, places$situ)

# The following columns are removed due to the large NA occurences but also due to their perceived lack of importance for prediction:
## voie - identifies the number of the road
## V1 - numeric index of the route number
## V2 - Letter alphanumeric index of the road
## pr - Home pr number (used for measurements on French roads)
## pr1 - Number of the distances in metres "bornes"
# plan- Road contour
# lartpc- Cetnral solid land width
# larrout - Witdh of the roadway
# env1 - Proximity to schools. Unfortunately the dataset is not clear and the feature should be removed
places <- places %>% select(-voie, -v1, -v2, -pr, -pr1, -plan, -lartpc, -larrout, -env1)

##Users File----
###Summary information
summary(users)
#Calculate all the NAs present in each column/feature and then assess if any action is required.
sapply(users, function(x) sum(length(which(is.na(x)))))

# To contrast how values can be checked for relevancy run a Near Zero Variance. 
# freqRatio: This is the ratio of the percentage frequency for the most common value over the second most common value.
# percentUnique: This is the number of unique values divided by the total number of samples multiplied by 100.
# For percentUnique, the lower the percentage, the lower the number of unique values.
# A high freqRatio indicates that the distributions is heavily skewed. Running a hist() on the location of a pedestrian proves this
# It does not mean we want to remove the data, but it provides an indication of the distribution.
# If the NZV is TRUE then it should be removed
nzv_users <- nearZeroVar(users, saveMetrics = TRUE)
nzv_users
hist(users$locp)

# Now compare NZV of places. Was there a pattern missed
nzv_places <- nearZeroVar(places, saveMetrics = TRUE)
nzv_places
#Yes. It recommends taking out vosp and infra. Lets not remove them but compare this finding with a PCA
#Incidently, a nzv() check on characteristics identified year and GPS and NZV. Not surprising given year = 2017. This is a good demonstration of NZV
nzv_characteristics <- nearZeroVar(characteristics, saveMetrics = TRUE)
nzv_characteristics

# Positon in the accident. Checking the distribution there is no value to include this feature.
table(users$place)
sum(users$place == 1, na.rm = TRUE) / sum(users$place, na.rm = TRUE)  

#   trajet - Reason for travelling.  9 indicates "other". All 0 values and NAs should be moved to 9
users$trajet <- ifelse(is.na(users$trajet), 9, users$trajet)
users$trajet <- ifelse(users$trajet == 0, 9, users$trajet)

table(users$secu)
hist(users$an_nais)

# There are 37 cases of the age not being recorded. The median value is used.
users[which(is.na(users$an_nais)),]
median(users$an_nais, na.rm=TRUE)
users$an_nais <- ifelse(is.na(users$an_nais), median(users$an_nais, na.rm=TRUE), users$an_nais)

# The following columns are removed due to the large NA occurences but also due to their perceived lack of importance for prediction:
## place - #56% missing
## secu - dataset is not sufficiently clear to use
## locp - Pedestrian Location - remove due to NZV analysis
## actp - Action of the Pedestrian - remove due to NZV analysis
## etapt - Was the pedestrian alone, in a group - removed as other pedestrian fields are removed
users <- users %>% select(-place, -secu, -locp, -actp, -etatp)

##Vehicle File----
###Summary information
summary(vehicles)
#Calculate all the NAs present in each column/feature and then assess if any action is required.
sapply(vehicles, function(x) sum(length(which(is.na(x)))))

nzv_vehicles <- nearZeroVar(vehicles, saveMetrics = TRUE)
nzv_vehicles

# The missing values are defaulted to category 0 - no indication.
#   choc - Point of the impact. 0 denotes not specified
#   catv - Vehicle category.  99 indicates "other vehicles". All NAs should be moved to 99
#   obsm - Mobile object struck.  9 indicates "other". All 0 values and NAs should be moved to 9
table(vehicles$manv)
vehicles$choc <- ifelse(is.na(vehicles$choc), 0, vehicles$choc)
vehicles$manv <- ifelse(is.na(vehicles$manv), 00, vehicles$manv)
vehicles$catv <- ifelse(is.na(vehicles$catv), 99, vehicles$catv)
vehicles$obsm <- ifelse(is.na(vehicles$obsm), 9, vehicles$obsm)
vehicles$obsm <- ifelse(vehicles$obsm == 0, 9, vehicles$obsm)

# The following columns are removed due to the large NA occurences but also due to their perceived lack of importance for prediction:
## senc - Direction of the traffic (flow)
## obs - Struck a fixed Obstacle - remove due to NZV analysis
## occutc - Number of occupants in the public transport - remove due to NZV analysis
vehicles <- vehicles %>% select(-obs, -occutc, -senc)

toc()




# Perform a PCA without having performed this manual assessment of every field 
# Isolate user observations with pedestrian information and assess separately. Only complete records. Does it matter


description(vehicles)
str()
glimpse()

n_distinct(unrated)     #How many films where not rated.
n_distinct(edx$movieId) #How many films have been rated
n_distinct(edx$userId)  #How many unique users provided ratings




Learning point: Near zero variance was also performed on the features using the nzv() command however
no benefit was gained over the general performance cost of running the function. Apart from rating and
rating year the majority of features are close to unique.



#Performance improvements for R - refer to O'Reilly 
#Graphically linking data to a map

# Free up memory. The movies dataset is kept in memory in order to perform some validations on movies only. For example number of movies released per year.
Once the other data has been fully cleaned and joined then remove the source data
rm(removed, movielens, ratings, temp)


set.seed(1)


toc()
tic("Joining data")
accidents <- characteristics %>% 
                left_join(places, by = 'Num_Acc') 
%>%
                left_join(communes, by = c("dep", "DepartCode")) 
table(accidents$dep)
table(communes$DepartCode)
ANSEO Communes is not complete

colnames(places)
toc()
##Exploratory Data Analysis
tic("Exploratory Data Analysis")

#Create a textual and a graphical summary of the data¶
#ggpairs() (from the GGally package) to create a pairwise scatterplot.

toc()

##Modelling preparation
###PCA
###KMeans
###HClustering




###Predictor preprocessing
#Near zero was assessed however it is not useful for the MovieLens dataset. 
#The majority of features are between 1:2. The function nearZeroVar is removed as it is processive intensive and does not add any value to this program.
#saveMetrics = TRUE enables detailed analysis of the feature comparison. If you receive an integer0 answer it indicates no features are being processed for removal.
tic("Model preparation")
toc()
###Model 1: Predict the same rating for all movies regardless of user
tic("Model 1: Predict the same rating for all movies regardless of user")
toc()
###Model 2: Modeling movie effects
#Using a linear regression lm() results in the vector memory being exhausted
tic("Model 2: Predict using the movie effect")
toc()

##Results
#Display the results of the models

# If time 
# Geospatial integration
# ANSEO - Need to ensure that the file can be linked to the accident files

