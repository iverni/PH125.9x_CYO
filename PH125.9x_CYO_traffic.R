#Load packages and Libraries ***********************
# broom     : Takes the messy output of built-in functions in R and turns them into tidy data frames.
# caret     : Set of functions to streamline prediction model creation
# ggalt     : Support geom_dumbbell()
# GGally    : Multivariate plots
# devtools  : Collection of R development tools
# openxlsx  : Simplifies the creation of Excel .xlsx files without the need for Java
# pacman    : This contains tools to more conveniently perform tasks associated with add-on packages.
# plotly    : Graphing library makes interactive, publication-quality graphs online
# pROC      : Tools for visualizing, smoothing and comparing receiver operating characteristic (ROC curves)
# readxl    : Excel xlsx integration from Hadley Wickham
# rgdal     : Provides bindings to the 'Geospatial' Data Abstraction Library ('GDAL')
# rpart.plot: Extends plot.rpart() and text.rpart() in the 'rpart' package.
# snakecase : Collection of miscellaneous utility functions, supporting data transformation
# stringi   : Character String Processing Facilities. Used for formatting lat/long in this project
# vcd       : Support Mosaics
if(!require(pacman))install.packages("pacman")
if(!require(devtools))install.packages('devtools')
devtools::install_github('bbc/bbplot')    #Load the BBC plots for use with ggplot2
pacman::p_load('devtools',                                # Development 
               'data.table','readxl',                     # Data Importation
               'tidyverse', 'dplyr', 'tidyr', 'stringr',  # Data Manipulation
               'sjmisc', 'snakecase', 'lubridate',        # Data Manipulation
               'stringi',                                 # Data Manipulation
               'ggplot2', 'bbplot', 'ggalt','GGally',     # Visualisation   
               'vcd',                                     # Visualisation 
               'rgdal', 'plotly',                         # Cartography         
               'caret', 'rpart.plot', 'pROC',             # Classification and Regression 
               'tictoc')                                  # Performance measuring

#Download the files ***********************
##Data Retrieval
tic("Retrieve French Data Gov files")
#data.gouv.fr is an open platform for French public data intended to encourage the reuse of data beyond the primary use of the data by the administration. 
#The French traffic datasets from 2017 are selected as it provided detailed accident information for a calendar year. It also presents several data science challenges (data cleanse and categorical data structure).
#The data is sourced from the accident reports made by the driver/police unit (police, gendarmerie, etc.) who intervened at the accident site. 
# These data is collected in a sheet entitled "bulletin of accident analysis". All of these forms constitute the national file of personal injury traffic known as "BAAC file" administered by the National Interministerial Observatory of Road Safety "ONISR".
datagov_url <- "https://www.data.gouv.fr/en/datasets/r/"
vehicles_url <- "109520e1-47ae-4294-a5b6-6d10c7bae9a6"        # Comma delimited
users_url <- "07bfe612-0ad9-48ef-92d3-f5466f8465fe"           # Comma delimited
places_url <- "9b76a7b6-3eef-4864-b2da-1834417e305c"          # Comma delimited
characteristics_url <- "9a7d408b-dd72-4959-ae7d-c854ec505354" # Comma delimited

#2017 Communes were sourced from INSEE (Institut National de la Statistique et des Etudes Economiques) and the population, region and department information joined together and the resulting file stored on the github project directory.
#The National Institute of Statistics and Economic Studies (INSEE) collects, produces, analyzes and disseminates information on the French economy and society. 
github_url   <- "https://github.com/iverni/PH125.9x_CYO/blob/master/Data%20files/" 
commune_file <- "communes2017.xlsx?raw=true"

##Cartography
#The maps of France used in the exploratory data analysis section of this project were also sourced from the data.gouv.fr platform. 
#However due to the extremely large file size of the shapefiles the original file was reduced to a significantly smaller size to greatly improve the performance of rendering the graphics.
#The files are also stored in the project github data repository (github_url).
#Shapefiles are used extensively to store spatial information and can be used to plot data on maps (Viswanathanm et al). 
#It is a geospatial vector data format spatially describing data such as points, lines, and polygons, representing for example borders, landmarks, roads, and lakes"
#https://www.data.gouv.fr/en/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/#_
#https://mapshaper.org - Excellent online tool that was used to reduce the shapefile size to 20% enabling map rendering in seconds rather than minutes.
#All four files are necessary when processing the shapefile and enabling data to be linked to the shapefile elements.
shapefile_shp <- "departements-100m.shp?raw=true"
shapefile_dbf <- "departements-100m.dbf?raw=true"
shapefile_prj <- "departements-100m.prj"
shapefile_shx <- "departements-100m.shx?raw=true"

#Function module to check if the file has already been downloaded. 
download_datefile <- function(url_ref, new_filename) {
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
download_datefile(paste0(github_url,commune_file),"communes2017.xlsx")
communes <- read_excel("communes2017.xlsx", sheet = 1)

#Retreive the shapefiles. They will be read during exploratory data analysis
download_datefile(paste0(github_url,shapefile_shp),"departements-100m.shp") #shape format; the feature geometry itself
download_datefile(paste0(github_url,shapefile_dbf),"departements-100m.dbf") #attribute format; columnar attributes for each shape,#dBASE format
download_datefile(paste0(github_url,shapefile_prj),"departements-100m.prj") #projection description of coordinates
download_datefile(paste0(github_url,shapefile_shx),"departements-100m.shx") #shape index format; a positional index of the feature geometry to allow seeking forwards and backwards quickly

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
#Concatenate Department and Commune columns together to support a join of the French state communes. 
#Use substr to only select the first two characters of the department field
characteristics <- characteristics %>% mutate(depcom = paste0(substr(dep, start = 1, stop = 2),com)) 
toc()

#File cleansing ***********************
#Each file must be checked and cleaned before joining the data together to support exploratory data analysis
tic("Initial Data Cleanse")

##Characteristic File
###Summary information
summary(characteristics)
#Calculate all the NAs present in each column/feature and then assess if any action is required.
sapply(characteristics, function(x) sum(length(which(is.na(x)))))

#From the analysis of the NAs only the atm (atmospheric conditions) and col (collison) features will be corrected.
#The adr (address) field is the postal address filled in for accidents occurring in built up areas. 
#This is not required (but will be checked by performing a NZV)
#The GPS, lat and long co-ordinate information is not required for this project. GIS data will be summarised at a department level
#13 Atmospheric conditions are missing from the dataset
#Replace the atm feature with the median. A histogram analysis of the values shows that the median is "normal" atmospheric conditions.
characteristics %>% ggplot(aes(atm)) + 
  geom_histogram(fill="#1380A1", binwidth = .5) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_x_continuous(breaks = seq(0, 9, 1),
                     limits=c(0, 9)) +
  xlab("Atmospheric Condition") +
  labs(tag="Analysis of Accidents") + 
  labs(title="Accidents by Atmopheric Condition") + 
  bbc_style()

characteristics$atm <- ifelse(is.na(characteristics$atm), median(characteristics$atm, na.rm=TRUE), characteristics$atm)
#Repeating the check for Na values shows the issue has been corrected
sapply(characteristics, function(x) sum(length(which(is.na(x)))))

#The median for the collision type is 6 - Other collision. However the value 6 is chosen as the replacement value because
# of "other collision" is the best category to apply for the missing data, irrespective of the median.
characteristics$col <- ifelse(is.na(characteristics$col), median(characteristics$col, na.rm=TRUE), characteristics$col)
#Latitude and longitude fields need to be formatted correct. 
#The stringi package provides an excellent command that enables characters to be replaced with the open to add another value
#For example, the first two characters of longitude are extracted e.g.
characteristics$long <- stri_sub_replace(characteristics$long, 3,1, omit_na=FALSE, value = ".")
characteristics$lat <- stri_sub_replace(characteristics$lat, 3,1, omit_na=FALSE, value = ".")
#Convert hrmn to hours and minutes
#Format the timestamp 
convert_date_format <- function(year, day, month, hour){
  #Build up the ISO 8601 formatting field
  month_2c <- sprintf("%02d",month)
  day_2c <- sprintf("%02d",day)
  hour_4c <- sprintf("%04d",hour) 
  date_temp <- paste0(year, month_2c, day_2c, " ", hour_4c)
  as_datetime(date_temp, "%y%d%m %H%M", tz="CET")
}
#Group by day or night
#Round the time to the nearest hour in order to group by hour later 
characteristics <- characteristics %>%
    mutate(day_night = ifelse(lum == 1, "day","night"),
           date_cet = as.POSIXct(convert_date_format(an, mois, jour, hrmn)),
           hrs = as.numeric(hour(round_date(date_cet, unit = "1 hour"))))

sapply(characteristics, function(x) sum(length(which(is.na(x)))))

##Places File
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
places$nbv <- as.numeric(ifelse(is.na(places$nbv), 0, places$nbv))
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

##Users File
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

# Now compare NZV of the "places" file. Was there a pattern missed
nzv_places <- nearZeroVar(places, saveMetrics = TRUE)
nzv_places
#Yes. It recommends taking out vosp and infra. Lets not remove them but compare this finding with a PCA
#Incidently, a nzv() check on characteristics identified year and GPS and NZV. Not surprising given year = 2017. This is a good demonstration of NZV
nzv_characteristics <- nearZeroVar(characteristics, saveMetrics = TRUE)
nzv_characteristics

#   trajet - Reason for travelling.  9 indicates "other". All 0 values and NAs should be moved to 9
users$trajet <- ifelse(is.na(users$trajet), 9, users$trajet)
users$trajet <- ifelse(users$trajet == 0, 9, users$trajet)
# There are 37 cases of the age not being recorded. The median value is used.
median(users$an_nais, na.rm=TRUE)
users$an_nais <- ifelse(is.na(users$an_nais), median(users$an_nais, na.rm=TRUE), users$an_nais)

#The severity of accidents is grouped to critical (fatal or serious injury) and normal response (light hospitalisation, no injury)
#The age profile is split into strata.
users <- users %>% mutate(gender = ifelse(sexe == 1, "male","female"),
                          severity = case_when(
                            grav == 1 ~ "normal",   #No injury
                            grav == 2 ~ "critical", #Fatality
                            grav == 3 ~ "critical", #Seriously injured
                            grav == 4 ~ "normal"    #Light injury
                          ),  
                          age = 2017-an_nais, 
                          age_profile = case_when(
                            age <= 15 ~ "child",
                            age > 15 & age <= 19 ~ "teenager",
                            age > 19 & age < 40 ~ "under40",
                            age >= 40 ~ "over40"))

#histogram of year of birth
users %>% ggplot(aes(an_nais)) + 
  geom_histogram(fill="#1380A1", binwidth = .6) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  xlab("Year of Birth") +
  labs(tag="Analysis of Injuries") + 
  labs(title="Year of Birth of involved parties") + 
  geom_vline(aes(xintercept = median(an_nais)),col='black', size = 2) +
  geom_text(data = data.frame(x=median(users$an_nais), y=0), mapping = aes(x, y, label=paste0("Median value:",x)), color="black", hjust =-.1, vjust = 1.5) + 
  bbc_style()

# The following columns are removed due to the large NA occurences but also due to their perceived lack of importance for prediction:
## place - #56% missing
## secu - dataset is not sufficiently clear to use
## locp - Pedestrian Location - remove due to NZV analysis
## actp - Action of the Pedestrian - remove due to NZV analysis
## etapt - Was the pedestrian alone, in a group - removed as other pedestrian fields are removed
users <- users %>% select(-place, -secu, -locp, -actp, -etatp)

##Vehicle File
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
vehicles$choc <- ifelse(is.na(vehicles$choc), 0, vehicles$choc)
vehicles$manv <- as.numeric(ifelse(is.na(vehicles$manv), 00, vehicles$manv)) #Encode the categorical
vehicles$catv <- as.numeric(ifelse(is.na(vehicles$catv), 99, vehicles$catv)) #Encode the categorical
vehicles$obsm <- ifelse(is.na(vehicles$obsm), 9, vehicles$obsm)
vehicles$obsm <- ifelse(vehicles$obsm == 0, 9, vehicles$obsm)

# The following columns are removed due to the large NA occurences but also due to their perceived lack of importance for prediction:
## senc - Direction of the traffic (flow)
## obs - Struck a fixed Obstacle - remove due to NZV analysis
## occutc - Number of occupants in the public transport - remove due to NZV analysis
vehicles <- vehicles %>% select(-obs, -occutc, -senc)

toc()
##Joining data ***********************
tic("Joining data")
#The following section contains the data table joins necessary to support subsequent exploratory data anaylsis and modelling.
#One to one cardinality between the accident characteristics, commune and place files. 
accidents <- characteristics %>% 
  left_join(places, by = 'Num_Acc') 

#Calculate summary data a department and commune level - number of accidents
depcom_summary <- characteristics %>%
  group_by(depcom) %>%
  summarise(accidents = n()) %>%                     #Number of accidents
  left_join(communes, by ="depcom") %>%
  na.omit() %>%                                      #Skip accidents if no valid commune can be found 
  mutate(acc_pp = accidents * 100000 / pop2016)      #Accident rate per 100,000 inhabitants
  
#Count the injury profile (number of people) by department and commune.
depcom_injuries <- accidents %>%
  left_join(users, by = 'Num_Acc') %>%
  left_join(vehicles, by = c("Num_Acc","num_veh")) %>%
  group_by(depcom, grav) %>%
  summarise(n = n()) %>%                               #Count the different injuries by depart/commune
  spread(grav, n) %>%                                  #Spread the injury field to columns
  rename('light' = '1',                                #Rename the columns
         'fatality' = '2',
         'serious' = '3',
         'uninjured' = '4') %>%
  mutate(light, light = ifelse(is.na(light), 0, light), #Replace NAs introduced by the spread with zero value
         fatality, fatality = ifelse(is.na(fatality), 0, fatality),
         serious, serious = ifelse(is.na(serious), 0, serious),
         uninjured, uninjured = ifelse(is.na(uninjured), 0, uninjured))  

#Calculate summary data a department and commune level - number of accidents
depcom_summary <- depcom_summary %>%
  left_join(depcom_injuries, by ="depcom") %>%
  mutate(mortality = fatality * 100000 / pop2016,      #Fatalties per 100,000 inhabitants
         serious_inj_rate = (fatality + serious) / (fatality + serious + light + uninjured))
rm(depcom_injuries)

#Calculate summary data a department level only. This will make EDA much easier later.
dep_summary <- depcom_summary %>%
            group_by(department, depart_name) %>%
            summarise(population = sum(pop2016),
                      accidents  = sum(accidents),
                      light      = sum(light),
                      fatality   = sum(fatality),
                      uninjured  = sum(uninjured),
                      serious    = sum(serious)) %>%
            mutate(mortality = fatality * 100000 / population,
                   serious_inj_rate = (fatality + serious) / (fatality + serious + light + uninjured))

#Join full data together (characteristics, plqce, users and vehicle type to support analysis of accident profiles)
#Format department to a 2 character code so that it may be used for joining with the french department shapefiles
accidents_fulldata <- accidents %>%
  mutate(dep = stri_sub(dep,1,2)) %>%   
  left_join(users, by = 'Num_Acc') %>%
  left_join(vehicles, by = c("Num_Acc","num_veh"))

toc()

#Exploratory Data Analysis ***********************
tic("Exploratory Data Analysis")

sum(communes$pop2016)                                                         #Approx. population of France 2016 by commune
n_distinct(characteristics$Num_Acc)                                           #How many accidents occurred in 2017
total_deaths <- as.numeric(users %>% filter(grav==2) %>% summarise(n = n()))  #How many people where killed
total_deaths
table(users$grav)                                                             #Summary of injury severity.
# 3600 Deaths, 28993 hospitised injuries, 47158 light injuries. 
mortality_avg <- total_deaths * 100000 / sum(communes$pop2016)                #Road Deaths per 100,000 inhabitants
mortality_avg
n_distinct(characteristics$Num_Acc) * 100000 / sum(communes$pop2016)          #Number of accidents per 100,000 inhabitants

##Accident information
#Having identified day or night, plot the histogram for all accidents by day or night. Luminosity is daylight (lum =1)
characteristics %>% ggplot(aes(day_night)) + 
  geom_histogram(fill="#1380A1", stat = "count") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(title="Accidents by Time of Day") + 
  labs(subtitle="Day: (lum = 1 ) Luminosity is daylight") + 
  labs(tag="Analysis of Accidents") + 
  bbc_style()

# Temporal information - month, day, year, time of day
#Display the distribution by the Day of the Week and hour of accidents. Identify the actual week day from the formatting accident time field.
#The temporal analysis of the time of day shozs two peak periods 08:00-09:00 and 17:00-19:00. Not surprisingly common work communiting 
characteristics %>% mutate(day_of_week = wday(date_cet)) %>%
  group_by(mois, day_of_week, hrs) %>%
  summarise(num_accidents = n()) %>%
  ggplot(aes(x=day_of_week, y=hrs, fill=num_accidents)) + 
  geom_tile() +
  scale_fill_continuous(name="#Accidents") +
  xlab("Day of Week  (Sunday = 1)")+
  ylab("Hours (24h format)")+
  labs(title="Temporal Analysis of Accidents",
       subtitle = "Number of Accidents by Hour and Day of the week (Sunday = 1)", 
       tag="Analysis of Accidents") + 
  scale_x_continuous(expand=c(0,0),breaks=1:7) + 
  scale_y_continuous(expand=c(0,0),breaks=1:24) + 
  theme(axis.text.x =element_text(angle = 90, hjust = 1)) 

#Display the distribution by the Day of the Week and hour of accidents. Identify the actual week day from the formatting accident time field.
#The temporal analysis of the time of day shozs two peak periods 08:00-09:00 and 17:00-19:00. Not surprisingly common work communiting 
characteristics %>% mutate(day_of_week = wday(date_cet)) %>%
  group_by(mois, day_of_week) %>%
  summarise(num_accidents = n()) %>%
  ggplot(aes(x=mois, y=day_of_week, fill=num_accidents)) + 
  geom_tile() +
  scale_fill_continuous(name="#Accidents") +
  xlab("Month")+
  ylab("Day of Week (Sunday = 1)")+
  labs(title="Temporal Analysis of Accidents",
       subtitle = "Number of Accidents by Hour and Day of the week",
       tag="Analysis of Accidents") + 
  scale_x_continuous(expand=c(0,0),breaks=1:12) + 
  scale_y_continuous(expand=c(0,0),breaks=1:7) + 
  theme(axis.text.x =element_text(angle = 90, hjust = 1))
  
##Injury information
###List of the Communes with the greatest number of fatalities and discuss
depcom_summary %>% arrange(desc(accidents)) %>% head(20)
#Marseille has the top number of recorded accidents. This is not surprising that the entire city has been been grouped at commune level. 
#The number of accidents at 292 per 100,0000 is far greater than the national average of 91 per 100,0000 and there is also the highest number of fatalities.
#However it is important to assess the mortality rate wich is 4.18, less than the national average of 5.42 for the 2017. Also 18% of the injuries were serious (that is fatal or seriously hospitalised).
#Paris arrondissements feature significantly also and follow the observation of Marseille in that the injury rate and mortality rates are significantly lower than the national average.
#Therefore perhaps it is more interesting to look at high mortality rates where more than 5 accidents (thereby ignoring outliers).
depcom_summary %>% filter(accidents > 5 & mortality > mortality_avg) %>% arrange(desc(accidents)) %>% head(20)
#Here we now see an entirely different list. Top of the list is Gennevilliers with 247 accidents and a mortality rate of 8.57. The serious injury rate is 7.26%.
#In contrast Saint-Pierre in Val-D'Oise had less accidents hozever more fatal (6 deaths/7.13 per 100,000 inhabitants), and a 27% serious injury rate.

###List of the Departments with the greatest number of fatalities and discuss
dep_summary %>% arrange(desc(accidents)) %>% head(20)
#Intuition would have suggested Paris as the department with the top number of accidents. However not so obvious is that the mortality rate is significantly less than the national average.
#Compare the figures to Val-D'Oise were there was 144 fatalaties, with a 31% severe injury rate. This contrasts sharpely with the figures from Paris 
#View by mortality rate
dep_summary %>% arrange(desc(mortality)) %>% head(20)
#When viewing by mortality rate the departments that are towards the top of the list are Alpine boarding departments (e.g. Haute-Saone, Alpes-De-Haute-Provence, and JURA, or Ariege in the French Pyrennes)
##Cartography
shapefile_name <- paste0(getwd(), "/departements-100m.shp")
france_shp <- readOGR(shapefile_name, stringsAsFactors = FALSE)  #Read the shapefile that has been downloaded
france.adm3.shp.df <- broom::tidy(france_shp, region = "code_insee")
france.adm3.shp.df <- france.adm3.shp.df %>% filter(!id %in% c("971","972","973","974","976")) #Exclude French overseas territories
france.adm3.shp.df <- france.adm3.shp.df %>%           
  left_join(dep_summary, by = c("id" = "department"))   #Link the data to the shapefile

#Generate the ggplot and then apply the ggplotly function to make the map interactive for Mortality Rate
gg <- ggplot(france.adm3.shp.df, aes(text = depart_name, label = serious_inj_rate, label2 = fatality, label3 = accidents)) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill= mortality), colour = "grey") + 
  labs(title="French Accident Mortality Rate 2017")
gg
ggplotly(gg) 

#Generate the ggplot and then apply the ggplotly function to make the map interactive for Serious Injury Rate
gg <- ggplot(france.adm3.shp.df, aes(text = depart_name, label = serious_inj_rate, label2 = fatality, label3 = accidents)) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill= serious_inj_rate), colour = "grey") + 
  labs(title="French Accident Serious Injury Rate 2017")
gg
ggplotly(gg) 

##Accident victim profile
#A mosaic plot is a square subdivided into rectangular tiles the area of which represents the conditional relative frequency for a cell in the contingency table. 
#Each tile is colored to show the deviation from the expected frequency 
#You can use the mosaic plot to discover the association between two variables. 
#Red tiles indicate significant negative residuals, where the frequency is less than expected. 
#Blue tiles indicate significant positive residuals, where the frequency is greater than expected.
#The colors represent the level of the residual for that cell / combination of levels.
#More specifically, blue means there are more observations in that cell than would be expected under the null model (independence)
#The intensity of the color represents the magnitude of the residual.
mosaic(~severity + age_profile + sexe, data=accidents_fulldata, 
       shade=TRUE, legend=TRUE)

assoc(~severity + age_profile  + sexe, data=accidents_fulldata, 
      shade=TRUE, legend=TRUE)

#Child Drivers
child_drivers <- accidents_fulldata %>% filter(age_profile == "child" & catu == 1) 
#The following mosiac shows that the main vehicle category where children are injured is category 2 - Cyclomoteur < 50cm3.
mosaic(~age_profile + severity + catv, data=child_drivers, shade=TRUE, legend=TRUE)
#List the Category of vehicles that children drove and where injured. Top of the list are 01 - Bicyclette and 02 - Cyclomoteur < 50cm3 
#Some other vehciles of interest:
# - 20 Special engine
# - 21 is an agricultural tractor
# - 30 Scooter < 50 cm3
# - 31 Motocyclette > 50 cm3 and <= 125cm3
# - 35/36 Quad bikes <50cm3 and >50cm3 respectively

#Graph of vechicle category where children were driving and fatality injured
accidents_fulldata %>%
  filter(age_profile =="child" & catu == 1, grav == 2 & !catv == 99) %>%
  group_by(catv, grav) %>%
  summarise(total = n()) %>%
  ggplot(aes(as.factor(catv), total)) +
  geom_point() + 
  labs(title="Child Driver Fatalities") + 
  xlab("Vehicle Category") +
  ylab("Number of fatalities") +
  theme(axis.text.x =element_text(angle = 90, hjust = 1)) 

#Department with the greatest injury and mortality rates for children
dep_child <- accidents_fulldata %>%
  left_join(dep_summary, by = c("dep" = "department")) %>%
  filter(age_profile =="child" & grav == 2) %>%
  group_by(dep, depart_name, grav) %>%
  summarise(total = n())

shapefile_name <- paste0(getwd(), "/departements-100m.shp")
france_shp <- readOGR(shapefile_name, stringsAsFactors = FALSE)  #Read the shapefile that has been downloaded
france.childen.shp.df <- broom::tidy(france_shp, region = "code_insee")
france.childen.shp.df <- france.childen.shp.df %>% filter(!id %in% c("971","972","973","974","976")) #Exclude French overseas territories
france.childen.df <- france.childen.shp.df %>%           
  left_join(dep_child, by = c("id" = "dep"))   #Link the data to the shapefile

#Generate the ggplot and then apply the ggplotly function to make the map interactive.
#Immediately the departments of Pas-De-Calais and the Somme are highlighted where several children have tragically lost their lives. 
#In addition to the obsevation of the vehicle classification and location in this area there is certainly justification for further study of the 
#issue in this area and the possibility of targeted educational training or regionally state intervention.
gg3 <- ggplot(france.childen.df, aes(text = depart_name)) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill= total), colour = "grey") + 
  labs(title="French Child Accident Mortalities 2017")
gg3
ggplotly(gg3) 

##Vehicle information
#Analysis shows that the main vechile category is 7 = regular car. Therefore this is removed to assess the impact of other vechile types. 
#The generic classification 99 "other vehicle" is also excluded. 
#The visualisation shozws that 33 Motorbikes > 125cm3 are the next category with the most fatalities (grav=2). 
#For serious injury with hospitalisation (grav =3) the standout category again is 3 Motorbikes > 125cm3. However there is greater variation
accidents_fulldata %>% filter(!catv == 99 & !catv == 7 ) %>%
  group_by(catv, grav) %>%
  summarise(n = n()) %>%
  ggplot(aes(as.factor(catv), n)) +
  geom_point() + 
  facet_grid(~grav) +
  labs(title="Accident Severity by Vehicle Type 2017",
       subtitle = "Excluding cars which are the primary accident vehicle") +
  xlab("Vehicle Category") +
  ylab("Number of Vehicles") 

##Model 1: GLM with stepwise feature determination - ***********************
tic("Model 1: GLM with stepwise feature determination")

accidents_to_model <- accidents_fulldata %>% 
  select(severity, mois, jour, day_night, age_profile, agg, atm, catr, catv, sexe)
accidents_to_model$severity <- as.numeric(accidents_to_model$severity == "normal")
sapply(accidents_to_model, function(x) sum(length(which(is.na(x)))))
#Predictors are added step by step until no new predictors add any substantial value to the model
#It is not guaranteed to find the best possible model. The stepwise regression procedure violates some statistical assumptions
#It can result in a model that makes little sense in the real world. It would not always be clear from an emergency call what are the circumstances
set.seed(1975)
indexes <- createDataPartition(accidents_to_model$severity, times = 1, p = 0.8, list = FALSE)
accTrain <- accidents_to_model[indexes, ]
accTest <- accidents_to_model[-indexes, ]

#Set the initial and full models. 
null_model <- glm(severity ~ 1, data = accTrain, family = "binomial")
full_model <- glm(severity ~ ., data = accTrain, family = "binomial")
#Step forward from the initial model to the complete model 
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
step_prob <- predict(step_model, type = "response")
#Plot the receiver operating characteristic (ROC)  
#The ROC curve plots sensitivity (TPR) versus 1 - specificity or the false positive rate (FPR).
#We can see that we obtain higher sensitivity with this approach for all values of specificity, which implies it is in fact a better method than guessing.
ROC <- roc(accTrain$severity, step_prob)
plot(ROC, col = "red")
#Area under the curve
auc(ROC)

#Fit best model identified during the stepwise feature determination
fit <- glm(severity ~ agg + catv + age_profile + catr + day_night + sexe + atm, data = accTrain, family = "binomial")
summary(fit)
#Predict the results of each iteration. Predict takes a fitted object from the glm() function and fits the model against the test set.
accTest$sev_prediction <- predict(fit, newdata = accTest, type = "response")
#Estimate the mean (to be used to compare with the fitted results)
mean(accidents_to_model$severity)

# Predict the severity if probability of severity is greater than average
accTest$severity_predicted <- ifelse(accTest$sev_prediction  > mean(accidents_to_model$severity), 1, 0)
mean(accTest$severity_predicted, na.rm = TRUE)

# Use a confusion matrix to tabulate each combination of prediction and actual value.
confmatrix <- confusionMatrix(as.factor(accTest$severity_predicted), as.factor(accTest$severity))
confmatrix
model_results <- tibble(method = "GLM", 
                        model_accuracy = confmatrix$overall['Accuracy'], 
                        model_sens = confmatrix$byClass['Sensitivity'],
                        model_spec = confmatrix$byClass['Specificity'],
                        model_F1 = confmatrix$byClass['F1'],
                        model_ppv = confmatrix$byClass['Pos Pred Value'],
                        model_npv = confmatrix$byClass['Neg Pred Value'])

#The model enables a prediction of the severity based on the features. Department and Commune have been removed to simplify the illustration
#A phone call is received to the emergency service who then ask a series of questions
enter_prediction <- data.frame(day_night = "day",        # Day or night
                               age_profile = "over40",   # Drivers age, if known
                               agg = 2,                  # Built up area (2) or countryside (1)?
                               atm = 1,                  # Weather conditions
                               catr = 3,                 # Highway 1, national route 2, department road 3?
                               catv = 7,                 # Type of vehicle
                               sexe = 2)                 # Sex of the driver 1 - Male 2 - Female

result <- predict(fit, newdata = enter_prediction, type = "response")
ifelse((1 - result) > mean(accidents_to_model$severity), "Critical Alert", "Normal Responders") 
predict(fit, newdata = enter_prediction, type = "terms") 

toc()
##Model 2: PCA ***********************
##PCA - looking for hidden features
#If you are looking at hidden features and want to represent the data in a low dimensional space, PCA is the method one should choose. 
#If you are looking at building a good classification or prediction model, then it is not a good idea to perform PCA first; 
#you should ideally include all the features and remove the redundant ones by any parametric method, such as forward or backward
#PCA is a multivariate statistical data analysis technique applicable for datasets with numeric variables only, 
#which computes linear combinations of the original variables and those principal components are orthogonal to each other in the feature space. 
#PCA is a method that uses eigen values and eigen vectors to compute the principal components, which is a linear combination of original variables. 
#The general rule of thumb is 80-20, if 80% variation in the data can be explained by 20% of the principal components.
#The results show that 15 components would be required to show the variance. Therefore PCA will not be used.

# Perform a PCA without having performed this manual assessment of every field 
# Isolate user observations with pedestrian information and assess separately. Only complete records. Does it matter
accidents_temp <- accidents_fulldata
accidents_temp$severity <- as.numeric(accidents_temp$severity == "normal")
accidents_temp$day_night <- as.numeric(accidents_temp$day_night == "night")
accidents_temp$dep <- as.numeric(accidents_temp$dep)
accidents_temp$com <- as.numeric(accidents_temp$com)

accidents_temp <- accidents_temp %>% 
  mutate(age_profile = case_when(
    age_profile == "child" ~ 0,
    age_profile == "teenager" ~ 1,
    age_profile == "under40" ~ 2,
    age_profile == "over40" ~ 4))  

#A key issue with PCA is categorical variables. The dataset contains encoded categories. This section attempts to use the encoded
#values to check if PCA can be applied. It is much more suitable for numerical values. 
#use selectif() to only select the numeric features
#Select all numeric fields from the full data set. Do not select the fields identified during NZV analysis
accidents_to_model <- accidents_temp %>% select_if(is.numeric) %>%
                      select(-an, -vosp, -catu, -grav)  #Driver is NZV due to the earlier select statement

#Check there are no missing values as this will prevent PCA from being executed
sapply(accidents_to_model, function(x) sum(length(which(is.na(x)))))
rm(accidents_temp)
#Perform a NZV on the dataset to identify if there are any redundant fields
nearZeroVar(accidents_to_model, saveMetrics = TRUE)

#There are two methods in R, prcomp() and princomp(), but they use two slightly different methods; 
#princomp() uses eigen vectors and prcomp() uses SVD method. Some researchers favor prcomp() as a method over princomp() method.
#SVD is selected for this projet
pca_svd <- prcomp(accidents_to_model, scale = TRUE, center = TRUE)
summary(pca_svd)

#Scree plot 
#The second common plot type for understanding PCA models is a scree plot. 
#A scree plot shows the variance explained as the number of principal components increases. 
#Sometimes the cumulative variance explained is plotted as well.
#The standard deviation of the principal components is available in the sdev component of the PCA model object. 
#Therefore the variance can be calculated by squaring this value
pca_var <- pca_svd$sdev^2
pve <- pca_var / sum(pca_var)
# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

##Model 3: RPart - Severity Prediction ***********************
# Build a Accident Severity model predicting accident severity outcome versus the predictors/features
# As this is a project submission a sample of the dataset is taken to ensure the model can be run on difference computer specifications
tic("Model 3: RPart")
#Use the same features that were applied in model 1
set.seed(1976)

#The following function creates entries in a tibble that contains all the results of each model assessed. It will be printed in the results section of this report.
#Calculate the average rating for each movie and calculate the bias (difference) for each
add_results <- function(n_accuracy, n_sens, n_spec, n_F1, n_ppv, n_npv, model_method){
  bind_rows(model_results,
            tibble(method=model_method,  
                   model_accuracy = n_accuracy,
                   model_sens = n_sens,
                   model_spec = n_spec,
                   model_F1 = n_F1,
                   model_ppv = n_ppv,
                   model_npv = n_npv))
}

accidents_to_model <- accidents_fulldata %>% 
  select(severity, agg, catv, age_profile, catr, day_night, sexe, atm)

#Create the training and test sets
indexes <- createDataPartition(accidents_to_model$severity, times = 1, p = 0.8, list = FALSE)
accTrain <- accidents_to_model[indexes, ]
accTest <- accidents_to_model[-indexes, ]
#Fit the model using rpart() 
modelFit <- rpart(as.factor(severity) ~ .,data=accTrain, method = "class", control = rpart.control(cp = 0))
rpart_prediction <- predict(modelFit, accTest, type = "class")
# Use a confusion matrix to tabulate each combination of prediction and actual value.
confmatrix2 <- confusionMatrix(rpart_prediction, as.factor(accTest$severity))
confmatrix2
model_results <- add_results(confmatrix2$overall['Accuracy'], 
                             confmatrix2$byClass['Sensitivity'],
                             confmatrix2$byClass['Specificity'],
                             confmatrix2$byClass['F1'],
                             confmatrix2$byClass['Pos Pred Value'],
                             confmatrix2$byClass['Neg Pred Value'],
                             "Rpart")

#Model a phone call received to the emergency service who then ask a series of questions
enter_rpart_pred <- data.frame(day_night = "day",        # Day or night
                               age_profile = "under40",  # Drivers age profile
                               agg = 1,                  # Built up area (2) or countryside (1)?
                               atm = 1,                  # Weather conditions
                               catr = 3,                 # Highway 1, national route 2, department road 3?
                               catv = 7,                 # Type of vehicle
                               sexe = 1)                 # Sex of the driver 1 - Male 2 - Female

result <- predict(modelFit, enter_rpart_pred, type = "class")
result
plotcp(modelFit)
rpart.plot(prune(modelFit, cp = 0.00045), type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

toc()