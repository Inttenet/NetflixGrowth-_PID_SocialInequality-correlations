#Data Processing stage
install.packages("dplyr")
install.packages("tidyr")
install.packages("readxl")
install.packages("readr")

library(dplyr)
library(tidyr)
library(readxl)
library(readr)

getwd()

### LOADING DATA ###
#Loading netflix data:
netflix_data <- read.csv("datasets_originais/dados_netflix_Dec_2021.csv")

#Loading World bank data:
GDP_data <- read.csv("datasets_originais/dados_world_bank.csv", header = F)

#Loading social inequality data:
salary_data <- read.csv("datasets_originais/dados_desigualdade_salarial_harvard.csv")

#Loading IMDB data:
IMDB_data <- read_tsv("datasets_originais/dados_imdb.tsv")

#Top 10 shows on NETFLIX per country
TOP10_data <- read_excel("datasets_originais/top_10_shows_netflix.xlsx")

#Netflix subscribers data from July/2021
sub_data <- read.csv("datasets_originais/assinantes_netflix_jul_2021.csv")

#ISO code from each country 
ISOCODE_data <- read.csv("datasets_originais/wikipedia-iso-country-codes.csv")

### DATASET CLEANING AND PREPARATION ### 
#Columns creating related to the difference between packages (standart - basic)
netflix_data$standard_basic_diff <- (netflix_data$Cost.Per.Month...Standard.... - netflix_data$Cost.Per.Month...Basic....)

#Columns creating related to the difference between packages (premium - standard)
netflix_data$premium_standard_diff <- (netflix_data$Cost.Per.Month...Premium.... - netflix_data$Cost.Per.Month...Standard....)

#Matching GDP and NETDLIX data
names(GDP_data)[names(GDP_data)== 'V1'] <- "Country"
netflix_GDP_data <- merge(netflix_data, GDP_data, by = "Country")

#Extracting 2020 GDP
netflix_GDP_2020_data <- netflix_GDP_data[-c(11:72,74,75)]
names(netflix_GDP_2020_data)[names(netflix_GDP_2020_data)=='V64'] <- "2020 GDP (World Bank)"

#Social inequality dataframe cleaning ()
salary_data <- salary_data[, c(1:3)]
anual_salary_data <- salary_data %>% group_by(country) %>% summarise(max = max(year, ra.rm = T))

#Matching dataframes:
salary_data <- merge(salary_data, anual_salary_data, by.x = c("country","year"), by.y = c("country", "max"))
netflix_GDP_salary2020_data <- merge(netflix_GDP_2020_data, salary_data, by.x=c("Country"), by.y=c("country"))

#Cleaning subscription dataset and merging with previous dataframe
sub_data <- sub_data[,c(1, 23, 24)]
complete <- merge(netflix_GDP_salary2020_data, sub_data, by=c("Country"))

#Matching country code with dataframe
ISOCODE_data <- ISOCODE_data[, c(1,3)]
complete <- merge(complete, ISOCODE_data, by.x=c("Country"), by.y = c("English.short.name.lower.case"))

#Saving "complete" dataframe
write.csv(complete, "datasets_finais/dataset1.csv")

### Cleaning and filtering "complete" dataframe ###
gender <- IMDB_data[, -c(1,4:8)]
names(gender)[names(gender)=="primaryTitle"] <- "show_title" 

#Associating gender with top 10 shows
topgender <- merge(TOP10_data, gender, by="show_title")

#Cleaninng previous dataframe in order to maintain one input for each top10:
topgender <- topgender[(topgender$category =="Films" & topgender$titleType=='movie') | (topgender$category == "TV" & topgender$titleType=='tvSeries'), ]
topgender <- distinct(topgender, show_title, week, country_name, category, titleType, cumulative_weeks_in_top_10, .keep_all = TRUE)
View(topgender)

#Maintain only movie gender information per country:
top_gender_country_data <- topgender[, -c(1, 3:9)]
View(top_gender_country_data)

#Dataframe pivot:
top_gender_country_data <- separate(top_gender_country_data,c("genres"), c("genero1","genero2","genero3"), sep=",")
top_gender_country_data <- pivot_longer(top_gender_country_data, c("genero1","genero2","genero3"), names_to = "genero123", values_to = "genres")
View(top_gender_country_data)

#Counting genres types:
genrercount <- count(top_gender_country_data, country_name, genres)
genrercount <- na.omit(genrercount)
genrercount <- subset(genrercount, genres!="\\N")

#Saving dataframe
write.csv(genrercount, "datasets_finais/dataset2.csv", row.names = FALSE)


### Cleaning and preparation for third combined dataset ###
#Renaming previous dataframe:
sunburst <- rename(genrercount, label=country_name)

#Removing hyphen:
sunburst$genres = sub("-", " ", sunburst$genres)

#Adjusting name:
sunburst$parent = c("total  - ")
sunburst$parent <- paste(sunburst$parent, sunburst$genres)
sunburst$id = c(" - ")
sunburst$id <- paste(sunburst$parent, sunburst$id)
sunburst$id <- paste(sunburst$id, sunburst$label)
sunburst$n <- as.numeric(sunburst$n)
