#Data Processing stage
install.packages("dplyr")
install.packages("tidyr")
install.packages("readxl")
install.packages("readr")

library(dplyr)
library(tidyr)
library(readxl)
library(readr)

### LOADING DATA ###
#Loading netflix data:
netflix_data <- read.csv("datasets_originais/dados_netflix_Dec_2021.csv")
View(netflix_data)

#Loading World bank data:
GDP_data <- read.csv("datasets_originais/dados_world_bank.csv", header = F)
View(GDP_data)

#Loading social inequality data:
salary_data <- read.csv("datasets_originais/dados_desigualdade_salarial_harvard.csv")
View(salary_data)

#Loading IMDB data:
IMDB_data <- read_tsv("datasets_originais/dados_imdb.tsv")
View(IMDB_data)

#Top 10 shows on NETFLIX per country
TOP10_data <- read_excel("datasets_originais/top_10_shows_netflix.xlsx")
View(TOP10_data)

#Netflix subscribers data from July/2021
sub_data <- read.csv("datasets_originais/assinantes_netflix_jul_2021.csv")
View(sub_data)

#ISO code from each country 
ISOCODE_data <- read.csv("datasets_originais/wikipedia-iso-country-codes.csv")
View(ISOCODE_data)


### DATASET CLEANING AND PREPARATION ### 
#Columns creating related to the difference between packages (standart - basic)
netflix_data$standard_basic_diff <- (netflix_data$Cost.Per.Month...Standard.... - netflix_data$Cost.Per.Month...Basic....)

#Columns creating related to the difference between packages (premium - standard)
netflix_data$premium_standard_diff <- (netflix_data$Cost.Per.Month...Premium.... - netflix_data$Cost.Per.Month...Standard....)

#Matching GDP and NETDLIX data
names(GDP_data)[names(GDP_data)== 'V1'] <- "Country"
netflix_GDP_data <- merge(netflix_data, GDP_data, by = "Country")

names(netflix_GDP_data)

#Extracting 2020 GDP
netflix_GDP_2020_data <- netflix_GDP_data[-c(11:72,74,75)]
names(netflix_GDP_2020_data)
names(netflix_GDP_2020_data)[names(netflix_GDP_2020_data)=='V64'] <- "2020 GDP (World Bank)"
View(netflix_GDP_2020_data)

#Social inequality dataframe cleaning ()
salary_data <- salary_data[, c(1:3)]
anual_salary_data <- salary_data %>% group_by(country) %>% summarise(max = max(year, ra.rm = T))
View(anual_salary_data)

#Matching dataframes:
