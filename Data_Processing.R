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

#Maintain only movie gender information per country:
top_gender_country_data <- topgender[, -c(1, 3:9)]

#Dataframe pivot:
top_gender_country_data <- separate(top_gender_country_data,c("genres"), c("genero1","genero2","genero3"), sep=",")
top_gender_country_data <- pivot_longer(top_gender_country_data, c("genero1","genero2","genero3"), names_to = "genero123", values_to = "genres")

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
sunburst$parent = c("total - ")
sunburst$parent <- paste(sunburst$parent, sunburst$genres)
sunburst$id = c(" - ")
sunburst$id <- paste(sunburst$parent, sunburst$id)
sunburst$id <- paste(sunburst$id, sunburst$label)
sunburst$n <- as.numeric(sunburst$n)

#Agregation 
added <- aggregate(sunburst$n, list(sunburst$genres), FUN= sum)
added <- rename(added, label = Group.1)
added <- rename(added, n = x)
added$n <-as.numeric(added$n)
added$genres <- c(NA)
added$parent <- c("total")
added$id <- c(" - ")
added$id <- paste(added$parent, added$id, added$label)

#calculating total:
total =sum(added$n)
total

#Matching everything for final dataframe:
sunburst <- rbind(added, sunburst)
sunburst <- rbind(c("total", total, NA, NA, "total"), sunburst)
sunburst <- sunburst[, -c(3)]
sunburst$n <- as.numeric(sunburst$n)

#Saving dataframe
write.csv(sunburst, "datasets_finais/dataset3.csv", row.names = FALSE)



### Cleaning and preparation of the fourth combined dataset ###
#Working with top 10 avoiding future performance problems of the graphics
top10sunburst <- sunburst[-c(1:28),]
top10sunburst$n <- as.numeric(top10sunburst$n)

#Top 10 genrer per country
top10sunburst <- top10sunburst %>% group_by(label) %>% top_n(10,n)
View(top10sunburst)

#Recalculating totals, adjusting and matching dataframe
top10add <- aggregate(top10sunburst$n, list(top10sunburst$parent), FUN=sum)
top10add <- rename(top10add, id = Group.1)
top10add <- rename(top10add, n=x)
top10add$label = sub("total - ", "", top10add$id)
top10add$parent = c("total")
top10add$n <- as.numeric(top10add$n)
total = sum(top10add$n)
top10sunburst <- rbind(top10add, top10sunburst)
top10sunburst <- rbind(c("total",total,NA,NA,"total"), top10sunburst)
top10sunburst$n <- as.numeric(top10sunburst$n)
View(top10sunburst)

#Saving dataframe
write.csv(top10sunburst,"datasets_finais/dataset4.csv",row.names = FALSE)



### Cleaning and preparation of fifth dataset ###
#Filtering previous dataset and creating another one
nototal <- sunburst[-c(1),]
nototal$parent = sub("total - ", "", nototal$parent)
nototal$parent = sub("total", NA, nototal$parent)
nototal$id = sub("total - ","",nototal$id)

#Saving dataframe
write.csv(top10sunburst,"datasets_finais/dataset5.csv",row.names = FALSE)



### Cleaning and preparation of sixth dataset ###
#Filtering previous dataset and creating another one
countrytree <- nototal[-c(1:28),]
countrytree <- rename(countrytree, parents = label)
countrytree <- rename(countrytree, labels = parent)
countrytree$id = c(" - ")
countrytree$id <- paste(countrytree$parent, countrytree$id)
countrytree$id <- paste(countrytree$id, countrytree$label)
countries <- aggregate(countrytree$n, list(countrytree$parents), FUN = sum)
countries <- rename(countries, labels = Group.1)
countries <- rename(countries, n = x)
countries$n <- as.numeric(countries$n)
countries$id <- countries$label
countries$parents <- c(NA)
countrytree <- rbind(countrytree, countries)

#Saving dataframe
write.csv(top10sunburst,"datasets_finais/dataset6.csv",row.names = FALSE)
