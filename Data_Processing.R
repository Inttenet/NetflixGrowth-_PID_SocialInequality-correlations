#Como pib e desigualdade social influenciam no crescimento da netflix?

install.packages("dplyr")
install.packages("tidyr")
install.packages("readxl")
install.packages("readr")

library(dplyr)
library(tidyr)
library(readxl)
library(readr)

#Loading netflix data:
dados_netflix <- read.csv("datasets_originais/dados_netflix_Dec_2021.csv")
View(dados_netflix)

#Loading World bank data:
dados_pib <- read.csv("datasets_originais/dados_world_bank.csv", header = F)
View(dados_pib)

#Loading social inequality data:
dados_salario <- read.csv("datasets_originais/dados_desigualdade_salarial_harvard.csv")
View(dados_salario)