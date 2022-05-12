# Analytics Web App for Big Data: How Does GDP and Social Inequality Influence Netflix's Growth?

# Configuring the working directory

setwd("C:/Users/andre/Documents/FCD/BigDataRAzure/Curso Atualizado/Cap04")
getwd()

########## Data Load and Cleanup Script ##########

# load the packages
library(dplyr)
library(tidyr)
library(readxl)
library(readr)

##### Data Load ##### 

# Loading Netflix data
dados_netflix <- read.csv("datasets_originais/dados_netflix_Dec_2021.csv")
View(dados_netflix)

# Loading the World Bank data
dados_pib <- read.csv("datasets_originais/dados_world_bank.csv", header = FALSE)
View(dados_pib)

# Loading salary inequality data
dados_salario <- read.csv("datasets_originais/dados_desigualdade_salarial_harvard.csv")
View(dados_salario)

# Loading data from IMDB:
dados_IMDB <- read_tsv("datasets_originais/dados_imdb.tsv")
View(dados_IMDB)

# Loading data on Netflix's Top 10 shows by country
dados_top10 <- read_excel("datasets_originais/top_10_shows_netflix.xlsx")
View(dados_top10)

# Loading Netflix subscriber data in July/2021
dados_sub <- read.csv("datasets_originais/assinantes_netflix_jul_2021.csv")
View(dados_sub)

# Loading ISO country code data
countrycode <- read.csv("datasets_originais/wikipedia-iso-country-codes.csv")
View(countrycode)

##### Cleaning and Preparing the First Combined Dataset ##### 

# Create a column with the data difference for the bar chart (standard plan - basic plan)
dados_netflix$basic_standard_diff = (dados_netflix$Cost.Per.Month...Standard.... - dados_netflix$Cost.Per.Month...Basic....)

# Create a column with the difference data for the bar chart (premium plan - standard plan)
dados_netflix$standard_premium_diff = (dados_netflix$Cost.Per.Month...Premium.... - dados_netflix$Cost.Per.Month...Standard....)

# Combine previous data with GDP data
names(dados_pib)[names(dados_pib) == 'V1'] <- 'Country'
dados_netflix_pib <- merge(dados_netflix, dados_pib, by = "Country")

# Extract the 2020 GDP
dados_netflix_pib2020 <- dados_netflix_pib[-c(11:72, 74, 75)] 
names(dados_netflix_pib2020)[names(dados_netflix_pib2020) == 'V64'] <- "2020 GDP (World Bank)"

# Clean up the salary inequality dataframe
dados_salario <- dados_salario[, c(1:3)]
dados_salario_ano <- dados_salario %>% group_by(country) %>% summarise(max = max(year, na.rm = TRUE))

# Combine dataframes
dados_salario <- merge(dados_salario, dados_salario_ano, by.x = c("country", "year"), by.y = c("country", "max"))
dados_netflix_pib_salario2020 <- merge(dados_netflix_pib2020, dados_salario, by.x=c("Country"), by.y=c("country"))

# Clear the billing and subscription dataset and match the previous dataframe
dados_sub <- dados_sub[,c(1, 23,24)]
complete <- merge(dados_netflix_pib_salario2020, dados_sub, by=c("Country"))

# Merge countrycode to choropleth map
countrycode <- countrycode[,c(1, 3)]
complete <- merge(complete, countrycode, by.x=c("Country"), by.y=c("English.short.name.lower.case"))
View(complete)

# Save the dataframe produced so far
write.csv(complete, "datasets_limpos/dataset1.csv", row.names = FALSE)

##### Cleaning and Preparing the Second Combined Dataset ##### 

# Clean and filter the IMDB dataframe
genero <- dados_IMDB[,-c(1, 4:8)]
View(genero)
names(genero)[names(genero) == 'primaryTitle'] <- 'show_title'
View(genero)

# Associate the genre with the Top 10 shows
topgenero <- merge(dados_top10, genero, by = "show_title")
View(topgenero)

# Clear previous dataframe to keep only 1 entry for each top 10
topgenero <- topgenero[(topgenero$category == "Films" & topgenero$titleType == "movie") | (topgenero$category == "TV" & topgenero$titleType == "tvSeries"), ] 
topgenero <- distinct(topgenero, show_title, week, country_name, category, titleType,cumulative_weeks_in_top_10, .keep_all= TRUE)
View(topgenero)

# Keep only movie genre information by country
topgeneropaises <- topgenero[,-c(1, 3:9)]
View(topgeneropaises)

# dataframe pivot
topgeneropaises <- separate(topgeneropaises, c("genres") , c("genero1", "genero2", "genero3"), sep = ",")
topgeneropaises <- pivot_longer(topgeneropaises, c("genero1", "genero2", "genero3"), names_to = "genero123", values_to = "genres")
View(topgeneropaises)

# Counts the number of genres
generocount <- count(topgeneropaises, country_name, genres)
generocount <- na.omit(generocount)
generocount <-subset(generocount, genres!="\\N")
generocount$n <- as.numeric(generocount$n)
View(generocount)

# save to disk
write.csv(generocount, "datasets_limpos/dataset2.csv", row.names = FALSE)

##### Cleaning and Preparing the Third Combined Dataset ##### 

# Renames the previous dataframe
sunburst <- rename(generocount, label = country_name)

# remove the dashes
sunburst$genres = sub("-", " ", sunburst$genres)

# set the name
sunburst$parent = c("total  - ")
sunburst$parent <- paste(sunburst$parent, sunburst$genres)
sunburst$id = c(" - ")
sunburst$id <- paste(sunburst$parent, sunburst$id)
sunburst$id <- paste(sunburst$id, sunburst$label)
sunburst$n <- as.numeric(sunburst$n)
View(sunburst)

# Aggregation
added <- aggregate(sunburst$n, list(sunburst$genres), FUN=sum)
added <- rename(added, label = Group.1)
added <- rename(added, n = x)
added$n <- as.numeric(added$n)
added$genres <- c(NA)
added$parent <- c("total")
added$id <- c(" - ")
added$id <- paste(added$parent, added$id)
added$id <- paste(added$id, added$label)
View(added)

# calculate sum
total = sum(added$n)
total

# Combine everything to the final dataframe
sunburst <- rbind(added, sunburst)
sunburst <- rbind(c("total", total, NA, NA, "total"), sunburst)
sunburst <- sunburst[,-c(3)]
sunburst$n <- as.numeric(sunburst$n)
View(sunburst)

# save to disk
write.csv(sunburst, "datasets_limpos/dataset3.csv", row.names = FALSE)

##### Cleaning and Preparation of the Combined Dataset Room ##### 

# Let's work with top 10 to avoid graphics performance issues
top10sunburst <- sunburst[-c(1:28),]
top10sunburst$n <- as.numeric(top10sunburst$n)
View(top10sunburst)

# Top 10 genres by country
top10sunburst <- top10sunburst %>% 
  group_by(label) %>%
  top_n(10,n)
View(top10sunburst)

# Recalculates the totals, adjusts and combines the dataframe
top10add <- aggregate(top10sunburst$n, list(top10sunburst$parent), FUN = sum)
top10add <- rename(top10add, id = Group.1)
top10add <- rename(top10add, n = x)
top10add$label = sub("total  -  ", "", top10add$id)
top10add$parent = c("total")
top10add$n <- as.numeric(top10add$n)
total = sum(top10add$n)
top10sunburst <- rbind(top10add, top10sunburst)
top10sunburst <- rbind(c("total", total, NA, NA, "total"), top10sunburst)
top10sunburst$n <- as.numeric(top10sunburst$n)
View(top10sunburst)

# save to disk
write.csv(top10sunburst, "datasets_limpos/dataset4.csv", row.names = FALSE)

##### Cleaning and Preparation of the Fifth Combined Dataset ##### 

# Filters the previous dataframe and creates a new one
nototal <- sunburst[-c(1),]
nototal$parent = sub("total  -  ", "", nototal$parent)
nototal$parent = sub("total", NA, nototal$parent)
nototal$id = sub("total  -  ", "", nototal$id)
View(nototal)

# save to disk
write.csv(nototal, "datasets_limpos/dataset5.csv", row.names = FALSE)

##### Cleaning and Preparing the Sixth Combined Dataset ##### 

# Filters the previous dataframe and creates a new one
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
View(countrytree)

# save to disk
write.csv(countrytree, "datasets_limpos/dataset6.csv", row.names = FALSE)



