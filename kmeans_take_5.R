#Libraries needed----
install.packages('cluster', 'magrittrr')
install.packages('factoextra', dependencies = TRUE)
library("cluster")
library("factoextra")
library("magrittr")
library(purrr)
library(dplyr)
library(tidyr)

  ##Load Datasets and build necessary subsets----
  #Load Given Data
  data_1 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/4-DMEF3YrBase.csv", colClasses=c('integer','factor'))
  data_2 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/DMEFLines3Dataset2.csv", as.is=T)
  
  #Load American Fact Finder Data
  median_income = read_excel("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/MedianZIP-3.xlsx")
  median_income$Zip = as.factor(median_income$Zip)
  
  #Women's Apparel
  rawData = subset(data_2, (data_2$MajorCategory==66))
  
  #Income of Household (proxied by median income of zipcode)
  HH = unique(rawData$HH_ID)
  data_1 = data_1[(data_1$HH_ID %in% HH),]
  HH_income = left_join(data_1, median_income, by=c("ZIPCode" = "Zip"))
  
  ##Building Features----
  #Price Over Quantity (Price Point)
  rawData$P_Q = with(rawData, rawData$Dollars/rawData$Quantity)
  
  #Month of Order
  rawData$OrderDate = as.Date(as.character(rawData$OrderDate),"%Y%m%d", origin = "1960-10-01")
  rawData$OrderMonth = months(as.POSIXlt(rawData$OrderDate, format="%Y%m%d"))
  rawData$OrderMonth = as.factor(rawData$OrderMonth)
  
  #Channel
  rawData$Channel = as.factor(rawData$Channel)
  
  #Testing for company 732 
  #Count number of orders by month 
  temp = rawData[(rawData$CompanyID==732),]
  month_count = temp %>%
    gather(OrderMonth, Val, OrderMonth) %>% 
    group_by(HH_ID, Val) %>% 
    summarise(n= n()) %>%
    ungroup() %>%
    spread(Val, n, fill=0)
  
  #Count channel they order through
  channel_count = temp %>%
    gather(Channel, Val, Channel) %>% 
    group_by(HH_ID, Val) %>% 
    summarise(n= n()) %>%
    ungroup() %>%
    spread(Val, n, fill=0)
  
  #Total Spent and average Price Paid
  total_spent_company = temp %>%
    group_by(HH_ID) %>%
    summarise(DollarsCompany = sum(Dollars), QuantityCompany = sum(Quantity), P_QCompany = mean(P_Q))
  
  #Total Spent, bought and Average Price Paid in Category
  HH_in_732_and_MC66 = rawData[(rawData$HH_ID %in% unique(temp$HH_ID)),]
  total_spent_category = HH_in_732_and_MC66 %>%
    group_by(HH_ID) %>%
    summarise(DollarsCategory = sum(Dollars), QuantityCategory = sum(Quantity), P_QCategory = mean(P_Q))
  
  #Total Spent
  HH_in_MC66 = data_2[((data_2$HH_ID %in% unique(temp$HH_ID))),] 
  DollarsOtherCategory = HH_in_MC66 %>%
    group_by(HH_ID) %>% 
    summarise(TotalSpending= sum(Dollars))
  
  head(DollarsOtherCategory)
  
  #Median Income and population
  HH_income_company = HH_income[(HH_income$HH_ID %in% unique(temp$HH_ID)),]
  
  #Combine all features 
  data = cbind(HH_income_company[,c(1,3,5)], total_spent_company[,2:ncol(total_spent_company)], 
               total_spent_category[,2:ncol(total_spent_category)],
               channel_count[,2:ncol(channel_count)], DollarsOtherCategory[,2])
  
  #Subset households with no zipcode information
  temp2 = subset(data, (is.na(data$Median) == TRUE | is.na(data$Pop)==TRUE))
  data = subset(data, (is.na(data$Median) == FALSE | is.na(data$Pop)==FALSE))
  
  #Clear workspace of temporary variable OPTIONAL
  rm("HH_income_company", "total_spent_category", "HH_in_732_and_MC66", "total_spent_company",
     "channel_count", "month_count")
  
  #K-means----
  library(ClusterR)
  library(factoextra)
  library(FactoMineR)
  
  
  #Create subsets
  set.seed(1234)
  subset = sample(nrow(data), nrow(data)*0.5)
  kmeans_subset = data[subset,]
  rest_data = data[-subset,]
  
  #Scale and weight
  scaled_data = scale(kmeans_subset[,c(4:ncol(kmeans_subset))], center=TRUE, scale = TRUE)
  x = Clara_Medoids(scaled_data, clusters=4, samples = 4, sample_size = 0.25, threads=3, 
                    fuzzy = TRUE, verbose = TRUE, seed =1234)
  
  
  