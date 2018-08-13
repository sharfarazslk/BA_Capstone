#Linear Regression
#Libraries Needed
#install.packages('dplyr', 'zipcode', 'ggmap', 'tidyr', 'readr', 'maps', 'zoo', 'parallel', 'lubridate')
#install.packages('FactoMineR', 'data.table')
library(dplyr)
library(zipcode)
library(tidyr)
library(ggmap)
library(maps)
library(zoo)
library(readxl)
library(parallel)
library(lubridate)
library(FactoMineR)
library(data.table)

  #Load Data
  data_1 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/4-DMEF3YrBase.csv", colClasses=c('integer','factor'))
  data_2 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/DMEFLines3Dataset2.csv", as.is=T)
  data_3 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/DMEFOrders3Dataset2.csv", as.is=T)
  
  #Create Subset
  major_categories = subset(data_2, (data_2$MajorCategory==66 | data_2$MajorCategory==30 |
                                       data_2$MajorCategory==43))
  
  #Create Price/Quantity Columns
  major_categories$P_Q = with(major_categories, major_categories$Dollars/major_categories$Quantity)
  
  #Prep data for regression
  #Create Month Column 
  #major_categories = major_categories[,4:ncol(major_categories)]
  major_categories$OrderDate = as.Date(as.character(major_categories$OrderDate),"%Y%m%d", origin = "1960-10-01")
  major_categories$OrderMonth = month(as.POSIXlt(major_categories$OrderDate, format="%Y%m%d"))
  major_categories$OrderMonth = as.factor(major_categories$OrderMonth)
  major_categories$Channel = as.factor(major_categories$Channel)
  
  #Binary Columns
  temp = major_categories
  temp = temp[,5:ncol(temp)]
  setDT(temp)[, c(levels(temp$Channel), "Channel") := 
              c(lapply(levels(Channel), function(x) as.integer(x == Channel)), .(NULL))]
  
  setDT(temp)[, c(levels(temp$OrderMonth), "OrderMonth") := 
                c(lapply(levels(OrderMonth), function(x) as.integer(x == OrderMonth)), .(NULL))]
  
  setDT(temp)[, c(levels(temp$ProductArea), "ProductArea") := 
                c(lapply(levels(ProductArea), function(x) as.integer(x == ProductArea)), .(NULL))]
  
  setDT(temp)[, c(levels(temp$MajorCategory), "MajorCategory") := 
                c(lapply(levels(MajorCategory), function(x) as.integer(x == MajorCategory)), .(NULL))]
  
  #Eig Decomposition
  # temp.pca = PCA(temp, scale.unit = TRUE, graph=T)
  # temp.pca$eig
  
  
  #Channel
  monthly = group_by(major_categories, )