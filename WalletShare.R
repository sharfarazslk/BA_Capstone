#Wallet Share Code
  #Libraries Needed
  install.packages('zoo', 'dplyr')
  library(zoo)
  library(dplyr)

  #Build Correct Dataset
  data_2 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/DMEFLines3Dataset2.csv", as.is=T)
  
  #Break into subset
  major_categories = subset(data_2, (data_2$MajorCategory==66 | data_2$MajorCategory==30 | 
                                       data_2$MajorCategory==43))
  #Break into Quarters
  major_categories$OrderDate = as.Date(as.character(major_categories$OrderDate), "%Y%m%d")
  major_categories$OrderQuarter = as.Date(as.yearqtr(major_categories$OrderDate))
  major_categories$OrderDate= NULL
  
  #Wallet Share by Dollar
    #Products
    products_dollars = group_by(major_categories, HH_ID, OrderQuarter, ProductArea)
    products_dollars = summarise(products_dollars, DollarsSpent = sum(Dollars))
    
    #MajorCategories
    majorcategories_dollar = group_by(major_categories, HH_ID, OrderQuarter, MajorCategory)
    majorcategories_dollar = summarise(majorcategories_dollar, DollarsSpent = sum(Dollars))
  
  #Wallet Share by Quantity
    #Products
    products_quantity = group_by(major_categories, HH_ID, OrderQuarter, ProductArea)
    products_quantity = summarise(products_quantity, TotalQuantity = sum(Quantity))
    
    #MajorCategories
    majorcategories_quantity = group_by(major_categories, HH_ID, OrderQuarter, MajorCategory)
    majorcategories_quantity = summarise(majorcategories_quantity, TotalQuantity = sum(Quantity))
    
    