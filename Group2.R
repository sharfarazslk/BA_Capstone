#BUSN460 Project

#Packages needed

#Load Data Set
  data_1 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/4-DMEF3YrBase.csv", colClasses=c('integer','factor'))
  data_2 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/DMEFLines3Dataset2.csv", as.is=T)
  data_3 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/DMEFOrders3Dataset2.csv", as.is=T)

#Clean data
  data_2$OrderDate = as.Date(as.character(data_2$OrderDate), "%Y%m%d")

#Exploratory 
  households = as.data.frame(unique(data_2[,1]))
  companies = as.data.frame(unique(data_2[,2]))
  household1 = data_2[(data_2$HH_ID==1),]

#Major Categories
  major_categories_string = "SELECT SUM(data_2.Quantity) as Quantity, data_2.MajorCategory
                    FROM data_2
                    GROUP BY data_2.MajorCategory
                    ORDER BY Quantity DESC"
  major_categories = sqldf(major_categories_string)

#Sales Channel over time
  install.packages("sqldf")
  install.packages('zoo')
  library(zoo)
  library(sqldf)
  data_2$OrderQuarter = as.Date(as.yearqtr(data_2$OrderDate))
  
  #Categories 66, 30, 43
  major_categories = subset(data_2, (data_2$MajorCategory==66 | data_2$MajorCategory==30 | 
                                       data_2$MajorCategory==43))
  
  channel_string = "SELECT SUM(major_categories.Quantity) as Quantity, major_categories.Channel, 
                    major_categories.OrderQuarter, major_categories.MajorCategory
                    FROM major_categories
                    GROUP BY major_categories.OrderQuarter, major_categories.Channel,
                    major_categories.MajorCategory"
  
  Sales_Channel_Over_Time = sqldf(channel_string)
  
  #Graph Information
  install.packages("ggplot2")
  library(ggplot2)
  library(scales)
  options(scipen=999)
  
  #For Category 66
  gg_wa= ggplot(subset(Sales_Channel_Over_Time, Sales_Channel_Over_Time$MajorCategory==66), 
             aes(x=OrderQuarter, y=Quantity, color = Channel)) + 
            geom_point() + geom_line() + labs(subtitle="Dollars Spent Through Each Channel", 
                                               y="Total Volume", x="Year", title="Women's Apparel") 
                                               
  plot(gg_wa)
  
  #For Category 30
  gg_hd= ggplot(subset(Sales_Channel_Over_Time, Sales_Channel_Over_Time$MajorCategory==30), 
                aes(x=OrderQuarter, y=Quantity, color = Channel)) + 
    geom_point() + geom_line() + labs(subtitle="Dollars Spent Through Each Channel", 
                                      y="Total Volume", x="Year", title="Home Decor") 
  
  plot(gg_hd)
  
  #For Category 43
  gg_ma= ggplot(subset(Sales_Channel_Over_Time, Sales_Channel_Over_Time$MajorCategory==43), 
                aes(x=OrderQuarter, y=Quantity, color = Channel)) + 
    geom_point() + geom_line() + labs(subtitle="Dollars Spent Through Each Channel", 
                                      y="Total Volume", x="Year", title="Men's Apparel") 
  
  plot(gg_ma)
  
  #Sales Channel per company over time
  channel_string = "SELECT SUM(data_2.Dollars) as DollarsSpent, data_2.Channel, data_2.OrderQuarter, data_2.CompanyID
                    FROM data_2
                    GROUP BY data_2.OrderQuarter, data_2.Channel, data_2.CompanyID"
  Sales_Channel_Company = sqldf(channel_string)
  
#Order/Sales volume per year
  channel_string = "SELECT SUM(data_2.Dollars) as DollarsSpent, data_2.Channel, data_2.OrderQuarter, data_2.CompanyID
                    FROM data_2
                    GROUP BY data_2.OrderQuarter, data_2.Channel, data_2.CompanyID"
  Sales_Channel_Company = sqldf(channel_string)
  
  #Company 732 and category 66
  major_categories = subset(data_2, (data_2$CompanyID==732 & data_2$MajorCategory==66))
  
  channel_string = "SELECT SUM(major_categories.Quantity) as Quantity, major_categories.Channel, 
                  major_categories.OrderQuarter
                  FROM major_categories
                  GROUP BY major_categories.OrderQuarter, major_categories.Channel"
  
  Sales_Channel_Over_Time = sqldf(channel_string)
  
  total_volume_string = "SELECT SUM(major_categories.Quantity) as Quantity, 
                  major_categories.OrderQuarter
                  FROM major_categories
                  GROUP BY major_categories.OrderQuarter"
  total_volume = sqldf(total_volume_string)
  
  gg_tv = ggplot(data=Sales_Channel_Over_Time, aes(x=OrderQuarter, y=Quantity, color = Channel)) +
        #ggplot(data=total_volume, aes(x=OrderQuarter, y=Quantity, color="black")) +
    geom_point() + geom_line() + labs(subtitle="Dollars Spent Through Each Channel", 
                                     y="Total Volume", x="Year", title="Company:732 Apparel")
    
  plot(gg_tv)
  
  #Dollar Per Year
  
  
#Product Portfolio
  major_categories = subset(data_2, data_2$MajorCategory==66)
  companies_66 = as.data.frame(unique(major_categories[,2]))
  top_10 = c(732, 734, 736, 217, 965, 837, 121, 738, 744, 913)
  temp = data_2[(data_2$CompanyID %in% top_10),]
  
  
  count_of_category_string = "SELECT COUNT(DISTINCT temp.CompanyID) as CategoryCount, temp.MajorCategory
                              FROM temp
                              GROUP BY temp.MajorCategory"
  
  count_of_category = sqldf(count_of_category_string)
  
  bar= ggplot(data=count_of_category, aes(x=MajorCategory, y=CategoryCount)) +
      geom_bar(stat="identity", fill="steelblue")
  bar
  
#Geographical Visualizations
  major_categories = subset(data_2, (data_2$MajorCategory==66 | data_2$MajorCategory==30 | 
                                       data_2$MajorCategory==43))
  
  HH_string = "SELECT SUM(major_categories.Dollars) as DollarsSpent, major_categories.HH_ID
                FROM major_categories
                GROUP BY major_categories.HH_ID"
  
  Sales_HH = sqldf(HH_string)
  
  HH_joined_string = "SELECT Sales_HH.HH_ID, Sales_HH.DollarsSpent, data_1.ZIPCode
                      FROM Sales_HH
                      INNER JOIN data_1 ON Sales_HH.HH_ID = data_1.HH_ID"
  
  HH_joined = sqldf(HH_joined_string)
  
  write.csv(HH_joined, file = "geographical_data.csv")
  
  #Build Map
  install.packages('zipcode', 'dplyr', 'tidyr', 'readr', 'ggmap')
  library(zipcode)
  library(dplyr)
  library(tidyr)
  library(ggmap)
  
  data("zipcode")
  all_zip = zipcode

  combined = inner_join(all_zip, HH_joined, by=c("zip"= "ZIPCode"))
  combined_string  = "SELECT combined.zip, SUM(combined.DollarsSpent) as DollarsSpent, combined.latitude,
                      combined.longitude 
                      FROM combined
                      GROUP BY combined.zip"
  
  combined = sqldf(combined_string)
  combined3 = subset(combined[(combined$DollarsSpent>16655),])
  combined2 = subset(combined[(combined$DollarsSpent<16655),])
  
  combined3$DollarsSpent = 16655
  combined4 = rbind(combined2, combined3)
  #combined2$DollarsSpent = (combined2$DollarsSpent - mean(combined2$DollarsSpent))/sd(combined2$DollarsSpent)
  
  map<-get_map(location='united states', zoom=4, maptype = "terrain",
               source='google',color='color')
  ggmap(map) + geom_point(
    aes(x=longitude, y=latitude, show_guide = TRUE, color=DollarsSpent), 
    data=combined4, alpha=.5, na.rm = T, cex=0.15)  + 
    scale_color_gradient(low = 'lightblue', high = 'blue')
  
  summary(combined4)