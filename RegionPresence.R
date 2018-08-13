#Region Presence
#Libraries Needed
install.packages('dplyr', 'zipcode', 'ggmap', 'tidyr', 'readr', 'maps', 'zoo')
install.packages('purrr', 'magick')
library(dplyr)
library(zipcode)
library(tidyr)
library(ggmap)
library(maps)
library(zoo)
library(purrr)


#Build Correct Dataset
data_1 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/4-DMEF3YrBase.csv", colClasses=c('integer','factor'))
data_2 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/DMEFLines3Dataset2.csv", as.is=T)

#Break into subset
major_categories = subset(data_2, (data_2$MajorCategory==66 | data_2$MajorCategory==30 |
                                     data_2$MajorCategory==43))

#major_categories = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/majorcategory.csv", as.is=T)
#major_categories$X = NULL

#Break into Quarters
major_categories$OrderDate = as.Date(as.character(major_categories$OrderDate), "%Y%m%d")
major_categories$OrderQuarter = as.Date(as.yearqtr(major_categories$OrderDate))
major_categories$OrderDate= NULL

#Join Zipcode information
data("zipcode")
all_zip = zipcode
major_categories = inner_join(major_categories, data_1, by=c("HH_ID"="HH_ID"))
major_categories = inner_join(major_categories, all_zip, by=c("ZIPCode"="zip"))
major_categories$city=NULL
major_categories$state=NULL

#Break into categories
category_1 = major_categories[(major_categories$MajorCategory==66),]
category_2 = major_categories[(major_categories$MajorCategory==30),]
category_3 = major_categories[(major_categories$MajorCategory==43),]
#category_1 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/category_1.csv", as.is=T)
#category_1$X = NULL
#category_2 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/category_2.csv", as.is=T)
# category_2$X = NULL
# category_3 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/category_3.csv", as.is=T)
# category_3$X = NULL

#Category 1
  top_10_companies = group_by(category_1, CompanyID)
  top_10_companies = summarise(top_10_companies, TotalQuantity = sum(Quantity))
  top_10_companies = arrange(top_10_companies, desc(TotalQuantity))
  top_3_companies = c(top_10_companies[1:3,1])
  
  #Graph for top 3 categories
  category_1_top_3 = category_1[(category_1$CompanyID %in% top_3_companies$CompanyID),]
  category_1_top_3 = category_1_top_3[,-c(1, 3:8)]
  category_1_top_3$CompanyID = as.factor(category_1_top_3$CompanyID)
  temp = category_1_top_3[1:100000,]
  temp_quarter = unique(temp$OrderQuarter)
  temp_quarter = temp_quarter[order(as.Date(temp_quarter, format="%Y%m%d"))]
  
  #Create timeline of map
  devtools::install_github("dkahle/ggmap")
  map<-get_map(location='united states', zoom=4, maptype = "terrain",
               source='google',color='color')
  
  frames = length(temp_quarter)
  for (i in 1:frames){
    
    ggmap(map) + geom_point(
      aes(x=longitude, y=latitude, color=CompanyID), 
      data=category_1_top_3[(category_1_top_3$OrderQuarter==temp_quarter[i]),], 
      alpha=.5, na.rm = T, cex=1)+
      theme(axis.text.x = element_text(angle = 60, vjust=0.15, size=8),
            legend.position=c(1,1),legend.justification=c(1,1),
            legend.direction="vertical",legend.text=element_text(size=8),
            legend.title=element_text(size=8, face="bold"),
            legend.box="horizontal", panel.background = element_blank(),
            legend.box.just = c("top"))+
      ggtitle(temp_quarter[i]) + ylab("Latitude") + xlab("Longitude")
    
    print(paste0("saving plot ", temp_quarter[i]))
    
    ggsave(filename = paste0("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-1/",temp_quarter[i],".png"),
           width = 12,height=12,dpi = 150)
  }
  
  #Create GIF
  list.files(path = "~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-1/", pattern = "*.png", full.names = T) %>% 
    map(image_read) %>% # reads each path file
    image_join() %>% # joins image
    image_animate(fps=1) %>% # animates, can opt for number of loops
    image_write("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-1/quarter_sales_c1.gif") # write to current dir
  
  #Count of unique zones
  category_1_top_3 = category_1[(category_1$CompanyID %in% top_3_companies$CompanyID),]
  category_1_geography = group_by(category_1_top_3, CompanyID, OrderQuarter)
  category_1_geography = summarize(category_1_geography, ZIPCount=n_distinct(ZIPCode))
  category_1_geography$CompanyID = as.factor(category_1_geography$CompanyID)
  
  ggplot(data=category_1_geography, aes(x=OrderQuarter, y=ZIPCount, color = CompanyID)) +
    geom_point() + geom_line() + labs(subtitle="Total Geographic Presence Over Time", 
                                      y="Zip Code Count", x="Year", title="Women's Apparel")
  
  ggsave(filename = paste0("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-1/trend_category1.png"),
         width = 8,height=8,dpi = 150)
  
#Category2
  top_10_companies = group_by(category_2, CompanyID)
  top_10_companies = summarise(top_10_companies, TotalQuantity = sum(Quantity))
  top_10_companies = arrange(top_10_companies, desc(TotalQuantity))
  top_3_companies = c(top_10_companies[1:3,1])
  
  #Graph for top 3 categories
  category_2_top_3 = category_2[(category_2$CompanyID %in% top_3_companies$CompanyID),]
  category_2_top_3 = category_2_top_3[,-c(1, 3:8)]
  category_2_top_3$CompanyID = as.factor(category_2_top_3$CompanyID)
  
  #Create timeline of map
  for (i in 1:frames){
    
    ggmap(map) + geom_point(
      aes(x=longitude, y=latitude, color=CompanyID), 
      data=category_2_top_3[(category_2_top_3$OrderQuarter==temp_quarter[i]),], alpha=.5, na.rm = T, cex=1)+
      #ylab("Latitude") + xlab("Longitude") +
      theme(axis.text.x = element_text(angle = 60, vjust=0.15, size=8),
            legend.position=c(1,1),legend.justification=c(1,1),
            legend.direction="vertical",legend.text=element_text(size=8),
            legend.title=element_text(size=8, face="bold"),
            legend.box="horizontal", panel.background = element_blank(),
            legend.box.just = c("top"))+
      ggtitle(temp_quarter[i]) + ylab("Latitude") + xlab("Longitude")
    
    print(paste0("saving plot ", temp_quarter[i]))
    
    ggsave(filename = paste0("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-2/",temp_quarter[i],".png"),
           width = 12,height=12,dpi = 150)
  }
  
  #Create GIF
  list.files(path = "~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-2/", pattern = "*.png", full.names = T) %>% 
    map(image_read) %>% # reads each path file
    image_join() %>% # joins image
    image_animate(fps=1) %>% # animates, can opt for number of loops
    image_write("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-2/quarter_sales_c2.gif") # write to current dir
  
  #Count of unique zones
  category_2_top_3 = category_2[(category_2$CompanyID %in% top_3_companies$CompanyID),]
  category_2_geography = group_by(category_2_top_3, CompanyID, OrderQuarter)
  category_2_geography = summarize(category_2_geography, ZIPCount=n_distinct(ZIPCode))
  category_2_geography$CompanyID = as.factor(category_2_geography$CompanyID)
  
  ggplot(data=category_2_geography, aes(x=OrderQuarter, y=ZIPCount, color = CompanyID)) +
    geom_point() + geom_line() + labs(subtitle="Total Geographic Presence Over Time", 
                                      y="Zip Code Count", x="Year", title="Home Decor")
  
  ggsave(filename = paste0("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-2/trend_category2.png"),
         width = 8,height=8,dpi = 150)
  
#Category3
  top_10_companies = group_by(category_3, CompanyID)
  top_10_companies = summarise(top_10_companies, TotalQuantity = sum(Quantity))
  top_10_companies = arrange(top_10_companies, desc(TotalQuantity))
  top_3_companies = c(top_10_companies[1:3,1])
  
  #Graph for top 3 categories
  category_3_top_3 = category_3[(category_3$CompanyID %in% top_3_companies$CompanyID),]
  category_3_top_3 = category_3_top_3[,-c(1, 3:8)]
  category_3_top_3$CompanyID = as.factor(category_3_top_3$CompanyID)
  
  #Create timeline map
  for (i in 1:frames){
    
    ggmap(map) + geom_point(
      aes(x=longitude, y=latitude, color=CompanyID), 
      data=category_3_top_3[(category_3_top_3$OrderQuarter==temp_quarter[i]),], alpha=.5, na.rm = T, cex=1)+
      theme(axis.text.x = element_text(angle = 60, vjust=0.15, size=8),
            legend.position=c(1,1),legend.justification=c(1,1),
            legend.direction="vertical",legend.text=element_text(size=8),
            legend.title=element_text(size=8, face="bold"),
            legend.box="horizontal", panel.background = element_blank(),
            legend.box.just = c("top"))+
      ggtitle(temp_quarter[i]) + ylab("Latitude") + xlab("Longitude")
    
    print(paste0("saving plot ", temp_quarter[i]))
    
    ggsave(filename = paste0("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-3/",temp_quarter[i],".png"),
           width = 12,height=12,dpi = 150)
  }
  
  #Create GIF
  list.files(path = "~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-3/", pattern = "*.png", full.names = T) %>% 
    map(image_read) %>% # reads each path file
    image_join() %>% # joins image
    image_animate(fps=1) %>% # animates, can opt for number of loops
    image_write("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-3/quarter_sales_c3.gif") # write to current dir
  
  #Count of unique zones
  category_3_top_3 = category_3[(category_3$CompanyID %in% top_3_companies$CompanyID),]
  category_3_geography = group_by(category_3_top_3, CompanyID, OrderQuarter)
  category_3_geography = summarize(category_3_geography, ZIPCount=n_distinct(ZIPCode))
  category_3_geography$CompanyID = as.factor(category_3_geography$CompanyID)
  
  ggplot(data=category_3_geography, aes(x=OrderQuarter, y=ZIPCount, color = CompanyID)) +
    geom_point() + geom_line() + labs(subtitle="Total Geographic Presence Over Time", 
                                      y="Zip Code Count", x="Year", title="Men's Apparel")
  
  ggsave(filename = paste0("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-3/trend_category3.png"),
         width = 8,height=8,dpi = 150)
