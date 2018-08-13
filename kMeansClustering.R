#K-Means Clustering
#Libraries Needed
install.packages('dplyr', 'zipcode', 'ggmap', 'tidyr', 'readr', 'maps', 'zoo')
install.packages('purrr', 'magick')
library(dplyr)
library(zipcode)
library(tidyr)
library(ggmap)
library(maps)
library(zoo)
#library(purrr)
library(readxl)

#Load Data
  data_1 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/4-DMEF3YrBase.csv", colClasses=c('integer','factor'))
  data_2 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/DMEFLines3Dataset2.csv", as.is=T)
  median_income = read_excel("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/MedianZIP-3.xlsx")
  median_income$Zip = as.factor(median_income$Zip)
  major_categories = subset(data_2, (data_2$MajorCategory==66 | data_2$MajorCategory==30 |
                                       data_2$MajorCategory==43))
  
  category_1 = major_categories[(major_categories$MajorCategory==66),]
  category_2 = major_categories[(major_categories$MajorCategory==30),]
  category_3 = major_categories[(major_categories$MajorCategory==43),]

#Join Income information
  #Category-1
  HH_1 = unique(category_1$HH_ID)
  data_1_c1 = data_1[(data_1$HH_ID %in% HH_1),]
  HH_income = left_join(data_1_c1, median_income, by=c("ZIPCode" = "Zip"))
  HH_income$X__1 = NULL
  category_1$P_Q = with(category_1, category_1$Dollars/category_1$Quantity)
  hist(category_1$P_Q, xlim = c(0,100), breaks=1000)
  
  #Bringing outliers P/Qdown to median of outliers
  outliers = category_1[(category_1$P_Q>=139.00),]
  outliers$P_Q = 139
  temp = category_1[(category_1$P_Q<139),]
  category_1 = rbind(temp, outliers)
  
  #Combine housing data with P/Q per household
  avgPQ = group_by(category_1, HH_ID)
  avgPQ = summarise(avgPQ, AveragePrice = mean(P_Q))
  
  #Kmeans
  kmean_c1 = merge(x= HH_income, y=avgPQ, by="HH_ID", all.x=TRUE)
  kmean_c1$Mean = NULL
  idx = which(is.na(kmean_c1$Median))
  temp = kmean_c1[idx,]
  kmean_c1 = kmean_c1[-idx,]
  
  #Within cluster distance
  wss <- (nrow(kmean_c1[, c(3,5)])-1)*sum(apply(kmean_c1[, c(3,5)],2,var))
  for (i in 2:20) wss[i] <- sum(kmeans(kmean_c1[, c(3,5)],
                                       centers=i)$withinss, iter.max =100)
  
  plot(1:20, wss, type="b",
       xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  #Kmeans
  set.seed(1234)
  mc = kmeans(kmean_c1[, c(3,5)], 4, iter.max = 100, nstart=20)
  kmean_c1 = cbind(kmean_c1, mc$cluster)
  
  #Summary
  colnames(kmean_c1)[6] = "Cluster" 
  summary_c1 = group_by(kmean_c1, Cluster)
  summary_c1 = summarise(summary_c1, MeanIncome = mean(Median), MeanPrice= mean(AveragePrice), NumberofHH = n())
  
  #Map
  data("zipcode")
  all_zip = zipcode
  all_zip$zip = as.factor(all_zip$zip)
  kmean_c1 = left_join(x=kmean_c1, y=all_zip[,c(1,4,5)], by=c("ZIPCode"= "zip"), all.x=TRUE)
  
  map<-get_map(location='united states', zoom=4, maptype = "terrain",
               source='google',color='bw')
  
  #For Legend Purposes
  temp2 = kmean_c1
  temp2$Cluster[temp2$Cluster==1] = 40000
  temp2$Cluster[temp2$Cluster==2] = 90000
  temp2$Cluster[temp2$Cluster==3] = 135000
  temp2$Cluster[temp2$Cluster==4] = 60000
  temp2$Cluster = as.factor(temp2$Cluster)
  
  ggmap(map) + geom_point(
    aes(x=longitude, y=latitude, color=Cluster), 
    data=temp2, 
    alpha=.5, na.rm = T, cex=0.01)+
    scale_fill_hue()+
    theme(axis.text.x = element_text(angle = 60, vjust=0.15, size=8),
          legend.position=c(1,1),legend.justification=c(1,1),
          legend.direction="vertical",legend.text=element_text(size=8),
          legend.title=element_text(size=8, face="bold"),
          legend.box="horizontal", panel.background = element_blank(),
          legend.box.just = c("top"))+
    guides(colour = guide_legend(override.aes = list(size=1)))+
    ggtitle("Household Clusters for Women's Apparel") + ylab("Latitude") + xlab("Longitude")
  
    ggsave(filename="~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category-1/cluster.png",
         width = 12,height=12,dpi = 200)
  
  #Write csv
  write.csv(kmean_c1, file="~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category66Clusters.csv")
  
  #Category-2
  HH_2 = unique(category_2$HH_ID)
  data_1_c2 = data_1[(data_1$HH_ID %in% HH_2),]
  HH_income = left_join(data_1_c2, median_income, by=c("ZIPCode" = "Zip"))
  HH_income$X__1 = NULL
  category_2$P_Q = with(category_2, category_2$Dollars/category_2$Quantity)
  hist(category_2$P_Q, xlim = c(0,100), breaks=1000)
  
  #Bringing outliers P/Qdown to median of outliers
  outliers = category_2[(category_2$P_Q>=100.00),]
  outliers$P_Q = 100
  temp = category_2[(category_2$P_Q<100),]
  category_2 = rbind(temp, outliers)
  
  #Combine housing data with P/Q per household
  avgPQ = group_by(category_2, HH_ID)
  avgPQ = summarise(avgPQ, AveragePrice = mean(P_Q))
  
  #Kmeans
  kmean_c2 = merge(x= HH_income, y=avgPQ, by="HH_ID", all.x=TRUE)
  kmean_c2$Mean = NULL
  idx = which(is.na(kmean_c2$Median))
  temp = kmean_c2[idx,]
  kmean_c2 = kmean_c2[-idx,]
  
  #Within cluster distance
  wss <- (nrow(kmean_c2[, c(3,5)])-1)*sum(apply(kmean_c2[, c(3,5)],2,var))
  for (i in 2:20) wss[i] <- sum(kmeans(kmean_c2[, c(3,5)],
                                       centers=i)$withinss, iter.max =100)
  
  plot(1:20, wss, type="b",
       xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  #Kmeans
  set.seed(1234)
  mc = kmeans(kmean_c2[, c(3,5)], 4, iter.max = 100, nstart=20)
  kmean_c2 = cbind(kmean_c2, mc$cluster)
  
  #Summary
  colnames(kmean_c2)[6] = "Cluster" 
  summary_c2 = group_by(kmean_c2, Cluster)
  summary_c2 = summarise(summary_c2, MeanIncome = mean(Median), MeanPrice= mean(AveragePrice), NumberofHH = n())
  
  #Map
  data("zipcode")
  all_zip = zipcode
  all_zip$zip = as.factor(all_zip$zip)
  kmean_c2 = left_join(x=kmean_c2, y=all_zip[,c(1,4,5)], by=c("ZIPCode"= "zip"), all.x=TRUE)
  
  map<-get_map(location='united states', zoom=4, maptype = "terrain",
               source='google',color='bw')
  
  #For Legend Purposes
  temp2 = kmean_c2
  temp2$Cluster[temp2$Cluster==1] = 60000
  temp2$Cluster[temp2$Cluster==2] = 85000
  temp2$Cluster[temp2$Cluster==3] = 120000
  temp2$Cluster[temp2$Cluster==4] = 40000
  temp2$Cluster = as.factor(temp2$Cluster)
  
  ggmap(map) + geom_point(
    aes(x=longitude, y=latitude, color=Cluster), 
    data=temp2, 
    alpha=.5, na.rm = T, cex=0.0000001)+
    #scale_fill_manual(values=wes_palette(n=4, name="Darjeeling"))+
    scale_fill_hue()+
    theme(axis.text.x = element_text(angle = 60, vjust=0.15, size=8),
          legend.position=c(1,1),legend.justification=c(1,1),
          legend.direction="vertical",legend.text=element_text(size=8),
          legend.title=element_text(size=8, face="bold"),
          legend.box="horizontal", panel.background = element_blank(),
          legend.box.just = c("top"))+
    guides(colour = guide_legend(override.aes = list(size=1)))+
    ggtitle("Clusters Based on Median Income") + ylab("Latitude") + xlab("Longitude")
  
  #Write csv
  write.csv(kmean_c2, file="~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category30Clusters.csv")
  
  #Join Income information
  #Category-3
  HH_3 = unique(category_3$HH_ID)
  data_1_c3 = data_1[(data_1$HH_ID %in% HH_3),]
  HH_income = left_join(data_1_c3, median_income, by=c("ZIPCode" = "Zip"))
  HH_income$X__1 = NULL
  category_3$P_Q = with(category_3, category_3$Dollars/category_3$Quantity)
  hist(category_3$P_Q, xlim = c(0,100), breaks=1000)
  
  #Bringing outliers P/Qdown to median of outliers
  outliers = category_3[(category_3$P_Q>=100.00),]
  outliers$P_Q = 100
  temp = category_3[(category_3$P_Q<100),]
  category_3 = rbind(temp, outliers)
  
  #Combine housing data with P/Q per household
  avgPQ = group_by(category_3, HH_ID)
  avgPQ = summarise(avgPQ, AveragePrice = mean(P_Q))
  
  #Kmeans
  kmean_c3 = merge(x= HH_income, y=avgPQ, by="HH_ID", all.x=TRUE)
  kmean_c3$Mean = NULL
  idx = which(is.na(kmean_c3$Median))
  temp = kmean_c3[idx,]
  kmean_c3 = kmean_c3[-idx,]
  
  #Within cluster distance
  wss <- (nrow(kmean_c3[, c(3,5)])-1)*sum(apply(kmean_c3[, c(3,5)],2,var))
  for (i in 2:20) wss[i] <- sum(kmeans(kmean_c3[, c(3,5)],
                                       centers=i)$withinss, iter.max =100)
  
  plot(1:20, wss, type="b",
       xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  #Kmeans
  set.seed(1234)
  mc = kmeans(kmean_c3[, c(3,5)], 4, iter.max = 100, nstart=20)
  kmean_c3 = cbind(kmean_c3, mc$cluster)
  
  #Summary
  colnames(kmean_c3)[6] = "Cluster" 
  summary_c3 = group_by(kmean_c3, Cluster)
  summary_c3 = summarise(summary_c3, MeanIncome = mean(Median), MeanPrice= mean(AveragePrice), NumberofHH = n())
  
  #Map
  data("zipcode")
  all_zip = zipcode
  all_zip$zip = as.factor(all_zip$zip)
  kmean_c3 = left_join(x=kmean_c3, y=all_zip[,c(1,4,5)], by=c("ZIPCode"= "zip"), all.x=TRUE)
  
  map<-get_map(location='united states', zoom=4, maptype = "terrain",
               source='google',color='bw')
  #For Legend Purposes
  temp2 = kmean_c3
  temp2$Cluster[temp2$Cluster==1] = 65000
  temp2$Cluster[temp2$Cluster==2] = 90000
  temp2$Cluster[temp2$Cluster==3] = 140000
  temp2$Cluster[temp2$Cluster==4] = 40000
  temp2$Cluster = as.factor(temp2$Cluster)
  
  ggmap(map) + geom_point(
    aes(x=longitude, y=latitude, color=Cluster), 
    data=temp2, 
    alpha=.3, na.rm = T, cex=0.0000001)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, vjust=0.15, size=8),
          legend.position=c(1,1),legend.justification=c(1,1),
          legend.direction="vertical",legend.text=element_text(size=8),
          legend.title=element_text(size=8, face="bold"),
          legend.box="horizontal", panel.background = element_blank(),
          legend.box.just = c("top"))+
    guides(colour = guide_legend(override.aes = list(size=2)))+
    #geom_jitter()+
    ggtitle("Clusters Based on Median Income") + ylab("Latitude") + xlab("Longitude")
  
  #Write csv
  write.csv(kmean_c3, file="~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/Category43Clusters.csv")
  
  