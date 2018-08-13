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

#Bringing outliers down to median
outliers = category_1[(category_1$P_Q>139.00),]


wss <- (nrow(HH_income[, c(3,5)])-1)*sum(apply(HH_income[, c(3,5)],2,var))
for (i in 2:20) wss[i] <- sum(kmeans(HH_income[, c(3,5)],
                                     centers=i)$withinss, iter.max =100)
plot(1:20, wss, type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

mc = kmeans(HH_income[, c(3,5)], 4)

temp = cbind(HH_income,mc$cluster)
colnames(temp)[6] = "Cluster"

# 
temp2 = merge(x= HH_income, y=temp[,c(1,6)], by="HH_ID", all.x=TRUE)
View(temp2)
write.csv(temp2, file="~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/HouseholdCluster.csv")
# 
# #Get Lat and Long
data("zipcode")
all_zip = zipcode
income_map = inner_join(temp2, all_zip[,c(1,4:5)], by=c("ZIPCode" = "zip"))
write.csv(income_map, file="~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/HouseholdCluster.csv")
#income_map = temp2
income_map = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/HouseholdCluster.csv")
income_map$X = NULL
income_map$Cluster = as.factor(income_map$Cluster)

map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

temp2 = sample_n(income_map, nrow(temp)*0.1)

ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, color=Cluster), 
  data=temp2, 
  alpha=.5, na.rm = T, cex=0.00001)+
  theme(axis.text.x = element_text(angle = 60, vjust=0.15, size=8),
        legend.position=c(1,1),legend.justification=c(1,1),
        legend.direction="vertical",legend.text=element_text(size=8),
        legend.title=element_text(size=8, face="bold"),
        legend.box="horizontal", panel.background = element_blank(),
        legend.box.just = c("top"))+
  guides(colour = guide_legend(override.aes = list(size=1)))+
  ggtitle("Clusters Based on Median Income") + ylab("Latitude") + xlab("Longitude")

#Sanity check
temp = group_by(income_map, Cluster)
temp = summarise(temp, MeanIncome = mean(Median), MeanPop = mean(Pop), NumberofHH = n())
View(temp)
temp3 = income_map[(is.na(income_map$Cluster)),]