#Week 1----------------------------------------
#Upload Data
library(readr)
data_1 <- read_csv("DMEF_DATA_SET_ELEVEN/4-DMEF3YrBase.CSV")
data_2 <- read_csv("DMEF_DATA_SET_ELEVEN/DMEFLines3Dataset2.CSV")
data_3 <- read_csv("DMEF_DATA_SET_ELEVEN/DMEFOrders3Dataset2.CSV")

#Clean data and add OrderQuarter column
data_2$OrderDate = as.Date(as.character(data_2$OrderDate), "%Y%m%d")
library(zoo)
data_2$OrderQuarter = as.Date(as.yearqtr(data_2$OrderDate))


#Data Summary 
str(data_3)             #review the data types and labels
head(data_3, n=20)      #check out the first 20 rows
summary(data_1)
summary(data_2)
summary(data_3)


#Top 3 Categories by Quantity------------------------------------------
#Top 3 Categories by Quantity = 66, 30, 43------------------------------------------
library(dplyr)
options(dplyr.print_max = 1e9)
top3CategoriesQuantity <- 
  data_2 %>% 
  group_by(MajorCategory) %>% 
  summarise(Quantity = sum(Quantity))
#Filter by Quantity
top3CategoriesQuantity[order(-top3CategoriesQuantity$Quantity),]


#Top 3 Categories by Dollars------------------------------------------
library(dplyr)
options(dplyr.print_max = 1e9)
top3CategoriesDollars <- 
  data_2 %>% 
  group_by(MajorCategory) %>% 
  summarise(Dollars = sum(Dollars))
#Filter by Dollars
top3CategoriesDollars[order(-top3Categories$Frequency),]



#Top Company in Each Category------------------------------------------
#Top Company in MajorCategory 66 = CompanyID 732------------------------------------------
TopCompany66 <- 
  data_2 %>% 
  group_by(CompanyID) %>% 
  filter(MajorCategory == 66) %>% 
  summarise(Frequency = sum(Quantity))
#Filter by Quantity
TopCompany66[order(-TopCompany66$Frequency),]

#Top Company in MajorCategory 66 = CompanyID 732------------------------------------------
TopCompany66OverTime <- 
  data_2 %>% 
  group_by(OrderQuarter) %>% 
  filter(MajorCategory == 66 & CompanyID == 732) %>% 
  summarise(Frequency = sum(Quantity))
#Filter by Quantity
TopCompany66OverTime[order(-TopCompany66OverTime$Frequency),]

#Top Company in MajorCategory 66 = CompanyID 732------------------------------------------
#Dollars over Time For MajorCategory 66 = CompanyID 732
TopCompany66DollarsOverTime <- 
  data_2 %>% 
  group_by(OrderQuarter) %>% 
  filter(MajorCategory == 66 & CompanyID == 732) %>% 
  summarise(Frequency = sum(Dollars))
#Filter by Dollars
TopCompany66DollarsOverTime[order(-TopCompany66DollarsOverTime$Frequency),]

#PlOT of Dollars over Time For MajorCategory 66 = CompanyID 732
library(ggplot2)
gg_732= ggplot(data = TopCompany66DollarsOverTime,
              aes(x=OrderQuarter, y=Frequency)) + 
  geom_point() + geom_line() + labs(subtitle="Overall Dollars Made", 
                                    y="Dollars", x="Year", title="Major Category 66, Company 732") 
plot(gg_732)

#Top Company by Dollars in MajorCategory 66 ------------------------------------------
TopCompany66Dollars <- 
  data_2 %>% 
  group_by(CompanyID) %>% 
  filter(MajorCategory == 66) %>% 
  summarise(Frequency = sum(Dollars))
#Filter by Quantity
TopCompany66Dollars[order(-TopCompany66Dollars$Frequency),]

Category66Comps <- c(732, 734, 736, 217, 965, 837, 121, 738, 744, 
913,866, 784,833, 760,205, 412, 36, 985, 958, 860, 125, 759, 767, 
637, 703, 820, 976, 979, 730, 933, 982,61, 740, 878, 978, 308, 
653, 688, 802, 841, 977, 746, 765, 201, 959, 973, 836, 661, 769, 
392, 147, 980, 206, 153, 393, 892, 879, 156, 814, 870, 823, 748, 
54, 238, 733, 755, 647, 843, 715, 873, 737, 757, 37, 631, 278, 
834, 768, 52, 231, 227, 324, 687, 184, 634, 816, 11, 643, 864, 
176, 662, 867, 107, 311, 719, 804, 53, 749, 48, 881, 244, 106, 
55, 770, 862, 123, 671, 258, 185, 726, 756, 182, 815, 672, 394, 
795, 807, 750, 617, 684, 223, 677, 309, 742, 111, 32, 633, 651, 
257, 94, 322, 670, 944, 877, 628, 713, 895, 3, 857, 822, 218, 
660, 954, 972, 659, 224, 650, 158, 195, 701, 808)
Category66CompsTop10<- c(732, 734, 736, 217, 965, 837, 121, 738, 744, 913)

#Top Company in MajorCategory 30 = CompanyID 153------------------------------------------
TopCompany30<- 
  data_2 %>% 
  group_by(CompanyID) %>% 
  filter(MajorCategory == 30) %>% 
  summarise(Frequency = sum(Quantity))
#Filter by Quantity
TopCompany30[order(-TopCompany30$Frequency),]

#Top Company by Dollars in MajorCategory 30 ------------------------------------------
TopCompany30Dollars <- 
  data_2 %>% 
  group_by(CompanyID) %>% 
  filter(MajorCategory == 30) %>% 
  summarise(Frequency = sum(Dollars))
#Filter by Quantity
TopCompany30Dollars[order(-TopCompany30Dollars$Frequency),]

#Dollars over Time For MajorCategory 30 = CompanyID 153
TopCompany153DollarsOverTime <- 
  data_2 %>% 
  group_by(OrderQuarter) %>% 
  filter(MajorCategory == 30 & CompanyID == 153) %>% 
  summarise(Frequency = sum(Dollars))
#Filter by Dollars
TopCompany153DollarsOverTime[order(-TopCompany153DollarsOverTime$Frequency),]

#Quantity over Time For MajorCategory 30 = CompanyID 153
TopCompany153QuantityOverTime <- 
  data_2 %>% 
  group_by(OrderQuarter) %>% 
  filter(MajorCategory == 30 & CompanyID == 153) %>% 
  summarise(Frequency = sum(Quantity))
#Filter by Dollars
TopCompany153QuantityOverTime[order(-TopCompany153QuantityOverTime$Frequency),]




#Top Company in MajorCategory 43 = CompanyID 963------------------------------------------
TopCompany43<- 
  data_2 %>% 
  group_by(CompanyID) %>% 
  filter(MajorCategory == 43) %>% 
  summarise(Frequency = sum(Quantity))
#Filter by Quantity
TopCompany43[order(-TopCompany43$Frequency),]

#Top Company by Dollars in MajorCategory 43 ------------------------------------------
TopCompany43Dollars <- 
  data_2 %>% 
  group_by(CompanyID) %>% 
  filter(MajorCategory == 43) %>% 
  summarise(Frequency = sum(Dollars))
#Filter by Quantity
TopCompany43Dollars[order(-TopCompany43Dollars$Frequency),]

#Dollars over Time For MajorCategory 43 = CompanyID 963
TopCompany963DollarsOverTime <- 
  data_2 %>% 
  group_by(OrderQuarter) %>% 
  filter(MajorCategory == 43 & CompanyID == 963) %>% 
  summarise(Frequency = sum(Dollars))
#Filter by Dollars
TopCompany963DollarsOverTime[order(-TopCompany963DollarsOverTime$Frequency),]

#Quantity over Time For MajorCategory 43 = CompanyID 963
TopCompany963QuantityOverTime <- 
  data_2 %>% 
  group_by(OrderQuarter) %>% 
  filter(MajorCategory == 43 & CompanyID == 963) %>% 
  summarise(Frequency = sum(Quantity))
TopCompany963QuantityOverTime

#Quantity over Time For MajorCategory 43 ------------------------------------------
TopCompany43OverTime<- 
  data_2 %>% 
  group_by(OrderQuarter) %>% 
  filter(MajorCategory == 43) %>% 
  summarise(Frequency = sum(Quantity))
TopCompany43OverTime

#PlOT of Quantity over Time For MajorCategory 43 = CompanyID 963
library(ggplot2)
gg_663QvT= ggplot() + 
    geom_point() + 
    geom_line(data = TopCompany43OverTime,
            aes(x=OrderQuarter, y=Frequency), color='blue') +
    geom_line(data = TopCompany963QuantityOverTime,
            aes(x=OrderQuarter, y=Frequency), color='red') +
    labs(subtitle="Overall Quantity Sold", 
          y="Quantity", x="Year", title="Major Category 43")
plot(gg_663QvT)



#Product Portfolio CompanyID 732------------------------------------------
ProdPort732<- 
  data_2 %>% 
  group_by(MajorCategory) %>% 
  filter(CompanyID == Category66Comps) %>% 
  summarise(Frequency = sum(Quantity))
#Filter by Quantity
ProdPort732[order(-ProdPort732$Frequency),]





