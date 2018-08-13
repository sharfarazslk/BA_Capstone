#Libraries Needed
library(purrr)
library(dplyr)
library(tidyr)

  ##Data neeeded----
  kmeans_1 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/kmeansresults.csv")
  kmeans_2 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/kmeansresults2.csv")
  
  
  #Get rid of extra columns
  kmeans_1 = kmeans_1[,c(2,5:14,18)]
  kmeans_2 = kmeans_2[,c(2,5:14,18)]
  write.csv(rbind(kmeans_1,kmeans_2), file="~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/kmeansresultsTotal.csv")
  
  #Cluster aligning
  kmeans_2$cluster[kmeans_2$cluster == 1] = 4
  kmeans_2$cluster[kmeans_2$cluster == 2] = 5
  kmeans_2$cluster[kmeans_2$cluster == 3] = 6
  
  kmeans_2$cluster[kmeans_2$cluster == 4] = 3
  kmeans_2$cluster[kmeans_2$cluster == 5] = 1
  kmeans_2$cluster[kmeans_2$cluster == 6] = 2
  
  #Bind together
  c1 = rbind(kmeans_1[kmeans_1$cluster==1,], kmeans_2[kmeans_2$cluster==1,])
  c2 = rbind(kmeans_1[kmeans_1$cluster==2,], kmeans_2[kmeans_2$cluster==2,])
  c3 = rbind(kmeans_1[kmeans_1$cluster==3,], kmeans_2[kmeans_2$cluster==3,])
  total = rbind(c1, c2, c3)
  
  ##Cluster Summary----
  kmeans_summary = group_by(total, cluster)
  kmeans_summary = summarise(kmeans_summary, sum(DollarsCompany), median(P_QCompany), 
                             sum(DollarsCategory), median(P_QCategory), sum(TotalSpending), count=n())
  
  
  #Women's Apparel----
  rawData = subset(data_2, (data_2$MajorCategory==66))
  rawData = inner_join(rawData, total[,c(1,14)], by= c("HH_ID"="HH_ID"))
  rawData$P_Q = with(rawData, rawData$Dollars/rawData$Quantity)
  rawData$OrderDate = as.Date(as.character(rawData$OrderDate),"%Y%m%d", origin = "1960-10-01")
  rawData$OrderDate =  format(rawData$OrderDate, "%m-%Y")
  rawData$OrderDate = as.factor(rawData$OrderDate)
  
  #Dollars Spent each cluster
  temp = rawData[(rawData$CompanyID==732),]
  temp = temp[,-c(2:3,5:6)]
  c_dollar_sum = temp %>% group_by(cluster, OrderDate) %>% summarise(Dollars = sum(Dollars))
  c_dollar_sum$cluster = as.factor(c_dollar_sum$cluster)
  
  t_dollar_sum = rawData %>% group_by(cluster, OrderDate) %>% summarise(Dollars = sum(Dollars))
  t_dollar_sum$cluster = as.factor(t_dollar_sum$cluster)
  
  ggplot(data=c_dollar_sum, aes(x=OrderDate, y=Dollars, group=cluster, color = cluster)) +
  geom_point() + geom_line()+ theme(axis.text.x=element_blank(),
                                    axis.ticks.x=element_blank())+
    labs(subtitle="Company 732", 
    y="Dollars Spent", x="Time", title="Change in Spending over Time Per Cluster")
  
  ggplot(data=t_dollar_sum, aes(x=OrderDate, y=Dollars, group=cluster, color = cluster)) +
    geom_point() + geom_line()+ theme(axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())+
    labs(subtitle="Women's Apparel", 
         y="Dollars Spent", x="Time", title="Change in Spending over Time Per Cluster")
  
  
  
  
  #Price Point
  c_PQ = temp %>% group_by(cluster, OrderDate) %>% summarise(PricePoint = median(P_Q))
  c_PQ$cluster = as.factor(c_PQ$cluster)
  t_PQ = rawData %>% group_by(cluster, OrderDate) %>% summarise(PricePoint = median(P_Q))
  t_PQ$cluster = as.factor(t_PQ$cluster)
  
  ggplot(data=c_PQ, aes(x=OrderDate, y=PricePoint, group=cluster, color = cluster)) +
    geom_point() + geom_line()+ theme(axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())+
    labs(subtitle="Company 732", 
         y="Dollars Spent", x="Time", title="Change in Price Point over Time Per Cluster")
  
  
  ggplot(data=t_PQ, aes(x=OrderDate, y=PricePoint, group=cluster, color = cluster)) +
    geom_point() + geom_line()+ theme(axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())+
    labs(subtitle="Women's Apparel", 
         y="Dollars Spent", x="Time", title="Change in Price Point over Time Per Cluster")
  
  #Count channel they order through
  channel_count = temp %>%
    gather(Channel, Val, Channel) %>% 
    group_by(cluster, OrderDate, Val) %>% 
    summarise(n= n()) %>%
    ungroup() %>%
    spread(Val, n, fill=0)
  
  
  
  ggplot(data=meltdf, aes(x=OrderDate, y=value, group=cluster, color = cluster)) +
    geom_point() + geom_line()+ theme(axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())+
    labs(subtitle="Women's Apparel", 
         y="Dollars Spent", x="Time", title="Change in Price Point over Time Per Cluster")
  
  
  