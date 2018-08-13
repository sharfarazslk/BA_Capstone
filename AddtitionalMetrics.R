#Additional Metrics
#Libraries Needed

#Load Data
data_1 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/4-DMEF3YrBase.csv", colClasses=c('integer','factor'))
data_2 = read.csv("~/OneDrive - drexel.edu/2018 Spring/BUSN460/Dataset Files/DMEFLines3Dataset2.csv", as.is=T)

#Break into subset
all_categories = data_2
major_categories = subset(data_2, (data_2$MajorCategory==66 | data_2$MajorCategory==30 |
                                     data_2$MajorCategory==43))

#Year of order
major_categories$OrderDate = as.Date(as.character(major_categories$OrderDate), "%Y%m%d")
major_categories$OrderYear = as.numeric(format(as.Date(major_categories$OrderDate), "%Y"))
all_categories$OrderYear = as.numeric(format(as.Date(all_categories$OrderDate), "%Y"))

#Build Yearly Expenditure by HouseHold for all categories
all_spending = group_by(all_categories, HH_ID, OrderYear)
all_spending = summarise(all_spending, TotalExpenditure = sum(Dollars))
summary(all_spending)

#Build Yearly Expensidture by Household for top categories
top3_spending = group_by(major_categories, HH_ID, MajorCategories, OrderYear)
top3_spending = summarise(major_categories, TotalExpenditure = sum(Dollars))
summary(top3_spending)
