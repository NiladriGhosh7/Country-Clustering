dataset<-read.csv('Data.csv')

# 1. Use spread from the tidyr package to make 1 row for each country and two columns with 
#    GDP per capita and CO2 per capita

install.packages('tidyr')
library(tidyr)

dataset1<-t(spread(dataset,key = CountryName,value = Value))

#Alternative Aproach
dataset<-read.csv('Data.csv')
dataset<-spread(dataset,key = IndicatorName,value = Value)


# 2. Update column names to be easier to use

dataset<-setNames(dataset,c('Country','CO2','GDP'))

# 3. Remove countries which have missing

dataset<- na.omit(dataset)
# 4. Add columns with normalised variables which have a mean of 0 and a variance of 1

#Normalize to Mean 0 , Variance 1

# dataset_without_NA$GDP_Norm <- (dataset_without_NA$GDP - mean(dataset_without_NA$GDP)/sd(dataset_without_NA$GDP))
# 
# dataset_without_NA$CO2_Norm <- (dataset_without_NA$CO2 - mean(dataset_without_NA$CO2)/sd(dataset_without_NA$CO2))

dataset$gdp_norm = (dataset$GDP - mean(dataset$GDP))/sd(dataset$GDP)
dataset$co2_norm = (dataset$CO2 - mean(dataset$CO2))/sd(dataset$CO2)


# 5. Plot the elbow curve for these normalised variables to determine how many clusters to use

data_norm=subset(dataset,select = c('gdp_norm','co2_norm'))

wcss<-vector()
for (i in 1:10) wcss[i]<-sum(kmeans(data_norm,i)$withinss)
plot(1:10,wcss,type='b')

# 6. Create k-means clusters
set.seed(123)
kmeans<-kmeans(data_norm,4,iter.max = 500,nstart = 10)

# 7. Plot the k-means clusters using clusplot

library(cluster)
clusplot(data_norm,kmeans$cluster,lines = 0,shade = TRUE,color = TRUE,
         labels = 2,plotchar = FALSE,span = TRUE,main = "Cluster Of Country",
         xlab = "GDP",ylab = "CO2")


# 8: calculate average gdp, average co2 and number of countries by cluster

library(dplyr)
dataset$cluster = kmeans$cluster
dataset_summary = dataset %>% 
  group_by(cluster) %>% 
  summarise(avg_gdp = mean(GDP),
            avg_co2 = mean(CO2),
            number_countries = n())
View(dataset %>% arrange(cluster))

