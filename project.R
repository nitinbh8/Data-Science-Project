
library(dplyr)
library(caret)
library(lattice)
library(ggplot2)
library(purrr)
library(factoextra)
library(caTools)
library(rsample)
library(class)
library(corrplot) #correlation plots
library(tidyverse)
library(cluster)
library(GGally)
library(plotly)
library(ROSE) #for oversampling and undersampling
library(compareDF)
Final_data = read.csv("Final_data.csv")

 

City_data = Final_data  %>% group_by(City.town) %>%
  summarise(Population=sum(Population..2016),Median_age=median(Median.age.of.the.population), 
            Average_age= mean(Average.age.of.the.population), Average_hsize = mean(Average.household.size),
            Median_income_onepersonH = median(Final_data$Median.total.income.of.one.person.households.in.2015....),
            Median_income_twopersonH = median(Final_data$Median.total.income.of.two.or.more.person.households), .groups) 
write.csv(City_data, file= "C:/Users/Nitin/OneDrive/Documents/project1/City_data.csv")

class(Final_data$Median.total.income.of.one.person.households.in.2015....)
dhcbd = median(Final_data$Median.total.income.of.one.person.households.in.2015....)

str(Final_data)

#project data
project_data = select(Final_data,-X,-X.1,-PostalCode,-MemberType,-BranchNum,
                      -BirthDate,-AgeGroup,-CreditCardStartDate,-MbrStartDate)

names(Final_data)
levels(project_data$AgeGroup)


class(project_data$LoanBalance..principle.)


project_data$LoanBalance..principle. = as.numeric(as.character(project_data$LoanBalance..principle.))

project_data$LoanBalance..principle.[is.na(project_data$LoanBalance..principle.)] = 0

hist(as.numeric(as.character(project_data$DepositBalance)))
hist(as.numeric(as.character(project_data$LoanBalance..principle.)))

View(project_data)
glimpse(project_data)
#project data_end

#productdata
product_data =  select(project_data,c(Age,DepositBalance,LoanBalance..principle.,HasWealthManagement,
                                      HasMortgage,HasCreditCard,HasPayroll,NumberProducts,TenureGroup))

levels(product_data$TenureGroup) = c(1:7)
levels(Final_data_cluster$TenureGroup)

class(product_data$TenureGroup)


View(Tenure)
age_category = cut(product_data$Age,breaks=c(0,35,50,60,90), include.lowest=TRUE,labels = c('Young_Adult','Adult','Senior','Supersenior'))
class(age_category)
table(age_category)
product_data$Age_category = age_category
product_data = select(product_data,-Age)
product_data$TenureGroup = as.numeric(product_data$TenureGroup)


product_data_O = filter(product_data[-c(97,308),]) #without outliers

dim(product_data)
class(Tenure)

View(Tenure)
#productdata_end



#levels(product_data$Age_category)= c(0,1,2)

table(product_data$Age_category)


glimpse(product_data)

#Standardization
product_data.stan = scale(product_data[,c(1:2,7:8)],center = T,scale = T)
stan_data = cbind(product_data.stan,product_data[,c(3:6,9)])
plot(product_data.stan)

#normalization
norm_minmax = function(x){
  (x- min(x)) /(max(x)-min(x))
}
product_data.norm <- as.data.frame(lapply(product_data[,c(1:2,7:8)], norm_minmax))
norm_data = cbind(product_data.norm,product_data[,c(3:6,9)])

#splitting the original data 
split = initial_split(product_data,prop = 0.7)
product_train=training(split)
product_test=testing(split)

#original data kmeans
fviz_nbclust(product_data[,1:8], kmeans, method = "wss")
fviz_nbclust(product_data_O[,1:8], kmeans, method = "wss")

fviz_nbclust(product_data[,1:8], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
fviz_nbclust(product_data_O[,1:8], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#3 is the optimal k
kmeans_raw = kmeans(product_data[,1:8], 3, nstart = 50) #k=3
kmeans_raw4 = kmeans(product_data[,1:8], 4, nstart = 50) #k=4
kmeans_raw5 = kmeans(product_data[,1:8], 5, nstart = 50) #k=5

outlier = filter(product_data[c(97,308),])

kmeans_raw_O = kmeans(product_data_O[,1:8], 3, nstart = 50) #without outlier
kmeans_raw_4O = kmeans(product_data_O[,1:8], 4, nstart = 50) #without outlier k =4
kmeans_raw_4O
kmeans_raw_5O = kmeans(product_data_O[,1:8], 5, nstart = 50) #without outlier k=5



fviz_cluster(kmeans_raw, product_data[,1:8], ellipse.type = "norm")

fviz_cluster(kmeans_raw_O, product_data_O[,1:8], ellipse.type = "norm")

fviz_cluster(kmeans_raw_4O, product_data_O[,1:8], ellipse.type = "norm")


  

aggregate(product_data[,7:8], by=list(cluster=kmeans_raw$cluster),mean)
aggregate(product_data[,1:2], by=list(cluster=kmeans_raw$cluster),mean)
aggregate(product_data[,3:8], by=list(cluster=kmeans_raw$cluster),median)
aggregate(product_data[,7], by=list(cluster=kmeans_raw$cluster),sum)

table(product_data$NumberProducts,kmeans_raw$cluster)

View(culster_data)

#Silhouette method for optimal k
sil <- silhouette(kmeans_raw$cluster, dist(product_data[,1:8]))
fviz_silhouette(sil)

sil2 <- silhouette(kmeans_raw_O$cluster, dist(product_data_O[,1:8]))
fviz_silhouette(sil2)

sil3 <- silhouette(kmeans_raw_4O$cluster, dist(product_data_O[,1:8]))
fviz_silhouette(sil3)

product_data$cluster = as.factor(kmeans_raw$cluster)

product_data_O$cluster = as.factor(kmeans_raw_O$cluster)

summary(product_data)

p = ggparcoord(data = product_data, columns = c(1:9), groupColumn = "cluster", scale = "std") + labs(x = "Variables", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)

p_O = ggparcoord(data = product_data_O, columns = c(1:9), groupColumn = "cluster", scale = "std") + labs(x = "Variables", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p_O)


#Standardization
product_data.stan = scale(product_data[,1:2],center = T,scale = T)
stan_data = cbind(product_data.stan,product_data[,3:9])
View(stan_data)

fviz_nbclust(stan_data[,1:8], kmeans, method = "wss")
fviz_nbclust(stan_data[,1:8], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

kmeans_stan = kmeans(stan_data[,1:8], 3, nstart = 50)
kmeans_stan
kmeans_stan4 = kmeans(stan_data[,1:8],4,nstart = 50)

fviz_cluster(kmeans_stan, stan_data[,1:8], ellipse.type = "norm")




#normalization model
norm_minmax = function(x){
  (x- min(x)) /(max(x)-min(x))
}
product_data.norm = as.data.frame(lapply(product_data[,1:2], norm_minmax))
norm_data = cbind(product_data.norm,product_data[,3:9])

str(norm_data)

w=fviz_nbclust(norm_data[,1:8], kmeans, method = "wss") + labs(subtitle = "Elbow method")
s=fviz_nbclust(norm_data[,1:8], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

ggpubr::ggarrange(w,s)
gridExtra::grid.arrange(w,s)  


kmeans_norm = kmeans(norm_data[,1:8], 3, nstart = 50)
kmeans_norm
kmeans_norm4 = kmeans(norm_data[,1:8], 4, nstart = 50)
kmeans_norm4
kmean_norm5 = kmeans(norm_data[,1:8],5,nstart = 50)

fviz_cluster(kmeans_norm, norm_data[,1:8], ellipse.type = "norm")

fviz_cluster(kmeans_norm4, norm_data[,1:8], geom="point",show.clust.cent = T)

product_data$cluster = as.factor(kmeans_norm$cluster)
class(product_data$cluster)

display(product_data)

sil_norm <- silhouette(kmeans_norm4$cluster, dist(norm_data[,1:8]))
fviz_silhouette(sil_norm)

Final_data_cluster = Final_data
par(mfrow=c(1,1))
p1= fviz_silhouette(sil3)
p2 = fviz_silhouette(sil_norm)
ggpubr::ggarrange(p1,p2)
gridExtra::grid.arrange(p1,p2)

Final_data_cluster$cluster = as.factor(kmeans_norm4$cluster)
Final_data_cluster$Age_category = age_category 
table(Final_data_cluster$Age_category)

aggregate(Final_data_cluster$City.town,by=list(cluster=kmeans_norm4$cluster),count(Final_data$City.town))
table(Final_data_cluster$cluster,Final_data_cluster$HasCreditCard)
table(Final_data_cluster$cluster,Final_data_cluster$HasMortgage)
table(Final_data_cluster$cluster,Final_data_cluster$City.town)

write.csv(Final_data_cluster, file  = "C:/Users/Nitin/OneDrive/Desktop/Data Science Project/Final_data_cluster1.csv")

table(Final_data_cluster$Age_category)


aggregate(product_data$TenureGroup, by=list(cluster=kmeans_norm4$cluster),mean)

`410`[1:999,]$cluster = as.factor(kmeans_norm4$cluster)

df = `410`[1:999,]

df$cluster = as.factor(kmeans_norm4$cluster)
`410` = df2
df2= `410`
str(df2)
summary(df2)
summary(Final_data)
write.csv(df, file  = "C:/Users/Nitin/OneDrive/Desktop/Data Science Project/df.csv")

rfmod4 <- randomForest(HasMortgage ~ ., data=testdf,
                       importance=TRUE, proximity=TRUE,
                       mtry=6,ntree=200)


s <- sample(1:nrow(Final_data), round(nrow(Final_data)*.8))
train_set <- Final_data[s, ]
test_set <- Final_data[-s, ]

rfover <- ovun.sample(HasMortgage~.,data = train_set, method = "over")$data
str(train_set)

rfmod4 <- randomForest(HasMortgage ~ ., data=test_set,
                       importance=TRUE, proximity=TRUE,
                       mtry=6,ntree=200)
df2 = select(df2,-SNo)

compare_df(deposit1,deposit2,"row")

deposit1 = df$DepositBalance
deposit2 = Final_data_cluster$DepositBalance

setdiff(deposit1,deposit2)
str(df2)

df2 = df2[-487,]
str(df2)

df2$cluster = as.factor(kmeans_norm4$cluster)

mean(df2$DepositBalance)
write.csv(df2, file  = "C:/Users/Nitin/OneDrive/Desktop/Data Science Project/df2.csv")

#oversampling-undersampling

str(norm_data)
table(norm_data$HasWealthManagement)
table(norm_data$HasMortgage)
table(norm_data$HasCreditCard)
table(norm_data$HasPayroll)
table(norm_data$TenureGroup)

data_balanced_over = ovun.sample(HasMortgage ~ ., data = norm_data, method = "over")$data
table(data_balanced_over$HasWealthManagement)
table(product_data$HasMortgage)

table(data_balanced_over$HasMortgage)


fviz_nbclust(data_balanced_over[,1:8], kmeans, method = "wss")
fviz_nbclust(data_balanced_over[,1:8], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


kmeans_norm_over = kmeans(data_balanced_over[,1:8], 3, nstart = 50)
kmeans_norm_over
kmeans_norm4_over = kmeans(data_balanced_over[,1:8], 4, nstart = 50)
kmeans_norm4_over


fviz_cluster(kmeans_norm, norm_data[,1:8], ellipse.type = "norm")

fviz_cluster(kmeans_norm4_over, data_balanced_over[,1:8], geom="point",show.clust.cent = T)

sil_over <- silhouette(kmeans_norm4_over$cluster, dist(data_balanced_over[,1:8]))
fviz_silhouette(sil_over)

normalized_data = ovun.sample(HasMortgage ~ ., data = Final_data, N=999, method = "over")$data

table(normalized_data$HasMortgage)
table(normalized_data$HasWealthManagement)
table(normalized_data$HasCreditCard)

aggregate(Final_data_cluster$City.town, by=list(cluster=kmeans_norm4),count)
aggregate(product_data[,1:2], by=list(cluster=kmeans_raw$cluster),mean)
aggregate(product_data[,3:8], by=list(cluster=kmeans_raw$cluster),median)
aggregate(product_data[,7], by=list(cluster=kmeans_raw$cluster),sum)

sum(Final_data_cluster$City.town)

table(Final_data_cluster$City.town)
plot(Final_data_cluster$City.town)

sum(as.numeric(Final_data_cluster$LoanBalance..principle.)/as.numeric(Final_data_cluster$DepositBalance))
class(Final_data_cluster$LoanBalance..principle.)

sum(as.numeric(Final_data_cluster$LoanBalance..principle.))/sum(as.numeric(Final_data_cluster$DepositBalance))
