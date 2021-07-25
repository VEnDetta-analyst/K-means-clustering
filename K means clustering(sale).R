download.file("https://ibm.box.com/shared/static/36ulo2vaeqyglj1dxz3b75093vdmgp5q.csv", destfile = "wholesale_customers.csv", quiet = FALSE)
sale = read.csv("wholesale_customers.csv", sep =',')
head(sale)
threshold=1.5
sale.groupChannel=NULL
sale.groupRegion=NULL
milk_mean=mean(sale$Milk)
milksd=sd(sale$Milk)
fresh_mean=mean(sale$Fresh)
freshsd=sd(sale$Fresh)
grocery_mean=mean(sale$Grocery)
grocerysd=sd(sale$Grocery)
frozen_mean=mean(sale$Frozen)
frozensd=sd(sale$Frozen)
detergent_mean=mean(sale$Detergents_Paper)
detergentsd=sd(sale$Detergents_Paper)
delicassen_mean=mean(sale$Delicassen)
delicassensd=sd(sale$Delicassen)

wssplot=function(sale,nc=15,seed=1234){
  wss=(nrow(sale)-1)*sum(apply(sale, 2, var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]=sum(kmeans(sale,centers = i)$withinss)}
  plot(1:nc,wss,type="b",xlab="no_of_clusters",ylab="within_groupss")}
wssplot(sale)
#we will select 5 clusters based on this plot
install.packages("NbClust")
library(NbClust)
set.seed(1234)
nc=NbClust(sale,min.nc = 2, max.nc = 6, method = "kmeans")
barplot(table(nc$Best.nc[1,]),xlab="no of clusters",ylab="No of criteria",main="No of clusterschosen by 6 criteria")
#Best no of clusters is 3
