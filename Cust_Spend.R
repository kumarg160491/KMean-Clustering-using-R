###################################################################
#----------- Loading Library-------------------------------------#
##################################################################
library (cluster)

###################################################################
#----------- Loading Dataset-------------------------------------#
##################################################################

cust_spend=read.csv('/home/kumar/Documents/Projects and Practices/practice/
                    R/cust_spend_analysis/Cust_Spend_Data_New.csv')
head(cust_spend)

###################################################################
#----------- Checking Null Value----------------------------------#
##################################################################

colSums(is.na(cust_spend))

summary(cust_spend[,-c(1,2)])


###################################################################
#----------- scaling Features-------------------------------------#
##################################################################

cust_spend_scaled=scale(cust_spend[,3:7],center = T,scale = T)
head(cust_spend_scaled)

###################################################################
#----------- Finding silhouette score-----------------------------#
##################################################################
k=2:10
silhouette_score=function(k){
  km<-kmeans(cust_spend_scaled,centers = k,nstart = 25)
  ss<-silhouette(km$cluster,dist(cust_spend_scaled))
  mean(ss[,3])
}


avg_sil=sapply(k,silhouette_score)

plot(k,type='b',avg_sil,frame=FALSE,xlab = 'No. of clusters',
     ylab = 'Avg Silhouette Score',main = 'Silhouette')

###################################################################
#----------- Finding WSS score------------------------------------#
##################################################################
wssplot=function(data,nc=15,seed=123){
  wss=c()
  for (i in 1:nc) {
    set.seed(seed)
    wss[i]=sum(kmeans(data,centers = i)$withinss)}
  plot(1:nc,wss,type = 'b',xlab = 'No. of cluster',
       ylab = 'within group sum of square')
}

wssplot(cust_spend_scaled,nc=5)

###################################################################
#----------- K-mean Clustering------------------------------------#
##################################################################
kmean.clus=kmeans(x=cust_spend_scaled,centers = 3)
kmean.clus


cust_spend$Cluster=kmean.clus$cluster
View(cust_spend)

aggr=aggregate(cust_spend[,-c(1,2,8)],list(cust_spend$Cluster),mean)
aggr


sil=silhouette(kmean.clus$cluster,dist(cust_spend_scaled))
head(sil[,1:3])

summary(sil)

###################################################################
#----------- Ploting cluster plot--------------------------------#
##################################################################

clusplot(cust_spend[,3:7],kmean.clus$cluster,lines = 1,color = T,shade = T)
