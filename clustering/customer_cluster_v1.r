#hierarchical clustering 
#since categorical var needs a special distance measurement method "gower", I'll skip
#categorical var for now and only use interval variable.
#data set is from sas book.

library(tidyverse); library(cluster);library(factoextra);
library(dendextend)
data01 <- read.csv(paste("H:/01_self study_flash drive/AAAschulich",
                         "/aaa_ma/time series/R learning/r_reference/R_exercise/model compare",
                         "/develop.csv",sep=""))
#change col names to lower case
names(data01) <- tolower(names(data01))
head(data01)

var_keep <- paste("ddabal_depamt_cashbk_checks_phone_savbal_pos_income_hmval_age_crscore",
"acctage_dep_teller_irabal_invbal_ilsbal_nsfamt_mtgbal_lores",sep="_")

data02 <- subset(data01,select=unlist(strsplit(var_keep,"_")))
data03 <- data02[complete.cases(data02),]
select <- sample(seq(1:length(data03$age)),40,replace=F)
data04 <- data03[select,]
cus_scale01 <- scale(data04)
#repalce NaN in cus_scale01 with 0!
cus_scale01[is.na(cus_scale01)] <-0
#remove cols only have 0 value. (note how to use colSums function)
cus_scale02 <- cus_scale01[,colSums(cus_scale01)!=0]

dis01 <- dist(cus_scale02,method="euclidean")

#elbow method to determin the optimal number of clusters. 
fviz_nbclust(cus_scale02, FUN = hcut, method = "wss")

method1 <- hclust(dis01,method="complete")
#plot dendrogram, k is numher of cluster in dendrogram
plot(method1,cex=0.6,hang=-1,main="dendrogram")
rect.hclust(method1,k=5,border=2:5)
sub_grp <- cutree(method1,k=5)
table(sub_grp)
#plot 2D cluster
fviz_cluster(list(cluster=sub_grp,data=cus_scale02))

