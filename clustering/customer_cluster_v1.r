#hierarchical clustering 
#since categorical var needs a special distance measurement method "gower", I'll skip
#categorical var for now and only use interval variable.
#data set is from sas book.

#factoextra is for clustering visualization
library(tidyverse); library(cluster);library(factoextra);
library(dendextend) #cut the dendrograph
library(sqldf);library(xlsx)
path_my <- paste("H:/01_self study_flash drive/AAAschulich",
                 "/aaa_ma/time series/R learning/r_reference/R_exercise/model compare",
                 "/develop.csv",sep="")

path_my_out <- paste("H:/01_self study_flash drive/AAAschulich",
                 "/aaa_ma/time series/R learning/r_reference/R_exercise/clustering_r",
                 "/develop3.xlsx",sep="")
data01 <- read.csv(path_my)
#change col names to lower case
names(data01) <- tolower(names(data01))
head(data01)

#add cid col to data
data02 <- cbind(cid=(1:nrow(data01)),data01)

var_keep <- paste("cid_ddabal_depamt_cashbk_checks_phone_savbal_pos_income_hmval_age_crscore",
"acctage_dep_teller_irabal_invbal_ilsbal_nsfamt_mtgbal_lores",sep="_")

data03 <- subset(data02,select=unlist(strsplit(var_keep,"_")))
data04 <- data03[complete.cases(data03),]
set.seed(12345)
select <- sample(seq(1:length(data04$age)),40,replace=F)
data05 <- data04[select,]

cus_scale01 <- data.frame(scale(data05[,2:ncol(data05)]))
cus_scale02 <- cbind(data05$cid, cus_scale01)

#repalce NaN in cus_scale01 with 0!
cus_scale02[is.na(cus_scale02)] <-0
#remove cols only have 0 value. (note how to use colSums function)
cus_scale02 <- data.frame(cus_scale02[,colSums(cus_scale02)!=0])

dis01 <- dist(cus_scale02,method="euclidean")

#elbow method to determin the optimal number of clusters. 
fviz_nbclust(cus_scale02, FUN = hcut, method = "wss")

method1 <- hclust(dis01,method="ward.D2")
#plot dendrogram, k is numher of cluster in dendrogram
plot(method1,cex=0.6,hang=-1,main="dendrogram")
rect.hclust(method1,k=5,border=2:5)
sub_grp <- cutree(method1,k=5)
table(sub_grp)
#plot 2D cluster
fviz_cluster(list(cluster=sub_grp,data=cus_scale02),labelsize =10,
             ggtheme = theme_minimal())

sub_grp2 <- cbind(cid=rownames(data.frame(sub_grp)),data.frame(sub_grp))
data_sum01 <- sqldf("select t1.*,t2.sub_grp as cluster from 
data05 t1 inner join sub_grp2 t2 on t1.cid=t2.cid")
write.xlsx(data_sum01,path_my_out)

