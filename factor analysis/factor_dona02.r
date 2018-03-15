#factor analysis for targeting donators. data is from website. 
library(stats)
library(psy)
library(psych) #for bartlett's test 
library(corrplot)#for correlation matrix
library(xlsx)
path_my <- paste("H:/01_self study_flash drive/AAAschulich",
                 "/aaa_ma/time series/R learning/r_reference/R_exercise/factor_analysis",
                 "/donation/dona.csv",sep="")
dona01 <- read.csv(path_my)
names(dona01) <-tolower(names(dona01))
var_select <- paste("donor_age@frequency_status_97nk@income_group@last_gift_amt",
               "lifetime_avg_gift_amt@lifetime_card_prom@lifetime_gift_amount",
               "lifetime_prom@median_home_value@median_household_income@months_since_first_gift",
               "months_since_last_gift@number_prom_12@per_capita_income",
               "recent_avg_card_gift_amt@recent_response_count@wealth_rating",sep="@")
               
dona02 <- subset(dona01,select=unlist(strsplit(var_select, "@")))
#select complete cases only
dona03 <- dona02[complete.cases(dona02),]
#correlation matrix
cor(dona03)
#bartlett's test
cortest.bartlett(cor(dona03),n=1000)

#correlation matrix plot
short_name <- names(dona03)
short_name[c(2,5,6,7,9,10,11,12,15,16)]<-c("freq_97nk","life_avg_gif","life_cd_pro","life_gif_amt",

"med_home_v","med_income","month_1st_gf","month_last_gf","r_a_g_amt","r_r_ct")

factor_num <-4
fit <- factanal(dona03,factor_num,scores=c("regression"),
                rotation="varimax",lower=0.1)

print(fit,digits=2,cutoff=0.3,sort=T)

names(fit)
scree.plot(fit$correlation)




dona04 <- dona03
names(dona04) <- short_name

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(cor(dona04),method="color",col=col(200),
         type="upper",tl.col="black",addCoef.col="black",order="hclust",
         diag=F,tl.cex=0.6,number.cex=0.5)

#plot F1 vs F2
par(mar=c(5,4,3,0.5))
plot(fit$loadings[,c(1,2)],type="n", main="factor & variable plot")
text(fit$loadings[,c(1,2)],labels=names(dona04),cex=0.75,col="blue")




#export dona03 to excel
#write.xlsx(dona03,paste("H:/01_self study_flash drive/AAAschulich",
#                        "/aaa_ma/time series/R learning/r_reference/R_exercise/factor_analysis",
#                        "/donation/dona03_for_sas.xlsx",sep=""))

#get communality
commu <- 1-fit$uniquenesses
commu2 <- sort(commu,decreasing=T)



#add room for the rotated labels
par(mar = c(9, 2, 0, 0) + 1.5)   
barplot(commu2,las=2,cex.names=0.9,col="sienna1",
        ylim=c(0,1),main="communality rank")



