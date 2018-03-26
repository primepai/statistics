#correspondence analysis of 2015 Canadian labor force survey.
#data is from Statistics Canada website. 
library(sqldf)
library(stringr)
library(xlsx)
library(tidyr)#for transform from "long" to "wide" 
library(FactoMineR) #for CA analysis. 


path_my <- paste("H:/01_self study_flash drive/AAAschulich",
                 "/aaa_ma/time series/R learning/r_reference/R_exercise/correspondence_analysis/",
                 "labor_force_survey_2015.csv",sep="")
labor01 <- read.csv(path_my)
dim(labor01)
labor02 <- subset(labor01,toupper(Geography) != "CANADA")
labor03 <- labor02[1:594,]
labor04 <- labor03[,c(1,3,4,5)]
names(labor04) <- c("location","occupation","gender","number_count")
labor04[,1] <- data.frame(as.character(labor04$location))

position <- which(labor03$X2015=="x")
numeric_col <- as.numeric(as.character(labor03$X2015))
numeric_col[position]<- 0.0
labor04[,4] <- numeric_col
#use shorter name for occupation
name <- unique(labor04$occupation)
name2 <- cbind(1:length(name),data.frame(name))
new_name_list <- paste("all/manage/senior manage/other manage/busi&finane/profess in busi/fin secre admin/",
"clerial/natural sci/health/prof health/tech health/social/social gov regli/teacher&professor/art culture/",
"sales/wholesale retail real-esta/sales clerk/chef&cook/proctect serv/childcare/sales recreation/transp equip operat/",
"contractor transp/construction/other trades/transp&equip operat/helper in transp/unique occu/unique occu in manuf/",
"machi assemble opera/labor in manuf",sep="")
new_name_list2 <- strsplit(new_name_list,"/")
name3 <- cbind(name2,new_name_list2)
names(name3) <- c("no.", "name","new_name")
temp <- as.character(name3$new_name)
# get rid of subtotal level aggregation. 
temp[c(1,2,5,9,10,13,16,17,24,30,31)] <- "subtotal"
name4 <-cbind(name2,temp)
names(name4) <- c("no.", "name","new_name")
#replace occupation name with shorter name
labor05 <- sqldf("select t1.location,sum(t1.number_count) as number_count,
t2.new_name from labor04 t1 inner join name4 t2 
on t1.occupation=t2.name where t2.new_name <> 'subtotal' group by 1,3")

prov_list <- gsub("Alberta","AB",labor05[,1])
prov_list <- gsub("British Columbia","BC",prov_list);prov_list <- gsub("Manitoba","MB",prov_list)
prov_list <- gsub("New Brunswick","NB",prov_list); prov_list <- gsub("Nova Scotia","NS",prov_list)
prov_list <- gsub("Ontario","ON",prov_list); prov_list <- gsub("Prince Edward Island","PE",prov_list)
prov_list <- gsub("Quebec","QC",prov_list);prov_list <- gsub("Saskatchewan","SK",prov_list)

labor06 <- cbind(prov_list,labor05[,2:3])
names(labor06) <- c("location","number_count","new_name")
write.xlsx(labor06, paste("H:/01_self study_flash drive/AAAschulich",
                          "/aaa_ma/time series/R learning/r_reference/R_exercise/correspondence_analysis/",
                          "labor06.xlsx",sep=""))
attach(labor06)
labor07 <- spread(labor06,key=new_name,value=number_count)

rownames(labor07)<- labor07$location
ca_labor07 <- CA(labor07[,c(-1)])
plot(ca_labor07,cex=0.7)
