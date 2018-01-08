library(data.table)
library(tree)

mydata <- fread(paste("https://archive.ics.uci.edu",
"/ml/machine-learning-databases/autos/imports-85.data",sep=""))
head(mydata)
mydata2 <- subset(mydata,(mydata$V2 !="?")&(mydata$V6 !="?")&(mydata$V10 !="?")&(mydata$V11 !="?")
                  &(mydata$V12 !="?")&(mydata$V13 !="?")&(mydata$V14 !="?")&(mydata$V17 !="?")
                  &(mydata$V19 !="?")&(mydata$V20 !="?")&(mydata$V21 !="?")&(mydata$V22 !="?")
                  &(mydata$V23 !="?")&(mydata$V24 !="?")&(mydata$V25 !="?")&(mydata$V26 !="?"))

# convert data type from char to factor and numeric using lapply function
mydata2[,c(3:9,15,16)] <- lapply(mydata2[,c(3:9,15,16)],factor)
mydata2[,c(10:14,17,19:26)] <- lapply(mydata2[,c(10:14,17,19:26)],as.numeric)
str(mydata2)

attach(mydata2)
set.seed(123)
train <- sample(1:nrow(mydata2),0.7*nrow(mydata2))
train_data <- mydata2[train]
test_data <- mydata2[-train]

#xname <- paste("V",c(3:17,19:25),sep="")
#tree_model <- tree(as.formula(paste("V26~",paste(xname,collapse = "+"))),train_data)
tree_model <- tree(V26~V4+V5+V6+V7+V8+V10+V11+V12+V13+V14+V15+V16+V17+V19+V20
                   +V21+V22+V23+V24+V25,train_data)
plot(tree_model)
text(tree_model,pretty=0)

tree_pred <- predict(tree_model,test_data)
mean((tree_pred-test_data$V26)^2) # result is 6662722
mean(abs(tree_pred-test_data$V26)) # mean abs error=1624 (checked with excel file 1.7.18), weka's=2624
cv_tree <- cv.tree(tree_model) 
names(cv_tree)
plot(cv_tree$size,cv_tree$dev,type="b",
     xlab="Tree Size",ylab="MSE")

pruned_model <- prune.tree(tree_model,best=12)
plot(pruned_model)
text(pruned_model)

# using linear regression
lr_model <- lm(V26~V4+V5+V6+V7+V8+V10+V11+V12+V13+V14+V15+V16+V17+V19+V20,train_data)
lr_predict <- predict(lr_model,test_data)
mean(abs(lr_predict-test_data$V26))
mean((lr_predict-test_data$V26)^2) #result is 3501887

summary(lr_model)

detach(mydata2)
write.csv(mydata2,"/Volumes/ROCK/01_self study_flash drive/AAAschulich/beyond_schulich/practice/decision tree/automobile.csv")
write.csv(test_data,"H:/01_self study_flash drive/AAAschulich/beyond_schulich/practice/decision tree/test_data.csv")
# variable name description
# 3. make: 
#   alfa-romero, audi, bmw, chevrolet, dodge, honda, 
# isuzu, jaguar, mazda, mercedes-benz, mercury, 
# mitsubishi, nissan, peugot, plymouth, porsche, 
# renault, saab, subaru, toyota, volkswagen, volvo 
# 
# 4. fuel-type: diesel, gas. 
# 5. aspiration: std, turbo. 
# 6. num-of-doors: four, two. 
# 7. body-style: hardtop, wagon, sedan, hatchback, convertible. 
# 8. drive-wheels: 4wd, fwd, rwd. 
# 9. engine-location: front, rear. 
# 10. wheel-base: continuous from 86.6 120.9. 
# 11. length: continuous from 141.1 to 208.1. 
# 12. width: continuous from 60.3 to 72.3. 
# 13. height: continuous from 47.8 to 59.8. 
# 14. curb-weight: continuous from 1488 to 4066. 
# 15. engine-type: dohc, dohcv, l, ohc, ohcf, ohcv, rotor. 
# 16. num-of-cylinders: eight, five, four, six, three, twelve, two. 
# 17. engine-size: continuous from 61 to 326. 
# 19. bore: continuous from 2.54 to 3.94. 
# 20. stroke: continuous from 2.07 to 4.17. 
# 21. compression-ratio: continuous from 7 to 23. 
# 22. horsepower: continuous from 48 to 288. 
# 23. peak-rpm: continuous from 4150 to 6600. 
# 24. city-mpg: continuous from 13 to 49. 
# 25. highway-mpg: continuous from 16 to 54. 
# 26. price: continuous from 5118 to 45400.





