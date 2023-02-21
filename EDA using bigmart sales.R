#https://www.kaggle.com/caciitg/a102-big-mart-sales-caciitg-r - use this for learning
#install.packages('C:/Users/IBM_ADMIN/Downloads/httr_1.3.1.zip',repos=NULL)
require(ggplot2)
#install.packages("Hmisc",dependencies = T)
#install.packages(c("psych","glmnet","tigerstats"),dependencies = T)
require(Hmisc)
require(psych)
require(car)
require(tigerstats)
require(Metrics)
require(SparseM)
require(glmnet)

#Importing the dataset
bm.train<-read.csv("C:/Users/user/Downloads/Imarticus related/R programming/Class/EDA/bigmart sales/bigmart_train.csv",header=T,na.strings=c(""," ","NA"))
bm.test<-read.csv("C:/Users/user/Downloads/Imarticus related/R programming/Class/EDA/bigmart sales/bigmart_test.csv",header=T,na.strings=c(""," ","NA"))
orig.bm.train<-bm.train # backup of bm.train
View(bm.test)
View(bm.train)
bm.test$Item_Outlet_Sales<- as.numeric(rep(0,nrow(bm.test)))
bm.combined<-rbind(bm.train,bm.test)

#viewing the dataset
names(bm.combined) 
#"Item_Identifier"           "Item_Weight"               "Item_Fat_Content"          "Item_Visibility"           "Item_Type"                
#[6] "Item_MRP"                  "Outlet_Identifier"         "Outlet_Establishment_Year" "Outlet_Size"               "Outlet_Location_Type"     
#[11] "Outlet_Type"               "Item_Outlet_Sales"
#creating a new variable - sales as categorical variable
bm.combined$sales_cat<-cut(bm.combined$Item_Outlet_Sales,4,c("low","med","good","high"))

fix(bm.train)
str(bm.train)
summary(bm.train)
#############univariate analysis#######################
#Item_Identifier
str(bm.combined$Item_Identifier)
bm.combined$Item_Identifier<-as.character(bm.combined$Item_Identifier)
bm.combined$Item_Identifier.2chars<-substr(bm.combined$Item_Identifier,1,2)
bm.combined$Item_Identifier.2chars<-as.factor(bm.combined$Item_Identifier.2chars)
str(bm.combined$Item_Identifier.2chars)
table(bm.combined$Item_Identifier.2chars)
levels(bm.combined$Item_Identifier.2chars)<-c("DRINKS","FOOD","NON-CONSUMABLES")
ggplot(bm.combined)+aes(x= Item_Identifier.2chars,y= Item_Outlet_Sales) + stat_summary(geom ="bar") # FOOD items are sold more in number
ggplot(bm.combined,aes(Item_Identifier.2chars,Item_Outlet_Sales)) + geom_bar(stat = "identity") #displays the sales while above graph displays frequency in each group

#Item_Weight
str(bm.combined$Item_Weight)
summary(bm.combined$Item_Weight) # there are 2439 - 1463+976NAs. need to be handled
boxplot(bm.train$Item_Weight) # no outliers
prop.table(table(is.na(bm.train$Item_Weight)))*100 # 17% of NAs are there - so imputation can be standalone or based on a variable
wt<-which(is.na(bm.train$Item_Weight))
aggregate(Item_Weight~Item_Identifier.2chars,data=bm.combined,mean,na.rm=T)
#bm.train$Item_Weight[wt]<-round(mean(bm.train$Item_Weight,na.rm=T),3) # rounding to 3 decimal values and replacing missing values with mean
aggregate(bm.combined$Item_Weight,list(bm.combined$Item_Identifier.2chars,bm.combined$Item_Type),FUN=median,na.rm=T) # we can see that in Dairy products, there are foods and drinks. so, mean vary for each type.
aggregate(bm.combined$Item_Weight,list(bm.combined$Item_Identifier.2chars,bm.combined$Item_Type),FUN=mean,na.rm=T)
bm.combined$Item_Weight<-ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type== "Baking Goods",12.27521,
                                ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Breads",11.15236,
                                       ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Breakfast",13.17911,
                                              ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Canned",12.34083,
                                                     ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Dairy",13.04178,
                                                            ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Frozen Foods",12.300,
                                                                   ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Fruits and Vegetables",13.100,
                                                                          ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Meat",12.350,
                                                                                 ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Seafood",11.650,
                                                                                        ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Snack Foods",12.850,
                                                                                               ifelse(bm.combined$Item_Identifier.2chars== "FOOD" & bm.combined$Item_Type=="Starchy Foods",12.850,
                                                                                                      ifelse(bm.combined$Item_Identifier.2chars== "DRINKS" & bm.combined$Item_Type=="Hard Drinks",10.195,
                                                                                                             ifelse(bm.combined$Item_Identifier.2chars== "DRINKS" & bm.combined$Item_Type=="Dairy",14.100,
                                                                                                                    ifelse(bm.combined$Item_Identifier.2chars== "DRINKS" & bm.combined$Item_Type=="Soft Drinks",11.80,
                                                                                                                           ifelse(bm.combined$Item_Identifier.2chars== "NON-CONSUMABLES" & bm.combined$Item_Type=="Health and Hygiene",12.350,
                                                                                                                                  ifelse(bm.combined$Item_Identifier.2chars== "NON-CONSUMABLES" & bm.combined$Item_Type=="Household",13.000,14.500))))))))))))))))
                                                                                                                                         
                                                                                        


#Item_Fat_Content
summary(bm.combined$Item_Fat_Content)
bm.combined$Item_Fat_Content<-as.factor(bm.combined$Item_Fat_Content)
unique(bm.combined$Item_Fat_Content)
levels(bm.combined$Item_Fat_Content)
bm.combined$Item_Fat_Content[which(bm.combined$Item_Fat_Content=="LF"| bm.combined$Item_Fat_Content=="low fat")]<-"Low Fat"
bm.combined$Item_Fat_Content[which(bm.combined$Item_Fat_Content=="reg")]<-"Regular"
bm.combined$Item_Fat_Content<-factor(bm.combined$Item_Fat_Content) # removing unwanted levels
prop.table(table(bm.combined$Item_Fat_Content)) # 65% of low fat products and 35% of regular fat products are there
#aggregate(bm.combined$Item_Fat_Content~bm.combined$Item_Identifier.2chars,FUN = length)
table(bm.combined$Item_Fat_Content,bm.combined$Item_Identifier.2chars) # Non consumables are marked as low fat. those can be marked as none
sm<-which(bm.combined$Item_Fat_Content=="Low Fat"&bm.combined$Item_Identifier.2chars=="NON-CONSUMABLES")
bm.combined$Item_Fat_Content<-as.character(bm.combined$Item_Fat_Content)
bm.combined$Item_Fat_Content[sm]<-"NONE"
bm.combined$Item_Fat_Content<-as.factor(bm.combined$Item_Fat_Content)
table(bm.combined$Item_Fat_Content)
ggplot(bm.combined)+aes(x = Item_Fat_Content,Item_Outlet_Sales)+geom_bar(stat = "identity") # It shows that items with "low fat" have contributed to more sales then "regular fat" content products

#Item_Visibility
str(bm.combined$Item_Visibility)
summary(bm.combined$Item_Visibility)
boxplot((bm.train$Item_Visibility)) # there are some outliers
outliers<-boxplot.stats(bm.combined$Item_Visibility)$out # finding the outlier values
length(outliers) #261 values are outliers
length(bm.combined$Item_Visibility) #14204 number of values are there
(261/14204)*100 # 1.8% outliers are there 
round(quantile(bm.train$Item_Visibility,c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.97,0.98,0.99,1)),2) # quantile distribution to look deeply
(length(outliers)/nrow(bm.train))*100 # there are 3% of outliers, So, standalone imputation can be done with mean/median.
outliers
min(outliers) # minimum value of outliers list
visibility1<-bm.combined$Item_Visibility[which(bm.combined$Item_Visibility<=min(outliers))]
#visibility1<-bm.combined$Item_Visibility[which(bm.combined$Item_Visibility<0.18784108)]#0.18781108 is the 98th quantile value
summary(visibility1) # mean value without the outliers is 0.06240 - this value can be used for standalone imputation.
#boxplot(visibility1)
#outliers1<-boxplot.stats(visibility1)$out
#boxplot.stats(visibility1[which(visibility1<=min(outliers1))])$out #it confirms that there are no more outliers
bm.combined$Item_Visibility[which(bm.combined$Item_Visibility>=0.18784108)]<-0.06240# outlier values are replaced by mean value
#df2<-as.data.frame(aggregate(bm.combined$Item_Visibility,by= list(bm.combined$Item_Type),FUN=mean))
#df1<-as.data.frame(aggregate(bm.combined$Item_Outlet_Sales,by= list(bm.combined$Item_Type),FUN=mean))
#colnames(df2)<-c("Item_Type","Item_Visibility")
#colnames(df1)<-c("Item_Type","Item_Outlet_Sales")
#df3<-merge(df1,df2,x =)
#df3<-df3[order(df3$Item_Outlet_Sales,decreasing = TRUE),] # Sales increases as visibility increases except for breakfast
#cor(bm.train$Item_Visibility,bm.train$Item_Outlet_Sales)

ggplot(bm.combined)+aes(x=Item_Type,y=Item_Visibility)+ facet_wrap(~Item_Fat_Content) + geom_boxplot()
ggplot(bm.combined)+aes(x =Item_Type,y=Item_Visibility)+geom_boxplot()
quantile(bm.combined$Item_Visibility, c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,.95,.96,.97,.98,.99,1))
quantile(bm.combined$Item_Visibility, c(.9,.95,1))
outliers<-boxplot.stats(bm.combined$Item_Visibility)$out

#Item_Type
summary(bm.combined$Item_Type)
bm.train$sales_cat<-cut(bm.train$Item_Outlet_Sales,breaks = c(33.29,834.25,1794.33,3101.3,13086.97),c("low","med","good","high"))
sales_type_tab<-table(bm.train$sales_cat,bm.train$Item_Type)
barplot(sales_type_tab,legend.text = T) # verifying if any particular type of product is being sold the most - all the items are almost equally sold
prop.table(table(bm.train$Item_Type,bm.train$sales_cat))*100
item_sales_sum<-as.data.frame(aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Item_Type),FUN=sum)) # seafood are sold the least and fruits and vegetables are sold the most
item_sales_sum<-item_sales_sum[order(item_sales_sum$x,decreasing = T),]
item_sales_avg<-aggregate(x = bm.train$Item_Outlet_Sales,by=list(bm.train$Item_Type),FUN = mean)
item_sales_avg<-item_sales_avg[order(item_sales_avg$x,decreasing = T),]

#Item_MRP
summary(bm.train$Item_MRP)
boxplot(bm.train$Item_MRP) # No Outliers
bm.train$Item_MRP.cat<-cut(bm.train$Item_MRP,breaks = c(31.29,93.83,143.01,185.64,266.89),c("cheap","ok","costly","very costly"))
barplot(table(bm.train$Item_MRP.cat,bm.train$sales_cat),legend.text = T)# check if mrp of product effects the sales - graph shows that if sale is less, it is because mostly cheaper items are involved. if sale is more, costly items are involved.
ggplot(bm.train)+aes(x=Item_MRP,y=Item_Outlet_Sales)+geom_point()
plot(Item_Outlet_Sales~Item_MRP,bm.train) # in the plot, cheaper items are densly populated and costly items are scarcely populated - it means that cheaper items are sold more. however, they
df<-as.data.frame(aggregate(bm.train$Item_MRP,list(bm.train$sales_cat),FUN=sum)) #this confirms in figures that cheaper items are making less total sale amount


#Outlet identifier
summary(bm.train$Outlet_Identifier)
str(bm.train$Outlet_Identifier)
length(unique(bm.train$Outlet_Identifier)) # there are 10 bigmart stores
bm.sales<-aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Outlet_Identifier),FUN=sum)
colnames(bm.sales)<-c("Outlet_Identifier","total_sales")
bm.sales[order(bm.sales$total_sales,decreasing = T),]
barplot(table(bm.train$Outlet_Identifier))
ggplot(bm.sales)+aes(x=total_sales)+geom_bar() #outlet 10 and 19 look to be less records
barplot(table(bm.train$sales_cat,bm.train$Outlet_Identifier),legend.text = T,beside = T)
# 1. out10 and out19 stores are having less sales - they are grocery stores
# 2. other outlets are having high sales 

#Outlet_Establishment_Year
summary(bm.train$Outlet_Establishment_Year)
str(bm.train$Outlet_Establishment_Year)
length(unique(bm.combined$Outlet_Establishment_Year)) # 10 outlets are established in 9 different years
bm.train$Outlet_Establishment_Year<-as.factor(bm.train$Outlet_Establishment_Year)

#older store has more sales
aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Outlet_Establishment_Year),FUN=sum)
#In 1985, Grocery store started has low sales while super market had more sales
table(bm.train$Outlet_Type,bm.train$Outlet_Establishment_Year)
aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Outlet_Establishment_Year,bm.train$Outlet_Identifier),FUN=sum) 
table(bm.train$Outlet_Identifier,bm.train$Outlet_Establishment_Year)
#1. In year 1985, 2 outlets are established - OUT19 and OUT27
#2. outlet 10, established in 1998 has low sales
#3. outlet 27, established in year 1985 has highest sales

#Outlet_size
summary(bm.train$Outlet_Size) # there are 2410 NA's
str(bm.train$Outlet_Size)
bm.train$Outlet_Size<-as.factor(bm.train$Outlet_Size)
table(bm.train$Outlet_Size) # here NAs are not considered as a seperate level.
#bm.train$Outlet_Size<-addNA(bm.train$Outlet_Size) # making NA to be considered as a new level
#bm.train$Outlet_Size<-factor(bm.train$Outlet_Size) # removing NA as a seperate level
table(bm.train$Outlet_Size,bm.train$Outlet_Identifier) # this shows that stores OUT10 and OUT17, out45 do not have their sizes identified
table(bm.train$Outlet_Type,bm.train$Outlet_Size)
OUT010 -
  OUT013 - high 
OUT017 -
  OUT018 - medium
OUT019 - small
OUT027 - medium
OUT035 - small
OUT045 - 
  OUT046 - small
OUT049 - medium
num<-which(is.na(bm.train$Outlet_Size)) # finding the observations whose outlet size is NA (not known)
bm.train1<-bm.train
for (i in num) {
 if(bm.train[i,11] == "Grocery Store"){
    bm.train[i,9] <- "Small"   
   }
}

for (i in num) {
  if(bm.train[i,11] == "Supermarket Type2"|bm.train[i,11] =="Supermarket Type3"){
    bm.train[i,9]<- "Medium"   
  }
}

#Outlet_Location_Type
summary(bm.train$Outlet_Location_Type)
str(bm.train$Outlet_Location_Type)
aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Outlet_Location_Type),FUN=sum) # location tier3 has more sales, location tier1 has low sales
barplot(table(bm.train$Outlet_Location_Type,bm.train$sales_cat),legend.text = T,beside = T) #tier1 located outlets have low sales
#cor(bm.train$Outlet_Location_Type,bm.train$Item_Outlet_Sales)
table(bm.train$Outlet_Identifier,bm.train$Outlet_Location_Type)# tier1 has 3 stores, tier2 has 3 stores, tier3 has 4 stores
table(bm.train$Outlet_Type,bm.train$Outlet_Size)

#Outlet_Location_Type
summary(bm.train$Outlet_Location_Type)
str(bm.train$Outlet_Location_Type)
aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Outlet_Location_Type),FUN=sum) # location tier3 has more sales, location tier1 has low sales
#barplot(table(bm.train$Outlet_Location_Type,bm.train$sales_cat),legend.text = T,beside = T) #tier1 located outlets have low sales
#cor(bm.train$Outlet_Location_Type,bm.train$Item_Outlet_Sales)
table(bm.train$Outlet_Identifier,bm.train$Outlet_Location_Type)# tier1 has 3 stores, tier2 has 3 stores, tier3 has 4 stores
#Item_outlet_type
summary(bm.train$Outlet_Type) # there are more number of transactions done in super market type1 because they are more in number of records
table(bm.train$Outlet_Type)
table(bm.train$Outlet_Type,bm.train$Outlet_Size,bm.train$Outlet_Location_Type)
# Supermarket type1 is big size in tier3, small in tier2, small and medium size in tier1
# supermarket type2 and 3 are of medium sizes in tier 3 location alone
# grocery stores are always small in size irrespective of their location

#in Tier3, Supermarket Type1 has High size
#in Tier2, Supermarket Type1 has Small size
#in Tier1, Supermarket Type1 has Medium and Small size


for (i in num) {
  if(bm.train[i,10] == "Tier 3"&bm.train[i,11] == "Supermarket Type1"){
    bm.train[i,9]<-"High"
  }
}

for (i in num) {
  if(bm.train[i,10] == "Tier 2"&bm.train[i,11] == "Supermarket Type1"){
    bm.train[i,9]<-"Small"
  }
}


####################bivariate analysis#############################
#checking item sales, mrp and product wise
s<-as.data.frame(aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Item_MRP,bm.train$Item_Type),FUN=sum))
#on closly monitoring the sales and MRP product wise, for few products, we can see that MRP is higher then the item sales
f<-which(bm.train$Item_MRP>bm.train$Item_Outlet_Sales) # finding the observation number which has item_mrp>item_outlet_sales
bm.train$Item_Outlet_Sales[f]<-bm.train$Item_MRP[f] # replacing item_outlet_sale with item_mrp

#is there any change in mrp of a same product in different stores?
names(bm.train)
l1<-subset(bm.train,select = c(1,6,9,11)) # selected item identifier, mrp, outlet size, outlet type respectively
l1<-l1[order(l1$Item_Identifier),] # same product is different prices in different outlet types and outlet sizes
#is the sales effected because of product pricing in different stores?
l2<-subset(bm.train,select = c(1,6,9,10,11,12))# selected item identifier, mrp, outlet size, outlet type, outlet sales respectively
l2<-l2[order(l2$Item_Identifier),]


#outlet type, outlet location, sales
aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Outlet_Type,bm.train$Outlet_Location_Type),FUN=sum)
#1.Grocery store in Tier 3 is making more sales then the Grocery store in Tier1 location
#2.Super market type1 is making sales in the order of TIER2>TIER1>TIER3
#3. Super market type2 and 3 are present only in TIER 3 so, we cannot conclude about them.
# We can consider outlet location to build model for now.

#Outlet establishment year, outlet type, sales
aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Outlet_Establishment_Year,bm.train$Outlet_Type),FUN=sum)
# there is no trend observed for sales when outlet established year and outlet type are taken into consideration.
# Outlet establishment year seems like doesnt effect the sales. lets try to see the sales by including store location also 
# to identify if establishment year has any effect on outlet sales
aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Outlet_Establishment_Year,bm.train$Outlet_Location_Type,bm.train$Outlet_Type),FUN=sum)
#On looking into sales based on outlet location, year of establishment, it seems like year of establishment is not effecting the sales
# So, we can ignore establishment year for model building

# checking if an item is sold more if the size of the store changes-item identifier, outlet type, outlet size
g<-as.data.frame(aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Item_Identifier,bm.train$Outlet_Type,bm.train$Outlet_Size),FUN=sum))
g<-g[order(g$Group.1),] # sorting based on item identifier
# it shows that people do not bother about the store size to purchase an item because
# an item if found in store of any size is not having any trend in sales based on outlet size

#outlet type, outlet size and sales
aggregate(bm.train$Item_Outlet_Sales,list(bm.train$Outlet_Type,bm.train$Outlet_Size),FUN=sum)
#this shows that a small size super market is making more sales than medium and big(high) sizes stores - for super markettype1 alone
#grocery stores are always small, supermarket type2 and supermarket type3 are always medium in size.
# this gives a picture that outlet size is not an important variable to consider for model building

# checking if the sale of a particular product is more based on the location of the store
ver1<-aggregate(bm.train$Item_Identifier,bm.train$Outlet_Location_Type,bm.train$Item_Outlet_Sales),FUN=sum)

#Working with missing values
colSums(is.na(bm.train)) # missing values present in Item_Weight and Outlet_Size

#impute  missing values - since their presence/absence doesnt effect the analysis
require(missForest)
install.packages("missForest",dependencies = TRUE)
bm.train<-bm.train[-1]
bm.train_nona<-missForest(bm.train)
bm.train$Item_Fat_Content<-as.factor(bm.train$Item_Fat_Content)
bm.train$Item_Type<-as.factor(bm.train$Item_Type)
bm.train_nona$ximp<-bm.train1
View(bm.train1)
##############################################################################################################
#            Old analysis
##############################################################################################################
#Univariate analysis

require(tigerstats)
bm.train.cat<-bm.train[c(1,3,5,7,9,10,11)]
bm.train.num<-bm.train[-c(1,3,5,7,9,10,11)]

#analyzing Item_Fat_Content
table(bm.train$Item_Fat_Content)# levels of Item_Fat_Content seems to be of different cases 
plot(bm.train$Item_Fat_Content)

#Analyzing Outlet_Size
rowPerc(table(bm.train$Outlet_Size))
plot(bm.train$Outlet_Size)#Medium sized stores are more in number(45.69%)

#Analyzing Outlet_Location_Type
rowPerc(table(bm.train$Outlet_Location_Type))
plot(bm.train$Outlet_Location_Type)#Outlets are more located in location type Tier3 (39.31%) and in location type Tier1 are less in number(28.02)

#Analyzing Outlet_Type
require(tig)
rowPerc(table(bm.train$Outlet_Type))
plot(bm.train$Outlet_Type)# Outlets of type supermarket type1 are more (65.43%)

#univariate analysis of numeric variables
summary(bm.train.num) # gives complete overvie of the numeric variables

#Outlet establishment year
bm.train$Outlet_Establishment_Year<-as.factor(bm.train$Outlet_Establishment_Year)
x<-aggregate(bm.train$Item_Outlet_Sales, by= list(bm.train$Outlet_Establishment_Year), FUN=sum)
y<-unique(bm.train$Outlet_Establishment_Year)

#multivariate analysis
#1.between categoric variables

#1.Analyzing Item_Weight
table(bm.train$Item_Weight)
weight.cat<-cut(bm.train$Item_Weight,5)
#######################################################################################################
#                 Variable Selection
#######################################################################################################
bm.model1<-lm(Item_Outlet_Sales~.,data = bm.select)
cutoff<-4/(nrow(bm.select)-length(bm.model1$coefficients)-2)
plot(bm.model1,which = 4,cook.levels = cutoff)
plot(bm.model1,which = 5,cook.levels = cutoff)
bm.select<-bm.select[-which(row.names(bm.select)==c("1255","6514")),]
vif(bm.model1)

bm.train2<-bm.train[-1]
rf<-randomForest(Item_Outlet_Sales~.,data = bm.train,oob.prox=T)
varImpPlot(rf) #Outlet_location type, outlet_size, Item_fat_content seem to be less significant and item_MRP is most significant variable
importance(rf)
bor1<-Boruta(Item_Outlet_Sales~.,data = bm.train,doTrace=2)
print(bor1)
#Boruta performed 99 iterations in 11.14275 mins.
#8 attributes confirmed important: Item_MRP, Item_Visibility, Item_Weight, Outlet_Establishment_Year,
#Outlet_Identifier and 3 more;
#1 attributes confirmed unimportant: Item_Fat_Content;
#1 tentative attributes left: Item_Type;
lm1<-lm(Item_Outlet_Sales~.-Item_Fat_Content,data = bm.train)

bm.train1<-subset(bm.train,select = c(1:11))
rf2<-randomForest(Item_Outlet_Sales~.,data = bm.train2)
varImpPlot(rf2)

#using linear regression to further identify the significant variables
#recoding the factor levels as numeric variables

levels(bm.train1$Item_Type)<-c(1:16)
bm.train1$Item_Type<-as.numeric(bm.train1$Item_Type)
levels(bm.train1$Outlet_Identifier)<-c(1:10)
bm.train1$Outlet_Identifier<-as.numeric(bm.train1$Outlet_Identifier)
levels(bm.train1$Item_Fat_Content)<-c(0,1)
bm.train1$Item_Fat_Content<-as.numeric(bm.train1$Item_Fat_Content)
levels(bm.train1$Outlet_Size)<-c(1:3)
bm.train1$Outlet_Size<-as.numeric(bm.train1$Outlet_Size)
levels(bm.train1$Outlet_Location_Type)<-c(1:3)
bm.train1$Outlet_Location_Type<-as.numeric(bm.train1$Outlet_Location_Type)
levels(bm.train1$Outlet_Type)<-c(1:4)
bm.train1$Outlet_Type<-as.numeric(bm.train1$Outlet_Type)
str(bm.train1)

#checking normal distribution of variables and linear relationship of the variables and target variables
pairs.panels(bm.train2) # all the variables are not normally distributed
normalize<-function(x){
  z<-(x-min(x))/(max(x)-min(x))
  return(z)}
bm.train1_norm<-as.data.frame(lapply(bm.train1[,1:10], normalize))
bm.train1_norm<-cbind(bm.train1_norm,bm.train1$Item_Outlet_Sales)
#give fix(bm.train1_norm) and correct the column name "bm.train1$Item_Outlet_Sales" to "Item_Outlet_Sales"
fix(bm.train1_norm)

lm1<-lm(Item_Outlet_Sales~.,data = bm.train1_norm)
summary(lm1)

#dropping one non-significant variable at each step and observing the R square value
bm.train1.1_norm<-subset(bm.train1_norm,select=c(2:11))
lm1.1<-lm(Item_Outlet_Sales~.,data = bm.train1.1_norm)
summary(lm1.1)

bm.train1.2_norm<-subset(bm.train1.1_norm,select=c(2:10))
lm1.2<-lm(Item_Outlet_Sales~.,data = bm.train1.2_norm)
summary(lm1.2)

bm.train1.3_norm<-subset(bm.train1.2_norm,select=c(1,3,4,6,7,8,9))
lm1.3<-lm(Item_Outlet_Sales~.,data = bm.train1.3_norm)
summary(lm1.3)

#after removing non-significant factors, R-square value doesn't seem to increase much.
step<-step(lm1)

require(car)
crPlots(lm1.3) # checking for linearity.( error line(red line) is near to green line) if good, go further 

#removing outliers
summary(lm1.4)
cutoff<-4/(nrow(bm.train1.4_norm)-length(lm1.4$coefficients)-1)

plot(lm1.4,which = 5, cook.levels = cutoff)
plot(lm1.4,which = 4, cook.levels = cutoff)
bm.train1.4_norm<-bm.train1.4_norm[-which(rownames(bm.train1.4_norm) %in% c("4290")),]
lm1.4<-lm(Item_Outlet_Sales~.,data=bm.train1.4_norm)
summary(lm1.4)

#####function to identify outliers######
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("\nPropotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("\nMean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("\nMean without removing outliers:", round(m1, 2), "n")
  cat("\nMean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}


