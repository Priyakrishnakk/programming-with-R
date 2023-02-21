#########Basics#############
1
2
3
4
5
1+2
456/34

#using c() function
vec1 = c(1,2,3)
c(1:100)

#using help
?c
help(c)

#vector creation
#method 1
vec1<-c(1:4)
vec1
#vector is extensible and can be modified
vec1[3]<-0

#vector is indexible. index starts from 1. in python, it starts from 0
vec1[1]
vec1[2]
vec1[3]

#using negative numbers for indexing
vec1[-1] #all except 1st term
vec1[-4] #all except 4th term

#accessing or subsetting specific elements of a vector
vec1[1:2]

#accessing last element of a vector
vec1[length(vec1)]

#logical indexing
vec2<-c(5:8)
sel<-c(TRUE,FALSE,TRUE,TRUE)
n<-vec2[sel]
print(n)

#conditional accessing
nums<-1:10
nums_odd<- nums[(nums%%2)!=0] #%% is a modulus operator, displays reminder after division
nums_even<-nums[(nums%%2)==0]

#vector operations
nums*2

#recycling rule
vec2<-c(5,6) 
vec1+vec2 #small vector is recycled till the operation is complete
vec1/vec2 
vec3<- c("stud1","stud2","stud3","stud4") #vector of 4 strings

#missing data
new_vec<-c(1,2,3,NA,5)
mean(new_vec)
mean(new_vec,na.rm = TRUE)

##Matrix - it is a 2 dimension vector############
##matrix creation - dimensions should be defined while creating it
marks<-matrix(data=c(23,25,22,24,21,25),nrow=2,ncol=3) #it is a 2 by 3 matrix which has 2 rows and 3 columns
marks
marks1<-matrix(data=c(23,25,22,24,21),nrow=2,ncol=3) #recycling rule applied
#row1,row2 represents student1 and student2 marks respectively

#naming columns and rows of a matrix
colnames(marks)<-c("exam1","exam2","exam3")
rownames(marks)<-c("stud1","stud2")
marks
#finding average of student 2
mean(marks[2,])
mean(marks["stud2",])
#finding mean of marks for exam2
mean(marks[,2])
mean(marks[,"exam2"])

######Lists###############
students<-c('stud1','stud2','stud3')
ages<-c(13,14,15)
classroom<-list(students,ages)
classroom

#accessing items in list
classroom[[2]]

#modifying elemets/items of list
classroom[[2]][1]<-12
classroom
ages #original vector age is not modified but only item of list is modified

#labelling list members and referencing
classroom<-list(student_names = students,student_ages=ages)
classroom$student_names
classroom$student_ages

#####dataframes###################
##Dataframes are useful data structures to represent tabular data
#like lists, they have rows and columns
#unlike lists, dataframes have defined number of rows and columns
students<-c('stud1','stud2','stud3')
ages<-c(13,14)
classroom<-list(student_names = students, student_ages=ages)
classroom
df1<-data.frame(classroom)#throws error because we have not defined the 3rd row of second column of the dataframe

#fixing the error
students<-c('stud1','stud2','stud3')
ages<-c(13,14,NA) #if there is no 3rd element, make it as NA - denotes missing value
classroom<-list(student_names = students, student_ages=ages)
classroom
df1<-data.frame(classroom)#throws error because we have not defined the 3rd row of second column of the dataframe
df1

#adding extra columns to dataframe
df1<-cbind(df1,student_marks=c(22,24,25))
df1
#adding extra rows
df2<-data.frame(student_names = 'stud4',student_ages=15,student_marks = 21)
df2
df1<-rbind(df1,df2)
df1

#accessing columns of dataframe
df1[,2]
df1[,c(1,2)]
df1[c(1,4),]
df1[which(df1$student_ages>=14),]
###############Session 2#############################
###############Inbuilt functions in R################
data() #displays inbuilt datasets
View(iris)
dim(iris) #dimensions of the dataset
str(iris) #structure of the dataset
class(iris$Sepal.Length) #checking the class of variable

summary(iris) #checking the complete detail and distribution of dataset
typeof(iris$Sepal.Length) #checking the type of variable
class(iris$Sepal.Length)

#num of levels of a factor variable
levels(iris$Species) #can be applied only on factor variable
iris3<-iris
#renaming the levels
levels(iris3$Species)<-1:3
levels(iris3$Species)
levels(iris3$Species)[1]<-"Setosa"
levels(iris3$Species)<-c("Setosa","Versicolar","Virginica")

#converting the existing datatype of a variable of a dataframe
as.numeric(dataset$variable)
iris$Sepal.Length<-as.integer(iris$Sepal.Length)
as.factor(dataset$variable)

iris3<-iris
names(iris3) #columns names of a dataframe
names(iris3)[c(1,2)]<- c("Sep.Length","Sep.Width")
names(iris3)[c(3,4)]<-c("Pet.Length","Pet.Width")
names(iris3)
##accessing data of a dataframe
iris3[2,3] # rownum,colnum
iris3[2,] #2nd row and all columns
iris3[,2] #2nd column and all rows
iris3[c(1,2,4)]

##subsetting a dataframe
iris3_subset<-iris3[c(1,2,3)] #subsetting required columns using the colnumber
iris3_subset<-iris3[-c(4,5)] #all columns except columns 4 and 5

#conditional subsetting or filtering
?subset
virginica_flowers<-subset(iris3,Species == 'Virginica') #method1
virginica_flowers<-iris3[(iris3$Species== 'Virginica'),] #method2

#multiple condition filtering
subset(iris3,Species %in% c("Virginica","Setosa"))
iris3[which(iris3$Species == 'setosa' | iris3$Species == 'virginica')] #undefined columns
iris3[which(iris3$Species == 'setosa' | iris3$Species == 'virginica'),]

#subsetting based on multiple variables
subset(iris3,Species =='Virginica' & Sepal.Width>3) #method1
iris3[which(iris3$Species=='Virginica' & iris3$Sepal.Width>3),] #method2

#sorting data
?order
iris4<-iris3[order(iris3$Sep.Length,decreasing = T),] #default is increasing
iris4[order(iris4$Species),]

#Random sampling
sample(LETTERS,5) #sampling without seed
set.seed(5)
sample(LETTERS,5) #sampling by setting the seed
samp<-sample(1:nrow(iris),25,replace = T)
sampdata<-iris4[samp,]
iris[sample(1:nrow(iris),25,replace = T),] # direct way of sampling a dataset

#############Session 3######################
#creating a new variable 
#convert Sepal.Length from cms to inches
iris4$Sepal.Length_new<-round(iris4$Sepal.Length/2.54)

#replacement of a variable values conditionally
iris4$Sepal.Length[iris4$Sepal.Length_new==3]<-NA

# iris4[which(iris4sepal.length_new==3),6]<-NA   # another method for above line


#binning a variable
#create bins - R takes the decision of bin size
iris4$Sepal.Length_cat<-cut(iris4$Sepal.Length_new,3,labels = c("Short","Medium","Tall"))
#customized bin size
iris4$Sepal.Length_cat1<-cut(iris4$Sepal.Length_new,c(1,2,3,4),c("Short","Medium","Tall"))

#concatenation
paste("z","b",sep="123")
paste("z","b",sep = ".")

#string/character operations
iris4$Species1<-substr(iris4$Species,start = 1,stop = 3)
iris4$Species2<-substring(iris4$Species,4) #from 4th character till last

#search a part of a string
grep("set",iris4$Species,ignore.case = T) 

#duplicate removal
nrow(unique(iris4))

#repeat function
text1<-rep(x=c(2,3),times=c(3,2))
text1
#overwriting the times function
text1<-rep(x=c(2,3),times=c(3,2),length=10)
text1
rep(sample(LETTERS,3),3) #here values are repeated   # important- diff b/w rep & replicate
replicate(4,sample(LETTERS,3)) #here the process is repeated

###combining and restructuring datasets########
rnorm(10) #generate 10 random numbers in a normal distribution
runif(10) #generate 10 random numbers in a uniform distribution


col1<-runif(20)
col2<-rnorm(20)
col3<-ifelse(col2<0,"negative","positive")
df1<-data.frame(col1,col2,col3)
df1


col1<-runif(20)
col2<-rnorm(20)
col3<-ifelse(col2<0,"negative","positive")
df2<-data.frame(col1,col2,col3)

rbind(df1,df2)   #rbind function is used to merge data frames by rows
col4<-runif(20)
col5<-rnorm(20)
col3<-ifelse(col5<0,"negative","positive")
df3<-data.frame(col4,col5,col3)

mer1<-merge(df1,df3) #combine 2 datasets by matching the observations according to values of common columns

####Loops in R############
##For Loop###
for (val in 1: 5)
{
  print(val)
}

##Break statement##
for (val in 1: 5)
{
  if (val == 3)
  {
    break
  }
  print(val)
}

###While loop####
val = 1

# using while loop
while (val <= 5)
{
  print(val)
  val = val + 1
}

##Apply functions
m1 <- matrix(x<-(1:10),nrow=5, ncol=6)
m1
apply(m1, 1, sum) # 2 denotes column operation while 1 is for row

#lapply - list apply - output us a list
movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
tolower(movies)
movies_lower <-lapply(movies, tolower)
movies_lower
movies_lower <-unlist(lapply(movies,tolower))
class(movies_lower)

#sapply - output is a vector or matrix
lapply(iris, min)
lapply(iris[-length(iris)], min)
x<-sapply(iris[-length(iris)], min)
class(x)


#tapply - applies the function across each group of a dataset
tapply(iris$Sepal.Width, iris$Species, median) #returns array
aggregate(iris$Sepal.Width,list(iris$Species),FUN=median) #returns dataframe
aggregate(cbind(iris$Sepal.Width, iris$Sepal.Length)~iris$Species,data=iris3,FUN=median)


##writing custom functions
add<-function(a,b){
  return(a+b)
}

shortname<-function(var){
  ans = substr(var,start = 1,stop = 3)
  return(ans)
}
iris3<-as.data.frame(iris)
iris3$short_name<-shortname(iris$Species)

#lambda like function
{function (x,y) x^y} (2,3)



unique(iris$Sepal.Length)

length(unique(iris$Sepal.Length))


names(iris)    #to know column names



#to delete columns syntax
# new_df <- subset (df, select = -c (var1, var3)) 
names(iris3)






View(Titanic)
titan=data.frame(Titanic)
table(titan$Sex)                    #shows valuecounts in sex column
table(titan$Sex ,titan$Survived)    #shows number of people survived based on sex


