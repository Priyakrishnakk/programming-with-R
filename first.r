1
9:1
10:12
vec1<-c(1:5)    # assigning values foe variable
vec1
vec1[3]<-0      #change number using index position
vec1

vec1[-1]      # all values except first value    # delets first number
vec1[-4]      # all numbers except fourth number   # delets fourth number
vec1[1:2]     # prints first and second number
vec1[-2:-3]   # delets 2nd and 3rd number
vec1[c(1,4)]  # prints 1st and 4th number

?c            # shows package details
help(c)       # same as above to see package details
vec1[length(vec1)]          # access last element


#logical indexing
vec2<-c(5:8)
vec2
n<-vec2[se1]
n


#conditional accessing

nums<-1:10
nums
nums_odd<- nums[(nums%2)!=0]  # odd numbers
nums_even<-[(nums%2)==0]


#vectoe opeartions

nums*2


#recycling rue

vec2<-c(5,6)
vec1+vec2      # vec1 is five numbers vec2 is only two... vec2 turns as 5 6 5 6 5
vec1
  
vec1*vec2

vec3<-c("studio1","studio2","studio3","studio4")   # string values


#missing values
new_vec<-c(1,2,3,NA,5)
mean(new_vec)
mean(new_vec,na.rm= TRUE)


marks<-matrix(data=c(22,25,23,21,24,23),nrow=2,ncol=3)   # fills values up and down
marks

colnames(marks)<-c("exam1","exam2","exam3")
marks

rownames(marks)<-c("maths","science")
marks

mean(marks[2,])    #rows,columns
mean(marks["maths",])




marks2<-matrix(data=c(22,25,23,21,24,23),nrow=2,ncol=3, byrow=TRUE)
marks2         # first fills values in rows and then in columns


marks2[2,1]<-40     # changes second row first value
marks2

marks2[2,3]<-NA
mean(marks2,na.rm= TRUE)
marks2



students<-c("study1","stidy2","stidy3")
ages<-c(13,15,12)
classroom<-list(students,ages)
classroom


classroom<-list(student_names=students, students_ages=ages)
classroom$student_names
classroom$students_ages


df1<-data.frame(classroom)
df1




students1<-c("study1","stidy2","stidy3")
ages1<-c(13,15)
classroom1<-list(students1,ages1)
classroom1

df2<-data.frame(classroom1)
df2   #  datafrane cannot create beacuse it has only two elements in ages



students1<-c("study1","stidy2","stidy3")
ages1<-c(13,15,NA)
classroom1<-list(students1,ages1)
classroom1

df2<-data.frame(classroom1)
df2


df1<-cbind(df1,students_marks=c(22,24,25))
df1


data()
View(iris)
dim(iris)



iris3<-iris
names(iris3)
names(iris3)[c(1,2)]<-c("sep.length","sep.width")
names(iris3)[c(3,4)]<-c("pet.length","pet.width")
names(iris3)


iris3[2,3]

iris3[c(1,2,4)]


iris3[,2]


virgina_flowers<-subset(iris3,Species=='virginica')
virgina_flowers


iris3[iris3$Species=='virginica',]

iris3[iris3$Species=='virginica',3]


iris3[iris3$Species=='setosa',]
iris3[which(iris3$Species=='virginica' & iris3$sep.width>1),]


?subset



iris3$sep.length_new <-round(iris3$sep.length/2.54)
iris3$sep.length_new


iris3$Sepal.Length_cat<-cut(iris3$Sepal.Length_new,3,labels = c("Short","Medium","Tall"))


iris3$sep.width_cat <- cut(iris3$sep.width,3,labels = c("small","med","tall"))

str(iris3)


iris3$sep.width_cat


iris3$sep.width_cat <- cut(iris3$sep.width,2,labels = c("small","med"))
iris3$sep.width_cat


unique(iris3$sep.length)


#string/character operations
iris4$Species1<-substr(iris4$Species,start = 1,stop = 3)
iris4$Species2<-substring(iris4$Species,4) #from 4th character till last



iris3$Species1<-substr(iris3$Species, start =1, stop =3)
iris3$Species1


names(iris3)

#to delete columns syntax
# new_df <- subset (df, select = -c (var1, var3)) 
names(iris3)





grep("set",iris3$Species,ignore.case = T) 



?c
