# learning R: 100 lines * 100 days
# day 1: function and dataframe
CirleArea <- function(r=1){
  return (pi * r^2)
}
CirleArea()
CirleArea(2)

fibs <- function(n){
  if (n==1 | n==2){
    return(1)
  }
  else {
    return(fibs(n-1)+fibs(n-2))
  }
}
fibs(6)
for(i in 1:10){print(fibs(i))}

factorial <- function(n){
  if(n==0|n==1){
    return(1)
  } else if (n>30){
    print("I cannot caculate a factor n>30.")
    return(-2)
  } else if (n<0){
    print("a negative number's factor is not allowed.")
    return(-2)
  } else{
    return (n*factorial(n-1))
  }
}

for (i in c(-1:32)){
  print(factorial(i))
}

result=1
i = 10
while(i>0){
  result = result * i
  i = i-1
}
print(result)

n10 <- numeric(10)
typeof(n10)
class(n10)

v1 = c(seq(2,12,2))
which.min(v1)
which.max(v1)
names(v1) = c(LETTERS[1:6])
v1
which.min(v1)
which.max(v1)
names(v1)=NULL

for (i in seq(15,1,-2.3)){
  print(i)
}
rm(i)

rm(list = ls(all.names = TRUE))

#Clean Titanic data
getwd()
library(tibble)
download.file("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv","data/titanic.csv")
tb = as_tibble(read.csv("data/titanic.csv"))

c(nrow(tb),ncol(tb))
View(tb)
colnames(tb)
sapply(tb,class)
summary(tb)
# drop ID which is not informational
tb$PassengerId=NULL
head(tb)

#check NA
sum(is.na(tb)) # counts every na in the whole df
sum(complete.cases(tb))
#install.packages('Amelia')
#library(Amelia)
#missmap(tb)
#NAs
for (col in colnames(tb)){
  print(paste(col,',',sum(is.na(tb[col]))))
}
#empties
for (col in colnames(tb)){
  print(paste(col,',',sum(tb[col]=='')))
}

ggplot(tibble(tb$Age),aes(tb$Age))+geom_bar()
table(tb['Survived'])
table(tb['Sex'])
unique(tb$'Parch')
unique(tb$Cabin)
sort(table(tb$'Pclass'))
str(tb)

#drop age NAs and emptys by rows
tb[!is.na(tb$Age) & tb$Cabin!="",]

#drop rows that contain na in 1 or more columns
titanic_dropedna <- na.omit(tb)

titanic_survivor = titanic_dropedna[titanic_dropedna$Survived == 1, ]
titanic_nonsurvivor = titanic_dropedna[titanic_dropedna$Survived == 0, ]

#barchart
barplot(table(titanic_survivor$Sex),main="Surived by age")
barplot(table(titanic_nonsurvivor$Sex,main="Not surived by age"))

# Grouped Bar Plot
TempCount = table(titanic_dropedna$Sex,titanic_dropedna$Survived)
barplot(TempCount,main="Surived by age",
        xlab = "Not Survived vs Survived",
        col=c("blue","red"),
        legend = TRUE,
        args.legend = list(x = "topright", inset=c(-0.05, -0.1)))

#histogram
hist(titanic_survivor$Age,main="Surived",xlab = "Age")
hist(titanic_nonsurvivor$Age,main = "Not surived",xlab = "Age")

hist(titanic_survivor$SibSp,main="Surived",xlab = "Number of Siblings/Spouses Aboard.")
hist(titanic_nonsurvivor$SibSp,main="Not Surived",xlab = "Number of Siblings/Spouses Aboard.")

write.csv(titanic_dropedna,"data/titanic_na.csv",row.names = FALSE)
