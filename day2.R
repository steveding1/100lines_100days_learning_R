# learning R: 100 lines * 100 days
# day 2: more dataframe factor and ggplot
# ningding100@gmail.com
set.seed(10)
runif(100, - 5, 10)

toupper("hello world")
library(ggplot2)
library(tibble)
titanic_na = tibble(read.csv("data/titanic_na.csv"))
colnames(titanic_na)
sum(is.na(titanic_na)) # is there any na content

as.factor(titanic_na$Survived)
titanic_na$Pclass = 
  factor(titanic_na$Pclass, order=TRUE, levels = c(3, 2, 1))

cut(titanic_na$Age, c(0,10,20,30,40,50,60,70,80,100))

titanic_na1 = titanic_na
mutate(titanic_na1,AgeRange="")
titanic_na1[titanic_na1$Age<=10,'AgeRange']<-'preteen'
titanic_na1[titanic_na1$Age>10&titanic_na1$Age<=35,'AgeRange']<-'youth'
titanic_na1[titanic_na1$Age>35&titanic_na1$Age<=55,'AgeRange']<-'midage'
titanic_na1[titanic_na1$Age>55,'AgeRange']<-'senior'
titanic_na1$AgeRange
age = c('preteen','youth','midage','senior')
titanic_na1$AgeRange=factor(titanic_na1$AgeRange,levels = age,ordered=TRUE)

str(titanic_na1) # Pclass and AgeRange has become a 'Ord.factor'

library(RColorBrewer)
barplot(table(titanic_na1$AgeRange,titanic_na1$Survived),
        las=1,
        names.arg = c("Not Survd","Survived"),
        col = brewer.pal(4, "Set2"),
        horiz = T,
        legend = TRUE,
        args.legend = list(x='topright', inset=c(0,-0.7)))
        
survivor <- titanic_na[titanic_na$Survived==1,]
survivor$Survived=NULL
nonsurvi <- titanic_na[titanic_na$Survived==0,]
nonsurvi$Survived=NULL

TempCount <- table(titanic_na$Sex,titanic_na$Survived)
barplot(TempCount,
        col = c("dark blue","red"),
        las=1,
        horiz = T,
        names.arg = c("Not Survd","Survived"),
        font.axis=1,
        legend=TRUE,
        args.legend = list(x='topright', inset=c(0,-0.2)))

#ways to select columns
library(dplyr)
select(titanic_na,c("Age","Sex","Survived"))

titanic_na %>%
select_if(is.character)

TempCount <- survivor[,c("Age","Sex")]
TempCount %>%
  ggplot( aes(x=Sex, y=Age),names.arg = c("Not Suvd","Suvived")) +
  geom_boxplot(col = c("dark blue","red")) +
  geom_jitter(color="black", size=0.2, alpha=0.9) +
  ggtitle("Suvivors by Age and Genders - boxplot with jitter") 

#Total No. of each sex
sample_size = survivor%>%group_by(Sex) %>%summarise(num=n())
TempCount %>%
  left_join(sample_size) %>%
  #mutate is for adding cols 
  #whereas transmute will only keep the new col!
  mutate( myaxis = paste0(toupper(Sex), "\n(Total ", num,")")) %>% 
  ggplot( aes(x=myaxis, y=Age ,fill=Sex)) +
  geom_violin() +
  geom_boxplot(width=0.3,alpha=0.2) +
  theme(legend.position="none")# Remove legend
  
#Survival by Pclass
ggplot(titanic_na1,aes(x=Survived,fill=Pclass)) +
  geom_bar(position = position_dodge())+
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_classic()

#Density plot
ggplot(titanic_na,aes(x=Age))+
  geom_density(fill='orange')

View(titanic_na1)
write.csv2(titanic_na1,'data/titanic_na1.csv2')

#scatter
#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
iris.df=tibble(iris)
dim(iris.df)
ggplot(iris.df,aes(x=Sepal.Length, y=Sepal.Width, col=Species,size=Petal.Length))+
  geom_point(aes(shape=Species))+
  geom_smooth(method=lm, se=FALSE)+ 
  stat_ellipse()

ggsave('iris_scat.jpg',dpi=300)
