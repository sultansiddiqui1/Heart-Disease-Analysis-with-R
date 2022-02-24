library(datasets)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse) 
pacman::p_load(dplyr)
install.packages("ggplot2")
library(ggplot2)

data<-read.csv("/home/sadiq/Desktop/heart.csv") #readind data downloaded from keagle that is in a csv format
data
head(data)#displaying the first few data to understand it better
tail(data)#displaying the last 6 entries to understand it better

#getting a rough idea of the data:

summary(data)

# getting a summary of the data set.
#few things that we can understand from the summary of the data:
#if we take the cholesterol values, we see that there is a huge difference between the minimum value and the maximum
#value. this tell us that there is a huge range and median value is 240, so this also tells us about the huge range 
#between the minimum and the maximum value. We can also figure that there is a good probability for the existence of 
#outliers in our data set.
#on the other hand if we take the column thalach(which stand for the thallium test), we can get an idea that the patients 
#ssufering from heart disease have very high thallium values and high cholesterol values as well.

ncol(data)# finding out the total number of coloumns in our data set
nrow(data)#finding out the total number or rows in our data set
colnames(data)#finding out the diffrent columns, and what they represent.


#data transformation: mutating data
data2 <- data %>%
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"), #recognizing the data as 1 as male and 0 as female, as our data is binary.
         fbs = if_else(fbs == 1, ">120", "<=120"), #fbs, or fasting blood sugar, it it is 1 then it is greater than 120 otherwise it is less than 120. 
         # we do the same thing for variopus values, so that data vusalization become easier in the future.
         exang = if_else(exang == 1, "YES" ,"NO"),
         cp = if_else(cp == 1, "ATYPICAL ANGINA",
                      if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg == 0, "NORMAL",
                           if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target == 1, "YES", "NO")# if target is one that means u have got a heart disease.
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())#manipulating the variables that we have written above.

data2  

#data visualization::
#barplot for targets, makes it easier to see and understand the data.

ggplot(data2,aes(x=data2$target,fill=data2$target))+
  geom_bar()+# the goem  just tell us what kind of bar we want, ion this case we wabnt a bar chart.
  xlab("Heart Disease")+
  ylab("number")+
  ggtitle("Heart Disease Detection overview")+
  scale_fill_discrete(name="heart disease", labels =c("Absence", "Presence"))


#as visible from the bar plot, the proportion of the people having a heart disease is more
#than the number of people that do not have a hear disease.

#getting the exact ratio:

prop.table(table(data2$target))
# we see that the data is not very biased, but rather pretty balanced, as the ratio
# of not having:having is 45:55 approximately, but bent towards people that have heart diseases.

#counting the frequency of the values of age.(basically exploring the age variable)::
head(data2)

data2 %>%
  group_by(age) %>%
  count() %>%
  filter(n>10) %>%
  ggplot()+
  geom_col(aes(age, n), fill = 'blue')+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("count")

#result of age analysis:
#most of the people are in the range of 55-60, people who are more elderly. the data set is tilted 
#towards elderly people rather than people int he younger side. this maybe because it is believed that the issues with the 
# heart normally start after a certain age

#compare blood pressure across the chest pain type:
head(data2) #getting a view of the data and teh column names

data2 %>%
  ggplot(aes(x=sex, y=trestbps))+#trestbps is the blood pressure
  geom_boxplot(fill="orange")+
  xlab("sex")+
  ylab("Blood Pressure")+
  facet_grid(~cp)


#analysis result:
# the first observation that we can make is the fact that in males we see a lot of outliers,
# especially in the atypical angina chest pain. we can also clearly see that the blood pressure is
#more consistent in females than in the males, as for males the blood pressure is almost going for 190 which is quite high,
#which also means it is more painful more the males then for the females because of the high blood pressure.


#analyzing cholestrol levels in males and females according to the chest pain.

head(data2)
data2 %>%
  ggplot(aes(x=sex, y=chol))+#chol stands for cholesterol.
  geom_boxplot(fill="brown")+
  xlab("sex")+
  ylab("cholestrol level")+
  facet_grid(~cp)

#result of the analysis:
#interpreting the type of chest pain based on  the cholesterol level. the cholesterol level as 
#we can see is more for the females. the skewness lines are also longer for the females than for the males.
#this result is differtent that the above one for the blood pressure.

#correlation:

install.packages("corrplot")
install.packages("ggplot2")
library(corrplot)
library(ggplot2)

cor_heart<-cor(data2[,10:14])
cor_heart
corrplot(cor_heart,method="circle",type="lower")

# with the help of the colors, this graph tells us which of the features are really correlated and which are not.
# as we see from the graph, the navy blue, dark blue color represents highly co-related.

