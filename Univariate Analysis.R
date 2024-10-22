install.packages("tidyverse") # install all required packages
library(tidyverse)    # loading all packages
library(data.table)



# Read the .dat file (skip the first 3 lines)
data <- read.table("C:\\Datasets\\adult\\adult.data", header = TRUE)


#Export the data to a CSV file
write.csv(data, "C:\\Users\\wankh\\Desktop\\Datasets\\adult\\adult.csv", row.names = TRUE)

# To read the csv file to DATA FRAME
df <- read.csv("C:\\Users\\wankh\\Desktop\\Datasets\\adult\\adult.csv", check.names = TRUE)
view(df)

#======================== FREQ DISTRIBUTION BY AGE (THE INTERVAL VARIABLE)=============================

freq_table_age<- table(df$age)    # table() generates frequency table
#print(freq_table_age, row.names = TRUE)
#cbind(freq_table_age)

set.seed(1)

cumfreq_age <- cumsum(freq_table_age)      # cumsum() for cumulative frequencies

#print("Cumulative Freqency Table")
#print(cumfreq_age)


#prob_age <- freq_table_age / nrow(df)    #  probability

#data_frame <- data.frame(freq_table_age, cumfreq_age, prob_age)
#colnames(data_frame) <- c("Age", "Frequency", "Cumulative_Frequency", "Age", "Probability")

data_frame <- data.frame(freq_table_age, cumfreq_age)
colnames(data_frame) <- c("Age", "Frequency", "Cumulative_Frequency")

print(data_frame)


#---------------create freq table by group using dplyr pkg----------------
install.packages('dplyr')
library(dplyr)

df %>%
  group_by(df$sex, df$age) %>%
  summarize(Freq=n())



#----------GRAPHS-------BY AGE---------------------

# LINE 

df1 <- data_frame( c(head(df)))
j= 1
for (i in 2:nrow(df)) {
  if(tolower(df[['sex']][i]) == tolower("Female"))
    {
    df1 [j,] <- df[i,]
    j <- j + 1
  }
  i <- i + 1
  
}
#df1 <- table(df$sex)
View(df1)

#-------------Line chart by age------------

df_age <- data.frame(freq_table_age)

plot(df_age$Freq, type = "o", 
     xlab = "Age", ylab = "Population",
     main = "Population distribution by age")

# SCATTER
set.seed(13)

n <- length(df$age)
x <- runif(n)
eps <- rnorm(n, 0, 0.25)

y <- 2 + 3 * x^2 + eps


plot(x, y, pch = 19, col = "black")
plot(y ~ x, pch = 19, col = "red") # Equivalent


# STEAM & LEAF plot

stem(df$age, scale = 1, width = 100)


# BOXPLOT
boxplot(df$age, main = " Adults Age wise data", ylab = "Age in years")

#--------mean(), median(), SD()----By Age-------------
mean(df$age)
median(df$age)
sd(df$age)

#------mode() user define function by age-------
library(modeest)
age_vec <- c(df$age)
print(age_vec)

 #mode_age <- Mode(age_vec)

calculate_mode <- function(x){
  uniq_values <- unique(x)
  uniq_cnt <- table(x)
  mode_value <- uniq_values[which.max(uniq_cnt)]
  return(mode_value)
}

mode_age <- calculate_mode(age_vec)
print(mode_age)


#------------Quartiles.... Percentiles and Deciles by age------------
quantile(df[['age']], p = c(0, 0.25, 0.5, 0.75, 1))

#quantile(df[['age']], p = c(0.05, .25, .5, .75, .95))

quantile(df$age, probs = c(0.125, 0.375, 0.625, 0.875))

#---------Deciles by age-------------
 des_age <- quantile(df$age, probs = seq(0.1, 1, by = 0.1))
print(des_age)

#res_desciles <- ntile(df$age, 30)
#print(res_desciles)


#-----Range-----------by age
range_value <- max(df$age) - min(df$age)
print(range_value)

#--------variance by age------
var(df$age) 

#-------standard error-----() sd/ square root of n)------------
print(sqrt(sum((df$age - mean(df$age)) ^ 2 / (length(df$age) -1 )))/ sqrt(length(df$age)))

print(sd(df$age)/sqrt(length(df$age)))

#library(plotrix)
print(std.error(df$age))


#===========Frequency distribution: HISTOGRAM by AGE (Continuous Attribute)===================
age <- df$age
hist(age, 
     main = "Frequency Distribution : By Age",
     xlab = "Adult Age",
     xlim = c(10, 100),
     freq = FALSE)
lines(density(df$age), lwd=3, col= 'blue')


histPercent <- function(x, ...) {
  H <- hist(age, plot = FALSE)
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density), "%", sep="")
  plot(H, freq = FALSE, labels = labs, ylim=c(0, 1.08*max(H$density)),...)
}

histPercent(df$age, col="gray")

#-------------Pie chart by Sex---------------

install.packages("lessR")
library(lessR)
install.packages("plotrix")
library(plotrix)

gender_count <- table(df$sex)

df_gender <- data.frame(gender_count)

#color <- brewer.pal(length(gender_count), "Set2")

#Pie(df_gender$Freq, clockwise = TRUE, labels = df_gender$Var1, col = color, cex = 1, border = color)      

#PieChart(df_gender$Freq, hole = 0, values = "%", data = df_gender, fill = c("lightblue", "pink"), main = "")

pie3D(df_gender$Freq, labels = df_gender$Var1, explode = 0.15, col = 2:3, labelcol = "black", border = "white")



#============ BAR PLOT by WORKCLASS ================
barplot(table(df$workclass), cex.axis = 0.8, cex.names = 0.7, 
        xlab = 'Frequency Distribution as per Workclass',
        ylab = 'Count')

#ggplot(df, aes(workclass)) + geom_bar()


#============ BAR PLOT by EDUCATION ================
barplot(table(df$education), cex.axis = 0.8, cex.names = 0.7, 
        xlab = 'Frequency Distribution : By Education',
        ylab = 'Count')

#============ BAR PLOT by Marital Status ================
barplot(table(df$marital.status), cex.axis = 0.8, cex.names = 0.55, 
        xlab = 'Frequency Distribution : By Marital Status',
        ylab = 'Count')



#============ BAR PLOT by Native Country ================
barplot(table(df$native.country), cex.axis = 0.8, cex.names = 0.7, 
        xlab = 'Frequency Distribution : By Native country',
        ylab = 'Count')

#============ BAR PLOT by Relationship ================
barplot(table(df$relationship), cex.axis = 0.8, cex.names = 0.7, 
        xlab = 'Frequency Distribution : By Relationship',
        ylab = 'Count')




#============ BAR PLOT by Occupation ================

barplot(table(df$occupation), cex.axis = 0.8, cex.names = 0.4, 
        xlab = 'Frequency Distribution : By Occupation',
        ylab = 'Count')

df_occu <- table(df$occupation)
dataFrame_occu <- data.frame(df_occu)

#-----------Pareto Chart for occupation----------
install.packages('qcc')
library(qcc)

defect_occu <- c(1843,3770,9,4099,4066,994,1370,2002,3295,149,4140,649,3650,928,1597)

#x axis titles
names(defect_occu) <- c("?","Adm-clerical", "Armed-Forces", "Craft-repair", "Exec-managerial", 
                        "Farming-fishing", "Handlers-cleaners", "Machine-op-inspct", 
                        "Other-service","Priv-house-serv","Prof-specialty","Protective-serv",
                        "Sales","Tech-support","Transport-moving")

pareto.chart(defect_occu, xlab = "Occupations", 
             ylab = "Frequency",
             col=heat.colors(length(df_occu)),
             cumperc = seq(0, 1000, by = 50),
             ylab2 = "Cumulative Percentage", #lable y right
             main = "Occupation levels in Adult dataset", #title of the chart
)
             
#quantile(defect_occu, p = c(0, 0.25, 0.5, 0.75, 1))

#----------Frequency table for occupation----------
freq_table_occu <- table(df$occupation)
print(freq_table_occu)

#--------Pie Chart for Occupation----------
library(RColorBrewer)
install.packages("plotrix")
library(plotrix)

occu_count <- c(dataFrame_occu$Freq)

color <- brewer.pal(length(occu_count), "Set2")
#pie_labels <- paste0(occu_count, "=", round(100*occu_count/sum(occu_count),2), "%")
#pie(occu_count, clockwise = TRUE, labels = pie_labels, col = color, cex = 1, border = color)

pie(occu_count, clockwise = TRUE, labels = occu_count, col = color, cex = 1, border = color)      


     
     

#===========Frequency distribution: HISTOGRAM by Education Num (Continuous)[ORDINAL] ===================
hist(df$education.num, 
     main = "Frequency Distribution : By Education Number",
     xlab = "Adult Education",
     xlim = c(0, 20),
     freq = FALSE)
lines(density(df$education.num), lwd=3, col= 'blue')


mean(df$education.num)
median(df$education.num)
sd(df$education.num)

summary(df$education.num)

boxplot(df$education.num, main = " Eucation Number", ylab = "Education by classes taken")

#------mode() user define function by education number-------
library(modeest)
edu_vec <- c(df$education.num)
print(edu_vec)

#mode_age <- Mode(age_vec)

calculate_mode <- function(x){
  uniq_values <- unique(x)
  uniq_cnt <- table(x)
  mode_value <- uniq_values[which.max(uniq_cnt)]
  return(mode_value)
}

mode_edu <- calculate_mode(edu_vec)
print(mode_edu)


#------------Quartiles.... Percentiles and Deciles by education num------------
quantile(df$education.num, p = c(0, 0.25, 0.5, 0.75, 1))


#---------Deciles by edu num-------------
desiles_eduNum <- quantile(df$education.num, probs = seq(0.1, 1, by = 0.1))
print(desiles_eduNum)

#res_desciles <- ntile(df$age, 30)
#print(res_desciles)


#-------------Line chart by edu num------------
freq_eduNum <- table(df$education.num)
df_eduNum <- data.frame(freq_eduNum)

plot(df_eduNum$Freq, type = "o", 
     xlab = "Classes Taken", ylab = "Population",
     main = "Education by class")

# SCATTER
set.seed(12)

n <- length(df$education.num)
x <- runif(n)
eps <- rnorm(n, 0, 0.25)

y <- 2 + 3 * x^2 + eps

  
  plot(x, y, pch = 19, col = "black")
plot(y ~ x, pch = 19, col = "red") # Equivalent


# STEAM & LEAF plot

#stem(df$education.num, scale = , width = 100)


#-----------Piechart for Education Number variable (ORDINAL VARIABLE)------------

eduNum <- table(df$education.num)
dataFrame_eduNum <- data.frame(eduNum)
eduNum_count <- c(dataFrame_eduNum$Freq)

color <- brewer.pal(length(eduNum_count), "Set2")
pie(eduNum_count, clockwise = TRUE, labels = eduNum_count, col = color, cex = 1, border = color)  






#================ FREQ TABLE FOR CATEGORICAL DATA =====$$$============

#library(data.table)
library("ggplot2")

 #---import dataset---
  
freq <- table(df$sex)
print("Frequency count of column SEX")
print(freq)

   # re-order levels
compute <- function(x) {
  freq <- table(x)
  factor(x, leves = names(sort(freq)))
}

  # plotting the data
ggplot(df, aes(x = compute(df['sex']))) +
  geom_bar() +
  xlab("Gender")

