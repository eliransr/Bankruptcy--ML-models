###############################################################################
#    Date : 24/09/2019                                                         #
#    Author: Eliran Srur , Chen Shlev , Matan Chover , Amir Mesilati          #                                             #
#    Purpose: Exploratory Data Analysis                                       #
###############################################################################
#-----------------------------
#------Package List---------
#-----------------------------

#------Chart------
#install.packages("ggplot2")
library(ggplot2)

#------Linear Model------
#install.packages('caTools')
library(caTools)

#install.packages('dplyr')
library(dplyr)

#------Random forest------
#install.packages('randomForest')
library(randomForest)

#------Missing values------
#install.packages('Amelia')
library(Amelia)

#------Correlations------ 
#install.packages("corrplot")
library(corrplot)
#install.packages("caret")
library(caret)

#------Text mining / Naive base------
#install.packages('tm')
library(tm)

#install.packages('e1071')
library(e1071)

#install.packages('wordcloud')
library(wordcloud)

#install.packages('SDMTools')
library(SDMTools)

#------Decision trees------
#install.packages('rpart')
library(rpart)

#install.packages('rpart.plot')
library(rpart.plot)

#install.packages('ISLR')
library('ISLR')

#-----kNN-------
#install.packages('class')
library(class)

#------ROC------
#install.packages('pROC')
library(pROC)

#-----Oversampeling------
#install.packages("ROSE")
library(ROSE)

#------Other Useful------
#install.packages("Cairo", "cairoDevice ")
#install.packages("gridExtra")
#install.packages("caret")
#install.packages("mice")
#install.packages("outliers")
#install.packages("moderndive")
#install.packages("effects")
#install.packages("VIM")
library(Cairo ,cairoDevice)
library(gridExtra)
library(caret)
library(mice)
library(outliers)
library(moderndive)
library(effects)
library("VIM")
myseed = 2019
#-----------------------------------

#-----------------------------------------
# Step 1: Loading of database
#-----------------------------------------

setwd("C:/Users/HP/Desktop/Eliran/îëììä ìäðãñä/ùðä â/ñîñèø á/ëøééú éãò åìîéãú îëåðä/R Project Final")
bankruptcy <- read.csv('bankruptcy.csv', header=TRUE, na.strings="?")
str(bankruptcy)
summary(bankruptcy)

#------------------------------------
# Step 2: Explore data 
#       : Change data type
#------------------------------------
bankruptcy.raw<-bankruptcy
colnames(bankruptcy.raw) # Id , Attr1 - Attr64 , class
dim(bankruptcy.raw)   # 5910 66
str(bankruptcy.raw)
summary(bankruptcy.raw)
View(bankruptcy.raw)

#-----------------------------------------------------------------------------
# Step 3: Checking total missing data for each column and total missing rows
#-----------------------------------------------------------------------------

any(is.na.data.frame(bankruptcy.raw))

missmap<-missmap(bankruptcy.raw, main = "Missing data", col = c('yellow', 'black'))

cat("Row with no missing data:", sum(complete.cases(bankruptcy.raw)))   # 3031 rows with no missing data
cat("Row with missing data: ", sum(!complete.cases(bankruptcy.raw)))    # 2879 rows with missing data

#---create vec with col that there is NA
make_na_vec <- function(data_frame){
  apply(bankruptcy.raw,2,function(data_frame)any(is.na.data.frame(data_frame)))
}

answer.vec<-function(data_frame){
  return(as.vector(make_na_vec(data_frame)))
}

check_answer <- answer.vec(bankruptcy.raw)
check_answer

#--show col that there is NA on graph
apply(bankruptcy.raw, 2, function(x) { sum(is.na(x)) })

library("VIM")
percent_missing_vlaue<-aggr(bankruptcy.raw , col=c('green','red'),
                            numbers=TRUE, sortVars=TRUE,
                            labels=names(bankruptcy.raw), cex.axis=.7,
                            gap=3, ylab=c("Missing data","Pattern"))# shows percentage of missing values in each column
percent_missing_vlaue
# Note: Can investigate removing Attr21, Attr27, Attr37, Attr45, Attr60
#       due to many missing values in columns
#
# After consulting Subject Matter Expert, derive the following conclusion: 
# - id : remove , no give a value to the model
# - Attr21: Changed missing value to 0. This refers to previous year's sales, because may be a new company.
# - Attr27: Changed missing value to 0, This shows how much return incurred through interest and other misc expenses. Some companies might not have loans from other institutions.
# - Attr37: Remove, because too much missing values. 
# - Attr45: Remove, because it is same as Attr60. Attr60 is more generalized form, as it does not includes Operating expenses.
# - Attr60: Don't remove, because this shows how companies managed to convert products to sales.

# Replace missing values from Attr21 and Attr27 with 0

bankruptcy.raw$Attr21[is.na(bankruptcy.raw$Attr21)] <- 0
bankruptcy.raw$Attr27[is.na(bankruptcy.raw$Attr27)] <- 0

# Remove column id , Attr37 and Attr45
bankruptcy.raw <- subset(bankruptcy.raw, select=-c(id ,Attr37,Attr45 ))
colnames(bankruptcy.raw)

#-----------------------------------------------------------
# Step 4: Check correlation table to see if some columns
#         can be removed
#         **returns a reordered correlation matrix.
#-----------------------------------------------------------
#--check correlation between cols ( without the target class)
correlationMatrix<- cor(bankruptcy.raw[1:(ncol(bankruptcy.raw)-1)], use = "complete.obs")
correlationMatrix
print(corrplot(correlationMatrix, method = "square" , type = "upper",order="original" ,
               tl.cex=0.6, cl.cex =0.6,cl.ratio=0.2 , cl.align.text="c" ,tl.col = "red" , main = "\nCorrelation Plot - without target"))

highlycorrelated <- findCorrelation(correlationMatrix, cutoff=0.8, verbose = T)
length(highlycorrelated)
highlycorrelated = sort(highlycorrelated)
highlycorrelated

# - Due to high correlation, these <<columns>> are recommended to be removed:before Treatment of NA values
#   1  2  7  8  9 10 11 12 14 18 19 22 23 26 28 30 31 33 35
#   37 39 43 44 45 47 48 49 51 62
#-----------------------------------------------------------
# Step 5: Remove some columns using: correlation
#         - Remove columns based on high correlation
#-----------------------------------------------------------

#----------------------------------------------------------
df_reduced_after_corr <- bankruptcy.raw[, -highlycorrelated]
colnames(df_reduced_after_corr)
#plot matrix to find high correlation between the att to the target 
correlationMatrix.target<- cor(df_reduced_after_corr[1:(ncol(df_reduced_after_corr))], use = "complete.obs")
correlationMatrix.target

print(corrplot(correlationMatrix.target, method = "square" , type = "upper",order="original" ,
               tl.cex=0.6, cl.cex =0.6,cl.ratio=0.2 , cl.align.text="c" ,tl.col = "red" , main = "\nCorrelation Plot - with target"))
colnames(df_reduced_after_corr)
#Delete culoms with Low currolation related to target goal
# Attr: 4,13,15,17,21,34,41,60 ,63
#Attr: 6,13,15,17,21,34,41,60 ,63
#qvec<-c(2,6,5,8,10,16,19,30,33)
# Attr: 4,13,15,17,21,34,41,60 ,63
qvec<-c(4,5,6,16,19,30,33)
qvec = sort(qvec)

df_reduced_after_corr <- df_reduced_after_corr[, -qvec]
colnames(df_reduced_after_corr)

#--------------------------------------------------
# shows percentage of missing values in each column
# after Remove columns based on high correlation
#--------------------------------------------------

missing_ds <- subset(df_reduced_after_corr, select = -c(class))
summary(missing_ds)

#Tabular view of missing values with respect to the columns they are present in
md.pattern(missing_ds)
ave_plot <- aggr(df_reduced_after_corr, col=c('green','yellow'),
                 numbers=TRUE, sortVars=TRUE,
                 labels=names(df_reduced_after_corr), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))

#AVERAGE FUNCTION
# change NA  to the average of his col
getmode <- function(v) 
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Function for removing outliers
out.rem<-function(x) 
{
  x[which(x==outlier(x))]=NA
  x
}

imputed_dataset=df_reduced_after_corr %>% mutate_if(is.numeric, funs(replace(.,is.na(.), mean(., na.rm = TRUE)))) %>%
  mutate_if(is.factor, funs(replace(.,is.na(.), getmode(na.omit(.)))))

any(is.na(imputed_dataset))

cat("Row with no missing data:", sum(complete.cases(imputed_dataset)))   # 5910 rows with no missing data
cat("Row with missing data: ", sum(!complete.cases(imputed_dataset)))   # 0 rows with missing data


#-----------------------------------------------------------
# Step 6: Use coercex for clean Exceptions values
#-----------------------------------------------------------
#--coercex
coercex.maxValue <- function(x,by){
  if(x<=by) return(x)
  return(by)
}

coercex.minValue <- function(x,by){
  if(x>=by) return(x)
  return(by)
}

#factor the target attribute
imputed_dataset$class<-as.factor(imputed_dataset$class)
str(imputed_dataset)
bankruptcy.prepared<-imputed_dataset
Bins.Data<-bankruptcy.prepared
colnames(Bins.Data)
summary(Bins.Data)

#Attr3
quantile(bankruptcy.prepared$Attr3, probs = c(0,0.5,1,2,5,10,25,50,75,90,95,97,99,99.5,100)/100)
ggplot(bankruptcy.prepared,aes(Attr3)) + geom_histogram(aes(fill=class),color='black' ,bins=50,alpha=0.5) +theme_bw()+xlim(-2,2)
bankruptcy.prepared$Attr3 <- sapply(bankruptcy.prepared$Attr3, coercex.maxValue, by = 1)
bankruptcy.prepared$Attr3 <- sapply(bankruptcy.prepared$Attr3,coercex.minValue, by=-1)
#plot after clean Exceptions values
ggplot(bankruptcy.prepared,aes(Attr3)) + geom_histogram(aes(fill=class),color='black' ,bins=50,alpha=0.5) +theme_bw()+xlim(-1,1)

#Bins
breaks.3<- c(-1,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,1)
bins.A3<- cut(bankruptcy.prepared$Attr3, breaks=breaks.3 , include.lowest = TRUE,right = FALSE)
summary(bins.A3)
Bins.Data$Attr3<-bins.A3
Bins.Data$Attr3<-as.factor(Bins.Data$Attr3)
#Analyse this attribute
ggplot(Bins.Data,aes(Attr3, fill = class)) + geom_bar(position = 'fill') 

#Attr4
ggplot(bankruptcy.prepared, aes(Attr4))+geom_histogram(binwidth=0.1)+xlim(0,35)
bankruptcy.prepared$Attr4 <- sapply(bankruptcy.prepared$Attr4, coercex.maxValue, by = 40) # 100 ?

#Attr5
quantile(bankruptcy.prepared$Attr5, probs = c(0,10,20,30,40,50,60,70,80,90,100)/100)
ggplot(bankruptcy.prepared, aes(Attr5))+geom_histogram()+xlim(-400,400)
bankruptcy.prepared$Attr5 <- sapply(bankruptcy.prepared$Attr5,coercex.maxValue, by=400)
bankruptcy.prepared$Attr5 <- sapply(bankruptcy.prepared$Attr5,coercex.minValue, by=-400)
#Bins
breaks.5<- c(-400,-200,-100-50,0,50,100,200,400)
bins.A5<- cut(bankruptcy.prepared$Attr5, breaks=breaks.5 , include.lowest = TRUE,right = FALSE)
summary(bins.A5)
Bins.Data$Attr5<-bins.A5
Bins.Data$Attr5<-as.factor(Bins.Data$Attr5)
#Analyse this attribute
ggplot(Bins.Data,aes(Attr5, fill = class)) + geom_bar(position = 'fill') 


#Attr6
quantile(bankruptcy.prepared$Attr6, probs = c(0,0.5,1,2,5,10,25,50,75,90,95,97,99,99.5,100)/100)
ggplot(bankruptcy.prepared,aes(Attr6)) + geom_histogram(aes(fill=class),color='black' ,bins=50,alpha=0.5) +theme_bw()+xlim(-1,1)
bankruptcy.prepared$Attr6 <- sapply(bankruptcy.prepared$Attr6,coercex.maxValue, by=1)
bankruptcy.prepared$Attr6 <- sapply(bankruptcy.prepared$Attr6,coercex.minValue, by=-1)
#Bins
breaks.6<- c(-1,-0.05,-0.03,-0.02,-0.01,0,0.01,0.02,0.03,0.05,1)
bins.A6<- cut(bankruptcy.prepared$Attr6, breaks=breaks.6 , include.lowest = TRUE,right = FALSE)
summary(bins.A6)
Bins.Data$Attr6<-bins.A6
Bins.Data$Attr6<-as.factor(Bins.Data$Attr6)
#Analyse this attribute
ggplot(Bins.Data,aes(Attr6, fill = class)) + geom_bar(position = 'fill') 
#Delete the att 
bankruptcy.prepared$Attr6<-NULL

#Attr13
ggplot(bankruptcy.prepared, aes(Attr13))+geom_histogram(binwidth=0.1)+xlim(-5,5)
bankruptcy.prepared$Attr13 <- sapply(bankruptcy.prepared$Attr13, coercex.maxValue, by = 5)
bankruptcy.prepared$Attr13 <- sapply(bankruptcy.prepared$Attr13, coercex.minValue, by = -5)

#Attr15
ggplot(bankruptcy.prepared, aes(Attr15))+geom_histogram(binwidth=0.1)+xlim(-2,100)
bankruptcy.prepared$Attr15 <- sapply(bankruptcy.prepared$Attr15, coercex.maxValue, by = 14000)
bankruptcy.prepared$Attr15 <- sapply(bankruptcy.prepared$Attr15, coercex.minValue, by = -10000)

#Attr16
quantile(bankruptcy.prepared$Attr16, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr16)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()
ggplot(bankruptcy.prepared, aes(Attr16))+geom_histogram(binwidth=0.1)+xlim(-1.5,5)
bankruptcy.prepared$Attr16 <- sapply(bankruptcy.prepared$Attr16,coercex.maxValue, by=4.5)
bankruptcy.prepared$Attr16 <- sapply(bankruptcy.prepared$Attr16,coercex.minValue, by=-1.4)
#Bins
breaks.16<- c(-1.4,-0.5,-0.25,0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75 ,4.5)
bins.A16<- cut(bankruptcy.prepared$Attr16, breaks=breaks.16 , include.lowest = TRUE,right = FALSE)
summary(bins.A16)
Bins.Data$Attr16<-bins.A16
Bins.Data$Attr16<-as.factor(Bins.Data$Attr16)
#Analyse this attribute
ggplot(Bins.Data,aes(Attr16, fill = class)) + geom_bar(position = 'fill') 

#Attr17
ggplot(bankruptcy.prepared, aes(Attr17))+geom_histogram(binwidth=0.1)+xlim(-2,50)
bankruptcy.prepared$Attr17 <- sapply(bankruptcy.prepared$Attr17, coercex.maxValue, by = 50)
summary(bankruptcy.prepared$Attr17)
#Attr20
quantile(bankruptcy.prepared$Attr20, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr20)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(0,215)
ggplot(imputed_dataset, aes(Attr20))+geom_histogram(binwidth=0.1)+xlim(-0.2,10)
bankruptcy.prepared$Attr20 <- sapply(bankruptcy.prepared$Attr20, coercex.maxValue, by = 200)
bankruptcy.prepared$Attr20 <- sapply(bankruptcy.prepared$Attr20, coercex.minValue, by = 0)
#Bins
breaks.20<- c(0,20,40,60,80,100,120,140,160,180,200)
bins.A20<- cut(bankruptcy.prepared$Attr20, breaks=breaks.20 , include.lowest = TRUE,right = FALSE)
summary(bins.A20)

Bins.Data$Attr20<-bins.A20
Bins.Data$Attr20<-as.factor(Bins.Data$Attr20)
#Analyse this Attribute
ggplot(Bins.Data,aes(Attr20, fill = class)) + geom_bar(position = 'fill') 
class(Bins.Data$Attr20)
colnames(Bins.Data)

#Attr21
ggplot(bankruptcy.prepared, aes(Attr21))+geom_histogram(binwidth=0.1)+xlim(0,3)
bankruptcy.prepared$Attr21 <- sapply(bankruptcy.prepared$Attr21, coercex.maxValue, by = 20)
bankruptcy.prepared$Attr21 <- sapply(bankruptcy.prepared$Attr21, coercex.minValue, by = 0)

#Attr24
quantile(bankruptcy.prepared$Attr24, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr24)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-1.1,2.1)
ggplot(bankruptcy.prepared, aes(Attr24))+geom_histogram(binwidth=0.1)+xlim(-2,2.5)
bankruptcy.prepared$Attr24 <- sapply(bankruptcy.prepared$Attr24,coercex.maxValue, by=2)
bankruptcy.prepared$Attr24 <- sapply(bankruptcy.prepared$Attr24,coercex.minValue, by=-1)
#Bins
breaks.24<- c(-1,-0.75,-0.25,0,0.25,0.5,0.75,1,1.25,1.5,1.75,2)
bins.A24<- cut(bankruptcy.prepared$Attr24, breaks=breaks.24 , include.lowest = TRUE,right = FALSE)
summary(bins.A24)
Bins.Data$Attr24<-bins.A24
Bins.Data$Attr24<-as.factor(Bins.Data$Attr24)
#Analyse this Attribute
ggplot(Bins.Data,aes(Attr24, fill = class)) + geom_bar(position = 'fill') 
colnames(Bins.Data)

#Attr25
quantile(bankruptcy.prepared$Attr25, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr25)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-2,1.1)
bankruptcy.prepared$Attr25 <- sapply(bankruptcy.prepared$Attr25, coercex.maxValue, by = 1)
bankruptcy.prepared$Attr25 <- sapply(bankruptcy.prepared$Attr25, coercex.minValue, by = -1.5)

#Bins
breaks.25<- c(-1.5,-1.25,-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1)
bins.A25<- cut(bankruptcy.prepared$Attr25, breaks=breaks.25 , include.lowest = TRUE,right = FALSE)
summary(bins.A25)

Bins.Data$Attr25<-bins.A25
Bins.Data$Attr25<-as.factor(Bins.Data$Attr25)
#Analyse this Attribute
ggplot(Bins.Data,aes(Attr25, fill = class)) + geom_bar(position = 'fill') 
colnames(Bins.Data)

#Attr27

quantile(bankruptcy.prepared$Attr27, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr27)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-50,10)
ggplot(bankruptcy.prepared, aes(Attr27))+geom_histogram(binwidth=0.1)+xlim(-5,20)
bankruptcy.prepared$Attr27 <- sapply(bankruptcy.prepared$Attr27, coercex.maxValue, by = 10)
bankruptcy.prepared$Attr27 <- sapply(bankruptcy.prepared$Attr27, coercex.minValue, by = -50)
ggplot(bankruptcy.prepared, aes(Attr27))+geom_histogram(binwidth=0.1)+xlim(-50,10)

#Bins
breaks.27<- c(-50,-40,-30,-20,-10,0,10)
bins.A27<- cut(bankruptcy.prepared$Attr27, breaks=breaks.27 , include.lowest = TRUE,right = FALSE)
summary(bins.A27)
Bins.Data$Attr27<-bins.A27
Bins.Data$Attr27<-as.factor(Bins.Data$Attr27)
#Analyse the Attribute
ggplot(Bins.Data,aes(Attr27, fill = class)) + geom_bar(position = 'fill') 
colnames(Bins.Data)

#Attr29
quantile(bankruptcy.prepared$Attr29, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr29)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(0,7.5)
bankruptcy.prepared$Attr29 <- sapply(bankruptcy.prepared$Attr29, coercex.maxValue, by = 6.8)
bankruptcy.prepared$Attr29 <- sapply(bankruptcy.prepared$Attr29, coercex.minValue, by = 1)
#Bins
breaks.29<- c(1,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5,5.25,5.5,5.75,6,6.25,6.8)
bins.A29<- cut(bankruptcy.prepared$Attr29, breaks=breaks.29 , include.lowest = TRUE,right = FALSE)
summary(bins.A29)

Bins.Data$Attr29<-bins.A29
Bins.Data$Attr29<-as.factor(Bins.Data$Attr29)
#Analayse this Attribute
ggplot(Bins.Data,aes(Attr29, fill = class)) + geom_bar(position = 'fill') 

#Attr32
quantile(bankruptcy.prepared$Attr32, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr32)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-1,560)

bankruptcy.prepared$Attr32 <- sapply(bankruptcy.prepared$Attr32, coercex.maxValue, by = 500)
bankruptcy.prepared$Attr32 <- sapply(bankruptcy.prepared$Attr32, coercex.minValue, by = 0)
#Bins
breaks.32<- c(0,50,100,150,200,250,300,350,400,450,500)
bins.A32<- cut(bankruptcy.prepared$Attr32, breaks=breaks.32 , include.lowest = TRUE,right = FALSE)
summary(bins.A32)
Bins.Data$Attr32<-bins.A32
Bins.Data$Attr32<-as.factor(Bins.Data$Attr32)

ggplot(Bins.Data,aes(Attr32, fill = class)) + geom_bar(position = 'fill') 


#Attr34
ggplot(bankruptcy.prepared, aes(Attr34))+geom_histogram(binwidth=0.1)+xlim(-5,30)
bankruptcy.prepared$Attr34 <- sapply(bankruptcy.prepared$Attr34, coercex.maxValue, by = 30)
#Attr36
quantile(bankruptcy.prepared$Attr36, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr36)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-0.5,10)
bankruptcy.prepared$Attr36 <- sapply(bankruptcy.prepared$Attr36, coercex.maxValue, by = 8)
bankruptcy.prepared$Attr36 <- sapply(bankruptcy.prepared$Attr36, coercex.minValue, by = 0.06)
#Bins
breaks.36<- c(0.06,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9)
bins.A36<- cut(bankruptcy.prepared$Attr36, breaks=breaks.36 , include.lowest = TRUE,right = FALSE)
summary(bins.A36)

Bins.Data$Attr36<-bins.A36
Bins.Data$Attr36<-as.factor(Bins.Data$Attr36)

ggplot(Bins.Data,aes(Attr36, fill = class)) + geom_bar(position = 'fill') 



#Attr39
quantile(bankruptcy.prepared$Attr39, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr39)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-1,1)
bankruptcy.prepared$Attr39 <- sapply(bankruptcy.prepared$Attr39,coercex.maxValue, by=0.6)
bankruptcy.prepared$Attr39 <- sapply(bankruptcy.prepared$Attr39,coercex.minValue, by=-0.6)
#Bins
breaks.39<- c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6)
bins.A39<- cut(bankruptcy.prepared$Attr39, breaks=breaks.39 , include.lowest = TRUE,right = FALSE)
summary(bins.A39)

Bins.Data$Attr39<-bins.A39
Bins.Data$Attr39<-as.factor(Bins.Data$Attr39)
#Analayse this Attribute
ggplot(Bins.Data,aes(Attr39, fill = class)) + geom_bar(position = 'fill') 

#Attr41
ggplot(bankruptcy.prepared, aes(Attr41))+geom_histogram(binwidth=0.1)+xlim(-6,6)
bankruptcy.prepared$Attr41 <- sapply(bankruptcy.prepared$Attr41, coercex.maxValue, by = 5)
bankruptcy.prepared$Attr41 <- sapply(bankruptcy.prepared$Attr41, coercex.minValue, by = -5)
#Attr42
quantile(bankruptcy.prepared$Attr42, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr42)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-1,1)
bankruptcy.prepared$Attr42 <- sapply(bankruptcy.prepared$Attr42,coercex.maxValue, by=0.4)
bankruptcy.prepared$Attr42 <- sapply(bankruptcy.prepared$Attr42,coercex.minValue, by=-0.4)
#Bins
breaks.42<- c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4)
bins.A42<- cut(bankruptcy.prepared$Attr42, breaks=breaks.42 , include.lowest = TRUE,right = FALSE)
summary(bins.A42)

Bins.Data$Attr42<-bins.A42
Bins.Data$Attr42<-as.factor(Bins.Data$Attr42)
#Analayse this Attribute
ggplot(Bins.Data,aes(Attr42, fill = class)) + geom_bar(position = 'fill') 

summary(Bins.Data)

#Attr43
quantile(bankruptcy.prepared$Attr43, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr43)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(7,600)
bankruptcy.prepared$Attr43 <- sapply(bankruptcy.prepared$Attr43,coercex.maxValue, by=600)
bankruptcy.prepared$Attr43 <- sapply(bankruptcy.prepared$Attr43, coercex.minValue, by = 0)
#Bins
breaks.43<- c(0,50,100,150,200,250,300,350,400,450,500,550,600)
bins.A43<- cut(bankruptcy.prepared$Attr43, breaks=breaks.43 , include.lowest = TRUE,right = FALSE)
summary(bins.A43)

Bins.Data$Attr43<-bins.A43
Bins.Data$Attr43<-as.factor(Bins.Data$Attr43)
#Analayse this Attribute
ggplot(Bins.Data,aes(Attr43, fill = class)) + geom_bar(position = 'fill') 

#Attr48
quantile(bankruptcy.prepared$Attr48, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr48)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-1.1,1)
bankruptcy.prepared$Attr48 <- sapply(bankruptcy.prepared$Attr48,coercex.maxValue, by=0.8)
bankruptcy.prepared$Attr48 <- sapply(bankruptcy.prepared$Attr48,coercex.minValue, by=-1.2)
#Bins
breaks.48<- c(-1.2,-0.75,-0.5,-0.25,0,0.25,0.5,0.8)
bins.A48<- cut(bankruptcy.prepared$Attr48, breaks=breaks.48 , include.lowest = TRUE,right = FALSE)
summary(bins.A48)

Bins.Data$Attr48<-bins.A48
Bins.Data$Attr48<-as.factor(Bins.Data$Attr48)
#Analayse this Attribute
ggplot(Bins.Data,aes(Attr48, fill = class)) + geom_bar(position = 'fill') 

#Attr52
quantile(bankruptcy.prepared$Attr52, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr52)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-2,6)
bankruptcy.prepared$Attr52 <- sapply(bankruptcy.prepared$Attr52,coercex.maxValue, by=3.5)
bankruptcy.prepared$Attr52 <- sapply(bankruptcy.prepared$Attr52, coercex.minValue, by = 0)
ggplot(bankruptcy.prepared,aes(Attr52)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-0.5,4)
#Bins
breaks.52<- c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5)
bins.A52<- cut(bankruptcy.prepared$Attr52, breaks=breaks.52 , include.lowest = TRUE,right = FALSE)
summary(bins.A52)

Bins.Data$Attr52<-bins.A52
Bins.Data$Attr52<-bins.A52<-as.factor(Bins.Data$Attr52<-bins.A52)
#Analayse this Attribute
ggplot(Bins.Data,aes(Attr52, fill = class)) + geom_bar(position = 'fill') 

#Attr54
quantile(bankruptcy.prepared$Attr54, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr54)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-10,75)
bankruptcy.prepared$Attr54 <- sapply(bankruptcy.prepared$Attr54, coercex.maxValue, by = 30)
bankruptcy.prepared$Attr54 <- sapply(bankruptcy.prepared$Attr54, coercex.minValue, by = -4)
ggplot(bankruptcy.prepared,aes(Attr54)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-6,40)
#Bins
breaks.54<- c(-4,0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30)
bins.A54<- cut(bankruptcy.prepared$Attr54, breaks=breaks.54 , include.lowest = TRUE,right = FALSE)
summary(bins.A54)

Bins.Data$Attr54<-bins.A54
Bins.Data$Attr54<-as.factor(Bins.Data$Attr54)
#Analayse this Attribute
ggplot(bankruptcy.prepared, aes(class,bankruptcy.prepared$Attr54)) + geom_boxplot() +ylim(-1,30)
ggplot(Bins.Data,aes(Attr54, fill = class)) + geom_bar(position = 'fill') 
#we can delete this Attribute
bankruptcy.prepared$Attr54<-NULL
summary(Bins.Data)


#Attr55
ggplot(bankruptcy.prepared, aes(Attr55))+geom_histogram(binwidth=0.1)+xlim(-2,6)
quantile(bankruptcy.prepared$Attr55, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr55)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-200000,200000)
bankruptcy.prepared$Attr55 <- sapply(bankruptcy.prepared$Attr55, coercex.maxValue, by = 100000)
bankruptcy.prepared$Attr55 <- sapply(bankruptcy.prepared$Attr55, coercex.minValue, by = -100000)
#Bins
breaks.55<- c(-100000,-80000,-60000,-40000,-20000,0,20000,40000,60000,80000,100000)
labels.55<-c("1","2","3","4","5","6","7","8","9","10")
bins.A55<- cut(bankruptcy.prepared$Attr55, breaks=breaks.55 ,labels =labels.55,include.lowest = TRUE,right = FALSE)
summary(bins.A55)

Bins.Data$Attr55<-bins.A55
Bins.Data$Attr55<-as.factor(Bins.Data$Attr55)
#Analayse this Attribute
ggplot(Bins.Data,aes(Attr55, fill = class)) + geom_bar(position = 'fill')
#we can delete this Attribute
bankruptcy.prepared$Attr55<-NULL
str(bankruptcy.prepared)


#Attr56
quantile(bankruptcy.prepared$Attr56, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr56)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5) +theme_bw()+xlim(-1,1.5)
bankruptcy.prepared$Attr56 <- sapply(bankruptcy.prepared$Attr56,coercex.maxValue, by=1.5)
bankruptcy.prepared$Attr56 <- sapply(bankruptcy.prepared$Attr56,coercex.minValue, by=-1)
#Bins
breaks.56<- c(-1,-0.7,-0.3,0,0.3,0.7,1 ,1.5)
bins.A56<- cut(bankruptcy.prepared$Attr56, breaks=breaks.56 , include.lowest = TRUE,right = FALSE)
summary(bins.A56)

Bins.Data$Attr56<-bins.A56
Bins.Data$Attr56<-as.factor(Bins.Data$Attr56)
#Analayse this Attribute

ggplot(Bins.Data,aes(Attr56, fill = class)) + geom_bar(position = 'fill')

#Attr57
quantile(bankruptcy.prepared$Attr57, probs = c(0,0.2,0.5,1,2,3,4,5,10,25,50,75,90,92,93,94,95,97,98,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr57)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5)+xlim(-4,2) +theme_bw()
bankruptcy.prepared$Attr57 <- sapply(bankruptcy.prepared$Attr57,coercex.maxValue, by=2)
bankruptcy.prepared$Attr57 <- sapply(bankruptcy.prepared$Attr57,coercex.minValue, by=-1.8)
#Bins
breaks.57<- c(-1.8,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1,1.25,1.5,1.75,2)
bins.A57<- cut(bankruptcy.prepared$Attr57, breaks=breaks.57 , include.lowest = TRUE,right = FALSE)
summary(bins.A57)

Bins.Data$Attr57<-bins.A57
Bins.Data$Attr57<-as.factor(Bins.Data$AttrT)
#Analayse this Attribute

ggplot(Bins.Data,aes(Attr57, fill = class)) + geom_bar(position = 'fill') 


#Attr58
quantile(bankruptcy.prepared$Attr58, probs = c(0,0.2,0.5,1,2,5,10,25,50,75,90,92,93,94,95,97,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr58)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5)+xlim(-0.5,3) +theme_bw()
bankruptcy.prepared$Attr58 <- sapply(bankruptcy.prepared$Attr58,coercex.maxValue, by=2)
bankruptcy.prepared$Attr57 <- sapply(bankruptcy.prepared$Attr57,coercex.minValue, by=0)
#Bins
breaks.58<- c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)
bins.A58<- cut(bankruptcy.prepared$Attr58, breaks=breaks.58 , include.lowest = TRUE,right = FALSE)
summary(bins.A58)
Bins.Data$Attr58<-bins.A58
Bins.Data$Attr58<-as.factor(Bins.Data$Attr58)
ggplot(Bins.Data,aes(Attr58, fill = class)) + geom_bar(position = 'fill') 

colnames(bankruptcy.prepared)

#Attr59
quantile(bankruptcy.prepared$Attr59, probs = c(0,0.2,0.5,1,2,5,10,25,50,75,90,92,93,94,95,97,99,99.2,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared, aes(Attr59))+geom_histogram(binwidth=0.1)+xlim(-4,2.2)
bankruptcy.prepared$Attr59 <- sapply(bankruptcy.prepared$Attr59,coercex.maxValue, by=1.6)
bankruptcy.prepared$Attr59 <- sapply(bankruptcy.prepared$Attr59,coercex.minValue, by=-0.2)
#Attr60
ggplot(bankruptcy.prepared, aes(Attr59))+geom_histogram(binwidth=0.1)+xlim(-5,2.5)
bankruptcy.prepared$Attr60 <- sapply(bankruptcy.prepared$Attr60, coercex.maxValue, by = 5)
#Attr61
quantile(bankruptcy.prepared$Attr61, probs = c(0,0.2,0.5,1,2,5,10,25,50,75,90,95,97,99,99.5,99.75,99.80,99.82,99.83,99.85,99.87,99.90,100)/100)
ggplot(bankruptcy.prepared,aes(Attr61)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5)+xlim(0,400) +theme_bw()
bankruptcy.prepared$Attr61 <- sapply(bankruptcy.prepared$Attr61, coercex.maxValue, by = 250)
bankruptcy.prepared$Attr61 <- sapply(bankruptcy.prepared$Attr61,coercex.minValue, by=0)
#Bins
breaks.61<- c(0,25,50,75,100,125,150,175,200,225,250)
bins.A61<- cut(bankruptcy.prepared$Attr61, breaks=breaks.61 , include.lowest = TRUE,right = FALSE)
summary(bins.A61)
Bins.Data$Attr61<-bins.A61
#Analayse this Attribute
Bins.Data$Attr61<-as.factor(Bins.Data$Attr61)
ggplot(Bins.Data,aes(Attr61, fill = class)) + geom_bar(position = 'fill') 

#can be clean this att becuse he isn't relevant
bankruptcy.prepared$Attr61<-NULL

#Attr62
quantile(bankruptcy.prepared$Attr62, probs = c(0,0.2,0.5,1,2,5,10,25,50,75,90,95,97,99,99.5,100)/100)
ggplot(bankruptcy.prepared,aes(Attr62)) + geom_histogram(aes(fill=class),color='black' ,bins=200 ,alpha=0.5)+xlim(0,1400) +theme_bw()
bankruptcy.prepared$Attr62 <- sapply(bankruptcy.prepared$Attr62, coercex.maxValue, by = 1500)
bankruptcy.prepared$Attr62 <- sapply(bankruptcy.prepared$Attr62, coercex.minValue, by = 0)
#Bins
breaks.62<- c(0,125,250,500,750,1000,1200,1400)
bins.A62<- cut(bankruptcy.prepared$Attr62, breaks=breaks.62 , include.lowest = TRUE,right = FALSE)
Bins.Data$Attr62<-bins.A62
Bins.Data$Attr62<-as.factor(Bins.Data$Attr62)
ggplot(Bins.Data,aes(Attr62, fill = class)) + geom_bar(position = 'fill') 

#Attr63
summary(bankruptcy.prepared$Attr63)
ggplot(bankruptcy.prepared, aes(Attr63))+geom_histogram(binwidth=0.1)+xlim(-1,40)
bankruptcy.prepared$Attr63 <- sapply(bankruptcy.prepared$Attr63, coercex.maxValue, by =40)#60?
bankruptcy.prepared$Attr63 <- sapply(bankruptcy.prepared$Attr63, coercex.minValue, by = 0)

#EDA for investigate the data
ggplot(bankruptcy.prepared, aes(class,bankruptcy.prepared$Attr62)) + geom_boxplot() +ylim(-1,10)

#convert the target col from (0,1) to (Yes , No)
#convert_yesno <- function(x){
#  if(x==0) return ('No')
#  return ('Yes')
#}
#bankruptcy.prepared$class <- sapply(bankruptcy.prepared$class, convert_yesno)
#bankruptcy.prepared$class<-as.factor(bankruptcy.prepared$class)
#str(bankruptcy.prepared$class)




######## îçé÷ú òîåãåú #######

bankruptcy.prepared$Attr6<-NULL
bankruptcy.prepared$Attr61<-NULL
bankruptcy.prepared$Attr52<-NULL
bankruptcy.prepared$Attr13<-NULL
bankruptcy.prepared$Attr15<-NULL
bankruptcy.prepared$Attr27<-NULL

bankruptcy.prepared$Attr41<-NULL
bankruptcy.prepared$Attr63<-NULL
bankruptcy.prepared$Attr34<-NULL
bankruptcy.prepared$Attr17<-NULL


colnames(bankruptcy.prepared)
#s<-bankruptcy.prepared
#str(s)
#bankruptcy.prepared<-s

#bankruptcy.prepared$Attr21<-NULL
#bankruptcy.prepared$Attr54<-NULL
#bankruptcy.prepared$Attr55<-NULL

#

#Delete culoms with Low currolation related to target goal
# Attr: 6,13,15,21,27,41,52,54,60
#qvec<-c(4,6,5,10,19,24,23 ,13 , 30 ) # # Attr: 4,13,15,17,21,34,37,41,45,60 ,63 #qvec<-c(2,6,5,8,10,16,19,30,33)
#qvec = sort(qvec)

#df_reduced_after_corr <- df_reduced_after_corr[, -qvec]
#colnames(bankruptcy.prepared)
#-----------------------------------------------------------
# Step 6: Build the model
#-----------------------------------------------------------
filter <-sample.split(bankruptcy.prepared$class , SplitRatio = 0.70)
df_train <-subset(bankruptcy.prepared ,filter == TRUE)
df_test <-subset(bankruptcy.prepared ,filter == FALSE)
y<-table(df_train$class)
y # 0 : 3850  , 1 : 287
x<-y[1]
x

head(df_train)
colnames(df_train)
table(df_test$class)
str(df_train)
str(df_test)

head(df_train)
colnames(df_train)
str(df_train)

# Training set
dim(df_train) # 4137   20
table(df_train$class)  # [class 0]:3850  [class 1]:287 

# Testing set
dim(df_test)  # 1773   20
table(df_test$class)   # [class 0]:1650  [class 1]:123 





#------------------------------------
# Step 7: Rebuild model (Naive Bayes)
#------------------------------------
filter <-sample.split(bankruptcy.prepared$class , SplitRatio = 0.70)
df_train <-subset(bankruptcy.prepared ,filter == TRUE)
df_test <-subset(bankruptcy.prepared ,filter == FALSE)
dim(df_train)
model.NB <- naiveBayes(df_train[,-21],df_train$class)
#prediction on the test set 
prediction.NB <- predict(model.NB,df_test[,-21], type = 'raw')
prediction_bankrupty.NB <- prediction.NB[,"1"]
actual.NB<-  df_test$class

ggplot(bankruptcy.prepared, aes(class)) + geom_bar()

predicted.NB <-prediction_bankrupty.NB >0.5 #Our Assumption
predicted.NB<-as.numeric(predicted.NB)
table(predicted.NB)

#confusion matrix 
confi_matrix.NB <- table(predicted.NB, actual.NB)
confi_matrix.NB
#             actual.NB
#predicted.NB   No  Yes
#         No  1478   70
#         Yes  172   53

TP <- confi_matrix.NB[2,2]
FP <- confi_matrix.NB[2,1]
TN <- confi_matrix.NB[1,1]
FN <- confi_matrix.NB[1,2]
#×œ×‘×“×•×§ 
precsion <- TP/(TP+FP) #0.5
recall <- TP/(TP+FN) #0.7073171
precsion
recall

accu.NB<-accuracy( predicted.NB,actual.NB , threshold = 0.5)
accu.NB

#---------------------------------------------
# Step 8: Build model   (Decision Tree)
#---------------------------------------------
filter <-sample.split(bankruptcy.prepared$class , SplitRatio = 0.70)
df_train <-subset(bankruptcy.prepared ,filter == TRUE)
df_test <-subset(bankruptcy.prepared ,filter == FALSE)
model.DT<- rpart(class~.,df_train)
rpart.plot<-rpart.plot(model.DT, box.palette = "RdBu",shadow.col = "gray",nn=TRUE)


predict.prob.DT <-predict(model.DT,newdata = df_test)
#predict.prob.DT<-predict.prob.DT[,2]
predicted.DT<- predict.prob.DT > 0.8  #Our Assumption
predicted.DT<-as.numeric(predicted.DT[,2])
actual.DT<-df_test$class
#confusion Matrix
conf_matrix.DT<- table(predicted.DT, actual.DT)
conf_matrix.DT


#                  actual.DT
#prediction.DT        0    1
#                0 1639   70
#                1   11   53

#error of the rpart
error.rpart <- 1-(sum(diag(conf_matrix.DT))/sum(conf_matrix.DT))
error.rpart #0.0676819

TP <- conf_matrix.DT[2,2]
FP <- conf_matrix.DT[2,1]
TN <- conf_matrix.DT[1,1]
FN <- conf_matrix.DT[1,2]
#×œ×‘×“×•×§ 
precsion <- TP/(TP+FP) #0.7777778

recall <- TP/(TP+FN) #0.2276423
precsion
recall


accu.DT<-accuracy( predicted.DT,actual.DT ,  threshold = 0.5)
accu.DT

#------------------------------------------------------
# Step 9: Build model with undersampling (randomforest)
#------------------------------------------------------
filter <-sample.split(bankruptcy.prepared$class , SplitRatio = 0.70)
df_train <-subset(bankruptcy.prepared ,filter == TRUE)
df_test <-subset(bankruptcy.prepared ,filter == FALSE)
samp_size = nrow(df_train[df_train$class=="1",])
samp_size # 287
table(df_train$class)
# Tuning
model.rf <- randomForest(class~.,  data = df_train, ntree=150, sampsize=c(samp_size,samp_size), set.seed(myseed)) # try with different num of ntree
model.rf
plot(model.rf)  # Seems like 50 trees is good

# Variable Importance Plot
varImpPlot(model.rf, sort = T, main="Variable Importance")

# Change model to 50 trees
model.rf <- randomForest(class~.,  data = df_train, ntree=50, sampsize=c(samp_size,samp_size), set.seed(myseed))
plot(model.rf)
#----------------------------------
# Step 10: Predict and Evaluation
#----------------------------------
predicted.RF<- predict(model.rf, newdata = df_test)
actual.RF<-df_test$class

confusion.matrix_RF<-table(predicted.RF, actual.RF)
confusion.matrix_RF
#result_predicted    0    1
#               0  1476  29
#               1  174   94

cm<-confusion.matrix( actual.RF ,predicted.RF)
cm
conf_matrix <- confusionMatrix(confusion.matrix_RF)  
conf_matrix
conf_matrix$overall[1] # Accuracy: 0.8855048  


#----------------------
# Step 11: Evaluation
#----------------------
library(caret)
TP <- cm[2,2]
FP <- cm[2,1]
TN <- cm[1,1]
FN <- cm[1,2]
#×œ×‘×“×•×§ 
precsion <- TP/(TP+FP)  #0.3655914

recall <- TP/(TP+FN) #0.8292683
precsion
recall

accu<-accuracy(actual.RF, predicted.RF, threshold = 0.5)
accu
#-------------------------------------------------------------
# Step 12: Tuning for mtry after fixing ntree in Random Forest
#         mtry: Number of variables randomly sampled as
#               candidates at each split
#-------------------------------------------------------------

bestmtry<-tuneRF(x=df_train[,-ncol(df_train)], y=df_train$class, stepFactor=1.5, improve=1e-5, ntree=50 ,doBest = F)
print(bestmtry)   # chosen mtry=9 

mtry_opt <- bestmtry[,"mtry"][which.min(bestmtry[,"OOBError"])]
print(mtry_opt) #the min is 33
#--------------------------
# Step 13: Rebuild model 
#--------------------------
model.rf <- randomForest(class~., data = df_train, ntree=50, sampsize=c(samp_size,samp_size), mtry=mtry_opt, set.seed(myseed)) 
model.rf

predicted.RF <- predict(model.rf, newdata = df_test)
actual.RF<-df_test$class
confusion.matrix_RF = table(predicted.RF,actual.RF)
confusion.matrix_RF
#        actual.RF       0     1
#predicted.RF        0  1481    29
#                    1  169     94

TP <- confusion.matrix_RF[2,2]
FP <- confusion.matrix_RF[2,1]
TN <- confusion.matrix_RF[1,1]
FN <- confusion.matrix_RF[1,2]
#×œ×‘×“×•×§ 
precsion <- TP/(TP+FP) #0.3828996

recall <- TP/(TP+FN) #0.7804878
precsion
recall

accu<-accuracy( predicted.RF,actual.RF, threshold = 0.5)
accu
#----------------------------------------------------------------------
# Conclusion:
# Since the target of this assignment is that among those companies
# that become bankrupt, how many we can predict correctly. Therefore,
# we have chosen our performance metrics to be specificity.
#
# We acheived a Specificity of  0.8993939  We can try improving the 
# result with other algorithms.
#-----------------------------------------------------------------------

#--------------------------------------------
#Step 14: Rebuild model (KNN model)
#--------------------------------------------
kNN_Df<-bankruptcy.prepared

filter_KNN <- sample.split(kNN_Df$class, SplitRatio = 0.7)
kNN.train <- subset(kNN_Df, filter_KNN ==T)
kNN.test <- subset(kNN_Df, filter_KNN ==F)
#vhange methid to the correct imballence correction(both. under, over)
over.kNN<- ovun.sample(class~. ,data=kNN.train, method='under' ,  N=nrow(df_train))$data
kNN.train$class <- over.kNN$class
table(kNN.train$class)
str(kNN.test)

kNN.1 <-  knn(kNN.train, kNN.test, kNN.train$class, k=10)
sum.kNN_1<-100 * sum(kNN.test$class == kNN.1)/100  # For knn = 1
kNN.5 <-  knn(kNN.train, kNN.test, kNN.train$class, k=15)
sum.kNN_5<-100 * sum(kNN.test$class == kNN.5)/100  # For knn = 5
kNN.20 <- knn(kNN.train, kNN.test, kNN.train$class, k=21)
sum.kNN_20<-100 * sum(kNN.test$class == kNN.20)/100 # For knn = 20
min(sum.kNN_1 ,sum.kNN_5,sum.kNN_20)

actual.kNN<-kNN.test$class
#confusion matrix
#------confusion.matrix_kNN.1
confusion.matrix_kNN.1 <- table(kNN.1 ,actual.kNN)
confusion.matrix_kNN.1

TP <- confusion.matrix_kNN.1[2,2]
FP <- confusion.matrix_kNN.1[2,1]
TN <- confusion.matrix_kNN.1[1,1]
FN <- confusion.matrix_kNN.1[1,2]

precision.kNN.1 <- TP/(TP +FP)
recall.kNN.1 <- TP/(TP + FN)
precision.kNN.1  #0.4
recall.kNN.1  #0.01626016

#--------confusion.matrix_kNN.5
confusion.matrix_kNN.5 <- table(kNN.5 ,actual.kNN)
confusion.matrix_kNN.5

TP <- confusion.matrix_kNN.5[2,2]
FP <- confusion.matrix_kNN.5[2,1]
TN <- confusion.matrix_kNN.5[1,1]
FN <- confusion.matrix_kNN.5[1,2]

precision.kNN.5 <- TP/(TP +FP)
recall.kNN.5 <- TP/(TP + FN)
precision.kNN.5  #0.4117647
recall.kNN.5  #0.06140351


#--------confusion.matrix_kNN.20
confusion.matrix_kNN.20 <- table(kNN.20 ,actual.kNN)
confusion.matrix_kNN.20

TP <- confusion.matrix_kNN.20[2,2]
FP <- confusion.matrix_kNN.20[2,1]
TN <- confusion.matrix_kNN.20[1,1]
FN <- confusion.matrix_kNN.20[1,2]

precision.kNN.20 <- TP/(TP +FP)
recall.kNN.20 <- TP/(TP + FN)
precision.kNN.20  #0.4444444
recall.kNN.20  #0.07017544

#---------------------------------------------
# Step 15: Rebuild model (Logistic Regression)
#---------------------------------------------
filter <-sample.split(bankruptcy.prepared$class , SplitRatio = 0.70)
df_train <-subset(bankruptcy.prepared ,filter == TRUE)
df_test <-subset(bankruptcy.prepared ,filter == FALSE)
model.LR <- glm(class ~. , family = binomial(link = 'logit') , data = df_train)
summary(model.LR)
anova(model.LR, test="Chisq")

predicted.LR <-predict(model.LR , newdata = df_test , type = 'response')
actual.LR<-df_test$class
#confusion matrix
confusion_matrix.LR <- table(as.numeric(predicted.LR >0.5) , actual.LR)
confusion_matrix.LR
#                  actual.LG
#prediction.LG        0    1
#                 0 1629   90
#                 1   21   33
TP <- confusion_matrix.LR[2,2]
FP <- confusion_matrix.LR[2,1]
TN <- confusion_matrix.LR[1,1]
FN <- confusion_matrix.LR[1,2]

precsion <- TP/(TP+FP) #0.6111111
precsion
recall <- TP/(TP+FN) #0.2682927
recall


accu.LR<-accuracy(actual.LR, predicted.LR, threshold = 0.5)
accu.LR



#------------------------------------
# Step 16 :ADABOOST
#------------------------------------
###adaboost
#install.packages('adabag')
library(adabag)
filter <-sample.split(bankruptcy.prepared$class , SplitRatio = 0.70)
df_train <-subset(bankruptcy.prepared ,filter == TRUE)
df_test <-subset(bankruptcy.prepared ,filter == FALSE)

head(df_train)
colnames(df_train)
str(df_train)

# Train set
dim(df_train) # 4137   21
table(df_train$class)  # [class 0]:3850  [class 1]:287 

# Test set
dim(df_test)  # 1773   21
table(df_test$class)   # [class 0]:1650  [class 1]:123 

# ADABOOST - build the model
model <- boosting(class~., data=df_train, boos=TRUE, mfinal=100)
View(model)
print(names(model))

print(model$trees[1])

predicted.ADA = predict(model, df_test)


#BOOSTED CONFUSION MATRIX
print(predicted.ADA$confusion)
confusion.matrix.ADA<-predicted.ADA$confusion

#               Observed Class
#Predicted Class    0    1
#               0 1629   69
#               1   21   54

#AFTER BOOSTING THE ALGORITHEM HAVE ERROR OF 3.9%
print(predicted.ADA$error)
#AFTER BOOSTING THE ALGORITHEM HAVE correction of 96.05%
auc.ADA<-(1-predicted.ADA$error)
auc.ADA ##0.9605



TP <- confusion.matrix.ADA[2,2] #54
FP <- confusion.matrix.ADA[2,1] #26
TN <- confusion.matrix.ADA[1,1] #1624
FN <- confusion.matrix.ADA[1,2] #69
#×œ×‘×“×•×§ 
precsion <- TP/(TP+FP) 
precsion # 0.8192771
recall <- TP/(TP+FN)
recall #0.5528455

#rocADA <- roc(df_test$class, as.numeric(pred$class),direction = "<" , levels = c(0,1))
#print(auc)##0.7719

#####################################################
#Step 17: TRY TO BALANCE THE DATA
#####################################################
#install.packages("DMwR")
library(DMwR)

## SMOTE : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(class ~., bankruptcy.prepared, perc.over = 1000, k = 5, perc.under = 100)
table(balanced.data$class)
ggplot(bankruptcy.prepared, aes(class)) + geom_bar(fill = aes("#FF6666","#7CAE00"))
View(balanced.data)

filter.balance <-sample.split(balanced.data$class , SplitRatio = 0.70)
df_train.balance <-subset(balanced.data ,filter == TRUE)
df_test.balance <-subset(balanced.data ,filter == FALSE)
##############################
#Step 18: TREE BAG MODEL
##############################
tbmodel <- train(class ~., data = df_train.balance, method = "treebag")

predictors <- names(df_train.balance)[names(df_train.balance) != 'class']
predicted.TB <- predict(tbmodel$finalModel, df_test.balance[,predictors])
actual.TB<-df_test.balance$class
#confusion matrix
confusion_matrix.TB <- table(predicted.TB  , actual.TB)
confusion_matrix.TB


TP <- confusion_matrix.TB[2,2]
FP <- confusion_matrix.TB[2,1]
TN <- confusion_matrix.TB[1,1]
FN <- confusion_matrix.TB[1,2]

precsion <- TP/(TP+FP) #0.6111111
precsion
recall <- TP/(TP+FN) #0.2682927
recall


#############################
#Step 22: naive bayes
#############################

## try to run NB again
dim(df_train.balance)

model.NB.balance <- naiveBayes(df_train.balance[,-20],df_train.balance$class)
#prediction on the test set 
prediction.NB.balance <- predict(model.NB.balance,df_test.balance[,-20], type = 'raw')
prediction_bankrupty.NB.balance <- prediction.NB.balance[,"1"]
actual.NB.balance<-  df_test.balance$class



predicted.NB.balance <-prediction_bankrupty.NB.balance >0.5 #Our Assumption
predicted.NB.balance<-as.numeric(predicted.NB.balance)
table(predicted.NB.balance)

#confusion matrix 
confi_matrix.NB.balance <- table(predicted.NB.balance, actual.NB.balance)
confi_matrix.NB.balance
#                    actual.NB.balance
#predicted.NB.balance    0    1
#                     0 1482   65
#                     1  168   58

TP.balance <- confi_matrix.NB.balance[2,2]
FP.balance <- confi_matrix.NB.balance[2,1]
TN.balance <- confi_matrix.NB.balance[1,1]
FN.balance <- confi_matrix.NB.balance[1,2]
#×œ×‘×“×•×§ 
precsion.balance <- TP.balance/(TP.balance+FP.balance) #0.5
precsion.balance
recall.balance <- TP.balance/(TP.balance+FN.balance) #0.7073171
recall.balance

accu.NB.balance<-accuracy( predicted.NB.balance,actual.NB.balance , threshold = 0.5)
accu.NB.balance



colnames(balanced.data)




#------------------------------------------------------
# Step 17: Rebuild ROC chart to compare between models
#------------------------------------------------------

rocNB<-roc(actual.NB  ,as.numeric(predicted.NB ), direction = "<" , levels = c(0,1) )
rocDT <- roc(actual.DT ,predicted.DT , direction = "<" , levels = c(0 , 1) )
rocRF <-roc(actual.RF  ,as.numeric(predicted.RF ), direction = "<" , levels = c(0,1) )
rockNN <-roc(actual.kNN  ,as.numeric(kNN.20 ), direction = "<" , levels = c(0,1) )
rocLR <- roc(actual.LR ,predicted.LR, direction = "<" , levels = c(0 , 1) )
rocADA <- roc(df_test$class, as.numeric(predicted.ADA$class),direction = "<" , levels = c(0,1))
rocTB <- roc(df_test.balance$class, as.numeric(predicted.TB),direction = "<" , levels = c(0,1))
rocNB.After_Balance<-roc(actual.NB.balance  ,predicted.NB.balance, direction = "<" , levels = c(0,1) )


plot(rocNB , col = 'red' , main = 'ROC chart')

par(new=TRUE)

plot(rocDT , col = 'blue' , main = 'ROC chart')

par(new=TRUE)

plot(rocRF , col = 'pink' , main = 'ROC chart')

par(new=TRUE)
plot(rockNN , col = 'orange' , main = 'ROC chart')

par(new=TRUE)
plot(rocLR , col = 'black' , main = 'ROC chart')

par(new=TRUE)

plot(rocADA , col = 'green' , main = 'ROC chart')
par(new=TRUE)

plot(rocTB , col = 'yellow' , main = 'ROC chart')
par(new=TRUE)

plot(rocNB.After_Balance , col = 'Purple' , main = 'ROC chart')

auc(rocNB) 
auc(rocDT) 
auc(rocRF) 
auc(rockNN) 
auc(rocLR) 
auc(rocADA) 
auc(rocTB) 
auc(rocNB.After_Balance)





##################################
#Step 18:  Neural nets
##############################
require(e1071)
require(neuralnet)
require(parallel)
require(ggplot2)
require(gridExtra)
library(ggthemes)
#install.packages("neuralnet")
#install.packages("ggthemes")
train = df_train
test = df_test
train$class = as.factor(train$class)
test$class = as.factor(test$class)


train2 = train
test2 = test
colnames(train)

train = data.frame(train[,c(1:6,8:14)])

test = data.frame(test[,c(1:6,8:14)])
train[,14] = train2[,7]
test[,14] = test2[,7]
# I think we only need e1071 library



xnames = colnames(train2)
xnames = xnames[c(1:6,8:14)]
nntrain = data.frame(train2[c(1:6,8:14)])
nntest = data.frame(test2[c(1:6,8:14)])
nntrain$class = train2[,7]
nntest$class = test2[,7]

# have to break down V58 into 2 columns
nntrain2 = data.frame(model.matrix(~factor(nntrain$class)-1))
nntest2 = data.frame(model.matrix(~factor(nntest$class)-1))


#Now see if we can center the data:
nntrain4 = scale(data.frame(nntrain[,1:12]))
nntest4 = scale(data.frame(nntest[,1:12]))


nntrain5 = data.frame(nntrain4)
nntest5 = data.frame(nntest4)


# Add back the two columns of BK and non-BK
nntrain6 = cbind(nntrain5, nntrain2)
nntest6 = cbind(nntest5, nntest2)
colnames(nntrain6)[13] = "Not_BK"
colnames(nntrain6)[14] = "BK"
colnames(nntest6)[13] = "Not_BK"
colnames(nntest6)[14] = "BK"


# Now we do NNEts

n = names(nntrain6[,1:12])
f = as.formula(paste("BK + Not_BK ~", paste(n, collapse = " + ")))
# Does this work? No. 2, and 3 layer NNets don't work.
# nn1 = neuralnet(f, data= nntrain6, hidden= 10, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)



pred = function(nn, dat) {
  yhat = compute(nn, dat)$net.result
  yhat = apply(yhat, 1, round) # rounding is better
  return(yhat)
}


hid = 3:30

fit_NN = function(hid){
  NN = neuralnet(f, data = nntrain6, hidden= hid, threshold = 0.1, linear.output = F, lifesign = "full", lifesign.step = 100)
  return(NN)
}

NN = mclapply(hid,fit_NN)
NN1 = NN[1]
NN2 = NN[2]
NN3 = NN[3]
NN4 = NN[4]
NN5 = NN[5]
NN6 = NN[6]
NN7 = NN[7]
NN8 = NN[8]
NN9 = NN[9]
NN10 = NN[10]
NN11 = NN[11]
NN12 = NN[12]
NN13 = NN[13]
NN14 = NN[14]
NN15 = NN[15]
NN16 = NN[16]
NN17 = NN[17]
NN18 = NN[18]
NN19 = NN[19]
NN20 = NN[20]
NN21 = NN[21]
NN22 = NN[22]
NN23 = NN[23]
NN24 = NN[24]
NN25 = NN[25]
NN26 = NN[26]
NN27 = NN[27]
NN28 = NN[28]


require(neuralnet)
require(NeuralNetTools)
require(nnet)
#install.packages("NeuralNetTools")
#install.packages("nnet")
#load("saved_output/objects/nn8.RData")

png(filename = "saved_output/plots/nn.png")
plotnet(NN3[[1]])
dev.off()

Mean_test_error = as.list(rep(0, 26))
Mean_train_error = as.list(rep(0, 26))
table_test = list()
table_train = list()

for(i in 1:26){
  train_result = as.matrix(t(pred(NN[[i]], nntrain6[,1:12])), nrow = length(nntrain6[,1]), ncol = 2)
  test_result = as.matrix(t(pred(NN[[i]], nntest6[,1:12])), nrow = length(nntest6[,1]), ncol= 2)
  table_test[[i]] = table(test_result[1:length(nntest[,1])],  nntest6$BK)
  table_train[[i]] = table(train_result[1:length(nntrain[,1])],  nntrain6$BK)
  Mean_test_error[[i]] = mean(test_result != nntest6[,c("BK","Not_BK")])
  Mean_train_error[[i]] = mean(train_result != nntrain6[,c("BK","Not_BK")])
}
View(table_train)



## FALSE POSITIVE RATE:
test_FPR_NNet = sapply(table_test, function(x) {1 -  x[2,2]/(x[2,1]+x[2,2])})
train_FPR_NNet = sapply(table_train, function(x) 1 -  x[2,2]/(x[2,1]+x[2,2]))

df_FPR_NNet = data.frame(Error = c(test_FPR_NNet,train_FPR_NNet), 
                         Set = c(rep("Test",26),rep("Train",26)),
                         Nodes = rep(seq(from = 3,to = 28, by = 1),2))


## FALSE NEGATIVE RATE:

testing_FNR_NNet = sapply(table_test, function(x) x[1,2]/(x[1,1]+x[1,2]))
training_FNR_NNet = sapply(table_train, function(x) x[1,2]/(x[1,1]+x[1,2]))

df_FNR_NNet = data.frame(Error = c(testing_FNR_NNet,training_FNR_NNet), 
                         Set = c(rep("Test",26),rep("Train",26)),
                         Nodes = seq(from = 3,to = 28, by = 1))
g1 = ggplot(df_FNR_NNet, aes(x = Nodes, y = Error, color = Set)) + 
  geom_line(size = 1) + 
  labs(title = "False Positive Rate/\nTesting&Training Sets NNet") +
  theme_economist()%+replace%theme(legend.position ="bottom",
                                   legend.direction = "vertical",
                                   legend.title = element_blank())+scale_color_fivethirtyeight()


g2 = ggplot(df_FNR_NNet, aes(x = Nodes, y = Error, color = Set)) + 
  geom_line(size = 1) + 
  labs(title = "False Positive Rate/\nTesting&Training Sets NNet") +
  theme_economist()%+replace%theme(legend.position ="bottom",
                                   legend.direction = "vertical",
                                   legend.title = element_blank())+scale_color_fivethirtyeight()

mean_test_error_matrix = rep(0, 26)
mean_train_error_matrix = rep(0, 26)
for(i in 1:26){
  mean_test_error_matrix[i] = Mean_test_error[[i]]
  mean_train_error_matrix[i] = Mean_train_error[[i]]
}

df_MTE_NNet = data.frame(Error = c(mean_test_error_matrix,mean_train_error_matrix), 
                         Set = c(rep("Test",26),rep("Train",26)),
                         Nodes = seq(from = 3,to = 28, by = 1))

g3 = ggplot(df_MTE_NNet, aes(x = Nodes, y = Error, color = Set)) + 
  geom_line(size = 1) + 
  labs(title = "False Positive Rate/\nTesting&Training Sets NNet") +
  theme_economist()%+replace%theme(legend.position ="bottom",
                                   legend.direction = "vertical",
                                   legend.title = element_blank())+scale_color_fivethirtyeight()


ggg = grid.arrange(g1,g2,g3, ncol = 3, nrow = 1)
ggsave(filename = "saved_output/plots/nn_plots.png", plot = ggg, device = "png")





