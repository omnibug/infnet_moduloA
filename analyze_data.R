# Bloco: Business Intelligence e Big Data Analytics: Valor
# Disciplina: Big Data Analytics com R
# Analysis Phase

#clear environment
rm(list=ls())
#clear Console = ctrl+l

# -------------------------
# install necessary packages
# -------------------------

# install.packages("RColorBrewer", dependencies=TRUE)
# install.packages("ggplot2", dependencies=TRUE)
# install.packages("scatterplot3d", dependencies=TRUE)
# install.packages("lattice", dependencies=TRUE)
# install.packages("party", dependencies=TRUE)
# install.packages("tm", dependencies=TRUE)
# install.packages("SnowballC", dependencies=TRUE)
# install.packages("wordcloud", dependencies=TRUE)
# install.packages("corrplot", dependencies=TRUE)
# install.packages("deldir", dependencies=TRUE)
# install.packages(c("sp", "maps", "maptools", "mapproj"), dependencies=TRUE)
# install.packages("reshape", dependencies=TRUE)

# loading packages
library(dplyr) # bind rows
require('RColorBrewer') # color paletes for graphs
library(reshape)
library(ggplot2)

# SET THE HOME DIR FOR WINDOWS OR LINUX ENVIRONMENTS
homeDir='C:/Users/Carlos/Documents/MEGAsync/MIT/Modulo_A_Valor/Atual/infnet_moduloA/'
#homeDir='/home/carlos/MEGAsync/MIT/Modulo_A_Valor/Atual/infnet_moduloA/' 
setwd(homeDir)

# loading common functions
library(stringr) # string manipulation
source('functions/fn_load_data.R') # calculate prediction metrics
source('functions/fn_check_missing_data.R') # check for missing data

# Common function to plot graphics - in the future create a sourced funtion for reusability
openPNG <- function(imagename){
  png(filename = paste(getwd(),'/graphs/',imagename,'.png', sep = ''),
      width = 670, height = 379)
}
# -------------------------
# MAIN
# -------------------------

# load data sets
load_data()

# check for missing data
check_missing_data(full)

# -------------------------
# BOXPLOTS
# -------------------------

# DATASET: Study
#study_habits <- read.csv("train_data.csv")
#study_habits_test <- read.csv("test_data.csv")

#head(descr)

STG <- subset(descr, Attribute == "STG", select=c(Description),1)
SCG <- subset(descr, Attribute == "SCG", select=c(Description),1)
STR <- subset(descr, Attribute == "STR", select=c(Description),1)
LPR <- subset(descr, Attribute == "LPR", select=c(Description),1)
PEG <- subset(descr, Attribute == "PEG", select=c(Description),1)
UNS <- subset(descr, Attribute == "UNS", select=c(Description),1)

# create a unique dataframe with the count of distinct values for each column
unique <- apply(full, 2, function(x)length(unique(x)))
uniqueDf <- cbind(read.table(text = names(unique)), unique)

# Opens the graphics file
openPNG('01_all_features_barplot')
# Basic Barplot
my_bar=barplot(uniqueDf$unique , border=F , names.arg=uniqueDf$V1 , las=2 , col=brewer.pal(6,"Accent") , ylim=c(0,125) , main="" )
abline(v=c(6.1) , col="grey")
# Add the text 
text(my_bar, uniqueDf$unique+4.04 , paste("n = ",uniqueDf$unique,sep="") ,cex=1) 
# Closes the graphics file
dev.off()


# example of melt function 
fullPivot <- melt(full[1:5])

summary(full$STG)
summary(full$SCG)
summary(full$STR)
summary(full$LPR)
summary(full$PEG)
summary(full$UNS)

# plot boxplots
# Opens the graphics file
openPNG('02_full_pivot_boxplots')
ggplot(fullPivot, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")
# Closes the graphics file
dev.off()

# Boxplots for each dataset independent variable
openPNG('03_STG_boxplot')
boxplot(STG~UNS, data=train, main=STG
        ,horizontal=TRUE # Horizontal boxplot
        ,las = 1 # Make all labels horizontal
        ,col=brewer.pal(5,"Accent") 
        ,notch = TRUE # nOTCHES FOR CI for median
        ,boxwex = 0.5 # Width of box as proportion of original
        ,whisklty = 1 # Whisker line type; 1 = solid line
        ,staplelty =0 # Staple - line at the end; 0 - none
        )
# Closes the graphics file
dev.off()
# Boxplots for each dataset independent variable
openPNG('04_SCG_boxplot')
boxplot(SCG~UNS, data=train, main=SCG
        ,horizontal=TRUE # Horizontal boxplot
        ,las = 1 # Make all labels horizontal
        ,col=brewer.pal(5,"Accent") 
        ,notch = FALSE # Notches FOR CI for median
        ,boxwex = 0.5 # Width of box as proportion of original
        ,whisklty = 1 # Whisker line type; 1 = solid line
        ,staplelty =0 # Staple - line at the end; 0 - none
        )
# Closes the graphics file
dev.off()
# Boxplots for each dataset independent variable
openPNG('05_STR_boxplot')
boxplot(STR~UNS, data=train, main=STR
        ,horizontal=TRUE # Horizontal boxplot
        ,las = 1 # Make all labels horizontal
        ,col=brewer.pal(5,"Accent") 
        ,notch = TRUE # nOTCHES FOR CI for median
        ,boxwex = 0.5 # Width of box as proportion of original
        ,whisklty = 1 # Whisker line type; 1 = solid line
        ,staplelty =0 # Staple - line at the end; 0 - none
        )
# Closes the graphics file
dev.off()
# Boxplots for each dataset independent variable
openPNG('06_LPR_boxplot')
boxplot(LPR~UNS, data=train, main=LPR
        ,horizontal=TRUE # Horizontal boxplot
        ,las = 1 # Make all labels horizontal
        ,col=brewer.pal(5,"Accent") 
        ,notch = TRUE # nOTCHES FOR CI for median
        ,boxwex = 0.5 # Width of box as proportion of original
        ,whisklty = 1 # Whisker line type; 1 = solid line
        ,staplelty =0 # Staple - line at the end; 0 - none
        )
# Closes the graphics file
dev.off()
# Boxplots for each dataset independent variable
openPNG('07_PEG_boxplot')
boxplot(PEG~UNS, data=train, main=PEG
        ,horizontal=TRUE # Horizontal boxplot
        ,las = 1 # Make all labels horizontal
        ,col=brewer.pal(5,"Accent") 
        ,notch = FALSE # nOTCHES FOR CI for median
        ,boxwex = 0.5 # Width of box as proportion of original
        ,whisklty = 1 # Whisker line type; 1 = solid line
        ,staplelty =0 # Staple - line at the end; 0 - none
        )
# Closes the graphics file
dev.off()

# -------------------------
# HISTOGRAMAS
# -------------------------

# DATASET: study habits
# Olhando os histogramas como densidades
plot(density(train$STG),main=STG)
plot(density(train$SCG),main=SCG)
plot(density(train$STR),main=STR)
plot(density(train$LPR),main=LPR)
plot(density(train$PEG),main=PEG)

# -------------------------
# MATRIZ DE SCATTERPLOTS
# -------------------------

# DATASET: study_habits
#------

# Fazendo mais uma matriz de scatterplots,
# incluindo a correla??o no painel superior mas agora com proporcionalidade.
openPNG('08_featscattcorr')
panel.corprop <- function(x, y, ...)
{
  par(usr = c(0, 1, 0, 1))
  txt <- as.character(format(cor(x, y), digits=2))
  text(0.5, 0.5, txt, cex = 6* abs(cor(x, y)))
}
pairs(train[1:5], main="Study Habits Dataset With Correlations", pch=21, bg=c("red","green3","blue")[unclass(train$UNS)], upper.panel=panel.corprop)
# Closes the graphics file
dev.off()

