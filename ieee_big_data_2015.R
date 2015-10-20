## IEEE Big Data 2015 
## Summary Plots

library(ggplot2);library(reshape2)
# require("Rmisc")
# require(Hmisc)
# library(scales); library(Kendall)
setwd('/home/saguinag/CategoryPaths/')
#hpcpsp_path = 'cat_ppr_DataFiles/kgcupscp.csv'

## file includes the hops
data_path   = 'pathsHopsFiles/kgcupscp_ppr_hops.csv'
hpcpsp_dat <-read.csv(data_path, na.strings ='inf')
df <- data.frame(hpcpsp_dat)
dim(df)
## filter data.frame
df <- df[df$prob>0,]
## filter records that have paths below 30
df <- df[df$cp<30,]
df <- df[df$clicks<30,]
## Test data.frame for nans
## http://stackoverflow.com/questions/5961839/remove-row-with-nan-value
df <-df[complete.cases(df), ]

## Selecting a few rows: df[1:5, ]
tdf   <- df[1:1000,]
tdf$x <- row.names(tdf)
tdf <- subset(tdf, select = -c(game, key,usr,prob, hops) )
#ggplot(tdf, aes(x=x)) + geom_histogram(data=subset(tdf,c(clicks)), fill="red", alpha = 0.2, binwidth=0.5)
#ggplot(tdf, aes(x=x)) + geom_histogram(binwidth = 0.5)


# 
# #ggplot(tdf, aes(x=x, fill=c(sp,clicks))) + geom_histogram(
# # Everything on the same plot
d<- melt(tdf, id.vars="x")
ggplot(d, aes(x=value, fill=variable)) + geom_histogram(binwidth=0.5) 
ggplot(d, aes(x=value, fill=variable)) + geom_histogram(binwidth=0.5) +
  facet_grid(variable~ .)
