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
colnames(tdf)
colnames(tdf) <- c("HP", "CatPath", "SP", "x")
#ggplot(tdf, aes(x=x)) + geom_histogram(data=subset(tdf,c(clicks)), fill="red", alpha = 0.2, binwidth=0.5)
#ggplot(tdf, aes(x=x)) + geom_histogram(binwidth = 0.5)

## Everything on the same plot
## Ref: http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
d<- melt(tdf, id.vars="x")
#ggplot(d, aes(x=value, fill=variable)) + geom_histogram(binwidth=0.5) 


## box plots / http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/
bp <- ggplot(d, aes(x=variable,y=value, fill=variable)) + 
  geom_boxplot() + guides(fill=FALSE) +
  scale_x_discrete(limits=c("HP","CatPath","SP")) +
  ggtitle("Plot of Path length \n Human, CatPath, and Shortest Paths ") +
  xlab("Length Metric") + ylab("Path Length") + ylim(0,15)
bp

## Histogram and ditribution
ggplot(d, aes(x=value, fill=variable)) + geom_histogram(binwidth=0.5, ) +
  facet_grid(variable~ .) + xlim(0, 15) + guides(fill=FALSE) + 
  xlab("Path Length") + ylab("Frequency")

## Node Similarity
in_path   = 'wiki_data/hops_and_catpath_length.csv'
dat <-read.csv(in_path, header=F, na.strings ='inf',strip.white=TRUE)
cpdf <- data.frame( dat )
cppf <- cpDf[complete.cases(cpdf), ]

cpdf$hops<- gsub("\\[|\\]", "", cpdf$V1)
a <- as.vector(cpdf$hops)[1]
## Convert factor to vector 
