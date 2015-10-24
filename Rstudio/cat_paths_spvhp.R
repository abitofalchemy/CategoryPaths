## Category Paths
## Author: saguinag

## Description: read and plot SP vs HP
setwd('/home/saguinag/CategoryPaths/')

## One file test
# example of wikipediagames
gameDataFrame <- read.csv(file='gamesDatafiles/1018340_wpgame_games.dat', header=TRUE, sep=',')
head(gameDataFrame)

hpDataFrame <- read.csv(file='humanpathsDatafiles/1018340_wpgame_games.dataframe', header=TRUE, sep=',')
head(hpDataFrame)

## Plot Data
library("ggplot2")
#  Histogram of human clicks
qplot(hpDataFrame$clicks,binwidth=0.5) + xlab('Clicks')+ggtitle('Number of Clicks per User/Game')

## Plot of clicks vs sp
spDatFrm <- read.csv(file='ssspGamesDatFiles/1018340_games_sp.dat', header=TRUE, sep=',') 
head(spDatFrm)
qplot(spDatFrm$sp, binwidth=0.5) + xlab('Clicks')+ggtitle('Shortest Paths')

## Read games, clicks, sp
gmHpSpData = read.csv("output_hpvssp.txt", header=TRUE, sep=',')
head(gmHpSpData)
## plot all distributions of sp and hp
qplot(gmHpSpData$sp, gmHpSpData$clicks, binwidth=0.5) + xlab('Shortest Paths')

## Ref: http://www.r-tutor.com/gpu-computing/correlation/kendall-tau-b

