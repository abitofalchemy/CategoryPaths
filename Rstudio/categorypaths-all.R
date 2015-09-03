setwd('/home/saguinag/CategoryPaths/scripts/')
# Read CSV: human paths
hpData <- read.csv(file='../humanpathDatafiles/1018340_wpgame_games.dataframe', header=TRUE, sep=',')
pwd
wd
ls
getwd()
setwd('/home/saguinag/CategoryPaths/')
getwd()
hpData <- read.csv(file='humanpathsDatafiles/1018340_wpgame_games.dataframe', header=TRUE, sep=',')
summary(hpData)
library("ggplot2")
ggplot(data=hpData, aes(x=clicks)) + geom_point()
ggplot(data=hpData, aes(x=clicks,y=game)) + geom_point(shape=1)
ggplot(data=hpData, aes(x=clicks)) + geom_point(shape=1)
ggplot(data=hpData, aes(y=clicks)) + geom_point(shape=1)
ggplot(data=hpData, aes(y=clicks)) + geom_point(shape=1)
library(gridExtra)
qplot(hpData$clicks,binwidth=.5)
# On plotting: http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
qplot(hpData$clicks,binwidth=.5) + xlab("Human Paths (number of clicks)")
qplot(hpData$clicks,binwidth=.5) + xlab("Clicks")+ggtitle('Human Paths (wikipediagame)')
# Embeding plots: https://support.rstudio.com/hc/en-us/articles/200552086-Using-R-Markdown
```
temp = list.files(pattern=)
temp = list.files(pattern="/home/saguinag/CategoryPaths/ssspGamesDatFiles/*.dat")
temp
setwd(""/home/saguinag/CategoryPaths/ssspGamesDatFiles/")
;
""
;
";"
setwd("/home/saguinag/CategoryPaths/ssspGamesDatFiles/")
temp = list.files(*.dat)
temp = list.files("*.dat")
temp
temp <-list.files("*.dat")
temp
temp <-list.files()
temp
save.image("~/CategoryPaths/scripts/categorypaths-all.R.RData")
savehistory("~/CategoryPaths/scripts/categorypaths-all.R")
