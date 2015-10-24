## Category Paths and SSSPs
library(ggplot2);library(reshape2)
# setup
getwd()
setwd("~/CategoryPaths/")

## Looking a the shortest paths;  

## Looking at category paths
cp_sp_path = 'ourAlgoScores/'
# single file in
cp_sp_dat <-read.csv('ourAlgoScores/1018340_sp_cp.csv')
# list of filenames
file.names <- list.files(cp_sp_path,pattern='*.csv')
data = data.frame()
for(i in 1:length(file.names)){
  cp_sp_dat <-read.csv(paste(cp_sp_path,file.names[i],sep = ''),
                       na.strings ='inf', 
                       colClasses=c('key'='factor','sp'='integer','cp'= 'integer'))
  df <- data.frame(cp_sp_dat)
  data <- rbind(data,df)
}
# sapply(cp_sp_dat, class)
summary(data)
g0 <- ggplot(data,aes(sp,cp))+geom_point(color="firebrick", alpha=.5,) + 
      xlim(0.5,5.5) 
g0
# Overlaid histograms
all_dat <- melt(data)
g1<-ggplot(all_dat, aes(x=value, fill=variable)) + 
  geom_histogram(binwidth=0.5, alpha=.5, position="identity") + 
  xlim(0,20) 
g1

## Wolcoxon Ranksum test
wilcox.test(data$sp, data$cp, paired=TRUE)

## Kendall's Tau Test

m<-cbind(data$sp,data$cp)
cor(m, method="kendall",use='pairwise')

cor.test(data$sp,data$cp, method='kendall')

## Human paths vs category paths
hp_sp_file = 'dataFiles/8083_hp_guuid_uuid_sssp.csv'
# single file in
hp_sp_dat <-read.csv(hp_sp_file,col.names=c('hp','game','user','sp'))
df <- data.frame(hp_sp_dat)
head(df)
summary(df$hp)


## Processing all the files
hp_sp_usr_game_path = 'dataFiles/'
file.names <- list.files(hp_sp_usr_game_path, pattern='*.csv')
data1 = data.frame()
for(i in 1:length(file.names)){
  cp_sp_dat <-read.csv(paste(hp_sp_usr_game_path,file.names[i],sep = ''),
                       na.strings ='inf', col.names=c('hp','game','user','sp'),
                       colClasses=c('hp'='integer','game'='factor','user'='factor','sp'='integer'))
  df1 <- data.frame(cp_sp_dat)
  data1 <- rbind(data1,df1)
}

## show the distribution of the paths
summary(data1)
p<-ggplot(data1, aes(x=sp, y=hp)) +
  geom_point(shape=21, show_guide=FALSE, color="firebrick", alpha=.5) +  
  scale_y_log10() + annotation_logticks(sides = "l")
p

##
## Category Path vs Human Paths
##
cp_hp_in_file = '~/CategoryPaths/hpGmUserCPdataFiles/hpGmUsersCP.csv'
data2 = data.frame()
cp_hp_dat <-read.csv(cp_hp_in_file, sep = ',',header=TRUE,
                       na.strings ='inf', 
                       colClasses=c('cp'='integer','hp'='integer','key'='factor','user'='factor'))
df1 <- data.frame(cp_hp_dat)
df1$ix<-NULL
head(df1)


## show the distribution of the paths
data2 <- rbind(data2,df1)
summary(data2)
ggplot(data2, aes(x=cp, y=hp)) +
  geom_point(shape=21, show_guide=FALSE, color="firebrick", alpha=.5) +  scale_y_log10() + 
  annotation_logticks(sides = "l") + coord_cartesian(xlim = c(0, 30))



# Overlaid histograms: Category Paths and Human Paths
cp_hp_mdat <- melt(data2);
head(cp_hp_mdat)
ggplot(cp_hp_mdat, aes(x=value, fill=variable)) + 
  geom_histogram(binwidth=0.5, alpha=.5, position="identity") + 
  xlim(0,30) + xlab('Path Length (# hops)') 


## describe the dataframe
# bubble plots if interested: http://www.matthewmaenner.com/blog/2010/11/23/how-to-make-bubble-charts-in-ggplot2/
# http://docs.ggplot2.org/0.9.3.1/geom_point.html
require(plyr)
cp_pl_cnt<-ddply(df1, .(cp), summarize, NumPathLengths = length(unique(key)))
hp_pl_cnt<-ddply(df1, .(hp), summarize, NumPathLengths = length(unique(key)))

p0 <- ggplot(cp_pl_cnt, aes(cp, NumPathLengths)) + geom_point(aes(size = NumPathLengths,colour = NumPathLengths)) +  scale_x_log10(limits = c(1,30)) + annotation_logticks(sides = "b")
ggsave(file="../public_html/test.svg", plot=p, width=9, height=6)
p1 <- ggplot(hp_pl_cnt, aes(hp, NumPathLengths)) + geom_point(aes(size = NumPathLengths,colour = NumPathLengths)) +  scale_x_log10() + annotation_logticks(sides = "b")
ggsave(file="../public_html/test.svg", plot=p, width=9, height=6)

# References
# 1 http://shiny.rstudio.com/tutorial/quiz/

