# library(ggplot2);library(reshape2)
# require("Rmisc")
# require(Hmisc)
# library(scales); library(Kendall)
# setwd('/home/saguinag/CategoryPaths/')
# 
# data_path   = 'gbSP.dat'
# gbHP_dat <-read.csv("gbHP.dat", na.strings ='inf')
# gbCP_dat <-read.csv("gbCP.dat", na.strings ='inf')
# gbSP_dat <-read.csv("gbSP.dat", na.strings ='inf')
# gbPP_dat <-read.csv("gbPP.dat", na.strings ='inf')
# df <- data.frame(gbHP_dat)
# df$
# head(df)
# 
# # p <- ggplot(df, aes(x=seq(1,nrow(df)), y=df$clicks)) + geom_point(shape=1) +
# # geom_line()
# # 
# # 
# # 
# # p df <- data.frame(gbsP_dat)
library(ggplot2);library(reshape2)
require("Rmisc")
require(Hmisc)
library(scales); library(Kendall)
setwd('/data/tweninge/')

data_path   = 'gbSP.dat'
gbSP_dat <-read.csv('gbSP.dat', na.strings ='inf')
gbHP_dat <-read.csv('gbHP.dat', na.strings ='inf')
gbCP_dat <-read.csv('gbCP.dat', na.strings ='inf')
gbPP_dat <-read.csv('gbPP.dat', na.strings ='inf')

sp = gbSP_dat$clicks
hp = gbHP_dat$sp
pp = gbPP_dat$prob
cp = gbCP_dat$sp

length(sp) = 30
length(hp) = 30
length(cp) = 30

hist(pp)



zz = cbind(seq(1,30), sp, hp, cp, pp)
zz[is.na(zz)] <- 0
df = data.frame(zz)

df$sp = df$sp/sum(df$sp)
df$hp = df$hp/sum(df$hp)
df$cp = df$cp/sum(df$cp)
df$pp = df$pp/sum(df$pp)

binned.x <- cut(df$pp, 30)


ggplot(df, aes(V1)) + 
  geom_line(aes(y = sp, colour = "var0")) + geom_point(aes(y = sp, colour = "var0"))+
  geom_line(aes(y = hp, colour = "var1")) + geom_point(aes(y = hp, colour = "var1"))+
  geom_line(aes(y = pp, colour = "var2")) + geom_point(aes(y = pp, colour = "var2"))+
  geom_line(aes(y = cp, colour = "var3")) + geom_point(aes(y = cp, colour = "var3"))