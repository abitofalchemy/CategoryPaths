library(ggplot2);library(reshape2)
require("Rmisc")
library("scales")
setwd('/home/saguinag/CategoryPaths/')
hpcpsp_path = 'cat_ppr_DataFiles/kgcupscp.csv'
hpcpsp_dat <-read.csv(hpcpsp_path, na.strings ='inf')
df0 <- data.frame(hpcpsp_dat)

## Basic analysis
# Fit a linear model to the data and save the model object: 
mod <- lm(cp ~ clicks, data = df0) 

# Create a list of character strings - the first component 
# produces the fitted model, the second produces a 
# string to compute R^2, but in plotmath syntax. 
if (lmp(mod) < 0.001) { 
  pval <- 0.001
} else if (lmp(mod) < 0.05) {
  pval <- 0.05
} else {
  pval <- lmp(mod)
}
pval

rout <- list(paste('Fitted model: ', round(coef(mod)[1], 3), ' + ', 
                   round(coef(mod)[2], 3), ' x', sep = ''), 
             paste('R^2 == ', round(summary(mod)[['r.squared']], 3), 
                   sep = ''),
             paste('p < ', round(pval,3), sep = '')  ) 

# This looks ugly, but I'm using the round() function to make the 
# equations look more sane. coef(mod) extracts the model 
# coefficients (intercept, then slope), and 
# summary(mod)[['r.squared']] extracts R^2. 

# See what they look like: 
rout 

## 
ggplot(df0, aes(x=cp, y=clicks)) +
  geom_point(alpha=.5,color = "cornflowerblue") + xlab('Category Paths (L)') + 
  ylab('Human Paths (L)') +
  xlim(0,20) + ylim(0,30) + geom_smooth(method = 'lm') +   
  #geom_text(data = data.frame(), aes(4.5, 30, label = "Pearson-R = -.87")
  geom_text(data = data.frame(), aes(x = 20, y = 30, label = rout[[1]]), hjust = 1) + 
  geom_text(data = data.frame(), aes(x = 20, y = 28.5, label = rout[[2]]), hjust = 1) +
  geom_text(data = data.frame(), aes(x = 20, y = 27, label = rout[[3]]), hjust = 1) +
  theme_classic() 
ggsave(file="plotFiles/figure1a.pdf",width=6, height=4)
## 
shapiro.test(df0$clicks)
corr

fit <- lm(df0$clicks ~ df0$cp)
rSquared <- summary(fit)$r.squared
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- lmp(fit)
rSquared
pVal
# 
# ## http://stackoverflow.com/questions/13114539
# ## Calculate RMSE and other values
# rmse <- round(sqrt(mean(resid(fit)^2)), 2)
# coefs <- coef(fit)
# b0 <- round(coefs[1], 2)
# b1 <- round(coefs[2],2)
# r2 <- round(summary(fit)$r.squared, 2)
# 
# ## Build the eqn
# eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
#                 r^2 == .(r2) * "," ~~ RMSE == .(rmse))
# ## Plot the data
# plot(clicks ~ cp, data = df,  xlim=c(0,30),ylim=c(0,20))
# abline(fit)
# text(2, 10, eqn, pos = 4)

###############################################################################
###############################################################################

cp_sp_path = 'ourAlgoScores/'
file.names <- list.files(cp_sp_path,pattern='*.csv')
df0 = data.frame()
for(i in 1:length(file.names)){
  cp_sp_dat <-read.csv(paste(cp_sp_path,file.names[i],sep = ''),
                       na.strings ='inf', 
                       colClasses=c('key'='factor','sp'='integer','cp'= 'integer'))
  df <- data.frame(cp_sp_dat)
  df0 <- rbind(data,df)
}
all_dat <- melt(data);

## Fit line, pVal, and Rsquared
mod <- lm(cp ~ sp, data = df0) 

# Create a list of character strings - the first component 
# produces the fitted model, the second produces a 
# string to compute R^2, but in plotmath syntax. 
rout <- list(paste('Fitted model: ', round(coef(mod)[1], 3), ' + ', 
                   round(coef(mod)[2], 3), ' x', sep = ''), 
             paste('R^2 == ', round(summary(mod)[['r.squared']], 3), 
                   sep = ''),
             paste('p <', if (lmp(mod)< 0.001) (0.001), sep = '< ') ) 

p1b <- ggplot(data,aes(cp,sp)) + geom_point(alpha = .5, color = "cornflowerblue") + 
  xlab('Category Paths (L)') + ylab('SSSP (L)') + xlim(0,20) + 
  geom_smooth(method = 'lm',na.rm = TRUE, se=TRUE) +
  geom_text(data = data.frame(), aes(x = 1, y = 5, label = rout[[1]]), hjust = 0) + 
  geom_text(data = data.frame(), aes(x = 1, y = 4.75, label = rout[[2]]), hjust = 0) +
  geom_text(data = data.frame(), aes(x = 1, y = 4.5, label = rout[[3]]), hjust = 0) +
  theme_classic()
ggsave(p1b, file="plotFiles/figure1b.pdf",width=6, height=4)

###############################################################################
###############################################################################

hpcpsp_path = 'cat_ppr_DataFiles/kgcupscp.csv'
hpcpsp_dat <-read.csv(hpcpsp_path, na.strings ='inf')
df <- data.frame(hpcpsp_dat)
head(df)
df$cp[which(!is.finite(df$cf))] = NA # force inf/-inf to NA
## Fit line, pVal, and Rsquared
mod <- lm(cp ~ prob, data = df) 
mod
lmp(mod)

# Create a list of character strings - the first component 
# produces the fitted model, the second produces a 
# string to compute R^2, but in plotmath syntax. 
rout <- list(paste('Fitted model: ', round(coef(mod)[1], 3), ' + ', 
                   round(coef(mod)[2], 3), ' x', sep = ''), 
             paste('R^2 == ', round(summary(mod)[['r.squared']], 3), 
                   sep = ''),
             paste('p <', if (anova(mod)$'Pr(>F)'[1]< 0.001) (0.001), sep = '< ') )

p1c <- ggplot(df, aes(x=cp, y=prob)) + geom_point(color = "cornflowerblue",alpha=.2) + 
  geom_smooth(method = 'lm') +
  xlab('Category Paths (L)') + ylab('PPR (p)') + 
  ylim(1e-8,1e-3) + xlim(0,20) + 
  geom_text(data = data.frame(), aes(x = 20, y = 1e-3, label = rout[[1]]), hjust = 1) + 
  geom_text(data = data.frame(), aes(x = 20, y = 0.95e-3, label = rout[[2]]), hjust = 1) +
  geom_text(data = data.frame(), aes(x = 20, y = 0.9e-3, label = rout[[3]]), hjust = 1) +
  theme_classic()
p1c
ggsave(p1c, file="plotFiles/figure1c.pdf",width=6, height=4)


## Another way
# f <- summary(mod)$coefficients
# slope <- round(f[2], 3)
# int   <- round(f[1], 3)
# tplot<- ggplot(df, aes(cp, prob)) +
#   geom_point(color = "cornflowerblue",alpha=.2) + 
#   geom_line(aes(y=prob),lty="dotted",color="Blue")+
#   #geom_point(aes(y=prob,colour="Mean_Tmin"),shape=8,size=5)+
# #  geom_line(aes(y=av,colour="5 yrs moving average",fill=F), size=0.8)+
#   stat_smooth(method=lm)+
#   geom_abline(aes(slope=slope,intercept=int,color="red")) + 
#   xlim(0,20) + ylim(1e-8,1e-3)
# tplot

###############################################################################
###############################################################################
## Plot 2 of 4
setwd('/home/saguinag/CategoryPaths/')
hpcpsp_path = 'cat_ppr_DataFiles/kgcupscp.csv'
hpcpsp_dat <-read.csv(hpcpsp_path, na.strings ='inf')
#colClasses=c('clicks'='float','cp'='float',
#                                   'game'='factor','key'='factor',
#                                   'prob'='double','sp'='float','usr'='factor'
df <- data.frame(hpcpsp_dat)
#
df$hpcpdelta<-(df$clicks - df$cp)
df$hpspdelta <- (df$clicks -df$sp)
head(df)


hpcp<-df$hpcpdelta
hpsp<-df$hpspdelta

ds <- data.frame(catpath=hpcp,sssp=hpsp) 
p2 <- ggplot(melt(ds), aes(x = variable, y = value)) + geom_boxplot() + ylim(-1.5,7.5) + xlab("") + ylab("Difference b/w Human Paths (raw)")
## Trim/slice the data.frame and computet he pct diff 
ds <- data.frame(sp=df$sp,hp=df$clicks, cp=df$cp)
## Next, compute the pct diff b/w hp-cp, hp-sp
ds <- transform(ds, hp_v_cp = (hp-sp)/((hp+sp)/2), hp_v_sp = (hp-cp)/((hp+cp)/2))

## Reshape into long format, from {SO}/questions/16896632
ds_long <- reshape2:::melt.data.frame(ds,
                                      id.vars=c("sp","hp","cp"),
                                      measure.vars=c("hp_v_cp","hp_v_sp"),
                                      variable.name=c("PathDiff"),
                                      value.name="pctdiff"
                                      )

## Summary statistics including SEM & CI
dfc <- summarySE(ds_long, measurevar="pctdiff",groupvars=c("PathDiff"), na.rm=TRUE)
dfc
## Plot of means with error bars
lg_breaks <- c(0.01, 0.02, 0.04, 0.06, 0.08, 0.10,0.5,1.0)
plt2a <- ggplot(dfc, aes(x=PathDiff, y=pctdiff, colour=PathDiff, group=PathDiff, fill=PathDiff, shape=PathDiff)) + 
  geom_errorbar(aes(ymin=pctdiff-ci, ymax=pctdiff+ci), colour="black", width=.1) + 
  geom_errorbar(aes(ymin=pctdiff-se, ymax=pctdiff+se), colour="red", width=.1) + 
  geom_point(size=3) + scale_y_continuous(breaks=lg_breaks,labels=percent) +
  coord_trans(y="log") + 
  theme_classic() + 
  xlab("Path Length Compared") + ylab("Percent Difference between Human Paths") 
plt2a
ggsave(plt2a, file="plotFiles/figure2.eps",width=6, height=4)

###############################################################################
###############################################################################
## Plot 3 of 4
hpcpsp_path = 'cat_ppr_DataFiles/kgcupscp.csv'
hpcpsp_dat <-read.csv(hpcpsp_path, na.strings ='inf')
#
dfc  <- data.frame()
dfc  <- summarySE(hpcpsp_dat, measurevar="cp",groupvars=c("clicks"), na.rm=TRUE)

plt3 <- ggplot(dfc, aes(x=clicks, y=cp)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=cp-ci, ymax=cp+ci), colour="black", width=.5,) + 
  geom_errorbar(aes(ymin=cp-se, ymax=cp+se), colour="red"  , width=1.0, alpha=0.5) + 
  scale_x_reverse() + 
  xlim(20,0) + ylim(3,10) + ylab("Category Length (L)") + xlab('Human Clicks (n)') +
  theme_classic() +
  theme(legend.position="None",
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16))
#
plt3
ggsave(plt3, file="/home/saguinag/public_html/figure3.pdf",width=6, height=4)

###############################################################################
###############################################################################
## Plot 4 of 4
## This plot is generated using python, b/c to group_by doesn't work on this
## system and it was easier to do this on python
## source code: 
