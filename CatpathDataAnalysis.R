library(ggplot2);library(reshape2)
require("Rmisc")
require(Hmisc)
library(scales); library(Kendall)
setwd('/home/saguinag/CategoryPaths/')
#hpcpsp_path = 'cat_ppr_DataFiles/kgcupscp.csv'
## file includes the hops
data_path   = 'pathsHopsFiles/kgcupscp_ppr_hops.csv'
hpcpsp_dat <-read.csv(data_path, na.strings ='inf')
df <- data.frame(hpcpsp_dat)

## Below: filter records where PPR is above 0
df <- df[df$prob>0,]
## filter records that have paths below 30
df <- df[df$cp<30,]
df <- df[df$clicks<30,]
head(df)

## stats
summary(df$clicks)
summary(df$cp)
summary(df$sp)
#
var(df$clicks)
var(df$cp)
var(df$sp)
#
sd(df$clicks)
sd(df$cp)
sd(df$sp)
# Mode statistic:
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(df$sp)
Mode(df$cp)
Mode(df$clicks)

## Plot the frequency as a function of path length
#http://www.tagwith.com/question_1264226_plot-frequency-distribution-of-one-column-data-in-r

hp.histogram = hist(df$clicks, breaks = 60, freq = F, xlab = 'Path length', ylim = c(0, 0.4),
                    ylab = 'Frequency', main = 'Histogram of')
hp.ylim.normal = range(1, hp.histogram$density, dnorm(df$clicks, mean = mean(df$clicks), sd = sd(df$clicks)), na.rm = T)
curve(dnorm(x, mean = mean(df$clicks), sd = sd(df$clicks)), add = T)
curve(dgamma(x, shape = mean(df$clicks)^2/var(df$clicks), scale = var(df$clicks)/mean(df$clicks)), add = T)

## cp
cp.histogram = hist(df$cp, breaks = 60, freq = F, xlab = 'Path length', ylim = c(0, 1),
                    ylab = 'Probability', main = 'Histogram of')
cp.ylim.normal = range(0, hp.histogram$density, dnorm(df$cp, mean = mean(df$cp), 
                                                      sd = sd(df$cp)), na.rm = T)
curve(dnorm(x, mean = mean(df$cp), sd = sd(df$cp)), add = T)
## sp
hist(df$sp, breaks = 10, freq = F, xlim = c(0, 6), xlab = 'Path Length', ylab = 'Relative Frequency', main = 'Histogram of')
curve(dgamma(x, shape = mean(df$sp)^2/var(df$sp), scale = var(df$sp)/mean(df$sp)), add = T)

# http://www.r-tutor.com/elementary-statistics/quantitative-data/cumulative-frequency-graph


ppr_gby_hp_se <- summarySE(data = df, measurevar="prob", groupvars = c('clicks'), 
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ppr_gby_hp_se <- cbind(ppr_gby_hp_se,  "type"="HP")
colnames(ppr_gby_hp_se)[1]<-'scores'
ppr_gby_cp_se <- summarySE(data = df, measurevar="prob", groupvars = c('cp'), 
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ppr_gby_cp_se <- cbind(ppr_gby_cp_se, "type" = "CP")
colnames(ppr_gby_cp_se)[1]<-'scores'
ppr_gby_sp_se <- summarySE(data = df, measurevar="prob", groupvars = c('sp'), 
                           na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
ppr_gby_sp_se <- cbind(ppr_gby_sp_se, "type"="SP")
colnames(ppr_gby_sp_se)[1]<-'scores'

pdat <- rbind(ppr_gby_hp_se, ppr_gby_cp_se, ppr_gby_sp_se)
pdat[is.na(pdat)] <- 0
g1<- ggplot(pdat, aes(x=scores, y=prob, group=type, linetype=type)) + 
  scale_y_log10(limits=c(1e-6,1e-3)) + xlim(14,1) +
  geom_errorbar(aes(ymin=prob-se, ymax=prob+se),na.rm = TRUE, colour="red", width=.4,linetype=1) +
  #geom_errorbar(aes(ymin=prob-ci, ymax=prob+ci),na.rm = TRUE, colour="black", width=.4, linetype=1) +
  geom_line() + geom_point(size=3) + scale_fill_grey(start = 0, end = 1) + geom_line(size=0.8) + 
  scale_linetype_manual(name="Path Type", values=c("solid","dotted","dashed"), labels=c("Human Path", "CatPath", "Shortest Path")) +
  xlab('CatPath Distance') + ylab('Personal Page Rank') + 
  theme_classic() + 
  theme(legend.key.width=unit(2,"line"),
        legend.position=c(0.15,0.85),
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16) 
        )
g1
ggsave(g1, file="plotFiles/figure8.pdf",width=6, height=4)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## Plot 1 of 6  

# shapiro.test(sample(df$clicks,5000)) # test for normality
# Shapiro-Wilk normality test
# 
# data:  sample(df$clicks, 5000)
# W = 0.2401, p-value < 2.2e-16

# https://stat.ethz.ch/R-manual/R-patched/library/stats/html/cor.html
corXY <- cor(data.frame(df$clicks,df$cp), use="complete.obs", method="kendall")
corXY <-cor.test(df$clicks, df$cp, use="complete.obs", method="kendall")
# Kendall's rank correlation tau
# 
# data:  df$clicks and df$cp
# z = 42.8867, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.1781463
ken_cpppr <- Kendall(df$cp, df$prob)
ken_cphops <- Kendall(df$cp, df$hops)
ken_cpsp  <- Kendall(df$cp, df$sp)
ken_cphp  <- Kendall(df$cp, df$clicks)
prs_cpppr <- cor.test(df$cp, df$prob, use="complete.obs", method="pearson")
prs_cphops <- cor.test(df$cp, df$hops, use="complete.obs", method="pearson")
prs_cpsp  <- cor.test(df$cp, df$sp,   use="complete.obs", method="pearson")
prs_cphp  <- cor.test(df$cp, df$clicks,   use="complete.obs", method="pearson")
# 
summary(lm(cp ~ prob,   data = df))[['r.squared']]
summary(lm(cp ~ sp,     data = df))[['r.squared']] 
summary(lm(cp ~ clicks, data = df))[['r.squared']]
#
#
Kendall(df$sp, df$prob)
Kendall(df$clicks, df$prob)
Kendall(df$sp, df$clicks)
#
cor.test(df$sp, df$prob, use="complete.obs", method="pearson")
cor.test(df$clicks, df$prob,   use="complete.obs", method="pearson")
cor.test(df$sp, df$clicks,   use="complete.obs", method="pearson")
# rSquared 
summary(lm(sp ~ prob,   data = df))[['r.squared']]
summary(lm(clicks ~ prob, data = df))[['r.squared']] 
summary(lm(sp ~ clicks, data = df))[['r.squared']] 


# rout <- list(paste('R^2 = ', round(summary(mod)[['r.squared']], 3), sep = ''),
#              paste('Kendall: ', out_hpppr, sep = '')  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# f
df <- df[df$prob>0,]
p11 <- ggplot(df[sample(nrow(df),500), ], aes(x=cp, y=prob)) + 
  geom_point(color = "black",alpha=.5, size=4) + 
  scale_y_log10(limits=c(1e-9,1e-3)) +
  xlab('CatPath Distance') + ylab('Personal Page Rank') + 
  geom_smooth(method = 'lm', na.rm = TRUE, se=TRUE) +
  xlim(0,15) + 
  #geom_text(data = data.frame(), aes(x = 20, y = 1e-3, label = rout[[1]]), hjust = 1) + 
  #geom_text(data = data.frame(), aes(x = 20, y = 0.95e-3, label = rout[[2]]), hjust = 1) +
  #geom_text(data = data.frame(), aes(x = 20, y = 0.9e-3, label = rout[[3]]), hjust = 1) +
  theme_classic() +
  theme(legend.position="None",
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16))
p11

p12 <- ggplot(df[sample(nrow(df),500), ], aes(x=cp, y=sp)) +
  geom_point(color = "black",alpha=.5, size=4) + 
  xlab('CatPath Distance') + ylab('Shortest Path (L)') +
  xlim(0,15) +
  geom_smooth(method = 'lm', na.rm = TRUE, se=TRUE) +
  #geom_text(data = data.frame(), aes(4.5, 30, label = "Pearson-R = -.87")
  #geom_text(data = data.frame(), aes(x = 20, y = 30, label = rout[[1]]), hjust = 1) + 
  #geom_text(data = data.frame(), aes(x = 20, y = 28.5, label = rout[[2]]), hjust = 1) +
  #geom_text(data = data.frame(), aes(x = 20, y = 27, label = rout[[3]]), hjust = 1) +
  theme_classic() +
  theme(legend.position="None",
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16))
p12

p13 <- ggplot(df[sample(nrow(df),500), ], aes(x=cp, y=clicks)) +
  geom_point(color = "black",alpha=.5, size=4) +  
  geom_smooth(method = 'lm', na.rm = TRUE, se=TRUE) +
  xlab('CatPath Distance') + ylab('Human Path (L)') +
  xlim(1,14) + ylim(1,20) + 
  theme_classic() +
  theme(legend.position="None",
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16))
p13
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## --------- ROW 2
p21 <- ggplot(df[sample(nrow(df),500), ], aes(x=sp, y=prob)) + 
  geom_point(color = "black",alpha=.5, size=4) +  
  geom_smooth(method = 'lm', na.rm = TRUE, se=TRUE) +
  scale_y_log10(limits=c(1e-9,1e-3)) +
  xlab('Shortest Path (L)') + ylab('Personal Page Rank') + 
  theme_classic() +
  theme(legend.position="None",
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16))
p21
p22 <- ggplot(df[sample(nrow(df),500), ], aes(x=clicks, y=prob)) + 
  geom_point(color = "black",alpha=.5, size=4) +  
  geom_smooth(method = 'lm', na.rm = TRUE, se=TRUE) +
  scale_y_log10(limits=c(1e-9,1e-3)) + xlim(0,25) + 
  xlab('Human Path (L)') + ylab('Personal Page Rank') + 
  theme_classic() +
  theme(legend.position="None",
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16))
p22
p23 <- ggplot(df[sample(nrow(df),500), ], aes(x=sp, y=clicks)) +
  geom_point(color = "black",alpha=.5, size=4) +  
  geom_smooth(method = 'lm', na.rm = TRUE, se=TRUE) +
  xlab('Shortest Path (L)') + ylab('Human Path (L)') +
  ylim(1,20) + 
  theme_classic() +
  theme(legend.position="None",
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16))
p23
ggsave(p11, file="plotFiles/figure11.pdf",width=6, height=4)
ggsave(p12, file="plotFiles/figure12.pdf",width=6, height=4)
ggsave(p13, file="plotFiles/figure13.pdf",width=6, height=4)
#
ggsave(p21, file="plotFiles/figure21.pdf",width=6, height=4)
ggsave(p22, file="plotFiles/figure22.pdf",width=6, height=4)
ggsave(p23, file="plotFiles/figure23.pdf",width=6, height=4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#  join PPR scores to the catpath, human path and shortest path
ggplot(df, aes(x=cp, y=hops)) +   geom_point(color = "cornflowerblue",alpha=.5) +
  geom_point(aes(x=clicks, y=prob$mean()), color = "firebrick",alpha=.2) + 
  geom_point(aes(x=sp, y=prob), color = "gray", alpha=.2) +
  #scale_fill_continuous(guide = "legend")+
  #scale_y_log10(limits=c(1e-9,1e-3)) +
  scale_y_log10() +
  xlab('CatPath Distance') + ylab('Personal Page Rank') + 
  xlim(0,20) + 
  theme_classic() 
#   scale_colour_manual("", 
#                       breaks = c("cp", "clicks", "sp"),
#                       values = c("red", "black", "blue") )
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


dfc  <- summarySE(df, measurevar="cp",groupvars=c("clicks"), na.rm=TRUE)
dfc[is.na(dfc)] <- 0
plt9 <- ggplot(dfc, aes(x=clicks, y=cp)) + 
  geom_errorbar(aes(ymin=cp-ci, ymax=cp+ci), colour="black", width=0.4) + 
  geom_errorbar(aes(ymin=cp-se, ymax=cp+se), colour="red",   width=0.4) + 
  geom_point(size=2) +  scale_x_reverse(limits=c(14.2,1.5), breaks=c(14,10,5))  +
  ylim(3,8) + ylab("CatPath Distance") + xlab("Human Path") + 
  theme_classic() + 
  theme(legend.key.width=unit(2,"line"),
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16) 
  ) 
#
plt9
ggsave(plt9, file="plotFiles/figure9.pdf",width=6, height=4)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Sum of Weights / Hops CatPath
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ggplot(df[sample(nrow(df),500), ], aes(x=cp, y=hops)) + geom_point(color = "black",alpha=.5, size=3) + 
  xlim(0,20) + ylim(0,10) +
  geom_smooth(method = 'lm', na.rm = TRUE, se=TRUE) +
  theme_classic() + ylab('CatPath Length') + xlab('CatPath Distance') + 
  theme(legend.key.width=unit(2,"line"),
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16) 
  )
ggsave(file="plotFiles/figure_hops.pdf",width=5, height=4)
##  Hops HumanPath
ggplot(df[sample(nrow(df),500), ], aes(x=clicks, y=hops)) +   
  geom_point(color = "black",alpha=.5, size=3) + 
  xlim(0,20) + ylim(0,7) +
  geom_smooth(method = 'lm', na.rm = TRUE, se=TRUE) +
  theme_classic() + ylab('CathPath Distance') + xlab('Human Path Length') + 
  theme(legend.key.width=unit(2,"line"),
        # Text settings for scale
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16) 
  )
ggsave(file="plotFiles/hops_humanpath.pdf",width=5, height=4)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#   Jaccard Coefficient
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
data_path   = '/data/zliu8/overlaps_hp_sssp.txt'
hpcpsp_dat <-read.csv(data_path,sep="\t", header=FALSE)
df1 <- data.frame(hpcpsp_dat)
df1$type <- 'sp'
colnames(df1)<- c("start", "end", "union", "inters", "type")
df1$jack <-  df1$inters/df1$union

data_path   = '/data/zliu8/overlaps_hp_cp.txt'
hpcpsp_dat <-read.csv(data_path,sep="\t", header=FALSE)
df2 <- data.frame(hpcpsp_dat)
df2$type <- 'cp'
colnames(df2)<- c("start", "end", "union", "inters", "type")
df2$jack <-  df2$inters/df2$union




head(df)
df <- rbind(df1, df2)

smse <- summarySE(data = df, measurevar="jack", groupvars = c('type'), 
                  na.rm = TRUE, conf.interval = 0.95, .drop = TRUE)
pj1<- ggplot(smse, aes(x=as.factor(type), y=jack)) +   geom_point(color = "black",alpha=.5, size=3) + 
  geom_errorbar(aes(ymin=jack-ci, ymax=jack+ci), colour="black", width=0.2) + 
  geom_errorbar(aes(ymin=jack-se, ymax=jack+se), colour="red", width=0.2) + 
  scale_x_discrete( labels=c("CatPath","Shortest Path"))+
  theme_classic() + ylab('Jaccard Coefficient') + xlab('') + ylim(.2,.22) +
  theme(legend.key.width=unit(2,"line"),
        # Text settings for scale
        axis.text = element_text(size=10),
        # Text settings for axis titles
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=10),
        panel.background = element_rect(fill = "transparent",colour =NA), # or theme_blank() 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        plot.background = element_rect(fill = "transparent",colour =NA)
  )

df1<-df1[order(-df1$jack),]
df2<-df2[order(-df2$jack),]



df1 = cbind( df1[with(df1,rev(order(jack))),], "rank"=c(1:11245))
df2 = cbind( df2[with(df2,rev(order(jack))),], "rank"=c(1:11245))

df <- rbind(df1, df2)

pj2<- ggplot(df, aes(y=jack, x=rank, group=type, linetype=type)) + 
  geom_line(size=0.65) +
  scale_y_log10(limits=c(.1,.5), breaks=c(0.1,0.3,0.5)) +  scale_x_log10(limits=c(40,11000)) + 
  scale_linetype_manual(name="Path Type", values=c("solid","dashed"), labels=c("CatPath", "Shortest Path")) + 
  theme_classic() + xlab("") + ylab("Jaccard Coefficient (log scale)") + 
  theme(legend.key.width=unit(4,"line"),
        legend.text = element_text(size = 14),
        #legend.justification=c(0,0), 
        legend.position=c(0.82,0.9),
        # Text settings for scale
        legend.title = element_text(size=14),
        axis.text = element_text(size=12),
        # Text settings for axis titles
        axis.title.y = element_text(size=16)
        #axis.title.x = element_text(size=16) 
        ) 

# using viewport
# Reference: 
# https://randpk.wordpress.com/2011/09/04/use-windows-fonts-when-creating-subplots-by-ggplot2/

pdf("plotFiles/figure_jacc.pdf", width=7, height=5)
subvp <-viewport(width=.5, height=.55, x=.36, y=.4)
pj2
print(pj1, vp=subvp)
dev.off()