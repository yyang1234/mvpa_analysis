rm(list=ls()) #clean console


library(ggplot2)
library(doBy)
library(cowplot)
library(plyr)
library(lattice)
library(Rmisc)
library(stringr)

# analysis libraries
library(rstatix)
library(dplyr)
library(car)

#library(afex)
library(ez)
library(schoRsch)

#
library(ggplot2)
library(grid)
library(fBasics)
library(tidyr)
library(reshape2)
library(agricolae)
library(Rmisc)
library(gdata)
library(rlist)
library(string)
#######################################################

pathResults <- '/Users/yiyang/DATA/numMVPA_analysis/outputs/derivatives/CoSMoMVPA'
smth2 <- read.csv(paste(pathResults, 'numMVPADecoding_s0_ratio150_202303281357fwhm2.csv', sep ='/'))

pathResults <- '/Users/yiyang/DATA/numMVPA_analysis/outputs/derivatives/CoSMoMVPA'
smth0 <- read.csv(paste(pathResults, 'numMVPADecoding_s0_ratio150_202303271511.csv', sep ='/'))

##################
smth0 <- smth0[-c(8:9)]
smth2 <- smth2[-c(8:9)]
smth0$subID <-as.factor(smth0$subID)
smth2$subID <-as.factor(smth2$subID)
head(smth0)
head(smth2)

#### order things a bit for easy manipulation for plotting


#name change from mask to roi
smth0$mask <-as.factor(smth0$mask)
names(smth0)[2] <- 'roi'

smth2$mask <-as.factor(smth2$mask)
names(smth2)[2] <- 'roi'
# let's make a expType to split the no_pitch exp from pitch exp
smth0$subNb <- as.numeric(smth0$subID)
smth2$subNb <- as.numeric(smth2$subID)

smth0$roi_order <- ifelse(smth0$roi == 'neurosynth_lIPS', 1,
                         ifelse(smth0$roi == 'neurosynth_rIPS', 2, 99))
smth2$roi_order <- ifelse(smth2$roi == 'neurosynth_lIPS', 1,
                          ifelse(smth2$roi == 'neurosynth_rIPS', 2, 99))

# think about other ways of ordering with below function
# mvpa$roi_order <- grepl('nopitch', mvpa$roi, fixed = TRUE)

# currently we  don't have 2 hemispheres for every ROI
smth0$hemis <- ifelse(substr(smth0$roi,12,12) == 'l', 'left',
                     ifelse(substr(smth0$roi,12,12) == 'r', 'right',NA))
smth2$hemis <- ifelse(substr(smth2$roi,12,12) == 'l', 'left',
                      ifelse(substr(smth2$roi,12,12) == 'r', 'right',NA))

# make everything factor
str(smth0)
str(smth2)
smth0$image<- as.factor(smth0$image)
smth2$image<- as.factor(smth2$image)
smth0$hemis<-as.factor(smth0$hemis)
smth2$hemis<-as.factor(smth2$hemis)
smth0$decodingCondition<-as.factor(smth0$decodingCondition)
smth2$decodingCondition<-as.factor(smth2$decodingCondition)
# subset the dataframe for plotting/analysis
img = 'beta' # or 't_maps'

subsetsmth0 = subset(smth0, image == img)
subsetsmth2 = subset(smth2, image == img)

str(subsetsmth0)
str(subsetsmth2)

df0 <- summarySE(data = subsetsmth0, 
                groupvars=c('decodingCondition','roi'),
                measurevar='accuracy', na.rm = TRUE)
df0

df2 <- summarySE(data = subsetsmth2, 
                 groupvars=c('decodingCondition','roi'),
                 measurevar='accuracy', na.rm = TRUE)
df2

######### analyze the data ######### 
results = leveneTest(accuracy ~ roi, subsetsmth0)
results

# test normality 
subsetsmth0 %>%
  group_by(roi) %>%
  shapiro_test(accuracy)

subsetsmth2 %>%
  group_by(roi) %>%
  shapiro_test(accuracy)

# ANOVA IS a bit useless in our case 
# # anova -with ez package
# anova1 <- ezANOVA(data=subsetmvpa, 
#                 dv=.(accuracy), 
#                 wid=.(subID), 
#                 within =.(roi), 
#                 detailed=TRUE, 
#                 type=3) #
# 
# anova1
# anova_out(anova1)
# 
# # lmm from afex package
# m1 <- mixed(accuracy ~  roi * hemis * subType + (1|subID), data = subsetmvpa)
# m1
# 
# m2 <- mixed(accuracy ~  roi + (1|subID), data = subsetmvpa)
# m2


# let's do t-test 

# separate for ROIs
head(subsetsmth2)
#subsetmvpa$accuracy <- subsetmvpa$accuracy + 50

lIPS_smth0 <-subset(subsetsmth0, roi =='neurosynth_lIPS')
rIPS_smth0 <-subset(subsetsmth0, roi =='neurosynth_rIPS')
lIPS_smth2 <-subset(subsetsmth2, roi =='neurosynth_lIPS')
rIPS_smth2 <-subset(subsetsmth2, roi =='neurosynth_rIPS')

# sig dif than zero?
t.test(lIPS_smth0$accuracy, mu = 0.25, alternative = 'greater') # t = 5.498, df = 14, p-value = 3.925e-05
t.test(rIPS_smth0$accuracy, mu = 0.25, alternative = 'greater') # t = 2.0319, df = 14, p-value = 0.0308
t.test(lIPS_smth2$accuracy, mu = 0.25, alternative = 'greater') # t = 3.7175, df = 14, p-value = 0.001148
t.test(rIPS_smth2$accuracy, mu = 0.25, alternative = 'greater') # t = 0.75784, df = 14, p-value = 0.2306
# t.test(lpreM$accuracy, mu = 0.5, alternative = 'greater') # t = 1.2451, df = 9, p-value = 0.1223
# t.test(rpreM$accuracy, mu = 0.5, alternative = 'greater') # t = 0, df = 9, p-value = 0.5
# t.test(SMA$accuracy, mu = 0.5, alternative = 'greater')  # t = 0.73855, df = 9, p-value = 0.2395
# t.test(cerebellum$accuracy, mu = 0.5, alternative = 'greater') # t = 0.41917, df = 9, p-value = 0.3425



############# PLOTTING  #############

# min(subsetmvpa$accuracy, na.rm = TRUE)
# max(subsetmvpa$accuracy, na.rm = TRUE)
# 
# setlimit = c(0,05) 
# setbreak = c(0,0.25, 0.5)


# rois = 'IPS'
# 
# shapesize = 1
# shapetype = 21
# shapestroke = 1
# transparent = 1 #0.6
# jitter  = position_jitterdodge(0.3) # position_jitter(width=0.3)



# colors 
# nonmetricGrayBad = "#9ec5aa" # complex green= 3d8c55ff, nonmetricGrap = 6B6B6B
# nonmetricGrayGood = "#3d8c55ff" 
# simplePurpleBad = "#c4a4c9"
# simplePurpleGood = "#8a4a95"
# conditions
# category2 = ''
# category1 = ''

# cond1= paste0(category2," Bad Tapper")
# cond2 = paste0(category2,' Good Tapper')
# cond3 = paste0(category1,' Bad Tapper')
# cond4 = paste0(category1,' Good Tapper')


con_label = c('aud_num','aud_seq','vis_num','vis_seq','vis_sim')
##### separate tappers
hemis_label = c('left','right','left','right','left','right','left','right','left','right')

# compare_voxelsz <-ggplot(NULL, 
#            aes(x = decodingCondition, y = accuracy, fill = hemis)) +
#   geom_bar(data = subsetmvpa, stat='summary', fun = 'mean', position = 'dodge', 
#             alpha = 0.5) +
#   geom_bar(data = subsetmvpa_26, stat='summary', fun = 'mean', position = 'dodge',
#             alpha = 0.5) 
#   
# compare_voxelsz

fig1 <- ggplot(NULL, aes(x = decodingCondition:hemis, y = accuracy)) +
  geom_bar(data = subsetsmth0, aes(fill = hemis),
           alpha = 0.5, stat='summary', fun = 'mean', fill = 'grey',
           width = 0.7) +
  geom_bar(data = subsetsmth2, aes(fill = hemis),
           alpha = 0.5, stat='summary', fun = 'mean', fill = 'lightblue',
           width = 0.7) +
  stat_summary(data = subsetsmth0,
               aes(x = decodingCondition:hemis,
                   y = accuracy,
                   group = decodingCondition),
               color = 'grey',fun = mean,
               geom = 'crossbar', width = 0.8, linewidth = .4) +
  stat_summary(data = subsetsmth2,
               aes(x = decodingCondition:hemis,
                   y = accuracy,
                   group = decodingCondition:hemis),
               color = 'lightblue',fun = mean,
               geom = 'crossbar', width = 0.8, linewidth = .4) +
  # geom_point(data = subsetsmth0,
  #            aes(x = decodingCondition:hemis,
  #                y = accuracy,
  #                group = decodingCondition:hemis,
  #                shape = subID), position = position_dodge(width = 0.75),
  #            size = 3, alpha = 0.6,color = 'grey') +
  # geom_point(data = subsetsmth2,
  #            aes(x = decodingCondition:hemis,
  #                y = accuracy,
  #                group = decodingCondition:hemis,
  #                shape = subID), position = position_dodge(width = 0.75),
  #            size = 3,alpha = 0.6, color = 'lightblue', fill = 'white') +
  geom_hline(yintercept=c(.25), linetype="dotted", colour="black", linewidth=.5) +
  theme_classic() +
  theme(plot.margin = unit(c(1,1,3,1), "lines"),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  annotate(geom = "text", x = c(1.5,3.5,5.5,7.5,9.5),y = 0, #label for decoding condition
           label = con_label, vjust = 4, size = 5) +
  annotate(geom = "text", x = c(1:10),y = 0,  #label for hemisphere
           label = hemis_label, vjust = 3, size = 4) +
  coord_cartesian(xlim = c(1, 10), ylim = c(0.01,0.45), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  scale_y_continuous(expand = c(0, 0.01)) +
  expand_limits(y = 0) +
  xlab('')

fig1

fig <- ggplot() +
  # geom_boxplot(alpha = 0.1, width = 0.75) +
  geom_point(data = subsetmvpa,
             aes(x = decodingCondition:hemis,
                 y = accuracy,
                 group = decodingCondition:hemis,
                 shape = subID), position = position_dodge(width = 0.75),
             size = 3, alpha = 0.5,color = 'red') +
  geom_point(data = subsetmvpa_26,
             aes(x = decodingCondition:hemis,
                 y = accuracy,
                 group = decodingCondition:hemis,
                 shape = subID), position = position_dodge(width = 0.75),
             size = 3,alpha = 0.4, color = 'blue') +
  # stat_summary(data = subsetmvpa,
  #              aes(x = decodingCondition:hemis,
  #                  y = accuracy,
  #                  group = decodingCondition:hemis),color = 'red',fun = mean, 
  #              geom = 'crossbar', width = 0.7) +
  # stat_summary(data = subsetmvpa_26,
  #              aes(x = decodingCondition:hemis,
  #                  y = accuracy,
  #                  group = decodingCondition:hemis),color = 'blue', fun = mean, 
  #              geom = 'crossbar', width = 0.7, alpha= 0.4) +
  geom_hline(yintercept=c(.25), linetype="dotted", colour="black", linewidth=.5) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank()) +
  annotate(geom = "text", x = c(1.5,3.5,5.5,7.5,9.5),y = 0.2, #label for decoding condition
           label = con_label, vjust = 8.5, size = 5) +
  annotate(geom = "text", x = c(1:10),y = 0.2,  #label for hemisphere
           label = hemis_label, vjust = 9.5, size = 4) +
  coord_cartesian(xlim = c(1, 10), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  theme(plot.margin = unit(c(1,1,3,1), "lines"))

fig

fig <- ggplot(data = subsetmvpa_26,
              aes(x = decodingCondition:hemis,
                  y = accuracy,
                  # colour = hemis,
                  # fill = hemis,
                  group = decodingCondition:hemis
              )) +
  # geom_boxplot(alpha = 0.1, width = 0.75) +
  geom_point(aes(shape = subID,color = subID), position = position_dodge(width = 0.75),
             size = 3,alpha = 0.6) +
  stat_summary(aes(group = decodingCondition:hemis),fun = mean, geom = 'crossbar', 
               width = 0.7) +
  geom_hline(yintercept=c(.25), linetype="dotted", colour="black", size=.5) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank()) +
  annotate(geom = "text", x = c(1.5,3.5,5.5,7.5,9.5),y = 0.2, #label for decoding condition
           label = con_label, vjust = 6.5, size = 5) +
  annotate(geom = "text", x = c(1:10),y = 0.2,  #label for hemisphere
           label = hemis_label, vjust = 6.5, size = 4) +
  coord_cartesian(xlim = c(1, 10), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  theme(plot.margin = unit(c(1,1,3,1), "lines"), axis.text.x= element_text(color = 'white')) +
  xlab('')+
  
  
  # stat_summary(fun = 'mean',geom = 'line')
  # geom_point(data = subsetmvpa,
  #            aes(x = decodingCondition,
  #                y = accuracy,
  #                color = subNb,
  #                group = roi_order))
  # 
  
  
fig
# fig <- ggplot(data = subsetmvpa, 
#               aes(x = roi,
#                   y = accuracy, 
#                   color = decodingCondition,
#                   group = decodingCondition, subNb)) +
#   geom_point(data=subsetmvpa, 
#              aes(x = roi, 
#                  y = accuracy), 
#              size = shapesize,
#              position = jitter, shape = shapetype, stroke = shapestroke, na.rm = TRUE) + 
#   stat_summary(aes(color = decodingCondition, subNb), fun=mean, fun.min = mean, fun.max = mean, 
#                geom="crossbar", size=0.6, width=0.6, position = position_dodge(width=.75), 
#                na.rm = TRUE) +
#   theme_classic() +
#   geom_errorbar(data = df, 
#                 aes(ymin = accuracy-se, ymax = accuracy+se, group = decodingCondition,, subNb), 
#                 color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
#   geom_hline(yintercept=c(.25), linetype="dotted", colour="black", size=.5) +
#   
#   ggtitle("") +
#   ylab("") +
#   xlab("") +
#   theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
#   theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
#   theme(axis.title.y=element_text(size=10, angle=90, colour='black')) +
#   scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
#   # scale_x_discrete(labels = c("lSTG All", "lSTG 10","lSTG 15","lSTG 20","rSTG All","rSTG 10","rSTG 15","rSTG 20"))+
#   # scale_color_manual(name = '', labels = c(cond1, cond2), values=c(nonmetricGrayBad, nonmetricGrayGood)) + 
#   # # theme(legend.position= "none")
#   theme(legend.position= c(.85, .85)) +
#   theme(legend.text=element_text(size=8)) +
#   theme(legend.title=element_text(size=9))
# fig

filename <- paste0(pathResults, 'Decoding_Simple_vs_',cond,  'voxelNb-', voxelSize, '_', rois,'tappers.png')
# ggsave(filename, fig, dpi=300, width=15, height=6, units='cm') 
ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512


#######################################################
########## ignore tappers #######################################################
#######################################################
df2 <- summarySE(data = subsetmvpa, 
                 groupvars=c('roi_order','roi'),
                 measurevar='accuracy', na.rm = TRUE)
df2

fig <- ggplot(data = subsetmvpa, 
              aes(x = roi,
                  y = accuracy),
              color = expType) +
  geom_jitter(size = shapesize, shape = shapetype, stroke = shapestroke, width=0.1, color = nonmetricGrayGood, 
              na.rm = TRUE) +
  stat_summary(aes(color = expType), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.3,
               na.rm = TRUE) +
  theme_classic() +
  geom_errorbar(data = df2, 
                aes(ymin = accuracy-se, ymax = accuracy+se), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75),
                na.rm = TRUE) +
  geom_hline(yintercept=c(.5), linetype="dotted", colour="black", size=.5) +
  
  ggtitle("") +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=10, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  scale_x_discrete(labels = c("lSTG All", "lSTG 10","lSTG 15","lSTG 20","rSTG All","rSTG 10","rSTG 15","rSTG 20"))+
  scale_color_manual(name = '', labels =c('Simple vs. Nonmetric'), values=c(nonmetricGrayGood)) + #
  theme(legend.position= c(.85, .9)) +
  theme(legend.text=element_text(size=9)) +
  theme(legend.title=element_text(size=9))
fig

filename <- paste0(pathResults, 'Decoding_Simple_vs_',cond,  'voxelNb-', voxelSize,  '_', rois, '.png')
ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512












##############################################################################################################
################################################################################################################
# OLDER VERSION

mvpa$roi_order <- ifelse(mvpa$mask == 'leftAud', 1, 
                         ifelse(mvpa$mask == 'rightAud', 2, 
                                ifelse(mvpa$mask == 'leftBG', 3, 
                                       ifelse(mvpa$mask == 'rightBG',4,
                                              ifelse(mvpa$mask == 'leftPremotor', 5,
                                                     ifelse(mvpa$mask == 'rightPremotor',6,7))))))
mvpa$roi_color_code <- ifelse(mvpa$mask == 'leftAud', 1, 
                              ifelse(mvpa$mask == 'rightAud', 1, 
                                     ifelse(mvpa$mask == 'leftBG', 2, 
                                            ifelse(mvpa$mask == 'rightBG',2,
                                                   ifelse(mvpa$mask == 'leftPremotor', 3,
                                                          ifelse(mvpa$mask == 'rightPremotor',3,4))))))

mvpa$mask <- ifelse(mvpa$mask == 'leftAud', 'audL', 
                    ifelse(mvpa$mask == 'rightAud', 'audR', 
                           ifelse(mvpa$mask == 'leftBG', 'bgL', 
                                  ifelse(mvpa$mask == 'rightBG','bgR',
                                         ifelse(mvpa$mask == 'leftPremotor', 'preL',
                                                ifelse(mvpa$mask == 'rightPremotor','preR','SMA'))))))
# sma roi consist of left/right hemispheres but for plotting 
# we can show under the same vx size as the others
# better way would be dividing the masks an re-run the decoding
mvpa$voxelToPlot <- mvpa$choosenVoxNb
#mvpa$voxelToPlot<- ifelse(mvpa$mask == 'SMA', mvpa$voxelToPlot/2, mvpa$voxelToPlot)

# ==============================================================================
# summary stats
df <- summarySE(data = mvpa, 
                groupvars=c('mask', 'roi_order', 'image','voxelToPlot','roi_color_code'),
                measurevar='accuracy')
df

#################
pd <- position_dodge(0.1)

filename <- paste(pathCosmoResults, '/plot/', 'Decoding_SimpleVsNonmetric-1Beta.png', sep = '')
title <- paste('Simple vs Nonmetric Rhythm Decoding ')


fig <- ggplot(df, aes(x = reorder(mask, roi_order), y = accuracy)) + 
  geom_point(data = mvpa, aes(group=subID), pos=pd, size=2, color=grey(0.8)) + 
  geom_point(size=2,col='black') + 
  geom_hline(yintercept=c(.5), linetype="dotted", colour="red", size=.5) +
  facet_grid(vars(image), vars(voxelToPlot)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),size=1,width=0.2) + 
  scale_y_continuous(limits=c(0, .90)) +
  xlab('ROIs')+
  ylab('classification acc.')+
  ggtitle(title)+
  theme_cowplot(font_size = 12,
                font_family = "",
                line_size = 0.3,
                rel_small = 6/12,
                rel_tiny = 9/12,
                rel_large = 14/12)


fig

ggsave(filename, device="png", units="in", width=18, height=9.08, dpi=300)  


######
# a quick look for significance
chance = 0.5
iImage = 't_maps'
iVoxelNb = 150 # 100 150
iSmoothing = 2
roi = 'preL' #preR, preL, audL, audR, SMA, bgR, bgL

dataAccuracy = subset(mvpa, image == iImage & choosenVoxNb == iVoxelNb & mask ==roi)
accuracyVector = dataAccuracy$accuracy - chance
res = t.test(accuracyVector)
res

#### not assuming normality - let's do non-parametric test

res = wilcox.test(accuracyVector)
Za = qnorm(res$p.value/2)
res




######## only for 1 contrast = MASK = ROI ##################
# only for 1 big roi from the allsounds vs. rest contrast:
mvpa$roi_order <- 1

df <- summarySE(data = mvpa, 
                groupvars=c('mask', 'roi_order', 'image','ffxSmooth','voxelToPlot'),
                measurevar='accuracy')



filename <- paste(pathCosmoResults, '/plot/', 'Decoding_SimpleVsComplex-1Beta.png', sep = '')
title <- paste('Simple vs Complex Rhythm Decoding ')


fig <- ggplot(df, aes(x = reorder(mask, roi_order), y = accuracy)) + 
  geom_point(data = mvpa, aes(group=subID), pos=pd, size=2, color=grey(0.8)) + 
  geom_point(size=2,col='black') + 
  geom_hline(yintercept=c(.5), linetype="dotted", colour="red", size=.5) +
  facet_grid(vars(image,ffxSmooth), vars(voxelToPlot)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),size=1,width=0.2) + 
  scale_y_continuous(limits=c(0, .90)) +
  ylab('classification acc.')+
  xlab('ROIs')+
  ggtitle(title)+
  theme_cowplot(font_size = 14,
                font_family = "",
                line_size = 0.3,
                rel_small = 10/12,
                rel_tiny = 10/12,
                rel_large = 14/12)


fig





###### figure for best ratio for decoding
#################################### destroy afterwards
# summary stats
df <- summarySE(data = mvpa,
                groupvars=c('mask', 'roi_order', 'image','ffxSmooth'),
                measurevar='accuracy')


#################
pd <- position_dodge(0.1)

filename <- paste(pathCosmoResults, '/plot/', 'Decoding_SimpleVsComplex-AllCondition-Ratio95.png', sep = '')
title <- paste('Simple vs Complex Rhythm Decoding ')


fig <- ggplot(df, aes(x = reorder(mask, roi_order), y = accuracy)) +
  geom_point(data = mvpa, aes(group=subID), pos=pd, size=2.5, color=grey(0.8)) +
  geom_point(size=3,col='black') +
  geom_hline(yintercept=c(.5), linetype="dotted", colour="red", size=.5) +
  facet_grid(vars(image,ffxSmooth)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),size=1,width=0.2) +
  scale_y_continuous(limits=c(0, .90)) +
  xlab('ROIs')+
  ylab('classification acc.')+
  ggtitle(title)+
  theme_cowplot(font_size = 14,
                font_family = "",
                line_size = 0.3,
                rel_small = 10/12,
                rel_tiny = 10/12,
                rel_large = 14/12)


fig

ggsave(filename, device="png", units="in", width=9, height=9.08, dpi=300)










########
# with for loop
# filterSmoothing <- c('0','2')
# filterImage <- c('beta', 't_maps')
# filterVoxelNb <- c('100','150','250','400','520')

# for (iImage in filterImage) {
#   
#   
#   for (iSmoothing in filterSmoothing) {
#     
#     for (iVoxelNb in filterVoxelNb) {
#       
#       print(paste(iImage, iSmoothing, iVoxelNb))
#       
#       title <- paste('SimplevsComplex - ',iImage,'s',iSmoothing, 'voxel', iVoxelNb)
#       
#       filename <- paste(pathCosmoResults, '/plot/', 'simpleVscomplex-', iImage, '_s', iSmoothing,'_', iVoxelNb,'_Vx.png', sep = '')
#       
#       fig <- ggplot(subset(df, image == iImage & choosenVoxNb == iVoxelNb & ffxSmooth == iSmoothing), aes(x = reorder(mask, roi_order), y = accuracy)) + 
#         geom_point(data = subset(mvpa, image == iImage & iVoxelNb == choosenVoxNb & iSmoothing == ffxSmooth), aes(group=subID), pos=pd, size=3, color=grey(0.8)) + 
#         geom_point(size=4,col='black') + 
#         geom_hline(yintercept=c(.5), linetype="dotted", colour="red", size=.5) +
#         geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se),size=1,width=0.2) + 
#         scale_y_continuous(limits=c(0, .90)) +
#         xlab('ROIs')+
#         ylab('classification acc.')+
#         ggtitle(title)+
#         theme_classic() +
#         theme(
#           text=element_text(size=16),
#           legend.position = 'right',
#           legend.text=element_text(size=14),
#           axis.line = element_line(size = 0.6),
#           axis.text.x = element_text(size=14,colour="black"),
#           axis.text.y = element_text(size=14, colour='black'))
#       ggsave(filename, device="png", units="in", width=9, height=4.54, dpi=300)  
#     }
#   }
#   
# }


