"
# ==================================================================
#
# SHVegetation FIGURE 1. WORD CLOUD AND DISTRIBUTION OF STUDY REGION
# 
# ==================================================================
# 
# TIME:        2023-04-08 19:10:19 BST
# AUTHOR:      Haoran Wu
# INSTITUTION: Environmental Change Institute
#              School of Geography and the Environment
#              University of Oxford
# 
# EMAIL:       haoran.wu@wolfson.ox.ac.uk
# 
# ==================================================================
# 
# DESCRIPTION:
#   Delivers a plot of word cloud and a stacked plot of study regions
# 
# ==================================================================
# 
# HOW TO USE:
#   Please run the script under its current direction. Make sure that
#     the following files are available in the same directon:
# 
# (WorkSpace) ---> Figure-1.R
#              |-> Most_Frequent_Words.csv
#              |-> full_record_abstract.RData
#              --> is_review.csv
#  where,
#    'Most_Frequent_Words.csv'       Analysed results of the most frequent words.
#    'full_record_abstract.RData'    Results of qualitative aessessments of abstracts of 3749 papers.
#    'is_review.csv'                 Whether a paper is a review or an article.
#
# ==================================================================
#
# OUTPUT: 'Figure-1.pdf' in the current directory.
#
# ==================================================================
"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0.Packages and Functions---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

if(!'Most_Frequent_Words.csv' %in% list.files()) 
  stop("Please make sure that the file 'Most_Frequent_Words.csv' is in the current directory.")
if(!'full_record_abstract.RData' %in% list.files()) 
  stop("Please make sure that the file 'full_record_abstract.RData' is in the current directory.")
if(!'is_review.csv' %in% list.files()) 
  stop("Please make sure that the file 'is_review.csv' is in the current directory.")

#packages
library(wordcloud)
library(ggplot2)
library(ggsci)
library(gridExtra)

#start graphic devices
pdf("Figure-1.pdf")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.Create Word Cloud Plot---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

d <- read.csv("Most_Frequent_Words.csv")
wordcloud(
  d$Terms, d$Frequency,
  colors = rgb(runif(50,0.2,0.8),runif(50,0.2,0.8),runif(50,0.2,0.8)),
  scale = c(2.5,0.5)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.Create Stacked Plot---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load data
load("full_record_abstract.RData")         # meta information got from qualitative assessments of published papaers
                                           #   a total of 3749 papers
v <- read.csv("is_review.csv")$is_review   # whether a paper is a review
tb$is_review <- v

#compute the number of papers 
y_review <- sum(tb$is_review=="y")/nrow(tb)                                          # review
y_global <- sum((tb$hemisphere=="global"&tb$region=="global")|tb$note!="")/nrow(tb)  # global studies
n_tropical <- sum(tb$region=="tropical"&tb$hemisphere=="n")/nrow(tb)                 # tropical studies (north hemisphere)
s_tropical <- sum(tb$region=="tropical"&tb$hemisphere=="s")/nrow(tb)                 # tropical studies (south hemisphere)
b_tropical <- sum(tb$region=="tropical"&tb$hemisphere=="global")/nrow(tb)            # tropical studies (both hemispheres)
n_temperate <- sum(tb$region=="temperate"&tb$hemisphere=="n")/nrow(tb)               # temperate studies (north hemisphere)
s_temperate <- sum(tb$region=="temperate"&tb$hemisphere=="s")/nrow(tb)               # temperate studies (south hemisphere)
n_boreal <- sum(tb$region=="polar")/nrow(tb)                                         # boreal studies 

#reshape data
percent <- c(
  #review,global,north,south,Both
  y_review,0,0,0,0,
  0,y_global,0,0,0,
  0,0,n_tropical,s_tropical,b_tropical,
  0,0,n_temperate,s_temperate,0,
  0,0,n_boreal,0,0
)*100

df <- data.frame(
  region=rep(c(1, 3, 4, 5, 6), each=5),
  hemisphere=factor(rep(c('review','global', 'NH', 'SH', 'Both'), times=5),
                    levels = c('NH','SH','Both','review','global')),
  percent=percent
)

#color settings 
colorVec <- c(
  rgb(201/255,127/255,125/255), #red
  rgb(115/255,131/255,136/255), #grey
  rgb(161/255,199/255,182/255),  #light green
  rgb(233/255,177/255,124/255), #yellow
  rgb(77/255,189/255,226/255)  #blue
)

colorVec <- c(
  rgb(0/255,109/255,44/255),
  rgb(44/255,162/255,99/255),
  #rgb(102/255*1.2,194/255*1.2,164/255*1.2),
  rgb(204/255,236/255,230/255),
  rgb(95/255,113/255,119/255),
  rgb(95/255,113/255,119/255)
)

#render plot
g <- ggplot(df, aes(fill=hemisphere, y=percent, x=region)) + 
  geom_bar(position='stack', stat='identity',width = 0.5,alpha=0.7,color=rgb(0/255,40/255,24/255),size=0.7) +
  theme_classic()+
  #scale_fill_jama()+
  scale_fill_manual(values = colorVec)+
  geom_vline(xintercept=2,linetype='dashed', color=rgb(0/256,40/256,40/256),size=1)+
  scale_x_continuous(limits=c(0.5,6.5),breaks=c(1,2,3,4,5,6),
                     labels = c('Review','','Global','Tropical','Temperate','Boreal'))+
  guides(fill = guide_legend(byrow = TRUE))+
  theme(axis.ticks.x = element_blank(),legend.title = element_blank(),
        axis.title.y=element_text(size=20,face="bold",colour="black"),
        axis.text.x=element_text(size = 17,colour="black"),#,angle=45,hjust = 1
        axis.text.y=element_text(size = 18,colour="black"),
        legend.position = c(0.15, 0.7),
        legend.text = element_text(size = 16,colour="black"),
        legend.spacing.y=unit(0.7,"lines")
  )+xlab(NULL)+ylab("Percentage (%)")

print(g)


#END
dev.off()
