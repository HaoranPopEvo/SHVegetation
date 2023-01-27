load("full_record_abstract.RData")
v <- read.csv("is_review.csv")$is_review
tb$is_review <- v

library(ggplot2)
require(ggsci)
y_review <- sum(tb$is_review=="y")/nrow(tb)
y_global <- sum((tb$hemisphere=="global"&tb$region=="global")|tb$note!="")/nrow(tb)
n_tropical <- sum(tb$region=="tropical"&tb$hemisphere=="n")/nrow(tb)
s_tropical <- sum(tb$region=="tropical"&tb$hemisphere=="s")/nrow(tb)
b_tropical <- sum(tb$region=="tropical"&tb$hemisphere=="global")/nrow(tb)
n_temperate <- sum(tb$region=="temperate"&tb$hemisphere=="n")/nrow(tb)
s_temperate <- sum(tb$region=="temperate"&tb$hemisphere=="s")/nrow(tb)
n_boreal <- sum(tb$region=="polar")/nrow(tb)

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
#factor(rep(c('review','global', 'tropical', 'temperate', 'boreal'), each=5),levels = c('review','global','tropical','temperate','boreal'))
#scale_fill_jama()
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

g<- ggplot(df, aes(fill=hemisphere, y=percent, x=region)) + 
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
