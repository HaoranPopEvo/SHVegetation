require(ggplot2)
require(ggrepel)
require(ncdf4)
require(raster)
require(tidyverse)
require(GiNA)
require(ggeasy)

#Step1: Generate Data
##1-1 Data1: Region Information
###Please import "t.RData"  --- object 'res' required
load("t.RData")

ids <- unique(res$id)  #extract region id from object 'res'
geo <- do.call(rbind,lapply(ids, function(idi){
  data.frame(
    id=idi,
    unique(res[res$id==idi,2:3])  
  )
}))
rm(list=ls()[ls()!="geo"])
geo$id <- as.character(lapply(geo$id, function(idi){
  idsplit <- strsplit(idi,"-")[[1]]
  paste0("Region ",idsplit[1],ifelse(idsplit[2]=="1","a","b"))
})) #longitude and latitude

##1-2 Data2: Region Border
selected_region <- list(
  region1 = c(x1 = -142.5,x2 = -112.5, y1 = 37.5, y2 = 67.5),
  region2 = c(x1 = -110,  x2 = -90,    y1 = 20,   y2 = 40  ),
  region3 = c(x1 = -85,   x2 = -45,    y1 = 0,    y2 = 15  ),
  region4 = c(x1 = -70,   x2 = -50,    y1 = -22.5,y2 = -45 ),
  region5 = c(x1 = -15,   x2 = 15,     y1 = 37.5, y2 = 60  ),
  region6 = c(x1 = -15,   x2 = 20,     y1 = -7.5, y2 = 10  ),
  region7 = c(x1 = 30,    x2 = 52.5,   y1 = -7.5, y2 = 15  ),
  region8 = c(x1 = 67.5,  x2 = 90,     y1 = 20,   y2 = 37.5),
  region9 = c(x1 = 105,   x2 = 130,    y1 = 30,   y2 = 40  ),
  region10= c(x1 = 135,   x2 = 157.5,  y1 = -40,  y2 = -15 )
)
selected_region <- do.call(rbind,lapply(selected_region, function(xx){
  xmax <- max(xx[1:2])
  xmin <- min(xx[1:2])
  ymax <- max(xx[3:4])
  ymin <- min(xx[3:4])
  data.frame(
    x=c(xmin,xmax,xmin,xmin),
    y=c(ymin,ymin,ymax,ymin),
    xend=c(xmax,xmax,xmax,xmin),
    yend=c(ymin,ymax,ymax,ymax)
  )
}))  #rearrange data to present line segments

#generate text label data.frame
text_label <- do.call(rbind,lapply(1:10,function(ii){
  local_df <- selected_region[(4*ii-3):(4*ii),]
  data.frame(
    id=paste("Region",ii),
    x=min(c(local_df$x,local_df$xend))+16,   #add increment of 'x' axis
    y=max(c(local_df$y,local_df$yend))+3.5  #add increment of 'y' axis
  )
}))


##1-3 Data3: background layer
back <- raster("Mean_Number_of_Season_Estimated.nc")
back <- as.data.frame(back,xy=TRUE)
back <- dplyr::rename(back,long=x,lat=y)
back <- back %>% filter(!is.na(Number.of.Season))
#back$Number.of.Season <- factor(back$Number.of.Season)

#Step2: Render Plot
#my_colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
#my_colormap <- c("0"="#0000CC","1"="#91E4FF","2"="#FFBE00","3"="#8A0000")

g <- ggplot()+
  borders('world', colour='grey', size=.05)+
  geom_tile(data = back, aes(x=long,y=lat,fill=Number.of.Season))+
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend),data = selected_region)+
  geom_point(aes(x=geo$long,y=geo$lat),size=0.7,shape=1)+
  geom_text(aes(x=text_label$x,y=text_label$y,label=text_label$id))+
  scale_fill_gradientn(colours=c("#0000CC","#91E4FF","#FFBE00","#8A0000"))+
  #scale_fill_gradientn(colours = my_colormap,name="Seasonality") +
  #scale_fill_manual(values=my_colormap)+
  theme_classic()+ylim(-60,80)+
  easy_remove_axes(which="both")+
  xlab("Longitude")+ylab("Latitude")
print(g)
#output width:height=2:1
# Canvas 14.5 * 7
