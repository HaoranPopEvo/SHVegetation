##parameter------------------

region <- c(xmin=-180, xmax=180, ymin=-80, ymax=90) #global

#region <- c(xmin=-100,xmax=-30,ymin=-50,ymax=0)  #south america
#region <- c(xmin=-20,xmax=60,ymin=-30,ymax=0)    #africa
#region <- c(xmin=90,xmax=180,ymin=-60,ymax=0)    #australia

##function------------------
require(ncdf4)
require(ggplot2)
require(ggeasy)
require(raster)
require(tidyverse)

#function to convert ndvi dataset to point-XY
nc_ndvi2point <- function(nc, var="ndvi",
                          region=c(xmin=-180,xmax=180,ymin=-90,ymax=90),
                          mar=0.2, na.omit=TRUE){
  ##Step1: read data and determine grid size
  dat <- ncvar_get(nc,var)
  gzlo <- 360/dim(dat)[1] #gzlo: grid size for longitude
  gzla <- 180/dim(dat)[2]  #gzla: grid size for latitude
  
  ##Step2: choose region
  centxs <- (1:dim(dat)[1]-1)*(360-gzlo)/(dim(dat)[1]-1)+gzlo/2-180
  validxs <- (1:dim(dat)[1])[centxs>=region[["xmin"]]&centxs<=region[["xmax"]]]
  centys <- (1:dim(dat)[2]-1)*(gzla-180)/(dim(dat)[2]-1)+90-gzla/2
  validys <- (1:dim(dat)[2])[centys>=region[["ymin"]]&centys<=region[["ymax"]]]
  
  ##Step3: generate points (x-crood decided by longitude and month, y-crood decided by latitude and ndvi value (from 0-1))
  df <- do.call(rbind,apply(expand.grid(validxs,validys,1:dim(dat)[3]),MARGIN = 1,function(xx){
    ro <- xx[1] #row
    co <- xx[2] #col
    mo <- xx[3] #half-month
    ndvi <- dat[ro,co,mo]/10000
    
    cent_x <- (ro-1)*(360-gzlo)/(dim(dat)[1]-1)+gzlo/2-180 #x-axis of center point of the grid
    x <- (mo-1)*gzlo*(1-mar)/(dim(dat)[3]-1)+cent_x-gzlo*(1-mar)/2
    cent_y <- (co-1)*(gzla-180)/(dim(dat)[2]-1)+90-gzla/2
    y <- ndvi*gzla*(1-mar)+cent_y-gzla*(1-mar)/2
    
    data.frame(x=x,y=y)
  },simplify = FALSE))
  
  if(na.omit){
    df[!is.na(df$y),]  
  } else{
    df
  }
}
#function to convert ndvi dataset to grids
generate_grid <- function(nc, var="ndvi",
                          region=c(xmin=-180,xmax=180,ymin=-90,ymax=90),
                          na.prop=0){
  ##Step1: read data and determine grid size
  dat <- ncvar_get(nc,var)
  gzlo <- 360/dim(dat)[1]  #gzlo: grid size for longitude
  gzla <- 180/dim(dat)[2]  #gzla: grid size for latitude
  
  ##Step2: choose region
  centxs <- (1:dim(dat)[1]-1)*(360-gzlo)/(dim(dat)[1]-1)+gzlo/2-180
  validxs <- (1:dim(dat)[1])[centxs>=region[["xmin"]]&centxs<=region[["xmax"]]]
  centys <- (1:dim(dat)[2]-1)*(gzla-180)/(dim(dat)[2]-1)+90-gzla/2
  validys <- (1:dim(dat)[2])[centys>=region[["ymin"]]&centys<=region[["ymax"]]]
  
  ##Step3: find center points
  cent_points <- apply(expand.grid(validxs,validys),MARGIN=1,function(xx){
    ro <- xx[1]
    co <- xx[2]
    data_1year <- dat[ro,co,]
    data_1year[data_1year<=0] <- NA###NEW
    if(sum(is.na(data_1year))/length(data_1year)<=na.prop){
      if(!all(na.omit(data_1year)<=1500)){##NEW
        data.frame(
          cent_x = (ro-1)*(360-gzlo)/(dim(dat)[1]-1)+gzlo/2-180, #x-axis of center point of the grid
          cent_y = (co-1)*(gzla-180)/(dim(dat)[2]-1)+90-gzla/2
        )
      } else{
        NULL##NEW
      }
    } else{
      NULL
    }
  },simplify = FALSE)
  if(is.null(cent_points)){
    stop("no grids can be generated.")
  } else{
    cent_points <- do.call(rbind,cent_points)
  }
  
  ##Step4: generate grids
  data.frame(
    xmin=cent_points$cent_x-gzlo/2,
    xmax=cent_points$cent_x+gzlo/2,
    ymin=cent_points$cent_y-gzla/2,
    ymax=cent_points$cent_y+gzla/2
  )
}

##exclude points that are not in grids
exclude_not_in_grid <- function(df, gridr){
  df$save <- F
  for(i in 1:nrow(gridr)){
    xmin <- gridr[i,1]
    xmax <- gridr[i,2]
    ymin <- gridr[i,3]
    ymax <- gridr[i,4]
    df[df$x>xmin&df$x<xmax&df$y>ymin&df$y<ymax,"save"] <- T
  }
  df[df$save,1:2]
}

##procedure-----------------
nc <- nc_open("multi_year_5sec_ndvi.nc")
df <- nc_ndvi2point(nc, region=region)
gridr <- generate_grid(nc, region=region, na.prop = 0.8)
new_df <- exclude_not_in_grid(df, gridr)

df <- new_df


##ADD BACKGROUND--------------
back <- raster("Averaged_Number_of_Season_VIP.nc")
back <- as.data.frame(back,xy=TRUE)
back <- dplyr::rename(back,long=x,lat=y)
back <- back %>% filter(!is.na(Number.of.Season))
#back$Number.of.Season<- factor(back$Number.of.Season)

print(
ggplot()+  #Canvas: 26x12inches  NEW 14.5*7
  borders('world', colour='#778899', size=.01, fill = '#FAFAFA') +
  geom_tile(data = back, aes(x=long,y=lat,fill=Number.of.Season))+
  geom_point(aes(x=df$x,y=df$y),size=0.0000000000000000000000001,shape=20)+
  theme_classic()+ 
  geom_rect(aes(xmin = gridr$xmin, ymin = gridr$ymin, 
                xmax= gridr$xmax, 
                ymax = gridr$ymax),
            alpha=0,color="black",
            size=0.1)+
  xlim(region[["xmin"]],region[["xmax"]])+
  ylim(-60,region[["ymax"]])+
  scale_fill_gradientn(colours=c("#0000CC","#91E4FF","#FFBE00","#8A0000"))+
  easy_remove_axes()
)

