library(tidyverse)
library(vroom)
library(geomorph)
library(abind)
library(imager)
library(exifr)
library(dietr)

#install_exiftool()

#image jdat
im_f <- list.files("data/images",full.names=T,recursive = T) %>% basename
sp <- dirname(list.files("data/images",full.names=T,recursive = T)) %>% basename()
sp_ <- cbind(im_file=im_f,sp=sp) %>% data.frame()

#trophic data
fw <- read_csv("data/FishTraits_14.3_FW.csv")
mar <- read_csv("data/Marine_Traits.csv")

diet <- ConvertFishbaseDiet(ExcludeStage=c("recruits/juv.","larvae"))


fw$DETRITUS
colnames(fw)
colnames(mar)
dist.2d <- function(x1, x2, y1, y2) {
  sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}
f <- list.files("data/points",full.names = T)[-4]

d <- vroom(f,id="file")



d_xy <- d %>% filter(Area==0) %>% data.frame %>% 
  mutate(im_file=gsub("_.csv","",basename(file))) %>% 
  left_join(sp_)
  

jaw_l <- d_xy%>% 
  group_by(file) %>% 
  summarize(
    Li=dist.2d(X[3],X[6],Y[3],Y[6]),
    Lo=dist.2d(X[3],X[1],Y[3],Y[1]),
    MA=Li/Lo,
    Aw=dist.2d(X[3],X[7],Y[3],Y[7]),
    JL=dist.2d(X[1],X[2],Y[1],Y[2]),
    MA_Aw=Aw/Lo
    ) 
  
diet$Taxonomy %>% unique()


#jaw length
jaw_l <- jaw_l %>% 
  mutate(im_file=gsub("_.csv","",basename(file))) %>% 
  left_join(sp_) 

diet
gsub("_.csv","",basename(jaw_l$file))

jaw_area <- d %>% filter(Area!=0) %>% 
  rename(var=`...1`) %>% 
  mutate(var=ifelse(var==8,"Aw_area","Jaw_area")) %>% 
  left_join(jaw_l %>% select(file,JL) %>% unique)

jaw_area %>% 
  filter(var=="Aw_area") %>% 
  ggplot(aes(log(JL),log(Area))) + geom_point()


jaw_l %>% 
  ggplot(aes(MA)) + geom_histogram()


d_xy %>% 
  ggplot(aes(X,Y,col=basename(file)))+
  geom_point()+scale_y_reverse()+coord_equal()+theme(legend.position="none")

xy_l <- d_xy %>% group_by(sp) %>% select(X,Y,sp)%>% group_split(.keep=F) %>% lapply(.,as.matrix)


xy_a <- abind(xy_l,along = 3)

dimnames(xy_a)[[3]] <- d_xy$sp %>% unique

gpa <- gpagen(xy_a,ProcD = F)



plot(gpa)

pca <- gm.prcomp(gpa$coords,transform = F)

plot(pca)

PCA <- pca$x %>% data.frame() %>%  mutate(sp=rownames(pca$x)) %>% 
  left_join(sp_) 

# PCA %>% 
#   ggplot(aes(TrophicLevel,Comp2))+geom_point()+geom_smooth(method="lm")
# al_coor <- lapply(1:dim(gpa$coords)[3], function(x) tibble(f=basename(f[x]),n=1:dim(gpa$coords)[1]) %>% cbind(gpa$coords[,,x])) %>% do.call(rbind,.)


#ignore
plot_spec <- function(f_,img_dir="data/images",col_="orange"){
  

  
  im_f <- list.files(img_dir,full.names = T,pattern= gsub("_.csv","",basename(f_)),recursive = T)
  

  if(length(im_f)==0) stop("No image with filename 'f' in image directory")
  
  xy <- read_csv(f_) %>% select(X,Y) %>% mutate(n=1:n()) %>% filter(X!=0)
  
  im<- load.image(im_f)
  plot(im)
}