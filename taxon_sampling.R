library(fishtree)
library(tidyverse)
library(ape)
library(rfishbase)

#MCZ cleared and stained
mcz_cs <- read_csv("data/mcz_cs.csv") %>% 
  mutate(sp=gsub("(^\\w* \\w*).*","\\1",SCI_NAME_WITH_AUTH_PLAIN),
         type="cs")
#MCZ cleared and skel
mcz_skel <- read_csv("data/mcz_skel.csv") %>% 
  mutate(sp=gsub("(^\\w* \\w*).*","\\1",SCI_NAME_WITH_AUTH_PLAIN),
         type="skel")


mar_dat <- read_csv("data/Marine_Traits.csv") %>% 
  rename(sp=Species)
fw_dat <- read_csv("data/FishTraits_14.3_FW.csv") %>% 
  mutate(sp=paste0(GENUS," ",SPECIES))

#fishtree phylogeny
phy <- fishtree_phylogeny()
phy$tip.label <- gsub("_"," ",phy$tip.label )

#species in mar traits and phy
mar_phy <- base::intersect(mar_dat$sp,phy$tip.label)

#species in fw traits and phy
fw_phy <- base::intersect(fw_dat$sp,phy$tip.label)

#WOW ~1700 sp!
all_sp <- c(mar_phy,fw_phy)

#OK ~400 sp
all_sp_mcz <- base::intersect(all_sp,c(mcz_cs$sp,mcz_skel$sp))

phy_mcz <- keep.tip(phy,all_sp_mcz)

plot(phy_mcz,cex=0.2)

mcz_fb <- species(all_sp_mcz,fields = c("Species","Family")) 
load("fishbase")
  
lt <- load_taxa()
mcz_fb %>% 
  select(Species) %>%  
  left_join(lt %>% select(Species,Family)) %>% 
  mutate(CS=ifelse(Species%in%mcz_cs$sp,"x",NA),
         SK=ifelse(Species%in%mcz_skel$sp,"x",NA), 
    done=NA,comments=NA) %>% write_csv("data/MCZ_specimens.csv")

all_ft <- fishtree_taxonomy()

sapply(mcz_fb$Species, function(x) dir.create(paste0("data/images/",x
)))



lapply(all_ft$rank)
fb_tbl("species") %>% colnames()
