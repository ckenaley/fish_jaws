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

# Cross listing diet data with our data - MG
sp_$has_diet_data <- sp_$sp %in% diet$DietItems$Species
sum(sp_$has_diet_data)
# We have 69 species with diet data - MG

# Trying to calculate trophic levels for our species with data
data(FishBasePreyVals)
converted.diet <- ConvertFishbaseDiet(ExcludeStage=c("recruits/juv.","larvae"))

my.diets <- converted.diet$DietItems %>%  filter(Species %in% sp_$sp)
my.taxonomy <- converted.diet$DietItems %>% filter(Species %in% sp_$sp) %>% select(Individual,Species)

#Calculate Trophic Levels - Initial Diet Data
my.TL <- DietTroph(DietItems = my.diets,PreyValues = FishBasePreyVals, Taxonomy = 
                     my.taxonomy, PreyClass=c("FoodI","FoodII","FoodIII"))


# Calculating Trophic Levels for EXTRA data that was manually compiled by MG
  # freshwater data
fw_extra <- read_csv("fw_fish_extras.csv")
fw_extra <- fw_extra[!apply(is.na(fw_extra), 1, all), ]
fw_tax <- "Archoplites interruptus, Centrarchus macropterus, Fundulus heteroclitus, Gila crassicauda, Lepomis gulosus, Orthodon microlepidotus, Piaractus brachypomus, Ptychocheilus grandis"
fw_tax <- data.frame(
  Species = c(
    "Archoplites interruptus",
    "Centrarchus macropterus",
    "Fundulus heteroclitus",
    "Gila crassicauda",
    "Lepomis gulosus",
    "Orthodon microlepidotus",
    "Piaractus brachypomus",
    "Ptychocheilus grandis"
  )
)


my.TL_fw <- FoodTroph(FoodItems = fw_extra, PreyValues = FishBasePreyVals, Taxonomy = fw_tax, PreyClass = c("FoodI", "FoodII", "FoodIII"), Iter= 100, SE.Type = "TrophLab")


  # marine data
marine_extra <- read_csv("marine_fish_extras.csv")
marine_tax <- data.frame(
  Species = c(
    "Antimora rostrata",
    "Bathysaurus ferox",
    "Brama brama",
    "Coelorinchus coelorinchus",
    "Coryphaenoides rupestris",
    "Ctenochaetus strigosus",
    "Echeneis neucratoides",
    "Genyatremus luteus",
    "Leptocottus armatus",
    "Lutjanus campechanus",
    "Pomacanthus imperator",
    "Porichthys notatus",
    "Priacanthus cruentatus",
    "Sebastomus norvegicus",
    "Selene setapinnis",
    "Tautoga onitis",
    "Thalassoma pavo",
    "Xenodermichthys copei",
    "Zebrasoma flavescens",
    "Zenopsis conchifera"
  )
)

my.TL_marine <- FoodTroph(FoodItems = marine_extra, PreyValues = FishBasePreyVals, Taxonomy = marine_tax, PreyClass = c("FoodI", "FoodII", "FoodIII"), Iter= 100, SE.Type = "TrophLab")

# Cleaning and joining all the TL data
TL_main <- bind_rows(my.TL$Species) %>%
  select(-nObs) %>%                     # remove nObs
  select(Species, TrophicLevel, SE)         

TL_fw <- bind_rows(my.TL_fw) %>%
  rename(Species = Individual) %>%          
  select(-any_of("Items")) %>%           
  select(Species, TrophicLevel, SE)

TL_marine <- bind_rows(my.TL_marine) %>%
  rename(Species = Individual) %>%          
  select(-any_of("Items")) %>%           
  select(Species, TrophicLevel, SE)

# The final compiled trophic level data for our 95 species
TL_all <- bind_rows(TL_main, TL_fw, TL_marine)

# MA
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
  mutate(im_file = gsub("_.csv", "", basename(file))) %>% 
  left_join(sp_) %>% 
  left_join(TL_all %>% select(Species, TrophicLevel, SE),
            by = c("sp" = "Species"))

jaw_l <- jaw_l %>%
  filter(!is.na(TrophicLevel))

diet
gsub("_.csv","",basename(jaw_l$file))

jaw_area <- d %>% filter(Area!=0) %>% 
  rename(var=`...1`) %>% 
  mutate(var=ifelse(var==8,"Aw_area","Jaw_area")) %>% 
  left_join(jaw_l %>% select(file,JL) %>% unique)

jaw_area %>% 
  filter(var=="Aw_area") %>% 
  ggplot(aes(log(JL),log(Area))) + geom_point()
  # error - invalid grpahics state? - MG

jaw_l %>% 
  ggplot(aes(MA)) + geom_histogram()
# error - invalid grpahics state? - MG

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

PCA <- pca$x %>%
  data.frame() %>%
  mutate(sp = rownames(pca$x)) %>%
  left_join(sp_, by = "sp") %>%
  left_join(TL_all, by = c("sp" = "Species"))


# PCA %>% 
#   ggplot(aes(TrophicLevel,Comp2))+geom_point()+geom_smooth(method="lm")
# al_coor <- lapply(1:dim(gpa$coords)[3], function(x) tibble(f=basename(f[x]),n=1:dim(gpa$coords)[1]) %>% cbind(gpa$coords[,,x])) %>% do.call(rbind,.)

# Plotting PCA colored by trophic level - MG
PCA %>%
  ggplot(aes(x = Comp1, y = Comp2, color = TrophicLevel)) +
  geom_point(na.rm = FALSE) +
  geom_smooth(data = subset(PCA, !is.na(TrophicLevel)), method = "lm") +
  scale_color_viridis_c(na.value = "lightgray") +
  theme_minimal()

al_coor <- lapply(1:dim(gpa$coords)[3], function(x) tibble(f=basename(f[x]),n=1:dim(gpa$coords)[1]) %>% cbind(gpa$coords[,,x])) %>% do.call(rbind,.)
  
# Plotting MA vs Trophic level - MG
jaw_l %>%
  ggplot(aes(x = MA, y = TrophicLevel)) + geom_point() + geom_smooth(method="lm")
    # general ideas: negative association (High MA = lower TL)
      # Relationship is weak with high scatter, large vertical spread across MA values
      # Suggests MA alone is not sufficient to predict trophic level
      # Suggests a multivariate or phylogenetically informed model needed

# some next steps:
# centroid size as a proxy for size
# functions in geomorph that work on procrustes shape info
# if we have the aligned landmarks: pleth.pgls function
# test of whether shape varies by size (c size =  centroid size)
# specify your variables that you want to include in the model other than the coordinates
# use library(fishtree) - loads a big phylogeny tree; load Rabosky tree


library(dplyr)
library(geomorph)
library(fishtree)
library(ape)


# Define specimen order and species list
##    (sp is longer; shape data has N files)
specimens <- unique(al_coor$f)
N <- length(specimens)                 # e.g., 129

sp_used <- sp[1:N]                     # species in same order as specimens
stopifnot(length(sp_used) == N)


# Validate landmarks and build p x k x N array

p <- 7
k <- 2

bad <- al_coor %>%
  group_by(f) %>%
  summarise(
    n_pts = n(),
    n_unique = n_distinct(n),
    min_n = min(n),
    max_n = max(n),
    .groups = "drop"
  ) %>%
  filter(!(n_pts == p & n_unique == p & min_n == 1 & max_n == p))

if (nrow(bad) > 0) {
  print(bad)
  stop("Some specimens do not have exactly 7 landmarks labeled 1..7.")
}

Y <- array(NA, dim = c(p, k, N),
           dimnames = list(
             landmark = 1:p,
             coord = c("X","Y"),
             species = sp_used
           ))

for (i in seq_along(specimens)) {
  df_i <- al_coor %>% filter(f == specimens[i]) %>% arrange(n)
  Y[,,i] <- as.matrix(df_i[, c("X","Y")])
}

stopifnot(all(dim(Y) == c(p, k, N)))


# GPA to get aligned coords + centroid size
gpa <- gpagen(Y, print.progress = FALSE)
Y_aligned <- gpa$coords
Csize <- gpa$Csize


# Fix duplicate species in sp_used (shape side)
##    keep only first occurrence per species

keep_idx <- !duplicated(sp_used)

sp_used2 <- sp_used[keep_idx]
Y_aligned2 <- Y_aligned[,, keep_idx]
Csize2 <- Csize[keep_idx]

stopifnot(!any(duplicated(sp_used2)))
stopifnot(dim(Y_aligned2)[3] == length(sp_used2))
stopifnot(length(Csize2) == length(sp_used2))


TL_all$Species <- as.character(TL_all$Species)

TL_all2 <- TL_all %>%
  group_by(Species) %>%
  summarise(
    TrophicLevel = mean(TrophicLevel, na.rm = TRUE),
    SE = mean(SE, na.rm = TRUE),
    .groups = "drop"
  )

TL_all2 <- as.data.frame(TL_all2)
stopifnot(!any(duplicated(TL_all2$Species)))


# Drop species with missing metadata (TL_all), then build dat

keep_meta <- sp_used2 %in% TL_all2$Species

dropped_meta <- sp_used2[!keep_meta]
if (length(dropped_meta) > 0) {
  print(dropped_meta)
  cat("Dropping", length(dropped_meta), "species with missing metadata.\n")
}

sp_used3 <- sp_used2[keep_meta]
Y_aligned3 <- Y_aligned2[,, keep_meta]
Csize3 <- Csize2[keep_meta]

dat <- TL_all2[match(sp_used3, TL_all2$Species), , drop = FALSE]
dat <- as.data.frame(dat)

dat$Csize <- Csize3
rownames(dat) <- dat$Species
stopifnot(all(rownames(dat) == sp_used3))


# Phylogeny: normalize names, drop species not in tree

phy <- fishtree_phylogeny()
phy$tip.label <- gsub("_", " ", phy$tip.label)

norm_names <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

lower_epithet <- function(x) {
  parts <- strsplit(x, " ")
  vapply(parts, function(p) {
    if (length(p) >= 2) paste(p[1], tolower(p[2]), sep = " ") else p[1]
  }, character(1))
}

# normalize species labels everywhere the same way
sp_used3 <- lower_epithet(norm_names(sp_used3))
dimnames(Y_aligned3)[[3]] <- sp_used3
rownames(dat) <- lower_epithet(norm_names(rownames(dat)))
dat$Species <- lower_epithet(norm_names(dat$Species))
phy$tip.label <- lower_epithet(norm_names(phy$tip.label))

# drop any species not present in the phylogeny
keep_tree <- sp_used3 %in% phy$tip.label

dropped_tree <- sp_used3[!keep_tree]
if (length(dropped_tree) > 0) {
  print(dropped_tree)
  cat("Dropping", length(dropped_tree), "species not found in phylogeny.\n")
}

sp_used4 <- sp_used3[keep_tree]
Y_aligned4 <- Y_aligned3[,, keep_tree]
Csize4 <- Csize3[keep_tree]

# rebuild dat to match sp_used4 exactly (guarantees correct order)
dat <- TL_all2[match(sp_used4, lower_epithet(norm_names(TL_all2$Species))), , drop = FALSE]
dat <- as.data.frame(dat)

dat$Species <- sp_used4
dat$Csize <- Csize4
rownames(dat) <- dat$Species
stopifnot(all(rownames(dat) == sp_used4))


# Prune tree, reorder shape + data to tree order (robust)
phy2 <- keep.tip(phy, sp_used4)
sp2 <- phy2$tip.label

# ensure all tree tips exist in dat
missing_dat <- setdiff(sp2, rownames(dat))
if (length(missing_dat) > 0) {
  cat("Tree tips missing from dat (first 25):\n")
  print(head(missing_dat, 25))
  stop("Species-name mismatch between phylogeny and dat.")
}

# reorder using match (never fails if names truly match)
idx <- match(sp2, rownames(dat))
stopifnot(!anyNA(idx))

dat2 <- dat[idx, , drop = FALSE]
rownames(dat2) <- sp2

Y2 <- Y_aligned4[,, match(sp2, sp_used4)]
stopifnot(all(dimnames(Y2)[[3]] == sp2))
stopifnot(all(rownames(dat2) == sp2))


# Run procD.pgls

# allometry test
fit_size <- procD.pgls(Y2 ~ Csize, phy = phy2, data = dat2, iter = 999)
summary(fit_size)

## Size is significant? - MG

# add trophic level
fit_tl <- procD.pgls(Y2 ~ Csize + TrophicLevel, phy = phy2, data = dat2, iter = 999)
summary(fit_tl)
## Trophic level NOT significant (0.157)


# MA
library(dplyr)

jaw_l_ma <- jaw_l %>%
  mutate(sp = as.character(sp)) %>%
  group_by(sp) %>%
  summarise(
    MA = mean(MA, na.rm = TRUE),
    .groups = "drop"
  )

stopifnot(!any(duplicated(jaw_l_ma$sp)))

dat2 <- dat2 %>%
  mutate(Species = as.character(Species)) %>%
  left_join(jaw_l_ma, by = c("Species" = "sp"))

dat2 <- as.data.frame(dat2)

keep_ma <- !is.na(dat2$MA)

dat2 <- dat2[keep_ma, , drop = FALSE]
Y2   <- Y2[,, keep_ma]

stopifnot(nrow(dat2) == dim(Y2)[3])
rownames(dat2) <- dat2$Species

# Species remaining after MA filtering
sp_final <- dat2$Species

# prune tree to exactly those species
phy2_ma <- keep.tip(phy2, sp_final)
sp_tree <- phy2_ma$tip.label

# reorder dat2 to match tree order
idx_dat <- match(sp_tree, dat2$Species)
stopifnot(!anyNA(idx_dat))
dat2_ma <- dat2[idx_dat, , drop = FALSE]
rownames(dat2_ma) <- dat2_ma$Species

# reorder Y2 to match tree order
idx_Y <- match(sp_tree, dimnames(Y2)[[3]])
stopifnot(!anyNA(idx_Y))
Y2_ma <- Y2[,, idx_Y]
dimnames(Y2_ma)[[3]] <- sp_tree

stopifnot(nrow(dat2_ma) == length(sp_tree))
stopifnot(dim(Y2_ma)[3] == length(sp_tree))

fit_ma <- procD.pgls(
  Y2_ma ~ Csize + MA,
  phy = phy2_ma,
  data = dat2_ma,
  iter = 999
)
summary(fit_ma)

# MA not significant (0.110)

# next steps
# use stereomorphs to get curves and landmarks all together 
# semilandmarks have to be an odd number of points 


#ignore
plot_spec <- function(f_,img_dir="data/images",col_="orange"){
  

  
  im_f <- list.files(img_dir,full.names = T,pattern= gsub("_.csv","",basename(f_)),recursive = T)
  

  if(length(im_f)==0) stop("No image with filename 'f' in image directory")
  
  xy <- read_csv(f_) %>% select(X,Y) %>% mutate(n=1:n()) %>% filter(X!=0)
  
  im<- load.image(im_f)
  plot(im)
}