### WEEREEWA (LAKE GEORGE) BACKED ARTEFACTS DRAFT R SCRIPT FOR LANDMARK ANALYSIS ###

### ARTICLE TITLE: One site, multiple backed artefact shapes: the localised production of specific tool shapes 
### for re-hafting ###
### AUTHORS: Amy Mosig Way, Loukas Koungoulos, Simon Wyatt-Spratt ###
### SCRIPT AUTHORS: Amy Mosig Way, Loukas Koungoulos, Simon Wyatt-Spratt ###
### SCRIPT CONTACT: amy.way@australian.museum ###
### LAST EDITED: 02/12/2021 ###

### SOME ACKNOWLEDGEMENT THAT WE'VE MOSTLY CRIBBED THIS SCRIPT FROM? ###

### ABSTRACT ###
### This paper examines backed artefact standardisation at Lake George, where a variety of backed artefact forms are
### known to occur. We find a pattern of clustering within sites of particular forms and argue that this 
### standardisation relates to localised re-tooling/re-hafting events. In addition, we find a separation between the 
### two forms regularly identified in Australia - geometric microliths and elongated asymmetrical backed points - 
### in terms of raw material and time, with elongated asymmetrical backed points only made on silcrete and chert, 
### while geometric microliths are produced on silcrete, chert and quartz, with strong alignment in shape across the 
### raw material groups. Quartz backed artefacts appear later in the sequence, and when this raw material is added to
### the backing repertoire, silcrete and chert are worked to produce matching forms. To better understand this 
### bi-modal pattern, we consider this site within continental trends, to ask what social processes are being mapped 
### by the distribution of these two standardised backed artefact forms? These two forms appear across language, 
### cultural, and environmental boundaries, from the eastern seaboard to the north-west of Western Australia, and 
### this lack of alignment between the distribution of material culture and the boundaries of single ethnic or 
### linguistic groups, leads us to ask what social processes are being mapped by the continental distribution of 
### these standardised objects? And do the two forms map different social processes? ###

### SYSTEM INFORMATION ###
### R version 4.1.1 (2021-08-10) # SWS - I think there's been an update to R - might need to download?
### Platform: x86_64-w64-mingw32/x64 (64-bit) # SWS - whatever computer we do a final test on
### Running under: Windows 10 x64 (build 19042.928) # SWS - whatever system we do a final test on

### ATTACHED BASE PACKAGES: (SWS - not quite sure what this is for but I'm copying from Hoggard - no idea what the
### numbers refer to.)

### [1] stats     graphics  grDevices utils     datasets 
### [6] methods   base 

### Install and load packages
if(!require("geomorph")) install.packages("geomorph", repos = "http://cran.us.r-project.org") # geomorph 4.0.1
if(!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.us.r-project.org") # ggplot2 3.3.5
if(!require("Momocs")) install.packages("Momocs", repos = "http://cran.us.r-project.org") # Momocs 1.3.2
if(!require("tibble")) install.packages("tibble", repos = "http://cran.us.r-project.org") # tibble 3.1.3
if(!require("tidyr")) install.packages("tidyr", repos = "http://cran.us.r-project.org") # tidyr 1.1.4

library(geomorph)
library(ggplot2)
library(Momocs)
library(tibble)
library(tidyr)

# set working directory #note: Amy at home, Amy.Way at the Museum - SWS - change to something more generic or link to
# upload data in Github (are we doing that?)

### Import Data - *.tps and *.csv files
LGBA_DISCARD_lm<-import_tps("LG_95_for paper.TPS")
LGBA_DISCARD_data<- read.csv("95_LG_BA_FINAL.csv", header=T)

### Change to factors
LGBA_DISCARD_data$DISCARD             <- as.factor(LGBA_DISCARD_data$DISCARD)
LGBA_DISCARD_data$nodule              <- as.factor(LGBA_DISCARD_data$nodule_X)
LGBA_DISCARD_data$nodule_Area         <- as.factor(LGBA_DISCARD_data$nodule_Area)
LGBA_DISCARD_data$nodule_two_or_more  <- as.factor(LGBA_DISCARD_data$nodule_two_or_more)
LGBA_DISCARD_data$SITE                <- as.factor(LGBA_DISCARD_data$SITE)
LGBA_DISCARD_data$raw_material        <- as.factor(LGBA_DISCARD_data$raw_material)
LGBA_DISCARD_data$SITE_detail         <- as.factor(LGBA_DISCARD_data$SITE_detail)
LGBA_DISCARD_data$import              <- as.factor(LGBA_DISCARD_data$import)
LGBA_DISCARD_data$Use_wear            <- as.factor(LGBA_DISCARD_data$Use_wear)
LGBA_DISCARD_data$Area                <- as.factor(LGBA_DISCARD_data$Area)
LGBA_DISCARD_data$Max_Length          <- as.factor(LGBA_DISCARD_data$Max_Length)
LGBA_DISCARD_data$Max_Width           <- as.factor(LGBA_DISCARD_data$Max_Width)
LGBA_DISCARD_data$Round               <- as.factor(LGBA_DISCARD_data$Round)
LGBA_DISCARD_data$Max_thick           <- as.factor(LGBA_DISCARD_data$Max_thick)
LGBA_DISCARD_data$elongation          <- as.factor(LGBA_DISCARD_data$elongation)

### Convert artefact ID column into the row names
LGBA_DISCARD_data <- column_to_rownames(LGBA_DISCARD_data, var = "ID")

### Define semi-landmarks and link data
semi.LMs <- matrix(
  c(1,2,3,  2,3,4, 3,4,5,  4,5,6,  5,6,7,  6,7,8,  7,8,9,   9,10,11,  10,11,12,  11,12,13,  12,13,14,  13,14,15,
    14,15,16,  15,16,1),
  ncol = 3, byrow = TRUE)

fw.links<-matrix(c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,10, 10,11, 11,12, 12,13, 13,14, 14,15, 15,16, 16,1),
                 ncol = 2, byrow = TRUE)

### Momocs link data
BA_Ldk <- Ldk(coo = LGBA_DISCARD_lm$coo,fac = LGBA_DISCARD_data, 
              links= fw.links, slidings = semi.LMs ) # Remove slidings = semi-landarks for fixed landmarks

### Perform GPA with sliding landmarks
gpa_95_slid  <- fgsProcrustes(BA_Ldk)

### Perform PCA
pca_95_slid  <- PCA(gpa_95_slid)

### Draw the mean shape of each nodule
ba_nd.ms <- MSHAPES(gpa_95_slid, ~nodule_X)
Out(ba_nd.ms$shp) %>% panel(names=TRUE, col = col_qual(32))

### Draw the mean shape of each raw material
ba_rm.ms <- MSHAPES(gpa_95_slid, ~raw_material)
Out(ba_rm.ms$shp) %>% panel(names=TRUE, col = col_qual(3))

### Draw the mean shape of total sample
bot.all<-LGBA_DISCARD_lm$coo %>% MSHAPES() %>% coo_plot() #SWS - is it supposed to open?

### Plot every object
# panel(gpa_BA_slid, fac="raw_material", names=TRUE) # SWS not working - no object gpa_BA_slid
# panel(gpa_95_slid, fac="raw_material", names=TRUE) # SWS still not working 
#- "Error in ldk_i[links[j, 2], 1] : subscript out of bounds"

### Plot gpa stack
stack(gpa_95_slid)

###  FIGURE ?. PC plot grouped by raw material
figure_pca_rm_plot <- plot_PCA(pca_95_slid, ~raw_material, morphospace=FALSE, legend = FALSE, points=TRUE, chull = TRUE, 
            chullfilled = TRUE, axesnames=FALSE,  axesvar = FALSE)
layer_points(figure_pca_rm_plot, pch = 16, cex = 0.5)
#layer_labelpoints(figure_pc_plot, cex=1.5)
layer_legend(figure_pca_rm_plot, cex = 1.5)
layer_axesnames(figure_pca_rm_plot, cex = 1.5, name = "PC")
layer_axesvar(figure_pca_rm_plot,cex = 1.5)
layer_grid(figure_pca_rm_plot, col = "#999999", lty = 3, grid = 2)
layer_morphospace_PCA(figure_pca_rm_plot,
                      position = c("range")[1],
                      nb = 12,
                      nr = 3,
                      nc = 6,
                      rotate = 0,
                      size = 0.6,
                      col = "black",
                      flipx = FALSE,
                      flipy = FALSE,
                      draw = TRUE
)

###  FIGURE ?. LDA plot grouped by raw material
plot(pca_95_slid) # NOTE - have to produce this plot first to produce the LDA
BA.lda_rm_slid <- LDA(gpa_95_slid, ~raw_material)

figure_lda_rm_plot <- plot_LDA(BA.lda_rm_slid, legend = FALSE, points=TRUE, chull = TRUE, chullfilled = TRUE, 
                            axesnames=FALSE, axesvar = FALSE)
layer_points(figure_lda_rm_plot, pch = 16, cex = 0.5)
layer_legend(figure_lda_rm_plot, cex = 1.5)
layer_axesnames(figure_lda_rm_plot, cex = 1.5, name = "LD")
layer_axesvar(figure_lda_rm_plot,cex = 1.5)
layer_grid(figure_lda_rm_plot, col = "#999999", lty = 3, grid = 2)

###  FIGURE ?. PC plot grouped by nodules
figure_pca_nd_plot <- plot_PCA(pca_95_slid, ~nodule_Area, morphospace=FALSE, legend = FALSE, points=TRUE, chull = TRUE, 
                            chullfilled = TRUE, axesnames=FALSE,  axesvar = FALSE)
layer_points(figure_pca_nd_plot, pch = 16, cex = 0.5)
#layer_labelpoints(figure_pc_plot, cex=1.5)
layer_legend(figure_pca_nd_plot, cex = 1.5)
layer_axesnames(figure_pca_nd_plot, cex = 1.5, name = "PC")
layer_axesvar(figure_pca_nd_plot,cex = 1.5)
layer_grid(figure_pca_nd_plot, col = "#999999", lty = 3, grid = 2)
layer_morphospace_PCA(figure_pca_nd_plot,
                      position = c("range")[1],
                      nb = 12,
                      nr = 3,
                      nc = 6,
                      rotate = 0,
                      size = 0.6,
                      col = "black",
                      flipx = FALSE,
                      flipy = FALSE,
                      draw = TRUE
)

###  FIGURE ?. LDA plot grouped by nodules
BA.lda_nd_slid <- LDA(gpa_95_slid, ~ nodule_Area)

figure_lda_nd_plot <- plot_LDA(BA.lda__nd_slid, legend = FALSE, points=TRUE, chull = TRUE, chullfilled = TRUE, 
                            axesnames=FALSE, axesvar = FALSE)
layer_points(figure_lda_nd_plot, pch = 16, cex = 0.5)
layer_legend(figure_lda_nd_plot, cex = 1.5)
layer_axesnames(figure_lda_nd_plot, cex = 1.5, name = "LD")
layer_axesvar(figure_lda_nd_plot,cex = 1.5)
layer_grid(figure_lda_nd_plot, col = "#999999", lty = 3, grid = 2)

### Plot nodule_plot
#Omit NAs from columns 8 and 9 - nodule two or more and nodule_area
LGBA_na<-LGBA_DISCARD_data %>% drop_na(nodule_Area, nodule_two_or_more) 

### Remove nas from 'nodule_two_or_more
nodules65_lm<-import_tps("65_LG_BAs_clean_nodules.tps") #SWS - suggestion - move all data importing up to the start

nodules65_data<- read.csv("65_nodules.csv", header=T)

### Make into factors
nodules65_data$nodule_area            <- as.factor(nodules65_data$nodule_Area)
nodules65_data$nodule_three_or_more   <- as.factor(nodules65_data$nodule_three_or_more) # SWS - why three or more?
nodules65_data$nodule_two_or_more     <- as.factor(nodules65_data$nodule_two_or_more)
nodules65_data$raw_material           <- as.factor(nodules65_data$raw_material)
nodules65_data$discard                <- as.factor(nodules65_data$DISCARD)
nodules65_data$site                   <- as.factor(nodules65_data$SITE)
nodules65_data$site_detail            <- as.factor(nodules65_data$SITE_detail)
nodules65_data$import                 <- as.factor(nodules65_data$import)
nodules65_data$use_wear               <- as.factor(nodules65_data$Use_wear)
nodules65_data$max_Length             <- as.factor(nodules65_data$Max_Length)
nodules65_data$max_Width              <- as.factor(nodules65_data$Max_Width)
nodules65_data$round                  <- as.factor(nodules65_data$Round)
nodules65_data$max_thick              <- as.factor(nodules65_data$Max_thick)

### column to rowname
nodules65_data <- column_to_rownames(nodules65_data, var = "ID") # SWS - comes up with an error message for me

Ldk_nodules65 <- Ldk(coo = nodules65_lm$coo,fac = nodules65_data, 
              links= fw.links, slidings = semi.LMs ) #remove slidings=semiLMs for fixed landmarks
gpa_BA_slid_nods65<- fgsProcrustes(Ldk_nodules65)
pca_BA_slid_nods65 <- PCA(gpa_BA_slid_nods65)

### Draw the mean shape of each raw material
ba_nd65.ms <- MSHAPES(gpa_BA_slid_nods65, ~nodule_two_or_more) #SWS - not drawing raw materials, wrong factor in code
# SWS cont. I cannot figure out a way to add raw_material as a factor the mshape object - or if there's another way
# SWS cont. to do it. I can't manually choose the colors in any way either. 
Out(ba_nd65.ms$shp) %>% panel(names=TRUE, fac = "raw_material", col = col_qual(3))

ba_rm65.ms <- MSHAPES(gpa_BA_slid_nods65, ~raw_material)
Out(ba_rm65.ms$shp) %>% panel(names=TRUE, col = col_qual(3))

#SWS - same issue as above - I cannot get colours into the meanshape

### pc plot - nodule two or more
p<-plot_PCA(pca_BA_slid_nod65s, ~nodule_Area, morphospace=FALSE, 
            legend = FALSE, points=TRUE, chull = TRUE, chullfilled = TRUE, 
            axesnames=FALSE,  axesvar = FALSE)
layer_points(p, pch = 16, cex = 0.5)
#layer_labelpoints(p, cex=1, col='black')
layer_legend(p, cex = 1.5)
layer_axesnames(p, cex = 1.5, name = "PC")
layer_axesvar(p,cex = 1.5)
layer_grid(p, col = "#999999", lty = 3, grid = 2)
layer_morphospace_PCA(p,
                      position = c("range")[1],
                      nb = 12,
                      nr = 3,
                      nc = 6,
                      rotate = 0,
                      size = 0.6,
                      col = "black",
                      flipx = FALSE,
                      flipy = FALSE,
                      draw = TRUE
)

### then LDA plot
BA.lda_slid<-LDA(pca_BA_slid_nod65s, ~ nodule_Area)
plot_LDA(BA.lda_slid, chullfilled = TRUE, zoom = 1.2, box = TRUE, points = TRUE)
x<-plot_LDA(BA.lda_slid, legend = FALSE,  zoom = 1.2,chull = TRUE, chullfilled = TRUE, axesnames=FALSE,
            axesvar = FALSE)
layer_points(x, pch = 16, cex = 0.5)
#layer_labelpoints(x, cex=1, col='black')
layer_legend(x, cex = 1.5)
layer_axesnames(x, cex = 1.5, name = "LD")
layer_axesvar(x,cex = 1.5)
layer_grid(p, col = "#999999", lty = 3, grid = 2)
layer_morphospace_LDA(x,
                      position = c("range")[1],
                      nb = 12,
                      nr = 3,
                      nc = 6,
                      rotate = 0,
                      size = 0.6,
                      col = "black",
                      flipx = FALSE,
                      flipy = FALSE,
                      draw = TRUE
)

#comes up with error which is preventing morphospace display
#Error in apply(coo, 2, function(x) diff(range(x))) : dim(X) must have a positive length

### FIGURE 2A
#plot gpa stack - remove links for plotting GPA
BA_Ldk2 <- Ldk(coo = BA_lm$coo,fac = BA_data,links= NULL, slidings = NULL ) # SWS - not working - wrong object

BA_Ldk2 <- Ldk(coo = BA_Ldk$coo,fac = LGBA_DISCARD_data,links= NULL, slidings = NULL ) # SWS - I've edited the above
#code to this - however I've not sure I've substituted the right objects - so please doublecheck

### plot gpa stack
stack(fgProcrustes(BA_Ldk2))

### draw the mean shape of each site
site.ms <- MSHAPES(gpa_BA_slid, ~site) # SWS - does not work - wrong object, wrong factor
Out(site.ms$shp) %>% panel(names=TRUE)

site.ms <- MSHAPES(gpa_95_slid, ~SITE) # SWS - correct version of the above
Out(site.ms$shp) %>% panel(names=TRUE)

### draw the mean shape of total sample
all.ms<-MSHAPES(gpa_BA_slid) # SWS - does not work - wrong object
coo_plot(all.ms, cex=1) 

all.ms<-MSHAPES(gpa_95_slid) # SWS - correct version of the above
coo_plot(all.ms, cex=1) 


###  FIGURE 2D kmeans clustering
KMEANS(pca_95_slid, centers = 5, pch = 16, cex=1.2, title(main = "Nodule 95, KMeans 5"))

KMEANS(pca_BA_slid_nods65, centers = 5, pch = 16, cex=1.2, title(main = "Nodule 65, KMeans 5"))

#67 nodules - SWS - should this be 65 nodules
KMEANS(pca_BA_slid_nods65, centers = 3, pch = 16, cex=1.2, title(main = "Nodule 65, KMeans 3")) # SWS - 

### boxplot nodules 2 or more elongation
nods_data<- read.csv("95_LG_BA_FINAL.csv", header=T)

### subset to remove NAs
x<-subset(nods_data, !is.na(nodule_two_or_more)) #RETURNS N=65

### boxplot
ggplot(x, aes(nodule, elongation)) + # SWS object "nodule" incorrect
  geom_boxplot() +
  geom_point()+ # can add (position = 'jitter')
 theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12))+
  labs(x="Nodule", y="BASI", size=12)

ggplot(x, aes(nodule_X, elongation)) + #SWS - is nodule_X the right factor, or should it be nodule_two_or_more
  geom_boxplot() +
  geom_point()+ # can add (position = 'jitter')
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12))+
  labs(x="Nodule", y="BASI", size=12)


#scatterplot of shape (elongation) by import for each area
#first subset

F1 <- subset(nods_data, nodule_Area=="F1")
F2 <- subset(nods_data, nodule_Area=="F2")
F3 <- subset(nods_data, nodule_Area=="F3")
G <- subset(nods_data, nodule_Area=="G1")
N4 <- subset(nods_data, nodule_Area=="N4")
N14 <- subset(nods_data, nodule_Area=="N14")

ggplot(nods_data, aes(x = elongation, y = nodule_Area, group = import_2)) + 
  geom_point(aes(size = import_2, shape = import_2, color = import_2)) +
  # geom_text(aes(label=nodule_two_or_more))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))+
  scale_color_manual(values=c(1,1,1,1,1,1,1, 2,2,2,2,3,3,3))+
  # scale_size_manual(values=c(2,2,2,2,2,2,2))+
  # ggtitle("PC1 and PC2 by BCJ Area F - three activity areas") +
  # legend(title = "Users By guides")+
  xlab("Elongation")+
  ylab ("Area")+
  # labs(color = "Nodule", shape="nodule_two_or_more")+
  # geom_smooth(method='lm', colour = "nodule_two_or_more", se = FALSE)+
  #stat_ellipse(type = "norm", linetype = 2, level = 0.8) +
  #geom_text(aes(label = ID), nudge_x = 0.01, size = 3)
  theme_minimal()

### SWS - question about the above - what is part of the code #?

#scatterplot of 65 nodules - elongation by BASI

ggplot(x, aes(x = elongation, y = BASI_matlab, group = nodule_two_or_more)) + 
  geom_point(aes(size = nodule_two_or_more, shape = nodule_two_or_more, color = nodule_two_or_more)) +
  # geom_text(aes(label=nodule_two_or_more))+
scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))+
  scale_color_manual(values=c(1,1,1,1,1,1,1, 2,2,2,2,3,3,3))+
# scale_size_manual(values=c(2,2,2,2,2,2,2))+
 # ggtitle("PC1 and PC2 by BCJ Area F - three activity areas") +
 # legend(title = "Users By guides")+
  xlab("elongation")+
  ylab ("BASI")+
 # labs(color = "Nodule", shape="nodule_two_or_more")+
 # geom_smooth(method='lm', colour = "nodule_two_or_more", se = FALSE)+
  #stat_ellipse(type = "norm", linetype = 2, level = 0.8) +
  #geom_text(aes(label = ID), nudge_x = 0.01, size = 3)
  theme_minimal()

#scatterplot of 105 nodules - elongation by BASI

ggplot(nods_data, aes(x = elongation, y = BASI_matlab, group = SITE)) + 
  geom_point(aes(size = SITE, shape = SITE, color = SITE)) +
  # geom_text(aes(label=nodule))+
  scale_shape_manual(values=c(16,16,16,16))+
 # scale_color_manual(values=c(1,2,3,4))+
   scale_size_manual(values=c(3,3,3,3,2,2))+
  # ggtitle("PC1 and PC2 by BCJ Area F - three activity areas") +
  # legend(title = "Users By guides")+
  xlab("elongation")+
  ylab ("BASI")+
  # labs(color = "Nodule", shape="nodule_two_or_more")+
  #geom_smooth(method='lm', colour = "nodule_two_or_more", se = FALSE)+
  stat_ellipse(type = "norm", linetype = 2, level = 0.8) +
  #geom_text(aes(label = ID), nudge_x = 0.01, size = 3)
  theme_minimal()


#scatterplot of 105 nodules - elongation by spit, group = RM

ggplot(nods_data, aes(x = elongation, y = spit, group = raw_material)) + 
  geom_point(aes(size = raw_material, shape = raw_material, color = raw_material)) +
  # geom_text(aes(label=nodule))+
  scale_shape_manual(values=c(16,16,16,16,16,16))+
  scale_color_manual(values=c("#52BE80", "darkorange1","deepskyblue3"))+
  scale_size_manual(values=c(4,4,4,4,4,4))+
  # ggtitle("PC1 and PC2 by BCJ Area F - three activity areas") +
  # legend(title = "Users By guides")+
  xlab("elongation")+
  ylab ("spit")+
  # labs(color = "Nodule", shape="nodule_two_or_more")+
  #geom_smooth(method='lm', se = TRUE)+
 # stat_ellipse(geom = "polygon") +
  stat_ellipse(type = "norm", linetype = 2, level = 0.8) +
  #geom_text(aes(label = ID), nudge_x = 0.01, size = 3)
  theme_minimal()

#scatterplot of 105 nodules - elongation by spit, group = RM

data105<- read.csv("105_LG_BA_FINAL.csv", header=T) # SWS - incorrect file name?

data95<- read.csv("95_LG_BA_FINAL.csv", header=T) # SWS - not sure why this is here? it works though.


ggplot(nods_data, aes(x = elongation, y = spit, group = nodule_Area)) + 
  geom_point(aes(size = nodule_Area, shape = nodule_Area, color = nodule_Area)) +
  # geom_text(aes(label=nodule))+
  scale_shape_manual(values=c(16,16,16,16,16,16,16,16))+
  #scale_color_manual(values=c("#52BE80", "darkorange1","deepskyblue3"))+
  scale_size_manual(values=c(4,4,4,4,4,4,4,4))+
  # ggtitle("PC1 and PC2 by BCJ Area F - three activity areas") +
  # legend(title = "Users By guides")+
  xlab("elongation")+
  ylab ("spit")+
  # labs(color = "Nodule", shape="nodule_two_or_more")+
  #geom_smooth(method='lm', se = TRUE)+
  # stat_ellipse(geom = "polygon") +
  stat_ellipse(aes(x=elongation, y=spit,color=nodule_Area),type = "norm", linetype = 2, level = 0.9)+
 # stat_ellipse(type = "norm", linetype = 2, level = 0.9) +
  #geom_text(aes(label = ID), nudge_x = 0.01, size = 3)
  theme_minimal()

##########################################################################
### SIMON'S CONTRIBUTION (TO BE SLOTTED IN WHERE APPROPRIATE) ############
##########################################################################

### Apologies in advance for the messy code and statistical ignorance ####

##########################################################################
########### FIGURE 3. PCA and LDA of activity areas ######################
##########################################################################

# Figure 3. a). Principal Component Analysis of landmarks of all nodules (n=65) that produced ???2 backed artefacts by 
# activity area. PC1 and PC2 shown (87.2% of cumulative variance). b). Linear Discriminant  Analysis of nodules 
# (n=65) that produced ???2 backed artefacts by activity area. LD1 and LD2 shown (85.4% of cumulative variance).  
# Created in Momocs (Bonhomme et al. 2014).

#3a. PC Plot 

scree(pca_BA_slid_nods65)
scree_plot(pca_BA_slid_nods65, nax = 1:15)
PCcontrib(pca_BA_slid_nods65, nax = 1:4)

figure_3a <- plot_PCA(pca_BA_slid_nods65, ~nodule_Area, morphospace = FALSE,
         legend = FALSE, points = TRUE, chull = TRUE, chullfilled = TRUE,
         axesnames = FALSE,  axesvar = FALSE)
layer_points(figure_3a, pch = 16, cex = 0.5)
layer_legend(figure_3a, cex = 1.5)
layer_axesnames(figure_3a, cex = 1.5, name = "PC")
layer_axesvar(figure_3a,cex = 1.5)
layer_grid(figure_3a, col = "#999999", lty = 3, grid = 2)
layer_morphospace_PCA(figure_3a,
                      position = c("range")[1],
                      nb = 12,
                      nr = 3,
                      nc = 6,
                      rotate = 0,
                      size = 0.6,
                      col = "black",
                      flipx = FALSE,
                      flipy = FALSE,
                      draw = TRUE
)

#3b. LD Plot

lda_BA_slid_nods65 <-LDA(pca_BA_slid_nods65, ~nodule_Area)

figure_3b <- plot_LDA(lda_BA_slid_nods65, axes = c(1, 2), zoom = 1.25, morphospace = FALSE,
                        legend = FALSE, points = TRUE, chull = TRUE, chullfilled = TRUE, 
                        axesnames = FALSE, axesvar = FALSE)
layer_points(figure_3b, pch = 16, cex = 0.5)
layer_legend(figure_3b, cex = 1.5)
layer_axesnames(figure_3b, cex = 1.5, name = "LD")
layer_axesvar(figure_3b,cex = 1.5)
layer_grid(figure_3b, col = "#999999", lty = 3, grid = 2)

##########################################################################
########### Identifying statistical difference between knapping areas ####
##########################################################################

#MANOVA AND PAIRWISE MANOVA
pca_BA_slid_nods65 %>% MANOVA(~nodule_Area, retain = 0.99)

pca_BA_slid_nods65 %>% MANOVA_PW(~nodule_Area, retain = 0.99)

##########################################################################
########### FIGURE 8. Elongation and BASI scatterplot ####################
##########################################################################

# Figure 8. Scatterplot of elongation and BASI. Raw material is by shape, site is by colour. Ellipses group by site. 
# Elongation is length/width and BASI as per Hiscock (2014), with 1 = symmetrical and 0 asymmetrical. Created in 
# ggplot2 (Wickham 2016).

data65   <-subset(nods_data, !is.na(nodule_two_or_more)) #RETURNS N=65

figure_8 <- ggplot(data65, aes(x = elongation, y = BASI_matlab, group = SITE_detail)) + 
  geom_point(aes(shape=raw_material, color=SITE_detail, size=7)) +
  scale_color_discrete(labels=c("BCJF1 North", "BCJF2 Centre", "BCJF3 South", "BCJG1 Centre", "BCJG2 South", 
                                "BCJG3 North", "WCL N14", "WCL N4")) +
  scale_shape_manual(values=c(15,16,17), labels=c("chert", "quartz", "silcrete")) +
  ggtitle("Elongation by BASI") +
  xlab("Elongation (length/width)") +
  ylab ("BASI") +
  guides(size="none") +
  guides(shape = guide_legend(override.aes = list(size=5)), colour = guide_legend(override.aes = list(size=5))) +
  labs(color = "Activity Areas", shape="Raw Materials") +
  stat_ellipse(aes(color=SITE_detail), type = "norm", linetype = 1, level = 0.8) +
  theme_minimal() +
  theme(legend.title=element_text(size=12), legend.key.size = unit(1, "cm"), legend.text=element_text(size=11))

plot(figure_8)

# This isn't correct but might be worth looking into - exports plot as an image in desired size
ggsave(way_et_al_figure8,
       filename = file.path(output_folder, "way_et_al_figure8.png"),
       width = 5,
       height = 5)

##########################################################################
########### FIGURE ?. NODULE MEAN SHAPE ##################################
##########################################################################

#draw the mean shape of each nodule
ba65.ms <- MSHAPES(gpa_BA_slid_nods65, ~nodule_two_or_more) # "factor passed was a character, and coerced to a factor."
quartz  <- ba65.ms$nodule_two_or_more$quartz %T>% coo_plot(border="blue")
Out(ba65.ms$shp) %>% panel(names=TRUE)

ba.ms <- MSHAPES(gpa_95_slid, ~nodule_X)
Out(ba.ms$shp) %>% panel(names=TRUE, col = col_qual(32))

panel()

### but see
# mean shape, per group
bot.ms <- mshapes(bot.f, 1)
beer   <- bot.ms$shp$beer    %T>% coo_plot(border="blue")
whisky <- bot.ms$shp$whisky  %T>% coo_draw(border="red")
legend("topright", lwd=1,
       col=c("blue", "red"), legend=c("beer", "whisky"))

### this code from Matzig et al might be key to solving the problem - but also might not

nicolas_colors_without_outliers <- randomcoloR::distinctColorPalette(ba.ms)

Momocs::panel(Momocs::slice(ba65.ms$shp, cluster == paste0("~nodule_two_or_more",i)),
              main = NULL,
              col = palette_qual)

Momocs::panel(Momocs::slice(nicolas_2016_without_outliers_w_cluster_PCA_mean_shapes_cluster_out, 
                            cluster == paste0("cluster_",i)),
              main = NULL,
              col = nicolas_colors_without_outliers[i])


### this also might solve it
mean_shapes_ba65 <- Momocs::Out(ba65.ms,
                                fac = data.frame(cluster = paste0("cluster_", c(1:n_clusters_petrik))))

panel(mean_shapes_petrik_cluster_out, 
      names = T,
      main = "Mean shapes of each UPGMA cluster (Petrík et al. 2018 data as outlines)",
      cex.names = 1.5,
      col = "grey")


lm_chert_rm         <- Momocs::filter(BA_Ldk, raw_material %in% c("CHERT")) # ALL CHERT EXAMPLES
lm_quartz_rm        <- Momocs::filter(BA_Ldk, raw_material %in% c("QUARTZ")) # ALL QUARTZ EXAMPLES
lm_silcrete_rm      <- Momocs::filter(BA_Ldk, raw_material %in% c("SILCRETE")) # ALL SILCRETE EXAMPLES

panel(lm_chert_rm, fac = "nodule_X")

##########################################################################
########### FIGURE ?. Boxplot Experiment #################################
##########################################################################

figure_boxplot <- ggplot(data65, aes(nodule_two_or_more, elongation)) +
  geom_boxplot(aes(fill = SITE_detail)) +
  geom_point(aes(color = SITE_detail)) +
  scale_color_discrete(labels=c("BCJ F1 North", "BCJ F2 Centre", "BCJ F3 South", "BCJ G1 Centre", 
                                "WCL N14", "WCL N4")) +
  ggtitle("Nodule Elongation") +
  xlab("Site & Nodule") +
  ylab ("Elongation (length/width)") +
  labs(color = "Activity Areas") +
  guides(size="none") +
  guides(shape = guide_legend(override.aes = list(size=5))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12))
  

plot(figure_boxplot)