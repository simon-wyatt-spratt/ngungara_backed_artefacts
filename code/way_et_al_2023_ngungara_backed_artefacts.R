# ARTICLE TITLE:  Investigating hafting and composite tool repair as factors
# creating variability in backed artefacts: Evidence from from Ngungara 
# (Weereewa/Lake George), Southeastern Australia

# AUTHOR: Amy Mosig Way, Loukas Koungoulos, Simon Wyatt-Spratt, Peter Hiscock

# JOURNAL: Archaeology in Oceania

# SCRIPT AUTHOR: Amy Mosig Way, Loukas Koungoulos, Simon Wyatt-Spratt

# SCRIPT CONTACT: amy.way@sydney.edu.au

# ACKNOWLEDGEMENTS: The authors would like to thank the traditional landowners 
# for their  assistance in undertaking the original PhD study and for sharing 
# language names for the lake.

# LAST EDITED: 31/03/2023

# ABSTRACT
# Across the Australian continent, backed artefacts are produced in enormous 
# numbers during the mid-late Holocene. Previous examinations have revealed 
# variation in the average shape of these artefacts, at both continental and 
# regional scales. To better understand the factors creating this variability, 
# we examine a large assemblage of backed artefacts from Ngungara 
# (Weereewa/Lake George), in southeastern Australia. This is one of the few 
# open sites in Australia which has high-resolution evidence for spatially 
# distinct, short-term workshops. Within these well-bounded workshops both 
# locally manufactured and imported backed artefacts are present. However, 
# across this landscape the shape of these artefacts is not uniform; rather 
# similarly shaped backed artefacts are concentrated in different workshop 
# areas. Through the analysis of backed artefacts in different workshops, we 
# suggest that ‘insert copying’ or the replacement of spent inserts with 
# similarly shaped, locally manufactured artefacts creates variability in 
# backed artefact shape.

# SYSTEM INFORMATION
# R version 4.2.2 (2022-10-31)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042.928)

# ATTACHED BASE PACKAGES:
# [1] stats     graphics  grDevices utils     datasets 
# [6] methods   base  

# SCRIPT

# KNOWN ISSUES
# None.

# INSTALL AND ACTIVIATE PACKAGES

if(!require("cowplot")) install.packages('cowplot', 
                                         repos ='http://cran.us.r-project.org') # cowplot 1.1.1
if(!require("ggbeeswarm")) install.packages("ggbeeswarm", 
                                            repos = "http://cran.us.r-project.org") # ggbeeswarm 0.7.1
if(!require("ggpubr")) install.packages("ggpubr", 
                                        repos = "http://cran.us.r-project.org") # ggpubr 0.6.0
if(!require("Momocs")) install.packages("Momocs", 
                                        repos = "http://cran.us.r-project.org") # Momocs 1.4.0
if(!require("tidyverse")) install.packages("tidyverse", 
                                           repos = "http://cran.us.r-project.org") # tidyverse 2.0.0
if(!require("viridis")) install.packages("viridis", 
                                         repos = "http://cran.us.r-project.org") # viridis 0.6.2

library(cowplot)    # load cowplot package
library(ggbeeswarm) # load ggbeeswarm package
library(ggpubr)     # load ggpubr package
library(Momocs)     # load Momocs package
library(tidyverse)  # load tidyverse package
library(viridis)    # load tidyverse package

# GET WORKING DIRECTORY
getwd() # show working directory

# IMPORT DATA
# import *.tps data
ngungara_BA_landmarks <- import_tps("data/ngungara_backed_artefacts.tps")

# import *.csv data
ngungara_BA_data      <- read.csv("data/ngungara_backed_artefacts_93.csv", 
                                  header = TRUE)
BCJG_data             <- read.csv("data/ngungara_backed_artefacts_area_g.csv", 
                                  header = TRUE)
BCJF_data             <- read.csv("data/ngungara_backed_artefacts_area_f7.csv", 
                                  header = TRUE)
# PREPARE DATA
# change factors
ngungara_BA_data$raw_material  <- as.factor(ngungara_BA_data$raw_material)
ngungara_BA_data$activity_area <- as.factor(ngungara_BA_data$activity_area)
ngungara_BA_data$spit          <- factor(ngungara_BA_data$spit, 
                                         levels = c( "10","9","8","7","6","5","4","3","2"))
ngungara_BA_data$activity_area <- as.factor(ngungara_BA_data$activity_area)

# convert our artefact ID column into the row names
ngungara_BA_data <- column_to_rownames(ngungara_BA_data, var = "ID")

# define semi-landmarks and link data
semi.LMs <- matrix(
  c(1,2,3,  2,3,4, 3,4,5,  4,5,6,  5,6,7,  6,7,8,  7,8,9,   9,10,11,  10,11,12,  11,12,13,  12,13,14,
    13,14,15, 14,15,16,  15,16,1),
  ncol = 3, 
  byrow = TRUE)

fw.links <- matrix(
  c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,10, 10,11, 11,12, 12,13, 13,14, 14,15,
    15,16, 16,1),
  ncol = 2,
  byrow = TRUE)

# Momocs link data
BA_Ldk <- Ldk(coo = ngungara_BA_landmarks$coo,
              fac = ngungara_BA_data, 
              links = fw.links, 
              slidings = semi.LMs ) # remove slidings = semi-landmarks for fixed landmarks

# perform GPA with sliding landmarks
gpa_93_slid <- fgsProcrustes(BA_Ldk)

# perform PCA
pca_93_slid <- PCA(gpa_93_slid)

# make data frame of PCs to export
PC_data     <- pca_93_slid %>% as_df(6)

# then write the dataframe into a *.csv file
write.csv(PC_data,"data/PC_data.csv", row.names = FALSE)

ngungara_PC_data <- read.csv("data/PC_data.csv", header = TRUE)

# FIGURE 3
# Figure 3 Plot of locally produced backed and other artefacts 
# with imported backed artefacts and scrapers in Areas 1-4. 
# Complete and broken backed artefacts plotted. Produced with ggplot.

# Figure 3a. Plot of Activity Area 1  

# changes plot order so that flakes are underneath backed artefacts
BCJG_data$Key <- factor(BCJG_data$Key, levels = c(
  "Flake local G4",
  "Core local G4",
  "Backed artefact local G4",
  "Backed artefact imported", 
  "Scraper imported",
  "Excavation boundary"))

figure_3a <- ggplot(BCJG_data %>%
                      arrange(Key),
                    aes(x = X, 
                        y = Y, 
                        colour = Key, 
                        shape = Key, 
                        size = Key,
                        fill = Key)) +
  geom_point() +
  scale_shape_manual(values = c(3,21,24,17,16,15)) +
  scale_fill_manual(values = c(2,8,8,8,1,7)) +
  scale_colour_manual(values = c(8,1,1,1,1,1)) +
  scale_size_manual(values = c(1,3,3,3,3,0.2)) +
  scale_x_continuous(limits = c(10,23)) + 
  scale_y_continuous(limits = c(10,23)) +
  coord_fixed(ratio = 1) +
  xlab("Metres") + 
  ylab("Metres") + 
  theme_bw()+ 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.minor = element_line(colour = "gray88")) +
  stat_ellipse(show.legend = FALSE, 
               inherit.aes = FALSE,
               aes(x = X, y = Y, color = activity_area),
               color = 1,
               linetype = 2,
               lwd = 0.8) +
  geom_text(x = 21, y = 22.5, 
            label = "Area 1", 
            show.legend = FALSE, 
            size = 4)

figure_3a

ggsave2("results/Way_et_al_2023_Figure_3a.png", 
        plot = figure_3a, 
        scale = 1.0, 
        width = 202, 
        height = 132, 
        units = "mm", 
        dpi = 300)

# Figure 3b. Plot of Activity Areas 2-4

# subset to remove all 'NAs' from column 'Key'
subF7 <- BCJF_data[BCJF_data$Key != "NA", ] 

# change plot order so flakes are underneath backed artefacts
subF7$Key <- factor(subF7$Key, levels = c(
  "Flake local F13",
  "Flake local F24",
  "Flake local F26",
  "Flake local F28",
  "Flake local F30",
  "Flake local quartz",
  "Core local quartz",
  "Core local F24",
  "Backed artefact local F13",
  "Backed artefact local F24",
  "Backed artefact local F26",
  "Backed artefact local F28",
  "Backed artefact local F30",
  "Backed artefact local quartz",
  "Backed artefact imported", 
  "Scraper imported",
  "anvil",
  "Excavation boundary"))

figure_3b <- ggplot(subF7 %>%
                      arrange(Key),
                    aes(x = X, 
                        y = Y, 
                        colour=Key, 
                        shape=Key, 
                        size=Key,
                        fill=Key)) +
  geom_point() +
  scale_shape_manual(values = c(3,3,3,3,3,3,21,21,24,24,24,24,24,24,17,16,8,15)) +
  scale_fill_manual(values = c(2,3,4,5,6,7,7,3,2,3,4,5,6,7,1,1,1,1)) +
  scale_colour_manual(values = c(2,3,4,5,6,7,1,1,1,1,1,1,1,1,1,1,1,1,1,1)) +
  scale_size_manual(values = c(1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,0.1)) +
  scale_x_continuous(limits = c(10,23)) + 
  scale_y_continuous(limits = c(10,36)) +
  coord_fixed(ratio = 1) +
  xlab("Metres") + 
  ylab("Metres") + 
  theme_bw()

figure_3b

ggsave2("results/Way_et_al_2023_Figure_3b.png", 
        plot = figure_3b, 
        scale = 1.0, 
        width = 132, 
        height = 132, 
        units = "mm", 
        dpi = 300)

# FIGURE 5.
# Figure 5 a. Principal Component plot of 93 backed artefacts by Activity Area, 
# b. Linear Discriminant plot. Figure produced with the momocs package.

# Figure 5a. PC plot of Activity Area

png(file = "results/Way_et_al_2023_Figure_5a.png", 
    width = 168.275, 
    height = 168.275, 
    units = "mm", 
    res = 300)

figure_5a <- plot_PCA(pca_93_slid, ~activity_area,
                      palette = pal_seq,
                      morphospace = FALSE, 
                      legend = FALSE, 
                      points = TRUE, 
                      chull = TRUE, 
                      chullfilled = TRUE, 
                      axesnames = FALSE,  
                      axesvar = FALSE)
layer_points(figure_5a, pch = 16, cex = 0.5)
#layer_legend(figure_4a, cex = 1.5)
layer_axesnames(figure_5a, cex = 1.5, name = "PC")
layer_axesvar(figure_5a,cex = 1.5)
layer_grid(figure_5a, col = "#999999", lty = 3, grid = 2)
#layer_labelgroups(figure_4a, cex = 1, rect = FALSE, alpha = 1)
layer_morphospace_PCA(figure_5a, position = c("range")[1], nb = 12, nr = 3,
                      nc = 6, rotate = 0, size = 0.6, col = "black",
                      flipx = FALSE, flipy = FALSE, draw = TRUE)

dev.off()

# Figure 5b. LDA plot of Activity Area

png(file = "results/Way_et_al_2023_Figure_5b.png", 
    width = 168.275, 
    height = 168.275, 
    units = "mm", 
    res = 300)

BA.lda_slid <- LDA(gpa_93_slid, ~ activity_area)
plot_LDA(BA.lda_slid, 
         chullfilled = TRUE, 
         zoom = 1, 
         box = TRUE, 
         points = TRUE)
figure_5b <- plot_LDA(BA.lda_slid,
                      palette = pal_seq,
                      legend = FALSE, 
                      points= TRUE, chull = TRUE,
                      chullfilled = TRUE, 
                      axesnames = FALSE, axesvar = FALSE)
layer_points(figure_5b, pch = 16, cex = 0.5)
layer_legend(figure_5b, cex = 1.5)
layer_axesnames(figure_5b, cex = 1.5, name = "LD")
layer_axesvar(figure_5b,cex = 1.5)
layer_grid(figure_5b, col = "#999999", lty = 3, grid = 2)
#layer_labelgroups(figure_4b, cex = 1, rect = FALSE, alpha = 1)

dev.off()

# FIGURE 6.
# Boxplot of PC1 by mode of entry in each Activity Area. 
# Figure produced with ggplot.Figure produced with the ggplot package.

figure_6 <- ggplot(ngungara_PC_data, 
                   aes(x = activity_area, 
                       y = PC1, 
                       color = mode_of_entry)) +
  geom_boxplot(aes(color = mode_of_entry)) +
  ggbeeswarm::geom_quasirandom(width = 0.3, 
                               varwidth = TRUE, 
                               alpha = 0.2) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Activity Area", y = "PC1", colour = "Mode of Entry")

figure_6

ggsave2("results/Way_et_al_2023_Figure_6.png", 
        plot = figure_6, 
        scale = 1.0, 
        width = 168.275, 
        height = 84.1375, 
        units = "mm", 
        dpi = 300)

# FIGURE 7
# Figure 7 PCA plot of raw material. Figure produced with the momocs package.

png(file = "results/Way_et_al_2023_Figure_7a.png", 
    width = 168.275, 
    height = 168.275, 
    units = "mm", 
    res = 300)

figure_7 <-plot_PCA(pca_93_slid, ~raw_material,
                     palette = pal_seq,
                     morphospace = FALSE, 
                     legend = FALSE, 
                     points = TRUE, 
                     chull = TRUE, 
                     chullfilled = TRUE, 
                     axesnames = FALSE,  
                     axesvar = FALSE)
layer_points(figure_7, pch = 16, cex = 0.5)
layer_legend(figure_7, cex = 1.5)
layer_axesnames(figure_7, cex = 1.5, name = "PC")
layer_axesvar(figure_7,cex = 1.5)
layer_grid(figure_7, col = "#999999", lty = 3, grid = 2)
layer_morphospace_PCA(figure_7,
                      position = c("range")[1],
                      nb = 12,
                      nr = 3,
                      nc = 6,
                      rotate = 0,
                      size = 0.6,
                      col = "black",
                      flipx = FALSE,
                      flipy = FALSE,
                      draw = TRUE)

dev.off()