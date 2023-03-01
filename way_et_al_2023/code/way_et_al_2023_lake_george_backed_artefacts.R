# ARTICLE TITLE:  Is re-hafting underlying the standardisation of backed 
# artefacts?  An example from Weereewaa/Lake Ngungara (Lake George), 
# Southeastern Australia

# AUTHOR: Amy Mosig Way, Loukas George Koungoulos, Simon Wyatt-Spratt, 
# Peter Hiscock, Ngambri Local Aboriginal Land Council

# JOURNAL: Archaeology in Oceania

# SCRIPT AUTHOR: Amy Mosig Way, Loukas George Koungoulos, Simon Wyatt-Spratt

# SCRIPT CONTACT: amy.way@sydney.edu.au

# LAST EDITED: 01/03/2023 

# ABSTRACT
# The identification of standardised tools in the archaeological record is one
# of the key methods by which archaeologists recognise behavioural patterns in
# the past. In Australia, one of the most preeminent standardised tools is the
# mid-late Holocene backed artefact. To better understand this 
# standardisation, we examine a large assemblage of backed artefacts from
# Weereewaa/Lake Ngungara (Lake George), in Southeastern Australia, which has 
# both locally manufactured and imported backed artefacts. These local and 
# imported artefacts are found side-by-side within well-bounded stone artefact 
# concentrations. Across this landscape two shapes are present: backed points 
# and geometric artefacts. Their distribution is not uniform; rather similarly 
# shaped local and imported backed artefacts are concentrated in defined areas.
# We test whether this pattern results from the removal of spent inserts from 
# hafts and their replacement by similarly shaped, locally manufactured 
# artefacts. We also test whether raw material has a structuring influence on 
# the shape of these artefacts.

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
LGBA_93_lm        <- import_tps("data/lake_george_backed_artefacts.tps")

# import *.csv data
LGBA_93_data      <- read.csv("data/lake_george_backed_artefacts_93.csv", 
                              header = TRUE)
BCJG_data         <- read.csv("data/lake_george_backed_artefacts_area_g.csv", 
                              header = TRUE)
BCJF_data         <- read.csv("data/lake_george_backed_artefacts_area_f7.csv", 
                              header = TRUE)

# change factors
LGBA_93_data$raw_material  <- as.factor(LGBA_93_data$raw_material)
LGBA_93_data$activity_area <- as.factor(LGBA_93_data$activity_area)
LGBA_93_data$spit          <- factor(LGBA_93_data$spit, 
                              levels = c( "10","9","8","7","6","5","4","3","2"))
LGBA_93_data$activity_area <- as.factor(LGBA_93_data$activity_area)

# convert our artefact ID column into the row names
LGBA_93_data <- column_to_rownames(LGBA_93_data, var = "ID")

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
BA_Ldk <- Ldk(coo = LGBA_93_lm$coo,fac = LGBA_93_data, 
              links= fw.links, slidings = semi.LMs ) # remove slidings = semi-landmarks for fixed landmarks

# perform GPA with sliding landmarks
gpa_93_slid <- fgsProcrustes(BA_Ldk)

# perform PCA
pca_93_slid <- PCA(gpa_93_slid)

# FIGURE 2.
# Plot of locally produced backed and other artefacts with imported backed 
# artefacts and scrapers in Areas 1-4. Complete and broken backed artefacts 
# plotted.

# Figure 2a. Plot of Activity Area 1  

# change plot order so flakes are underneath backed artefacts
BCJG_data$Key <- factor(BCJG_data$Key, levels = c(
  "Flake local G4",
  "Core local G4",
  "Backed artefact local G4",
  "Backed artefact imported", 
  "Scraper imported",
  "Excavation boundary"))

figure_2a <- ggplot(BCJG_data %>%
                      arrange(Key),
                    aes(x = X, 
                        y = Y, 
                        colour = Key, 
                        shape = Key, 
                        size = Key,
                        fill = Key)) +
  geom_point() +
  scale_shape_manual(values = c(3,21,24,17,16,20)) +
  scale_fill_manual(values = c(2,8,8,8,1,7)) +
  scale_colour_manual(values = c(8,1,1,1,1,1)) +
  scale_size_manual(values = c(1,3,3,3,3,1)) +
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

figure_2a

ggsave2("results/Way_et_al_2023_Figure_2a.png", 
        plot = figure_2a, 
        scale = 1.0, 
        width = 202, 
        height = 132, 
        units = "mm", 
        dpi = 300)

# Figure 2b. Plot of Activity Areas 2-4

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
  "Excavation boundary"))

figure_2b <- ggplot(subF7 %>%
                      arrange(Key),
                    aes(x = X, 
                        y = Y, 
                        colour=Key, 
                        shape=Key, 
                        size=Key,
                        fill=Key)) +
  geom_point() +
  scale_shape_manual(values = c(3,3,3,3,3,3,21,21,24,24,24,24,24,24,17,16,20)) +
  scale_fill_manual(values = c(2,3,4,5,6,7,7,3,2,3,4,5,6,7,1,1,1)) +
  scale_colour_manual(values = c(2,3,4,5,6,7,1,1,1,1,1,1,1,1,1,1,1,1,1)) +
  scale_size_manual(values = c(1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,1)) +
  scale_x_continuous(limits = c(10,23)) + 
  scale_y_continuous(limits = c(10,36)) +
  coord_fixed(ratio = 1) +
  xlab("Metres") + 
  ylab("Metres") + 
  theme_bw()

figure_2b

ggsave2("results/Way_et_al_2023_Figure_2b.png", 
        plot = figure_2b, 
        scale = 1.0, 
        width = 132, 
        height = 132, 
        units = "mm", 
        dpi = 300)

# FIGURE 4.
# 4a. Principal Component plot of 95 backed artefacts by Activity Area, 4b. 
# Linear Discriminant plot. Figure produced with the Momocs package.

# Figure 4a. PC plot of Activity Area

png(file = "results/Way_et_al_2023_Figure_4a.png", 
    width = 168.275, 
    height = 168.275, 
    units = "mm", 
    res = 300)

figure_4a <- plot_PCA(pca_93_slid, ~activity_area,
                     palette = pal_seq,
                     morphospace = FALSE, 
                     legend = FALSE, 
                     points = TRUE, 
                     chull = TRUE, 
                     chullfilled = TRUE, 
                     axesnames = FALSE,  
                     axesvar = FALSE)
layer_points(figure_4a, pch = 16, cex = 0.5)
#layer_legend(figure_4a, cex = 1.5)
layer_axesnames(figure_4a, cex = 1.5, name = "PC")
layer_axesvar(figure_4a,cex = 1.5)
layer_grid(figure_4a, col = "#999999", lty = 3, grid = 2)
#layer_labelgroups(figure_4a, cex = 1, rect = FALSE, alpha = 1)
layer_morphospace_PCA(figure_4a, position = c("range")[1], nb = 12, nr = 3,
                      nc = 6, rotate = 0, size = 0.6, col = "black",
                      flipx = FALSE, flipy = FALSE, draw = TRUE)

dev.off()

# Figure 4b. LDA plot of Activity Area

png(file = "results/Way_et_al_2023_Figure_4b.png", 
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
figure_4b <- plot_LDA(BA.lda_slid,
                     palette = pal_seq,
                     legend = FALSE, 
                     points= TRUE, chull = TRUE,
                     chullfilled = TRUE, 
                     axesnames = FALSE, axesvar = FALSE)
layer_points(figure_4b, pch = 16, cex = 0.5)
layer_legend(figure_4b, cex = 1.5)
layer_axesnames(figure_4b, cex = 1.5, name = "LD")
layer_axesvar(figure_4b,cex = 1.5)
layer_grid(figure_4b, col = "#999999", lty = 3, grid = 2)
#layer_labelgroups(figure_4b, cex = 1, rect = FALSE, alpha = 1)

dev.off()

# FIGURE 5.
# Boxplot of elongation of backed artefacts by mode of import in each Activity 
# Area. CR-BA = locally produced backed artefact, Imported BA = imported backed
# artefact. Geometric microliths have an elongation index lower than 2, and 
# points have an elongation index greater than 2. Figure produced with the 
# ggplot package.

figure_5 <- ggplot(LGBA_93_data, 
                   aes(x = activity_area, 
                       y = elongation, 
                       color = mode_of_entry)) +
  geom_boxplot(aes(color = mode_of_entry)) +
  ggbeeswarm::geom_quasirandom(width = 0.3, varwidth = TRUE, alpha = 0.2) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Activity Area", y = "Elongation (length/width)", colour = "Mode of Entry")

figure_5

ggsave2("results/Way_et_al_2023_Figure_5.png", 
        plot = figure_5, 
        scale = 1.0, 
        width = 168.275, 
        height = 84.1375, 
        units = "mm", 
        dpi = 300)

# FIGURE 6.
# 6a. PCA plot of raw material, 6b. scatterplot of elongation by spit, 
# coloured by raw material. Figure produced with the Momocs package.

# Figure 6a. PCA plot of raw material.

png(file = "results/Way_et_al_2023_Figure_6a.png", 
    width = 168.275, 
    height = 168.275, 
    units = "mm", 
    res = 300)

figure_6a <-plot_PCA(pca_93_slid, ~raw_material,
                     palette = pal_seq,
                     morphospace = FALSE, 
                     legend = FALSE, 
                     points = TRUE, 
                     chull = TRUE, 
                     chullfilled = TRUE, 
                     axesnames = FALSE,  
                     axesvar = FALSE)
layer_points(figure_6a, pch = 16, cex = 0.5)
#layer_legend(figure_6a, cex = 1.5)
layer_axesnames(figure_6a, cex = 1.5, name = "PC")
layer_axesvar(figure_6a,cex = 1.5)
layer_grid(figure_6a, col = "#999999", lty = 3, grid = 2)
layer_morphospace_PCA(figure_6a,
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

# Figure 6b. Scatterplot of elongation by spit

figure_6b <-ggplot(LGBA_93_data, aes(x = elongation, y = spit, 
                                     group = raw_material)) + 
  geom_point(aes(color = raw_material, size = 4, alpha = 0.9)) +
  scale_colour_viridis_d() +
  stat_ellipse(type = "norm", linetype = 2, level = 0.8) +
  theme_minimal() +
  labs(x = "Elongation (length/width)", y = "Spit", color = "Raw Material") +
  guides(size = "none", shape = "none", alpha = "none", 
         color = guide_legend(override.aes = list(size = 4)))

figure_6b

ggsave2("results/Way_et_al_2023_Figure_6b.png", 
        plot = figure_6b, 
        scale = 1.0, 
        width = 200, 
        height = 168.275, 
        units = "mm", 
        dpi = 300)

# FIGURE 7. 
# PCA and LDA plots of raw material by presence/absence of quartz

# Figure 7a. PCA plot

png(file = "results/Way_et_al_2023_Figure_7a.png", 
    width = 168.275, 
    height = 168.275, 
    units = "mm", 
    res = 300)

figure_7a <-plot_PCA(pca_93_slid, ~Qu_present, palette = pal_seq,
                     morphospace = FALSE, legend = FALSE, 
                     points = TRUE, chull = TRUE, chullfilled = TRUE, 
                     axesnames = FALSE, axesvar = FALSE)
layer_points(figure_7a, pch = 16, cex = 0.5)
#layer_legend(figure_7a, cex = 1.5)
layer_axesnames(figure_7a, cex = 1.5, name = "PC")
layer_axesvar(figure_7a, cex = 1.5)
layer_grid(figure_7a, col = "#999999", lty = 3, grid = 2)
layer_morphospace_PCA(figure_7a,
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

# Figure 7b. LDA plot quartz present

png(file = "results/Way_et_al_2023_Figure_7b.png", 
    width = 168.275, 
    height = 168.275, 
    units = "mm", 
    res = 300)

BA.lda_slid<-LDA(gpa_93_slid, ~ Qu_present)
figure_7b <-plot_LDA(BA.lda_slid, palette = pal_seq, legend = FALSE, 
                     points = TRUE, chull = TRUE, 
            chullfilled = TRUE, axesnames = FALSE,  axesvar = FALSE)
layer_points(figure_7b, pch = 16, cex = 0.5)
layer_legend(figure_7b, cex = 1)
layer_axesnames(figure_7b, cex = 1.5, name = "LD")
layer_axesvar(figure_7b,cex = 1.5)
layer_grid(figure_7b, col = "#999999", lty = 3, grid = 2)

dev.off()