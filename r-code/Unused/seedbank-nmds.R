# ------------------------------------------------------------------------------
# LOAD LIBRARIES ---------------------------------------------------------------
# ------------------------------------------------------------------------------
library(readr)        # read in and output .csv files
library(tidyverse)    # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)         # makes file paths for input and output cleaner
library(vegan)        # runs the NMDS model
library(factoextra)   # makes pca plots from ggplot2

# TUTORIALS
# https://jkzorz.github.io/2019/06/06/NMDS.html
# https://jkzorz.github.io/2020/04/04/NMDS-extras.html
# https://rpubs.com/CPEL/NMDS

# ------------------------------------------------------------------------------
# IMPORT SEEDBANK DATA ---------------------------------------------------------
# ------------------------------------------------------------------------------
seedbank.data <- read.csv(file = here("data", "updatedseedbank-data-sum.csv"))
summary(seedbank.data)
names(seedbank.data)

# ------------------------------------------------------------------------------
# NMDS SEEDBANK DATA -----------------------------------------------------------
# ------------------------------------------------------------------------------

# SET UP NMDS DATA (FILTER SITES OF INTEREST)
nmds.seedbank.data <- seedbank.data %>% 
  #filter(Site %in% c("Colfax", "Greeley", "Howard", "Merrick"))
  # filter(Site %in% c("ENREC", "SCAL"))
  filter(Site == "Greeley")

# GRAB COLUMNS FOR NMDS COMMUNITY/SPECIES
comCols <- names(which(colSums(nmds.seedbank.data[,9:61])>0))
comCols

# CREATE MATRIX FOR NMDS INFORMATION
nmds.com.matrix <- as.matrix(nmds.seedbank.data[,comCols])

# GRAB ENVIRONMENT INFORMAITION
envCols <- c("Site", "Treatment")
nmds.env <- nmds.seedbank.data[,envCols]

# FIT NMDS MODEL
set.seed(56156)
nmds.mod <- metaMDS(nmds.com.matrix, distance = "bray") 
nmds.mod # THE STRESS VALUE IS IMPORTANT. SHOULD BE < 0.2 SO YOU'RE ALRIGHT!

# EVALUATE MODEL FIT
goodness(nmds.mod)
stressplot(nmds.mod)

# EXTRACT THE FIRST TWO NMDS DIMENSIONS (i.e. x and y-axes)
data.scores = as.data.frame(scores(nmds.mod))
data.scores$Site <- nmds.seedbank.data$Site
data.scores$Treatment <- nmds.seedbank.data$Treatment
summary(data.scores)

# EXTRACT SPECIES LOADINGS
species.vectors <- as.data.frame(nmds.mod$species)
summary(species.vectors)

# GRAB ENVIRONMENT INFORMATION
env.mod = envfit(nmds.mod, nmds.env, permutations = 999, na.rm = TRUE)
env.mod

# EXTRACT THE VECTOR COORDINATES
# env.coords.cont <- as.data.frame(scores(env.mod, "vectors"))*ordiArrowMul(env.mod)
# env.coords.cont

env.coord.cat <- as.data.frame(scores(env.mod, "factors"))
env.coord.cat

# PLOT NMDS OUTPUT
nmds.plot <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
                geom_point(aes(color = Treatment), size = 3, alpha = 0.5) + 
                geom_point(data = env.coord.cat, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
                geom_text(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = row.names(env.coord.cat), colour = "navy", fontface = "bold", alpha = 0.8) + 
                geom_segment(data = species.vectors, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), size = 0.1, alpha = 0.1, colour = "grey30") +
                geom_text(data = species.vectors, aes(x = MDS1, y = MDS2), colour = "grey30", label = row.names(species.vectors), size = 3, alpha = 0.5) + 
                theme_bw() +
                theme(aspect.ratio = 1) +
                scale_colour_manual(values = c("orange", "steelblue", "green4")) +
                scale_fill_manual(values = c("orange", "steelblue", "green4"))
nmds.plot
