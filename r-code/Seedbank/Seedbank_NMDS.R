#NMDS Analysis of Seedbank Data
## May need to consider PCA or PCoA because stress value for ENREC is too small (dataset likely not large enough because stress is 0)


# LOAD LIBRARIES ---------------------------------------------------------------
library(readr)        # read in and output .csv files
library(lattice)
library(tidyverse)    # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)         # makes file paths for input and output cleaner
library(vegan)        # runs the NMDS model
library(factoextra)   # makes pca plots from ggplot2
library(cowplot)

# TUTORIALS
# https://jkzorz.github.io/2019/06/06/NMDS.html
# https://jkzorz.github.io/2020/04/04/NMDS-extras.html
# https://rpubs.com/CPEL/NMDS

# IMPORT SEEDBANK DATA ---------------------------------------------------------
seedbank.data <- read.csv(file = here("data", "FINAL_DATA", "updatedseedbank-data-sum.csv"))
summary(seedbank.data)
names(seedbank.data)



# Removal of rare species making up less than 0.01% of the seedbank -------
filtered_seedbank <- seedbank.data %>% 
  group_by(Site) %>% 
  #filter(Experiment %in% c(expType)) %>%
  summarize(across(AMATU:SIDSP, sum)) %>%
  mutate(Total = rowSums(select(., AMATU:SIDSP)), .before = AMATU) %>% 
  pivot_longer(cols = c("AMATU":"SIDSP"),
               names_to = "Species",
               values_to = "Count") %>%
  filter(Count > 0) %>% 
  mutate(Pct = Count/Total) %>%
  filter(Pct > 0.001)

filtered_seedbank
#This is my reference list for manually adding in species that make up 0.1% or greater of each sites respective seedbank.

  


  

#Listed below are EACH sites individual NMDS
#If you need to rerun any part of code for each individual site, you must start from the top down until the nmds graph is named (didn't change names when copy/pasting to next site)

# Old Trial (ignore) -----------------------------------------------------------

# # SET UP NMDS DATA (FILTER SITES OF INTEREST)
# nmds.seedbank.data <- seedbank.data %>% 
# filter(Site %in% c("Colfax"))
# 
# 
# 
# 
# #filter(Treatment != "Vetch")
# #filter(Site %in% c("Colfax", "Greeley", "Howard", "Merrick"))
# # filter(Site %in% c("ENREC", "SCAL"))
# # filter(Site %in% c("ENREC"))
# 
# comCols <- names(which(colSums(nmds.seedbank.data[,9:66])>0))
# comCols
# 
# 
# # GRAB COLUMNS FOR NMDS COMMUNITY/SPECIES
# comCols <- names(which(colSums(nmds.seedbank.data[,c("AMATU","SOLRO","ERICA","SCHAC","OXAST")])>0))
# comCols
# 
# # CREATE MATRIX FOR NMDS INFORMATION
# nmds.com.matrix <- as.matrix(nmds.seedbank.data[,comCols])
# 
# # GRAB ENVIRONMENT INFORMAITION
# envCols <- c("Site", "Treatment")
# nmds.env <- nmds.seedbank.data[,envCols]
# 
# # FIT NMDS MODEL
# set.seed(56156)
# nmds.mod <- metaMDS(nmds.com.matrix, distance = "bray")
# nmds.mod # THE STRESS VALUE IS IMPORTANT. SHOULD BE < 0.2 SO YOU'RE ALRIGHT!
# 
# 
# # EVALUATE MODEL FIT
# goodness(nmds.mod)
# stressplot(nmds.mod)
# 
# # EXTRACT THE FIRST TWO NMDS DIMENSIONS (i.e. x and y-axes)
# data.scores = as.data.frame(scores(nmds.mod))
# data.scores$Site <- nmds.seedbank.data$Site
# data.scores$Treatment <- nmds.seedbank.data$Treatment
# summary(data.scores)
# 
# # EXTRACT SPECIES LOADINGS
# species.vectors <- as.data.frame(nmds.mod$species)
# summary(species.vectors)
# 
# # GRAB ENVIRONMENT INFORMATION
# env.mod = envfit(nmds.mod, nmds.env, permutations = 999, na.rm = TRUE)
# env.mod
# 
# # EXTRACT THE VECTOR COORDINATES
# # env.coords.cont <- as.data.frame(scores(env.mod, "vectors"))*ordiArrowMul(env.mod)
# # env.coords.cont
# 
# env.coord.cat <- as.data.frame(scores(env.mod, "factors"))
# env.coord.cat
# 
# #make the hulls that points will connect to 
# site_hull <- 
#   data.scores %>% # dataframe of site scores
#   unite("site_trt", Site, Treatment, remove = FALSE) %>%
#   group_by(site_trt) %>% # grouping variables: farm AND treatmnet
#   slice(chull(NMDS1, NMDS2)) # points that polygons will connect
# 
# 
# #install.packages('ggrepel')
# library(ggrepel)
# 
# # PLOT NMDS OUTPUT
# nmds.plot <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
#   geom_point(aes(color = Treatment), size = 3, alpha = 0.5) + 
#   geom_point(data = env.coord.cat, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 4, alpha = 0.6, colour = "#335c67") +
#   geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = row.names(env.coord.cat), colour = "black", fontface = "bold", alpha = 0.8) + 
#   geom_segment(data = species.vectors, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), size = 0.1, alpha = 0.1, colour = "grey30") +
#   geom_text_repel(data = species.vectors, aes(x = MDS1, y = MDS2), colour = "grey30", label = row.names(species.vectors), size = 3, alpha = 0.5) + 
#   theme_bw() +
#   geom_polygon(data = site_hull, 
#                aes(x = NMDS1, 
#                    y = NMDS2, 
#                    fill = Treatment),
#                alpha = 0.3)+
#   theme(aspect.ratio = 1) +
#   scale_colour_manual(values = c("#f48c06", "#d00000", "green4","gray")) +
#   scale_fill_manual(values = c("#f48c06", "#d00000", "green4","gray"))
# nmds.plot


# COLFAX ------------------------------------------------------------------
# SET UP NMDS DATA (FILTER SITES OF INTEREST)
nmds.seedbank.data <- seedbank.data %>% 
  filter(Site %in% c("Colfax"))
summary(nmds.seedbank.data)
# comCols <- names(which(colSums(nmds.seedbank.data[,9:66])>0))
# comCols


# GRAB COLUMNS FOR NMDS COMMUNITY/SPECIES (these are species making up > 0.01% of the site's seedbank)
comCols <- names(which(colSums(nmds.seedbank.data[,c("AMATU","SOLRO","ERICA","SCHAC","OXAST","MOLVE")])>0))
comCols

# CREATE MATRIX FOR NMDS INFORMATION
nmds.com.matrix <- as.matrix(nmds.seedbank.data[,comCols])

# GRAB ENVIRONMENT INFORMAITION
envCols <- c("Site", "Treatment")
nmds.env <- nmds.seedbank.data[,envCols]


# FIT NMDS MODEL
set.seed(56156)
nmds.mod <- metaMDS(nmds.com.matrix, distance = "bray", try = 100)
nmds.mod 
#STRESS = 0.03

# EVALUATE MODEL FIT
# goodness(nmds.mod)
# stressplot(nmds.mod)

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

#make the hulls that points will connect to 
site_hull <- 
  data.scores %>% # dataframe of site scores
  unite("site_trt", Site, Treatment, remove = FALSE) %>%
  group_by(site_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


#install.packages('ggrepel')
library(ggrepel)

# PLOT NMDS OUTPUT
colfax.nmds <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = Treatment), size = 3, alpha = 0.5) + 
  #geom_point(data = env.coord.cat, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 4, alpha = 0.6, colour = "#335c67") +
  geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = NA)+
  # geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = row.names(env.coord.cat), colour = "black", fontface = "bold", alpha = 0.8) + 
  
  geom_segment(data = species.vectors, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), size = 0.1, alpha = 0.1, colour = "grey30") +
  geom_text_repel(data = species.vectors, aes(x = MDS1, y = MDS2), colour = "grey30", label = row.names(species.vectors), size = 3, alpha = 0.5) + 
  theme_bw() +
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = Treatment),
               alpha = 0.3)+
  theme(aspect.ratio = 1) +
  labs(title = "Colfax")+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  scale_colour_manual(values = c("darkgray", "#9d0208", "green4","gray")) +
  scale_fill_manual(values = c("darkgray", "#9d0208", "green4","gray"))+
  #annotate("text", x = 0.75, y = 0.75, label = "Stress = 0.03")+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())

colfax.nmds




# GREELEY -----------------------------------------------------------------

# SET UP NMDS DATA (FILTER SITES OF INTEREST)
nmds.seedbank.data <- seedbank.data %>% 
  filter(Site %in% c("Greeley"))

# GRAB COLUMNS FOR NMDS COMMUNITY/SPECIES
comCols <- names(which(colSums(nmds.seedbank.data[,c("AMATU","SOLRO","POROL","SINAR","ERASP","SOLPH","MELOF","CHANU","VERST","SECCE","CERFO","POAAN", "ECHCR","MEDLU","SOLPT","ERICA","MOLVE","OXAST","DIGSA","AMARE","CHEAL","SETVI", "EUPMA","AEGCY","THLAR","AMAPA","SETPU","DIGIS","ERACIL","VERBR")])>-1))

comCols

# CREATE MATRIX FOR NMDS INFORMATION
nmds.com.matrix <- as.matrix(nmds.seedbank.data[,comCols])

# GRAB ENVIRONMENT INFORMAITION
envCols <- c("Site", "Treatment")
nmds.env <- nmds.seedbank.data[,envCols]

# FIT NMDS MODEL
set.seed(56156)
nmds.mod <- metaMDS(nmds.com.matrix, distance = "bray")
nmds.mod #STRESS = 0.09


# EVALUATE MODEL FIT
# goodness(nmds.mod)
# stressplot(nmds.mod)

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

#make the hulls that points will connect to 
site_hull <- 
  data.scores %>% # dataframe of site scores
  unite("site_trt", Site, Treatment, remove = FALSE) %>%
  group_by(site_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


#install.packages('ggrepel')
library(ggrepel)

# PLOT NMDS OUTPUT
greeley.nmds <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = Treatment), size = 3, alpha = 0.5) + 
  #geom_point(data = env.coord.cat, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 4, alpha = 0.6, colour = "#335c67") +
  geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = NA)+
  #geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = row.names(env.coord.cat), colour = "black", fontface = "bold", alpha = 0.8) + 
  geom_segment(data = species.vectors, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), size = 0.1, alpha = 0.1, colour = "grey30") +
  geom_text_repel(data = species.vectors, aes(x = MDS1, y = MDS2), colour = "grey30", label = row.names(species.vectors), size = 3, alpha = 0.5) + 
  theme_bw() +
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = Treatment),
               alpha = 0.3)+
  theme(aspect.ratio = 1) +
  labs(title = "Greeley")+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  
  scale_colour_manual(values = c("darkgray", "#9d0208", "green4","gray")) +
  scale_fill_manual(values = c("darkgray", "#9d0208", "green4","gray"))+
  #annotate("text", x = 0.8, y = 0.7, label = "Stress = 0.09")+
  theme(legend.position = "none")+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())

greeley.nmds




# HOWARD ------------------------------------------------------------------

# SET UP NMDS DATA (FILTER SITES OF INTEREST)
nmds.seedbank.data <- seedbank.data %>% 
  filter(Site %in% c("Howard"))


# GRAB COLUMNS FOR NMDS COMMUNITY/SPECIES
comCols <- names(which(colSums(nmds.seedbank.data[,c("AMATU","ERICA","MOLVE","OXAST","AMARE","CHEAL","SETVI","AMAPA","SETPU","VERBR","SETFA","AMAAL","THLAR","ABUTH","CAPBU")])>-1))


comCols <- names(which(colSums(nmds.seedbank.data[,c("AMATU","ERICA","SCHAC","MEDLU","SOLPT","STELME","CYPES","ANAAR","VERPE","MOLVE","OXAST","AMARE","CHEAL","SETVI","AMAPA","SETPU","VERBR","SETFA","AMAAL","THLAR","ABUTH","CAPBU")])>-1))

comCols

# CREATE MATRIX FOR NMDS INFORMATION
nmds.com.matrix <- as.matrix(nmds.seedbank.data[,comCols])

# GRAB ENVIRONMENT INFORMAITION
envCols <- c("Site", "Treatment")
nmds.env <- nmds.seedbank.data[,envCols]

# FIT NMDS MODEL
set.seed(56156)
nmds.mod <- metaMDS(nmds.com.matrix, distance = "bray")
nmds.mod #stress = 0.046 or 0.05


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

#make the hulls that points will connect to 
site_hull <- 
  data.scores %>% # dataframe of site scores
  unite("site_trt", Site, Treatment, remove = FALSE) %>%
  group_by(site_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


#install.packages('ggrepel')
library(ggrepel)

# PLOT NMDS OUTPUT
howard.nmds <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = Treatment), size = 3, alpha = 0.5) + 
  #geom_point(data = env.coord.cat, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 4, alpha = 0.6, colour = "#335c67") +
  #geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = row.names(env.coord.cat), colour = "black", fontface = "bold", alpha = 0.8) + 
  geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = NA)+
  geom_segment(data = species.vectors, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), size = 0.1, alpha = 0.1, colour = "grey30") +
  geom_text_repel(data = species.vectors, aes(x = MDS1, y = MDS2), colour = "grey30", label = row.names(species.vectors), size = 3, alpha = 0.5) + 
  theme_bw() +
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = Treatment),
               alpha = 0.3)+
  theme(aspect.ratio = 1) +
  labs(title = "Howard")+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  scale_colour_manual(values = c("darkgray", "#9d0208", "green4","gray")) +
  scale_fill_manual(values = c("darkgray", "#9d0208", "green4","gray"))+
  #annotate("text", x = 0.7, y = 0.8, label = "Stress = 0.05")+
  theme(legend.position = "none")

howard.nmds





# MERRICK -----------------------------------------------------------------

# SET UP NMDS DATA (FILTER SITES OF INTEREST)
nmds.seedbank.data <- seedbank.data %>% 
  filter(Site %in% c("Merrick"))

# GRAB COLUMNS FOR NMDS COMMUNITY/SPECIES
comCols <- names(which(colSums(nmds.seedbank.data[,c("MOLVE","SCHAC","CHEAL","SETPU","FAGES","AMARE","SETVI","AMAPA","ECHCR","SOLPT","DIGSA","ANAAR","LINDU","AMATU","CYPES")])>-1))
comCols

# CREATE MATRIX FOR NMDS INFORMATION
nmds.com.matrix <- as.matrix(nmds.seedbank.data[,comCols])

# GRAB ENVIRONMENT INFORMAITION
envCols <- c("Site", "Treatment")
nmds.env <- nmds.seedbank.data[,envCols]

# FIT NMDS MODEL
set.seed(56156)
nmds.mod <- metaMDS(nmds.com.matrix, distance = "bray")
nmds.mod 
#STRESS = 0.03

# EVALUATE MODEL FIT
# goodness(nmds.mod)
# stressplot(nmds.mod)

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

#make the hulls that points will connect to 
site_hull <- 
  data.scores %>% # dataframe of site scores
  unite("site_trt", Site, Treatment, remove = FALSE) %>%
  group_by(site_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


#install.packages('ggrepel')
library(ggrepel)

# PLOT NMDS OUTPUT
merrick.nmds <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = Treatment), size = 3, alpha = 0.5) + 
  # geom_point(data = env.coord.cat, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 4, alpha = 0.6, colour = "#335c67") +
  # geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = row.names(env.coord.cat), colour = "black", fontface = "bold", alpha = 0.8) + 
  geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = NA)+
  geom_segment(data = species.vectors, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), size = 0.1, alpha = 0.1, colour = "grey30") +
  geom_text_repel(data = species.vectors, aes(x = MDS1, y = MDS2), colour = "grey30", label = row.names(species.vectors), size = 3, alpha = 0.5) + 
  theme_bw() +
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = Treatment),
               alpha = 0.3)+
  theme(aspect.ratio = 1) +
  labs(title = "Merrick")+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  
  scale_colour_manual(values = c("darkgray", "#9d0208", "green4","gray")) +
  scale_fill_manual(values = c("darkgray", "#9d0208", "green4","gray"))+
  #annotate("text", x = 0.95, y = 0.85, label = "Stress = 0.03")+
  theme(axis.title.y = element_blank())


merrick.nmds




# ENREC -------------------------------------------------------------------

# SET UP NMDS DATA (FILTER SITES OF INTEREST)
nmds.seedbank.data <- seedbank.data %>% 
  filter(Site %in% c("ENREC")) %>% 
  filter(Treatment != "VetchDNI") #VetchDNI is Vetch do not include...filtered it out.


# GRAB COLUMNS FOR NMDS COMMUNITY/SPECIES
comCols <- names(which(colSums(nmds.seedbank.data[,c("AMATU","ERICA","MOLVE","AMARE","ABUTH","CHEAL","EUPMA","POROL","THLAR","SETVI","SCHAC")])>-1))
comCols

# CREATE MATRIX FOR NMDS INFORMATION
nmds.com.matrix <- as.matrix(nmds.seedbank.data[,comCols])
nmds.com.matrix


# GRAB ENVIRONMENT INFORMAITION
envCols <- c("Site", "Treatment")
nmds.env <- nmds.seedbank.data[,envCols]

# FIT NMDS MODEL
set.seed(56156)
nmds.mod <- metaMDS(nmds.com.matrix, distance = "bray")
nmds.mod 
#STRESS = 0, DATASET NOT LARGE ENOUGH - this is where the issue is!!!!


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

#make the hulls that points will connect to 
site_hull <- 
  data.scores %>% # dataframe of site scores
  unite("site_trt", Site, Treatment, remove = FALSE) %>%
  group_by(site_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


#install.packages('ggrepel')
library(ggrepel)

# PLOT NMDS OUTPUT
enrec.nmds <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = Treatment), size = 3, alpha = 0.5) + 
  # geom_point(data = env.coord.cat, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 4, alpha = 0.6, colour = "#335c67") +
  # geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = row.names(env.coord.cat), colour = "black", fontface = "bold", alpha = 0.8) + 
  geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = NA)+
  geom_segment(data = species.vectors, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), size = 0.1, alpha = 0.1, colour = "grey30") +
  geom_text_repel(data = species.vectors, aes(x = MDS1, y = MDS2), colour = "grey30", label = row.names(species.vectors), size = 3, alpha = 0.5) + 
  theme_bw() +
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = Treatment),
               alpha = 0.3)+
  theme(aspect.ratio = 1) +
  labs(title = "ENREC")+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  theme(axis.title.y = element_blank())+
  scale_colour_manual(values = c("darkgray", "#B46504", "#708D81","gray")) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81","gray"))+
  #annotate("text", x = 0.99, hjust = 1, y = 1.4, label = "Stress = < 0.01")+
  theme(legend.position = "none")+
  theme(aspect.ratio = 1)

enrec.nmds





# SCAL --------------------------------------------------------------------

# SET UP NMDS DATA (FILTER SITES OF INTEREST)
nmds.seedbank.data <- seedbank.data %>% 
  filter(Site %in% c("SCAL"))

comCols <- names(which(colSums(nmds.seedbank.data[,c("AMATU","ERICA","CHEAL","ABUTH","AMAHY","TRITE","MOLVE","OXAST","AMARE","EUPMA","AMAPA","SOLPT","SIDSP")])>-1))
comCols

# CREATE MATRIX FOR NMDS INFORMATION
nmds.com.matrix <- as.matrix(nmds.seedbank.data[,comCols])

# GRAB ENVIRONMENT INFORMAITION
envCols <- c("Site", "Treatment")
nmds.env <- nmds.seedbank.data[,envCols]

# FIT NMDS MODEL
set.seed(56156)
nmds.mod <- metaMDS(nmds.com.matrix, distance = "bray")
nmds.mod #sTRESS = 0.1


# EVALUATE MODEL FIT
# goodness(nmds.mod)
# stressplot(nmds.mod)

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

#make the hulls that points will connect to 
site_hull <- 
  data.scores %>% # dataframe of site scores
  unite("site_trt", Site, Treatment, remove = FALSE) %>%
  group_by(site_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


#install.packages('ggrepel')
library(ggrepel)

# PLOT NMDS OUTPUT
scal.nmds <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = Treatment), size = 3, alpha = 0.5) + 
  # geom_point(data = env.coord.cat, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 4, alpha = 0.6, colour = "#335c67") +
  # geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = row.names(env.coord.cat), colour = "black", fontface = "bold", alpha = 0.8) + 
  geom_text_repel(data = env.coord.cat, aes(x = NMDS1, y = NMDS2 + 0.1), label = NA)+
  geom_segment(data = species.vectors, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), size = 0.1, alpha = 0.1, colour = "grey30") +
  geom_text_repel(data = species.vectors, aes(x = MDS1, y = MDS2), colour = "grey30", label = row.names(species.vectors), size = 3, alpha = 0.5) + 
  theme_bw() +
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = Treatment),
               alpha = 0.3)+
  theme(aspect.ratio = 1) +
  labs(title = "SCAL")+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
  scale_colour_manual(values = c("darkgray", "#B46504", "#708D81","gray")) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81","gray"))+
 #annotate("text", x = 1, y = 0.7, hjust = 1, label = "Stress = 0.10")+
  theme(aspect.ratio = 1)+
  theme(axis.title.y = element_blank())

scal.nmds






colfax.nmds
greeley.nmds
howard.nmds
merrick.nmds
enrec.nmds
scal.nmds



# All graphs put together -------------------------------------------------------
library(patchwork)

title <- ggdraw()+
  draw_label("Seedbank NMDS", fontface = "bold", hjust = 0.5)


# plot_grid(colfax.nmds, greeley.nmds, howard.nmds, merrick.nmds)
# 
# plot_grid(enrec.nmds, scal.nmds)

#use patchwork instead

enrec.nmds + scal.nmds

colfax.nmds+greeley.nmds+merrick.nmds+howard.nmds+enrec.nmds+scal.nmds+ncol(3)+plot_annotation(title = "Non-Metric Multidimensional Scaling by Site", theme = theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold"))) -> plotnmds
plotnmds


ggsave(plotnmds, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\NMDSUPDATED.png", dpi = 600, width = 24, height = 12)

