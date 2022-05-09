# ------------------------------------------------------------------------------
# LOAD LIBRARIES ---------------------------------------------------------------
# ------------------------------------------------------------------------------
#install.packages('readr')
library(readr)        # read in and output .csv files
library(lattice)
library(tidyverse)    # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)         # makes file paths for input and output cleaner
library(vegan)        # runs the NMDS model
library(factoextra)   # makes pca plots from ggplot2
#install.packages('factoextra')

# TUTORIALS
# https://jkzorz.github.io/2019/06/06/NMDS.html
# https://jkzorz.github.io/2020/04/04/NMDS-extras.html
# https://rpubs.com/CPEL/NMDS

# ------------------------------------------------------------------------------
# IMPORT SEEDBANK DATA ---------------------------------------------------------
# ------------------------------------------------------------------------------
seedbank.data <- read.csv(file = here("data", "updatedseedbank-data-sum.csv")) %>%
  mutate(Experiment = ifelse(Site %in% c("ENREC", "SCAL"), "Research Station", "On Farm"), .before = "Site") %>%
  mutate(Site_Trt = paste(Site, Treatment, sep = "-"), .after = Treatment) %>% 
  filter(Treatment != "VetchDNI")
summary(seedbank.data)
names(seedbank.data)

# ------------------------------------------------------------------------------
# NMDS SEEDBANK DATA -----------------------------------------------------------
# ------------------------------------------------------------------------------

# Specify desired experiment (Research Station / On Farm) and percent to filter at
# grab names of seed species < desired % (0.05 or 0.01)
# uncomment the filter for desired experiment type (Research Station or On Farm)

expType = c("Research Station", "On Farm") 
# expType = c("Research Station")
# expType = c("On Farm")

#pctDesired = 0.05
pctDesired = 0.01

# ------------------------------

# SET UP NMDS DATA (FILTER SITES OF INTEREST)
nmds.seedbank.data <- seedbank.data %>% 
  filter(Experiment %in% c(expType)) %>%
  mutate(Site = factor(Site, levels = c("Colfax", "Greeley", "Howard", "Merrick", "SCAL", "ENREC")))


# GRAB COLUMNS FOR NMDS COMMUNITY/SPECIES
# grab names of seed species < desired % (0.05 or 0.01)
# uncomment the filter for desired experiment type (Research Station or On Farm)

filtered_seedbank <- seedbank.data %>% 
  filter(Experiment %in% c(expType)) %>%
  summarize(across(AMATU:SIDSP, sum)) %>%
  mutate(Total = rowSums(., na.rm = T)) %>%
  pivot_longer(cols = c("AMATU":"SIDSP"),
               names_to = "Species",
               values_to = "Count") %>%
  mutate(Pct = Count/Total) %>%
  filter(Pct > 0.01)


#individual seedbanks filtered instead
filtered_seedbank <- seedbank.data %>% 
  group_by(Site) %>% 
  filter(Experiment %in% c(expType)) %>%
  summarize(across(AMATU:SIDSP, sum)) %>%
  mutate(Total = rowSums(select(., AMATU:SIDSP)), .before = AMATU) %>% 
  pivot_longer(cols = c("AMATU":"SIDSP"),
               names_to = "Species",
               values_to = "Count") %>%
  filter(Count > 0) %>% 
  mutate(Pct = Count/Total) %>%
  filter(Pct > 0.01)


#2/2 this is where I stopped knowing what was going on!! lol 

comCols <- filtered_seedbank$Species
comCols


# CREATE MATRIX FOR NMDS INFORMATION
nmds.com.matrix <- as.matrix(nmds.seedbank.data[,comCols])



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

# CREATE HULLS
site_hull <- 
  data.scores %>% # dataframe of site scores
  unite("site_trt", Site, Treatment, remove = FALSE) %>%
  group_by(site_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull2 <- 
  site_hull %>% 
  group_by(site_trt) %>% 
  slice(c(1, n()))

# EXTRACT SPECIES LOADINGS
species.vectors <- as.data.frame(nmds.mod$species) %>%
  as.data.frame() %>%
  rownames_to_column("Species") %>%
  expand_grid("Site" = unique(nmds.seedbank.data$Site)) %>%
  mutate(Site = factor(Site, levels = c("Colfax", "Greeley", "Howard", "Merrick", "SCAL", "ENREC")))
summary(species.vectors)

# ------------------------------------------------------------------------------
# PERMUTATION ANOVA (Not used in the plot anymore - we now use the hulls) ------
# ------------------------------------------------------------------------------
# GRAB ENVIRONMENT INFORMAITION
envCols <- c("Treatment", "Site", "Site_Trt")
nmds.env <- nmds.seedbank.data[,envCols]

env.mod = envfit(nmds.mod, nmds.env, permutations = 999, na.rm = TRUE)
env.mod

env.coord.cat <- as.data.frame(scores(env.mod, "factors")) %>%
  rownames_to_column("Variable") %>%
  filter(substr(Variable, 1, 8) == "Site_Trt") %>%
  mutate(Variable = substr(Variable, 9, 23)) %>%
  separate(Variable, into = c("Site", "Treatment")) %>%
  mutate(Site = factor(Site, levels = c("Colfax", "Greeley", "Howard", "Merrick", "SCAL", "ENREC")))
env.coord.cat

# ------------------------------------------------------------------------------
# PLOT NMDS OUTPUT -------------------------------------------------------------
# ------------------------------------------------------------------------------

p_green <- "#619B44"
p_blue <- "dodgerblue4"#"#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FAE549FD" #"#FFE100"
p_gray <- "#E7E6E6"
p_purp <- "#8B1C62"

mycols <- c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02")
# scales::show_col(mycols)

nmds.plot <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = Treatment),
               alpha = 0.3) + 
  geom_path(data = site_hull,
            aes(x = NMDS1, 
                y = NMDS2,
                group = Treatment,
                linetype = Treatment
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 0.3) +
  geom_path(data = site_hull2, 
            aes(x = NMDS1, 
                y = NMDS2,
                group = Treatment,
                linetype = Treatment
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 0.3) +
  geom_point(aes(color = Treatment, fill = Treatment), size = 1, alpha = 0.5) + 
  # geom_point(data = env.coord.cat, aes(x = NMDS1, y = NMDS2, color = Treatment, fill = Treatment), shape = "diamond", size = 2, alpha = 0.6) +
  geom_segment(data = species.vectors, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), size = 0.1, alpha = 0.3, colour = "grey30") +
  geom_text(data = species.vectors, aes(x = MDS1, y = MDS2, label = Species), colour = "grey30", size = 3, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", alpha = 0.3) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  facet_wrap(~Site) +
  scale_colour_manual(values = mycols) +
  scale_fill_manual(values = mycols)
nmds.plot

# Save out plot
ggsave(nmds.plot, filename = here("2022.01.20-files/nmds-plot.png"), width = 12, height = 6)

