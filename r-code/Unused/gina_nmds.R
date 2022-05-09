library(tidyverse)
library(vegan) #--does nmds
library(here)
#install.packages('vegan')

seedbank.data <- read.csv(file = here("data", "updatedseedbank-data-sum.csv"))
summary(seedbank.data)
names(seedbank.data)

# analysis ----------------------------------------------------------------

df_dat <- 
  seedbank.data %>%
  #filter(Site == "Greeley") %>%
  group_by(Site, Treatment) %>%
  summarize_at(vars(AMATU:SIDSP), ~sum(., na.rm = TRUE)) %>% 
  unite("eu", Site, Treatment, remove = TRUE) %>% 
  filter(eu != "ENREC_Vetch")
df_dat

mat_dat <- 
  df_dat %>% 
  column_to_rownames(var = "eu")

nmds_res <- metaMDS(mat_dat, distance = 'bray', autotransform = F, expand = F)
plot(nmds_res)
cc_pdist <- dist(scores(nmds_res, display = 'species'))


#redo so we can insert treatment and site
site_scores <- 
  as.data.frame(scores(nmds_res)) %>%
  rownames_to_column() %>% 
  separate(rowname, into = c("Site", "Treatment"), remove = F) %>% 
  rename(summary = rowname) #%>% 
  # unite("site_sys", site, sys_trt, remove = F)



site_scores %>% 
  write_csv("data/NMDS_scores.csv")

spp_scores  <- 
  as.data.frame(scores(nmds_res, "species")) %>%
  rownames_to_column(., var = "speciesID") %>% 
  filter(speciesID != "PLOT") %>% 
  filter(speciesID != "UNKNOWN")

spp_scores %>% 
  write_csv("data/species_scores.csv")

# Makes polygons for site by treatment 

#I dont really know why she did this?? but its used in the nmds

site_hull <- 
  site_scores %>% # dataframe of site scores
  unite("site_trt", Site, Treatment, remove = FALSE) %>%
  group_by(site_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull %>% 
  write_csv("data/nmds-site-hulls.csv")



# # do anova ----------------------------------------------------------------
# 
# 
# #--on all sites, duh site is sig, cc is not at all
# adonis2(df_dat %>% select(-1) %>% as.matrix() ~ 
#           site + cc_trt, data = (df_dat %>% 
#                                    separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
#                                    mutate_if(is.character, as.factor)),
#         by = "margin"
# )
# 
# 
# # individual sites, where cc-ing was sig? ---------------------------------
# 
# #--funcke
# adonis2(df_dat %>%
#           separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
#           filter(site == "Funcke") %>%  
#           select(-(site:rep)) %>% 
#           as.matrix() ~ 
#           cc_trt, data = (df_dat %>% 
#                             separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
#                             filter(site == "Funcke") %>% 
#                             mutate_if(is.character, as.factor)),
#         by = "margin"
# )
# 
# 
# #--silage
# adonis2(df_dat %>%
#           separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
#           filter(crop_sys == "silage") %>%  
#           select(-(site:rep)) %>% 
#           as.matrix() ~ 
#           cc_trt, data = (df_dat %>% 
#                             separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
#                             filter(crop_sys == "silage") %>% 
#                             mutate_if(is.character, as.factor))
# )
# 
# 
# ################# keep outlier ################################
# 
# # analysis ----------------------------------------------------------------
# 
df_dat_full <-
  pfi_ghobsraw %>%
  #filter(!(site_name == "Funcke" & rep == 4)) %>% #--remove outlier
#   group_by(site_name, sys_trt, cc_trt, blockID) %>%
#   summarize_at(vars(AMATU:UD), ~sum(., na.rm = TRUE)) %>%
#   unite("eu", site_name, sys_trt, cc_trt, blockID, remove = TRUE)
# 
# mat_dat_full <-
#   df_dat_full %>%
#   column_to_rownames(var = "eu")
# 
# 
# nmds_res <- metaMDS(mat_dat_full, distance = 'bray', autotransform = F, expand = F)
# #stress = 0.102
# plot(nmds_res)
# cc_pdist <- dist(scores(nmds_res, display = 'sites'))

# #--need help knowing what to report about this fit
# 
# site_scores <- 
#   as.data.frame(scores(nmds_res)) %>%
#   rownames_to_column() %>% 
#   separate(rowname, into = c("site", "sys_trt", "cc_trt", "blockID", "rep"), remove = F) %>% 
#   rename(site_sys = rowname) %>% 
#   unite("site_sys", site, sys_trt, remove = F)
# 
# site_scores %>% 
#   write_csv("01_stats-nmds/st_nmds-site-full.csv")
# 
# spp_scores  <- 
#   as.data.frame(scores(nmds_res, "species")) %>%
#   rownames_to_column(., var = "speciesID")
# 
# spp_scores %>% 
#   write_csv("01_stats-nmds/st_nmds-spp-full.csv")
# 
# # Makes polygons for site by treatment
# site_hull <- 
#   site_scores %>% # dataframe of site scores
#   unite("site_sys_trt", site, sys_trt, cc_trt, remove = FALSE) %>%
#   group_by(site_sys_trt) %>% # grouping variables: farm AND treatmnet
#   slice(chull(NMDS1, NMDS2)) # points that polygons will connect
# 
# site_hull %>% 
#   write_csv("01_stats-nmds/st_nmds-site-hulls-full.csv")
# 
# 
# 

# Figures below -----------------------------------------------------------------

# exploratory figure ------------------------------------------------------

# note: manuscript figure is created in make-figs folder

#install.packages('ggrepel')
library(ggrepel)

mycols <- c("darkred", "darkgoldenrod", "burlywood3", "darkolivegreen4","red3", "indianred3")
scales::show_col(mycols)
theme_set(theme_bw())


ggplot() +
  geom_point(data = site_scores, 
             aes(x = NMDS1, 
                 y = NMDS2, 
                 color = Site, shape = Treatment), 
             size = 3, 
             alpha = 0.6) +
  geom_text_repel(data = spp_scores, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID), 
                  alpha = 0.5) + 
  geom_polygon(data = site_hull, #need to create this still.....
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = Site),
               alpha = 0.3) + 
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = mycols) +
  scale_fill_manual(values = c("#1B9E77", "#1B9E77",
                               "#D95F02", "#D95F02",
                               "#7570B3", "#7570B3",
                               "#E6AB02", "#E6AB02",
                               "blue1", "blue2",
                               "blue3", "red1",
                               "red3")) +
  labs(color = "Site",
       shape = "Cover Crop Treatment")+
  guides(fill = FALSE)+
  theme_bw() +
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12))

#THIS ENTIRE THING RAN 12/27

#so this created an NMDS. it shows NMDS scores by site and then again by species? need to get gina's library in to see how this actually looks.


# tutor me ----------------------------------------------------------------
# 
# https://rstudio-pubs-static.s3.amazonaws.com/246172_1930ddfb5f064b2bab54b11016ab407e.html
# 
# birds <- read.csv('https://raw.githubusercontent.com/collnell/lab-demo/master/bird_by_fg.csv')
# trees <- read.csv('https://raw.githubusercontent.com/collnell/lab-demo/master/tree_comp.csv')
# 
# #--question, is tree B associated with differences in bird comp?
# 
# bird.matrix <- as.matrix(birds[,3:9])##response variables in a sample x species matrix
# trees$B <- as.factor(trees$B)
# 
# bird.manova <- manova(bird.matrix~as.factor(B), data=trees) ##manova test
# summary(bird.manova) #--sort of
# 
# #--make data a matrix
# weed_matrix <- as.matrix(mat_dat)
# 
# 
# #--get groupings
# trts <- 
#   mat_dat %>% 
#   rownames_to_column() %>% 
#   separate(rowname, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
#   mutate_if(is.character, as.factor) 
# 
# #--doesn't work. just use adonis like lydia
# cc.manova <- manova(weed_matrix ~ as.factor(cc_trt), data = trts)
# summary(cc.manova)
# 


# lydias ------------------------------------------------------------------

#dbRDA - don't like this....
cc.dbrda <- capscale(weed_matrix ~ site + cc_trt, distance='bray', data = trts)
plot(cc.dbrda)
anova(cc.dbrda)

# marginal SS
# adonis2(matrix_dat ~ loc_sys+cc_trt, data = wide_datw, by = 'margin')
# adonis2(weed_matrix ~ site + cc_trt, data = trts, by = 'margin')



#-----more lydia, haven't gone through


# ^^ need to use betadispr to test for equal variance among groups
groups <- wide_datw %>%
  unite(loc_sys_cc, loc_sys, cc_trt, sep = "_") %>%
  select(loc_sys_cc) %>%
  unlist() %>%
  unname()
# testing homogeneity - CC is ok, CC*LOC is ok, LOC is not homoegenous...
b <- betadisper(vegdist(matrix_dat), wide_datw$cc_trt)
b
anova(b)
plot(b)
# figure ------------------------------------------------------------------

site_hull2 <- 
  site_hull %>% 
  #group_by(cc_trt, site_sys, residue) %>% 
  slice(c(1, n()))


# Original Code -----------------------------------------------------------

#nmds3 <- 
ggplot() +
  #--grasses
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "grass"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "italic",
                  alpha = 0.5, 
                  color = "gray70") + # Species as text - better!
  #--forbs
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "forb"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "bold",
                  alpha = 0.6,
                  color = "gray70") + # Species as text - better!
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = site_sys_trt),
               alpha = 0.3) + 
  geom_path(data = site_hull,
            aes(x = NMDS1, 
                y = NMDS2,
                group = interaction(cc_trt, crop_sys, site),
                linetype = cc_trt
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 1) +
  geom_path(data = site_hull2, 
            aes(x = NMDS1, 
                y = NMDS2,
                group = interaction(cc_trt, crop_sys, site),
                linetype = cc_trt
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 0.9) +
  facet_wrap(.~site_sys+residue) +
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = c(p_pink, p_green, p_blue, p_orange, p_purp)) +
  scale_fill_manual(values = c(p_yellow, p_yellow,
                               p_purp, p_purp,
                               p_green, p_green,
                               p_blue, p_blue,
                               p_orange, p_orange)) +
  labs(color = "Site",
       linetype = "Cover Crop Treatment")+
  guides(fill = FALSE,
         shape = F)+
  theme_minimal() + 
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.justification = c(1, 0),
        legend.position = c(0.95, 0.15),
        #legend.text       = element_text(size = 12),
        #legend.title      = element_text(size = 14),
        #axis.title        = element_text(size = 14),
        #axis.text         = element_text(size = 12),
        strip.text = element_text(face = "bold", size = rel(1.2)),
        #legend.key.size = unit(0.8, "lines"),
        legend.title = element_text(size = rel(1), face = "bold"),
        legend.text = element_text(size = rel(1)))


ggsave("02_make-figs/manu-new/fig3.jpg", width = 8.3, height = 5.7)


# My adaptation -----------------------------------------------------------

# figure ------------------------------------------------------------------

site_hull2 <- 
  site_hull %>% 
  group_by(cc_trt, site_sys, residue) %>% 
  slice(c(1, n()))

#nmds3 <- 
ggplot() +
  geom_text_repel(data = spp_scores) 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID,
                  fontface = "italic",
                  alpha = 0.5, 
                  color = "gray70") + # Species as text - better!
  #--forbs
  # geom_text_repel(data = spp_scores %>% filter(functional_grp == "forb"), 
  #                 aes(x = NMDS1, 
  #                     y = NMDS2, 
  #                     label = speciesID),
  #                 fontface = "bold",
  #                 alpha = 0.6,
  #                 color = "gray70") + # Species as text - better!
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = site_trt),
               alpha = 0.3) + 
  geom_path(data = site_hull,
            aes(x = NMDS1, 
                y = NMDS2,
                group = interaction(Treatment, Site),
                linetype = Treatment
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 1) +
  # geom_path(data = site_hull2, 
  #           aes(x = NMDS1, 
  #               y = NMDS2,
  #               group = interaction(cc_trt, crop_sys, site),
  #               linetype = cc_trt
  #           ),
  #           na.rm = TRUE,
  #           color = "gray40",
  #           size = 0.9) +
  facet_wrap(.~site_sys+residue) +
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = c(p_pink, p_green, p_blue, p_orange, p_purp)) +
  scale_fill_manual(values = c(p_yellow, p_yellow,
                               p_purp, p_purp,
                               p_green, p_green,
                               p_blue, p_blue,
                               p_orange, p_orange)) +
  labs(color = "Site",
       linetype = "Cover Crop Treatment")+
  guides(fill = FALSE,
         shape = F)+
  theme_minimal() + 
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.justification = c(1, 0),
        legend.position = c(0.95, 0.15),
        #legend.text       = element_text(size = 12),
        #legend.title      = element_text(size = 14),
        #axis.title        = element_text(size = 14),
        #axis.text         = element_text(size = 12),
        strip.text = element_text(face = "bold", size = rel(1.2)),
        #legend.key.size = unit(0.8, "lines"),
        legend.title = element_text(size = rel(1), face = "bold"),
        legend.text = element_text(size = rel(1)))


ggsave("02_make-figs/manu-new/fig3.jpg", width = 8.3, height = 5.7)
