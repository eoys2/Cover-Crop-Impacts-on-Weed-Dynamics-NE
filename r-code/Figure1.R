#Faceting total and pigweed seedbank graphs


# Libraries ---------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)

#NOTE: MUST INSERT MODEL+EMMEANS RESULTS FOR THIS TO RUN FUNCTIONALLY IN ONE SCRIPT

# GGPLOT: Combine the on-farm and research station plots together ----------------------------------------------
totseeds_weeds_onfarm_emmeansplot <- totseeds_weeds.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw()+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  #theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(legend.position = "none")+
  #theme(aspect.ratio = 1) +
  #scale_y_continuous("Total Weed Seeds") +
  scale_y_continuous(limits = c(0,6000),expression(paste("Estimated Total Seedbank Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totseeds_weeds_onfarm_emmeansplot


# PLOT ESTIMATED TREATMENT MEANS
totseeds_weeds_research_emmeansplot <- totseeds_weeds.research.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw() +
  #theme(aspect.ratio = 1) +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  #theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  scale_y_continuous(limits = c(0,6000),expression(paste("Estimated Total Seedbank Density m"^"-2"))) +
  scale_fill_manual(values = c("darkgray", "#B46504", "#708D81"))
totseeds_weeds_research_emmeansplot


totseeds_weeds_research_emmeansplot+totseeds_weeds_onfarm_emmeansplot+  plot_layout(ncol = 1, widths = c(0.25,0.25), heights = c(2,4))+
  plot_annotation(title = "Estimated Total Seedbank Density", theme = theme(plot.title = element_text(size = 18, hjust = 0.5)))-> newplotA
newplotA

gtA <- patchwork::patchworkGrob(newplotA)

gridExtra::grid.arrange(gtA, left = textGrob(expression(paste("Estimated Total Seedbank Density (weed seeds  ", m^-2, ")")), rot = 90)) -> totalseedbank_plot

#Save as desired
#Saved: 3/11/22
#ggsave(totalseedbank, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\Figures2\\Seedbank\\totalseedbank.png", dpi = 600, width = 6, height = 6)





# Pigweeds ----------------------------------------------------------------

combined.data2$Treatment <- factor(combined.data2$Treatment, levels = c("Check","Cereal Rye","Hairy Vetch","Multi Species"))


totpigs_onfarm_emmeansplot <- totpigs.onfarm.emmeans %>% 
  as_tibble() %>%
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 3.625^2) * 20 ) / 10000 ))), width = 0.1) +
  facet_wrap(~Site) +
  theme_bw() +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  #theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(panel.spacing = unit(1, "lines"))+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  #annotate("text", x=2, y=2000, label= "boat") + 
  #theme(aspect.ratio = 1) +
  #labs(x = Treatment, y = expression(Estimated Pigweed Density m))
  scale_y_continuous(limits = c(0,6000),expression(paste("Estimated Pigweed Seedbank Density weeds m"^"-2")))+
  scale_fill_manual(values = c("darkgray", "#9d0208"))
totpigs_onfarm_emmeansplot


totpigs_research_emmeansplot <- totpigs.research.emmeans %>% 
  as_tibble() %>%
  #filter(Site == "SCAL") %>% 
  ggplot(aes(x = Treatment, y = response*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = (response-SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 )), ymax = (response+SE)*(1 / (((pi * 1.5875^2) * 20 ) / 10000 ))), width = 0.1) + 
  facet_wrap(~Site) +
  theme_bw() +
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  #theme(aspect.ratio = 1) +
  scale_y_continuous(limits = c(0,6000), expression(paste("Estimated Pigweed Seedbank Density m"^"-2"))) +
  scale_fill_manual(values = c("Check" = "darkgray", "Cereal Rye"="#B46504","Hairy Vetch"="#708D81"))
totpigs_research_emmeansplot


totpigs_research_emmeansplot+totpigs_onfarm_emmeansplot+
  plot_layout(ncol = 1, widths = c(1.5,1.5), heights = c(2,4))+
  plot_annotation(title = "Estimated Pigweed Seedbank Density", theme = theme(plot.title = element_text(size = 18, hjust = 0.25)))-> newplotB
newplotB


gtB <- patchwork::patchworkGrob(newplotB)
gridExtra::grid.arrange(gtB, left = textGrob(expression(paste("Estimated Pigweed Seedbank Density (weed seeds  ", m^-2, ")")), rot = 90)) -> pigweedseedbank_plot

#Save as desired (note: you will need to change the pathway to your own)
#Save 3/11/22
#ggsave(pigweedseedbank, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\WRITING\\UpdatedFigure_3.pdf", dpi = 950, width = 6, height = 6)



grid.arrange(totalseedbank_plot, pigweedseedbank_plot, ncol=2, widths = c(0.85,1)) -> Figure1
Figure1
ggsave(Figure1, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\WEEDS_CC_PUB\\Figures\\Figure1_noasterick.pdf", dpi = 1200, width = 12, height = 6)

ggsave(Figure1, file = "C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\WEEDS_CC_PUB\\Figures\\Figure1_noasterickPNG.png", dpi = 1200, width = 12, height = 6)


