#### CONCESSIONS PROJECT - SCIENTIOMETRIC ####
#working directory
setwd("~/projetos/museu/concessoes")
#libraries
library(readxl)
library(tidyverse)
library(ggrepel)
library(ggsflabel)
library(ggpubr)
library(rworldmap)
library(sp)
library(rgdal)

#raw data
concession.publication.raw.data <- readxl::read_excel("data/20230222_bd_cienciometria.xlsx", na = "null")
#checking
head(concession.publication.raw.data)
str(concession.publication.raw.data)
length(unique(concession.publication.raw.data$ID))

#keeping full scoping studies
concession.publication.data.stay <- concession.publication.raw.data[concession.publication.raw.data$Keep=="T",]
#checking
head(concession.publication.data.stay)
str(concession.publication.data.stay)
length(unique(concession.publication.data.stay$ID))



#### maps & graphs ####
#graph publication year
g1 <- concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% 
                                           mutate(Year = cut(Year, breaks = seq(1970, 2025, 5))) %>%
                                              group_by(Year) %>% 
                                                 summarise(N_publications = n()) %>%
                                           mutate(pp = as.character(round(100 * N_publications / sum(N_publications),2)),
                                                  lab = paste0(N_publications, " (", pp, "%)")) %>% 
                                           ggplot(aes(x = Year, y = N_publications)) +
                                              geom_bar(stat="identity",fill="#aec6cf", show.legend = F) +
                                              geom_text(aes(label = lab), size = 5, vjust = .1) +
                                              scale_x_discrete("Year", labels=c("1970-74", "1980-84", "1985-89", "1990-94", "1995-99",
                                                                             "2000-04", "2005-09", "2010-14", "2015-19", "2020-22"))+
                                              ylab("N. publications") +
                                              theme(axis.title.y = element_text(family = "serif", size = 44),
                                                    axis.title.x = element_blank(),
                                                    axis.text.x = element_text(family = "serif", size = 36, angle = 45, hjust = 1),
                                                    axis.text.y = element_blank(),
                                                    axis.line.y = element_line(linewidth = 1), axis.line.x = element_line(linewidth = 1),
                                                    axis.ticks.y = element_line(linewidth = 1), axis.ticks.x = element_line(linewidth = 1),
                                                    legend.title = element_text(family = "serif", size = 36),
                                                    legend.text = element_text(family = "serif", size = 36),
                                                    panel.background = element_blank(),
                                                    panel.spacing.x = unit(1, "lines"),
                                                    panel.spacing.y = unit(2, "lines"),
                                                    strip.text = element_text(family = "serif", size = 36))

png("results/publications_by_year.png", width = 1024, height = 576, units = "px", bg = "transparent")
g1                                              
dev.off()






#map study area
# data.frame with the ISO3 country names plus a variable to merge to the map data
NStudy.byCountry.df <- concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% 
                                                            group_by(country) %>%
                                                              summarise(NStudy=n()) %>%
                                                              ungroup()

NStudy.byCountry.df  <- data.frame(NStudy.byCountry.df)

# join your NStudy.df to the country map data
NStudy.byCountry.map <- joinCountryData2Map(NStudy.byCountry.df, joinCode = "NAME",
                                            nameJoinColumn = "country")

NStudy.byCountry.map <- spTransform(NStudy.byCountry.map, CRS("+proj=longlat +ellps=WGS84"))

NStudy.byCountry.map <- sf::st_as_sf(NStudy.byCountry.map)


g2 <- ggplot(data = NStudy.byCountry.map) +
         geom_sf(mapping = aes(fill = NStudy), show.legend = FALSE) + 
         geom_sf_label_repel(aes(label = NStudy), size = 5, label.padding = unit(1, "mm"), label.r = unit(.5, "mm"),
                             min.segment.length = .5, direction = "y", max.overlaps = Inf, nudge_x = 1)+
         #geom_text_repel(aes(label = NStudy, geometry = geometry), stat = "sf_coordinates", size = 3)+
         scale_fill_gradient(low = "#dee8eb", high = "#7ea4b3")+
         xlab("") + ylab("")

png("results/publications_by_region.png", width = 1440, height = 900, units = "px", bg = "transparent")
g2                                              
dev.off()






#graph topic per study area
g3 <- concession.publication.data.stay %>% filter(Region != "NA") %>%
                                              group_by(Region) %>%
                                                 count(Topic) %>%
                                              ungroup() %>%
                                                 ggplot(aes(x=reorder(Topic, n), y=n, fill=Region))+
                                                    geom_bar(stat = "identity")+
                                                    scale_fill_manual(values = c("#cbb466", "#cfaec6", "#b38c7e", "#aecfb7", "#8c7eb3"))+
                                                    coord_flip()+
                                                    labs(x="Topics", y="N. publications") +
                                                    theme(axis.title = element_text(family = "serif", size = 22),
                                                          axis.text.x = element_text(family = "serif", size = 18, hjust = .5),
                                                          axis.text.y = element_text(family = "serif", size = 18, vjust = .5),
                                                          axis.line.y = element_line(linewidth = 1), axis.line.x = element_line(linewidth = 1),
                                                          axis.ticks.y = element_line(linewidth = 1), axis.ticks.x = element_line(linewidth = 1),
                                                          legend.title = element_text(family = "serif", size = 18),
                                                          legend.text = element_text(family = "serif", size = 18),
                                                          legend.position = c(0.85,0.2),
                                                          panel.background = element_blank(),
                                                          panel.spacing.x = unit(1, "lines"),
                                                          panel.spacing.y = unit(2, "lines"),
                                                          strip.text = element_text(family = "serif", size = 18))

png("results/topic_by_region.png", width = 1024, height = 576, units = "px", bg = "transparent")
g3                                              
dev.off()






#graph effect time since logging
g4 <- concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% 
                                             group_by(Region) %>%
                                               filter(Region != "NA") %>%
                                               count(`Time effect`) %>%
                                             ungroup() %>%
                                               mutate(`Time effect` = factor(`Time effect`, levels = c(">50", "30-50", "10-30", "<=10"))) %>% 
                                                 ggplot(aes(x=`Time effect`, y=n, fill=Region))+
                                                   geom_bar(stat = "identity")+
                                                   scale_fill_manual(values = c("#cbb466", "#cfaec6", "#b38c7e", "#aecfb7", "#8c7eb3"))+
                                                   coord_flip(ylim = c(0, 500)) +
                                                   theme(legend.position = c(0.9,0.4), legend.background = element_blank())+
                                                   labs(x="Time since logging", y="") +
                                                   theme(axis.title = element_text(family = "serif", size = 22),
                                                         axis.text.x = element_text(family = "serif", size = 18, hjust = .5),
                                                         axis.text.y = element_text(family = "serif", size = 18, vjust = .5),
                                                         axis.line.y = element_line(linewidth = 1), axis.line.x = element_line(linewidth = 1),
                                                         axis.ticks.y = element_line(linewidth = 1), axis.ticks.x = element_line(linewidth = 1),
                                                         legend.title = element_text(family = "serif", size = 18),
                                                         legend.text = element_text(family = "serif", size = 18),
                                                         legend.position = "none",
                                                         panel.background = element_blank(),
                                                         panel.spacing.x = unit(1, "lines"),
                                                         panel.spacing.y = unit(2, "lines"),
                                                         strip.text = element_text(family = "serif", size = 18))

png("results/timeeffect_by_region.png", width = 1024, height = 480, units = "px", bg = "transparent")
g4                                              
dev.off()






#graph n logging events
g5 <- concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% 
                                            group_by(Region) %>%
                                               filter(Region != "NA") %>%
                                               count(`Logging events`) %>%
                                            ungroup() %>%
                                               mutate(`Logging events` = factor(`Logging events`, levels = c(">2", "2", "1"))) %>% 
                                                  ggplot(aes(x=`Logging events`, y=n, fill=Region))+
                                                     geom_bar(stat = "identity")+
                                                     scale_fill_manual(values = c("#cbb466", "#cfaec6", "#b38c7e", "#aecfb7", "#8c7eb3"))+
                                                     coord_flip(ylim = c(0, 500))+
                                                     theme(legend.position = c(0.9,0.4), legend.background = element_blank())+
                                                     labs(x="N. looging events", y="") +
                                                     theme(axis.title = element_text(family = "serif", size = 22),
                                                           axis.text.x = element_text(family = "serif", size = 18, hjust = .5),
                                                           axis.text.y = element_text(family = "serif", size = 18, vjust = .5),
                                                           axis.line.y = element_line(linewidth = 1), axis.line.x = element_line(linewidth = 1),
                                                           axis.ticks.y = element_line(linewidth = 1), axis.ticks.x = element_line(linewidth = 1),
                                                           legend.title = element_text(family = "serif", size = 18),
                                                           legend.text = element_text(family = "serif", size = 18),
                                                           legend.position = "none",
                                                           panel.background = element_blank(),
                                                           panel.spacing.x = unit(1, "lines"),
                                                           panel.spacing.y = unit(2, "lines"),
                                                           strip.text = element_text(family = "serif", size = 18))

png("results/nevents_by_region.png", width = 1024, height = 480, units = "px", bg = "transparent")
g5                                              
dev.off()






#graph intensity
g6 <- concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% 
                                            group_by(Region) %>%
                                               filter(Region != "NA") %>%
                                               count(Intensity) %>%
                                            ungroup() %>%
                                                mutate(Intensity = factor(Intensity, levels = c("high", "medium", "low"))) %>% 
                                                    ggplot(aes(x=Intensity, y=n, fill=Region))+
                                                       geom_bar(stat = "identity")+
                                                       scale_fill_manual(values = c("#cbb466", "#cfaec6", "#b38c7e", "#aecfb7", "#8c7eb3"))+
                                                       coord_flip(ylim = c(0, 500))+
                                                       theme(legend.position = c(0.9,0.4), legend.background = element_blank())+
                                                       labs(x="Intensity", y="") +
                                                       theme(axis.title = element_text(family = "serif", size = 22),
                                                             axis.text.x = element_text(family = "serif", size = 18, hjust = .5),
                                                             axis.text.y = element_text(family = "serif", size = 18, vjust = .5),
                                                             axis.line.y = element_line(linewidth = 1), axis.line.x = element_line(linewidth = 1),
                                                             axis.ticks.y = element_line(linewidth = 1), axis.ticks.x = element_line(linewidth = 1),
                                                             legend.title = element_text(family = "serif", size = 18),
                                                             legend.text = element_text(family = "serif", size = 18),
                                                             legend.position = "none",
                                                             panel.background = element_blank(),
                                                             panel.spacing.x = unit(1, "lines"),
                                                             panel.spacing.y = unit(2, "lines"),
                                                             strip.text = element_text(family = "serif", size = 18))

png("results/intensity_by_region.png", width = 1024, height = 480, units = "px", bg = "transparent")
g6                                              
dev.off()






png("results/geral.png", width = 1440, height = 1080, units = "px", bg = "transparent")
ggarrange(
  g3, ncol = 2, nrow = 1, widths = c(2,1), labels = "(A)",
  ggarrange(
    g4, g5, g6, ncol = 1, nrow = 3, align = "hv", labels = c("(B)", "(C)", "(D)"), label.x = -.05
  )
)
dev.off()
#