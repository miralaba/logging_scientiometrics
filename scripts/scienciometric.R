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

#adjustments (replacing neotropical by neotropics, aves by birds and trees* by trees)
concession.publication.raw.data$Region <- ifelse(concession.publication.raw.data$Region == "Netropical", "Neotropics", concession.publication.raw.data$Region)
concession.publication.raw.data$Topic <- ifelse(concession.publication.raw.data$Topic == "Aves", "Birds", concession.publication.raw.data$Topic)
concession.publication.raw.data$Topic <- ifelse(concession.publication.raw.data$Topic == "Trees*", "Trees", concession.publication.raw.data$Topic)


#excluding 2022 -- searches were conducted in June 2022
concession.publication.raw.data %>% distinct(ID, .keep_all = T) %>% filter(Year < 2022)

#counting docuemnts by type
concession.publication.raw.data %>% distinct(ID, .keep_all = T) %>% filter(Year < 2022) %>% group_by(`Document Type`) %>% summarise(n())

#keeping full scoping studies
concession.publication.data.stay <- concession.publication.raw.data[concession.publication.raw.data$Keep=="T",]
#checking
head(concession.publication.data.stay)
str(concession.publication.data.stay)
length(unique(concession.publication.data.stay$ID))

#excluding 2022
concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% filter(Year < 2022)

#counting docuemnts by type
concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% filter(Year < 2022) %>% group_by(`Document Type`) %>% summarise(n())


#### maps & graphs ####
#graph publication year
g1 <- concession.publication.data.stay %>% 
            distinct(ID, .keep_all = T) %>% 
            filter(Year < 2022) %>% 
            group_by(Year) %>% 
               summarise(N_publications = n()) %>%
            ungroup() %>% 
            ggplot(aes(x = Year, y = N_publications)) +
                geom_area(fill="#aec6cf") + geom_point() + geom_line() +
                geom_smooth(method="glm", method.args = list(family = poisson), se = F, color = "#838996") +
                scale_x_continuous("", labels = 1975:2021, breaks = 1975:2021, expand = c(0,0.1)) +
                ylab("N. publications") +
                theme(axis.title = element_text(family = "serif", size = 22),
                      axis.text.x = element_text(family = "serif", size = 16, angle = 45, hjust = 1),
                      axis.text.y = element_text(family = "serif", size = 16),
                      axis.line = element_line(linewidth = 1),
                      axis.ticks = element_line(linewidth = 1),
                      panel.background = element_blank())



#png("results/publications_by_year.png", width = 1024, height = 576, units = "px", bg = "transparent")
#g1                                              
#dev.off()



#map study area
# data.frame with the ISO3 country names plus a variable to merge to the map data
NStudy.byCountry.df <- concession.publication.data.stay %>% 
                           distinct(ID, .keep_all = T) %>% 
                           filter(Year < 2022) %>% 
                           group_by(country) %>%
                               summarise(NStudy=n()) %>%
                           ungroup() %>% 
                           arrange(desc(NStudy))

NStudy.byCountry.df  <- data.frame(NStudy.byCountry.df)

# join your NStudy.df to the country map data
NStudy.byCountry.map <- joinCountryData2Map(NStudy.byCountry.df, joinCode = "NAME",
                                            nameJoinColumn = "country")

NStudy.byCountry.map <- spTransform(NStudy.byCountry.map, CRS("+proj=longlat +ellps=WGS84"))

NStudy.byCountry.map <- sf::st_as_sf(NStudy.byCountry.map)

brk <- c(0, ceiling(max(NStudy.byCountry.map$NStudy, na.rm = T)/2), max(NStudy.byCountry.map$NStudy, na.rm = T))


g2 <- ggplot(data = NStudy.byCountry.map) +
        geom_sf(mapping = aes(fill = NStudy)) +
        coord_sf(ylim = c(-60,60)) + 
        #geom_sf_label_repel(aes(label = NStudy), size = 5, label.padding = unit(1, "mm"), label.r = unit(.5, "mm"),
        #                    min.segment.length = .5, direction = "y", max.overlaps = Inf, nudge_x = 1)+
        scale_fill_gradient(low = "#dee8eb", high = "#7ea4b3", na.value = "#ffffff",
                            breaks = brk, labels = brk, limits = brk[-2])+
        xlab("") + ylab("") + 
        theme_bw() + 
        theme(axis.title = element_text(family = "serif", size = 22),
              axis.text = element_blank(),
              panel.background = element_blank(),
              legend.title = element_blank(), legend.background = element_blank(),
              legend.direction = "horizontal", legend.position = c(.1,.1),
              legend.text = element_text(family = "serif", size = 12),
              legend.key.width = unit(1, "cm"))



#png("results/publications_by_region.png", width = 1440, height = 900, units = "px", bg = "transparent")
#g2                                              
#dev.off()

png("results/general_trend.png", width = 1500, height = 1080, units = "px")
ggarrange(
  g1, g2, ncol = 1, nrow = 2, labels = c("(A)", "(B)"), label.x = -.01,
  heights = c(1,1.5)
)
dev.off()




#graph topic per study area
NStudy.byTopic.df <- concession.publication.data.stay %>% 
                         filter(Region != "NA" & Year < 2022) %>% 
                         group_by(Topic) %>%
                            summarize(n()) %>%
                         ungroup() %>% 
                         arrange(desc(`n()`))

write.csv(NStudy.byTopic.df, "results/N_study_bytopic.csv", row.names = F)


g3 <- concession.publication.data.stay %>% filter(Region != "NA" & Year < 2022) %>%
                                              group_by(Region) %>%
                                                 count(Topic) %>%
                                              ungroup() %>%
                                                 ggplot(aes(x=reorder(Topic, n), y=n, fill=Region))+
                                                    geom_bar(stat = "identity") +
                                                    geom_hline(yintercept = 20, linetype = "dashed", linewidth = 1, color ="gray56") + 
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




#study gap evidence
region.interest <- c("Neotropics", "Africa", "Insular Asia", "Continental Asia")
topic.interest <- c("Forest structure/damage/biomass", "Exploited tree species", "Seed dispersal", "Logging and bushmeat", "Impacts on non-timbers", "Amphibians")

g4 <- concession.publication.data.stay %>% 
                     select(Region, `Logging events`, `Time effect`, Intensity, Topic) %>% 
                     filter(Region %in% region.interest) %>%
                     filter(Topic %in% topic.interest) %>% 
                     replace(. == "Insular Asia" | . == "Continental Asia", "Asia") %>%
                     replace(is.na(.), "unknown") %>%
                     group_by(Region, `Logging events`, `Time effect`, Intensity, Topic) %>% 
                     tally() %>% 
                     ungroup() %>% 
                     mutate(`Time effect` = factor(`Time effect`, levels = c("<=10", "10-30", "30-50", ">50", "unknown")),
                            `Logging events` = factor(`Logging events`, levels = c(">2", "2", "1", "unknown")),
                            Intensity = factor(Intensity, levels = c("low", "medium", "high", "unknown"))) %>% 
                     ggplot(aes(`Time effect`, Topic, color = Intensity, alpha=.5, size=n)) +
                     geom_point(position = position_dodge(.3)) +
                     scale_color_manual(values = c("#07b6f8", "#f8c207", "#f8073d", "#97969b")) +
                     scale_size_continuous(breaks = c(1,5,10,15,20), range = c(3,10)) +
                     labs(x="", y="") +
                     guides(alpha = "none", size = guide_legend("#N. Studies"), color = guide_legend(override.aes = list(shape=15, size = 7))) +
                                        facet_grid(`Logging events`~Region) +
                     theme(axis.title = element_text(family = "serif", size = 22),
                           axis.text.x = element_text(family = "serif", size = 18, angle = 45, hjust = 1),
                           axis.text.y = element_text(family = "serif", size = 18, vjust = .5),
                           axis.line.y = element_line(linewidth = 1), axis.line.x = element_line(linewidth = 1),
                           axis.ticks.y = element_line(linewidth = 1), axis.ticks.x = element_line(linewidth = 1),
                           legend.title = element_text(family = "serif", size = 18),
                           legend.text = element_text(family = "serif", size = 18),
                           panel.background = element_blank(),
                           panel.spacing.x = unit(1, "lines"),
                           panel.spacing.y = unit(2, "lines"),
                           strip.text = element_text(family = "serif", size = 18),
                           legend.key = element_rect(fill = "transparent"))
                     


png("results/study_gap.png", width = 1024, height = 750, units = "px")
g4
dev.off()




#graph effect time since logging
NStudy.byTimeeffect.df <- concession.publication.data.stay %>% 
                             distinct(ID, .keep_all = T) %>%
                             filter(Region != "NA" & Year < 2022) %>% 
                             group_by(`Time effect`) %>%
                                summarize(n()) %>%
                             ungroup() %>% 
                             arrange(desc(`n()`))



g5 <- concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% 
                                             group_by(Region) %>%
                                               filter(Region != "NA" & Year < 2022) %>%
                                               count(`Time effect`) %>%
                                             ungroup() %>%
                                               mutate(`Time effect` = factor(`Time effect`, levels = c(">50", "30-50", "10-30", "<=10"))) %>% 
                                                 ggplot(aes(x=`Time effect`, y=n, fill=Region))+
                                                   geom_bar(stat = "identity")+
                                                   geom_hline(yintercept = 20, linetype = "dashed", linewidth = 1, color ="gray56") + 
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

#png("results/timeeffect_by_region.png", width = 1024, height = 480, units = "px", bg = "transparent")
#g4                                              
#dev.off()






#graph n logging events
NStudy.byNevents.df <- concession.publication.data.stay %>% 
                            distinct(ID, .keep_all = T) %>%
                            filter(Region != "NA" & Year < 2022) %>% 
                            group_by(`Logging events`) %>%
                                summarize(n()) %>%
                            ungroup() %>% 
                            arrange(desc(`n()`))



g6 <- concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% 
                                            group_by(Region) %>%
                                               filter(Region != "NA" & Year < 2022) %>%
                                               count(`Logging events`) %>%
                                            ungroup() %>%
                                               mutate(`Logging events` = factor(`Logging events`, levels = c(">2", "2", "1"))) %>% 
                                                  ggplot(aes(x=`Logging events`, y=n, fill=Region))+
                                                     geom_bar(stat = "identity")+
                                                     geom_hline(yintercept = 20, linetype = "dashed", linewidth = 1, color ="gray56") + 
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

#png("results/nevents_by_region.png", width = 1024, height = 480, units = "px", bg = "transparent")
#g5                                              
#dev.off()






#graph intensity
NStudy.byIntensity.df <- concession.publication.data.stay %>% 
                              distinct(ID, .keep_all = T) %>%
                              filter(Region != "NA" & Year < 2022) %>% 
                              group_by(Intensity) %>%
                                  summarize(n()) %>%
                              ungroup() %>% 
                              arrange(desc(`n()`))



g7 <- concession.publication.data.stay %>% distinct(ID, .keep_all = T) %>% 
                                            group_by(Region) %>%
                                               filter(Region != "NA" & Year < 2022) %>%
                                               count(Intensity) %>%
                                            ungroup() %>%
                                                mutate(Intensity = factor(Intensity, levels = c("high", "medium", "low"))) %>% 
                                                    ggplot(aes(x=Intensity, y=n, fill=Region))+
                                                       geom_bar(stat = "identity")+
                                                       geom_hline(yintercept = 20, linetype = "dashed", linewidth = 1, color ="gray56") + 
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
                                                             #legend.position = "none",
                                                             panel.background = element_blank(),
                                                             panel.spacing.x = unit(1, "lines"),
                                                             panel.spacing.y = unit(2, "lines"),
                                                             strip.text = element_text(family = "serif", size = 18))

#png("results/intensity_by_region.png", width = 1024, height = 480, units = "px", bg = "transparent")
#g6                                              
#dev.off()



png("results/study_sites_characteristics.png", width = 920, height = 1080, units = "px", bg = "transparent")
ggarrange(
    g5, g6, g7, ncol = 1, nrow = 3, align = "hv", labels = c("(A)", "(B)", "(C)")#, label.x = -.05
)
dev.off()


#png("results/effects.png", width = 1440, height = 1080, units = "px", bg = "transparent")
#ggarrange(
#  g3, ncol = 2, nrow = 1, widths = c(2,1), labels = "(A)",
#  ggarrange(
#    g4, g5, g6, ncol = 1, nrow = 3, align = "hv", labels = c("(B)", "(C)", "(D)"), label.x = -.05
#  )
#)
#dev.off()
#