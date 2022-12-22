#### CONCESSIONS PROJECT - SCIENTIOMETRIC ####
#working directory
setwd("~/projetos/museu/concessoes")
#libraries
library(readxl)
library(tidyverse)
library(rworldmap)
library(sp)
library(rgdal)

#original data
concession.publication.data.original <- readxl::read_excel("20220706_bd_cienciometria.xlsx")

#editions
concession.publication.data.noNA <- concession.publication.data.original[!is.na(concession.publication.data.original$Keep),]
concession.publication.data.stay <- concession.publication.data.noNA[concession.publication.data.noNA$Keep=="T",]
head(concession.publication.data.stay)
#duplicate
#df1 <- concession.publication.data.stay


#### maps & graphs ####
#graph publication year
concession.publication.data.stay %>% mutate(Year.by5=cut(Year, breaks=seq(1970,2025,5))) %>%
                                        count(Year.by5) %>%
                                        ggplot(aes(x=Year.by5, y=n, fill="red")) +
                                        geom_col(show.legend = F)+
                                        scale_x_discrete("Year", labels=c("1970-75", "85-90", "91-95", "96-2000",
                                                                          "2001-05", "2020-2022"))
                                        

#map study area
# ISO3 names of the countries
Countries <- c("BRA", "COD", "IND", "IDN")

# data.frame with the ISO3 country names plus a variable to merge to the map data
NStudy.byCountry.df <- concession.publication.data.stay %>% group_by(Region) %>%
                                                              summarise(NStudy=n()) %>%
                                                              ungroup()
# some editions
NStudy.byCountry.df  <- data.frame(NStudy.byCountry.df)
NStudy.byCountry.df  <- NStudy.df [-c(4,6),]
NStudy.byCountry.df$Region[1] <- Countries[2]
NStudy.byCountry.df$Region[2] <- Countries[3]
NStudy.byCountry.df$Region[3] <- Countries[4]
NStudy.byCountry.df$Region[4] <- Countries[1]

# join your NStudy.df to the country map data
NStudy.byCountry.map <- joinCountryData2Map(NStudy.byCountry.df, joinCode = "ISO3",
                                            nameJoinColumn = "Region")

NStudy.byCountry.map <- spTransform(NStudy.byCountry.map, CRS("+proj=longlat +ellps=WGS84"))

NStudy.byCountry.map <- sf::st_as_sf(NStudy.byCountry.map)

centroide<-st_centroid(NStudy.byCountry.map[NStudy.byCountry.map$Region %in% Countries,"geometry"])
centroide

ggplot(data = NStudy.byCountry.map) +
  geom_sf()+
  xlab("") + ylab("")+
  annotate(geom = "text", 
           x = c(-53.17256, 23.57684, 117.3629, 79.54024), # from centroid obj
           y = c(-10.65544, -2.838937, -2.271715, 22.81924), # from centroid obj
           label = c(48, 14, 14, 25), # from NStudy.df$NStudy
           color = c("darkgreen", "orange", "brown", "red"), 
           size = 6)

#graph topic per study area
concession.publication.data.stay %>% group_by(Region) %>%
                                      filter(Region != "NA") %>%
                                      count(Topic) %>%
                                      ungroup() %>%
                                      ggplot(aes(x=Topic, y=n, fill=Region))+
                                      geom_bar(stat = "identity")+
                                      scale_fill_manual(values = c("orange", "red", "brown", "darkgreen", "gray85"))+
                                      coord_flip()+
                                      theme(legend.position = c(0.8,0.8), legend.background = element_blank())
                                      

#graph effect time since logging
concession.publication.data.stay %>% group_by(Region) %>%
                                      filter(Region != "NA") %>%
                                      count(`Time effect`) %>%
                                      ungroup() %>%
                                      ggplot(aes(x=`Time effect`, y=n, fill=Region))+
                                      geom_bar(stat = "identity")+
                                      scale_fill_manual(values = c("orange", "red", "brown", "darkgreen", "gray85"))+
                                      coord_flip()+
                                      theme(legend.position = c(0.8,0.2), legend.background = element_blank())

#graph n logging events


#