# Map Figure
setwd("~/Long term abundance")
library(tidyverse)
library(sf)
library(ggspatial)

WRPA_all <-st_read("../database exploration/WRA Shapefile/Surface Water Water Resource Plan Areas.shp") %>%
  dplyr::select(SWWRPANAME) %>% st_make_valid() 

#WRPA <- WRPA_all %>% filter(SWWRPANAME == Focus_Area)

stream<-st_read("../Jerom/RMaps/mdb_Stream_layer_GDA1994.shp") 
states<-st_read("../Jerom/RMaps/Australian_states_GDA1994.shp")


all_data <- read.csv("All e-catch_30_11_2022_WRPA_fixed.csv") %>% mutate(project_segment = paste0(ProjectName,":",SegmentName))

segments <- all_data %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")
all_data
all_data <- all_data %>% filter(!project_segment %in% bad_list) %>%
  filter(Method == "BTE" | Method == "BPE") %>%
  mutate(EffortData = str_replace_all(EffortData, pattern = "\"\"", replacement = "\"")) %>%
  mutate(boat = str_extract(EffortData, pattern="\"ElectrofishingDuration\"......"),
         ESecs = parse_number(boat),
         boat = NULL)# %>%



basins <- c("Barwon-Darling Watercourse", "Gwydir", "Intersecting Streams",
            "Lachlan", "Macquarie-Castlereagh", "Murrumbidgee", "Namoi", 
            "New South Wales Border Rivers", "New South Wales Murray",
            "New South Wales Lower Darling") #

all_data <- all_data %>% #filter(CommonName == "Golden perch") %>%
  mutate(#Date = lubridate::mdy(Date),
    #Year = lubridate::year(Date),
    SiteID = as.factor(as.character(SiteID))) %>%
  filter(SWWRPANAME_NEW %in% basins) %>%
  mutate(SWWRPANAME_NEW = as.factor(SWWRPANAME_NEW),
         Sampling_duration = as.numeric(ESecs),
         ESecs = NULL) %>%
  filter(Sampling_duration>0) %>% 
  filter(Sampling_duration < 600) %>%
  distinct()# removes a event with sample time of zero secs - there are some negatives???
all_data <- all_data %>% #filter(CommonName == "Golden perch") %>%
  mutate(#Date = lubridate::mdy(Date),
    #Year = lubridate::year(Date),
    SiteID = as.factor(as.character(SiteID))) %>%
  filter(SWWRPANAME_NEW %in% basins)# %>% 
locs <- all_data %>% select(coords.x2, coords.x1) %>% distinct()


BDW <- WRPA_all %>% filter(SWWRPANAME == "Barwon-Darling Watercourse")
WRPA_all <- WRPA_all %>% mutate(SWWRPANAME = case_when(SWWRPANAME %in% basins ~ SWWRPANAME,
                                                      T ~ "Other Murray-Darling Basin")) %>%
  filter(SWWRPANAME != "Barwon-Darling Watercourse")

p1 <- ggplot(locs, aes(coords.x1, coords.x2)) +
  layer_spatial(data=states,colour="black", lwd=0.8, fill="khaki3")+
  layer_spatial(WRPA_all, aes(fill=SWWRPANAME), col="black", 
                alpha=0.9, size=1)+
  layer_spatial(BDW, aes(col = SWWRPANAME), size=2, fill=NA)+
  #layer_spatial(WRPA, aes(fill=SWWRPANAME))+
  layer_spatial(data=stream,colour="grey30")+
  #layer_spatial(data=rivers,colour="red")+
  geom_point(shape=4,size=2)+
  scale_fill_manual(name="Water Resource Planning Area",
                    values = rev(c("grey90", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF",
                               "#72FE5EFF", "#C7EF34FF", "#FABA39FF", "#F66B19FF",
                               "#CB2A04FF", "#7A0403FF")))+ # turbo colour scale
  scale_colour_manual(values=c("hotpink"), name=NULL)+
  coord_sf(xlim=c(st_bbox(WRPA_all)[1], st_bbox(WRPA_all)[3]), 
           ylim= c(st_bbox(WRPA_all)[2],st_bbox(WRPA_all)[4]))+
  scale_x_continuous(breaks=c(138,142,146,150))+
  theme_bw()+
  ylab("Latitude") + xlab("Longitude")+
  theme(legend.position = "right",
        axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        legend.title = element_text(face="bold", size=10),
        legend.text = element_text(colour="black", size=8))
p1
#ggsave("Manuscript map.png", dpi = 600, width=21, height=21, unit="cm")
#ggsave("Manuscript map.pdf", dpi = 600, width=21, height=21, unit="cm")


### Inset map

p2 <- ggplot() +
  layer_spatial(data=states,colour="black", lwd=0.8, fill="khaki3")+
  layer_spatial(WRPA_all,col="black", fill = "grey90" ,
                alpha=0.9, size=0.5)+
  geom_rect(col="black", aes(xmin=st_bbox(WRPA_all)[1],xmax= st_bbox(WRPA_all)[3], 
                        ymin= st_bbox(WRPA_all)[2], ymax = st_bbox(WRPA_all)[4]),
            fill=NA)+
  #layer_spatial(BDW, aes(col = SWWRPANAME), size=2, fill=NA)+
  #layer_spatial(WRPA, aes(fill=SWWRPANAME))+
  #layer_spatial(data=stream,colour="grey30")+
  #layer_spatial(data=rivers,colour="red")+
  #geom_point(shape=4,size=2)+
  #scale_fill_manual(name="Water Resource Planning Area",
  #                  values = rev(c("grey90", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF",
  #                                 "#72FE5EFF", "#C7EF34FF", "#FABA39FF", "#F66B19FF",
  #                                 "#CB2A04FF", "#7A0403FF")))+ # turbo colour scale
  #scale_colour_manual(values=c("hotpink"), name=NULL)+
  #coord_sf(xlim=c(st_bbox(WRPA_all)[1], st_bbox(WRPA_all)[3]), 
  #         ylim= c(st_bbox(WRPA_all)[2],st_bbox(WRPA_all)[4]))+
  #scale_x_continuous(breaks=c(138,142,146,150))+
  theme_classic()+
  ylab("Latitude") + xlab("Longitude")+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(colour="black", size=10),
        axis.ticks = element_blank(),
        axis.line = element_blank())


library(patchwork)

p2 + p1 + plot_layout(guides = 'collect', widths = c(0.5,1)) & theme(legend.position = "bottom")

#ggsave("Manuscript map.png", dpi = 600, width=21, unit="cm")
#ggsave("Manuscript map.pdf", dpi = 600, width=21, height=21, unit="cm") # editted in Acrobat


#### Can we do individual maps for the valleys

p20 <- p1 + coord_sf(xlim=c(141, 148.3), ylim = c(-37, -33.5)) + theme(legend.position = "none",
                                                                      title = element_text(face="bold", size=14)) +
  ggtitle("NSW Murray (Purple)")
p20

ggsave("Murray map.png", dpi=600, width=21, units="cm")

p3 <- p1 + coord_sf(xlim=c(141, 145.5), ylim = c(-34.5, -30)) + theme(legend.position = "none",
                                                                      title = element_text(face="bold", size=14)) +
  ggtitle("NSW Lower Darling (Blue)")
p3

ggsave("Lower Darling map.png", dpi=600, width=21, units="cm")

p4 <- p1 + coord_sf(xlim=c(148.5, 152), ylim = c(-30, -28.45)) + theme(legend.position = "none",
                                                                      title = element_text(face="bold", size=14)) +
  ggtitle("NSW Border Rivers (Green)")
p4

ggsave("Border Rivers map.png", dpi=600, width=21, units="cm")

p5 <- p1 + coord_sf(xlim=c(147.2, 151.5), ylim = c(-32, -29.6)) + theme(legend.position = "none",
                                                                      title = element_text(face="bold", size=14)) +
  ggtitle("Namoi (Lime)")
p5

ggsave("Namoi map.png", dpi=600, width=21, units="cm")

p6 <- p1 + coord_sf(xlim=c(143, 149.5), ylim = c(-36.5, -33.5)) + theme(legend.position = "none",
                                                                      title = element_text(face="bold", size=14)) +
  ggtitle("Murrumbidgee (Yellow)")
p6

ggsave("Murrumbidgee map.png", dpi=600, width=21, units="cm")

p7 <- p1 + coord_sf(xlim=c(143.7, 150), ylim = c(-34.9, -32.2)) + theme(legend.position = "none",
                                                                      title = element_text(face="bold", size=14)) +
  ggtitle("Lachlan (Orange)")
p7

ggsave("Lachlan map.png", dpi=600, width=21, units="cm")

p8 <- p1 + coord_sf(xlim=c(145.7, 150.5), ylim = c(-34, -29.9)) + theme(legend.position = "none",
                                                                      title = element_text(face="bold", size=14)) +
  ggtitle("Macquarie-Castlereagh (Tan/Pale Orange)")
p8

ggsave("Macquarie-Castlereagh map.png", dpi=600, width=21, units="cm")

p9 <- p1 + coord_sf(xlim=c(142.6, 149.5), ylim = c(-32.5, -29)) + theme(legend.position = "none",
                                                                      title = element_text(face="bold", size=14)) +
  ggtitle("Intersecting Streams (Red) & Barwon-Darling (Pink))")
p9

ggsave("Intersecting Streams and Barwon-Darling map.png", dpi=600, width=21, units="cm")

p10 <- p1 + coord_sf(xlim=c(148.5, 152), ylim = c(-30.8, -29)) + theme(legend.position = "none",
                                                                      title = element_text(face="bold", size=14)) +
  ggtitle("Gwydir (Brown)")
p10

ggsave("Gwydir map.png", dpi=600, width=21, units="cm")

