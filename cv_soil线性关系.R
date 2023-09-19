library(openxlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(cowplot)
library(Cairo)
library(RColorBrewer)

SEMdata <- read.csv("O:\\china_biomass\\SEMdata_finaldata425.csv")
SEMdata <- read.xlsx("O:\\china_biomass\\SEMdata_finaldata425.xlsx",sheet=1)
#加入gracewater
gracewater <- read.csv("O:\\china_biomass\\gracewater_2013_2017.csv")
colnames(gracewater)[2] <- 'gracewater'
SEMdata <- left_join(SEMdata,gracewater,by="ID")
write.csv(SEMdata,file="O:\\china_biomass\\SEMdata_gracewater.csv",row.names = FALSE)

my.formula <- y ~ poly(x,1,raw=TRUE)
SEMdata[42] <- 1
colnames(SEMdata)[42] <- "climate_zone"
SEMdata$climate_zone[SEMdata$湿地区 == '东部滨海区'] <- "Coastal region"
SEMdata$climate_zone[SEMdata$湿地区 == '热带亚热带湿润区'] <- 'Subtropical humid zone'
SEMdata$climate_zone[SEMdata$湿地区 == '温带湿润半湿润区'] <- "Temperate humid and semi-humid zone"
SEMdata$climate_zone[SEMdata$湿地区 == '温带干旱半干旱区'] <- "Temperate arid and semi-arid zone"
SEMdata$climate_zone[SEMdata$湿地区 == '西南高原区'] <- "Tibetan Plateau region"


####PH####
brewer.pal.info
sfcolor <- brewer.pal(name="PiYG",8)#PiYG  RdBu
linecolor <- brewer.pal(name="Blues",3)#PiYG Greys
p1 <- ggplot(SEMdata, aes(x = PH, y = cv)) + #"#FDCDAC"
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+# name = "Spe")+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed","#558ebd"
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = c(0.2,0.6))+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("pH (*10)")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p1
####N####
p2 <- ggplot(SEMdata, aes(x = N, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = "bottom")+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("TN (cg/kg)")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p2
####SOC####
p3 <- ggplot(SEMdata, aes(x = soc, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = c(1.5,1.5))+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("SOC (dg/kg)")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p3
####拼图####
plantplot <- plot_grid(
  p1+theme(legend.position = 'none'),
  p2+theme(legend.position = 'none'),
  p3+labs(colour = "Climate zones")+theme(legend.position = c(1.65, 0.5),
           legend.title = element_text(family = "serif",face = "bold",size = 16),
           legend.text = element_text(family = "serif",size = 14)),
  labels = c(
    "A","B","C",
    label_size = 12,label_fontfamily = "serif",#label_x = 0.85,label_y = 0.95,
    nrow = 2,align='h'))#LETTERS[1:6]
plantplot
ggsave("O:\\china_biomass\\出图\\relationship_soil.pdf",plantplot,
       width = 20, height = 20, units = "cm")

####altitude####
brewer.pal.info
sfcolor <- brewer.pal(name="RdYlGn",5)#PiYG  RdBu
linecolor <- brewer.pal(name="Blues",3)#PiYG Greys
p4 <- ggplot(SEMdata, aes(x = altitude, y = cv)) + #"#FDCDAC"
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+# name = "Spe")+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed","#558ebd"
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = c(0.2,0.6))+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("Altitude (m)")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p4
####lat####
p5 <- ggplot(SEMdata, aes(x = lat, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = "bottom")+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("LAT")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p5
####lon####
p6 <- ggplot(SEMdata, aes(x = lon, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = "bottom")+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("LON")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p6
####MAT####
SEMdata[,43] <- SEMdata$temp_mean*0.1
colnames(SEMdata)[43] <- "MATmut"
p7 <- ggplot(SEMdata, aes(x = MATmut, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = "bottom")+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab(expression(paste("MAT (",degree,"C)")))+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p7
####MAP####
SEMdata[,44] <- SEMdata$pre_mean*0.1
colnames(SEMdata)[44] <- "MAPmut"
p8 <- ggplot(SEMdata, aes(x = MAPmut, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = "bottom")+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("MAP (mm)")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p8
####MAT sd####
SEMdata[,45] <- SEMdata$temp_std*0.1
colnames(SEMdata)[45] <- "MAT_STDmut"
p9 <- ggplot(SEMdata, aes(x = MAT_STDmut, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = "bottom")+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab(expression(paste("Temp_SD (",degree,"C)")))+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p9
####MAP sd####
SEMdata[,45] <- SEMdata$pre_std*0.1
colnames(SEMdata)[45] <- "MAP_STDmut"
p10 <- ggplot(SEMdata, aes(x = MAP_STDmut, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = "bottom")+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("Prec_SD (mm)")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p10
####water####
p11 <- ggplot(SEMdata, aes(x = gracewater, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = "bottom")+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("EWT (cm)")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
p11
####拼图####
plantplot <- plot_grid(
  p1+theme(legend.position = 'none'),
  p4+theme(legend.position = 'none'),
  p7+theme(legend.position = 'none'),
  p9+theme(legend.position = 'none'),
  p2+theme(legend.position = 'none'),
  p5+theme(legend.position = 'none'),
  p8+theme(legend.position = 'none'),
  p10+theme(legend.position = 'none'),
  p3+theme(legend.position = 'none'),
  p6+theme(legend.position = 'none'),
  p11+labs(colour = "Climate zones")+
  #   p1+theme(legend.position = 'none'),
  # p2+theme(legend.position = 'none'),
  # p3+theme(legend.position = 'none'),
  # p4+theme(legend.position = 'none'),
  # p5+theme(legend.position = 'none'),
  # p6+theme(legend.position = 'none'),
  # p7+theme(legend.position = 'none'),
  # p8+theme(legend.position = 'none'),
  # p9+theme(legend.position = 'none'),
  # p10+theme(legend.position = 'none'),
  # p11+labs(colour = "Climate zones")+
    theme(legend.position = c(1.7, 0.5),#legend.direction = "horizontal",
       legend.title = element_text(family = "serif",face = "bold",size = 14),
       legend.text = element_text(family = "serif",size = 11)),
  labels = c(
    "A","D","G","J","B","E","H","K","C","F","I",
    # "A","B","C","D","E","F","G","H","I","J","K",
    label_size = 12,label_fontfamily = "serif",#label_x = 0.85,label_y = 0.95,
    nrow = 3,align='h'))#LETTERS[1:6]
plantplot
ggsave("O:\\china_biomass\\出图\\relationship815.jpg",plantplot,
       width = 30, height = 25, units = "cm")

#############EVI-water####
alldata <- read_xls("O:\\china_biomass\\多样性和cv_new.xls")
alldata <- alldata[,c(2,26:32)]
waterdata <- read.csv("O:\\china_biomass\\gracewater_2010_2016.csv")
waterdata <- waterdata[,c(2:9)]
pdata <- left_join(alldata,waterdata,by="ID")

a1 <- ggplot(pdata, aes(x = gracewater, y = cv)) + #
  geom_point(size=3.5,alpha=0.5,aes(colour=climate_zone))+
  scale_color_manual(values = sfcolor[c(1,2,3,7,8)])+
  theme_get()+
  geom_smooth(method = "glm",
              color=linecolor[3],
              fill="lightgray",#linetype="dashed",
              alpha=.6,
              linewidth=2,se=T,level=0.95,  #置信区间
              formula = my.formula)+
  ##图例
  # guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
  #        fill=guide_legend(title=NULL))+
  # theme(legend.position = "bottom")+
  theme(legend.position = 'none')+
  theme(axis.title.y = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.title.x = element_text(family = "serif",colour = "black",size = 18))+
  theme(axis.text.x = element_text(family = "serif",colour = "black",size = 14))+
  theme(axis.text.y = element_text(family = "serif",colour = "black",size = 14))+
  # ylab("WCA (ha)")+
  ylab("Ecosystem stability")+
  xlab("EWT (cm)")+
  ##R2  ..eq.label..,  ,..eq.label..,..rr.label..
  stat_poly_eq(
    aes(label =paste( ..eq.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.85)+
  stat_poly_eq(
    aes(label =paste( ..rr.label..,..p.value.label.., sep = "~~~")),
    formula = my.formula,  parse = TRUE,
    family="serif",size = 4,color="black",
    label.x = 0.1,label.y = 0.78)+
  # facet_grid(.~"Spartina alterniflora")+  #, switch = "x"
  theme(strip.text.x = element_text(family = "serif",size = 14))
a1





