# - November 2023
# - Spiliopoulou K.


## GLOBAL PATTERNS OF KBAS AND PAS 
##	
##	1. Main paper plots
##
##	2. Supplementary information plots
##
##
#_____________________________________________________________________________________________________
#
## Libraries and Sources
#_____________________________________________________________________________________________________

#load packages
library(paletteer)
library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)
library(hrbrthemes)
library(ggpubr)

#wrorking directory
maindir<- ""

#_____________________________________________________________________________________________________
#
##  Analysis
#_____________________________________________________________________________________________________

  
# -----------------------------------------    • 1 •    ---------------------------------------------- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 1.1. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~
######### Distribution of KBA coverage values WORLD #############
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country")
d<- read.csv("2022.csv", sep = ',', dec = '.', header = TRUE)

ggplot(d,aes(x=percentage)) +
    geom_histogram(fill=cols[1], color= cols[1], binwidth = 2) +
	#geom_density(aes(y= after_stat(count)),fill="#69b3a2", color="#e9ecef", alpha=0.8) +
	labs(x="Percentage of each KBA covered by PAs",y="Number of KBAs") +
    theme_minimal_hgrid(12, rel_small = 1) +
		theme(
		axis.title.y = element_text(hjust = 0.5,vjust=2, size=30,
			margin = margin(r=0.2,l = 0.3, unit = "in")),
		axis.title.x = element_text(hjust=0.5,vjust=-2, size=30, 
			margin = margin(t=0.2,b = 0.3, unit = "in")),
		axis.text.x = element_text(size = 24),axis.text.y = element_text(size = 24),
		plot.margin = margin(1, 1, 1, 1, "cm"))
		#scale_x_continuous(expand = c(0.0,0), breaks=seq(0,100.5,25),limits = c(0, 100.5))+
		#scale_y_continuous(expand = c(0.0,0), breaks=seq(0,100.5,25),limits = c(0, 100.5))


#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 1.2. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~
########### Linear regression among the formulations ############

setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/indicator_value/country")
d<- read.csv("indicator_country(Sept2022)_2022.csv", sep = ',', dec = '.', header = TRUE)

	ggplot(d, aes(x=indicator_median, y=indicator_mean)) +
		geom_point(size= 3.5,color="#69b3a2") +
		labs(x=expression("I"["median"] ~ "(%)"),y=expression("I"["mean"] ~ "(%)")) +
		#theme_ipsum() +
		geom_smooth(method=lm , color="#E69F00", fill="#69b3a2", se=FALSE, size=2.5) +
		stat_cor(method = "kendall", cor.coef.name = "R", p.accuracy = 0.001, 
        label.x = 6, label.y = 88, label.sep = "\n", size=9.5) +
		theme_minimal_hgrid(12, rel_small = 1) +
		theme(
		axis.title.y = element_text(hjust = 1,vjust=2, size=39,
			margin = margin(r=0.2,l = 0.3, unit = "in")),
		axis.title.x = element_text(hjust=1,vjust=-2, size=39, 
			margin = margin(t=0.2,b = 0.3, unit = "in")),
		axis.text.x = element_text(size = 24),axis.text.y = element_text(size = 24),
		plot.margin = margin(1, 1, 1, 1, "cm"))+
		scale_y_continuous(expand = c(0.0,0))
		
	ggplot(d, aes(x=total_cov, y=indicator_mean)) +
		geom_point(size= 3.5,color="#69b3a2") +
		labs(x=expression("I"["total"] ~ "(%)"),y=" ") +
		#theme_ipsum() +
		geom_smooth(method=lm , color="#E69F00", fill="#69b3a2", se=FALSE, size=2.5) +
		stat_cor(method = "kendall", cor.coef.name = "R", p.accuracy = 0.001, 
        label.x = 6, label.y = 88, label.sep = "\n", size=9.5) +
		theme_minimal_hgrid(12, rel_small = 1) +
		theme(
		axis.title.y = element_text(hjust = 1,vjust=2, size=39,
			margin = margin(r=0.2,l = 0.3, unit = "in")),
		axis.title.x = element_text(hjust=1,vjust=-2, size=39, 
			margin = margin(t=0.2,b = 0.3, unit = "in")),
		axis.text.x = element_text(size = 24),axis.text.y = element_text(size = 24),
		plot.margin = margin(1, 1, 1, 1, "cm"))+
		scale_y_continuous(expand = c(0.0,0))
			
	ggplot(d, aes(x=n_complete_cov_97, y=indicator_mean)) +
		geom_point(size= 3.5,color="#69b3a2") +
		labs(x=expression("I"["complete"] ~ "(%)"),y=expression("I"["mean"] ~ "(%)")) +
		#theme_ipsum() +
		geom_smooth(method=lm , color="#E69F00", fill="#69b3a2", se=FALSE, size=2.5) +
		stat_cor(method = "kendall", cor.coef.name = "R", p.accuracy = 0.001, 
        label.x = 68, label.y = 13, label.sep = "\n", size=9.5) +
		theme_minimal_hgrid(12, rel_small = 1) +
		theme(
		axis.title.y = element_text(hjust = 1,vjust=2, size=39,
			margin = margin(r=0.2,l = 0.3, unit = "in")),
		axis.title.x = element_text(hjust=1,vjust=-2, size=39, 
			margin = margin(t=0.2,b = 0.3, unit = "in")),
		axis.text.x = element_text(size = 24),axis.text.y = element_text(size = 24),
		plot.margin = margin(1, 1, 1, 1, "cm"))+
		scale_y_continuous(expand = c(0.0,0), breaks=seq(0,100.5,25),limits = c(0, 100.5))

I5<- 100-d$distance
	ggplot(d, aes(x=I5, y=indicator_mean)) +
		geom_point(size= 3.5,color="#69b3a2") +
		labs(x=expression("I"["distance"] ~ "(%)"),y=" ") +
		#theme_ipsum() +
		geom_smooth(method=lm , color="#E69F00", fill="#69b3a2", se=FALSE, size=2.5) +
		stat_cor(method = "kendall", cor.coef.name = "R", p.accuracy = 0.001, 
        label.x = 6, label.y = 88, label.sep = "\n", size=9.5) +
		theme_minimal_hgrid(12, rel_small = 1) +
		theme(
		axis.title.y = element_text(hjust = 1,vjust=2, size=39,
			margin = margin(r=0.2,l = 0.3, unit = "in")),
		axis.title.x = element_text(hjust=1,vjust=-2, size=39, 
			margin = margin(t=0.2,b = 0.3, unit = "in")),
		axis.text.x = element_text(size = 24),axis.text.y = element_text(size = 24),
		plot.margin = margin(1, 1, 1, 1, "cm"))+
		scale_y_continuous(expand = c(0.0,0), breaks=seq(0,100.5,25),limits = c(0, 100.5))
		
#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 1.3. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~		
######## Relationship of KBA coverage and KBA area WORLD ########
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country")
d<- read.csv("2022.csv")
kba_area_log<- log(d$area_kbas)

ggplot(d, aes(x=kba_area_log, y=percentage)) +
		geom_point(size= 1.5,color="#69b3a2") +
		labs(x=expression("log(KBA extent in km"^2~")"),y="% of each KBA covered \nby protected areas") +
		#theme_ipsum() +
		# Minimal grid theme that only draws horizontal lines
		theme_minimal_hgrid(12, rel_small = 1) +
		theme(
		axis.title.y = element_text(hjust = 0.5,vjust=2, size=26,
			margin = margin(r=0.2,l = 0.3, unit = "in")),
		axis.title.x = element_text(hjust=0.5,vjust=-2, size=26, 
			margin = margin(t=0.2,b = 0.3, unit = "in")),
		axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20),
		plot.margin = margin(1, 1, 1, 1, "cm"))+
		scale_y_continuous(expand = c(0.0,0), breaks=seq(0,100.5,25),limits = c(0, 100.5))
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 1.4. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~	
######## Correlation matrix of complete and non-complete ########












#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 1.5. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~	
########                     Distance                    ########
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/indicator_value/country")
d<- read.csv("indicator_country(Sept2022)_2022.csv", sep = ',', dec = '.', header = TRUE)

d1<-d%>%
#drop_na()%>%
#select(SitRecID, country,percentage_SitRecID)%>%
unique()%>%
arrange(Country)%>%
group_by(Country)%>%
#mutate(N_KBA=length(SitRecID))%>%
filter(KBAs>=10)

#add in an ideal country
i<-as.data.frame(10)
i$Country<-c("ideal")
i$indicator_mean<-as.numeric(100)
i$KBAs<-10
i$ISO3<-"III"
i$SD<- as.numeric(0)
i$Mean<- as.numeric(100)

d1<-rbind(d1, i)

#make plot
dist<- 100-d1$distance
x_limits<- c(50,NA)

ggplot(d1,aes(x=Mean, y=SD, color=dist))+
	geom_point(size=4,	alpha=0.8, stroke = 2)+
	scale_colour_gradient(low=lighten("#69b3a2",amount =0.2), high=darken("#69b3a2",amount =0.7)) +
	geom_text_repel(aes(x=Mean, y=SD, label = ISO3),max.overlaps = 11, size = 7)+
	labs(x="Mean % coverage of KBAs by protected areas", y = "Standard Deviation") +
	theme_minimal_hgrid(12, rel_small = 1) +
	theme(aspect.ratio=0.5,
	legend.title = element_text(size = 17),
	legend.text = element_text(size = 15),
	legend.key.size = unit(0.5, "cm"),
	legend.key.width = unit(1.7,"cm"),
	legend.direction="horizontal", legend.position=c(0.65,1),
	axis.title.y = element_text(hjust = 0.5,vjust=2, size=30,
		margin = margin(r=0.2,l = 0.3, unit = "in")),
	axis.title.x = element_text(hjust=0.5,vjust=-2, size=30, 
		margin = margin(t=0.2,b = 0.3, unit = "in")),
	axis.text.x = element_text(size = 25),axis.text.y = element_text(size = 25),
	plot.margin = margin(1, 1, 1, 1, "cm"))+
	
	guides(col = guide_colourbar(title = "Distance")) +
	scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
	scale_y_continuous(expand = c(0.0,0),breaks=seq(0,60,20),limits = c(0, 65))


#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 1.6. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~
#########      Trends over time for all formulations    #########
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/indicator_value/world")
d<- read.csv("indicator_world(Sept2023).csv")
d<- d[-14,]
d<- d %>% gather(key=formulations, value=value, c(indicator_mean, indicator_median, 
	total_cov, n_complete_cov_97, I),factor_key=FALSE)
# Hide all of the text labels.
d$text_label <- ""
# Let's just label these items.
ix_label <- c(13, 26, 39, 52, 65)
d$text_label[ix_label] <- c("Imean", "Imedian","Iarea","Icovered","Icompleteness")


plot<- ggplot(d, aes(y=value, x=year, color=formulations, fill= formulations)) +
	geom_line(size=2) +
    geom_point(shape=21, size=6) +
	
	coord_cartesian(clip = "off") +
	geom_text_repel(
    aes(color = formulations, 
	label = text_label),
	#c(expression("I"["mean"]), expression("I"["median"]),expression("I"["total"]),
	#	expression("I"["complete"]),expression("I"["distance"]))),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2024, NA),
    hjust = 0,
    segment.size = 1.4,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20) +
	geom_point(size=0.001,fill = ifelse(d$text_label == "", "grey50", "black"))+
	
	paletteer::scale_fill_paletteer_d("ggsci::default_jama",name=" ",
	breaks=c("indicator_mean", "indicator_median","total_cov", "n_complete_cov_97","I"))+
	paletteer::scale_color_paletteer_d("ggsci::default_jama",name=" ",
	breaks=c("indicator_mean", "indicator_median","total_cov", "n_complete_cov_97","I"))+
   
	guides(color = "none", fill="none") +
	
	theme_minimal_hgrid(12, rel_small = 1) +
	labs(x="Year",y="Indicator value (%)") + 
	
	theme(
		axis.title.y = element_text(size=30,hjust = 0.5,vjust=2,
			margin = margin(r=0.2,l = 0.3, unit = "in")),
		axis.title.x = element_text(size=30, hjust=0.5,vjust=-2, 
			margin = margin(t=0.2,b = 0.3, unit = "in")),
		axis.text.x = element_text(size = 24),axis.text.y = element_text(size = 24),
		plot.margin = margin(1, 6, 1, 1, "cm"),
		aspect.ratio=0.5) +
	scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022))+
	scale_y_continuous(expand = c(0.0,0),breaks=seq(0,65,10),limits = c(0, 65))
	
grDevices::png(filename = "trends.png", res = 330)
grDevices::dev.off()


# -----------------------------------------    • 2 •    ---------------------------------------------- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 2.1. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~
#########  Distribution of KBA coverage values COUNTRY ##########
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country")
d<- read.csv("2022_names.csv")

iso3<- unique(d$ISO3)
iso3<- split(iso3, rep(1:4, length.out = length(iso3), each = ceiling(length(iso3)/4)))
cols<- c("#69b3a2","#e9ecef")


plots<- list()

for (i in 1:length(iso3[[1]])){
dc<- filter(d, ISO3==iso3[[1]][i])

 plot<- ggplot(dc,aes(x=percentage)) +    
	geom_histogram( fill=cols[1], color= cols[1], binwidth = 2) +
	#geom_density(aes(y= after_stat(count)),fill="#69b3a2", color="#e9ecef", alpha=0.6) +
	labs(title=unique(dc$Country)) +
    theme_minimal_hgrid(12, rel_small = 1) +
	theme(
		plot.title=element_text(size=7.5, hjust=0),
		axis.title.x=element_blank(),
        axis.title.y=element_blank(),
		axis.text.x = element_text(size = 5.6),axis.text.y = element_text(size = 5.6))
		
df<- ggplot_build(plot)$data[[1]]

plots[[i]]<- plot + ylim(0,max(df$count))
}

grDevices::png(filename = "distribution1.png", width = 2900, height = 4200,
                 res = 330)
 figure<- ggarrange(plotlist=plots, ncol = 6, nrow = 10)+
  theme(plot.margin = margin(0,0,0.4,0.4, "cm")) 
 annotate_figure(figure,
    bottom = text_grob("% of each KBA covered by protected areas", size = 14,hjust=0.5,vjust=0.1),
    left = text_grob("Number of KBAs", size = 14,rot = 90,hjust = 0,vjust=0.8))

grDevices::dev.off()
	

#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 2.2. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Relationship of KBA coverage and KBA area COUNTRY #######
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country")
d<- read.csv("2022_names.csv")

iso3<- unique(d$ISO3)
iso3<- split(iso3, rep(1:4, length.out = length(iso3), each = ceiling(length(iso3)/4)))

plots<- list()

for (i in 1:length(iso3[[4]])){
dc<- filter(d, ISO3==iso3[[4]][i])
dc$kba_area_log<- log(dc$area_kbas)

#fit lm model 
lmfit<- lm(percentage ~ kba_area_log, data = dc)
#get coefficients
if(nrow(dc)<2){
pvalue<-0
rsq<- 0
}else{
pvalue<- summary(lmfit)$coefficients[2,4]
rsq<- summary(lmfit)$r.squared
}

if(pvalue=="NaN") pvalue<-0
if(rsq=="NaN") rsq<-0

if(pvalue<=0.05 & rsq>0.4) {
plots[[i]]<- ggplot(dc, aes(x=kba_area_log, y=percentage)) +
		geom_point(size= 1,color="#69b3a2") +
		labs(title=unique(dc$Country)) +
		stat_cor(method = "kendall", cor.coef.name = "R", p.accuracy = 0.001, 
        label.x.npc = "left",label.y= 75,family="bold",label.sep = "\n", size=2.5, color="black") +
		geom_smooth(method=lm , color="#E69F00", fill="#69b3a2", se=FALSE, size=0.8) +
		#theme_ipsum() +
		# Minimal grid theme that only draws horizontal lines
		theme_minimal_hgrid(12, rel_small = 1) +
		theme(
		plot.title=element_text(size=7.5, hjust=0),
		axis.title.x=element_blank(),
        axis.title.y=element_blank(),
		axis.text.x = element_text(size = 5.6),axis.text.y = element_text(size = 5.6))+
		scale_y_continuous(expand = c(0.0,0), breaks=seq(0,100.5,25),limits = c(0, 100.5))
} else{
plots[[i]]<- ggplot(dc, aes(x=kba_area_log, y=percentage)) +
	geom_point(size= 1,color="#69b3a2") +
	labs(title=unique(dc$Country)) +
	theme_minimal_hgrid(12, rel_small = 1) +
	theme(
	plot.title=element_text(size=7.5, hjust=0),
	axis.title.x=element_blank(),
    axis.title.y=element_blank(),
	axis.text.x = element_text(size = 5.6),axis.text.y = element_text(size = 5.6))+
	scale_y_continuous(expand = c(0.0,0), breaks=seq(0,100.5,25),limits = c(0, 100.5))
}
}

grDevices::png(filename = "area4.png", width = 2900, height = 4200,
                 res = 330)
 figure<- ggarrange(plotlist=plots, ncol = 6, nrow = 10)+
  theme(plot.margin = margin(0,0,0.4,0.4, "cm")) 
 annotate_figure(figure,
    bottom = text_grob(expression("Extent of each KBA (log km"^2~")"), size = 14,hjust=0.5,vjust=0.1),
    left = text_grob("% of each KBA covered by protected areas", size = 14,rot = 90,hjust = 0.5,vjust=1.2))
grDevices::dev.off()


###################################################################################################
###################################################################################################