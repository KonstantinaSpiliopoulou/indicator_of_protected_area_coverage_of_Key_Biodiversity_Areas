
## INDICATOR FORMULATION EUCLIDEAN DISTANCE 
##	
##	1. Import KBAs, PAs and countries.
##
##	2. Fix ISO3.
##
##	3. Dissolve PAs
##		
##	4. Calculate overlap of KBAs per country and generate single csv for KBAs' overlap
##
##	5. Calculate indicator
##		5.1. per country
##		5.2. per continent
##		5.3. worldwide
##
##
#_____________________________________________________________________________________________________

## Libraries and Sources
#_____________________________________________________________________________________________________

#check if pacman is installed
if("pacman" %in% rownames(installed.packages()) == FALSE) {
					message("installing the 'pacman' package")
					install.packages("pacman")}

#load packages					
packages<- c("dplyr", "foreign", "sf", "tidyverse", "lwgeom", "ggpubr", "doParallel",
				"foreach", "snow")
pacman::p_load(packages, character.only = TRUE)

#wrorking directory
maindir<- "C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/"

#remove scientific notation
options(scipen=999)

#_____________________________________________________________________________________________________
#
##  Analysis
#_____________________________________________________________________________________________________

  
# -----------------------------------------    • 1 •    ---------------------------------------------- 
#import continent data
continent<- read.csv(paste0(maindir, "input/country_continent_codes.csv"), stringsAsFactors=FALSE) %>%
	select(Three_Letter_Country_Code, Continent_Name) %>% rename(Continent = Continent_Name,
	ISO3 = Three_Letter_Country_Code)

#import kbas
kbas<- st_read(paste0(maindir,"input/KBAs_Sept2021_02.gpkg"),stringsAsFactors=FALSE) %>% 
	st_transform(4326) %>% st_set_precision(1000000) %>% st_make_valid() %>% 
	select(SitRecID, ISO3, Country, IbaStatus, LegacyKBA, AzeStatus, AddedDate, ChangeDate,
	KBA_Qual) %>% left_join(., continent)
kbas$area_kbas<- as.numeric(units::set_units(st_area(kbas), km^2))

#import pas
pas<- st_read(paste0(maindir,"input/WDPA_WDOECM_Feb2022_all.gpkg"),stringsAsFactors=FALSE) %>% 
	st_transform(4326) %>% st_set_precision(1000000) %>% st_make_valid() %>% 
	select(WDPAID, WDPA_PID, ISO3, STATUS, STATUS_YR) %>% left_join(., continent)
pas$area_pas<- as.numeric(units::set_units(st_area(pas), km^2))


# -----------------------------------------    • 2 •    ----------------------------------------------
#fix pas_global ISO3 codes
pas$ISO3<- plyr::mapvalues(pas$ISO3, 
			from=c("ALA","ASC","CPT","GGY","IMN","JEY","TAA","WAK","XAD","XKO","XNC"), 
			  to=c("FIN","SHN","FRA","GBR","GBR","GBR","SHN","UMI","CYP","SRB","CYP"))
unassigned_pas <- pas[pas$ISO3 == " " | is.na(pas$ISO3) | pas$ISO3 == '---',]
ifelse(nrow(unassigned_pas)==0, 
		"####    PAs are good to go    ####", 
		"####    PROBLEM: Check unassigned_pas for PAs that need fixing    ####")

#remove disputed (cannot be assigned to a country) kbas
kbas<- kbas[kbas$Country != 'Disputed',] 
#fix kbas_global ISO3 codes 
iso3_fix<-	data.frame(Country=c("Palau","Aruba (to Netherlands)","Libya","Belarus",
			"Guadeloupe (to France)","Norfolk Island (to Australia)","Russia (Asian)","Laos","India",
			"Cuba","High Seas","Falkland Islands (Malvinas)"), 
			ISO3=c("PLW","ABW","LBY","BLR","GLP","NFK","RUS","LAO","IND","CUB","ABNJ","FLK"))
fixed_kbas<- kbas[which(kbas$Country %in% iso3_fix$Country),] %>% 
			select(-ISO3) %>% full_join(.,iso3_fix)
kbas<- kbas[-which(kbas$Country %in% iso3_fix$Country),]
kbas<- rbind(kbas,fixed_kbas)
kbas_toFix<- filter(kbas, ISO3 %in% c("---", " ") | is.na(ISO3) | is.na(Country) | 
			Country== "NA" | Country=="")
ifelse(nrow(kbas_toFix)==0, "####    KBAs are good to go    ####", 
							"####    PROBLEM: Check kbas_toFix for KBAs that need fixing    ####")


# -----------------------------------------    • 3 •    ----------------------------------------------
#create folders to store outputs
for(d in c("pas_noOverlaps/","kbas_overlap_country/")){
for(i in 2010:2022){
dir.create(file.path(maindir, "output/", d,i), showWarnings = FALSE)
}}

#select countries to use in the analysis
iso3<- unique(test$ISO3)
iso3<- c("CRI","DOM","IND","NAM","UGA")# 2020
d<- c("HUN","IND","KHM","LBN","MTQ","NIC")#2021
c("CHL", "GBR", "NCL","NLD","PAN","POL","SYC","TZA")

c("ARG", "BES","BGD","CHN","CMR","COD","DZA","ECU","EGY","FIN","FJI","FRO","GAB","GNB","HND","IRL","LKA","LUX",
	"MDG","MSR","MYT","PHL","PLW","PNG","PRT","PYF","REU","SEN","TLS")

year<- as.character(2020)

##Create and check cluster
cl <- makeSOCKcluster(5)
registerDoParallel(cl)
clusterEvalQ(cl, {
library('doParallel') 
"Connected"
})

strt<-Sys.time()
foreach(y=year) %:%
#loop throught 
foreach(i=1:length(iso3), .packages= c('sf',"dplyr", "tidyverse")) %dopar% {
###test no parallel loop
###for (i in 1:length(iso3)){
sf::sf_use_s2(FALSE)
#set working directory	
setwd(paste0(maindir,"output/pas_noOverlaps/"))

## Create cases for having dissolved PAs in 2010 or not
if(file.exists(paste0("2010/",iso3[i],".gpkg"))==TRUE){
#if(file.exists(paste0(y,"/",iso3[i],".gpkg"))==TRUE) next
	pac<- pas %>% filter(ISO3== iso3[i] & STATUS_YR %in% c("2011":y)) %>% st_make_valid() 
	
	
	if(nrow(pac) %in% 0 ==TRUE){ 
		temp<- st_read(paste0(maindir,"output/pas_noOverlaps/2010/",iso3[i],".gpkg"),
			stringsAsFactors=FALSE)
		st_write(temp,paste0(maindir,"output/pas_noOverlaps/",y,"/",iso3[i],".gpkg"),
			row.names=FALSE, delete_dsn = TRUE)	
	}# end of if 1
		
	if("GEOMETRYCOLLECTION"%in% st_geometry_type(pac)== TRUE){ #start of if
	gem<- st_cast(pac)[which(st_is(st_cast(pac), "GEOMETRYCOLLECTION")),]
	test<- gem %>% st_buffer(., 0.0) %>% st_make_valid() %>% 
		st_cast("MULTIPOLYGON")
	pac<-st_cast(pac)[which(!st_is(st_cast(pac), "GEOMETRYCOLLECTION")),]
	pac<- rbind(pac,test)
	}# end of if 2
	
	temp<- st_read(paste0(maindir,"output/pas_noOverlaps/2010/",iso3[i],".gpkg"),
		stringsAsFactors=FALSE)
	pac<- pac %>% bind_rows(., temp) %>% 
		st_union() %>% st_sf()	%>% st_make_valid()
	#create output for pas with no overlapings per country
	st_write(pac,paste0(maindir,"output/pas_noOverlaps/",y,"/",iso3[i],".gpkg"),
		row.names=FALSE, delete_dsn = TRUE)
	# release memory
	gc()

	
}else{

#if(file.exists(paste0(y,"/",iso3[i],".gpkg"))==TRUE) next
	#filter pas and control for overlappings  
	pac<- pas %>% filter(ISO3== iso3[i] & STATUS_YR<= y) %>% st_make_valid()
	if(nrow(pac) %in% 0 ==TRUE){ 
	results<- data.frame(indicator= 0) 
	write.csv(results,paste0(maindir,"output/kbas_overlap_country/",y,"/",iso3[i],".csv"),
		row.names=FALSE)
	
	}else{
		
		if("GEOMETRYCOLLECTION"%in% st_geometry_type(pac)== TRUE){ #start of if
		gem<- st_cast(pac)[which(st_is(st_cast(pac), "GEOMETRYCOLLECTION")),]
		test<- gem %>% st_buffer(., 0.0) %>% st_make_valid() %>% 
			st_cast("MULTIPOLYGON")

		pac<-st_cast(pac)[which(!st_is(st_cast(pac), "GEOMETRYCOLLECTION")),]
		pac<- rbind(pac,test)
		}# end of if 2
	
	pac<- pac %>% st_union() %>% st_sf() %>% st_make_valid()
	#create output for pas with no overlapings per country
	st_write(pac,paste0(maindir,"output/pas_noOverlaps/",y,"/",iso3[i],".gpkg"),
		row.names=FALSE, delete_dsn = TRUE)
	}# end of if 1
	# release memory
	gc()

}# end of else
# release memory
gc()
}# end of foreach

#Shut down cluster and check time
stopCluster(cl)
print(Sys.time()-strt)


# -----------------------------------------    • 4 •    ----------------------------------------------
#select countries to use in the analysis
iso3<- unique(kbas$ISO3)
year<- as.character(2010:2022)
#find what is not yet complete
data<- list()
for (i in year){
setwd(paste0("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/pas_noOverlaps/",i))
files1<- list.files() %>% gsub("*.gpkg","",.)
setwd(paste0("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country/",i))
files2<- list.files() %>% gsub("*.csv","",.)
files<- files1[!files1 %in% files2]
data[[i]]<- files
}
#format data
for (i in 1:length(data)){
data[[i]]<-paste0(names(data[i]),"/", data[[i]])
}
data<- unlist(data)

data<- list()
for (i in 1:length(year)){
data[[i]]<- paste0(year[i], "/", iso3)
}
data<- unlist(data)



##Create and check cluster
cl <- makeSOCKcluster(11)
registerDoParallel(cl)
clusterEvalQ(cl, {
library('doParallel') 
"Connected"
})


strt<-Sys.time()
#loop throught 
foreach(i=data, .packages= c('sf',"dplyr", "tidyverse")) %dopar% {
sf::sf_use_s2(FALSE)
gc()
#set working directory	
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/pas_noOverlaps/")

#if(file.exists(paste0(y,"/",iso3[i],".gpkg"))!=TRUE) next
#import pas
pac<- st_read(paste0(i,".gpkg"), stringsAsFactors=FALSE) %>% st_transform(4326) %>% 
st_set_precision(1000000) %>% st_make_valid()
	
	if("GEOMETRYCOLLECTION"%in% st_geometry_type(pac)== TRUE){ #start of if
	gem<- st_cast(pac)[which(st_is(st_cast(pac), "GEOMETRYCOLLECTION")),]
	test<- gem %>% st_buffer(., 0.0) %>% st_make_valid() %>% st_cast("MULTIPOLYGON")
	pac<-st_cast(pac)[which(!st_is(st_cast(pac), "GEOMETRYCOLLECTION")),]
	pac<- rbind(pac,test)
	}# end of if

#filter kbas
kbac<- kbas %>% filter(ISO3 == str_split(i, "/")[[1]] [2])

#intersect KBAs and PAs and calculate area of overlap
	sf::sf_use_s2(FALSE)
	overlap<- st_intersection(st_make_valid(kbac),pac) 
		if(nrow(overlap)==0) {
		results<- kbac %>% st_drop_geometry() %>% mutate(area_overlap=0,percentage= 0)
		write.csv(results,paste0(
		"C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country/",i,".csv"),
		row.names=FALSE)
		} else {
		overlap$area_overlap<- as.numeric(units::set_units(st_area(overlap), km^2))
		#calculate percentage(%) of overlap 
		overlap<- overlap %>% st_drop_geometry() %>% mutate(percentage= area_overlap*100/area_kbas)
		#create output for % overlap for each kba per country
		write.csv(overlap,paste0(
		"C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country/",i,".csv"),
		row.names=FALSE)
		}
gc()
}

#Shut down cluster and check time
stopCluster(cl)
print(Sys.time()-strt)


# -----------------------------------------    • 5 •    ----------------------------------------------
year<- as.character(2010:2022)
kbas<- st_drop_geometry(kbas) %>% mutate(area_overlap=0,percentage=0)

#import kbas
for (i in year){

#load files
setwd(paste0(maindir,"output/kbas_overlap_country/",i))
files<- list.files(pattern= "*.csv")
kbas_overlap<- lapply(files,function(x) read.csv(x, stringsAsFactors=FALSE))
names(kbas_overlap)<- gsub("*.csv","",files)
	
	#correct files 1
	for (z in 1:length(kbas_overlap)){
		if (ncol(kbas_overlap[[z]])==1){
			kbas_overlap[[z]]<- kbas %>% filter(ISO3 == names(kbas_overlap[z])) %>% 
			mutate(area_overlap=0,percentage=0)
		}
	}
	#correct files 2
	for (c in 1:length(kbas_overlap)){
	temp<- filter(kbas, ISO3 == unique(kbas_overlap[[c]]$ISO3)) %>% 
		filter(!SitRecID %in% kbas_overlap[[c]]$SitRecID)
	kbas_overlap[[c]]<- rbind(kbas_overlap[[c]],temp)
	}

#make list to dataframe
temp<- do.call(rbind,kbas_overlap)
#write output	
write.csv(temp, paste0(maindir, "output/kbas_overlap_country/",i,".csv"), row.names=FALSE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 5.1. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~
#calculate indicator per country
results<- list()

for (i in 1:length(year)){
#set working directory
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country/")

# import annual kba overlap
kbas_overlap<- read.csv(paste0(year[i],".csv"),stringsAsFactors=FALSE) %>% 
	dplyr::select(-Continent) %>% left_join(continent) %>% unique()
# correct data
kbas_overlap$percentage[kbas_overlap$percentage > 100] <- 100 
kbas_overlap$Country[which(kbas_overlap$ISO3=="RUS")]<- "Russia"

#add ideal country
ideal<-data.frame(SitRecID= 999901:999910,ISO3="III", Country="ideal", IbaStatus= NA,
	LegacyKBA=NA, AzeStatus=NA, AddedDate=NA, ChangeDate=NA, KBA_Qual=NA, Continent="ideal",
	area_kbas=NA, area_overlap=100, percentage=100)
kbas_overlap<- rbind(kbas_overlap, ideal) %>% unique()

#get country iso3
iso3<- unique(kbas_overlap$ISO3)

temp<- list()
for (z in 1:length(iso3)){
#loop through countries
df<- kbas_overlap %>% filter(ISO3==iso3[z]) %>% group_by(SitRecID) %>% 
	mutate(percentage_SitRecID= sum(percentage))
	
complete_cov_100<- df %>% filter(percentage_SitRecID>=100)	
complete_cov_99<- df %>% filter(percentage_SitRecID>=99)	
complete_cov_98<- df %>% filter(percentage_SitRecID>=98)
complete_cov_97<- df %>% filter(percentage_SitRecID>=97)
complete_cov_96<- df %>% filter(percentage_SitRecID>=96)
complete_cov_95<- df %>% filter(percentage_SitRecID>=95)

complete_uncov_0<- df %>% filter(percentage_SitRecID<=0)
complete_uncov_1<- df %>% filter(percentage_SitRecID<=1)
complete_uncov_2<- df %>% filter(percentage_SitRecID<=2)
complete_uncov_3<- df %>% filter(percentage_SitRecID<=3)
complete_uncov_4<- df %>% filter(percentage_SitRecID<=4)
complete_uncov_5<- df %>% filter(percentage_SitRecID<=5)

ind<- df %>% select(SitRecID,percentage_SitRecID) %>% unique()
	
temp[[z]]<- data.frame(
	ISO3= unique(df$ISO3),
	Country= unique(df$Country),
	Continent= unique(df$Continent),
	KBAs= length(unique(df$SitRecID)),
	Mean= mean(ind$percentage_SitRecID),
	SD=sd(ind$percentage_SitRecID),
	
	indicator_mean= sum(ind$percentage_SitRecID)/length(unique(df$SitRecID)), 
	indicator_median= median(ind$percentage_SitRecID),
	total_cov= sum(df$area_overlap)*100/sum(unique(df$area_kbas)),
	
	n_complete_cov_100= 
		length(unique(complete_cov_100$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_99= 
		length(unique(complete_cov_99$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_98= 
		length(unique(complete_cov_98$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_97= 
		length(unique(complete_cov_97$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_96= 
		length(unique(complete_cov_96$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_95= 
		length(unique(complete_cov_95$SitRecID))*100/length(unique(df$SitRecID)),
		
	n_complete_ucov_0= 
		length(unique(complete_uncov_0$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_1= 
		length(unique(complete_uncov_1$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_2= 
		length(unique(complete_uncov_2$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_3= 
		length(unique(complete_uncov_3$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_4= 
		length(unique(complete_uncov_4$SitRecID))*100/length(unique(df$SitRecID)),		
	n_complete_cov_5= 
		length(unique(complete_uncov_5$SitRecID))*100/length(unique(df$SitRecID)),	

	stringsAsFactors=FALSE)
}
results[[i]]<- do.call(rbind,temp) %>% arrange(Mean) %>% mutate(SD = ifelse(is.na(SD), 0, SD))

# get distance
temp2<- results[[i]] %>% dplyr::select(-Continent) %>% unique() %>%
	magrittr::set_rownames(.$ISO3) %>% dplyr::select(Mean, SD)
temp3<- as.data.frame(as.matrix(dist(temp2,method="euclidean"))) %>% mutate(ISO3= row.names(temp2)) %>%
	pivot_longer(-c(ISO3), names_to="to", values_to="distance") %>% filter(to=="III") %>% as.data.frame() %>%
	dplyr::select(-to)

#create final output
final<- left_join(results[[i]],temp3)

write.csv(final, 
	paste0(maindir,"output/indicator_value/country/indicator_country(Sept2022)_",year[i],".csv"), 
	row.names=FALSE)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 5.2. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate indicator per continent
results<- list()

for (i in 1:length(year)){
#set working directory
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country/")

# import annual kba overlap
kbas_overlap<- read.csv(paste0(year[i],".csv"),stringsAsFactors=FALSE) %>% 
	dplyr::select(-Continent) %>% left_join(continent) %>% unique()
# correct data
kbas_overlap$percentage[kbas_overlap$percentage > 100] <- 100 
kbas_overlap$Country[which(kbas_overlap$ISO3=="RUS")]<- "Russia"

#add ideal country
ideal<-data.frame(SitRecID= 999901:999910,ISO3="III", Country="ideal", IbaStatus= NA,
	LegacyKBA=NA, AzeStatus=NA, AddedDate=NA, ChangeDate=NA, KBA_Qual=NA, Continent="ideal",
	area_kbas=NA, area_overlap=100, percentage=100)
kbas_overlap<- rbind(kbas_overlap, ideal) %>% unique()

#get continent names
con<- unique(kbas_overlap$Continent) [-1]

temp<- list()
for (z in 1:length(con)){
#loop through countries
df<- kbas_overlap %>% filter(Continent==con[z]) %>% group_by(SitRecID) %>% 
	mutate(percentage_SitRecID= sum(percentage))
	
complete_cov_100<- df %>% filter(percentage_SitRecID>=100)	
complete_cov_99<- df %>% filter(percentage_SitRecID>=99)	
complete_cov_98<- df %>% filter(percentage_SitRecID>=98)
complete_cov_97<- df %>% filter(percentage_SitRecID>=97)
complete_cov_96<- df %>% filter(percentage_SitRecID>=96)
complete_cov_95<- df %>% filter(percentage_SitRecID>=95)

complete_uncov_0<- df %>% filter(percentage_SitRecID<=0)
complete_uncov_1<- df %>% filter(percentage_SitRecID<=1)
complete_uncov_2<- df %>% filter(percentage_SitRecID<=2)
complete_uncov_3<- df %>% filter(percentage_SitRecID<=3)
complete_uncov_4<- df %>% filter(percentage_SitRecID<=4)
complete_uncov_5<- df %>% filter(percentage_SitRecID<=5)

ind<- df %>% select(SitRecID,percentage_SitRecID) %>% unique()
	
temp[[z]]<- data.frame(
	Continent= unique(df$Continent),
	KBAs= length(unique(df$SitRecID)),
	Mean= mean(ind$percentage_SitRecID),
	SD=sd(ind$percentage_SitRecID),
	
	indicator_mean= sum(ind$percentage_SitRecID)/length(unique(df$SitRecID)), 
	indicator_median= median(ind$percentage_SitRecID),
	total_cov= sum(df$area_overlap)*100/sum(unique(df$area_kbas)),
	
	n_complete_cov_100= 
		length(unique(complete_cov_100$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_99= 
		length(unique(complete_cov_99$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_98= 
		length(unique(complete_cov_98$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_97= 
		length(unique(complete_cov_97$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_96= 
		length(unique(complete_cov_96$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_95= 
		length(unique(complete_cov_95$SitRecID))*100/length(unique(df$SitRecID)),
		
	n_complete_ucov_0= 
		length(unique(complete_uncov_0$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_1= 
		length(unique(complete_uncov_1$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_2= 
		length(unique(complete_uncov_2$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_3= 
		length(unique(complete_uncov_3$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_4= 
		length(unique(complete_uncov_4$SitRecID))*100/length(unique(df$SitRecID)),		
	n_complete_cov_5= 
		length(unique(complete_uncov_5$SitRecID))*100/length(unique(df$SitRecID)),	

	stringsAsFactors=FALSE)
}
results[[i]]<- do.call(rbind,temp) %>% arrange(Mean) %>% mutate(SD = ifelse(is.na(SD), 0, SD))

# get distance
temp2<- results[[i]] %>% magrittr::set_rownames(.$Continent) %>% dplyr::select(Mean, SD)
temp3<- as.data.frame(as.matrix(dist(temp2,method="euclidean"))) %>% mutate(Continent= row.names(temp2)) %>%
	pivot_longer(-c(Continent), names_to="to", values_to="distance") %>% filter(to=="ideal") %>% 
	as.data.frame() %>%	dplyr::select(-to)

#create final output
final<- left_join(results[[i]],temp3)

write.csv(final, 
	paste0(maindir,"output/indicator_value/continent/indicator_continent(Sept2022)_",year[i],".csv"), 
	row.names=FALSE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~  • 5.3. •  ~~~~~~~~~~~~~~~~~~~~~~~~~~
#calculate indicator world
results<- list()

for (i in 1:length(year)){
#set working directory
setwd("C:/Projects/02_Inspire4Nature/Indicator/Analysis/distance/output/kbas_overlap_country/")

# import annual kba overlap
kbas_overlap<- read.csv(paste0(year[i],".csv"),stringsAsFactors=FALSE) %>% 
	dplyr::select(-Continent) %>% #left_join(continent) %>% 
	unique()
# correct data
kbas_overlap$percentage[kbas_overlap$percentage > 100] <- 100 
kbas_overlap$Country[which(kbas_overlap$ISO3=="RUS")]<- "Russia"

df<- kbas_overlap %>% group_by(SitRecID) %>% mutate(percentage_SitRecID= sum(percentage))
	
complete_cov_100<- df %>% filter(percentage_SitRecID>=100)	
complete_cov_99<- df %>% filter(percentage_SitRecID>=99)	
complete_cov_98<- df %>% filter(percentage_SitRecID>=98)
complete_cov_97<- df %>% filter(percentage_SitRecID>=97)
complete_cov_96<- df %>% filter(percentage_SitRecID>=96)
complete_cov_95<- df %>% filter(percentage_SitRecID>=95)

complete_uncov_0<- df %>% filter(percentage_SitRecID<=0)
complete_uncov_1<- df %>% filter(percentage_SitRecID<=1)
complete_uncov_2<- df %>% filter(percentage_SitRecID<=2)
complete_uncov_3<- df %>% filter(percentage_SitRecID<=3)
complete_uncov_4<- df %>% filter(percentage_SitRecID<=4)
complete_uncov_5<- df %>% filter(percentage_SitRecID<=5)

ind<- df %>% select(SitRecID,percentage_SitRecID) %>% unique()
	
results[[i]]<- data.frame(
	year= year[i],
	KBAs= length(unique(df$SitRecID)),
	Mean= mean(ind$percentage_SitRecID),
	SD=sd(ind$percentage_SitRecID),
	
	indicator_mean= sum(ind$percentage_SitRecID)/length(unique(df$SitRecID)), 
	indicator_median= median(ind$percentage_SitRecID),
	total_cov= sum(df$area_overlap)*100/sum(unique(df$area_kbas)),
	
	n_complete_cov_100= 
		length(unique(complete_cov_100$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_99= 
		length(unique(complete_cov_99$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_98= 
		length(unique(complete_cov_98$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_97= 
		length(unique(complete_cov_97$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_96= 
		length(unique(complete_cov_96$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_cov_95= 
		length(unique(complete_cov_95$SitRecID))*100/length(unique(df$SitRecID)),
		
	n_complete_ucov_0= 
		length(unique(complete_uncov_0$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_1= 
		length(unique(complete_uncov_1$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_2= 
		length(unique(complete_uncov_2$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_3= 
		length(unique(complete_uncov_3$SitRecID))*100/length(unique(df$SitRecID)),
	n_complete_ucov_4= 
		length(unique(complete_uncov_4$SitRecID))*100/length(unique(df$SitRecID)),		
	n_complete_cov_5= 
		length(unique(complete_uncov_5$SitRecID))*100/length(unique(df$SitRecID)),	

	stringsAsFactors=FALSE)
}

results<- do.call(rbind,results) %>% arrange(Mean) %>% mutate(SD = ifelse(is.na(SD), 0, SD))

#add ideal world value
ideal<-data.frame(year= "9999",KBAs=10000, Mean=100, SD=0)
results<- bind_rows(results, ideal) %>% unique()
# get distance
temp2<- results %>% magrittr::set_rownames(.$year) %>% dplyr::select(Mean, SD)
temp3<- as.data.frame(as.matrix(dist(temp2,method="euclidean"))) %>% mutate(year= row.names(temp2)) %>%
	pivot_longer(-c(year), names_to="to", values_to="distance") %>% filter(to=="9999") %>% 
	as.data.frame() %>%	dplyr::select(-to)

#create final output
final<- left_join(results,temp3)

write.csv(final, 
	paste0(maindir,"output/indicator_value/world/indicator_world(Sept2023).csv"), 
	row.names=FALSE)



##########################################################################################################
##########################################################################################################

