library(sapfluxnetr)
library(ggplot2)
library(dplyr)
library(lubridate)

# Change to folder with SFN RData files (see https://zenodo.org/records/3971689)

sumNA <- function(x) if(all(is.na(x))) NA_integer_ else sum(x, na.rm=TRUE)

folder_sfn<- file.path("SAPFLUXNET_DATA","0.1.5","RData","plant")

sfn_metadata <- read_sfn_metadata(folder = folder_sfn, .write_cache = FALSE)

site <- 'US-UMB'
df <- read_sfn_data('USA_UMB_CON',folder=folder_sfn)

df_daily <- df %>% 
  daily_metrics(tidy=TRUE,metadata=sfn_metadata) 

#--------------------------------------------------------------------------
# Correction (https://doi.org/10.1029/2022GL100100, https://doi.org/10.1016/j.agrformet.2019.03.012)
df_daily$sapflow_mean <- df_daily$sapflow_mean*1.405

df_daily %>%
  ggplot(aes(TIMESTAMP, sapflow_mean, col = pl_code)) + geom_line()

# Sap flow per basal area per species -------------------------------------
df_sp <- df_daily %>% 
  mutate(Jab = sapflow_mean*24/(pi*(pl_dbh/2)^2)) %>% 
  group_by(TIMESTAMP, pl_species) %>% 
  mutate(Et_sp = mean(Jab,na.rm=T)*((st_basal_area*sp_basal_area_perc/100)/1000),
         Et_sp_n = length(!is.na(Jab))) %>% 
  distinct(TIMESTAMP,si_code, pl_species, .keep_all = TRUE ) %>% 
  select(TIMESTAMP,si_code, pl_species,sp_basal_area_perc,st_basal_area,Et_sp,Et_sp_n)

# Stand T -----------------------------------------------------------------

df_stand<- df_sp %>% 
  group_by(TIMESTAMP) %>% 
  summarise(Et = sumNA(Et_sp),
            Et_coverage=sumNA(sp_basal_area_perc[!is.na(Et_sp)])) 

df_stand$Et_coverage


# Plots --------------------------------------------------------------------

df_stand %>% 
  #dplyr::filter(year(TIMESTAMP)==2012) %>% 
  ggplot()+
  geom_line(aes(x=TIMESTAMP,y=Et))+
  theme(strip.text.y = element_text(size=16, angle=0,face="bold"))+
  labs(x="", y="Stand transpiration [mm·day-1]")+
  theme(title=element_text(size=18),axis.text=element_text(size=18),legend.title=element_blank(),
        legend.position="top", legend.box = "horizontal",
        legend.text=element_text(size=18))

df_sp %>% 
  ggplot()+
  geom_line(aes(x=TIMESTAMP,y=Et_sp,col=pl_species))+
  theme(strip.text.y = element_text(size=16, angle=0,face="bold"))+
  labs(x="", y="Species transpiration [mm·day-1]")+
  theme(title=element_text(size=18),axis.text=element_text(size=18),legend.title=element_blank(),
        legend.position="top", legend.box = "horizontal",
        legend.text=element_text(size=18))

write.csv(df_stand, paste0("SAPFLUXNET_DATA/Extracted_Stand_Scale_T_FluxNet/",
                           site,"_daily_T.csv"), row.names = FALSE)

