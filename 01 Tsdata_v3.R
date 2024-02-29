
#############################################################
#############################################################
### 시군구 정보 매칭
### 행정동, 법정동 통일 등..
#############################################################
#############################################################


library(dplyr)
library(readxl)
library(lubridate)


#######################################
## 1. 기상청 데이터 - 250개 시군구로 매칭 ####
#######################################

# C:/Users/Jieun/Dropbox/1.손상자료/2401_질병청_손상 projection_논문화/Code/00 dataprep/01 datamerge 코드 참고!
# 결과물: C:/Users/Jieun/Dropbox/1.손상자료/2401_질병청_손상 projection_논문화/Data/d_e_240218.csv

d_obs <- read.csv("C:/Users/Jieun/Dropbox/1.손상자료/2401_질병청_손상 projection_논문화/Data/d_e_240218.csv")

# 시군구별 list로 바꾸기
obs_list <- list()
sgginfo <- unique(d_obs$sgg_b)
for (i in 1:length(sgginfo)){
  sgginfo_temp <- sgginfo[[i]]
  temp <- subset(d_obs, d_obs$sgg_b==sgginfo_temp)
  print(sgginfo_temp)
  obs_list[[i]] <- temp
}
names(obs_list) <- sgginfo

save(obs_list,
     file="C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/historical_obs_1121_240227.RData")



#######################################
## 2. CMIP 데이터 - 250개 시군구로 매칭 (결국 SGIS에서 붙은 시군구 정보임) ####
## 행정동 값만 있는데 법정동 값 붙여주는 것
#######################################
rm(list=ls())

# 1) 관측값-법정동 연결한 sgginfo 데이터 ####
# load("C:/Users/Jieun/Dropbox/1.Projection studies/sgginfo.RData")
load("C:/Users/Jieun/Dropbox/1.손상자료/2401_질병청_손상 projection_논문화/Data/sgginfo.RData") #위에랑 같은 거임
sgginfo$sgg_b <- as.character(sgginfo$sgg_b); sgginfo$sgg_h <- as.character(sgginfo$sgg_h); 

# 2) CMIP 데이터 불러오기 ####
load("C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/tmeanlist_sgg_240227.RData")
d1 <- rbind(tmeanlist_sgg[["ACCESS-CM2_tashist"]], tmeanlist_sgg[["ACCESS-CM2_tasssp245"]]) # 임의로 데이터 하나 선정
sgginfo_cmip <- data.frame(sgg_h=unique(d1$sgg_h))

# 3) 관측값-법정동 연결된 시군구에 CMIP 데이터 잘 붙는지 확인 ####
sgginfo2 <- left_join(sgginfo_cmip, sgginfo, by="sgg_h")
# 미추홀구 하나만 제대로 안 붙음 # 미추홀구가 2018년에 바뀌어서...... SGIS에서는 남구로 되어 있음
sgginfo2$sgg_b <- ifelse(sgginfo2$sgg_h==23030, 28177, sgginfo2$sgg_b) #메뉴얼로 작업

# 4) CMIP 데이터에 법정동 merge하고 시군구별 리스트로 바꾸기 ####
## (1) tmean
model_list <- substr(names(tmeanlist_sgg), 1, str_locate(names(tmeanlist_sgg), "_")[,1]-1) %>% unique()
tmean_scn_bysgg <- list()

for (i in 1:length(model_list)){
  
  # model 
  model_temp <- model_list[[i]]
  print(model_temp)
  
  #### (a) SSP245
  print("---SSP245")
  d1 <- rbind(tmeanlist_sgg[[paste0(model_temp, "_tashist")]], 
              tmeanlist_sgg[[paste0(model_temp, "_tasssp245")]] %>% filter(year(date) >= 2015)) 
  d2 <- left_join(d1, sgginfo2, by="sgg_h") # Merge 시에는 행정동 코드 사용 (SGIS 데이터가 행정동 사용)
  sum(is.na(d2))
  tmean_245 <- list() 
  for (i in 1:length(sgginfo2$sgg_b)){ ## 결국에 공단에서 쓰이는 코드는 법정동이기에.. 
    sgginfo_temp <- sgginfo2$sgg_b[[i]]
    temp <- subset(d2, d2$sgg_b==sgginfo_temp)
    tmean_245[[i]] <- temp
  }
  names(tmean_245) <- sgginfo2$sgg_b
  
  #### (b) SSP370
  print("---SSP370")
  d1 <- rbind(tmeanlist_sgg[[paste0(model_temp,"_tashist")]], 
              tmeanlist_sgg[[paste0(model_temp,"_tasssp370")]] %>% filter(year(date) >= 2015)) 
  d2 <- left_join(d1, sgginfo2, by="sgg_h")
  sum(is.na(d2))
  tmean_370 <- list()
  for (i in 1:length(sgginfo2$sgg_b)){
    sgginfo_temp <- sgginfo2$sgg_b[[i]]
    temp <- subset(d2, d2$sgg_b==sgginfo_temp)
    tmean_370[[i]] <- temp
  }
  names(tmean_370) <- sgginfo2$sgg_b
  
  #### (c) SSP585
  print("---SSP585")
  d1 <- rbind(tmeanlist_sgg[[paste0(model_temp,"_tashist")]], 
              tmeanlist_sgg[[paste0(model_temp,"_tasssp585")]] %>% filter(year(date) >= 2015))
  d2 <- left_join(d1, sgginfo2, by="sgg_h")
  sum(is.na(d2))
  tmean_585 <- list()
  for (i in 1:length(sgginfo2$sgg_b)){
    sgginfo_temp <- sgginfo2$sgg_b[[i]]
    temp <- subset(d2, d2$sgg_b==sgginfo_temp)
    tmean_585[[i]] <- temp
  }
  names(tmean_585) <- sgginfo2$sgg_b
  

# 모델별로 리스트에 저장  
  tmean_scn_bysgg[[paste0(model_temp,"_tmean_245")]] <- tmean_245
  tmean_scn_bysgg[[paste0(model_temp,"_tmean_370")]] <- tmean_370
  tmean_scn_bysgg[[paste0(model_temp,"_tmean_585")]] <- tmean_585

}

# save
save(tmean_scn_bysgg, 
     file="C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/tmean_scn_bysgg_240227.RData")


