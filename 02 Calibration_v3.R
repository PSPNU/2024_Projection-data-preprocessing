#######################################
### STEP 2. Calibration (Bias-correction)
### 법정동 기준으로 작업 진행!!
#######################################

library(dplyr)
library(readxl)
library(lubridate)
library(stringr)

rm(list=setdiff(ls(),"tmean_scn_bysgg")) #용량 너무 커서.. 01 Tsdata_v3.R 돌리고 오면 됨

## 1. 관측 데이터, Projection 데이터, sgginfo 불러오기
# load("C:/Users/Jieun/Dropbox/1.Projection studies/historical_obs_1121.RData")
# load("C:/Users/Jieun/Dropbox/1.Projection studies/tmeanlist_bysgg_v2.RData")
# load("C:/Users/Jieun/Dropbox/1.Projection studies/rhumlist_bysgg_v2.RData")
# load("C:/Users/Jieun/Dropbox/1.Projection studies/sgginfo.RData")

load("C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/historical_obs_1121_240227.RData")
load("C:/Users/Jieun/Dropbox/1.손상자료/2401_질병청_손상 projection_논문화/Data/sgginfo.RData") #위에랑 같은 거임
load("C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/tmean_scn_bysgg_240227.RData")


# Check
names(obs_list)== names(tmean_scn_bysgg[[1]])
sort(names(obs_list))==sort(names(tmean_scn_bysgg[[1]])) # 둘 간 정렬순서 같음 !!


## 2. 시군구별로 hempel 작업
source("C:/Users/Jieun/Dropbox/1.손상자료/2308_질병청_심정지/Code/Calibration/fhempel.R")

# 1) tmean
model_list <- substr(names(tmean_scn_bysgg), 1, str_locate(names(tmean_scn_bysgg), "_")[,1]-1) %>% unique()
tmean_245_cal <- list()
tmean_370_cal <- list()
tmean_585_cal <- list()
tmean_scn_cal <- list()

model_list <- substr(names(tmean_scn_bysgg), 1, str_locate(names(tmean_scn_bysgg), "_")[,1]-1) %>% unique()

for (j in 1:length(model_list)){
  
  # model 
  model_temp <- model_list[[j]]
  # print(model_temp)
  
  # (1) SSP245
  sort(names(obs_list))==sort(names(tmean_scn_bysgg[[paste0(model_temp,"_tmean_245")]])) # 둘 간 정렬순서 다름 !!
  sgginfo$sgg_b == names(obs_list) 
  sgginfo$sgg_b == names(tmean_scn_bysgg[[paste0(model_temp,"_tmean_245")]])
  
  tmean_245 <- tmean_scn_bysgg[[paste0(model_temp,"_tmean_245")]]
  print(paste0(model_temp,"_tmean_245"))
  
  for (i in 1:length(tmean_245)){
    
    temp_sgg <- sgginfo$sgg_b[i]
    print(paste0("--- sgg: ", temp_sgg, " ---"))
    
    # observed data subset 후 변수 순서 설정
    ind <- which(names(obs_list)==temp_sgg)
    obs_temp <- obs_list[[ind]]
    obs_temp$date <- as.Date(paste0(substr(obs_temp$date2,1,4),"-",substr(obs_temp$date2,5,6),"-",substr(obs_temp$date2,7,8)))
    obs_temp$tmean <- obs_temp$TEMP
    obs_temp <- obs_temp %>% dplyr::select(date,tmean)
    obs_temp <- obs_temp[order(obs_temp$date),]
    
    # scenario data subset 후 변수 순서 설정
    ind2 <- which(names(tmean_245)==temp_sgg)
    SSP245_temp <- tmean_245[[ind2]] %>% as.data.frame()
    SSP245_temp$date <- as.Date(SSP245_temp$date)
    SSP245_temp <- SSP245_temp %>% dplyr::select(date, tmean)
    
    # hempel
    SSP245_temp_cal <- fhempel(obs_temp, SSP245_temp)
    head(SSP245_temp)
    head(SSP245_temp_cal)
    
    # 다시 정보 추가
    SSP245_temp_cal$sgg_b <- temp_sgg
    # SSP245_temp_cal$month <- as.numeric(substr(SSP245_temp_cal$date,6,7))
    # SSP245_temp_cal$year <- as.numeric(substr(SSP245_temp_cal$date,1,4))
    # SSP245_temp_cal$day <- as.numeric(substr(SSP245_temp_cal$date,9,10))
    
    # 저장
    tmean_245_cal[[i]] <- SSP245_temp_cal
  }
  names(tmean_245_cal) <- sgginfo$sgg_b
  
  # (2) SSP370
  sort(names(obs_list))==sort(names(tmean_scn_bysgg[[paste0(model_temp,"_tmean_370")]])) # 둘 간 정렬순서 다름 !!
  sgginfo$sgg_b == names(obs_list) 
  
  tmean_370 <- tmean_scn_bysgg[[paste0(model_temp,"_tmean_370")]]
  print(paste0(model_temp,"_tmean_370"))
  
  for (i in 1:length(tmean_370)){
    
    temp_sgg <- sgginfo$sgg_b[i]
    print(paste0("--- sgg: ", temp_sgg, " ---"))
    
    # observed data subset 후 변수 순서 설정
    ind <- which(names(obs_list)==temp_sgg)
    obs_temp <- obs_list[[ind]]
    obs_temp$date <- as.Date(paste0(substr(obs_temp$date2,1,4),"-",substr(obs_temp$date2,5,6),"-",substr(obs_temp$date2,7,8)))
    obs_temp$tmean <- obs_temp$TEMP
    obs_temp <- obs_temp %>% dplyr::select(date,tmean)
    obs_temp <- obs_temp[order(obs_temp$date),]
    
    # scenario data subset 후 변수 순서 설정
    ind2 <- which(names(tmean_370)==temp_sgg)
    SSP370_temp <- tmean_370[[ind2]] %>% as.data.frame()
    SSP370_temp$date <- as.Date(SSP370_temp$date)
    SSP370_temp <- SSP370_temp %>% dplyr::select(date, tmean)
    
    # hempel
    SSP370_temp_cal <- fhempel(obs_temp, SSP370_temp)
    # head(SSP370_temp)
    # head(SSP370_temp_cal)
    
    # 다시 정보 추가
    SSP370_temp_cal$sgg_b <- temp_sgg
    # SSP370_temp_cal$month <- as.numeric(substr(SSP370_temp_cal$date,6,7))
    # SSP370_temp_cal$year <- as.numeric(substr(SSP370_temp_cal$date,1,4))
    # SSP370_temp_cal$day <- as.numeric(substr(SSP370_temp_cal$date,9,10))
    
    # 저장
    tmean_370_cal[[i]] <- SSP370_temp_cal
  }
  names(tmean_370_cal) <- sgginfo$sgg_b
  
  
  # (3) SSP585
  sort(names(obs_list))==sort(names(tmean_scn_bysgg[[paste0(model_temp,"_tmean_585")]])) # 둘 간 정렬순서 다름 !!
  sgginfo$sgg_b == names(obs_list) 
  
  tmean_585 <- tmean_scn_bysgg[[paste0(model_temp,"_tmean_585")]]
  print(paste0(model_temp,"_tmean_585"))
  
  for (i in 1:length(tmean_585)){
    
    temp_sgg <- sgginfo$sgg_b[i]
    print(paste0("--- sgg: ", temp_sgg, " ---"))
    
    # observed data subset 후 변수 순서 설정
    ind <- which(names(obs_list)==temp_sgg)
    obs_temp <- obs_list[[ind]]
    obs_temp$date <- as.Date(paste0(substr(obs_temp$date2,1,4),"-",substr(obs_temp$date2,5,6),"-",substr(obs_temp$date2,7,8)))
    obs_temp$tmean <- obs_temp$TEMP
    obs_temp <- obs_temp %>% dplyr::select(date,tmean)
    obs_temp <- obs_temp[order(obs_temp$date),]
    
    # scenario data subset 후 변수 순서 설정
    ind2 <- which(names(tmean_585)==temp_sgg)
    SSP585_temp <- tmean_585[[ind2]]%>% as.data.frame()
    SSP585_temp$date <- as.Date(SSP585_temp$date)
    SSP585_temp <- SSP585_temp %>% dplyr::select(date, tmean)
    
    # hempel
    SSP585_temp_cal <- fhempel(obs_temp, SSP585_temp)
    # head(SSP585_temp)
    # head(SSP585_temp_cal)
    
    # 다시 정보 추가
    SSP585_temp_cal$sgg_b <- temp_sgg
    # SSP585_temp_cal$month <- as.numeric(substr(SSP585_temp_cal$date,6,7))
    # SSP585_temp_cal$year <- as.numeric(substr(SSP585_temp_cal$date,1,4))
    # SSP585_temp_cal$day <- as.numeric(substr(SSP585_temp_cal$date,9,10))
    
    # 저장
    tmean_585_cal[[i]] <- SSP585_temp_cal
  }
  names(tmean_585_cal) <- sgginfo$sgg_b
  
  
  
  tmean_scn_cal[[paste0(model_temp,"_tmean_245")]] <- tmean_245_cal
  tmean_scn_cal[[paste0(model_temp,"_tmean_370")]] <- tmean_370_cal
  tmean_scn_cal[[paste0(model_temp,"_tmean_585")]] <- tmean_585_cal
  
  
}

# 저장
save(tmean_scn_cal,
     file="C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/tmean_scn_cal_240227.RData")






###################################
### # bias-correction 확인용 그래프
###################################

model_temp <- model_list[[j]]

cal <- tmean_scn_cal[[paste0(model_temp,"_tmean_245")]][[1]]
cal <- cal %>% filter(year(date) %in% c(2011:2021))
cal$month <- month(cal$date)
cal$yday <- yday(cal$date)
cal <- cal %>% group_by(month, yday) %>% summarise(tmean=mean(tmean))

proj <- tmean_scn_bysgg[[paste0(model_temp,"_tmean_245")]][[1]]
proj <- proj %>% filter(year(date) %in% c(2011:2021))
proj$date <- as.Date(proj$date)
proj$month <- month(proj$date)
proj$yday <- yday(proj$date)
proj <- proj %>% group_by(month, yday) %>% summarise(tmean=mean(tmean))

obs <- obs_list[[1]]
obs$date <- as.Date(paste0(substr(obs$date2,1,4),"-",
                           substr(obs$date2,5,6),"-",
                           substr(obs$date2,7,8)))
obs$month <- month(obs$date)
obs$yday <- yday(obs$date)
obs <- obs %>% group_by(month, yday) %>% summarise(TEMP=mean(TEMP))



plot(obs$yday, obs$TEMP, type="l", ylab="temperature", xlab="day of the year")
lines(x=proj$yday, y=proj$tmean, col="blue")
lines(x=cal$yday, y=cal$tmean, col="red")
legend("topright", 
       c("Observation","Projection: Before calibration", "Projection: After calibration"),
       fill=c("black","blue","red"))


plot(obs$date, obs$TEMP, type="l")
lines(x=proj$date, y=proj$tmean, col="blue")
lines(x=cal$date, y=cal$tmean, col="red")
