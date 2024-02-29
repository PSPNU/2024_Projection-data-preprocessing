
####################################################
############ 1. 데이터 추출 및 일별 데이터로 정제
############ 참고: https://pjbartlein.github.io/REarthSysSci/netCDF.html#reading-restructuring-and-writing-netcdf-files-in-r
####################################################

memory.limit(size=9999999999999)
options(digits=10)
`%notin%` <- Negate(`%in%`)

library(ncdf4)
library(tidyverse)
library(raster)
library(spData)
library(lubridate)
# library(RNetCDF)
library(dplyr)
library(readxl)

# set variable
var <- c("tas","hurs","huss")
path <- c("_day_ACCESS-CM2_historical_20000101-20141231.KR.nc",
          "_day_ACCESS-CM2_ssp245_20150101-21001231.KR.nc",
          "_day_ACCESS-CM2_ssp370_20150101-21001231.KR.nc",
          "_day_ACCESS-CM2_ssp585_20150101-21001231.KR.nc",
          
          "_day_CanESM5_historical_20000101-20141231.KR.nc",
          "_day_CanESM5_ssp245_20150101-21001231.KR.nc",
          "_day_CanESM5_ssp370_20150101-21001231.KR.nc",
          "_day_CanESM5_ssp585_20150101-21001231.KR.nc",
          
          "_day_CESM2_historical_20000101-20141231.KR.nc",
          "_day_CESM2_ssp245_20150101-21001231.KR.nc",
          "_day_CESM2_ssp370_20150101-21001231.KR.nc",
          "_day_CESM2_ssp585_20150101-21001231.KR.nc",
          
          "_day_CNRM-CM6-1_historical_r1i1p1f2_gr_20000101-20141231.KR.nc",
          "_day_CNRM-CM6-1_ssp245_r1i1p1f2_gr_20150101-21001231.KR (1).nc",
          "_day_CNRM-CM6-1_ssp370_r1i1p1f2_gr_20150101-21001231.KR.nc",
          "_day_CNRM-CM6-1_ssp585_r1i1p1f2_gr_20150101-21001231.KR.nc",
          
          "_day_CNRM-ESM2-1_historical_20000101-20141231.KR.nc",
          "_day_CNRM-ESM2-1_ssp245_20150101-21001231.KR.nc",
          "_day_CNRM-ESM2-1_ssp370_20150101-21001231.KR.nc",
          "_day_CNRM-ESM2-1_ssp585_20150101-21001231.KR.nc",
          
          "_day_KACE-1-0-G_historical_20000101-20141230.KR.nc",
          "_day_KACE-1-0-G_ssp245_20150101-21001230.KR.nc",
          "_day_KACE-1-0-G_ssp370_20150101-21001230.KR.nc",
          "_day_KACE-1-0-G_ssp585_20150101-21001230.KR.nc")

path_grid <- expand.grid(path, var)
path_grid$Var3 <- (c("hist","ssp245","ssp370","ssp585"))
path_grid$model <- c(rep("ACCESS-CM2", 4),rep("CanESM5", 4),rep("CESM2", 4),rep("CNRM-CM6-1", 4),rep("CNRM-ESM2-1", 4),rep("KACE-1-0-G", 4))


#### 1. tas #####################################################
tmeanlist <- list()
path_grid_temp <- path_grid %>% filter(Var2=="tas")

for (i in 1:nrow(path_grid_temp)){
  
  # load data
  path_temp <- paste0("D:/ICCP/CMIP6/",path_grid_temp[i,2],path_grid_temp[i,1])
  ncin <- nc_open(path_temp)
  # print(ncin)
  print(paste0(path_grid_temp[i,"model"], " ====== ", path_grid_temp[i,2], path_grid_temp[i,3]))
  
  # 변수명 지정
  var_temp <- path_grid_temp[i,2]
  dname <- as.character(var_temp)
  
  # get longitude and latitude
  lon <- ncvar_get(ncin,"lon")
  nlon <- dim(lon)
  head(lon)
  lat <- ncvar_get(ncin,"lat")
  nlat <- dim(lat)
  head(lat)
  
  # get time
  time <- ncvar_get(ncin,"time")
  tunits <- ncatt_get(ncin,"time","units")
  nt <- dim(time)
  nt 
  
  # get variable
  var_array <- ncvar_get(ncin, dname)
  dlnmae <- ncatt_get(ncin, dname, "long_name")
  dunits <- ncatt_get(ncin, dname, "units")
  fillvalue <- ncatt_get(ncin, dname, "_FillValue")
  nval <- dim(var_array)
  
  # dataframe
  df <- data.frame(noncompliance::expand.grid.DT(time,lat,lon), c(var_array))
  colnames(df) <- c("date", "lat", "lon", 'tmean_K')
  df$scn <- path_grid_temp[i,"Var3"]
  df$tmean <- df$tmean_K-273.15 #온도 단위 K로 되어 있어서 C로 변환
  df$model <- path_grid_temp[i,"model"]
  df <- df[,c("date","lat","lon","tmean","tmean_K","scn","model")]
  
  # save as list
  tmeanlist[[paste0(path_grid_temp[i,"model"],"_",path_grid_temp[i,2], path_grid_temp[i,3])]] <- df
  
}


#### 모델별로 Date 확인 및 coord 뽑아내기####
coord_list <- list()

###### 1) ACCESS-CM2 #####################################################
tmeanlist_temp <- tmeanlist[str_detect(names(tmeanlist),"ACCESS-CM2")]

# 좌표 갯수 확인 및 저장
coord_list[["ACCESS-CM2"]] <- unique(tmeanlist_temp[[1]][c("lat","lon")]) #20개
coord_cnt <-  coord_list[["ACCESS-CM2"]] %>% nrow()
coord_list[["ACCESS-CM2"]]$id <- seq(1,coord_cnt)
coord_list[["ACCESS-CM2"]]$model <- "ACCESS-CM2"

# historical
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"hist")] %>% as.data.frame()
length(unique(a[,1])) #5479개 (통과)
tmeanlist[["ACCESS-CM2_tashist"]]$date2 <- sort(rep(seq(ymd('2000-01-01'), ymd('2014-12-31'), by ='days'), coord_cnt))

# future
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"ssp245")] %>% as.data.frame()
length(unique(a[,1])) #31411개 (통과)
tmeanlist[["ACCESS-CM2_tasssp245"]]$date2 <- sort(rep(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'), coord_cnt))
tmeanlist[["ACCESS-CM2_tasssp370"]]$date2 <- sort(rep(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'), coord_cnt))
tmeanlist[["ACCESS-CM2_tasssp585"]]$date2 <- sort(rep(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'), coord_cnt))



###### 2) CanESM5  #####################################################
tmeanlist_temp <- tmeanlist[str_detect(names(tmeanlist),"CanESM5")]

# 좌표 갯수 확인 및 저장
coord_list[["CanESM5"]] <- unique(tmeanlist_temp[[1]][c("lat","lon")]) #4개
coord_cnt <-  coord_list[["CanESM5"]] %>% nrow()
coord_list[["CanESM5"]]$id <- 1:coord_cnt
coord_list[["CanESM5"]]$model <- "CanESM5"

# historical
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"hist")] %>% as.data.frame()
length(unique(a[,1])) #5475개 (윤년빠짐)
dateinfo <- subset(seq(ymd('2000-01-01'), ymd('2014-12-31'), by ='days'),
       seq(ymd('2000-01-01'), ymd('2014-12-31'), by ='days') %notin% as.Date(c("2000-02-29", "2004-02-29", "2008-02-29", "2012-02-29")))
tmeanlist[["CanESM5_tashist"]]$date2 <- sort(rep(dateinfo, coord_cnt))

# future
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"ssp245")] %>% as.data.frame()
length(unique(a[,1])) #31390개 (윤년빠짐)
dateinfo <- subset(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'),
                   seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days') %notin% as.Date(paste0(seq(2016, 2099, by=4), "-02-29")))
tmeanlist[["CanESM5_tasssp245"]]$date2 <- sort(rep(dateinfo, coord_cnt))
tmeanlist[["CanESM5_tasssp370"]]$date2 <- sort(rep(dateinfo, coord_cnt))
tmeanlist[["CanESM5_tasssp585"]]$date2 <- sort(rep(dateinfo, coord_cnt))


###### 2-1) 윤년 채워넣기 ###### 
model_list <- c(paste0("CanESM5_", c("tashist","tasssp245","tasssp370","tasssp585")))
for (i in 1:length(model_list)){
  
  model_temp <- model_list[i]
  print(model_temp)
  
  # 모델별로 subset
  a <- tmeanlist[[model_temp]]

  # 윤년 앞뒤날짜 뽑아냄
  a1 <- a %>% filter(date2 %in% as.Date(paste0(seq(2000, 2099, by=4), "-02-28")))
  a2 <- a %>% filter(date2 %in% as.Date(paste0(seq(2000, 2099, by=4), "-03-01")))
  a3 <- rbind(a1, a2)
  a3$year <- year(a3$date2)
  
  # 윤년 앞뒤날짜 기준으로 mean 값 구함
  a4 <- a3 %>% group_by(lat, lon, year) %>% summarise(scn=unique(scn),
                                                      model=unique(model),
                                                      tmean = mean(tmean),
                                                      tmean_K = mean(tmean_K)) %>% mutate(date2=paste0(year, "-02-29"))
  a4 <- a4[order(a4$year, a4$lat, a4$lon),]
  a4$date <- NA; a4$year <- NULL
  
  # 기존 데이터에 rbind
  a5 <- rbind(a, a4)
  a5 <- a5[order(a5$date2, a5$lat, a5$lon),]
  
  # 확인
  print(paste0("원래 행 수: ", nrow(a)))
  print(paste0("추가한 후 행 수: ", nrow(a5)))
  print(paste0("날짜 일치하는 행 수: ", sum(a$date2==a5$date2)))
  print(paste0("lat 일치 수: ", sum(a5$lat==a$lat)))
  print(paste0("lon 일치 수: ", sum(a5$lon==a$lon)))
  
  # 수정된 데이터 
  tmeanlist[[model_temp]] <- a5
}

# 윤년 날짜 잘 삽입되었는지 확인
sum(unique(tmeanlist$CanESM5_tashist$date2)==unique(tmeanlist$`ACCESS-CM2_tashist`$date2))

a <- tmeanlist[["CanESM5_tashist"]]
a %>% filter(date2 %in% as.Date(paste0(seq(2000, 2099, by=4), "-02-29")))



###### 3) CESM2  #####################################################
tmeanlist_temp <- tmeanlist[str_detect(names(tmeanlist),"CESM2")]

# 좌표 갯수 확인 및 저장
coord_list[["CESM2"]] <- unique(tmeanlist_temp[[1]][c("lat","lon")]) #36개
coord_cnt <-  coord_list[["CESM2"]] %>% nrow()
coord_list[["CESM2"]]$id <- 1:coord_cnt
coord_list[["CESM2"]]$model <- "CESM2"

# historical
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"hist")] %>% as.data.frame()
length(unique(a[,1])) #5475개 (윤년빠짐)
dateinfo <- subset(seq(ymd('2000-01-01'), ymd('2014-12-31'), by ='days'),
                   seq(ymd('2000-01-01'), ymd('2014-12-31'), by ='days') %notin% as.Date(c("2000-02-29", "2004-02-29", "2008-02-29", "2012-02-29")))
tmeanlist[["CESM2_tashist"]]$date2 <- sort(rep(dateinfo, coord_cnt))

# future
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"ssp245")] %>% as.data.frame()
length(unique(a[,1])) #31390개 (윤년빠짐)
dateinfo <- subset(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'),
                   seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days') %notin% as.Date(paste0(seq(2016, 2099, by=4), "-02-29")))
tmeanlist[["CESM2_tasssp245"]]$date2 <- sort(rep(dateinfo, coord_cnt))
tmeanlist[["CESM2_tasssp370"]]$date2 <- sort(rep(dateinfo, coord_cnt))
tmeanlist[["CESM2_tasssp585"]]$date2 <- sort(rep(dateinfo, coord_cnt))


###### 3-1) 윤년 채워넣기 ###### 
model_list <- c(paste0("CESM2_", c("tashist","tasssp245","tasssp370","tasssp585")))
for (i in 1:length(model_list)){
  
  model_temp <- model_list[i]
  print(model_temp)
  
  # 모델별로 subset
  a <- tmeanlist[[model_temp]]
  
  # 윤년 앞뒤날짜 뽑아냄
  a1 <- a %>% filter(date2 %in% as.Date(paste0(seq(2000, 2099, by=4), "-02-28")))
  a2 <- a %>% filter(date2 %in% as.Date(paste0(seq(2000, 2099, by=4), "-03-01")))
  a3 <- rbind(a1, a2)
  a3$year <- year(a3$date2)
  
  # 윤년 앞뒤날짜 기준으로 mean 값 구함
  a4 <- a3 %>% group_by(lat, lon, year) %>% summarise(scn=unique(scn),
                                                      model=unique(model),
                                                      tmean = mean(tmean),
                                                      tmean_K = mean(tmean_K)) %>% mutate(date2=paste0(year, "-02-29"))
  a4 <- a4[order(a4$year, a4$lat, a4$lon),]
  a4$date <- NA; a4$year <- NULL
  
  # 기존 데이터에 rbind
  a5 <- rbind(a, a4)
  a5 <- a5[order(a5$date2, a5$lat, a5$lon),]
  
  # 확인
  print(paste0("원래 행 수: ", nrow(a)))
  print(paste0("추가한 후 행 수: ", nrow(a5)))
  print(paste0("날짜 일치하는 행 수: ", sum(a$date2==a5$date2)))
  print(paste0("lat 일치 수: ", sum(a5$lat==a$lat)))
  print(paste0("lon 일치 수: ", sum(a5$lon==a$lon)))
  
  # 수정된 데이터 
  tmeanlist[[model_temp]] <- a5
}

# 윤년 날짜 잘 삽입되었는지 확인
sum(unique(tmeanlist$CanESM5_tashist$date2)==unique(tmeanlist$`ACCESS-CM2_tashist`$date2))
a <- tmeanlist[["CESM2_tashist"]]
a %>% filter(date2 %in% as.Date(paste0(seq(2000, 2099, by=4), "-02-29")))




###### 4) CNRM-CM6-1  #####################################################
tmeanlist_temp <- tmeanlist[str_detect(names(tmeanlist),"CNRM-CM6-1")]

# 좌표 갯수 확인 및 저장
coord_list[["CNRM-CM6-1"]] <- unique(tmeanlist_temp[[1]][c("lat","lon")]) #20개
coord_cnt <-  coord_list[["CNRM-CM6-1"]] %>% nrow()
coord_list[["CNRM-CM6-1"]]$id <- 1:coord_cnt
coord_list[["CNRM-CM6-1"]]$model <- "CNRM-CM6-1"

# historical
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"hist")] %>% as.data.frame()
length(unique(a[,1])) #5479개 (통과)
tmeanlist[["CNRM-CM6-1_tashist"]]$date2 <- sort(rep(seq(ymd('2000-01-01'), ymd('2014-12-31'), by ='days'), coord_cnt))

# future
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"ssp245")] %>% as.data.frame()
length(unique(a[,1])) #31411개 (통과)
tmeanlist[["CNRM-CM6-1_tasssp245"]]$date2 <- sort(rep(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'), coord_cnt))
tmeanlist[["CNRM-CM6-1_tasssp370"]]$date2 <- sort(rep(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'), coord_cnt))
tmeanlist[["CNRM-CM6-1_tasssp585"]]$date2 <- sort(rep(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'), coord_cnt))




###### 5) CNRM-ESM2-1  #####################################################
tmeanlist_temp <- tmeanlist[str_detect(names(tmeanlist),"CNRM-ESM2-1")]

# 좌표 갯수 확인 및 저장
coord_list[["CNRM-ESM2-1"]] <- unique(tmeanlist_temp[[1]][c("lat","lon")]) #20개
coord_cnt <-  coord_list[["CNRM-ESM2-1"]] %>% nrow()
coord_list[["CNRM-ESM2-1"]]$id <- 1:coord_cnt
coord_list[["CNRM-ESM2-1"]]$model <- "CNRM-ESM2-1"

# historical
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"hist")] %>% as.data.frame()
length(unique(a[,1])) #5479개 (통과)
tmeanlist[["CNRM-ESM2-1_tashist"]]$date2 <- sort(rep(seq(ymd('2000-01-01'), ymd('2014-12-31'), by ='days'), coord_cnt))

# future
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"ssp245")] %>% as.data.frame()
length(unique(a[,1])) #31411개 (통과)
tmeanlist[["CNRM-ESM2-1_tasssp245"]]$date2 <- sort(rep(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'), coord_cnt))
tmeanlist[["CNRM-ESM2-1_tasssp370"]]$date2 <- sort(rep(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'), coord_cnt))
tmeanlist[["CNRM-ESM2-1_tasssp585"]]$date2 <- sort(rep(seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days'), coord_cnt))



###### 6) KACE-1-0-G  #####################################################
tmeanlist_temp <- tmeanlist[str_detect(names(tmeanlist),"KACE-1-0-G")]

# 좌표 갯수 확인 및 저장
coord_list[["KACE-1-0-G"]] <- unique(tmeanlist_temp[[1]][c("lat","lon")]) #20개
coord_cnt <-  coord_list[["KACE-1-0-G"]] %>% nrow()
coord_list[["KACE-1-0-G"]]$id <- 1:coord_cnt
coord_list[["KACE-1-0-G"]]$model <- "KACE-1-0-G"

# historical
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"hist")] %>% as.data.frame()
length(unique(a[,1])) #5400개 (매달 30일로 설정됨)
dateinfo <- seq(ymd('2000-01-01'), ymd('2014-12-31'), by ='days') #정상적인 날짜
dateinfo <- subset(dateinfo, substr(dateinfo, 9,10) != 31) #31일 모두 제외
dateinfo <- subset(dateinfo, dateinfo %notin% as.Date(c("2000-02-29", "2004-02-29", "2008-02-29", "2012-02-29"))) #윤년 제외
dateinfo <- c(as.character(dateinfo), paste0(2000:2014, "-02-29"), paste0(2000:2014, "-02-30")) #2월도 30일까지 있는 것으로 우선은 만들어 두기
tmeanlist[["KACE-1-0-G_tashist"]]$date2 <- sort(rep(dateinfo, coord_cnt))

###### 6-1) historical 빠진 날짜 채워넣기 ###### 
# 31일 채워넣기
# 2/30 제외하기 
# 윤년 아닐경우 2/29 제외하기

# 모델별로 subset
model_temp <- "KACE-1-0-G_tashist" 
a <- tmeanlist[[model_temp]]

## 6-1-1) 31일 채워넣기
# 31일 앞뒤날짜 뽑아냄
a1 <- a %>% filter(date2 %in% paste0(sort(c(paste0(2000:2014, "-01"), paste0(2000:2014, "-02"), paste0(2000:2014, "-03"), paste0(2000:2014, "-04"), 
                                            paste0(2000:2014, "-05"), paste0(2000:2014, "-06"),paste0(2000:2014, "-07"), paste0(2000:2014, "-08"), 
                                            paste0(2000:2014, "-09"), paste0(2000:2014, "-10"), paste0(2000:2014, "-11"), paste0(2000:2014, "-12"))),
                                     "-30"))
a2 <- a %>% filter(date2 %in% paste0(sort(c(paste0(2000:2014, "-01"), paste0(2000:2014, "-02"), paste0(2000:2014, "-03"), paste0(2000:2014, "-04"), 
                                            paste0(2000:2014, "-05"), paste0(2000:2014, "-06"),paste0(2000:2014, "-07"), paste0(2000:2014, "-08"), 
                                            paste0(2000:2014, "-09"), paste0(2000:2014, "-10"), paste0(2000:2014, "-11"), paste0(2000:2014, "-12"))),
                                     "-01"))
a3 <- rbind(a1, a2)
a3$year <- substr(a3$date2, 1,4) %>% as.numeric()
a3$month <- substr(a3$date2, 6,7) %>% as.numeric()

# matched month 
a3$matched_month <- ifelse(substr(a3$date2, 10,10)==1, a3$month-1, a3$month)
a3$matched_month <- ifelse(a3$matched_month==0, 12, a3$matched_month)
a3 <- a3[order(a3$date2, a3$lat, a3$lon), ]

# 31일 값 계산
a4 <- a3 %>% group_by(lat, lon, year, matched_month) %>% 
  summarise(scn=unique(scn),
            model=unique(model),
            tmean = mean(tmean),
            tmean_K = mean(tmean_K)) %>% 
  mutate(date2=paste0(year, "-", sprintf("%02d", matched_month), "-31"))
a4 <- a4 %>% filter(matched_month %in% c(1,3,5,7,8,10,12)) # 31일이 있는 월만 남김
a4 <- a4[order(a4$year, a4$lat, a4$lon),]
a4$date <- NA; a4$year <- NULL; a4$matched_month <- NULL

# 기존 데이터에 rbind
a5 <- rbind(a, a4)
a5 <- a5[order(a5$date2, a5$lat, a5$lon),]

## 6-1-2) 2/30 빼기
a5 <- subset(a5, a5$date2 %notin% paste0(2000:2014, "-02-30"))

## 6-1-3) 윤년이 아니라면 2/29 빼기
a5$ind <- ifelse(substr(a5$date2, 1,4) %notin% seq(2000, 2014, by=4) &
                   substr(a5$date2, 6, 10) =="02-29", "del", "save")
a5 <- subset(a5, a5$ind=="save")
a5$ind <- NULL
a5 <- a5[order(a5$date2, a5$lat, a5$lon),]

# 확인
print(paste0("원래 행 수: ", nrow(a)))
print(paste0("추가한 후 행 수: ", nrow(a5)))
print(paste0("날짜 일치하는 행 수: ", sum(a$date2==a5$date2)))
print(paste0("lat 일치 수: ", sum(a5$lat==a$lat)))
print(paste0("lon 일치 수: ", sum(a5$lon==a$lon)))

# 수정된 데이터 
tmeanlist[[model_temp]] <- a5

# future
a <- tmeanlist_temp[str_detect(names(tmeanlist_temp),"ssp245")] %>% as.data.frame()
length(unique(a[,1])) #30960개 (매달 30일로 설정됨)
dateinfo <- seq(ymd('2014-01-01'), ymd('2099-12-31'), by ='days') #정상적인 날짜
dateinfo <- subset(dateinfo, substr(dateinfo, 9,10) != 31) #31일 모두 제외
dateinfo <- subset(dateinfo, dateinfo %notin% as.Date(paste0(seq(2016, 2099, by=4), "-02-29"))) #윤년 제외
dateinfo <- c(as.character(dateinfo), paste0(2014:2099, "-02-29"), paste0(2014:2099, "-02-30")) #2월도 30일까지 있는 것으로 우선은 만들어 두기
tmeanlist[["KACE-1-0-G_tasssp245"]]$date2 <- sort(rep(dateinfo, coord_cnt))
tmeanlist[["KACE-1-0-G_tasssp370"]]$date2 <- sort(rep(dateinfo, coord_cnt))
tmeanlist[["KACE-1-0-G_tasssp585"]]$date2 <- sort(rep(dateinfo, coord_cnt))


###### 6-2) ssp 시나리오 빠진 날짜 채워넣기 ###### 
# 31일 채워넣기
# 2/30 제외하기 
# 윤년 아닐경우 2/29 제외하기

model_list <- c(paste0("KACE-1-0-G_", c("tasssp245","tasssp370","tasssp585")))
for (i in 1:length(model_list)){
  
  # 모델별로 subset
  model_temp <- model_list[i]
  a <- tmeanlist[[model_temp]]
  print(model_temp)
  
  ## 6-2-1) 31일 채워넣기
  # 31일 앞뒤날짜 뽑아냄
  a1 <- a %>% filter(date2 %in% paste0(sort(c(paste0(2014:2099, "-01"), paste0(2014:2099, "-02"), paste0(2014:2099, "-03"), paste0(2014:2099, "-04"), 
                                              paste0(2014:2099, "-05"), paste0(2014:2099, "-06"),paste0(2014:2099, "-07"), paste0(2014:2099, "-08"), 
                                              paste0(2014:2099, "-09"), paste0(2014:2099, "-10"), paste0(2014:2099, "-11"), paste0(2014:2099, "-12"))),
                                       "-30"))
  a2 <- a %>% filter(date2 %in% paste0(sort(c(paste0(2014:2099, "-01"), paste0(2014:2099, "-02"), paste0(2014:2099, "-03"), paste0(2014:2099, "-04"), 
                                              paste0(2014:2099, "-05"), paste0(2014:2099, "-06"),paste0(2014:2099, "-07"), paste0(2014:2099, "-08"), 
                                              paste0(2014:2099, "-09"), paste0(2014:2099, "-10"), paste0(2014:2099, "-11"), paste0(2014:2099, "-12"))),
                                       "-01"))
  a3 <- rbind(a1, a2)
  a3$year <- substr(a3$date2, 1,4) %>% as.numeric()
  a3$month <- substr(a3$date2, 6,7) %>% as.numeric()
  
  # matched month 
  a3$matched_month <- ifelse(substr(a3$date2, 10,10)==1, a3$month-1, a3$month)
  a3$matched_month <- ifelse(a3$matched_month==0, 12, a3$matched_month)
  a3 <- a3[order(a3$date2, a3$lat, a3$lon), ]
  
  # 31일 값 계산
  a4 <- a3 %>% group_by(lat, lon, year, matched_month) %>% 
    summarise(scn=unique(scn),
              model=unique(model),
              tmean = mean(tmean),
              tmean_K = mean(tmean_K)) %>% 
    mutate(date2=paste0(year, "-", sprintf("%02d", matched_month), "-31"))
  a4 <- a4 %>% filter(matched_month %in% c(1,3,5,7,8,10,12)) # 31일이 있는 월만 남김
  a4 <- a4[order(a4$year, a4$lat, a4$lon),]
  a4$date <- NA; a4$year <- NULL; a4$matched_month <- NULL
  
  # 기존 데이터에 rbind
  a5 <- rbind(a, a4)
  a5 <- a5[order(a5$date2, a5$lat, a5$lon),]
  
  ## 6-2-2) 2/30 빼기
  a5 <- subset(a5, a5$date2 %notin% paste0(2014:2099, "-02-30"))
  
  ## 6-2-3) 윤년이 아니라면 2/29 빼기
  a5$ind <- ifelse(substr(a5$date2, 1,4) %notin% seq(2016, 2099, by=4) &
                     substr(a5$date2, 6, 10) =="02-29", "del", "save")
  a5 <- subset(a5, a5$ind=="save")
  a5$ind <- NULL
  a5 <- a5[order(a5$date2, a5$lat, a5$lon),]
  
  # 확인
  print(paste0("원래 행 수: ", nrow(a)))
  print(paste0("추가한 후 행 수: ", nrow(a5)))
  print(paste0("날짜 일치하는 행 수: ", sum(a5$date2==a$date2)))
  print(paste0("lat 일치 수: ", sum(a5$lat==a$lat)))
  print(paste0("lon 일치 수: ", sum(a5$lon==a$lon)))
  
  # 수정된 데이터 
  tmeanlist[[model_temp]] <- a5
  
}



########## coord 저장 --> QGIS에서 작업 이어서
write.csv(coord_list[["ACCESS-CM2"]], "C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/coord_CMIP6_1.csv",row.names = F)
write.csv(coord_list[["CanESM5"]], "C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/coord_CMIP6_2.csv",row.names = F)
write.csv(coord_list[["CESM2"]], "C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/coord_CMIP6_3.csv",row.names = F)
write.csv(coord_list[["CNRM-CM6-1"]], "C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/coord_CMIP6_4.csv",row.names = F)
write.csv(coord_list[["CNRM-ESM2-1"]], "C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/coord_CMIP6_5.csv",row.names = F)
write.csv(coord_list[["KACE-1-0-G"]], "C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/coord_CMIP6_6.csv",row.names = F)





####################################################
############ 2. 바다에 해당되는 그리드 마스킹 처리 후 
############    가장 가까운 격자에 매칭되도록 시군구 정제
############    바다에 해당되는 격자는 제거하고 진행!!! 
############ QGIS에서 작업 진행 (00 QGIS_v1.1.txt 참고)
####################################################

# 파일명: coord_1-6_sgg.xlsx

# load xlsx
coord <- rbind(
  read_xlsx("C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/202402/coord_1-6_sgg.xlsx", sheet = 1),
  read_xlsx("C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/202402/coord_1-6_sgg.xlsx", sheet = 2),
  read_xlsx("C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/202402/coord_1-6_sgg.xlsx", sheet = 3),
  read_xlsx("C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/202402/coord_1-6_sgg.xlsx", sheet = 4),
  read_xlsx("C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/202402/coord_1-6_sgg.xlsx", sheet = 5),
  read_xlsx("C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/202402/coord_1-6_sgg.xlsx", sheet = 6))


#  tmeanlist 데이터에 id 기준으로 시군구 붙이기
tmeanlist_sgg <- list(); 
for (i in 1:length(tmeanlist)){
  
  # data subset
  a <- tmeanlist[[i]]
  model_temp <- a$model %>% unique()
  
  # coord subset
  coord_temp <- coord %>% filter(model==model_temp)
  
  # length 달라서 merge 안 되는 문제 해결
  a$lat <- round(a$lat,8) 
  a$lon <- round(a$lon,8)
  coord_temp$lat <- round(coord_temp$lat,8)
  coord_temp$lon <- round(coord_temp$lon,8)
  
  # Merge
  aa <- left_join(coord_temp[,c("sgg_h","lat","lon","id")], a, by=c('lat',"lon")) 
  
  # 데이터 문제로.. 필요한 변수만 남김
  aa$ date <- aa$date2; aa$date2 <- NULL

  # save as list
  # tmeanlist_sgg[[names(tmeanlist)[[i]]]] <- aa[,c("id","date","sgg_h","tmean","scn","model")]
  tmeanlist_sgg[[names(tmeanlist)[[i]]]] <- aa[,c("id","date","sgg_h","tmean")]
  print(names(tmeanlist)[[i]])
  
}



# Save
save(tmeanlist_sgg, file= "C:/Users/Jieun/Dropbox/1.Projection studies/1. Dry_moist/Data/tmeanlist_sgg_240227.RData")
