yeartime<-function(x){
  #x<-11#月份
  dayN<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  dayNum<-c(1,32,60,91,121,152,182,213,244,274,305,335)
  dayS<-c("2019-01-","2019-02-","2019-03-","2018-04-","2019-05-","2019-06-"
          ,"2019-07-","2019-08-","2019-09-","2018-10-","2019-11-","2019-12-")
  NUM<-c("01","02","03","04","05","06","07","08","09")
  year<-c()
  for(i in 1:12)
  {
    for(j in 1:dayN[i])
    {
      if(j<10)
      {
        j<-NUM[j]
      }
      day_1<-paste0(dayS[i],j)
      year<-c(year,day_1)
    }
  }
  year<-year[dayNum[x]:(dayNum[x+1]-1)]
  return(year)#输出为一个数组
  #样本：[1] "2019-11-01" "2019-11-02" "2019-11-03"
}#时间函数构建文件名格式化提取
TrajGenerate<-function(x,y,z,e,f,year){
  library(dplyr)
  library(tidyr)
  # x<-1#日期
  # y<-"08"#小时
  # z<-1800#分隔时间数
  # e<-35#分隔估计长度上限
  # f<-1#分隔估计长度下限
  # function(x,y,z,e,f)
  load(paste0("2019_day/",year[x],".rdata"))
  #输入dataframe格式:sbbm chr---ID chr---time chr---ymd chr---h chr
  #数据样本:510700000000090048 川YQ6388 2019-11-01 08:06:06 2019-11-01 08
  track3<-filter(track3,h ==y)
  cars<-track3
  basic_sbbm<-data.frame(basic_sbbm)
  names(basic_sbbm)<-c('sbbm')
  basic_sbbm$sbbm<-as.character(basic_sbbm$sbbm)
  basic_sbbm<-data.frame(unique(basic_sbbm))
  carss<-merge(basic_sbbm,cars,by= "sbbm", all = F)
  cars<-carss
  cars<-filter(cars,ID!="污损车牌")
  cars<-filter(cars,ID!="污损牌照")
  cars<-filter(cars,ID!="未识别")
  g<-group_by(cars,cars$ID)
  index<-group_indices(g,ID)
  df<-data.frame(index)
  cars<-cbind(cars,df)
  cars<-cars[order(cars$index,as.POSIXlt(cars$time) ),]
  diff_time<-diff(as.POSIXlt(cars$time))
  diff_times<-data.frame(diff_time)
  add<-data.frame(diff_time=c(0))
  diff_times<-rbind(diff_times,add)
  cars<-cbind(cars,diff_times)
  next_car<-data.frame(cars$ID)
  next_car<-data.frame(nextcar=next_car[-1,])
  lastcar<-data.frame(nextcar=c("00000"))
  next_car<-data.frame(rbind(next_car,lastcar))
  next_sbbm<-data.frame(cars$sbbm)
  next_sbbm<-data.frame(nextsbbm=next_sbbm[-1,])
  lastsbbm<-data.frame(nextsbbm=c("00000"))
  next_sbbm<-data.frame(rbind(next_sbbm,lastsbbm))
  cars<-cbind(cars,next_car,next_sbbm)
  cars$index <-as.character(cars$index )
  cars$ID <-as.character(cars$ID )
  cars$nextcar <-as.character(cars$nextcar )
  cars$nextsbbm <-as.character(cars$nextsbbm )
  cars <- cars[which( cars$sbbm!=cars$nextsbbm),]
  f1<-c(cars$diff_time)
  #参数z
  f1[which(f1<z)] <-0
  f1[which(f1>=z)] <-1
  f1<-data.frame(f1)
  segment<-f1[,c('f1')]
  segment<-as.numeric(segment)
  j<-0
  cc<-NULL
  c<-NULL
  for( i in 1: length(segment))
  {
    c<-c(c,j)
    if(segment[i]==1)
    {
      j<-j+1
    }
    if(i%%10000==0)
    {
      cc<-c(cc,c)
      c<-NULL
      # print(i)
    }

  }
  cc<-c(cc,c)
  cc<-data.frame(cc)
  cars<-cbind(cars,cc)
  cars<-unite(cars,"index_cc",c("index","cc"), sep="-", remove = F)
  g_car<-group_by(cars,index_cc)
  g_car<-summarise(g_car,count=n())
  g_car[1:100,]
  plot(sort(g_car$count))
  #参数e和f
  # g_car <- g_car[-which(g_car$count>e),]}
  g_car <- g_car[-which(g_car$count<=f),]
  track<-merge(cars,g_car,by = "index_cc", all = FALSE)
  track<-filter(track,diff_time!=0)
  track$abstime<-unclass(as.POSIXct(track$time))
  g_OD<-group_by(track,index_cc)
  g_OD_start<-summarise(g_OD,abstime=min(abstime))
  g_OD_end<-summarise(g_OD,abstime=max(abstime))
  g_OD_time<-rbind(g_OD_start,g_OD_end)
  OD_order_1<-g_OD_time[order(g_OD_time$index_cc,g_OD_time$abstime ),]
  OD_order_2<-g_OD_time[order(g_OD_time$index_cc,-g_OD_time$abstime ),]
  OD<-cbind(OD_order_1,OD_order_2)
  OD<-OD[,-c(3)]
  names(OD)<-c('index_cc','t1','t2')
  OD<-with(OD,OD[t2>t1,])
  OD_and_track<-merge(track,OD,by= 'index_cc')
  tmp<-filter(OD_and_track,abstime==t1 | abstime ==t2)
  tmp_s<-tmp[order(tmp$index_cc,tmp$abstime ),]
  tmp_e<-tmp[order(tmp$index_cc,-tmp$abstime ),]
  tmp_s<-tmp_s[,c("index_cc","sbbm","abstime"),]
  tmp_e<-tmp_e[,c("index_cc","sbbm","abstime"),]
  tmp<-cbind(tmp_s,tmp_e)
  names(tmp)<-c('index_s','sbbm_s','abstime_s','index_e','sbbm_e','abstime_e')
  tmp<-filter(tmp,abstime_e>abstime_s)
  tmp<-tmp[,c('index_s','sbbm_s','sbbm_e')]
  names(tmp)<-c('index_cc','sbbm_s','sbbm_e')
  OD_and_track<-merge(tmp,OD_and_track,by= 'index_cc', all = F)
  OD_and_track<-filter(OD_and_track,OD_and_track$sbbm_s!=OD_and_track$sbbm_e)
  OD_and_track$total_time<-OD_and_track$t2-OD_and_track$t1
  OD_and_track<-filter(OD_and_track,OD_and_track$total_time<6000 & OD_and_track$total_time>1000)
  test<-OD_and_track
  ######################
  test = test[,c(1,2,3,4,5,6)]
  colnames(test)[1] = 'index'
  colnames(test)[2] = 'SBBM_S'
  colnames(test)[3] = 'SBBM_E'
  colnames(test)[4] = 'SBBM'
  return(test)
  #输出dataframe格式:index chr---SBBM_S chr---SBBM_E chr---SBBM chr---ID chr---time chr
  #数据样本:100-13 510700000000010074 510700000000010036 510700000000010036 川A24BY5 2019-11-01 08:56:04
}#first和second的轨迹提取办法 必要要先跑yeartime_2019函数
MappingSBBM<-function(test,file1,file2){
  library(xlsx)
  #输入dataframe格式:index chr---SBBM_S chr---SBBM_E chr---SBBM chr---ID chr---time chr
  #数据样本:100-13 510700000000010074 510700000000010036 510700000000010036 川A24BY5 2019-11-01 08:56:04
  y <- read.xlsx(file1,1,encoding = "UTF-8")
  #"biao/shebei.xlsx"
  SBBM <- y$SBBM
  y$SBBM_S <-SBBM
  y$SBBM_E <-SBBM


  filter_SBBM <- function(x,y){
    x$SBBM = as.character(x$SBBM)
    y$SBBM = as.character(y$SBBM)
    y$CROSS_ID = as.character(y$CROSS_ID)
    a = unique(y$SBBM)
    x = x[which(x$SBBM %in% a),]
    return(x)
  }
  filter_SBBM_S <- function(x,y){
    x$SBBM_S = as.character(x$SBBM_S)
    y$SBBM_S = as.character(y$SBBM_S)
    y$CROSS_ID = as.character(y$CROSS_ID)
    a = unique(y$SBBM_S)
    x = x[which(x$SBBM_S %in% a),]
    return(x)
  }
  filter_SBBM_E <- function(x,y){
    x$SBBM_E = as.character(x$SBBM_E)
    y$SBBM_E = as.character(y$SBBM_E)
    y$CROSS_ID = as.character(y$CROSS_ID)
    a = unique(y$SBBM_E)
    x = x[which(x$SBBM_E %in% a),]
    return(x)
  }

  a1 = filter_SBBM(test,y)#
  a2 = filter_SBBM_S(a1,y)
  a3 = filter_SBBM_E(a2,y)


  add_crossID <- function(x,y){
    x$SBBM = as.character(x$SBBM)
    y$SBBM = as.character(y$SBBM)
    y$CROSS_ID = as.character(y$CROSS_ID)
    b = unique(x$SBBM)
    c = y[which(y$SBBM %in% b),]
    c = c[,c("SBBM","CROSS_ID")]
    x = merge(x,c)
    return(x)
  }
  add_crossID_S <- function(x,y){
    x$SBBM_S = as.character(x$SBBM_S)
    y$SBBM_S = as.character(y$SBBM_S)
    y$CROSS_ID = as.character(y$CROSS_ID)
    b = unique(x$SBBM_S)
    c = y[which(y$SBBM_S %in% b),]
    c = c[,c("SBBM_S","CROSS_ID")]
    colnames(c)[2] = 'CROSS_ID_S'
    x = merge(x,c)
    return(x)
  }
  add_crossID_E <- function(x,y){
    x$SBBM_E = as.character(x$SBBM_E)
    y$SBBM_E = as.character(y$SBBM_E)
    y$CROSS_ID = as.character(y$CROSS_ID)
    b = unique(x$SBBM_E)
    c = y[which(y$SBBM_E %in% b),]
    c = c[,c("SBBM_E","CROSS_ID")]
    colnames(c)[2] = 'CROSS_ID_E'
    x = merge(x,c)
    return(x)
  }

  d1 = add_crossID(a3,y) #CROSS_ID
  d2 = add_crossID_S(d1,y)
  d3 = add_crossID_E(d2,y)

  roadNet <- read.csv(file2,sep = ',',header = T)
  #"biao/CBD_RoadNode.csv"
  colnames(roadNet)[1] = 'newKmlID'

  colnames(roadNet)[4] = 'CROSS_ID'
  CROSS_ID <- roadNet$CROSS_ID
  roadNet$CROSS_ID_S <-CROSS_ID
  roadNet$CROSS_ID_E <-CROSS_ID

  add_newkmlID <- function(x,y){
    x$CROSS_ID = as.numeric(x$CROSS_ID)
    y$CROSS_ID = as.numeric(y$CROSS_ID)
    b = unique(x$CROSS_ID)
    c = y[which(y$CROSS_ID %in% b),]
    c = c[,c("CROSS_ID","newKmlID")]
    x = merge(x,c)
    x = x[order(x$index,x$time),]
  }
  add_newkmlID_S <- function(x,y){
    x$CROSS_ID_S = as.numeric(x$CROSS_ID_S)
    y$CROSS_ID_S = as.numeric(y$CROSS_ID_S)
    b = unique(x$CROSS_ID_S)
    c = y[which(y$CROSS_ID_S %in% b),]
    c = c[,c("CROSS_ID_S","newKmlID")]
    colnames(c)[2] = 'newKmlID_S'
    x = merge(x,c)
    x = x[order(x$index,x$time),]
  }
  add_newkmlID_E <- function(x,y){
    x$CROSS_ID_E = as.numeric(x$CROSS_ID_E)
    y$CROSS_ID_E = as.numeric(y$CROSS_ID_E)
    b = unique(x$CROSS_ID_E)
    c = y[which(y$CROSS_ID_E %in% b),]
    c = c[,c("CROSS_ID_E","newKmlID")]
    colnames(c)[2] = 'newKmlID_E'
    x = merge(x,c)
    x = x[order(x$index,x$time),]
  }
  test1 = add_newkmlID(d3,roadNet)
  test2 = add_newkmlID_S(test1,roadNet)
  test9 = add_newkmlID_E(test2,roadNet)


  S <- test9$newKmlID_S
  E <- test9$newKmlID_E
  X <- paste0(S,"-",E)
  test9$ODID<-X


  test<-test9[,c(7,9,10,13)]
  #输出dataframe格式:index chr---time chr---newKmlID chr---ODID chr
  #数据样本:100-13 2019-11-01 08:25:19       80 80-35
}#匹配表中数据
TrajFillTrajset<-function(test,x,y,file1){
  library(dplyr)
  library(tidyr)
  library(xlsx)
  library(igraph)
  #输入dataframe格式:index chr---time chr---newKmlID chr---ODID chr
  #数据样本:100-13 2019-11-01 08:25:19       80 80-35
  # eee<-"2019-11-01 08:25:19"
  # substr(eee,1,10)
  timeplan<-test$time[1]
  timeplan<-substr(timeplan,1,10)
  #x<-5 为最短OD长度
  #y<-"08"为时间
  y_open<-paste0(y,"_")
  colnames(test)[3] = 'newkmlID'
  rownames(test) <-c(1:nrow(test))
  result<-test

  g_test1<-group_by(result,ODID,index)
  g_test1<-summarise(g_test1,count=n())
  g_test1<-group_by(g_test1,ODID)
  g_test1<-summarise(g_test1,count=n())
  plot(sort(g_test1$count))
  g_test1 <- filter(g_test1,count>=x)#11111
  g <- merge(g_test1,result)
  track2 <- g[,-2]

  car<-track2
  for (i in 1:(nrow(car)-1)) {
    if(car[i,]$newkmlID != car[i+1,]$newkmlID){
      if(car[i,]$time == car[i+1,]$time){
        if(car[i,]$index == car[i+1,]$index){
          track2<-filter(track2 ,index!= car[i,]$index)
        }
      }
    }
  }

  track2$time <- as.numeric(as.POSIXct(track2$time))
  track2 = track2[order(track2$index,track2$time),]

  luwang <- read.csv(file1,header = T,sep=",")
  #"biao/CBD_RoadNet.csv"
  s <- data.frame(start = luwang$start,end = luwang$end,weight = luwang$distance)
  h <- make_graph(t(s[,1:2]+1),directed = FALSE)
  h <- set_graph_attr(h,'weight',s[,3])

  guiji_id  = as.character(unique(track2$index))
  guiji_id_num <- length(guiji_id)
  a<-0
  time_big<-c()
  index_big<-c()
  newkmlID_big<-c()
  ODID_big<-c()
  time_total<-c()
  index_total<-c()
  newkmlID_total<-c()
  ODID_total<-c()
  time<-c()
  index<-c()
  newkmlID<-c()
  ODID<-c()
  guiji<-c()
  time_num <-0
  time_bigT <-0
  time_whole<-c()
  index_whole<-c()
  newkmlID_whole<-c()
  ODID_whole<-c()

  guiji <- c()
  start<-as.numeric(0)
  end <-as.numeric(1)
  g_test1<-group_by(track2,index)
  g_test1<-summarise(g_test1,count=n())
  guiji_num <- as.numeric(g_test1$count)
  vv <- 0
  tt_num<-0
  time_start <- Sys.time()

  for(i in 1:guiji_id_num){

    # i<-1
    if(guiji_num[i]>1){
      start <- end
      end <- end+guiji_num[i]
      guiji <- track2[start:(end-1),]

      v <- i/guiji_id_num
      if(v >= vv){
        print(vv)
        time_end <- Sys.time()
        print(time_end-time_start)
        time_start<-time_end
        vv <- vv+0.01
      }

      for(j in 1:(guiji_num[i]-1)){
        # j<-1
        z <- guiji[j:(j+1),]
        by <- z[1,]
        by_1 <-z[nrow(z),]
        data <- as.numeric(as.character(z$newkmlID))
        a <- shortest_paths(h,data[1]+1,data[2]+1,weights = graph_attr(h,'weight'))
        a <- as.numeric(as.character(a$vpath[[1]]-1))
        if(length(a)>2){
          n <- length(a)
          n1<- length(unique(a))
          if(n==n1){


            a <- a[-1]
            a <- a[-length(a)]
            t_n <- length(a)
            t <- by
            n <- length(a)
            t <- t[rep(1,each=n),]
            t_t<- (by_1$time - by$time)
            class(by_1$time)
            t_x<-t_t/(t_n+1)
            if(t_x<1)
            {
              tt_num <- tt_num+1
            }
            t$newkmlID <- a

            y <- c((1:n)*t_x)
            t$time <- as.character(t$time+y)
            t$index<- as.character(t$index)

            time <- append(time,t$time)
            index <- append(index,t$index)
            newkmlID <- append(newkmlID,t$newkmlID)
            ODID <- append(ODID,t$ODID)
            rm(t)
            if(length(time)>10000){
              time_total<-append(time_total,time)
              index_total<-append(index_total,index)
              newkmlID_total<-append(newkmlID_total,newkmlID)
              ODID_total<-append(ODID_total,ODID)
              rm(time)
              rm(index)
              rm(newkmlID)
              rm(ODID)
              time<-c()
              index<-c()
              newkmlID<-c()
              ODID<-c()
              time_num <- time_num + 1
            }
            if(time_num >= 10){
              time_big<-append(time_big,time_total)
              index_big<-append(index_big,index_total)
              newkmlID_big<-append(newkmlID_big,newkmlID_total)
              ODID_big<-append(ODID_big,ODID_total)
              rm(time_total)
              rm(index_total)
              rm(newkmlID_total)
              rm(ODID_total)
              time_total<-c()
              index_total<-c()
              newkmlID_total<-c()
              ODID_total<-c()
              time_num <- 0
              time_bigT <- time_bigT + 1
            }
            if(time_bigT >= 10){
              time_whole<-append(time_whole,time_big)
              index_whole<-append(index_whole,index_big)
              newkmlID_whole<-append(newkmlID_whole,newkmlID_big)
              ODID_whole<-append(ODID_whole,ODID_big)
              rm(time_big)
              rm(index_big)
              rm(newkmlID_big)
              rm(ODID_big)
              gc()
              time_big<-c()
              index_big<-c()
              newkmlID_big<-c()
              ODID_big<-c()
              time_bigT <- 0
            }
          }
        }
      }

    }
  }
  time_total<-append(time_total,time)
  index_total<-append(index_total,index)
  newkmlID_total<-append(newkmlID_total,newkmlID)
  ODID_total<-append(ODID_total,ODID)

  time_big<-append(time_big,time_total)
  index_big<-append(index_big,index_total)
  newkmlID_big<-append(newkmlID_big,newkmlID_total)
  ODID_big<-append(ODID_big,ODID_total)

  time_whole<-append(time_whole,time_big)
  index_whole<-append(index_whole,index_big)
  newkmlID_whole<-append(newkmlID_whole,newkmlID_big)
  ODID_whole<-append(ODID_whole,ODID_big)

  time <- as.character(time_whole)
  index <- as.character(index_whole)
  newkmlID <- as.character(newkmlID_whole)
  ODID <- as.character(ODID_whole)
  xx = data.frame(ODID,time,index,newkmlID)
  track2 <- track2[,c(4,2,1,3)]
  track3 <- rbind(track2,xx)#
  rm(track2)
  gc()
  track3 <- track3[order(track3$index,track3$time),]
  test <- track3
  save(test,file=paste0("Data/00_Rdata/x/",y_open,timeplan,".rdata"))
  return(test)
  #输出dataframe格式:newkmlID chr---index chr---ODID chr---time chr
  #数据样本:80 100-13 80-35 1572567919
}#路网填补
Realtime_TrajGenerate<-function(x,y,year,file1,file2,file3){
  load(paste0("2019_day/",year[x],".rdata"))
  #x<-5 为最短OD长度
  #y<-"08"为时间
  #输入dataframe格式:sbbm chr---ID chr---time chr---ymd chr---ymd chr---h chr
  #数据样本:510700000000090048 川YQ6388 2019-11-01 08:06:06 2019-11-01 08
  track3<-filter(track3,h ==y)
  timeplan<-y
  cars<-track3
  basic_sbbm<-data.frame(basic_sbbm)
  names(basic_sbbm)<-c('sbbm')
  basic_sbbm$sbbm<-as.character(basic_sbbm$sbbm)

  basic_sbbm<-data.frame(unique(basic_sbbm))
  cars<-merge(basic_sbbm,cars,by= "sbbm", all = F)
  cars<-filter(cars,ID!="污损车牌")
  cars<-filter(cars,ID!="污损牌照")
  cars<-filter(cars,ID!="未识别")
  #CROSS_ID
  shebei <- read.xlsx(file1,1,encoding = "UTF-8")
  #"biao/shebei.xlsx"
  names(shebei)[4]= 'sbbm'
  shebei = shebei[,c(4,5)]
  shebei$sbbm = as.character(shebei$sbbm)
  shebei$CROSS_ID = as.character(shebei$CROSS_ID)
  cars$sbbm = as.character(cars$sbbm)
  cars = merge(cars,shebei,all.x = T)

  #KMLID
  road <- read.csv(file2,sep = ',',header = T)
  #"biao/CBD_RoadNode.csv"
  road_kml <- road[,c(4,1)]
  names(road_kml) = c("CROSS_ID","kml")
  road_kml <-filter(road_kml,CROSS_ID !="NA")
  cars = merge(cars,road_kml,all.x = T)
  cars<-filter(cars,kml!="NA")

  #鎸夎溅鐗屽垎杞ㄨ抗
  g<-group_by(cars,ID)
  index<-group_indices(g,ID)
  df<-data.frame(index)
  cars<-cbind(cars,df)

  car = cars
  #nb = unique(car$day)
  nb = unique(car$ymd)
  result = data.frame()

  g<-group_by(cars,index)
  b <- summarise(g,count=n())
  b<-filter(b, count==1)
  a = b$index
  cars = cars[-which(cars$index %in% a),]
  cars = cars[,c("index","time","kml")]
  c = cars
  c = c[order(c$index,c$time),]
  kml1 = c$kml
  kml1 = kml1[-1]
  time1 = c$time
  time1 = time1[-1]
  index1 = c$index
  index1 = index1[-1]
  c = c[-nrow(c),]
  c$time1 = time1
  c$index1 = index1
  c$kml1 = kml1
  c = c[-which(c$index != c$index1),]
  d = c[which(c$time == c$time1),]
  e = d[which(d$kml != d$kml1),]
  index = e$index
  cars = cars[-which(cars$index %in% index),]


  #濉ˉ
  track2<-cars
  # rm(cars)
  # gc()
  colnames(track2)[3] = 'newkmlID'
  rownames(track2) <-c(1:nrow(track2))

  track2$time <- as.numeric(as.POSIXct(track2$time))
  track2 = track2[order(track2$index,track2$time),]

  #

  luwang <- read.csv(file3,header = T,sep=",")
  #"biao/CBD_RoadNet.csv"
  s <- data.frame(start = luwang$start,end = luwang$end,weight = luwang$distance)
  h <- make_graph(t(s[,1:2]+1),directed = FALSE)
  h <- set_graph_attr(h,'weight',s[,3])
  #id
  guiji_id  = as.character(unique(track2$index))
  guiji_id_num <- length(guiji_id)
  a<-0
  time_big<-c()
  index_big<-c()
  newkmlID_big<-c()

  time_total<-c()
  index_total<-c()
  newkmlID_total<-c()

  time<-c()
  index<-c()
  newkmlID<-c()

  guiji<-c()
  time_num <-0
  time_bigT <-0
  time_whole<-c()
  index_whole<-c()
  newkmlID_whole<-c()

  #data.frame
  guiji <- c()
  start<-as.numeric(0)
  end <-as.numeric(1)
  g_test1<-group_by(track2,index)
  g_test1<-summarise(g_test1,count=n())
  guiji_num <- as.numeric(g_test1$count)
  vv <- 0
  tt_num<-0
  time_start <- Sys.time()
  for(i in 1:guiji_id_num){
    # i<-1

    if(guiji_num[i]>1){
      start <- end
      end <- end+guiji_num[i]
      guiji <- track2[start:(end-1),]

      v <- i/guiji_id_num
      if(v >= vv){
        print(vv)
        time_end <- Sys.time()
        print(time_end-time_start)
        time_start<-time_end
        vv <- vv+0.01
      }

      for(j in 1:(guiji_num[i]-1)){
        j<-1
        z <- guiji[j:(j+1),]
        by <- z[1,]
        by_1 <-z[nrow(z),]
        data <- as.numeric(as.character(z$newkmlID))
        a <- shortest_paths(h,data[1]+1,data[2]+1,weights = graph_attr(h,'weight'))
        a <- as.numeric(as.character(a$vpath[[1]]-1))
        if(length(a)>2){
          n <- length(a)
          n1<- length(unique(a))
          if(n==n1){


            a <- a[-1]
            a <- a[-length(a)]
            t_n <- length(a)
            t <- by
            n <- length(a)
            t <- t[rep(1,each=n),]
            t_t<- (by_1$time - by$time)#鏍煎紡闂
            class(by_1$time)
            t_x<-t_t/(t_n+1)
            if(t_x<1)
            {
              tt_num <- tt_num+1
            }
            t$newkmlID <- a

            y <- c((1:n)*t_x)
            t$time <- as.character(t$time+y)
            t$index<- as.character(t$index)

            time <- append(time,t$time)
            index <- append(index,t$index)
            newkmlID <- append(newkmlID,t$newkmlID)
            # ODID <- append(ODID,t$ODID)
            # rm(t)
            if(length(time)>10000){
              time_total<-append(time_total,time)
              index_total<-append(index_total,index)
              newkmlID_total<-append(newkmlID_total,newkmlID)
              # ODID_total<-append(ODID_total,ODID)
              rm(time)
              rm(index)
              rm(newkmlID)
              # rm(ODID)
              time<-c()
              index<-c()
              newkmlID<-c()
              # ODID<-c()
              time_num <- time_num + 1
            }
            if(time_num >= 10){
              time_big<-append(time_big,time_total)
              index_big<-append(index_big,index_total)
              newkmlID_big<-append(newkmlID_big,newkmlID_total)
              # ODID_big<-append(ODID_big,ODID_total)
              rm(time_total)
              rm(index_total)
              rm(newkmlID_total)
              # rm(ODID_total)
              time_total<-c()
              index_total<-c()
              newkmlID_total<-c()
              # ODID_total<-c()
              time_num <- 0
              time_bigT <- time_bigT + 1
            }
            if(time_bigT >= 10){
              time_whole<-append(time_whole,time_big)
              index_whole<-append(index_whole,index_big)
              newkmlID_whole<-append(newkmlID_whole,newkmlID_big)
              # ODID_whole<-append(ODID_whole,ODID_big)
              rm(time_big)
              rm(index_big)
              rm(newkmlID_big)
              # rm(ODID_big)
              gc()
              time_big<-c()
              index_big<-c()
              newkmlID_big<-c()
              # ODID_big<-c()
              time_bigT <- 0
            }
          }
        }
      }

    }
  }
  time_total<-append(time_total,time)
  index_total<-append(index_total,index)
  newkmlID_total<-append(newkmlID_total,newkmlID)
  # ODID_total<-append(ODID_total,ODID)

  time_big<-append(time_big,time_total)
  index_big<-append(index_big,index_total)
  newkmlID_big<-append(newkmlID_big,newkmlID_total)
  # ODID_big<-append(ODID_big,ODID_total)

  time_whole<-append(time_whole,time_big)
  index_whole<-append(index_whole,index_big)
  newkmlID_whole<-append(newkmlID_whole,newkmlID_big)
  # ODID_whole<-append(ODID_whole,ODID_big)

  time <- as.character(time_whole)
  index <- as.character(index_whole)
  newkmlID <- as.character(newkmlID_whole)
  # ODID <- as.character(ODID_whole)
  xx = data.frame(time,index,newkmlID)
  # track2 <- track2[,c(4,2,1,3)]
  track3 <- rbind(track2,xx)
  track3$index<- as.numeric(track3$index)

  track3 <- track3[order(track3$index,track3$time),]


  test <- track3
  rm(track2)
  rm(track3)
  gc()


  test$time = as.numeric(test$time)
  test$time = as.POSIXct(test$time,origin="1970-01-01 00:00:00")
  names(test)[3]= "kml"
  cars = test
  cars = separate(cars,time,c("ymd","time1"),sep = " ",remove = F)
  #cars = separate(cars,ymd,c("y","month","day"),sep = "-",remove = T)
  cars = separate(cars,time1,c("h","m","s"),sep = ":",remove = T)
  #cars = cars[,c("index","time","day","h","kml")]
  cars = cars[,c("index","time","ymd","kml")]

  time1 = cars$time
  time1 = time1[-1]
  kml1 = cars$kml
  kml1 = kml1[-1]
  index1 = cars$index
  index1 = index1[-1]
  # day1 = cars$day
  # day1 = day1[-1]
  ymd1 = cars$ymd
  ymd1 = ymd1[-1]
  cars = cars[-nrow(cars),]
  cars$time1 = time1
  cars$kml1 = kml1
  cars$index1 = index1
  #cars$day1 = day1
  cars$ymd1 = ymd1
  cars = cars[-which(cars$index != cars$index1),]

  cars = unite(cars,'kml_s_e',c("kml","kml1"),sep = "-",remove = T)
  #g<-group_by(cars,kml_s_e,day,h)
  g<-group_by(cars,kml_s_e,ymd)
  b <- summarise(g,count=n())
  b = as.data.frame(b)
  #b = unite(b,"kml-d-h",c("kml_s_e","day","h"),sep = "-",remove = F)
  b = unite(b,"kml-ymd",c("kml_s_e","ymd"),sep = "-",remove = F)


  road1 <- read.csv(file3,sep = ',',header = T)
  #"biao/CBD_RoadNet.csv"
  road1<-road1[,c(1,2)]
  s = road1$end
  e = road1$start
  a = data.frame(start = s,end = e)
  road1 = rbind(road1,a)
  road1 = unite(road1,'kml_s_e',c("start","end"),sep = '-',remove = T)
  c = merge(b,road1,all = F)


  #day = unique(b$day)
  ymd = unique(b$ymd)
  #day = data.frame(day = day)
  ymd = data.frame(ymd = ymd)
  a = merge(road1,ymd,all = T)
  a$count = 0
  #a = unite(a,"kml-d-h",c("kml_s_e","day","h"),sep = "-",remove = F)
  a = unite(a,"kml-ymd",c("kml_s_e","ymd"),sep = "-",remove = F)
  #a1 = c$`kml-d-h`
  a1 = c$`kml-ymd`
  a = a[-which(a$`kml-ymd` %in% a1),]

  c = rbind(c,a)
  c = c[,c('kml_s_e','ymd','count')]

  result = rbind(result,c)



  result = result[order(result$kml_s_e,result$ymd),]
  timeplan<-paste0(timeplan,"_")
  #生成对比表
  save(result,file=paste0("Data/00_Rdata/y/",timeplan,year[x],".rdata"))
  return(result)
}#车牌划分生成对比表 必要先跑yeartime_2019函数
TopK<-function(x,y,z,year){
  #输入为11月的某号区间
  CountX<-c()
  CountY<-c()
  CorNum<-c()
  CountT<-c()
  z<-paste0(z,"_")
  for(xyz in x:y){
    # xyz<-4
    load(paste0("Data/00_Rdata/x/",z,year[xyz],".rdata"))
    #filter_road的输出 test
    #输入dataframe格式:newkmlID chr---index chr---ODID chr---time chr
    #数据样本:80 100-13 80-35 1572567919
    load(paste0("Data/00_Rdata/y/",z,year[xyz],".rdata"))
    #filter_car的输出 result
    #对比表
    test$time <- as.numeric(test$time)
    test$time <- as.POSIXct(test$time,origin="1970-01-01 00:00:00")

    test1<-test[-1,]$newkmlID
    test1<-append(test1,-1)
    test2<-test[-1,]$index
    test2<-append(test2,-1)
    test$newkmlID1<-test1
    test$index1<-test2
    test3<-filter(test,index==index1)
    test3<-unite(test3,"kml-kml1",c("newkmlID","newkmlID1"),sep = "-",remove = F)


    ts<-group_by(test3,ODID)
    ts<-summarise(ts,count=n())
    colnames(test3)[1]<-'kml'
    xy<-group_by(test3,kml)
    xy<-summarise(xy,count=n())

    xx<-result

    xx$kml<-as.character(xx$kml)

    xz<-merge(xx,xy,by='kml')

    library(ggplot2)

    library("corrplot")

    # install.packages("corrplot")
    corr <- cor(xz[,4:5])
    corr
    corrplot(corr = corr)
    corrplot(corr = corr,order="AOE",type="upper",tl.pos="tp")


    corrplot(corr = corr,add=TRUE, type="lower", method="number",order="AOE", col="black",diag=FALSE,tl.pos="n", cl.pos="n")
    CorNum<-c(CorNum,cor(xz$count.x,xz$count.y,method = "pearson"))

    x1<-xz[order(-xz$count.x),]
    x1<-x1[1:20,]
    x1[20,"count.x"]
    x2<-xz[order(-xz$count.y),]
    x2<-x2[1:20,]
    x2[20,"count.y"]
    a<-0
    for(i in 1:20){

      if(x2[i,"count.x"]>=x1[20,"count.x"]){
        a<-a+1
      }
    }
    b<-0
    for(i in 1:20){

      if(x1[i,"count.y"]>=x2[20,"count.y"]){
        b<-b+1
      }
    }
    CountX<-c(CountX,a/20)
    CountY<-c(CountY,b/20)
    CountT<-c(CountT,x2$ymd[1])
  }
  # CountT<-as.character(CountT)
  total<-data.frame(CountT,CountX,CountY,CorNum)
  total$CountT<-as.character(total$CountT)
  #输入dataframe格式:CountT chr---CountX chr---CountY chr---CorNum chr
  #数据样本:2019-11-02   0.45   0.45 0.7517095
  # save(total,file=("OD周末2/2-3_08相关系数.rdata"))

  save(total,file=paste0("Data/00_Rdata/相关系数/",year[x],"-",year[y],".rdata"))
  return(total)
}
Calc_K<-function(year,x){
  CountX<-c()
  CountY<-c()
  Countk1<-c()
  Countk2<-c()
  CountT<-c()
  x<-paste0(x,"_")
  for(xyz in 1:length(year)){
    # xyz<-4
    load(paste0("Data/00_Rdata/x/",x,year[xyz],".rdata"))

    #filter_road的输出 test
    #输入dataframe格式:newkmlID chr---index chr---ODID chr---time chr
    #数据样本:80 100-13 80-35 1572567919
    # load(paste0("y_9/18_",year[xyz],".rdata"))
    load(paste0("Data/00_Rdata/y/",x,year[xyz],".rdata"))
    #filter_car的输出 result
    #对比表

    test$time <- as.numeric(test$time)
    test$time <- as.POSIXct(test$time,origin="1970-01-01 00:00:00")

    test1<-test[-1,]$newkmlID
    test1<-append(test1,-1)
    test2<-test[-1,]$index
    test2<-append(test2,-1)
    test$newkmlID1<-test1
    test$index1<-test2
    test3<-filter(test,index==index1)
    test3<-unite(test3,"kml-kml1",c("newkmlID","newkmlID1"),sep = "-",remove = F)


    ts<-group_by(test3,ODID)
    ts<-summarise(ts,count=n())
    colnames(test3)[1]<-'kml'
    xy<-group_by(test3,kml)
    xy<-summarise(xy,count=n())

    xx<-result

    xx$kml<-as.character(xx$kml)

    xz<-merge(xx,xy,by='kml')

    x1<-xz[order(-xz$count.x),]
    x1<-x1[1:20,]
    x2<-xz[order(-xz$count.y),]
    x2<-x2[1:20,]
    x1$k<- x1$count.x/x1$count.y
    x2$k<- x2$count.x/x2$count.y
    k1<-median(x1$k)
    k2<-median(x2$k)
    Countk1<-c(Countk1,k1)
    Countk2<-c(Countk2,k2)
    CountT<-c(CountT,x1$ymd[1])
  }
  total<-data.frame(CountT,Countk1,Countk2)
  total$k<-round((total$Countk1+total$Countk2)/2)
  # save(total,file=("OD周末2/K2值.rdata"))
  #输入dataframe格式:CountT chr---Countk1 chr---Countk2 chr---k chr
  #数据样本:2019-11-02 5.174434 4.842699 5
  total_k<-total
  return (total_k)
}
OD_Generate<-function(year,total_k,x){
  test_total<-c()
  x<-paste0(x,"_")
  for(xyz in  1:length(year)){
    load(paste0("Data/00_Rdata/x/",x,year[xyz],".rdata"))
    test_total<-rbind(test_total,test)
  }
  test_total <- test_total[order(test_total$index,test_total$time),]
  k<-ceiling(mean(total_k$k))
  ODN <-unique(test_total$ODID)
  # x<-as.data.frame(ODN)
  num_OD <- length(ODN)
  for(i in 1:num_OD){
    # i<-1
    ODS <-c()
    ODS <-test_total[which(test_total$ODID == ODN[i]),]
    ODS <- ODS[order(ODS$index,ODS$time),]
    g_test1<-group_by(ODS,index)
    g_test1<-summarise(g_test1,num=n())
    ODS<-merge(g_test1,ODS,by  = "index",all=T)

    test1<-ODS[-1,]$newkmlID
    test1<-append(test1,-1)
    test2<-ODS[-1,]$index
    test2<-append(test2,-1)
    ODS$newkmlID1<-test1
    ODS$index1<-test2
    test3<-filter(ODS,index==index1)
    test3<-unite(test3,"newkmlID",c("newkmlID","newkmlID1"),sep = "_",remove = T)
    ODS<-test3[,c(1,2,3,4,5)]
    ODS$num<-ODS$num-1
    Op<-ODS
    Oq<-Op
    for(j in 1:(k-1)){
      Op<-Oq
      Op$index<-paste0(Op$index,"_",j)
      ODS<-rbind(ODS,Op)
    }

    name<-as.character(unique(ODS$ODID) )
    write.csv(ODS,file=paste0("Data/02_ODcsv/",name,".csv"),row.names=FALSE)
  }

}
python_fenglu<-function(x,y){
  # y<-"22_27=27_26,44_43=43_41"
  Fenglu<-c(paste(x,y))

  # 两个参数 第一个是项目工作目录，第二个是封路方案
  # flag <- try(system(paste0('python E:/CBD_2020/fenglu/run_this.py ',Fenglu), intern = TRUE))
  flag <- try(system(paste0('python ',x,'fenglu/run_this.py ',Fenglu), intern = TRUE))
  print(flag)
}
Heatmap <- function(x,y){
  library(dplyr)
  library(tidyr)
  library(leaflet)

  result1 = x
  names(result1) = c("newkmlID","count")
  result1 = separate(result1,newkmlID,c("kml","kml1"),sep = "_",remove = T)

  #匹配经纬度
  road1 <- y
  road = road1[,c(2,3,1)]
  names(road) = c("Lng","Lat","kml")
  a = duplicated(road$kml)
  road <- road[!a,]
  result1 = merge(result1,road,all.x = T)
  names(road) = c("Lng_e","Lat_e","kml1")
  result1 = merge(result1,road,all.x = T)

  #根据车流量划分交通拥堵等级并匹配相应等级颜色
  c<-cut(result1$count,breaks = c(-1,100,500,800,2000),labels = c('畅通','基本畅通','拥堵','严重拥堵'),right = T)
  result1$State = c
  color = c("#FF0000", "#FFAA00", "#AAFF00" ,"#00FF00")
  State = c('严重拥堵','拥堵','基本畅通','畅通')
  df_Color = data.frame(color = color,State = State)
  result1 = merge(result1,df_Color,all.x = T)
  result1 = result1[order(result1$count,decreasing = F),]

  #可视化
  map = leaflet(result1)  %>%  addTiles()
  for (i in 1:nrow(result1)) {
    map <- addPolylines(map,lng=c(result1[i,'Lng'],result1[i,'Lng_e']), lat = c(result1[i,'Lat'],result1[i,'Lat_e']),
                        color = as.character(result1[i, c('color')])
    )
  }
  map
}
Shiny_Heatmap <- function(x,y){
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(tidyr)
  ui <- fluidPage(
    column(6,
           h3("封路前流量"),
           leafletOutput("leaf_num1",height = 1000)
    ),
    column(6,
           h3("封路后流量"),
           leafletOutput("leaf_num2",height = 1000)
    )
  )

  server <- function(input, output) {
    #左边图
    output$leaf_num1 <- renderLeaflet({
      x
    })

    #右边图
    output$leaf_num2 <- renderLeaflet({
      y
    })
  }
  shinyApp(ui, server)
}
Bad_carID <- function(x,shebei,lukou){
  library(leaflet)
  library(dplyr)
  library(tidyr)
  h = x
  h$wusun = h$count1.x -h$count1.y
  shebei <- shebei
  names(shebei)[4]= 'sbbm'
  shebei = shebei[,c(4,5)]
  shebei$sbbm = as.character(shebei$sbbm)
  shebei$CROSS_ID = as.character(shebei$CROSS_ID)

  h1 = merge(h,shebei,all.x = T)
  h1 = filter(h1,CROSS_ID != "Na")

  road <- lukou
  road = road[,c(1,3,4)]
  names(road)= c('CROSS_ID','Lng','Lat')

  h1 = merge(h1,road,all.x = T)
  h1$Lng = as.character(h1$Lng)
  h1$Lat = as.character(h1$Lat)
  #h1 = h1[1:20,]
  h1$Lng = as.numeric(h1$Lng)
  h1$Lat = as.numeric(h1$Lat)
  h1$b = paste(round(h1$b,2)*100,"%")

  h1 = h1[,c(1,3,5,6,7,8)]

  h1 = unite(h1,"dis_wusun",c("wusun","count1.x","b"),sep = "_")
  df1 = data.frame(CROSS_ID = unique(h1$CROSS_ID))
  wusun = c(h1[1,2])
  dis_wusun = c()
  for (i in 1:(nrow(h1)-1)){

    if (h1[i,1]== h1[i+1,1]){
      wusun = paste(wusun,h1[i+1,2],sep = " ")
    }else{
      if (length(wusun)==0){
        wusun = paste(wusun,h1[i+1,2],sep = " ")
      }
      #wusun = paste(wusun,h1[i+1,2],sep = " ")

      dis_wusun = append(dis_wusun,wusun)
      wusun = c(h1[i+1,2])
    }
  }
  dis_wusun = append(dis_wusun,wusun)

  df1$wusun = dis_wusun
  df1$Lng = unique(h1$Lng)
  df1$Lat = unique(h1$Lat)

  map = leaflet(df1)  %>%  addTiles()
  for (i in 1:nrow(df1)) {
    map <- addLabelOnlyMarkers(map,lng=df1[i,'Lng'], lat = df1[i,'Lat'],
                               label = as.character(df1[i,'wusun']),labelOptions = labelOptions(noHide = T,
                                                                                                direction = 'top',
                                                                                                testOnly = T)
    )
  }
  map
}
