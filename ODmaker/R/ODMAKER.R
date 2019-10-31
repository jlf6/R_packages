#c("sbbm","ID","time")
odmaker<-function(cars){
library(dplyr)
library(tidyr)
#cars = x
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


##############filters#############
cars$index <-as.character(cars$index )
cars$ID <-as.character(cars$ID )
cars$nextcar <-as.character(cars$nextcar )
cars$nextsbbm <-as.character(cars$nextsbbm )
cars <- cars[which( cars$sbbm!=cars$nextsbbm),]

f1<-c(cars$diff_time)
f1[which(f1<3000)] <-0
f1[which(f1>=3000)] <-1
f1<-data.frame(f1)

###########add sub index###
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
###########add sub index###

cars<-unite(cars,"index_cc",c("index","cc"), sep="-", remove = F)

g_car<-group_by(cars,index_cc)
g_car<-summarise(g_car,count=n())


#g_car <- g_car[-which(g_car$count>30),]
g_car <- g_car[-which(g_car$count<=4),]

track<-merge(cars,g_car,by = "index_cc", all = FALSE)

cars<-filter(cars,ID!="污损车牌")
cars<-filter(cars,ID!="污损牌照")
cars<-filter(cars,ID!="未识别")


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
#tmp_s<-tmp[c(1:nrow(tmp)-1),]
tmp_e<-tmp[order(tmp$index_cc,-tmp$abstime ),]
#tmp_e<-tmp[c(2:nrow(tmp)),]
tmp_s<-tmp_s[,c("index_cc","sbbm","abstime"),]
tmp_e<-tmp_e[,c("index_cc","sbbm","abstime"),]
tmp<-cbind(tmp_s,tmp_e)
names(tmp)<-c('index_s','sbbm_s','abstime_s','index_e','sbbm_e','abstime_e')
tmp<-filter(tmp,abstime_e>abstime_s)
tmp<-tmp[,c('index_s','sbbm_s','sbbm_e')]
names(tmp)<-c('index_cc','sbbm_s','sbbm_e')
OD_and_track<-merge(tmp,OD_and_track,by= 'index_cc', all = F)
#######################filter cycle track#############
OD_and_track<-filter(OD_and_track,OD_and_track$sbbm_s!=OD_and_track$sbbm_e)
#######################filter long time track#############
OD_and_track$total_time<-OD_and_track$t2-OD_and_track$t1
OD_and_track<-filter(OD_and_track,OD_and_track$total_time<6000 & OD_and_track$total_time>1000)


test<-OD_and_track
}

