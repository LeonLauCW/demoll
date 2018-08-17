#######-----城市公交站点设置的优化分析------#########

#####---画折线图（普通画法）---#####
setwd('C:\\Users\\59559\\Documents')
time09 <- read.csv('./data/time/time09.csv', stringsAsFactors = F)
time10 <- read.csv('./data/time/time10.csv', stringsAsFactors = F)
time11 <- read.csv('./data/time/time11.csv', stringsAsFactors = F)
time12 <- read.csv('./data/time/time12.csv', stringsAsFactors = F)
time13 <- read.csv('./data/time/time13.csv', stringsAsFactors = F)

plot(time09[,1], time09[,2], col=5, type='o', xaxt='n', yaxt='n', 
     xlim = c(5,23), ylim=c(0,550000), xlab='时间点', ylab='刷卡人数（单位：万')
title(main = '图例')

lines(time10[,1], time10[,2], col=1, type='o')
lines(time11[,1], time11[,2], col=2, type='o')
lines(time12[,1], time12[,2], col=3, type='o')
lines(time13[,1], time13[,2], col=4, type='o')

axis(side=1, at=c(5,10,15,20), labels=c('5:00','10:00','15:00','20:00')) # x轴
axis(side=2, at=c(0,100000,200000,300000,400000,500000), 
     labels=c('0','20','30','40','50','60')) # y轴
legend('top', lty=c(1), legend=c('2014-06-09','2014-06-10','2014-06-11',
                                      '2014-06-12','2014-06-13'),
       col=c(1,2,3,4,5), bty='n') #bty=”o”默认的，=”n”表示没有边框




#####---画折线图（循环画法）---######
setwd('C:\\Users\\59559\\Documents\\data\\time')
plot(time09[,1], time09[,2], col=5, type='o', xaxt='n', yaxt='n', 
     xlim = c(5,23), ylim=c(0,550000), xlab='时间点', ylab='刷卡人数（单位：万')
file <- list.files()
d <- paste('./', file, sep='')
for (i in 2:length(d)) {
  time <- read.csv(file = d[i])
  lines(time[,1], time[,2], col=i-1)
}
axis(side=1, at=c(5,10,15,20), labels=c('5:00','10:00','15:00','20:00')) # x轴
axis(side=2, at=c(0,100000,200000,300000,400000,500000), 
     labels=c('0','20','30','40','50','60')) # y轴
legend('top', lty=c(1), legend=c('2014-06-09','2014-06-10','2014-06-11',
                                 '2014-06-12','2014-06-13'),
       col=c(1,2,3,4,5), bty='n') #bty=”o”默认的，=”n”表示没有边框





#####----数据处理----#####
setwd('D:\\学习\\数据挖掘\\data')
library(plyr)
data <- read.csv('./gps/gps_20140609.csv', header=T)
data <- ddply(data, .(业务时间,卡片记录编码,车牌号), tail, n=1) # 去重
x <- which(data[,5] == '68路') # 取出68路的数据
y <- data[x,]
# z <- data[grep('68路', data[,5]),] # 方法2（y=z）

write.csv(y, './gjc.csv', row.names = F)

# 批量读入去重
file <- list.files()
shuju <- list.files(file[which(file=='gps')])
d <- paste('./', 'gps', '/' , shuju, sep='')
for (i in 2:length(d)) {
  data <- read.csv(file = d[i], stringsAsFactors = F) # 使用stringsAsFactors=F来防止data.frame把向量转为factor
  data <- ddply(data, .(业务时间,卡片记录编码,车牌号), tail, n=1) # 去重
  x <- which(data[,5] == '68路') # 取出68路的数据
  y <- data[x,]
  write.table(y, './gjc.csv', append=T, sep=',', row.names=F, col.names=F)
}



######----构建模型----########

###### DBSCAN算法聚类
library(fpc)
setwd('D:\\学习\\数据挖掘\\data')
data <- read.csv('./gjc.csv', stringsAsFactors = F)
# 聚类
model <- dbscan(data[,c(1,2)], eps = 0.0011, MinPts = 3, method = 'raw', showplot = 1)

data$'上车站点' <- model$cluster # 聚类出来的站点

table(model$cluster)
max(model$cluster)
write.csv(data, './gjc_zd.csv', row.names = F)

# 统计
data <- read.csv('./gjc_zd_(有实际站点).csv', stringsAsFactors = F)
table(data$上车站点)
table(data$实际站点)


########## 计算下车人数
Time <- strsplit(data$业务时间,' ')
time <- 0
for (i in 1:length(Time)) {
  time[i] <- Time[[i]][2]
  time[i] <- paste('2014/06/09', time[i], sep = ' ')
}
data$业务时间 <- strptime(time, format = '%Y/%m/%d %H:%M')

# 统计不同时间段
point <- c('2014/06/09 05:00:00','2014/06/09 08:00:00','2014/06/09 09:00:00',
           '2014/06/09 18:00:00','2014/06/09 19:00:00','2014/06/09 23:00:00')

lj <- c('./gjc/时段1_68.csv', './gjc/时段2_68.csv', './gjc/时段3_68.csv', 
        './gjc/时段4_68.csv', './gjc/时段5_68.csv')

for(k in 2: length(point)) {
  gjc <- data[which(data$业务时间 >= point[k-1] & data$业务时间 < point[k]), ]
  write.csv(gjc, lj[k-1], row.names = F)
}

zd1 <- table(data$实际站点)
zd <- data.frame(zd1)
colnames(zd) <- c('上车站点','上车人数')
zd$'线路名称' <- '68'
data1 <- cbind(zd$线路名称,zd$上车站点,zd$上车人数)
colnames(data1) <- c('线路名称','上车站点','上车人数')

write.csv(data1,'./gjc_num.csv',row.names = F)


num <- function(data){
  gjc <- data.frame('线路名称'=0 ,'上车站点' =0, '上车人数' = 0)
  zd = unique(data$实际站点)
  for(i in 1:length(zd)){
    data1 <- data[which(data$实际站点 == zd[i]),]
    gjc[i,1] <- '68路'
    gjc[i,2] <- zd[i]
    gjc[i,3] <- nrow(data1)
  }
  return(gjc)
}

file <-list.files()
shuju <- list.files(file[which(file=='gjc')])
dir <- paste('./',file[which(file=='gjc')],'/',shuju,sep = '')

lj <- c('./number/时段1_39.csv','./number/时段2_39.csv','./number/时段3_39.csv','./number/时段4_39.csv','./number/时段5_39.csv')

for(g in 1:length(dir)){
  date <- read.csv(file = dir[g],header = T,sep=',',stringsAsFactors = F)
  gjc <- num(date)
  write.csv(gjc,lj[g],row.names = F)
}

data <- read.csv('./gjc_zd_(有实际站点).csv', stringsAsFactors = F)
gjc <- num(data)
write.csv(gjc,'./gjc_num_39.csv',row.names = F)



######## 计算下车人数

da <- read.csv('./number/时段1_339.csv', stringsAsFactors = F)



##计算权重
#### OD矩阵
datanum <- function(da){
  # 计算站点吸引权重
  da$'吸引权重' <- round(da$上车人数/sum(da$上车人数),3)
  
  da <- da[order(da$上车站点),]
  
  # 站点数数学期望
  pro <- data.frame()
  x <- nrow(da)
  a <- sum(1:38)/38
  
  m <- c(da$上车站点)
  
  #方法1
  # Fij为i 站上车，途经j-i个站点下车的概率
  # for (i in 1:x) {
  #   for (j in 1:x) {
  #     if(i<j){
  #       F_ij <- exp(1)^(-a)*a^(j-i)/factorial(j-i)
  #       pro[i,j] <- F_ij
  #     }else{
  #       pro[i,j] <- 0
  #     }
  #   }
  # 创建新的一列，计算Fij * Wj
  #   pro[i,(x+1)] <- sum(pro[i,c(1:x)]*da$吸引权重)
  # }
  
  pro <- matrix(0,x,x+1)
  
  
  # 方法二（直接调用函数）
  for (i in 1:x) {
    for (j in 1:x) {
      F_ij <- dpois((j-i),a) # 泊松分布函数
      pro[i,j] <- F_ij
    }
    pro[i,(x+1)] <- sum(pro[i,c(1:x)]*da$吸引权重)
  }
  
  
  # 计算下车人数
  OD <- data.frame()
  for (i in 1:x){
    for (j in 1:x){
      if(i<j){
        # 最终出行站数概率pij
        P <- (pro[i,j]*da[j,4]/pro[i,(x+1)])
        
        OD[i,j] <- round(P*da[i,3])
      }
      if(i>=j){
        OD[i,j] <- 0
      }
    }
  }
  
  # 每一列,上车总人数
  for (j in 1:nrow(OD)) {
    OD[(length(OD)+1),j] <- sum(OD[c(1:length(OD)),j])
  }
  # 每一行，下车总人数
  rownums<- nrow(OD)
  for (i in 1:rownums) {
    OD[i,rownums] <- sum(OD[i,c(1:rownums-1)])
  }
  name<- as.character(da$上车站点)
  rownames(OD) <- c(name,'下车总人数')
  colnames(OD) <- c(name,'上车总人数')
  return(OD)
}



file <- list.files()
shuju <- list.files(file[which(file=='number')])
dir <- paste('./',file[which(file =='number')],'/',shuju,sep='')

lj1 <- c('./OD/时段1_68.csv','./OD/时段2_68.csv','./OD/时段3_68.csv','./OD/时段4_68.csv','./OD/时段5_68.csv')
lj2 <- c('./num/时段1_68.csv','./num/时段2_68.csv','./num/时段3_68.csv','./num/时段4_68.csv','./num/时段5_68.csv')

for(g in 1:length(dir)){
  data<- read.csv(file=dir[g],stringsAsFactors = F)
  OD <- datanum(data)
  write.csv(OD,lj1[g])
  data$'下车人数'<-0
  for(i in 1:(length(OD)-1)){
    a<- which(data$上车站点 == names(OD)[i])
    data$'下车人数'[a] <- OD[nrow(OD),i]
    write.csv(data,lj2[g],row.names = F)
  }
}



c<-datanum(da)




