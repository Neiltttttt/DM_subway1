# @author Qianzhang Tian
# @date: 2017-06-15

library(dplyr)   ## for data cleaning & formatting
library(tidyr)   ## for pivot
library(amap)    ## for kmeans


#################### data pre-processing #####################
# 读入数据
dataset1 <- read.table("C:/Users/Neil/Desktop/170620/work-160912new/part-00000",sep="\t")
dataset2 <- read.table("C:/Users/Neil/Desktop/170620/work-160913new/part-00000",sep="\t")
dataset3 <- read.table("C:/Users/Neil/Desktop/170620/work-160914new/part-00000",sep="\t")
dataset4 <- read.table("C:/Users/Neil/Desktop/170620/work-160915new/part-00000",sep="\t")
dataset5 <- read.table("C:/Users/Neil/Desktop/170620/work-160916new/part-00000",sep="\t")
dataset6 <- read.table("C:/Users/Neil/Desktop/170620/lei-160917new/part-00000",sep="\t")
dataset7 <- read.table("C:/Users/Neil/Desktop/170620/lei-160918new/part-00000",sep="\t")


dataset <- rbind(dataset2,dataset4)


colnames(dataset) <- c("ID","startTime","startStation","arriveTime","arriveStation","userType")


# change data to tbl_df type
dataset_df <- tbl_df(dataset)

dataset_df$hour <- substr(dataset_df$arriveTime,12,13)

# dataset_df$timeGap <- difftime(dataset_time$arriveTime,dataset_time$startTime)

##  增加判断是否为同站进出的属性
# dataset_df$isSame <- 0 
# w <- which(dataset_df$startStation == dataset_df$arriveStation)
# dataset_df$isSame[w] <- 1 

ids <- group_by(dataset_df,ID)


hours <- group_by(dataset_df,ID,hour)



# group by ID
idcount <- summarise(ids,
                  count = n() ## count number,
                  )
# group by ID,hour 
hourcount <- summarise(hours,
                count =n()
                )

## 使用tidyr将长数据转换为宽数据（pivot）
hourcount_expand <- spread(data = hourcount, key = hour, value = count, fill = 0)


# ratio$ratio <- ratio$count.y / ratio$count.x
# ratio <- ratio[c(1,3,5)]

# 左链接
# test <- left_join(hourcount,idcount,by="ID")


## 合并上面两个表
result <- merge(idcount,hourcount_expand,by=c("ID"),all=T)
result$`00`<- result$`00` / result$count
result$`04`<- result$`04` / result$count
result$`05`<- result$`05` / result$count
result$`06`<- result$`06` / result$count
result$`07`<- result$`07` / result$count
result$`08`<- result$`08` / result$count
result$`09`<- result$`09` / result$count
result$`10`<- result$`10` / result$count
result$`11`<- result$`11` / result$count
result$`12`<- result$`12` / result$count
result$`13`<- result$`13` / result$count
result$`14`<- result$`14` / result$count
result$`15`<- result$`15` / result$count
result$`16`<- result$`16` / result$count
result$`17`<- result$`17` / result$count
result$`18`<- result$`18` / result$count
result$`19`<- result$`19` / result$count
result$`20`<- result$`20` / result$count
result$`21`<- result$`21` / result$count
result$`22`<- result$`22` / result$count
result$`23`<- result$`23` / result$count

## 获取userType属性,并混入表内
type <- dataset_df[,c(1,6)]
result <- merge(result,type,by=c("ID"),all=T)

## 最终数据集
result <- result[,-2]

## 写出数据
# write.csv(result,"C:/Users/Neil/Desktop/170620/tt.csv")


############################# Clustering ##############################
set.seed(123456) ## fix the random seed to produce the same results
group = Kmeans(result[,c(-1)],4)
o = order(group$cluster)
cluster <- data.frame(result$userType[o],group$cluster[o])
t <- table(result$userType,group$cluster)


plot(result$ID, result$userType, type="n", xlab="ID", ylab="UserType")
text(x=result$ID, y=result$userType, labels=result$userType, col=rainbow(4)[group$cluster])
