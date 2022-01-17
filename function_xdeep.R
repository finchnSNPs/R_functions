library(dplyr)

####xdeep()

#Filters dataframe by specified depth on a per individual basis. Limited
#usage to poolfstat coverage output.

xdeep<-function(in_df,depth){
  if ("cov.EasternForest" %in% colnames(in_df)){
    temp<-filter(in_df,cov.EasternForest >= depth)
    print(paste(nrow(temp),"SNPs remaining",sep=" "))}
  else{temp<-in_df}
  if ("cov.EasternSavanna" %in% colnames(in_df)){
    temp1<-filter(temp,cov.EasternSavanna >= depth)
    print(paste(nrow(temp1),"SNPs remaining",sep=" "))}
  else{temp1<-temp}
  if ("cov.NorthernSavanna1" %in% colnames(in_df)){
    temp2<-filter(temp1,cov.NorthernSavanna1 >= depth)
    print(paste(nrow(temp2),"SNPs remaining",sep=" "))}
  else{temp2<-temp1}
  if ("cov.NorthernSavanna2" %in% colnames(in_df)){
    temp3<-filter(temp2,cov.NorthernSavanna2 >= depth)
    print(paste(nrow(temp3),"SNPs remaining",sep=" "))}
  else{temp3<-temp2}
  if ("cov.SouthernSavanna" %in% colnames(in_df)){
    temp4<-filter(temp3,cov.SouthernSavanna >= depth)
    print(paste(nrow(temp4),"SNPs remaining",sep=" "))}
  else{temp4<-temp3}
  if ("cov.WesternForest" %in% colnames(in_df)){
    temp5<-filter(temp4,cov.WesternForest >= depth)
    print(paste(nrow(temp5),"SNPs remaining",sep=" "))}
  else{temp5<-temp4}
  return(temp5)
}

#examples included in this repository
s_in<-read.csv("s_in.csv",header=1)
test_out<-xdeep(s_in,100)