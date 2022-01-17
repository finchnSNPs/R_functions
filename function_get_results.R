
#Function get_results() calls get_mode() to provide one row of summary
#statistics for a column of data in R. Includes mean, median, mode, minimum,
#maximum, standard error, upper and lower bonds of the 95% confidence interval.

####get_mode()

#Used in the get_results function below. Calculates mode for a column of
#data|Column of data frame.

getmode<-function(column_of_data) {
  uniqv <- unique(column_of_data)
  uniqv[which.max(tabulate(match(column_of_data, uniqv)))]
}

####get_results()

#Calculates mean, median, mode, min, max, and 95% confidence interval for a
#column of data|Note on data & column of data frame.

#Example Usage:
#ind_percent_ontarget<-get_results("individual percent on target",yield_tdf_full$ind_percent_ontarget)
#Note_about_stat must follow character syntax with double quotes: "Note about stat"

get_results<-function(note_about_stat,column_of_data){
  mean<-mean(column_of_data,na.rm = TRUE)
  median<-median(column_of_data,na.rm = TRUE)
  mode<-getmode(column_of_data)
  min<-min(column_of_data,na.rm = TRUE)
  max<-max(column_of_data,na.rm = TRUE)
  err<-qt(0.975,df=length(column_of_data)-1)*sd(column_of_data,na.rm = TRUE)/sqrt(length(column_of_data))
  up<-mean+err
  lo<-mean-err
  note_about_stat<-note_about_stat
  results<-cbind.data.frame(note_about_stat,mean,median,mode,min,max,err,up,lo)
  return(results)
}

#Example data
#use intervals_dataframe.csv 

df_in<-read.csv("../R_misc/intervals_dataframe.csv",header=1)

#intervals_dataframe must follow this format: (may include other columns)
#SNP Chromosome Position   sstart     send
#FH126 chr3 29812608 29812556 29812660
#FH129 chr2 63563842 63563764 63563920
#FH39  chr7 40812594 40812473 40812715
#FH48  chr11 54271511 54271423 54271598
#FH94  chr15 13862864 13862751 13862977
#S03   chr27 28578887 28578816 28578957

#Usage
get_results("Positions",df_in$Position)

