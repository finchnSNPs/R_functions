library(dplyr)

####find_snps()

#determines if list of SNPs occur within a list of intervals by position. 
#utilizes the between() function from dplyr

#Example Usage:
#matches<-find_snps(intervals_dataframe, snps_dataframe)

#intervals_dataframe must follow this format: (may include other columns)
#SNP Chromosome Position   sstart     send
#FH126 chr3 29812608 29812556 29812660
#FH129 chr2 63563842 63563764 63563920
#FH39  chr7 40812594 40812473 40812715
#FH48  chr11 54271511 54271423 54271598
#FH94  chr15 13862864 13862751 13862977
#S03   chr27 28578887 28578816 28578957

#snps_dataframe must follow this format: (may include other columns)
#SNP Chromosome Position
#FH126 chr3 29812608 
#FH129 chr2 63563842 
#FH39  chr7 40812594 
#FH48  chr11 54271511
#FH94  chr15 13862864
#S03   chr27 28578887

#examples included in this repository
#intervals_dataframe.csv
#snps_dataframe.csv

find_snps<-function(intervals_in, snps_in){
  in_specs<-list()
  snp_specs<-list()
  match_list<-list()
  in_specs[[1]]<-intervals_in$SNP
  in_specs[[2]]<-intervals_in$Chromosome
  in_specs[[3]]<-intervals_in$sstart
  in_specs[[4]]<-intervals_in$send
  counter=0
  for (i in 1:length(in_specs[[1]])){
    fil_snps<-data.frame(filter(snps_in,Chromosome == in_specs[[2]][i]))
    print(paste("Processing interval:",in_specs[[1]][i],sep=" "))
    snp_specs[[1]]<-fil_snps$SNP
    snp_specs[[2]]<-fil_snps$Position
    snp_specs[[3]]<-fil_snps$Chromosome
    for (j in 1:length(snp_specs[[1]])){
      if (isTRUE(between(snp_specs[[2]][j],in_specs[[3]][i],in_specs[[4]][i]))){
        match<-data.frame(snp_specs[[1]][j])
        names(match)<-"SNP_match"
        match$Chromosome<-snp_specs[[3]][j]
        match$Position<-snp_specs[[2]][j]
        match$interval<-in_specs[[1]][i]
        match$int_Chromosome<-in_specs[[2]][i]
        match$sstart<-in_specs[[3]][i]
        match$send<-in_specs[[4]][i]
        counter=counter+1
        print(paste(counter,"matches found",sep=" "))
        match_list[[j]]<-match
      } 
    }
  }
  matches<-data.frame(do.call(rbind,match_list))
  print(paste("Done",counter,"matches found",sep=" "))
  return(matches)
}

intervals_dataframe<-read.csv("intervals_dataframe.csv",header=1)
snps_dataframe<-read.csv("snps_dataframe.csv",header=1)

matches<-find_snps(intervals_dataframe, snps_dataframe)

head(matches)



