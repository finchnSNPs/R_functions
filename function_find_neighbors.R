library(dplyr)

####find_neighbors() 

#Finds SNPs that are in the neighborhood (flanking region length specified) of
#some desired regions. Utilizes the between() function from dplyr.

#Not to be used with >5000 query SNPs. Takes too long!

find_neighbors<-function(query_pos,snps_in,hood){
  q_specs<-list()
  snp_specs<-list()
  neighbors_list<-list()
  q_specs[[1]]<-query_pos$SNP
  q_specs[[2]]<-query_pos$Chromosome
  q_specs[[3]]<-query_pos$sstart
  q_specs[[4]]<-(query_pos$sstart-hood)
  q_specs[[5]]<-(query_pos$sstart+hood)
  counter=0
  for (i in 1:length(q_specs[[1]])){
    fil_snps<-data.frame(filter(snps_in,Chromosome == q_specs[[2]][i]))
    print(paste("Processing:",q_specs[[1]][i],sep=" "))
    snp_specs[[1]]<-fil_snps$SNP
    snp_specs[[2]]<-fil_snps$Chromosome
    snp_specs[[3]]<-fil_snps$Position
    for (j in 1:length(snp_specs[[1]])){
      if (isTRUE(between(snp_specs[[3]][j],q_specs[[4]][i],q_specs[[5]][i]))){
        neighbors<-data.frame(snp_specs[[1]][j])
        names(neighbors)<-"SNP"
        neighbors$Chromosome<-snp_specs[[2]][j]
        neighbors$Position<-snp_specs[[3]][j]
        neighbors$q_SNP<-q_specs[[1]][i]
        neighbors$q_Chromosome<-q_specs[[2]][i]
        neighbors$q_sstart<-q_specs[[3]][i]
        neighbors$q_range1<-q_specs[[4]][i]
        neighbors$q_range2<-q_specs[[5]][i]
        counter=counter+1
        print(paste(counter,"neighbors found",sep=" "))
        neighbors_list[[j]]<-neighbors
      } 
    }
  }
  neighbors<-data.frame(do.call(rbind,neighbors_list))
  print(paste("Done",counter,"neighbors found",sep=" "))
  return(neighbors)
}

#examples included in this repository
#q_in.csv
#s_in.csv
q_in<-read.csv("q_in.csv",header=1)
s_in<-read.csv("s_in.csv",header=1)
s_in<-sample_n(s_in,5000)
test_out<-find_neighbors(q_in,s_in,300)
head(test_out)
