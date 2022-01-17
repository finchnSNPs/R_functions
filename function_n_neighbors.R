library(dplyr)

####n_neighbors()

#Counts SNPs occurring within a list of intervals by position. 
#utilizes the between() function from dplyr

#Not to be used with >5000 query SNPs. Takes too long!

n_neighbors<-function(query,snps_in,hood){
  q_specs<-list()
  snp_specs<-list()
  neighbors_list<-list()
  n_neighbors_list<-list()
  out_list<-list()
  q_specs[[1]]<-query$SNP
  q_specs[[2]]<-query$Chromosome
  q_specs[[3]]<-query$Position
  q_specs[[4]]<-(query$Position-hood)
  q_specs[[5]]<-(query$Position+hood)
  for (i in 1:length(q_specs[[1]])){
    counter=0
    fil_snps<-data.frame(filter(snps_in,Chromosome == q_specs[[2]][i]))
    print(paste("Processing:",q_specs[[1]][i],sep=" "))
    snp_specs[[1]]<-fil_snps$SNP
    snp_specs[[2]]<-fil_snps$Chromosome
    snp_specs[[3]]<-fil_snps$Position
    for (j in 1:length(snp_specs[[1]])){
      if (isTRUE(between(snp_specs[[3]][j],q_specs[[4]][i],q_specs[[5]][i]))){
        counter=counter+1
      } 
      neighbors<-data.frame(q_specs[[1]][i])
      names(neighbors)<-"SNP"
      neighbors$n_neighbors<-counter
      n_neighbors_list[[i]]<-neighbors
    }
    print(paste(counter,"neighbors found",sep=" "))
  }
  n_neighbors<-data.frame(do.call(rbind,n_neighbors_list))
  return(n_neighbors)
}

#examples included in this repository
#q_in.csv
#s_in.csv
q_in<-read.csv("q_in.csv",header=1)
s_in<-read.csv("s_in.csv",header=1)
test_out<-n_neighbors(q_in,s_in,300)
head(test_out)
#number of SNPs with 5 or fewer neighbors within 300 bp flanking on either side
#of the SNP position
nrow(unique(filter(test_out,n_neighbors<=5)))
fewer_neighbors<-unique(filter(test_out,n_neighbors<=5))
q_fewer_neighbors<-q_in%>%filter(SNP%in%fewer_neighbors$SNP)
