# R_misc
functions and stuff

###find_snps()

determines if list of SNPs occur within a list of intervals by position. 
utilizes the between() function from dplyr

####Example Usage:
matches<-find_snps(intervals_dataframe, snps_dataframe)

####examples included in this repository
intervals_dataframe.csv
snps_dataframe.csv
