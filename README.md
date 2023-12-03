# Functions in R

Functions developed by finchnSNPs at Oregon State University or University of Washington for various projects. 

## Part I

Functions for processing coverage statistics generated by bbmap.sh (BBTools. DOE Joint Genome Institute. https://jgi.doe.gov/data-and-tools/bbtools/.) with parameter "covstats=<output.file>" 

These functions were originally published as supplement for: 
Finch, K.N., Jones, F.A. and Cronn, R.C., 2019. Genomic resources for the Neotropical tree genus *Cedrela* (Meliaceae) and its relatives. BMC Genomics, 20(1), p.58.
[Read the open access article here](https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-018-5382-6)

And was published as a full dataset:
Kristen N. Finch. 2018. Dataset for genomic resources for the neotropical tree genus Cedrela (Meliaceae) and its relatives [Data set]. Oregon State University, Corvallis, OR, USA. https://doi.org/10.7267/NV935820Q 

These functions that were developed specifically to process coverage statistics files from bbmap.sh. 

## see functions_covstats and usage examples here:
https://finchnsnps.github.io/R_functions/functions_covstats_usage.html

Call|Function|Input(s)|
----------------|----------------|----------------|
get_cov_percent|Generates a dataframe with covered percent. One column per specimen.|List of data frames|
get_depth|Generates a dataframe with depth of coverage. One column per specimen.|List of data frames|
get_files|Generates a file list.|File pattern|
get_reads_mapped|Generates a dataframe with sum of mapped reads. One column per specimen.|List of data frames|
get_spec_ids|Uses a regular expression to generate the specimen id list from the filenames.|File list and "header"|
get_spp|Uses a regular expression to generate the species list from the filenames.|File list and "header"|
get_target_ids|Saves column 1 of coverage stats life and designates it as target ids.|File list and "header"|
make_df|Generates data frame with specimens as columns and targets as rows.|Coverage stat data & specimen ids & target ids|
make_tdf|Generates data frame with targets as columns and specimens as rows.|Coverage stat data & specimen ids & target ids & species list|
readin_files|Generated a list of data frames from a file list|File list|

## Part II

Functions for SNP assessment and filtering according to various parameters. Limited to outputs from poolfstat (and some personal preferences), but available for revision to fit other data inputs. 

Call|Function|Input(s)|
----------------|----------------|----------------|
find_snps|Determines if list of SNPs occur within a list of intervals by position|Query (q_in.csv) and subject (s_in.csv) SNP dataframes.|
xdeep|Filters SNPs by population depth.|SNP dataframe with poolfstat coverage output columns for each population and target depth|
find_neighbors|Finds SNPs that are in the neighborhood (flanking region length specified) of some desired regions.|Query and subject SNP dataframes and flanking length in basepairs|
n_neighbors|Counts adjacent SNPs for all SNPs. Not to be used with >5000 query SNPs.|Query and subject SNP dataframes and flanking length in basepairs|



