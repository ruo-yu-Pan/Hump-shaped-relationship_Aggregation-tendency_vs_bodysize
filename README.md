# Hump-shaped relationship between aggregation tendency and body size within fish populations
 
This repository is an implementaion of the research work *Hump-shaped relationship between aggregation tendency and body size within fish populations*.

The repository contains:
- Source code


Raw data including fishery survey data, sea bottom tempeature (SBT), and fishing mortality can be downloaded on Zenodo (https://doi.org/10.5281/zenodo.4008438) or at the links provided in the paper. 
* Raw data for sea surface temperature (SST) is too big to upload. If you need the raw SST data, Please download it at the links provided in the paper (https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html).


#### 3 kinds of Rscript
*Script* :   The R script with   **"Script"**  at the beginning of the name contains the main code for data compilation and analysis in this study.  
*Function* : The R script with **"Function"** at the beginning of the name include the function needed in the "Script"  
*Plot* :     The R script with   **"Plot"**   at the beginning of the name is mainly for plotting. 

#### Repository structure
I categorized the code into 4 groups, which is stored as four folders: 
- Habitat_information: species habitat information
- Size_information: Observe data and specify sp-specific length range and sp-specific length bin
- Temp_information: Function and Script for collating SBT and SST data are included
- Analysis: size-based Taylor's power law


# Step by Step Analysis
1. Download data from the repository on Zenodo, and save it with the name "SizeAggregTend_data".
2. Download code and (1) save at the same path as "SizeAggregation_data" and (2) named it "SizeAggregTend_code"
2. Run the *Script* code in the folder **Habitat_information**, **Size_information**, and **Temp_information** in order to process raw data.  
  These scripts process the raw data, and save the compiled data in "compiled" directory.  
3. Run the *Script* code in the folder **Analysis** in order, from *Script_1* to *Script_5*

# Contact
If you find any bugs or have any questions about the implementation, pelease contact us via elthina02017@gmail.com
