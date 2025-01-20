# Unstable Supply and Future Shortages of Wild Forage Fish Heighten Risks to Global Fed Aquaculture Production

Yue Liu1,2,3, Ziyu Jiang1,2,3, Richard S. Cottrell4, Michael F. Tlusty5, 
Kevin Fitzsimmons6, Ling Cao1,2,* 

1 State Key Laboratory of Marine Environmental Science, College of Ocean and Earth Sciences, Xiamen University, Xiamen, 316005, China 
2 State Key Laboratory of Mariculture Breeding, Xiamen University, Xiamen, 361102, China
3 School of Oceanography, Shanghai Jiao Tong University, Shanghai, 200240, China
4 Institute for Marine and Antarctic Studies, College of Sciences and Engineering, University of Tasmania, Sandy Bay TAS 7001, Australia
5 School for the Environment, University of Massachusetts Boston, Boston, MA 02125, USA 
6 College of Agriculture, Life & Environmental Sciences, University of Arizona, Tucson, AZ 85721, USA
* Corresponding author: caoling@xmu.edu.cn 
These authors contributed equally: Y. Liu and Z. Jiang

This is the official repository for paper "Unstable Supply and Future Shortages of Wild Forage Fish Heighten Risks to Global Fed Aquaculture Production".

## Usage

* all input data is in input.zip file
* please unarchive input.zip and rename the folder into data
* then run all script in 01-Modelling and 02-Plotting one by one in order follow how to use:

## How to use

1. 01_forageFishDemand.R - sumulates the forage fish demand historically;
2. 02_ffdrEstimation.R - calculate the Forage Fish Dependency Ratio for each species;
3. 03_exploitMap_cMSY.R - using CMSY method to explore the status of forage fish stocks;
4. 04_scenarioModelInput.R & 05_scenarioModeling_simu.R - generates the nessessary input for scenario model and run simulation;
5. 06_ffReplacement.R - calculated the forage fish amount should have been replaced under each scenario;
6. 07_country_taxa_weight.R & 10_modeloutput.R - function for calculating weight for allocate forage fish loss and output all results to a table after running.
7. Folder **02-Plotting** contains scripts that would generate all figures shown in paper.
8. No need to modify scipts in **00-Support Functions**.
