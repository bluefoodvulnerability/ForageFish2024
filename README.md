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

1. *01_forageFishDemand.R* - Simulates the historical demand for forage fish demand.
2. *02_ffdrEstimation.R* - Calculates the Forage Fish Dependency Ratio (FFDR) for each species.
3. *03_exploitMap_cMSY.R* – Uses the CMSY method to explore the likely status of forage fish stocks.
4. *04_scenarioModelInput.R* & 05_scenarioModeling_simu.R - Generate the necessary input for the scenario model and run the simulation.
5. *06_ffReplacement.R* - Calculates how much forage fish would need to be replaced under each scenario.
6. *07_country_taxa_weight.R* & 10_modeloutput.R – Provide functions for calculating weights to allocate forage fish losses and output all results to a table once the simulations are complete.
7. The *02-Plotting* folder contains the scripts for generating all figures in the manuscript.
8. The *00-Support Functions* folder contains backend functions that do not require modification or execution; they are provided solely as supporting resources.
