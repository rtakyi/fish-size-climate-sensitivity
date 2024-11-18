#add some details here includeing workflow 


## heading here

### TODOs

- Document all equations in the manuscript - ask CB next week if need help
- Create some figures to put in the manuscript. 
- Add a new euqation that calculates Linf and/or K given a temperature
- Repeat our first figure, but with temperature on the x-axis. 
- Re-run the figure for a selection of different species parameters. e.g. a sardine, a benthic gadioid, a tuna, a slow growing deep sea fish, a shark. Coudl use LH categories in King and McFarlane 2003 or some other analysis of fish growth LH types. e.g. https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14076
- Then also need to look at how results are affected by fmishing mortality rate.

## Method
### Data collection
- Developed predefined keywords and search phrases based on the life history parameter, such as "asymptotic length and weight of Lutjanus fulgens", "growth rate of Sarsinops sagax" and "mortality rate of Engraulis encrasicolus", et.
- Systematically searched web of the ISI Web of Science, Science Direct, and Google Scholar  
- Populated excel sheet with the life history data collected, named it the file case_study_parameters and converted it to a CSV

### Analysis of data
- Load libraries, including TropFishR, tiddyverse and patchwork
- The theme for the final output was setup
- read the csv file (case_study_parameters) with the read.csv function and assigned it to the name
- The read file was assigned a new name (target_species) that calls out each species with it's ID per row
- Variability in the temperature from the IPCC's CMIP6 projections was setup as a sequence from the minimum of 1.5 oC to the maximum of 4 oC at coefficient value of 0.02 with multiple scenarios 
- All the scenarios was setup and assigned to "gall"
- The logistic selectivity (to calculate the selectivity), von Bertalanffy functions for length and weight (to calculate length and weight at age), fish caught at age, abundnace at age and stock indicators were setup 
- The "For loop" function with iteration through each row "irow" was setup for the data frame "scnr_clim" 
-
