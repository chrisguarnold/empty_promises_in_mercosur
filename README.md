# Arnold, Christian (2017): "Empty Promises and Non-Incorporation in Mercosur", International Interactions


## Code
All code is executed in one go from master.r. Don't forget to set the working
directory relative to the root level.

## Output
The code runs from master.r and produces all figures and tables from the paper
and its appendix in the folder "/output".

## Data
There are a number of different data sets.
* master.RData: data on Mercosur's regulations collected for the paper
* dat_imputed_foo.RData results from the imputations
* gomezmera_crisislevel.csv: Conflict Data from Gomez Mera (2013)
* DPI2010.dta: Worldbank Governance Indicators

The data in the master file is quite abundant and draws from the work I put into the project for my PhD. The file make_data.r takes the master data and builds several data objects for further analysis. You might find "datlong" to be the most useful one.

The key variables to calculate incorporation success and incorporation duration as of 2008 are:
* tdec: Date governments adopt a regulation
* tcens: Last time a regulation is observed. Can be either
  * Incorporation date
  * End of observation period ("right censored data")
* incdummy: has the regulation been incorporated until end of the observation period (2008)?
