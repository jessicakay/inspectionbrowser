# inspectionbrowser
based on https://www.publichealthpost.org/research/prisoners-health-is-public-health/
jkant@bu.edu
 
Python and R (tidy) framework for collecting, mining, sorting and analyzing 
public health data on MA prisons, jails and detention centers.
 
inspectionbrowser is made up of two components, Junkfood (document analysis, dataset building and data merging) and Garnish (data cleaning and visualization) a third component, the scraper (platecleaner.py) is not available publicly at this time.


 inspectionbrowser uses the following third-party libraries:
 
 Python: docx-reader, pandas
 
 R: ggplot2, dplyr, stringr, lubridate
 
 Bash & Python: scraper


Disclaimer: at this time, inspectionbrowser isn't meant to be out-of-the-box useable. Due to time constraints, certain values needed to be hardcoded. Most likely, these data extractions will be done quarterly once the final version of junkfood.py is complete, and maintained here as well as on deeperthanwater.org. Similarly, this code is not pretty. It is bloated, it needs to be "golfed" badly. I tried four different versions of the function that writes the dataset, 3 of which can still to some degree be found in the source code. Please be patient. 
