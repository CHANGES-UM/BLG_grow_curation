# data_curation_examples

This repository contains code used for cleaning and combining data from the bluegill grow workflow that was transcribe via volunteers on the Zooniverse platform as well as a tutorial on how to match museum data to historical data. 

**The following are in this repository:** \  
**blg_grow folder** - contains the code used for cleaning and joining data from Zooniverse output. The first script, **01_blg_dropdown_cleaning.R**, demonstrates how to clean data from a dropdown option in Zooniverse (in this case dates). The second script, **02_blg_text_cleaning.R**, demonstrates how to clean text fields from Zooniverse (in this case bluegill length), as well as dropdown options for bluegill age. \

**data folder** - contains the raw data files for the bluegill growth curation. \

**museum_matching folder** - contains data and a tutorial, **museum_tutorial.Rmd**, for joining historical lake summary information with museum records. \

**basic_lake_matching.R** - code example for cleaning and joining lake historical data to the Michigan authority lake file.
