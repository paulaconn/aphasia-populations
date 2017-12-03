# aphasia-populations
Processing of AphasiaBank's populations with and without non-fluent aphasia

## Files:
* descriptive_data.py: generates descriptive statistics and histograms for outcome datasets. Outputs 	files into Plots directory.
* preprocessing.r: Filters each CSV file to obtain each population. Outputs CSV files into the 'Output CSV' directory.

## Directories:
* Output CSV: Stores four CSV files for filtered populations that are created with preprocessing.R
* Removed Data: Stores 38 CSV files for removed data with preprocessing.R
* Plots: Stores histograms, scatter matricies, distribution charts, and descriptive data output from descriptive_data.py.

## Requirements:
* R-3.3.2 or higher. Download at: http://cran.r-project.org
* python 3.6 or higher. Download at: https://www.python.org/downloads/

The following files can be downloaded from the [AphasiaBank Database](http://aphasia.talkbank.org): 
* aphasiaDemographics: Demographic data for individuals with aphasia
* aphasiaNoComp: AphasiaBank CHAT output for all aphasia participants in percentages
* aphasiaNoCompNum: AphasiaBank CHAT output for all aphasia participants in raw numbers
* aphasiaTestResults: Testing data for individuals with aphasia
* controlDemographics: Demographic data for individuals without aphasia
* controlNoComp: AphasiaBank CHAT output for individuals without aphasia
* controlNoCompNum: AphasiaBank CHAT output for individuals without aphasia, in raw numbers

## Usage:
1. Run preprocessing.R
2. Run descriptive_data.py

## Citation:

Feel free to use this code and alter it to fit your research needs. If you reference this content, please cite one of the following:

Garcia, P. 2017. Aphasia Populations. Github repository. https://github.com/paulaux/aphasia-populations

Garcia, P. 2017. Distribution of Language Measures among Individuals with and without Non-fluent Aphasia. In *Proceedings of the 10th International Conference on Pervasive Technologies Related to Assistive Environments (PETRA'17)*. ACM Press, New York, NY, 252-253. DOI:https://doi.org/10.1145/3056540.3076214.