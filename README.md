# Introduction to Machine Learning (Part 1: Analysis)
**Hanzehogeschool Groningen: Bioinformatics Project Year 3, Period 9**

Performing an Exploratory Data Analysis on a chosen data set and creating a Machine Learning Algorithm that is able to predict class labels for instances.


## About the project
The goal of this project is to create and learn about Machine Learning Algorithms. Multiple topics and competences are involved in this process, including an initial Exploratory Data Analysis on the data set, keeping a research log to make sure everything is reproducible and the creation of a Java wrapper around the final machine learning algorithm.  
At the end a scientific report is written that discusses the data exploration, the strategy and methodology of benchmarking, performance and optimization in relation to machine learning algorithms. Another important aspect of the project is the ability to critically look at work that has been done, and thus giving and receiving peer feedback.


## Chosen Data Set and Research
The [Breast Cancer Wisconsin (Diagnostic)](https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29) data set has been chosen to do this project about. It was created to easily and accurately diagnose breast masses with just a fine needle aspiration (FNA), ten visually assessed characteristics were identified that were relevant to diagnosis.  
Data was gathered by scanning a sample from the FNA after staining so the cell nuclei were highlighted, the boundaries of all individual nuclei were marked and the nuclei thus isolated from each other. A program then computed values for each of ten characteristics of each nuclei, measuring size, shape and texture. After that the mean, standard error and worst extreme values of these features were computed, resulting in a total of 30 nuclear features for each sample.  
The full data set consists of 569 cases and was used to train an algorithm that can classify and differentiate between benign and malignant FNA samples of breast masses.


## Repository File Structure
### Project Tree
```bash
Project-Machine-Learning-Part1_Analysis
├── research_log.pdf
├── report.pdf
├── LICENSE
├── README.md
├── data
│   ├── raw
│   │   ├── codebook.txt
│   │   ├── wdbc.data
│   │   └── wdbc.names
│   └── processed
│       ├── algorithms_performance.csv
│       └── data.arff
├── output
│   └── figures
└── src
    ├── rmd
    │   ├── research_log.Rmd
    │   └── report.Rmd
    ├── scripts
    │   └── split_violin_plot.R
    └── report_subfiles
        ├── abbreviations.tex
        ├── abstract.tex
        ├── after_body.Rmd
        ├── before_body.tex
        ├── import.tex
        └── references.bib
```

### research_log(.Rmd/.pdf)
This research log has all the steps taken from the entire process, from the initial data exploration to the creation of the final machine learning algorithm. The project is easily reproducible because of this, so it's a great opportunity to learn the steps taken for creating a machine learning algorithm.

### report(.Rmd/.pdf)
To properly report on all findings this report has been written, it gets into all the results and discusses any complications encountered or what could be improved when reproducing this project. A project proposal is also included for a possible project for the minor Application Design from the Bioinformatics Bachelor at the Hanze.

### / data
The publicly available data set was downloaded and placed in the `raw` data subdirectory for easy access, these are the `wdbc.data` and `wdbc.names` files. Because the data file does not have a header line, a codebook was created to use in tandem with the data, enabling to easily set column names, graph titles or axis labels.  
In the `preprocessed` subdirectory is the data that is generated during this project. `data.arff` is the dataset to be used for the machine learning algorithms and  `algorithms_performance.csv` contains the performance data, such as speed, accuracy and the confusion matrix, of all the tested and tried out algorithms.

### / src
The `src` directory houses the `scripts` and `report_subfiles` sub-directories where files are placed so the repository is uncluttered.
- `/report_subfiles`: To keep the report text file itself as clean as possible, everything other than the actual article text is split up in separate files and these are located in the sub-directory `report_subfiles`. This way all the formatting and code does not clutter the text and makes it easier to find everything. 
- `/scripts`: The same principle is used for all `.R` functions and scripts used in this project, these will be put in the `scripts` directory.


## Installation
*This project was written on MacOS in RStudio (version 2022.02.0) with R version 4.1.3 for Apple silicon arm64.*  

First, a working R environment is needed, which can be installed from [the CRAN website](https://cran.r-project.org/) by carefully following the instructions there.  
Second, either [RStudio](https://www.rstudio.com/products/rstudio/download/), or another editor of choice should be installed.
It should be noted that to be able to knit the documents LaTeX is needed.   
Finally, the required packages listed below should be installed, after which the project should be reproducible.

Make sure that before running or knitting any files the working directory is that of this repository!


## Required Packages
The following R packages are required for this project and should be installed through an R console using the `install.packages()` function:
- data.table
- ggpubr
- kableExtra
- sass
- tidyverse
- pander
- ggplot2
- ggbiplot (might need to download from GitHub using 'remotes' package)

To easily install any missing packages the code below can be used instead, which should be pasted and run an R console:
```r
required_packages <- c("tidyverse", "pander", "ggplot2", "data.table", "ggpubr", "remotes", "kableExtra", "sass")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
remotes::install_github("vqv/ggbiplot")
```


## Useful links
* [Project Info Page](https://michielnoback.github.io/bincourses/data_mining_project.html)
* [Link to data set on UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29)
* [PDF of original article about data set](https://minds.wisconsin.edu/bitstream/handle/1793/59692/TR1131.pdf)
* [Secondary article by same authors](https://www.researchgate.net/profile/Nick-Street/publication/2302195_Breast_Cancer_Diagnosis_and_Prognosis_Via_Linear_Programming/links/54182c0b0cf25ebee9880a81/Breast-Cancer-Diagnosis-and-Prognosis-Via-Linear-Programming.pdf)


## Contact
Vincent Talen  
v.k.talen@st.hanze.nl