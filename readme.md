# Software Analystics With R

This little experiment is based on the Java Parser Software of a company called "intooitus" and which can be downloaded here: https://www.intooitus.com/node/85/download.
Output of this parser applied to Java Source Code is a so called MSE[1] file which contains Quality Metrics for the entities which have been parsed, mainly packages and classes.
Currently the R script just builds a dataframe with data coming from the class data provided in a mse file

* Find examples for mse File in directory: example_mse_files
* Find examples for plots in directory: example_plots

Disclaimer - Currently this is more or less a hack, whose aim is to show that the pipeline inFamix parser, read in with R, visualize with R works 

# How to use it?
* Download the parser from here: https://www.intooitus.com/node/85/download
* Run it with your Java Project
* Save the output file somewhere
* Open the R file "ReadInMSEFile.R" and set the varaible "fullqualifiedFileName" to point to your mse file. Save it
* Run the R file
* In the End you will have a dataframe called "classData" which contains all objects of Type INFAMIX.CLASS, i.e. the class files of your java project minus interfaces. 
* Now it's up to you to run several plots. The R file contains 3 examples of plots in the very last lines, a treemap, a xy plot and a boxplot (find them in example_plots)

#Further Notes
Based on the work here I have created a small R shiny web app which is hosted here: https://github.com/huberp/DataScienceSpecDataProducts
It's nothing spectacular, just my course project for the Coursera Data Science Specialization "Creating Data Products" Course.

---
[1] http://rmod.lille.inria.fr/archives/reports/Duca11c-Cutter-deliverable22-MSE-FAMIX30.pdf
