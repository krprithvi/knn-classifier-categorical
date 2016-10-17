KNN Classifier based on proximities
-----------------------------------

What is it ?
------------
The scripts "analyzeincome.R" runs KNN classifier
on the income dataset and provides the predictions on their classes.
It calculates the accuracy over different proximity measures and K values.

Dataset - https://archive.ics.uci.edu/ml/datasets/Census+Income

Location for the input
----------------------
Please place the income dataset (training and test) in the same folder with the code.

If using the bash script to run the program, ensure that the names of the datasets
match. 


How to use it ?
--------------
Option - 1 : Please use bash script to run the program with default values
             ( Takes the default value for K - 5 and default names of the files ) 

Option - 2 : For more flexibility, please use the following command line program

# Analyze Income Dataset

Usage: analyzeincome.R [options]


Options:


        -r CHARACTER, --training_dataset=CHARACTER
                Training dataset Income

        -t CHARACTER, --testing_dataset=CHARACTER
                Test dataset Income

        -k CHARACTER, --proximity=CHARACTER
                K closest data objects

        --outputfile_proximity1_gower=CHARACTER
                Output filename for gower coefficient

        --outputfile_proximity2=CHARACTER
                Output filename for proximity2 function

        -h, --help
                Show this help message and exit

Location for the output 
----------------------
The output files will be generated in the same folder as the code and input

Installation of dependencies
----------------------------
The scripts depends on three packages "MASS", "optparse" and "Rcpp"

The installation script "install.R" checks if the library is present,if not then it will 
try to install it. 

Please ensure internet connectivity for the package installation to succeed.
