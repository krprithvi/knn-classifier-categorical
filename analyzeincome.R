#!/usr/bin/env Rscript
options(warn=-1)

# Installation script for packages optparse and Rcpp. If not available then will install
source("install.r")

# Import required packages
library("optparse")

# Get command line arguments. Exit if not provided.
option_list = list(
    make_option(c("-r", "--training_dataset"), type="character", default=NULL, 
                  action="store", help="Training dataset Income", metavar="character"),
    make_option(c("-t", "--testing_dataset"), type="character", default=NULL, 
                  action="store", help="Test dataset Income", metavar="character"),
    make_option(c("-k", "--proximity"), type="character", default=5, 
                  action="store", help="K closest data objects", metavar="character"),
    make_option(c("--outputfile_proximity1_gower"), type="character", default="income_proximity1", 
                  action="store", help="Output filename for gower coefficient", metavar="character"),
    make_option(c("--outputfile_proximity2"), type="character", default="income_proximity2", 
                  action="store", help="Output filename for proximity2 function", metavar="character")
); 
 
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

# check for arguments
if (is.null(opt$training_dataset) | is.null(opt$testing_dataset)){
    print_help(opt_parser)
    stop("Please provide a training and testing filename", call.=FALSE)
}

# Get the input command line arguments into variables
input_dataset <- opt$training_dataset
testds_filename <- opt$testing_dataset
output_gower_filename <- opt$outputfile_proximity1_gower
output_proximity2_filename <- opt$outputfile_proximity2
k <- strtoi(opt$proximity)

# Remove output file if it already exists
if(file.exists(output_gower_filename)) {
    invisible(file.remove(output_gower_filename))
}

# Write to the output file
header <- c("TID", "AC", "PC", "PP")
write(header , file=paste(output_gower_filename,"_oneneighbor.csv", sep = ""), sep=",", ncolumns=4)
write(header , file=paste(output_gower_filename,"_majorityvote.csv", sep = ""), sep=",", ncolumns=4)
write(header , file=paste(output_gower_filename,"_inversedistance.csv", sep = ""), sep=",", ncolumns=4)

# Load the data set
incomeds <- read.csv(input_dataset, strip.white=TRUE)
testds <- read.csv(testds_filename, strip.white=TRUE)

## ===================================[start] Measure performance===================================

confusionmatrix <- function(pc, ac){
    uniqueclasses <- unique(ac)
    confusionmatrix <- matrix(data=0, nrow = length(uniqueclasses), ncol = length(uniqueclasses))
    sapply(uniqueclasses, function(i) { sapply(uniqueclasses, function(j, i){ confusionmatrix[i,j] <<- sum(pc[which(ac==uniqueclasses[i])] == uniqueclasses[j])}, i)})
    colnames(confusionmatrix) <- paste(rep('P-'), trimws((as.character(uniqueclasses))))
    rownames(confusionmatrix) <- paste(rep('A-'), trimws((as.character(uniqueclasses))))
    return(confusionmatrix)
}

accuracyrate <- function(pc, ac){return(round(sum(pc==ac)/length(pc),2))}
errorrate <- function(pc, ac){return(round(sum(pc!=ac)/length(pc),2))}

printstats <- function(filename, heading){
    result <- read.csv(filename)
    cat("=================================\n\n\n")
    cat(paste(c(heading, "\n")))
    cat(paste(c("Accuracy : ", accuracyrate(result[,3], result[,2]), "\n"), sep=""))
    cat(paste(c("Error : ", errorrate(result[,3], result[,2]), "\n"), sep=""))
    cm <- confusionmatrix(result[,3], result[,2])
    tpr = cm[1,1] /  (cm[1,1] + cm[1,2])
    tnr = cm[2,2] /  (cm[2,1] + cm[2,2])
    fpr = 1 - tnr
    fnr = 1 - tpr
    precision = cm[1,1] / (cm[1,1] + cm[2,1])
    recall = cm[1,1] / (cm[1,1] + cm[1,2])
    fmeasure = (2*recall*precision)/(precision + recall)
    tpr = round(tpr, digits = 2)
    tnr = round(tnr, digits = 2)
    fnr = round(fnr, digits = 2)
    fpr = round(fpr, digits = 2)
    precision = round(precision, digits = 2)
    recall = round(recall, digits = 2)
    fmeasure = round(fmeasure, digits = 2)
    cat(paste(c("TPR", tpr, "TNR", tnr, "FPR", fpr, "FNR", fnr, "\n")))
    cat(paste(c("Precision", precision, "Recall", recall, "fmeasure", fmeasure)))
    knitr::kable(cm)
}
## ===================================[end] Measure performance===================================
# ===================================[start] Gower's distance ===================================

# Declaration of attributes and its types that are used for proximity measurement
incomeattributes <- incomeds[,c(2, 3, 6, 7, 8, 9, 10, 11, 14, 15)]
testattributes <- testds[,c(2, 3, 6, 7, 8, 9, 10, 11, 14, 15)]
columntypes <- c("numerical", "nominal", "numerical", "nominal", "nominal", "nominal", "nominal", "nominal", "numerical", "nominal")

# Create matrices to compute Gower's distance/dissimilarity
combined_dm <- matrix(data=0, nrow=nrow(testattributes), ncol=nrow(incomeattributes))
dissimilarity_matrix <- matrix(data=NA, nrow=nrow(testattributes), ncol=nrow(incomeattributes))
delta_matrix <- matrix(data=0, nrow=nrow(testattributes), ncol=nrow(incomeattributes))

# Computes dissimilarity matrix for numerical attribute
process_numerical_column <- function(i, cattribute=cattribute, tattribute=tattribute) {
    cno <- tattribute[i];
    dissimilarity_matrix[i,] <<- abs((cattribute - cno) / max(1,(max(cattribute) - min(cattribute))));
}

# Computes dissimilarity matrix for nominal attribute
process_nominal_column <- function(i, cattribute=cattribute, tattribute=tattribute) {
    ccategory <- as.character(tattribute[i]);
    cattribute <- as.character(cattribute);
    dissimilarity_matrix[i,] <<- as.numeric(!(cattribute == ccategory));
}

process_each_column <- function(i) {
    cattribute <- incomeattributes[,i]
    tattribute <- testattributes[,i]
    ctype <- columntypes[i]

    if(ctype == "numerical") {
        invisible(sapply(1:length(tattribute), process_numerical_column, cattribute, tattribute))
        delta_matrix <<- delta_matrix + 1
        combined_dm <<- combined_dm + dissimilarity_matrix;
    }

      if(ctype == "nominal") {
        invisible(sapply(1:length(tattribute), process_nominal_column, cattribute, tattribute))
        delta_vector <- as.numeric(!(lapply(cattribute, as.character) == " ?"))
        delta_vector_test <- as.numeric(!(lapply(tattribute, as.character) == " ?"))
        cdelta_matrix <- delta_vector_test %*% t(delta_vector)
        delta_matrix <<- delta_matrix + cdelta_matrix
        combined_dm <<- combined_dm + (cdelta_matrix * dissimilarity_matrix);
    }
}

invisible(sapply(1:ncol(incomeattributes), process_each_column))

combined_dm <- combined_dm / delta_matrix
# ===================================[start] KNN Classifier ===================================
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

onenearestneighbor <- function(i, rowids){
    aclass <- as.character(testds[i,16])
    pclass <- as.character(incomeds[rowids[1],16])
    pp <- NA
    return(c(i, aclass, pclass, pp))
}

idtoclassmapping <- function(i){ as.character(incomeds[i, 16])}

majorityvote <- function(i, proximityvec, rowids){
    #print(proximityvec)
    aclass <- as.character(testds[i,16])
    knearestclasses <- lapply(rowids, idtoclassmapping)
    pclass <- Mode(knearestclasses)
    pp <- sum(knearestclasses == as.character(pclass)) / length(knearestclasses)
    return(c(i, aclass, pclass, pp))
}

weighteddistance <- function(i, weightexponent = 1){min(10^10, as.numeric(1/(i^weightexponent)))}
posteriorprobabilities <- function(i, knearestclasses, weighteddistances){
    as.numeric(knearestclasses == i) %*% weighteddistances
}

inversedistance <- function(i, proximityvec, rowids){ 
    aclass <- as.character(testds[i,16])
    weighteddistances <- unlist(lapply(proximityvec, weighteddistance, 2))
    knearestclasses <- lapply(rowids, idtoclassmapping)
    uniqueknearestclasses <- unique(knearestclasses)
    pps <- unlist(lapply(uniqueknearestclasses, posteriorprobabilities, knearestclasses, weighteddistances))
    pclass <- uniqueknearestclasses[order(pps)[1]]
    pp <- sort(pps)[1]/sum(pps)
    pp <- floor(pp*10000)/10000.0;
    return(c(i, aclass, pclass, pp))
}
# ===================================[end] KNN Classifier ===================================

findnearestkneighbors <- function(i){
    distance_vector <- combined_dm[i,]
    proximityvec <- head(sort(distance_vector), k)
    rowids <- head(order(distance_vector), k)
    outputvec <- onenearestneighbor(i, rowids)
    write(paste(outputvec, collapse= ","), file=paste(output_gower_filename,"_oneneighbor.csv", sep = ""), append=TRUE, sep=" ")
    outputvec <- majorityvote(i, proximityvec, rowids)
    write(paste(outputvec, collapse= ","), file=paste(output_gower_filename,"_majorityvote.csv", sep = ""), append=TRUE, sep=" ")
    outputvec <- inversedistance(i, proximityvec, rowids)
    write(paste(outputvec, collapse= ","), file=paste(output_gower_filename,"_inversedistance.csv", sep = ""), append=TRUE, sep=" ")
}

invisible(sapply(1:nrow(testattributes), findnearestkneighbors))
printstats(paste(output_gower_filename,"_oneneighbor.csv", sep = ""), "Gower Dissimilarity - One Neighbor")
printstats(paste(output_gower_filename,"_majorityvote.csv", sep = ""), "Gower Dissimilarity - Majority vote")
printstats(paste(output_gower_filename,"_inversedistance.csv", sep = ""), "Gower Dissimilarity - Inverse Distance")

# ===================================[end] Gower's dissimilarity ===================================

header <- c("TID", "AC", "PC", "PP")
write(header , file=paste(output_proximity2_filename,"_oneneighbor.csv", sep = ""), sep=",", ncolumns=4)
write(header , file=paste(output_proximity2_filename,"_majorityvote.csv", sep = ""), sep=",", ncolumns=4)
write(header , file=paste(output_proximity2_filename,"_inversedistance.csv", sep = ""), sep=",", ncolumns=4)

# Write to the output file

incomeattributes <- incomeds
testattributes <- testds

# Take care of the Outliers
incomeattributes$capital_gain[incomeattributes$capital_gain == 99999] <- 29999
incomeattributes$hour_per_week[incomeattributes$hour_per_week > 70] = 70
testattributes$capital_gain[testattributes$capital_gain == 99999] <- 29999
testattributes$hour_per_week[testattributes$hour_per_week > 70] = 70

# Replace the missing data with the value occuring most number of times in the attribute
incomeattributes$workclass[incomeattributes$workclass == " ?"] <- " Private"
incomeattributes$workclass <- factor(incomeattributes$workclass)
testattributes$workclass[testattributes$workclass == " ?"] <- " Private"
testattributes$workclass <- factor(testattributes$workclass)

incomeattributes$occupation[incomeattributes$occupation == " ?"] <- " Adm-clerical"
incomeattributes$occupation <- factor(incomeattributes$occupation)
testattributes$occupation[testattributes$occupation == " ?"] <- " Adm-clerical"
testattributes$occupation <- factor(testattributes$occupation)

incomeattributes$native_country[incomeattributes$native_country == " ?"] <- " United-States"
incomeattributes$native_country <- factor(incomeattributes$native_country)
testattributes$native_country[testattributes$native_country == " ?"] <- " United-States"
testattributes$native_country <- factor(testattributes$native_country)

# Discretize the data for age and hours_per_week attributes
categorized_age <- cut(incomeattributes$age, seq(0,100,20), right=TRUE, labels=c(1:5))
categorized_hour_per_week <- cut(incomeattributes$hour_per_week, seq(0,70,10), right=TRUE, labels=c(1:7))
test_categorized_age <- cut(testattributes$age, seq(0,100,20), right=TRUE, labels=c(1:5))
test_categorized_hour_per_week <- cut(testattributes$hour_per_week, seq(0,70,10), right=TRUE, labels=c(1:7))

# Dimensionality Reduction. Combine capital_gain and capital_loss into a single attribute
net_capital <- incomeattributes$capital_gain - incomeattributes$capital_loss
test_net_capital <- testattributes$capital_gain - testattributes$capital_loss
# Discretize the data for net_capital
categorized_net_capital <- cut(net_capital, seq(-10000, 30000, 10000), right=FALSE, labels=c(1:4))
test_categorized_net_capital <- cut(test_net_capital, seq(-10000, 30000, 10000), right=FALSE, labels=c(1:4))

convert_nominal_to_numeric <- function(i) {
    incomeattributes[, i] <<- as.numeric(incomeattributes[, i])
}
convert_nominal_to_numeric_test <- function(i) {
    testattributes[, i] <<- as.numeric(testattributes[, i])
}

# Feeding the preprocessed data into the data frame
incomeattributes["age"] <- categorized_age
incomeattributes["hour_per_week"] <- categorized_hour_per_week
incomeattributes["net_capital"] <- categorized_net_capital 
testattributes["age"] <- test_categorized_age
testattributes["hour_per_week"] <- test_categorized_hour_per_week
testattributes["net_capital"] <- test_categorized_net_capital 

# Converting nominal columns to numerical
col_to_convert_nominal_to_numeric <- c(3, 7, 8, 9, 10, 11, 15)
invisible(sapply(col_to_convert_nominal_to_numeric, convert_nominal_to_numeric))
invisible(sapply(col_to_convert_nominal_to_numeric, convert_nominal_to_numeric_test))

# Get number of levels in each attribute of the data set to be processed.
# The formula for dissimilarity we are using is {ds <- summation(d/l)} where
# ds is dissimalrity value, d is the difference in the levels of respective nominality
# and l is total number of levels of all the attributes taken together

# No of levels
levels = 5+1+16+1+1+1+1+1+4+1+10

# Take the subset of the main data frame with selected attributes
attributes_to_choose <- c(2, 3, 6, 7, 8, 9, 10, 11, 14, 15, 17)
income_attributes <- incomeattributes[, attributes_to_choose]
test_attributes <- testattributes[, attributes_to_choose]

# Assign 'nominal' and 'ordinal' to columns for processing
column_types <- c("ordinal", "nominal", "ordinal", "nominal", "nominal", "nominal", "nominal", "nominal", "ordinal", "nominal", "ordinal")

# Initializing a matrix used to store preprocessed data for computation with each record
preprocessed_data_matrix <- matrix(data=0, nrow=nrow(income_attributes), ncol=ncol(income_attributes))

process_each_column <- function(i, current_row=current_row) {
    current_attribute <- current_row[i]
    current_col <- income_attributes[, i]
    ctype <- column_types[i]

    if(ctype == "ordinal") {
          preprocessed_data_matrix[, i] <<- abs(as.numeric(current_col) - as.numeric(current_attribute));
      }

      if(ctype == "nominal") {
          preprocessed_data_matrix[, i] <<- as.numeric(!(c(current_col) == c(current_attribute)));
      }
}

find_k_nearest_neighbours <- function(i) {
    current_row <- test_attributes[i, ]
    invisible(sapply(1:ncol(current_row), process_each_column, current_row))
    distance_vector <- rowSums(preprocessed_data_matrix)/levels
    proximityvec <- head(sort(distance_vector), k)
    rowids <- head(order(distance_vector), k)
    outputvec <- onenearestneighbor(i, rowids)
    write(paste(outputvec, collapse= ","), file=paste(output_proximity2_filename,"_oneneighbor.csv", sep = ""), append=TRUE, sep=" ")
    outputvec <- majorityvote(i, proximityvec, rowids)
    write(paste(outputvec, collapse= ","), file=paste(output_proximity2_filename,"_majorityvote.csv", sep = ""), append=TRUE, sep=" ")
    outputvec <- inversedistance(i, proximityvec, rowids)
    write(paste(outputvec, collapse= ","), file=paste(output_proximity2_filename,"_inversedistance.csv", sep = ""), append=TRUE, sep=" ")
}

invisible(sapply(1:nrow(test_attributes), find_k_nearest_neighbours))
printstats(paste(output_proximity2_filename,"_oneneighbor.csv", sep = ""), "Modified Jaccard - One Neighbor")
printstats(paste(output_proximity2_filename,"_majorityvote.csv", sep = ""), "Modified Jaccard - Majority vote")
printstats(paste(output_proximity2_filename,"_inversedistance.csv", sep = ""), "Modified Jaccard - Inverse Distance")
