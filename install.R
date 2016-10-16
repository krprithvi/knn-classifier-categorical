# Check if the package is available. If not then install
if (!require('optparse')) {
    install.packages("optparse", dependencies=TRUE, repos='http://cran.us.r-project.org', quiet=TRUE)
}
if (!require('MASS')) {
    install.packages("optparse", dependencies=TRUE, repos='http://cran.us.r-project.org', quiet=TRUE)
}
if (!require('knitr')) {
    install.packages("optparse", dependencies=TRUE, repos='http://cran.us.r-project.org', quiet=TRUE)
}
if (!require('knnGarden')) {
    install.packages("optparse", dependencies=TRUE, repos='http://cran.us.r-project.org', quiet=TRUE)
}
if (!require('class')) {
    install.packages("optparse", dependencies=TRUE, repos='http://cran.us.r-project.org', quiet=TRUE)
}
