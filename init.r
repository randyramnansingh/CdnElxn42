#
# Example R code to install packages
# See http://cran.r-project.org/doc/manuals/R-admin.html#Installing-packages for details
#

###########################################################
# Update this line with the R packages to install:

my_packages = c("shiny", "ggplot2", "randomForest", "ibmdbR")

###########################################################
sink(stderr())
print("init.r running")
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))
install.packages("ibmdbRXt_1.0.0.tar.gz", repos=NULL, type="source")
print("init.r done")