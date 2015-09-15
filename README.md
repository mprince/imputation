imputation
==========

## Release Notes
- **9/15/2015**: Originally forked from [JeffWong](github.com/jeffwong/imputation)
- **9/15/2015**: removed multiple methods to focus on KNN.

Missing data imputation (also known as matrix completion) is an extremely difficult science that tries to fill in missing values of a dataset with the best guess.  Recently, it was popularized by the Netflix Challenge, where a matrix of Netflix users and their movie ratings were presented to the data science community to see if algorithms could be developed to predict how a user would rate a certain movie that the user has not yet seen.

References:
* [Missing value estimation methods for DNA microarrays](http://bioinformatics.oxfordjournals.org/content/17/6/520.full.pdf).  Troyanskaya, et al.
* [Improved methods for the imputation of missing data by nearest neighbor methods](http://www.sciencedirect.com/science/article/pii/S0167947315001061) Tutz and Ramzan 2015

##Imputation Algorithms Presented

* Mean Imputation
* k-Nearest Neighbors
* Locally weighted least squares

## Description of methods

* **meanImpute** is a good way to start any missing data problem.  It's the fastest imputation technique and does reasonably well
* **lmImpute** Sometimes, we want to identify missing values and impute them by fitting a line through its neighbors.  This can be done by taking a set of points {y_t, x_t} and regressing y_t on the index t.  Additionally, we can use a locally weighted least squares regression line to taylor the weights of the data points that are observed near the missing ones.
* **kNNImpute** is a classic imputation methods described in Troyanskaya. kNN is only good when the number of features is small .


##Algorithm Design

Each function in this package includes the imputation algorithm as well as a cross validatiion algorithm.  The CV algorithm artificially eliminates 1/3 of the data in a dataset, and runs the imputation function.  Using the completed data, the RMSE is calculated on the portion of the data that was artificially removed only.  Different imputation  algorithms will perform differently on different datasets, so it is important to have these functions for comparison.

