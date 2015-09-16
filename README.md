imputation
==========

*Quoted from [JeffWong](github.com/jeffwong/imputation)*: Missing data imputation (also known as matrix completion) is an extremely difficult science that tries to fill in missing values of a dataset with the best guess.  Recently, it was popularized by the Netflix Challenge, where a matrix of Netflix users and their movie ratings were presented to the data science community to see if algorithms could be developed to predict how a user would rate a certain movie that the user has not yet seen.


## Release Notes
- **9/15/2015**: Originally forked from [JeffWong](github.com/jeffwong/imputation)
- **9/16/2015**: Package has been completely re-written to:
    - (1) Focus exclusively on kNN (specifically weighted-kNN)
    - (2) To work with larger datasets (ie- for memory efficiency), distance is computed row by row, instead of for the entire matrix at a time.
    - (3) Distances are specified as L-q distances:
    ```
    \deqn{d_q(x_i, x_j) =
        \left{\frac{1}{m} \sum_{s=1}^p |x_{is} - x_{js}|^q I(x_{is} = observed)
        I(x_{js} = observed) \right}}
        where
            - \eqn{m} is the number of columns where both \eqn{x_i} and \eqn{x_j} are observed.
            - \eqn{q} is an integer >= 1
    ```
    - (4) Weights for kNN are calculated via Gaussian kernals


## References:
* [Improved methods for the imputation of missing data by nearest neighbor methods](http://www.sciencedirect.com/science/article/pii/S0167947315001061) Tutz and Ramzan 2015

## Imputation Algorithms Presented

* k-Nearest Neighbors

