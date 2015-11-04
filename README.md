imputation
==========

Missing data imputation (also known as matrix completion) is a difficult science that tries to fill in missing values of a dataset with a best guess. Multiple methods for such problems are available (eg. mean imputation, EM, etc). This package implements missing data imputation via weighted k nearest neighbors (w-kNN).

Tests on the current version indicate the algorithm runs with exponential time complexity. Of note, >=v0.6 implements canopies to speed up the runtime on large datasets. Canopies are based on the ideas in McCallum et al (2000). Canopies are based on distance to the dataset centroid. In general, since canopies overlap with their neighbors, the use of canopies reduces the time complexity from  \eqn{2^{O(n)}} to approximately \eqn{2^{O(9n / c)}} where c is the number of canopies. Since, in large datasets, c can be quite large, this is a substantial savings.

Canopies produce an approximate solution although they may produce an equivalent solution. Equivalence is guaranteed under the following condition. If for all observations x with k nearest neighbors, the canopy containing x also contains all k nearest neighbors. This should be the case when distance to each ovservation x is highly correlated to distance of each observation to the dataset centroid.

## Release Notes
- **11/3/2015**: v0.7 released. Distance calculations now done in C++ (via Rcpp). Timing tests (same dimensions as below) on the serialized code indicate a ~40% improvement in speed at both 1k and 10k obs. At 100 obs it's only about a third faster.
- **10/26/2015**: Timing tests (see `profile1.R` and `knn_timing.Rdata`) run on Intel Xeon processor using 7 of 8 cores with variable n= c(10, 100, 1000, 10000, 100000) and p = 30.
    - *Non-parallel:* Tests indicate that non-parallel code is roughly O(n^2) or possibly slightly faster.
    - *Parallel:* Parallel code (7 cores) runtime is roughly 42% faster at 1000 obs and 77% faster at 10000 obs, although parallel exhibits exponential time complexity.
    - *Canopies:* In parallel at 10k obs, the non-canopied code runtime is roughly 50% faster than the canopied code. At 100k obs, the canopied code is roughly 60% faster than the noncanopied code. Moreover, the relative speed of canopied code at (100k / 10k) vs non-canopied code at (100k / 10k) is a 5x improvement. This gap should widen as observations increase.
- **10/20/2015**: v0.6.1 -- implements canopies based on the ideas in McCallum et al (2000) to reduce computation time when working with large datasets.
- **10/6/2015**: Wrote and passed unit tests. v0.5
- **10/5/2015**: Removed references to `sapply(...)`, using `unlist(lapply(...))` instead. Added parallel option to `impute_prelim` (v0.4.1.).
- **9/24/2015**: v0.4- option to use `library(parallel)` to parallelize imputation. Speed tests on **Intel i7-4600U (4-cores) with 12GB RAM** with tests on 1, 3, and 4 cores show a  speed improvement of 14% with 3 cores and 9% with 4 cores.
- **9/22/20115**: v0.3- removed references to `kernlab::kernelMatrix`. Improved speed by ~4x
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
    - (4) Weights for kNN are calculated via Gaussian kernels
- **9/15/2015**: Originally forked from [JeffWong](github.com/jeffwong/imputation)

## References:
* [Improved methods for the imputation of missing data by nearest neighbor methods](http://www.sciencedirect.com/science/article/pii/S0167947315001061) Tutz and Ramzan 2015
* [Efficient clustering of high-dimensional data sets with application to reference matching](http://machinelearning102.pbworks.com/w/file/fetch/47221870/canopy-kdd00.pdf) McCallum et al 2000

## Imputation Algorithms Presented

* k-Nearest Neighbors
* two-stage (ie canopied) k-Nearest Neighbors

