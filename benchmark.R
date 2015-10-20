

library(imputation)
library(microbenchmark)

dat_list <- list(x1= matrix(rnorm(300), nrow= 10),
                 x2= matrix(rnorm(3000), nrow= 100),
                 x3= matrix(rnorm(30000), nrow= 1000),
                 x4= matrix(rnorm(300000), nrow= 10000),
                 x5= matrix(rnorm(3000000), nrow= 100000))

dat_list <- lapply(dat_list, function(l) {l[l>1.25] <- NA; return(l)})

# 01. Non-parallel, no canopies
#------------------------------------------------
(m1 <- microbenchmark(
  n10= kNN_impute(x= dat_list[[1]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= FALSE),
  n100= kNN_impute(x= dat_list[[2]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= FALSE), 
  n1000= kNN_impute(x= dat_list[[3]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= FALSE), 
  n10000= kNN_impute(x= dat_list[[4]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= FALSE), times= 50L))


# 02. parallel, no canopies
#------------------------------------------------
(m2 <- microbenchmark(
  n10= kNN_impute(x= dat_list[[1]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= TRUE, leave_cores= 1),
  n100= kNN_impute(x= dat_list[[2]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                   parallel= TRUE, leave_cores= 1), 
  n1000= kNN_impute(x= dat_list[[3]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                    parallel= TRUE, leave_cores= 1), 
  n10000= kNN_impute(x= dat_list[[4]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                     parallel= TRUE, leave_cores= 1), times= 50L))

# 03. parallel, canopies vs no-canopies
#------------------------------------------------
(m3 <- microbenchmark(
  n10k_no_can= kNN_impute(x= dat_list[[4]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= TRUE, leave_cores= 1),
  n100k_no_can= kNN_impute(x= dat_list[[5]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                   parallel= TRUE, leave_cores= 1), 
  n10k_can= kNN_impute(x= dat_list[[4]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                    parallel= TRUE, leave_cores= 1, n_canopies= 100), 
  n100k_can= kNN_impute(x= dat_list[[5]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                     parallel= TRUE, leave_cores= 1, n_canopies= 1000), times= 50L))
