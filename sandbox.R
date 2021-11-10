setwd("D:/Documents/Projects/species_outlier")

# Load libraries
library(yada)
library(dplyr)
library(doParallel)
library(foreach)
registerDoParallel(detectCores())

# Clear the workspace
rm(list=ls())

#results
data_dir <- "results"

#name of analysis
analysis_name <- 'species_test'

#make sure we have a data_dir
if (!file.exists(data_dir)) {
  stop("data_dir does not exist")
}

#load in data (removed NA's in x)
var_info <-  yada::load_var_info("data/var_info.csv")
data_file <- 'data/pantheria_NAomit.csv' 

dat <- load_cp_data(data_file, var_info)

# Extract the main problem

main_problem <- dat$problem
save_problem(data_dir, analysis_name, main_problem)

# Load the main problem from file
problem0 <- readRDS(build_file_path(data_dir, analysis_name, "main_problem"))

# Build ordinal problems (main problem no folds)
ord_prob_list <- build_univariate_ord_problems(data_dir,
                                               analysis_name,
                                               add_folds=F)

#prep model fit
base_seed <- 264528
set.seed(base_seed)
seed_vect <- sample.int(1000000, length(ord_prob_list), replace=F)

# Solve the ordinal problems in parallel and save to user-defined directory
ord_success <-
  foreach::foreach(i=1:length(ord_prob_list), .combine=cbind) %dopar% {
    yada::solve_ord_problem(data_dir,
                            analysis_name,
                            ord_prob_list[[i]],
                            anneal_seed=seed_vect[i])
  }

# Build continuous problems (main problem no folds)
cont_prob_list <- build_univariate_cont_problems(data_dir,
                                                 analysis_name,
                                                 add_folds=F)

# Solve the continuous problems in parallel and save to user-defined directory
cont_success <-
  foreach::foreach(i=1:length(cont_prob_list), .combine=cbind) %dopar% {
    yada::solve_cont_problem(data_dir, analysis_name, cont_prob_list[[i]])
  }

##TO DO: Below are two crude functions to automate AIC selection. I've used them
#to plot the requisite variables previously, but there are scale issues to be 
#solved. i.e. we have an x value that is ~100000000 grams (a whale?).
#The plots are not the most elegant so I have left off for now to see what your
#thoughts are. 

#Ordinal AIC

j_ord <- problem0$mod_spec$J

#create and empty list to store ordinal results
ord <- setNames(vector("list", j_ord), problem0$var_names[1:3])

# Loop over each ord var, select the best model,print best model,plot fit
for (i in 1:j_ord) {
  
  var_name <- problem0$var_names[i]
  
  print(paste0("AIC model selection for ", var_name))
  
  aic_output <- yada::build_aic_output(data_dir, analysis_name, var_name,
                                       format_df=T, save_file=TRUE)
  
  c <- filter(aic_output, rank == 1)$model
  
  print(c)
  
}

#continuous AIC

k_cont <- problem0$mod_spec$K

#create and empty list to store ordinal results
cont <- setNames(vector("list", k_cont), problem0$var_names[4:33])

for (i in 1:k_cont) {
  
  var_name <- problem0$var_names[4:33][i]
  
  print(paste0("AIC model selection for ", var_name))
  
  aic_output <- yada::build_aic_output(data_dir, analysis_name, var_name,
                                       format_df=T, save_file=TRUE)
  
  c <- filter(aic_output, rank == 1)$model
  
  print(c)
  
}

#A very crude plot of BMR. Shows the fit, but again we have scale issues. 

z <- readRDS("results/solutiony_species_test_cont_k_5_bmr_pow_law_lin_pos_int.rds")

xplot <- seq(min(problem0$x),max(problem0$x), len=100)
h <- calc_mean_univariate_cont(xplot,z$th_y,z$mod_spec)

max(dat$cp_df$adult_bm)

plot(dat$cp_df$adult_bm,dat$cp_df$bmr, xlim = c(0,100))
lines(xplot,h, col='red')

################################################################################
