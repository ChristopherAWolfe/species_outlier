# Load libraries
library(yada)

# Clear the workspace
rm(list=ls())

data_dir <- file.path(".", "data")

if (!file.exists(data_dir)) {
  stop("data_dir does not exist")
}

var_info <-  yada::load_var_info(file.path(data_dir, "var_info.csv"))

data_file_path <- file.path(data_dir, "pantheria_RECODE.csv")
raw_data <- read.csv(data_file_path, stringsAsFactors=FALSE)

na_value <- -999
x_var_name <- var_info$Variable[which(var_info$Type == "x")]
x <- raw_data[, x_var_name]
x[x == na_value] <- NA

# TODO: create Y by itereating over variables in var_info