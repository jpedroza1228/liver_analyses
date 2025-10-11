library(tidyverse)
library(mice)
library(miceadds)

link <- "https://raw.githubusercontent.com/jpedroza1228/liver_analyses/refs/heads/main/data/nhanes_2011_2013_cardio_cog.csv"

data <- read_csv(here::here("data/nhanes_2011_2013_cardio_cog.csv"))

data <- data |> 
  select(-1)

data |>
  inspectdf::inspect_na() |>
  inspectdf::show_plot()

colnames(data)

pred_matrix <- make.predictorMatrix(data = data)
imp_method <- make.method(data = data)

pred_matrix[, c("id", "cerad_complete_status", "animal_fluency_sample_test")] <- 0

prac <- 
  mice(
  data,
  maxit = 0,
  m = 1,
  method = imp_method,
  predictorMatrix = pred_matrix
)

prac
prac$loggedEvents

rm(prac)
gc()

parallel::detectCores()

set.seed(12345)
model_imp <-
mice(
  data,
  maxit = 30,
  m = 50,
  method = imp_method,
  predictorMatrix = pred_matrix
)
# model_imp$loggedEvents

saveRDS(model_imp, "cardio_cog_indicator_imputation.rds")


system("gsutil cp cardio_cog_indicator_imputation.rds gs://rds-files-from-analyses/")

# OR FROM TERMINAL
# gsutil cp ~/cardio_cog_indicator_imputation.rds gs://rds-files-from-analyses/

