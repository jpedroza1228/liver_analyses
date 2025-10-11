# script for MICE imputation
# pak::pkg_install("arrow")

library(tidyverse)
library(mice)
library(miceadds)

hdl <- arrow::read_parquet(here::here("data/cleaned/hdl.parquet"))
ldl <- arrow::read_parquet(here::here("data/cleaned/ldl.parquet"))
bio <- arrow::read_parquet(here::here("data/cleaned/bio.parquet"))

blood <- left_join(hdl, ldl, "id") |>
  left_join(bio, "id")

data <- arrow::read_parquet(here::here('data/nhanes_data_2013_2017.parquet'))

data <- left_join(data, blood, "id")

data |> glimpse()

inspectdf::inspect_na(data) |>
  inspectdf::show_plot()


data |>
  mutate(
    across(
      c(
        female,
        race_ethnic,
        birth_country,
        citizen,
        length_us
      )
    )
  )



pred_matrix <- make.predictorMatrix(data = data)
imp_method <- make.method(data = dadta)


# left off here

pred_matrix[, "id"] <- 0

pred_matrix[, "birth_country"] <- 0
pred_matrix[c("told_prediabetes", "told_risk_diabetes", "could_risk_diabetes"), "total_chol_mg_dl"] <- 0

imp_method[c(
  "hdl_chol_mg_dl",
  "trigly_mg_dl",
  "ldl_chol_mg_dl",
  "total_chol_mg_dl"
)] <- "midastouch"

imp_method[c(
  "albumin_g_dl",
  "alp_iu_l",
  "ast_u_l",
  "alt_u_l",
  "ggt_u_l",
  "total_bilirubin_mg_dl",
  "bmi",
  "waist_circumference",
  "min_sedentary",
  "num_meals_not_home_prepare",
  "num_ready_eat_food_30day",
  "num_frozen_meal_30day"
)] <- "pmm"

imp_method[c(
  "citizen",
  "length_us",
  "ed",
  "annual_house_income",
  "alc_drink12_yr",
  "ever_45_drink_everyday",
  "gen_health",
  "covered_insurance",
  "told_angina",
  "told_heart_attack",
  "told_liver_cond",
  "told_cancer",
  "dr_told_exercise",          
  "you_control_wt",
  "you_reduce_fat",
  "told_prediabetes",          
  "told_risk_diabetes",
  "could_risk_diabetes",
  "vig_rec_pa",
  "mod_rec_pa",
  "told_hep_b",
  "told_hep_c",
  "told_high_bp",
  "dr_told_high_chol",        
  "hep_a_anti",
  "hep_b_core_anti"
)] <- "cart"

prac <- 
  mice(
  latino,
  maxit = 0,
  m = 1,
  method = imp_method,
  predictorMatrix = pred_matrix
)

prac
prac$loggedEvents

rm(prac)
gc()

set.seed(12345)
model_imp <- 
  futuremice(
    latino,
    m = 60,
    maxit = 30,
    method = imp_method,
    predictorMatrix = pred_matrix,
    parallelseed = 12345,
    n.core = 4
  )

model_imp$loggedEvents