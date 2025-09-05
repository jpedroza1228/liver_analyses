library(tidyverse)
# library(poLCA)
library(mice)
library(miceadds)
library(haven)

filterd <- dplyr::filter
selectd <- dplyr::select
cat_map <- purrr::map

# 2015 - 2016 data
# https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015

# hdl cholesterol
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HDL_I.htm
hdl <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HDL_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    hdl_chol_mg_dl = lbdhdd
  )

#hdl <- hdl |> 
#  mutate(
#    hdl_bi = case_when(
#            hdl_chol_mg_dl < 60 ~ 2,
#            TRUE ~ 1
#          )
#  )

# ldl cholesterol & triglycerides
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TRIGLY_I.htm
ldl <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TRIGLY_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    trigly_mg_dl = lbxtr,
    ldl_chol_mg_dl = lbdldl
  )

#ldl <- ldl |> 
#  mutate(
#    trigly_mg_dl = case_when(
#      trigly_mg_dl > 150 ~ 2,
#      TRUE ~ 1
#    ),
#    ldl_bi = case_when(
#      ldl_chol_mg_dl > 100 ~ 2,
#      TRUE ~ 1
#    )
#  )

# total cholesterol
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TCHOL_I.htm
total_chol <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TCHOL_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    total_chol_mg_dl = lbxtc
  )

#total_chol <- total_chol |> 
#  mutate(
#    total_chol_bi = case_when(
#      total_chol_mg_dl > 200 ~ 2,
#      TRUE ~ 1
#    )
#  )

# hepatitis A
# 1 = +, 2 = -, 3 = Indeterminate
hep_a <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HEPA_I.xpt") |> 
  janitor::clean_names() |> 
  rename(
    hep_a_anti = lbxha
  ) |> 
  mutate(
    hep_a_anti = as.factor(hep_a_anti)
  )

# hepatitis B

hep_b1 <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HEPBD_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    hep_b_core_anti = lbxhbc
    # hep_b_surf_antigen = lbdhbg
  ) |> 
  mutate(
    hep_b_core_anti = as.factor(hep_b_core_anti)
  )

# use surface for hep B
# 1 = +, 2 = -
hep_b2 <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HEPB_S_I.xpt") |> 
  janitor::clean_names() |> 
  rename(
    hep_b_surf_anti = lbxhbs
  ) |> 
  mutate(
    hep_b_surf_anti = as.factor(hep_b_surf_anti)
  )

#hep C
# 1 = +, 2 = -, 3 = - screening hcv antibody
hep_c <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HEPC_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    hep_c_rna = lbxhcr
  ) |> 
  mutate(
    hep_c_rna = as.factor(hep_c_rna)
  )

# blood work
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BIOPRO_I.htm
bio <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BIOPRO_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    albumin_g_dl = lbxsal, # 3.5 - 5.5
    alp_iu_l = lbxsapsi, # 44 - 147 iu/l
    ast_u_l = lbxsassi, # 8-33 u/l
    alt_u_l = lbxsatsi, # 7-56 u/l
    ggt_u_l = lbxsgtsi, # below 50 u/l
    total_bilirubin_mg_dl = lbxstb # 0.2-1.3 mg/dl
  )

# demographics
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.htm
demo <- 
  read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    sex = riagendr,
    age = ridageyr,
    race_latino = ridreth1,
    birth_country = dmdborn4,
    citizen = dmdcitzn,
    length_us = dmdyrsus,
    ed = dmdeduc2,
    marital = dmdmartl,
    total_num_house = dmdhhsiz,
    annual_house_income = indhhin2
  )

# alcohol use
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/ALQ_I.htm#ALQ151
alc <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/ALQ_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    alc_drink12_yr = alq101,
    ever_45_drink_everyday = alq151
  ) |> 
  filterd(
    alc_drink12_yr != 7 &
    ever_45_drink_everyday != 7
  ) |> 
  mutate(
    across(
      c(
        alc_drink12_yr,
        ever_45_drink_everyday
      ),
      ~case_when(
        is.na(.x) ~ NA_integer_,
        .x == 9 ~ NA_integer_,
        TRUE ~ .x
      )
    ),
    across(
      c(
        alc_drink12_yr,
        ever_45_drink_everyday
      ),
      ~as.factor(.x)
    )
  )

# change it so 1 = year, 0 = no
alc <- alc |> 
  mutate(
    alc_drink12_yr = if_else(
      alc_drink12_yr == 1,
      1,
      0
    ),
    ever_45_drink_everyday = if_else(
      ever_45_drink_everyday == 1,
      1,
      0
    ),
    across(
      c(
        alc_drink12_yr,
        ever_45_drink_everyday
      ),
      ~as.factor(.x)
    )
  )

# general health
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HSQ_I.htm
gen_health <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HSQ_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    gen_health = hsd010
  ) |> 
  filterd(
    gen_health != 7
  )

gen_health <- gen_health |> 
  mutate(
    gen_health = case_when(
      gen_health == 1 ~ 5,
      gen_health == 2 ~ 4,
      gen_health == 3 ~ 3,
      gen_health == 4 ~ 2,
      gen_health == 5 ~ 1,
      is.na(gen_health) ~ NA_integer_,
      gen_health == 9 ~ NA_integer_
    ),
    gen_health = as.factor(gen_health)
  )

# diabetes
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DIQ_I.htm
diabetes <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DIQ_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    # told_diabetes = diq010,
    told_prediabetes = diq160, 
    told_risk_diabetes = diq170,
    could_risk_diabetes = diq172
  ) |> 
  filterd(
    # told_diabetes != 7 &
    told_prediabetes != 7 &
    told_risk_diabetes != 7 &
    could_risk_diabetes != 7
  ) |> 
  mutate(
    across(
      c(
        # told_diabetes,
        told_prediabetes,
        told_risk_diabetes,
        could_risk_diabetes
      ),
      ~case_when(
        is.na(.x) ~ NA_integer_,
        .x == 9 ~ NA_integer_,
        TRUE ~ .x
      )
    )
  )

diabetes <- diabetes |> 
  mutate(
    # told_diabetes = case_when(
      # told_diabetes == 1 ~ 2, #yea
      # told_diabetes == 2 ~ 0, #no
      # told_diabetes == 3 ~ 1 #borderline
    # ),
    across(
      c(
        told_prediabetes,
        told_risk_diabetes,
        could_risk_diabetes
      ),
      ~if_else(.x == 1, 1, 0)
    ),
    across(
      c(
        # told_diabetes,
        told_prediabetes,
        told_risk_diabetes,
        could_risk_diabetes
      ),
      ~as.factor(.x)
    )
  )

# diet behavior and nutrition
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DBQ_I.htm
diet <- 
  read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DBQ_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    # diet_healthy = dbq700,
    # eat_fastfood = cbq505,
    num_meals_not_home_prepare = dbd895,
    num_ready_eat_food_30day = dbd905,
    num_frozen_meal_30day = dbd910
  ) |>
  filterd(
    # diet_healthy != 7 &
    num_meals_not_home_prepare != 7777 &
    num_ready_eat_food_30day != 7777 &
    num_frozen_meal_30day != 7777
  ) |> 
  mutate(
    across(
      c(
        num_meals_not_home_prepare,
        num_ready_eat_food_30day,
        num_frozen_meal_30day
      ),
      ~case_when(
        is.na(.x) ~ NA_integer_,
        .x == 9999 ~ NA_integer_,
        .x == 5555 ~ 22,
        .x == 6666 ~ 91,
        TRUE ~ .x
      )
    # ),
    # diet_healthy = case_when(
      # diet_healthy == 1 ~ 5, # 5 is Excellent
      # diet_healthy == 2 ~ 4,
      # diet_healthy == 3 ~ 3,
      # diet_healthy == 4 ~ 2,
      # diet_healthy == 5 ~ 1, # 1 is now Poor
      # diet_healthy == 9 ~ NA_integer_,
      # is.na(diet_healthy) ~ NA_integer_
    # ),
    # eat_fastfood = case_when(
      # eat_fastfood == 1 ~ 1,
      # eat_fastfood == 2 ~ 0,
      # eat_fastfood == 9 ~ NA_integer_,
      # is.na(eat_fastfood) ~ NA_integer_
    # ),
    # across(
      # c(
        # diet_healthy,
        # eat_fastfood
      # ),
      # ~as.factor(.x)
    )
  )

# physical activity
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/PAQ_I.htm#PAQ605
pa <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/PAQ_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    vig_rec_pa = paq650,
    mod_rec_pa = paq665,
    min_sedentary = pad680
  ) |> 
  filterd(
    vig_rec_pa != 7 &
    mod_rec_pa != 7 &
    min_sedentary != 7777
  ) |> 
  mutate(
    across(
      c(
        vig_rec_pa,
        mod_rec_pa
      ),
      ~case_when(
        .x == 1 ~ 1, #now Yes
        .x == 2 ~ 0, #now No
        .x == 9 ~ NA_integer_,
        is.na(.x) ~ NA_integer_,
        TRUE ~ .x
      )
    ),
    across(
      c(
        vig_rec_pa,
        mod_rec_pa
      ),
      ~as.factor(.x)
    ),
    min_sedentary = case_when(
      min_sedentary == 9999 ~ NA_integer_,
      is.na(min_sedentary) ~ NA_integer_,
      TRUE ~ min_sedentary
    )
  )

# health insurance
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HIQ_I.htm
insurance <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HIQ_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    covered_insurance = hiq011
  ) |> 
  filterd(
    covered_insurance != 7
  ) |> 
  mutate(
    covered_insurance = case_when(
      covered_insurance == 1 ~ 1, #now Yes
      covered_insurance == 2 ~ 0, #now No
      covered_insurance == 9 ~ NA_integer_,
      is.na(covered_insurance) ~ NA_integer_,
      TRUE ~ covered_insurance
    ),
    covered_insurance = as.factor(covered_insurance)
  )

# medical conditions
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/MCQ_I.htm
# 20 plus
med_cond <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/MCQ_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    told_angina = mcq160d,
    told_heart_attack = mcq160e,
    told_stroke = mcq160f,
    told_liver_cond = mcq160l,
    told_jaundice = mcq203,
    told_cancer = mcq220,
    dr_told_lose_wt = mcq365a,
    dr_told_exercise = mcq365b,
    dr_told_reduce_salt = mcq365c,
    dr_told_reduce_fat = mcq365d,
    you_control_wt = mcq370a,
    you_increase_exercise = mcq370b,
    you_reduce_salt = mcq370c,
    you_reduce_fat = mcq370d
  ) |> 
  filterd(
    told_angina != 7 &
    told_heart_attack != 7 &
    told_stroke != 7 &
    told_liver_cond != 7 &
    told_jaundice != 7 &
    told_cancer != 7 &
    dr_told_lose_wt != 7 &
    dr_told_exercise != 7 &
    dr_told_reduce_salt != 7 &
    dr_told_reduce_fat != 7 &
    you_control_wt != 7 &
    you_increase_exercise != 7 &
    you_reduce_salt != 7 &
    you_reduce_fat != 7
  ) |> 
  mutate(
    across(
      -seqn,
      ~case_when(
        .x == 1 ~ 1, #now Yes
        .x == 2 ~ 0, #now No
        .x == 9 ~ NA_integer_,
        is.na(.x) ~ NA_integer_,
        TRUE ~ .x
      )
    ),
    across(
      -seqn,
      ~as.factor(.x)
    )
  )

# hepatitis
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HEQ_I.htm
hep <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HEQ_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    told_hep_b = heq010,
    told_hep_c = heq030
  ) |> 
  filterd(
    told_hep_b != 7 &
    told_hep_c != 7
  ) |> 
  mutate(
    across(
      -seqn,
      ~case_when(
        .x == 1 ~ 1, #now Yes
        .x == 2 ~ 0, #now No
        .x == 9 ~ NA_integer_,
        is.na(.x) ~ NA_integer_,
        TRUE ~ .x
      )
    ),
    across(
      -seqn,
      ~as.factor(.x)
    )
  )

# blood pressure and cholesterol
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BPQ_I.htm#BPQ020
bp_chol <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BPQ_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    told_high_bp = bpq020,
    dr_told_high_chol = bpq080
  ) |> 
  filterd(
    told_high_bp != 7 &
    dr_told_high_chol != 7
  ) |> 
  mutate(
    across(
      -seqn,
      ~case_when(
        .x == 1 ~ 1, #now Yes
        .x == 2 ~ 0, #now No
        .x == 9 ~ NA_integer_,
        is.na(.x) ~ NA_integer_,
        TRUE ~ .x
      )
    )
  )

# body measures
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BMX_I.htm
body <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BMX_I.xpt") |> 
  janitor::clean_names() |> 
  selectd(
    seqn,
    bmi = bmxbmi,
    waist_circumference = bmxwaist
  )

# Joining Biological Data together
bio_full <- bio |> 
  full_join(hdl, by = "seqn") |> 
  full_join(ldl, by = "seqn") |> 
  full_join(total_chol, by = "seqn")

most_missing <- 
  bio_full |> 
  # selectd(
    # -matches("cat$"),
    # -matches("bi$")
  # ) |> 
  pivot_longer(
    -seqn
  ) |> 
  filterd(
    is.na(value)
  ) |> 
  group_by(
    seqn
  ) |> 
  count(
    value
  ) |> 
  filterd(
    n > 2
  ) |> 
  ungroup() |> 
  pull(seqn)

most_missing

# bio_full |> count()

# bio_full <- 
#   bio_full |> 
#   filterd(
#     !seqn %in% c(most_missing)
#     # !is.na(trigly_mg_dl) &
#     # !is.na(ldl_chol_mg_dl)
#   )

data <- 
  demo |> 
  filterd(
    age >= 20
  ) |> 
  left_join(bio_full, by = "seqn")

nrow(data)
nrow(alc)
nrow(gen_health)
nrow(diet)
nrow(insurance)
nrow(med_cond)
nrow(body)
nrow(diabetes)
nrow(pa)
nrow(bp_chol)

data <- 
  data |> 
  left_join(alc, by = "seqn")

data <- 
  data |> 
  left_join(gen_health, by = "seqn")

data <- 
  data |> 
  left_join(diet, by = "seqn")

data <- 
  data |> 
  left_join(insurance, by = "seqn")

data <- 
  data |> 
  left_join(med_cond, by = "seqn")

data <- 
  data |> 
  left_join(body, by = "seqn")

data <-
  data |> 
  left_join(diabetes, by = "seqn")

data <-
  data |> 
  left_join(pa, by = "seqn")

# data <-
#   data |> 
#   left_join(hep, by = "seqn")

data <-
  data |> 
  left_join(bp_chol, by = "seqn")

# data <-
#   data |> 
#   left_join(hep_a, by = "seqn")

# data <-
#   data |> 
#   left_join(hep_b1, by = "seqn")

# data <-
#   data |> 
#   left_join(hep_b2, by = "seqn")

# data <-
#   data |> 
#   left_join(hep_c, by = "seqn")

data <- 
  data |> 
  filterd(
    annual_house_income != 77 &
    marital != 77 &
    ed != 7 &
    length_us != 77 &
    citizen != 7 &
    birth_country != 77
  ) |>
  mutate(
    across(
      c(
        marital,
        annual_house_income,
        length_us,
        birth_country
    ),
    ~case_when(
        is.na(.x) ~ NA_integer_,
      .x == 99 ~ NA_integer_,
      TRUE ~ .x
    )
  ),
  across(
      c(
        ed,
        citizen
    ),
    ~case_when(
        is.na(.x) ~ NA_integer_,
      .x == 9 ~ NA_integer_,
      TRUE ~ .x
    )
  ),
  across(
      c(
        citizen,
        length_us,
        marital,
        ed,
        annual_house_income,
        birth_country,
        race_latino,
        sex,
        total_num_house
    ),
    ~as.factor(.x)
  )
)

data <-
  data |> 
  mutate(
    male = if_else(sex == 1, 1, 0),
    across(
      c(
        # matches("_cat$"),
        # matches("_bi$"),
        told_high_bp,
        dr_told_high_chol,
        male
      ),
      ~as.factor(.x)
    )
  ) |> 
  selectd(
    -sex
  )

data <- 
  data |> 
  mutate(
    min_sedentary_hr = min_sedentary/60
  ) |> 
  select(
    -min_sedentary
  )
  
data |> 
  ggplot(
    aes(
      min_sedentary_hr
    )
  ) +
  geom_histogram(
    color = "black",
    fill = "seagreen"
  )

glimpse(data)

data |> count(race_latino)

# saveRDS(data, here::here("adult_nhanes.rds"))

inspectdf::inspect_na(data)

glimpse(data)

pred_matrix <- make.predictorMatrix(data = data)
imp_method <- make.method(data = data)

pred_matrix[, c("seqn", "birth_country")] <- 0
pred_matrix[c(
  "albumin_g_dl", "alc_drink12_yr",
  "alp_iu_l", "alt_u_l", "annual_house_income", "ast_u_l",
"bmi", "citizen", "covered_insurance",
"dr_told_exercise", "dr_told_high_chol",
"ed", "ever_45_drink_everyday", "gen_health",
"ggt_u_l", "length_us", "min_sedentary_hr",
"mod_rec_pa", "num_frozen_meal_30day", "num_meals_not_home_prepare",
"num_ready_eat_food_30day", "told_angina", "told_cancer",
"told_heart_attack", "told_high_bp", "told_jaundice",
"told_liver_cond", "told_stroke", "total_bilirubin_mg_dl",
"vig_rec_pa", "waist_circumference", "you_control_wt",
"you_reduce_fat"
),
"ldl_chol_mg_dl"] <- 0
pred_matrix[c(
  "told_prediabetes", "told_risk_diabetes",
"could_risk_diabetes", "albumin_g_dl", "alc_drink12_yr",
"alp_iu_l", "alt_u_l", "annual_house_income", "ast_u_l",
"bmi", "citizen", "covered_insurance",
"dr_told_exercise", "dr_told_high_chol",
"ed", "ever_45_drink_everyday", "gen_health",
"ggt_u_l", "length_us", "min_sedentary_hr",
"mod_rec_pa", "num_frozen_meal_30day", "num_meals_not_home_prepare",
"num_ready_eat_food_30day", "told_angina", "told_cancer",
"told_heart_attack", "told_high_bp", "told_jaundice",
"told_liver_cond", "told_stroke", "total_bilirubin_mg_dl",
"vig_rec_pa", "waist_circumference", "you_control_wt",
"you_reduce_fat"
),
"total_chol_mg_dl"] <- 0

imp_method

# imp_method[c(
#   "hdl_chol_mg_dl",
#   "trigly_mg_dl",
#   "ldl_chol_mg_dl",
#   "total_chol_mg_dl"
# )] <- "midastouch"

# imp_method[c(
#   "albumin_g_dl",
#   "alp_iu_l",
#   "ast_u_l",
#   "alt_u_l",
#   "ggt_u_l",
#   "total_bilirubin_mg_dl",
#   "bmi",
#   "waist_circumference",
#   "min_sedentary",
#   "num_meals_not_home_prepare",
#   "num_ready_eat_food_30day",
#   "num_frozen_meal_30day"
# )] <- "pmm"

# imp_method[c(
#   "citizen",
#   "length_us",
#   "ed",
#   "annual_house_income",
#   "alc_drink12_yr",
#   "ever_45_drink_everyday",
#   "gen_health",
#   "covered_insurance",
#   "told_angina",
#   "told_heart_attack",
#   "told_liver_cond",
#   "told_cancer",
#   "dr_told_exercise",          
#   "you_control_wt",
#   "you_reduce_fat",
#   "told_prediabetes",          
#   "told_risk_diabetes",
#   "could_risk_diabetes",
#   "vig_rec_pa",
#   "mod_rec_pa",
#   "told_hep_b",
#   "told_hep_c",
#   "told_high_bp",
#   "dr_told_high_chol",        
#   "hep_a_anti",
#   "hep_b_core_anti"
# )] <- "cart"

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

# prac2 <- 
#   mice(
#   data,
#   maxit = 100,
#   m = 5,
#   method = imp_method,
#   predictorMatrix = pred_matrix
# )
# prac2$loggedEvents

set.seed(12345)
model_imp <- 
  mice(
  data,
  maxit = 100,
  m = 60,
  method = imp_method,
  predictorMatrix = pred_matrix
)

# saveRDS(model_imp, here::here("RDS Files/all_race_imputed_data.rds"))

# future::plan("multisession", workers = future::availableCores() - 1)
# progressr::handlers("progress")

# set.seed(12345)
# model_imp <- progressr::with_progress(
#   futuremice(
#     data,
#     m = 60,
#     method = imp_method,
#     predictorMatrix = pred_matrix,
#     parallelseed = 12345
#   )
#   )

# model_imp$loggedEvents |> 
#   as_tibble() |> 
#   group_by(
#     out
#   ) |> 
#   count(
#     dep
#     ) |> 
#   gt::gt()

# run when done with parallel imputations
future::plan("sequential")

plot(model_imp, layout = c(10, 10))
densityplot(model_imp)
bwplot(model_imp)
xyplot(model_imp, trigly_mg_dl ~ total_chol_mg_dl)

