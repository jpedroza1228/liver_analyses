# All analyses combined (for sharing on GitHub)

# Loading In Packages & Functions -----------------------------

library(tidyverse)
# library(poLCA)
library(mice)
library(miceadds)
library(haven)

cat_map <- purrr::map
select <- dplyr::select

options(scipen = 9999)

react_table <- function(data){
  reactable::reactable(
    {{data}},
    filterable = TRUE,
    sortable = TRUE,
    searchable = TRUE
  )
}

chi_cramerv_func <- function(list_name, var1, var2){

  var1_sym <- sym(rlang::as_name(enquo(var1)))
  var2_sym <- sym(rlang::as_name(enquo(var2)))

  chi_find <- map_dfr(
    {{list_name}},
    ~chisq.test(
      .x[[var1_sym]],
      .x[[var2_sym]]
    )$statistic |> 
      as_tibble()
  ) |> 
    summarize(
      avg = mean(value, na.rm = TRUE),
      avg = round(avg, 3)
    )
  
  chi_p <- map_dfr(
    {{list_name}},
    ~chisq.test(
      .x[[var1_sym]],
      .x[[var2_sym]]
    )$p.value |> 
      as_tibble()
  ) |> 
    summarize(
      avg = mean(value, na.rm = TRUE),
      avg = round(avg, 3)
    )

  cramer_find <- map_dfr(
    {{list_name}},
    ~effectsize::cramers_v(
      .x[[var1_sym]],
      .x[[var2_sym]]
    )$Cramers_v_adjusted |> 
      as_tibble()
  ) |> 
    summarize(
      avg = mean(value, na.rm = TRUE),
      avg = round(avg, 3)
    )

  list(chi_find, chi_p, cramer_find)
}

# Data Wrangling -----------------------------

# 2015 - 2016 data
# https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015

# hdl cholesterol
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HDL_I.htm
hdl <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HDL_I.xpt") |> 
  janitor::clean_names() |> 
  select(
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
  select(
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
  select(
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
  select(
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
  select(
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
  select(
    seqn,
    albumin_g_dl = lbxsal, # 3.5 - 5.5
    alp_iu_l = lbxsapsi, # 44 - 147 iu/l
    ast_u_l = lbxsassi, # 8-33 u/l
    alt_u_l = lbxsatsi, # 7-56 u/l
    ggt_u_l = lbxsgtsi, # below 50 u/l
    total_bilirubin_mg_dl = lbxstb # 0.2-1.3 mg/dl
  )

#bio <- bio |> 
#  mutate(
#    albumin_cat = case_when(
#      albumin_g_dl > 5.5 ~ 3,
#      albumin_g_dl < 3.5 ~ 2,
#      TRUE ~ 1
#    ),
#    alp_cat = case_when(
#      alp_iu_l > 147 ~ 3,
#      alp_iu_l < 44 ~ 2,
#      TRUE ~ 1
#    ),
#    alt_cat = case_when(
#      alt_u_l > 56 ~ 3,
#      alt_u_l < 7 ~ 2,
#      TRUE ~ 1
#    ),
#    ast_cat = case_when(
#      ast_u_l > 33 ~ 3,
#      ast_u_l < 8 ~ 2,
#      TRUE ~ 1
#    ),
#    bilirubin_cat = case_when(
#      total_bilirubin_mg_dl > 1.3 ~ 3,
#      total_bilirubin_mg_dl < 0.2 ~ 2,
#      TRUE ~ 1
#    ),
#    albumin_bi = case_when(
#      albumin_g_dl > 5.5 ~ 2,
#      albumin_g_dl < 3.5 ~ 2,
#      TRUE ~ 1
#    ),
#    alp_bi = case_when(
#      alp_iu_l > 147 ~ 2,
#      alp_iu_l < 44 ~ 2,
#      TRUE ~ 1
#    ),
#    alt_bi = case_when(
#      alt_u_l > 56 ~ 2,
#      alt_u_l < 7 ~ 2,
#      TRUE ~ 1
#    ),
#    ast_bi = case_when(
#      ast_u_l > 33 ~ 2,
#      ast_u_l < 8 ~ 2,
#      TRUE ~ 1
#    ),
#    ggt_bi = case_when(
#      ggt_u_l > 50 ~ 2,
#      TRUE ~ 1
#    ),
#    bilirubin_bi = case_when(
#      total_bilirubin_mg_dl > 1.3 ~ 2,
#      total_bilirubin_mg_dl < 0.2 ~ 2,
#      TRUE ~ 1
#    )
#  )

#remove_plots <- pmap(
#  list(
#    bio |> 
#    select(
#    albumin_g_dl,
#    alp_iu_l,
#    ast_u_l,
#    alt_u_l,
#    total_bilirubin_mg_dl
#  ),
#  bio |> 
#    select(
#    albumin_cat,
#    alp_cat,
#    ast_cat,
#    alt_cat,
#    bilirubin_cat
#  ),
#  bio |> 
#    select(
#    albumin_g_dl,
#    alp_iu_l,
#    ast_u_l,
#    alt_u_l,
#    total_bilirubin_mg_dl
#  ) |> colnames(),
#  bio |> 
#    select(
#    albumin_cat,
#    alp_cat,
#    ast_cat,
#    alt_cat,
#    bilirubin_cat
#  ) |> colnames()
#),
#  ~ggplot(
#    bio,
#    aes(
#      ..1
#    )
#  )  +
#    geom_histogram(
#      aes(
#        fill = as.factor(..2)
#      )
#    ) +
#    labs(title = glue::glue("{..3} + {..4}")) +
#    NULL
#)


# demographics
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.htm
demo <- 
  read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.xpt") |> 
  janitor::clean_names() |> 
  select(
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
  select(
    seqn,
    alc_drink12_yr = alq101,
    ever_45_drink_everyday = alq151
  ) |> 
  filter(
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
  select(
    seqn,
    gen_health = hsd010
  ) |> 
  filter(
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
  select(
    seqn,
    # told_diabetes = diq010,
    told_prediabetes = diq160, 
    told_risk_diabetes = diq170,
    could_risk_diabetes = diq172
  ) |> 
  filter(
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
  select(
    seqn,
    # diet_healthy = dbq700,
    # eat_fastfood = cbq505,
    num_meals_not_home_prepare = dbd895,
    num_ready_eat_food_30day = dbd905,
    num_frozen_meal_30day = dbd910
  ) |>
  filter(
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
  select(
    seqn,
    vig_rec_pa = paq650,
    mod_rec_pa = paq665,
    min_sedentary = pad680
  ) |> 
  filter(
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
  select(
    seqn,
    covered_insurance = hiq011
  ) |> 
  filter(
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
  select(
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
  filter(
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
  select(
    seqn,
    told_hep_b = heq010,
    told_hep_c = heq030
  ) |> 
  filter(
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
  select(
    seqn,
    told_high_bp = bpq020,
    dr_told_high_chol = bpq080
  ) |> 
  filter(
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
  select(
    seqn,
    bmi = bmxbmi,
    waist_circumference = bmxwaist
  )

# Joining Biological Data together
bio_full <- bio |> 
  full_join(hdl, by = "seqn") |> 
  full_join(ldl, by = "seqn") |> 
  full_join(total_chol, by = "seqn")

inspectdf::inspect_na(bio_full) |> 
  inspectdf::show_plot()

most_missing <- 
  bio_full |> 
  # select(
    # -matches("cat$"),
    # -matches("bi$")
  # ) |> 
  pivot_longer(
    -seqn
  ) |> 
  filter(
    is.na(value)
  ) |> 
  group_by(
    seqn
  ) |> 
  count(
    value
  ) |> 
  filter(
    n > 2
  ) |> 
  ungroup() |> 
  pull(seqn)

most_missing

# bio_full |> count()

bio_full <- 
  bio_full |> 
  filter(
    !seqn %in% c(most_missing)
    # !is.na(trigly_mg_dl) &
    # !is.na(ldl_chol_mg_dl)
  )

# bio_full <-
  # bio_full |> 
  # mutate(
    # hdl_bi = case_when(
      # hdl_chol_mg_dl < 60 ~ 2,
      # is.na(hdl_chol_mg_dl) ~ NA_integer_,
      # TRUE ~ 1
    # ),
    # ldl_bi = case_when(
      # ldl_chol_mg_dl > 100 ~ 2,
      # is.na(ldl_chol_mg_dl) ~ NA_integer_,
      # TRUE ~ 1
    # ),
    # total_chol_bi = case_when(
      # total_chol_mg_dl > 200 ~ 2,
      # is.na(total_chol_mg_dl) ~ NA_integer_,
      # TRUE ~ 1
    # ),
    # trigly_bi = case_when(
      # trigly_mg_dl > 150 ~ 2,
      # is.na(trigly_mg_dl) ~ NA_integer_,
      # TRUE ~ 1
    # )
  # )

inspectdf::inspect_na(bio_full) |> 
  inspectdf::show_plot()

# Data Cleaning
# 20+ because medical conditions are 20+ sampled
latino <- demo |> 
  filter(
    race_latino %in% c(1, 2) &
    age >= 20
  ) |> 
  left_join(bio_full, by = "seqn")

latino <- latino |> 
  left_join(alc, by = "seqn")

latino <- latino |> 
  left_join(gen_health, by = "seqn")

latino <- 
  latino |> 
  left_join(diet, by = "seqn")

latino <- 
latino |> 
  left_join(insurance, by = "seqn")

latino <- 
latino |> 
  left_join(med_cond, by = "seqn")

latino <- 
latino |> 
  left_join(body, by = "seqn")

latino <-
  latino |> 
  left_join(diabetes, by = "seqn")

latino <-
  latino |> 
  left_join(pa, by = "seqn")

latino <-
  latino |> 
  left_join(hep, by = "seqn")

latino <-
  latino |> 
  left_join(bp_chol, by = "seqn")

latino <-
  latino |> 
  left_join(hep_a, by = "seqn")

latino <-
  latino |> 
  left_join(hep_b1, by = "seqn")

latino <-
  latino |> 
  left_join(hep_b2, by = "seqn")

latino <-
  latino |> 
  left_join(hep_c, by = "seqn")

latino <- latino |> 
  filter(
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

latino <-
  latino |> 
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
  select(
    -sex
  )

# saveRDS(latino, here::here("latino_20plus.rds"))


# Data Imputing -----------------------------
# missing data
md.pattern(latino)

inspectdf::inspect_na(latino)

pred_matrix <- make.predictorMatrix(data = latino)
imp_method <- make.method(data = latino)

pred_matrix[, "seqn"] <- 0

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

# saveRDS(model_imp, here::here("projects/nhanes/imputation_model_more_var.rds"))


plot(model_imp, layout = c(10, 10))
densityplot(model_imp)
bwplot(model_imp)
xyplot(model_imp, trigly_mg_dl ~ total_chol_mg_dl)

propplot <- function(x, formula, facet = "wrap", ...) {
  library(ggplot2)

  cd <- data.frame(mice::complete(x, "long", include = TRUE))
  cd$.imp <- factor(cd$.imp)
  
  r <- as.data.frame(is.na(x$data))
  
  impcat <- x$meth != "" & sapply(x$data, is.factor)
  vnames <- names(impcat)[impcat]
  
  if (missing(formula)) {
    formula <- as.formula(paste(paste(vnames, collapse = "+",
                                      sep = ""), "~1", sep = ""))
  }
  
  tmsx <- terms(formula[-3], data = x$data)
  xnames <- attr(tmsx, "term.labels")
  xnames <- xnames[xnames %in% vnames]
  
  if (paste(formula[3]) != "1") {
    wvars <- gsub("[[:space:]]*\\|[[:print:]]*", "", paste(formula)[3])
    # wvars <- all.vars(as.formula(paste("~", wvars)))
    wvars <- attr(terms(as.formula(paste("~", wvars))), "term.labels")
    if (grepl("\\|", formula[3])) {
      svars <- gsub("[[:print:]]*\\|[[:space:]]*", "", paste(formula)[3])
      svars <- all.vars(as.formula(paste("~", svars)))
    } else {
      svars <- ".imp"
    }
  } else {
    wvars <- NULL
    svars <- ".imp"
  }
  
  for (i in seq_along(xnames)) {
    xvar <- xnames[i]
    select <- cd$.imp != 0 & !r[, xvar]
    cd[select, xvar] <- NA
  }
  
  
  for (i in which(!wvars %in% names(cd))) {
    cd[, wvars[i]] <- with(cd, eval(parse(text = wvars[i])))
  }
  
  meltDF <- reshape2::melt(cd[, c(wvars, svars, xnames)], id.vars = c(wvars, svars))
  meltDF <- meltDF[!is.na(meltDF$value), ]
  
  
  wvars <- if (!is.null(wvars)) paste0("`", wvars, "`")
  
  a <- plyr::ddply(meltDF, c(wvars, svars, "variable", "value"), plyr::summarize,
             count = length(value))
  b <- plyr::ddply(meltDF, c(wvars, svars, "variable"), plyr::summarize,
             tot = length(value))
  mdf <- merge(a,b)
  mdf$prop <- mdf$count / mdf$tot
  
  plotDF <- merge(unique(meltDF), mdf)
  plotDF$value <- factor(plotDF$value,
                         levels = unique(unlist(lapply(x$data[, xnames], levels))),
                         ordered = T)
  
  p <- ggplot(plotDF, aes(x = value, fill = get(svars), y = prop)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme(legend.position = "bottom", ...) +
    ylab("proportion") +
    scale_fill_manual(name = "",
                      values = c("black",
                                 colorRampPalette(
                                   RColorBrewer::brewer.pal(9, "Blues"))(x$m + 3)[1:x$m + 3])) +
    guides(fill = guide_legend(nrow = 1))
  
  if (facet == "wrap")
    if (length(xnames) > 1) {
      print(p + facet_wrap(c("variable", wvars), scales = "free"))
    } else {
      if (is.null(wvars)) {
        print(p)
      } else {
        print(p + facet_wrap(wvars, scales = "free"))
      }
    }
  
  if (facet == "grid")
    if (!is.null(wvars)) {
      print(p + facet_grid(paste(paste(wvars, collapse = "+"), "~ variable"),
                           scales = "free"))
    }
}

propplot(model_imp)

# Latent Class Analyse w/ Imputed Data -----------------------------

model_imp <- read_rds(here::here("RDS Files/imputation_model_more_var.rds"))

set.seed(12345)
model_all <- mice::complete(model_imp, "all")
model_long <- mice::complete(model_imp, "long")

model_all <- cat_map(
  model_all,
  ~.x |> 
    mutate(
      albumin_bi = case_when(
        albumin_g_dl > 5.5 ~ 2,
        albumin_g_dl < 3.5 ~ 2,
        TRUE ~ 1
      ),
      alp_bi = case_when(
        alp_iu_l > 147 ~ 2,
        alp_iu_l < 44 ~ 2,
        TRUE ~ 1
      ),
      alt_bi = case_when(
        alt_u_l > 56 ~ 2,
        alt_u_l < 7 ~ 2,
        TRUE ~ 1
      ),
      ast_bi = case_when(
        ast_u_l > 33 ~ 2,
        ast_u_l < 8 ~ 2,
        TRUE ~ 1
      ),
      ggt_bi = case_when(
        ggt_u_l > 50 ~ 2,
        TRUE ~ 1
      ),
      hdl_bi = case_when(
        hdl_chol_mg_dl < 60 ~ 2,
        TRUE ~ 1
      ),
      ldl_bi = case_when(
        ldl_chol_mg_dl > 100 ~ 2,
        TRUE ~ 1
      ),
      bilirubin_bi = case_when(
        total_bilirubin_mg_dl > 1.3 ~ 2,
        total_bilirubin_mg_dl < 0.2 ~ 2,
        TRUE ~ 1
      ),
      total_chol_bi = case_when(
        total_chol_mg_dl > 200 ~ 2,
        TRUE ~ 1
      ),
      trigly_bi = case_when(
        trigly_mg_dl > 150 ~ 2,
        TRUE ~ 1
      )
    )
)

# Latent Class Analysis -------------------------------------------------------------------
indicators <- 
  cat_map(
  model_all,
  ~.x |> 
    select(
      seqn,
      # age,
      # sex,
      matches("_bi$")
    )
)

lca_func <- cbind(
  albumin_bi,
  alp_bi,
  alt_bi,
  ast_bi,
  ggt_bi,
  hdl_bi,
  ldl_bi,
  bilirubin_bi,
  # total_chol_bi,
  trigly_bi
) ~ 1

set.seed(12345)
lca_models <- cat_map(
  1:5,
  ~poLCA(
    lca_func,
    data = indicators[[1]][, -1],
    nclass = .x,
    maxiter = 10000,
    graphs = FALSE,
    nrep = 10
  )
)
reshape2::melt(lca_models[[2]]$probs, level = 2) |>
  ggplot(
    aes(
      L2,
      value,
      fill = Var2
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  geom_hline(
    yintercept = .5,
    linetype = 2,
    lwd = 1.25,
    color = "red"
    ) +
  # coord_flip() +
  facet_wrap(
    ~Var1,
    ncol = 1
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, .1)
  ) +
  see::scale_fill_okabeito() +
  theme_light() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 1
    ),
    strip.background = element_rect(
      fill = "black",
      color = "white"
    ),
    strip.text = element_text(
      size = 20
    )
  )

reshape2::melt(lca_models[[3]]$probs, level = 2) |>
  ggplot(
    aes(
      L2,
      value,
      fill = Var2
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  geom_hline(
    yintercept = .5,
    linetype = 2,
    lwd = 1.25,
    color = "red"
    ) +
  # coord_flip() +
  facet_wrap(
    ~Var1,
    ncol = 1
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, .1)
  ) +
  see::scale_fill_okabeito() +
  theme_light() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 1
    ),
    strip.background = element_rect(
      fill = "black",
      color = "white"
    ),
    strip.text = element_text(
      size = 20
    )
  )


fit <- tibble(
  classes = 1:5,
  loglik = map_dbl(
    lca_models,
    ~.x$llik
  ),
  aic = map_dbl(
    lca_models,
  ~.x$aic
  ),
  bic = map_dbl(
    lca_models,
    ~.x$bic
  )
)

fit

# Dropping Non-Discriminant Indicators
lca_func2 <- cbind(
  # albumin_bi,
  # alp_bi,
  alt_bi,
  ast_bi,
  ggt_bi,
  hdl_bi,
  ldl_bi,
  # bilirubin_bi,
  # total_chol_bi,
  trigly_bi
) ~ 1


lca_func <- function(data){
  cat_map(
    1:5,
    ~poLCA(
      lca_func2,
      data = data,
      nclass = .x,
      maxiter = 10000,
      graphs = FALSE,
      nrep = 10
    )
  )
}

lca_func(indicators[[1]])

set.seed(12345)
lca_diff <- cat_map(
  indicators,
  ~lca_func(
    data = .x
  )
)

reshape2::melt(lca_diff[[1]][[2]]$probs, level = 2) |>
  ggplot(
    aes(
      L2,
      value,
      fill = Var2
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  geom_hline(
    yintercept = .5,
    linetype = 2,
    lwd = 1.25,
    color = "red"
    ) +
  # coord_flip() +
  facet_wrap(
    ~Var1,
    ncol = 1
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, .1)
  ) +
  see::scale_fill_okabeito() +
  theme_light() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 1
    ),
    strip.background = element_rect(
      fill = "black",
      color = "white"
    ),
    strip.text = element_text(
      size = 20
    )
  )

# saveRDS(lca_diff, here::here("RDS Files/lca_classes1to5_allimputations_nototal.rds"))

# Fit Indices Comparisons

bic_tbl <- tibble(
  class1 = map_dbl(
  1:length(indicators),
  ~lca_diff[[.x]][[1]]$bic
  ),
  class2 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[2]]$bic
    ),
  class3 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[3]]$bic
    ),
  class4 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[4]]$bic
    ),
  class5 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[5]]$bic
    )
)

aic_tbl <- tibble(
  class1 = map_dbl(
  1:length(indicators),
  ~lca_diff[[.x]][[1]]$aic
  ),
  class2 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[2]]$aic
    ),
  class3 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[3]]$aic
    ),
  class4 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[4]]$aic
    ),
  class5 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[5]]$aic
    )
)

llik_tbl <- tibble(
  class1 = map_dbl(
  1:length(indicators),
  ~lca_diff[[.x]][[1]]$llik
  ),
  class2 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[2]]$llik
    ),
  class3 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[3]]$llik
    ),
  class4 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[4]]$llik
    ),
  class5 = map_dbl(
    1:length(indicators),
    ~lca_diff[[.x]][[5]]$llik
    )
)

fit_indices <-
  rbind(
  llik_tbl |> 
  summarize(
    across(
      everything(),
      ~mean(.x, na.rm = TRUE)
    )
  ) |> 
  mutate(
    type = "log_lik"
  ),
  aic_tbl |> 
  summarize(
    across(
      everything(),
      ~mean(.x, na.rm = TRUE)
    )
  ) |> 
  mutate(
    type = "aic"
  ),
  bic_tbl |> 
  summarize(
    across(
      everything(),
      ~mean(.x, na.rm = TRUE)
    )
  ) |> 
  mutate(
    type = "bic"
  )
)

fit_indices

# saveRDS(fit_indices, here::here("RDS Files/fit_indices_classes1to5_nototal.RDS"))


lca2class <- 
  cat_map(
    1:length(lca_diff),
    ~lca_diff[[.x]][[2]]
  )

reshape2::melt(lca2class[[1]]$probs, level = 2) |>
  ggplot(
    aes(
      L2,
      value,
      fill = Var2
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  geom_hline(
    yintercept = .5,
    linetype = 2,
    lwd = 1.25,
    color = "red"
    ) +
  # coord_flip() +
  facet_wrap(
    ~Var1,
    ncol = 1
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, .1)
  ) +
  see::scale_fill_okabeito() +
  theme_light() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 1
    ),
    strip.background = element_rect(
      fill = "black",
      color = "white"
    ),
    strip.text = element_text(
      size = 20
    )
  )

lca3class <- 
  cat_map(
    1:length(lca_diff),
    ~lca_diff[[.x]][[3]]
  )

reshape2::melt(lca3class[[1]]$probs, level = 2) |>
  ggplot(
    aes(
      L2,
      value,
      fill = Var2
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "stack"
  ) +
  geom_hline(
    yintercept = .5,
    linetype = 2,
    lwd = 1.25,
    color = "red"
    ) +
  # coord_flip() +
  facet_wrap(
    ~Var1,
    ncol = 1
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, .1)
  ) +
  see::scale_fill_okabeito() +
  theme_light() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 1
    ),
    strip.background = element_rect(
      fill = "black",
      color = "white"
    ),
    strip.text = element_text(
      size = 20
    )
  )

# Stuck w/ 2 classes due to class 2 & 3 looking similar
# Also model parsimony along with slight differences in fit indices
# lead to using 2 classes


alt_sep2 <- map_dfr(1:60, ~lca2class[[.x]]$probs$alt_bi[,1] |> as_tibble()) |> 
  mutate(
    imp = rep(1:60, each = 2),
    initial_class = rep(1:2, 60)
  ) |> 
  rename(
    alt_norm = value
  ) |> 
  group_by(
    imp
  ) |> 
  mutate(
    alt_ab = 1 - alt_norm,
    lat_class = case_when(
      alt_norm > .8 ~ "class1",
      alt_norm <= .8 ~ "class2"
    )
  ) |>
  ungroup() |> 
  select(
    imp,
    initial_class,
    lat_class,
    alt_norm,
    alt_ab
  )

ast_sep2 <- map_dfr(1:60, ~lca2class[[.x]]$probs$ast_bi[,1] |> as_tibble()) |> 
  mutate(
    imp = rep(1:60, each = 2),
    initial_class = rep(1:2, 60)
  ) |> 
  rename(
    ast_norm = value
  ) |> 
  group_by(
    imp
  ) |> 
  mutate(
    ast_ab = 1 - ast_norm
  ) |> 
  ungroup()

ggt_sep2 <- map_dfr(1:60, ~lca2class[[.x]]$probs$ggt_bi[,1] |> as_tibble()) |> 
  mutate(
    imp = rep(1:60, each = 2),
    initial_class = rep(1:2, 60)
  ) |> 
  rename(
    ggt_norm = value
  ) |> 
  group_by(
    imp
  ) |> 
  mutate(
    ggt_ab = 1 - ggt_norm
  ) |> 
  ungroup()
# ggt_sep2

hdl_sep2 <- map_dfr(1:60, ~lca2class[[.x]]$probs$hdl_bi[,1] |> as_tibble()) |> 
  mutate(
    imp = rep(1:60, each = 2),
    initial_class = rep(1:2, 60)
  ) |> 
  rename(
    hdl_norm = value
  ) |> 
  group_by(
    imp
  ) |> 
  mutate(
    hdl_ab = 1 - hdl_norm
  ) |> 
  ungroup()

ldl_sep2 <- map_dfr(1:60, ~lca2class[[.x]]$probs$ldl_bi[,1] |> as_tibble()) |> 
  mutate(
    imp = rep(1:60, each = 2),
    initial_class = rep(1:2, 60)
  ) |> 
  rename(
    ldl_norm = value
  ) |> 
  group_by(
    imp
  ) |> 
  mutate(
    ldl_ab = 1 - ldl_norm
  ) |> 
  ungroup()

trigly_sep2 <- map_dfr(1:60, ~lca2class[[.x]]$probs$trigly_bi[,1] |> as_tibble()) |> 
  mutate(
    imp = rep(1:60, each = 2),
    initial_class = rep(1:2, 60)
  ) |> 
  rename(
   trigly_norm = value
  ) |> 
  group_by(
    imp
  ) |> 
  mutate(
    trigly_ab = 1 - trigly_norm
  ) |> 
  ungroup()

lca_group <-
  alt_sep2 |> 
  left_join(ast_sep2) |> 
  left_join(ggt_sep2) |> 
  left_join(hdl_sep2) |> 
  left_join(ldl_sep2) |> 
  left_join(trigly_sep2)

# Made sure all the classes being referenced would all match
lca_group

lca_group1 <-
  lca_group |> 
  group_by(
    lat_class
  ) |> 
  summarize(
    across(
      -c(
        imp
      ),
      ~mean(.x, na.rm = TRUE)
    )
  ) |> 
  ungroup() |> 
  select(
    lat_class,
    matches("_ab$")
  ) |> 
  mutate(
    initial_class = case_when(
      lat_class == "class1" ~ "1",
      lat_class == "class2" ~ "2"
    )
  ) |> 
  relocate(
    initial_class,
    .after = lat_class
  )

lca_group1

lca_group2 <-
  lca_group |> 
  group_by(
    lat_class
  ) |> 
  summarize(
    across(
      -c(
        imp
      ),
      ~mean(.x, na.rm = TRUE)
    )
  ) |> 
  ungroup() |> 
  select(
    lat_class,
    matches("_norm$")
  ) |> 
  mutate(
    initial_class = case_when(
      lat_class == "class1" ~ "1",
      lat_class == "class2" ~ "2"
    )
  ) |> 
  relocate(
    initial_class,
    .after = lat_class
  )

lca_grouped <- rbind(
  lca_group1 |>
  rename_with(
    ~str_remove(
      .x,
      "_ab$"
    )
  ),
  lca_group2 |>
  rename_with(
    ~str_remove(
      .x,
      "_norm$"
    )
  )
) |> 
  mutate(
    type = c(rep("abnormal", 2), rep("normal", 2))
  )

lca_grouped
# class 1 = cholesterol issues
# class 2 = most at risk

# saveRDS(lca_grouped, here::here("RDS Files/lca_class_combined_nototal.rds"))


lca_grouped |> 
  pivot_longer(
    -c(
      lat_class,
      initial_class,
      type
    )
  ) |> 
  mutate(
    type = as.factor(type),
    type = relevel(type, ref = "normal")
  ) |> 
  ggplot(
    aes(
      name,
      value
    )
  ) +
  geom_col(
    aes(
      fill = type
    ),
    color = "black"
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = 2,
    lwd = 1.25,
    color = "red" 
  ) +
  coord_flip() +
  facet_wrap(
    vars(lat_class),
    ncol = 1
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, .1)
  ) +
  see::scale_fill_okabeito() +
  theme_light() +
  theme(
    legend.position = "top",
    axis.text = element_text(
      size = 14
    )
  )

lca2class[[1]]$posterior |> 
  as_tibble() |> 
  mutate(
    seqn = indicators[[1]]$seqn
  ) |> 
    pivot_longer(
  -seqn
    ) |>  
  group_by(seqn) |> 
    filter(
      value == max(value)
    ) |> 
    ungroup() |> 
  head()

indicators[[1]] |> 
  filter(seqn %in% c(83737, 83742, 83775, 83787, 83818, 83823))


posterior_counts <- 
  cat_map(
  1:length(lca2class),
  ~lca2class[[.x]]$posterior |> 
    as_tibble() |> 
    mutate(
      seqn = indicators[[.x]][,1]
    ) |> 
      relocate(
        seqn,
        .before = V1
      ) |>
      pivot_longer(
        -c(
          seqn
        )
      ) |> 
      group_by(
        seqn
      ) |> 
      filter(
        value == max(value)
      ) |> 
      ungroup(seqn) |> 
      count(name)
) 

posterior_counts[[1]]


class_membership <- 
  cat_map(
  1:length(lca2class),
  ~lca2class[[.x]]$posterior |> 
    as_tibble() |> 
    mutate(
      seqn = indicators[[.x]][,1]
    ) |> 
      relocate(
        seqn,
        .before = V1
      ) |>
      pivot_longer(
        -c(
          seqn
        )
      ) |> 
      group_by(
        seqn
      ) |> 
      filter(
        value == max(value)
      ) |> 
      ungroup() |> 
      left_join(
        posterior_counts[[.x]]
      ) |> 
      mutate(
        latent_class = case_when(
          n < 100 ~ "class2",
          n > 500 ~ "class1"
        )
      )
)

# The smallest n class (group 2) is what I'm determining is class 2 (most at risk)
# largest n class (group 1) is class 1 (cholesterol issues)

model_all <- 
  cat_map(
  1:length(model_all),
  ~left_join(
    model_all[[.x]],
    class_membership[[.x]] |> 
    select(
      seqn,
      lat_class_prob = value,
      lat_class_size = n,
      lat_class = latent_class
    )
  )
)

model_all <-
  cat_map(
    model_all,
    ~.x |> 
      mutate(
        lat_class = as.factor(lat_class),
        lat_class = relevel(lat_class, ref = "class1")
      )
)

model_all

# saveRDS(model_all, here::here("RDS Files/data_w_lca_class_membership_nototal.RDS"))


# ------------------------------------------------------- Can start here instead to use RDS files ---------------------------------------------
library(tidyverse)
library(poLCA)
library(mice)
library(miceadds)
library(haven)

latino <- read_rds(here::here("RDS Files/latino_20plus.rds"))
# latino <- read_rds(here::here("RDS Files/latino_20plus_num2.rds"))
lca_diff <- read_rds(here::here("RDS Files/lca_classes1to5_allimputations_nototal.rds"))
fit_indices <- read_rds(here::here("RDS Files/fit_indices_classes1to5_nototal.RDS"))
lca_grouped <- read_rds(here::here("RDS Files/lca_class_combined_nototal.rds"))
model_all <- read_rds(here::here("RDS Files/data_w_lca_class_membership_nototal.RDS"))

cat_map <- purrr::map
select <- dplyr::select

options(scipen = 9999)

react_table <- function(data){
  reactable::reactable(
    {{data}},
    filterable = TRUE,
    sortable = TRUE,
    searchable = TRUE
  )
}

chi_cramerv_func <- function(list_name, var1, var2){

  var1_sym <- sym(rlang::as_name(enquo(var1)))
  var2_sym <- sym(rlang::as_name(enquo(var2)))

  chi_find <- map_dfr(
    {{list_name}},
    ~chisq.test(
      .x[[var1_sym]],
      .x[[var2_sym]]
    )$statistic |> 
      as_tibble()
  ) |> 
    summarize(
      avg = mean(value, na.rm = TRUE),
      avg = round(avg, 3)
    )
  
  chi_p <- map_dfr(
    {{list_name}},
    ~chisq.test(
      .x[[var1_sym]],
      .x[[var2_sym]]
    )$p.value |> 
      as_tibble()
  ) |> 
    summarize(
      avg = mean(value, na.rm = TRUE),
      avg = round(avg, 3)
    )

  cramer_find <- map_dfr(
    {{list_name}},
    ~effectsize::cramers_v(
      .x[[var1_sym]],
      .x[[var2_sym]]
    )$Cramers_v_adjusted |> 
      as_tibble()
  ) |> 
    summarize(
      avg = mean(value, na.rm = TRUE),
      avg = round(avg, 3)
    )

  list(chi_find, chi_p, cramer_find)
}

model_all <-
  cat_map(
  model_all,
  ~.x |> 
    mutate(
      fatty_liver_z = 0.953 * log(trigly_mg_dl) + 0.139 * bmi + 0.718 * log(ggt_u_l) + 0.053 * waist_circumference - 15.745,
      fatty_liver_index = exp(fatty_liver_z)/(1 + exp(fatty_liver_z))*100,
      fatty_liver_group = case_when(
        fatty_liver_index < 30 ~ "low_risk",
        fatty_liver_index >= 30 & fatty_liver_index <= 60 ~ "medium_risk", 
        fatty_liver_index > 60 ~ "high_risk"
      ),
      fatty_liver_group = as.factor(fatty_liver_group)
    )
)

model_all <- cat_map(
  model_all,
  ~.x |> 
    mutate(
      fatty_liver_group = as.factor(fatty_liver_group),
      fatty_liver_group = relevel(fatty_liver_group, ref = "low_risk"),
      fatty_liver_group = fct_relevel(
        fatty_liver_group,
        "low_risk",
        "medium_risk",
        "high_risk"
      )
    )
)

model_all <- cat_map(
  model_all,
  ~.x |> 
    mutate(
      across(
        c(
          covered_insurance,
          vig_rec_pa,
          told_liver_cond,
          told_risk_diabetes,
          sex,
          ever_45_drink_everyday
        ),
        ~if_else(.x == "2", 0, 1)
      ),
      across(
        c(
          sex,
          covered_insurance,
          ever_45_drink_everyday,
          told_liver_cond,
          told_risk_diabetes,
          vig_rec_pa
        ),
        ~as.factor(.x)
      )
    )
)

model_all <-
  cat_map(
  model_all,
  ~.x |> 
    mutate(
      across(
        c(
          told_angina,
          told_heart_attack,
          told_stroke,
          told_jaundice,
          told_cancer,
          told_risk_diabetes,
          told_prediabetes
        ),
        ~if_else(
          .x == 1,
          1,
          0
        )
      )
    )
)

model_all <- 
  cat_map(
  model_all,
  ~.x |> 
    mutate(
      min_sedentary_hr = min_sedentary/60
    )
)

# Percentages for Latent Classes
map_dfr(
  model_all,
  ~.x |> 
    count(lat_class) |> 
    mutate(
      prop = n/sum(n)*100
    ) |> 
    filter(
      lat_class == "class1"
    ) |> 
    pull(prop) |> 
    as_tibble()
) |> 
  summarize(
    avg = mean(value)
  )

map_dfr(
  model_all,
  ~.x |> 
    count(lat_class) |> 
    mutate(
      prop = n/sum(n)*100
    ) |> 
    filter(
      lat_class == "class2"
    ) |> 
    pull(prop) |> 
    as_tibble()
) |> 
  summarize(
    avg = mean(value)
  )

# Paper Black & White Version of LCA Findings
lca_grouped |> 
  pivot_longer(
    -c(
      lat_class,
      initial_class,
      type
    )
  ) |> 
  mutate(
    type = as.factor(type),
    type = relevel(type, ref = "normal"),
    name = str_to_upper(name),
    name = case_when(
      name == "TRIGLY" ~ "Triglycerides",
      TRUE ~ name
    ),
    lat_class = case_when(
      lat_class == "class1" ~ "Cholesterol Issues",
      lat_class == "class2" ~ "At-Risk"
    ),
    lat_class = fct_relevel(
      as.factor(lat_class),
      "Cholesterol Issues",
      "At-Risk"
    )
  ) |> 
  filter(
    type == "abnormal"
  ) |> 
  ggplot(
    aes(
      name,
      value
    )
  ) +
  geom_col(
    fill = "gray70",
    color = "black"
  ) +
  geom_hline(
  yintercept = 0.5,
  linetype = 3,
  lwd = 1.25,
  color = "black" 
  ) +
  coord_flip() +
  facet_wrap(
    vars(lat_class),
    ncol = 1
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, .1)
  ) +
  # scale_fill_manual(
  #   values = c("gray30", "gray70"),
  #   labels = c("normal", "abnormal")
  # ) +
  labs(
    x = "",
    y = "Probability"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.text = element_text(
      size = 14,
      color = "black"
    ),
    axis.title = element_text(
      size = 20,
      color = "black"
    ),
    strip.text = element_text(
      size = 20,
      color = "black"
    )
  )

# Associations Between Indicators -------------------------------------

albumin_values <- list()
alp_values <- list()
alt_values <- list()
ast_values <- list()
ggt_values <- list()
hdl_values <- list()
ldl_values <- list()
bilirubin_values <- list()
total_chol_values <- list()
trigly_values <- list()

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  albumin_values[[var]] <- chi_cramerv_func(
    model_all,
    albumin_bi,
    {{var}}
  )
}

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  alp_values[[var]] <- chi_cramerv_func(
    model_all,
    alp_bi,
    {{var}}
  )
}

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  alt_values[[var]] <- chi_cramerv_func(
    model_all,
    alt_bi,
    {{var}}
  )
}

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  ast_values[[var]] <- chi_cramerv_func(
    model_all,
    ast_bi,
    {{var}}
  )
}

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  ggt_values[[var]] <- chi_cramerv_func(
    model_all,
    ggt_bi,
    {{var}}
  )
}

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  hdl_values[[var]] <- chi_cramerv_func(
    model_all,
    hdl_bi,
    {{var}}
  )
}

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  ldl_values[[var]] <- chi_cramerv_func(
    model_all,
    ldl_bi,
    {{var}}
  )
}

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  bilirubin_values[[var]] <- chi_cramerv_func(
    model_all,
    bilirubin_bi,
    {{var}}
  )
}

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  total_chol_values[[var]] <- chi_cramerv_func(
    model_all,
    total_chol_bi,
    {{var}}
  )
}

for (var in model_all[[1]] |> select(matches("bi$")) |> names()){
  var_sym <- sym(var)

  trigly_values[[var]] <- chi_cramerv_func(
    model_all,
    trigly_bi,
    {{var}}
  )
}

albumin_values
alp_values
alt_values
ast_values
ggt_values
hdl_values
ldl_values
bilirubin_values
total_chol_values
trigly_values

# Modeling w/ Latent Classes --------------------------------------

set.seed(12345)
model_mids <- datlist2mids(model_all)

set.seed(12345)
control_fit <- with(
  model_mids,
  glm(
    lat_class ~ age + sex + covered_insurance,
    family = binomial(link = "logit")
  )
)

control_pool <- pool(control_fit)
broom::tidy(control_pool) |> 
  mutate(
    exp = exp(control_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(control_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()

# Class 1 is the reference group (healthy group/cholesterol issues)

set.seed(12345)
control_fatty_fit <- with(
  model_mids,
  nnet::multinom(
    fatty_liver_group ~ age + sex + covered_insurance
  )
)

control_fatty_pool <- pool(control_fatty_fit)
broom::tidy(control_fatty_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
    exp = exp(control_fatty_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(control_fatty_pool$pooled$estimate),
    across(
      -c(
        term,
        y.level
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob,
    y.level
  ) |> 
  react_table()

# Told Liver Condition

# sex = male, covered_insurance = has insurance, vig_rec_pa = engages in vigorous recreational physical activity, ever_45_drink_everyday = has drank 4/5 drinks everyday at one point,
# told_liver_cond = told liver condition, told_risk_diabetes = told at risk for diabetes

set.seed(12345)
liver_cond_fit <-
  with(
  model_mids,
  glm(
    told_liver_cond ~ age + sex + covered_insurance +
    vig_rec_pa + 
    # min_sedentary +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    ever_45_drink_everyday +
    lat_class,
    family = binomial("logit")
  )
)

liver_cond_pool <- pool(liver_cond_fit)
#liver_cond_pool
broom::tidy(liver_cond_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
    exp = exp(liver_cond_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(liver_cond_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()

set.seed(12345)
liver_cond_mod_fit <-
  with(
  model_mids,
  glm(
    told_liver_cond ~ age + sex + covered_insurance +
    vig_rec_pa*lat_class + 
    num_ready_eat_food_30day*lat_class +
    ever_45_drink_everyday*lat_class +
    min_sedentary_hr*lat_class,
    family = binomial("logit")
  )
)

liver_cond_mod_pool <- pool(liver_cond_mod_fit)
#liver_cond_sed_pool
broom::tidy(liver_cond_mod_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
    exp = exp(liver_cond_mod_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(liver_cond_mod_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()


# fatty liver index instead of latent classes
set.seed(12345)
liver_cond_fatty_fit <-
  with(
  model_mids,
  glm(
    told_liver_cond ~ age + sex + covered_insurance +
    vig_rec_pa + 
    # min_sedentary +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    ever_45_drink_everyday +
    fatty_liver_group
    ,
    family = binomial("logit")
  )
)

liver_cond_fatty_pool <- pool(liver_cond_fatty_fit)
#liver_cond_fatty_pool
broom::tidy(liver_cond_fatty_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
    exp = exp(liver_cond_fatty_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(liver_cond_fatty_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()


# Waist Circumference
set.seed(12345)
wt_fit <-
  with(
  model_mids,
  lm(
    waist_circumference ~ age + sex + covered_insurance +
    vig_rec_pa + 
    # min_sedentary +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    ever_45_drink_everyday +
    lat_class
  )
)

wt_pool <- pool(wt_fit)
#wt_pool
broom::tidy(wt_pool) |> 
  mutate(
    conf95_low = estimate - 1.96*std.error,
    conf95_high = estimate + 1.96*std.error,
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    matches("conf95")
  ) |> 
  react_table()

# Body Mass Index
set.seed(12345)
bmi_fit <-
  with(
  model_mids,
  lm(
    bmi ~ age + sex + covered_insurance +
    vig_rec_pa + 
    # min_sedentary +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    ever_45_drink_everyday +
    lat_class
  )
)

bmi_pool <- pool(bmi_fit)
#wt_pool
broom::tidy(bmi_pool) |> 
  mutate(
    conf95_low = estimate - 1.96*std.error,
    conf95_high = estimate + 1.96*std.error,
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    matches("conf95")
  ) |> 
  react_table()

# Excessive Drinking
set.seed(12345)
drink45_fit <-
  with(
  model_mids,
  glm(
    ever_45_drink_everyday ~ age + sex + covered_insurance +
    vig_rec_pa + 
    # min_sedentary +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    lat_class,
    family = binomial("logit")
  )
)

drink45_pool <- pool(drink45_fit)
#drink45_pool
broom::tidy(drink45_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
    exp = exp(drink45_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(drink45_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()

set.seed(12345)
drink45_fatty_fit <-
  with(
  model_mids,
  glm(
    ever_45_drink_everyday ~ age + sex + covered_insurance +
    vig_rec_pa + 
    # min_sedentary +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    fatty_liver_group,
    family = binomial("logit")
  )
)

drink45_fatty_pool <- pool(drink45_fatty_fit)
#drink45_fatty_pool
broom::tidy(drink45_fatty_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
    exp = exp(drink45_fatty_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(drink45_fatty_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()

# Diabetes
set.seed(12345)
diabetes_risk_fit <-
  with(
  model_mids,
  glm(
    told_risk_diabetes ~ age + sex + covered_insurance +
    vig_rec_pa +
    # min_sedentary +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    lat_class,
    family = binomial("logit")
  )
)

diabetes_risk_pool <- pool(diabetes_risk_fit)
#diabetes_risk_pool
broom::tidy(diabetes_risk_pool) |> 
  mutate(
    #conf95_low = estimate - 1.96*std.error,
    #conf95_high = estimate + 1.96*std.error,
    exp = exp(diabetes_risk_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(diabetes_risk_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      #~round(.x, 4)
      ~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()

set.seed(12345)
diabetes_risk_fatty_fit <-
  with(
  model_mids,
  glm(
    told_risk_diabetes ~ age + sex + covered_insurance +
    vig_rec_pa + 
    # min_sedentary +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    fatty_liver_group,
    family = binomial("logit")
  )
)

diabetes_risk_fatty_pool <- pool(diabetes_risk_fatty_fit)
#diabetes_risk_fatty_pool
broom::tidy(diabetes_risk_fatty_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
    exp = exp(diabetes_risk_fatty_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(diabetes_risk_fatty_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      #~round(.x, 4)
      ~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()

set.seed(12345)
fatty_fit <- with(
  model_mids,
  lm(
    fatty_liver_index ~ age + sex + covered_insurance +
    vig_rec_pa + 
    min_sedentary_hr +
    num_ready_eat_food_30day +
    ever_45_drink_everyday +
    lat_class
  )
)

fatty_pool <- pool(fatty_fit)
broom::tidy(fatty_pool) |> 
  mutate(
    conf95_low = estimate - 1.96*std.error,
    conf95_high = estimate + 1.96*std.error,
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    matches("conf95"),
  ) |> 
  react_table()



# ----------------------------------------------------exploratory analyses------------------------------
set.seed(12345)
liver_cond_mod_fit <-
  with(
  model_mids,
  glm(
    told_liver_cond ~ age + sex + covered_insurance +
    vig_rec_pa*lat_class + 
    num_ready_eat_food_30day*lat_class +
    ever_45_drink_everyday*lat_class +
    min_sedentary_hr*lat_class,
    family = binomial("logit")
  )
)

liver_cond_mod_pool <- pool(liver_cond_mod_fit)
#liver_cond_mod_pool
broom::tidy(liver_cond_mod_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
    exp = exp(liver_cond_mod_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(liver_cond_mod_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()



set.seed(12345)
wt_mod_fit <-
  with(
  model_mids,
  lm(
    waist_circumference ~ age + sex + covered_insurance +
    vig_rec_pa*lat_class + 
    num_ready_eat_food_30day*lat_class +
    ever_45_drink_everyday*lat_class +
    min_sedentary_hr*lat_class
  )
)

wt_mod_pool <- pool(wt_mod_fit)
#wt_mod_pool
broom::tidy(wt_mod_pool) |> 
  mutate(
    conf95_low = estimate - 1.96*std.error,
    conf95_high = estimate + 1.96*std.error,
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    matches("conf95")
  ) |> 
  react_table()


set.seed(12345)
bmi_mod_fit <-
  with(
  model_mids,
  lm(
    bmi ~ age + sex + covered_insurance +
    vig_rec_pa*lat_class + 
    num_ready_eat_food_30day*lat_class +
    ever_45_drink_everyday*lat_class +
    min_sedentary_hr*lat_class
  )
)

bmi_mod_pool <- pool(bmi_mod_fit)
#bmi_mod_pool
broom::tidy(bmi_mod_pool) |> 
  mutate(
    conf95_low = estimate - 1.96*std.error,
    conf95_high = estimate + 1.96*std.error,
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    matches("conf95")
  ) |> 
  react_table()


set.seed(12345)
drink45_mod_fit <-
  with(
  model_mids,
  glm(
    ever_45_drink_everyday ~ age + sex + covered_insurance +
    vig_rec_pa*lat_class + 
    min_sedentary_hr*lat_class +
    num_ready_eat_food_30day*lat_class,
    family = binomial("logit")
  )
)

drink45_mod_pool <- pool(drink45_mod_fit)
#drink45_mod_pool
broom::tidy(drink45_mod_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
    exp = exp(drink45_mod_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(drink45_mod_pool$pooled$estimate),
    across(
      -c(
        term
      ),
      ~round(.x, 4)
      #~round(.x, 2)
    )
  ) |> 
  select(
    term:p.value,
    exp,
    matches("conf95"),
    prob
  ) |> 
  react_table()
