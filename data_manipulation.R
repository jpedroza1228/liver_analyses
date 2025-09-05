library(tidyverse)
library(haven)
library(mice)
library(miceadds)

cat_map <- purrr::map
select <- dplyr::select

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

# ldl cholesterol & triglycerides
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TRIGLY_I.htm
ldl <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TRIGLY_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    trigly_mg_dl = lbxtr,
    ldl_chol_mg_dl = lbdldl
  )

# total cholesterol
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TCHOL_I.htm
total_chol <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TCHOL_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    total_chol_mg_dl = lbxtc
  )

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

# general health
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HSQ_I.htm
gen_health <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HSQ_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    gen_health = hsd010
  ) |> 
  mutate(
    gen_health = as.factor(gen_health)
  )

# diabetes
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DIQ_I.htm
diabetes = read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DIQ_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    told_prediabetes = diq160,
    told_risk_diabetes = diq170,
    could_risk_diabetes = diq172
  ) |> 
  filter(
    told_prediabetes != 7 &
    told_risk_diabetes != 7 &
    could_risk_diabetes != 7
  ) |> 
  mutate(
    across(
      c(
        told_prediabetes,
        told_risk_diabetes,
        could_risk_diabetes
      ),
      ~case_when(
        is.na(.x) ~ NA_integer_,
        .x == 9 ~ NA_integer_,
        TRUE ~ .x
      )
    ),
    across(
      c(
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
    num_meals_not_home_prepare = dbd895,
    num_ready_eat_food_30day = dbd905,
    num_frozen_meal_30day = dbd910
  ) |>
  filter(
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
        .x == 9 ~ NA_integer_,
        is.na(.x) ~ NA_integer_,
        TRUE ~ .x
      )
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

bio <- bio |> 
  full_join(hdl, by = "seqn") |> 
  full_join(ldl, by = "seqn") |> 
  full_join(total_chol, by = "seqn")

inspectdf::inspect_na(bio)

most_missing <- 
  bio |> 
  pivot_longer(
    -seqn
  ) |> 
  filter(
    is.na(value)
  ) |> 
  group_by(
    seqn
  ) |> 
  count(value, sort = TRUE) |>
  filter(
    n > 2
  ) |> 
  pull(seqn)

most_missing

bio <- 
  bio |> 
  filter(
    !seqn %in% c(most_missing)
    # !is.na(trigly_mg_dl) &
    # !is.na(ldl_chol_mg_dl)
  )

# Data Cleaning
# 20+ because medical conditions are 20+ sampled
latino <- demo |> 
  filter(
    race_latino %in% c(1, 2) &
    age >= 20
  ) |> 
  left_join(bio, by = "seqn")

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
    vig_rec_pa = as.factor(vig_rec_pa),
    mod_rec_pa = as.factor(mod_rec_pa),
    told_high_bp = as.factor(told_high_bp),
    dr_told_high_chol = as.factor(dr_told_high_chol)
  )

#latino <- latino |> 
#  mutate(
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
#    hdl_bi = case_when(
#      hdl_chol_mg_dl < 60 ~ 2,
#      TRUE ~ 1
#    ),
#    ldl_bi = case_when(
#      ldl_chol_mg_dl > 100 ~ 2,
#      TRUE ~ 1
#    ),
#    bilirubin_bi = case_when(
#      total_bilirubin_mg_dl > 1.3 ~ 2,
#      total_bilirubin_mg_dl < 0.2 ~ 2,
#      TRUE ~ 1
#    ),
#    total_chol_bi = case_when(
#      total_chol_mg_dl > 200 ~ 2,
#      TRUE ~ 1
#    ),
#    trigly_bi = case_when(
#      trigly_mg_dl > 150 ~ 2,
#      TRUE ~ 1
#    )
#  )

# saveRDS(latino, here::here("projects/nhanes/latino_20plus.rds"))

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