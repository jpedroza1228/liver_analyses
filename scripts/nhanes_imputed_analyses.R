# start here with data
library(tidyverse)
library(mclust)


cat_map <- purrr::map

react_table <- function(data){
  reactable::reactable(
    {{data}},
    filterable = TRUE,
    sortable = TRUE,
    searchable = TRUE
  )
}

latino <- read_rds(here::here("projects/nhanes/latino_20plus.rds"))

latino |> count()

psych::describe(latino)[c("mean", "sd")]

latino |> 
  count(sex) |> 
  mutate(
    perc = n/sum(n)*100
  )

cat_map(
  latino |>
  select(
    sex,
    race_latino,
    birth_country,
    citizen,
    length_us,
    ed,
    annual_house_income
  ),
  ~count(data.frame(x=.x), x) |> 
    drop_na() |> 
      mutate(
        perc = round(n/sum(n)*100, 2)
      )
)

psych::describe(
  latino |>
    select(
      albumin_g_dl,
      alp_iu_l,
      ast_u_l,
      alt_u_l,
      ggt_u_l,
      total_bilirubin_mg_dl,
      hdl_chol_mg_dl,
      trigly_mg_dl,
      ldl_chol_mg_dl,            
      total_chol_mg_dl
    ),
    na.rm = TRUE
)[c("mean", "sd")]

inspectdf::inspect_na(latino)

model_imp <- read_rds(here::here("projects/nhanes/imputation_model2.rds"))

set.seed(12345)
model_all <- mice::complete(model_imp, "all")

#potential factor analysis/princial components analysis
# fa_df <- model_all[[1]] |> 
#   select(
#     albumin_g_dl,
#       alp_iu_l,
#       ast_u_l,
#       alt_u_l,
#       ggt_u_l,
#       total_bilirubin_mg_dl,
#       hdl_chol_mg_dl,
#       trigly_mg_dl,
#       ldl_chol_mg_dl,            
#       total_chol_mg_dl
#   )
# 
# ev <- eigen(cor(fa_df))
# 
# library(psych)
# 
# scree(fa_df, pc = FALSE)
# 
# fa.parallel(fa_df, fa = "fa")
# 
# fa_fit <- 
#   fa(
#     fa_df,
#     nfactors = 2,
#     rotate = "oblique"
#   )
# print(fa_fit)


colnames(model_all[[1]])

indicators <- 
  cat_map(
  model_all,
  ~.x |> 
    select(
      albumin_g_dl,
      alp_iu_l,
      ast_u_l,
      alt_u_l,
      ggt_u_l,
      total_bilirubin_mg_dl,
      hdl_chol_mg_dl,
      trigly_mg_dl,
      ldl_chol_mg_dl,            
      total_chol_mg_dl
    )
)

# lpa_bic <- cat_map(
#   indicators,
#   ~mclustBIC(.x)
# )

# saveRDS(lpa_bic, here::here("projects/nhanes/lpa_model_selection.rds"))

lpa_bic <- read_rds(here::here("projects/nhanes/lpa_model_selection.rds"))

# map_dfr(
#   1:length(lpa_bic),
#   ~lpa_bic[[.x]][1:9, 1:14] |> 
#     as_tibble()
# ) |> 
#   select(
#     VVE
#   ) |> 
#   mutate(
#     id = rep(seq(1, 9, 1), 100),
#     imp = rep(1:100, each = 9),
#     VVE = round(VVE, 2)
#   ) |> 
#   drop_na(VVE) |> 
#   mutate(
#     diff = VVE - lag(VVE, default = first(VVE)),
#     diff = round(diff, 2)
#   ) |> 
#   filter(
#     id %in% c(3, 4, 5, 6)
#   ) |> 
#   react_table()

map_dfr(
  1:length(lpa_bic),
  ~lpa_bic[[.x]][1:9, 1:14] |> 
    as_tibble()
) |> 
  select(
    VVE
  ) |> 
  mutate(
    id = rep(seq(1, 9, 1), 100),
    imp = rep(1:100, each = 9),
    VVE = round(VVE, 2)
  ) |> 
  drop_na(VVE) |>
  group_by(
    id
  ) |> 
  summarize(
    mean_vve = mean(VVE),
    sd_vve = sd(VVE),
    across(
      c(
        mean_vve,
        sd_vve
      ),
      ~round(.x, 1)
    )
  ) |> 
  ungroup() |> 
  mutate(
    diff = mean_vve - lag(mean_vve, default = first(mean_vve))
  ) |> 
  react_table()

Mclust(indicators[[1]], G = 5)$parameters$mean |> 
  as_tibble() |>  
  mutate(
    across(everything(), ~round(.x, 1)),
    indicator = colnames(indicators[[1]])
  ) |> 
  react_table()

colnames(indicators[[1]])

# albumin_g_dl           # 3.5 - 5.5
# alp_iu_l               # 44 - 147 iu/l
# alt_u_l                # 7-56 u/l
# ast_u_l                # 8-33 u/l
# ggt_u_l                # below 50 u/l
# HDL                    # 60 + = normal
# LDL                    # Under 100 = normal
# total_bilirubin_mg_dl  # 0.2-1.3 mg/dl
# Total Cholesterol      # Under 200 = normal
# trigly                 # under 150 = normal

# START HERE FOR NEW 100 IMPUTATIONS DATASET

# 5 Classes ----------------------------------------------------------------------------------
lpa <- cat_map(
  indicators,
  ~Mclust(
    .x,
    G = 5
  )
)

# saveRDS(lpa, here::here("projects/nhanes/lpa_model.rds"))

lpa <- read_rds( here::here("projects/nhanes/lpa_model.rds"))

# map_dfr(
#   1:length(lpa_bic),
#   ~lpa[[.x]]$parameters$mean |>
#   as_tibble() |> 
#   mutate(
#     indicator = colnames(indicators[[.x]])
#   ) |> 
#   relocate(
#     indicator, 
#     .before = V1
#   )
# ) |> 
#   mutate(
#     imp = rep(1:100, each = 10)
#   ) |> 
#     group_by(
#       indicator
#     ) |> 
#     summarize(
#       across(
#         matches("^V"),
#         ~mean(.x)
#       )
#     ) |> 
#     mutate(
#       across(
#         matches("^V"),
#         ~round(.x, 2)
#       )
#     ) |> 
#   react_table()

# find the average means across all imputations for each indicator
# all_profile_mean <- map_dfr(
#   1:length(lpa_bic),
#   ~lpa[[.x]]$parameters$mean |>
#   as_tibble() |> 
#   mutate(
#     indicator = colnames(indicators[[.x]])
#   ) |> 
#   relocate(
#     indicator, 
#     .before = V1
#   )
# ) |> 
#   mutate(
#     imp = rep(1:100, each = 10)
#   )

# saveRDS(all_profile_mean, here::here("projects/nhanes/all_imputed_profile_means.rds"))

all_profile_mean <- read_rds(here::here("projects/nhanes/all_imputed_profile_means.rds"))

all_profile_mean |> 
  group_by(
    indicator
  ) |> 
  summarize(
    across(
      matches("^V"),
      ~mean(.x)
    )
  ) |> 
  mutate(
    across(
      matches("^V"),
      ~round(.x, 2)
    )
  ) |> 
  react_table()


# albumin_g_dl           # 3.5 - 5.5
# alp_iu_l               # 44 - 147 iu/l
# alt_u_l                # 7-56 u/l
# ast_u_l                # 8-33 u/l
# ggt_u_l                # below 50 u/l
# HDL                    # 60 + = normal
# LDL                    # Under 100 = normal
# total_bilirubin_mg_dl  # 0.2-1.3 mg/dl
# Total Cholesterol      # Under 200 = normal
# trigly                 # under 150 = normal

# Group 1
# Albumin
# ALP
# ALT
# AST
# GGT
# HDL
# LDL
# total bilirubin
# total cholesterol
# triglyercides

# Group 2
# Albumin
# ALP
# ALT
# AST
# GGT
# HDL
# LDL
# total bilirubin
# total cholesterol
# triglyercides

# Group 3
# Albumin
# ALP
# ALT
# AST
# GGT
# HDL
# LDL
# total bilirubin
# total cholesterol
# triglyercides

# Group 4
# Albumin
# ALP
# ALT
# AST
# GGT
# HDL
# LDL
# total bilirubin
# total cholesterol
# triglyercides

# Group 5
# Albumin
# ALP
# ALT
# AST
# GGT
# HDL
# LDL
# total bilirubin
# total cholesterol
# triglyercides

# find mode of each id for all imputations
all_profile <- 
  map_dfr(
  1:length(indicators),
  ~lpa[[.x]]$z |> 
    as_tibble() |> 
    mutate(
      seqn = model_all[[.x]]$seqn
    )
) |> 
  mutate(
    imp = rep(1:100, each = 938)
  )

# saveRDS(all_profile, here::here("projects/nhanes/all_imputed_profile_probabilities.rds"))

all_profile <- read_rds(here::here("projects/nhanes/all_imputed_profile_probabilities.rds"))

# all_profile |> 
#   pivot_longer(
#     -c(
#       seqn,
#       imp
#     )
#   ) |> 
#   arrange(seqn) |> 
#   group_by(
#     seqn,
#     imp
#   ) |>
#   filter(
#     value == max(value)
#   )

# max_profile <- all_profile |> 
#   pivot_longer(
#     -c(
#       id,
#       imp
#     )
#   ) |> 
#   arrange(id) |> 
#   group_by(
#     id,
#     imp
#   ) |>
#   filter(
#     value == max(value)
#   ) |> 
#   ungroup(imp) |> 
#   count(
#     name
#   ) |> 
#   filter(
#     n == max(n)
#   ) |> 
#   ungroup()
  
# saveRDS(max_profile, here::here("projects/nhanes/most_common_profile.rds"))

# max_profile <- read_rds(here::here("projects/nhanes/most_common_profile.rds"))

# prof_dupes <- max_profile |> 
#   count(id) |> 
#   filter(
#     n > 1
#   ) |> 
#   pull(id)
# 
# set.seed(12345)
# max_profile_resp <- max_profile |> 
#   filter(
#     id %in% c(prof_dupes)
#   ) |> 
#   select(
#     -n
#   ) |> 
#   group_by(
#     id
#   ) |> 
#   slice_sample(n = 1)
# 
# max_profile <- max_profile |> 
#   filter(
#     !id %in% c(prof_dupes)
#   ) |> 
#   full_join(max_profile_resp)
# 
# max_profile
# 
mean_profile <- 
all_profile |>
  pivot_longer(
    -c(
      seqn,
      imp
    )
  ) |> 
  group_by(
    seqn,
    name
  ) |>
  summarize(
    avg = mean(value)
  ) |>
  ungroup(name) |>
  filter(
    avg == max(avg)
  ) |> 
  ungroup()

mean_profile <- mean_profile |> 
    mutate(
      name = case_when(
        name == "V1" ~ "1",
        name == "V2" ~ "2",
        name == "V3" ~ "3",
        name == "V4" ~ "4",
        name == "V5" ~ "5"
      )
    ) |> 
    select(
      -avg
    )

# max_profile <- max_profile |> 
#   mutate(
#     name = case_when(
#       name == "V1" ~ "1",
#       name == "V2" ~ "2",
#       name == "V3" ~ "3",
#       name == "V4" ~ "4",
#       name == "V5" ~ "5"
#     )
#   ) |> 
#   select(
#     -n
#   )
# 
# mean_profile |> 
#   full_join(max_profile, by = "id") |> 
#   mutate(
#     same = (name.x == name.y)
#   ) |> 
#   count(same)

# saveRDS(mean_profile, here:::here("projects/nhanes/average_profile.rds"))

mean_profile <- read_rds(here:::here("projects/nhanes/average_profile.rds"))

mean_profile


# ------------------------- Putting Profiles Together w/ Data ----------------------------------------------
all_prof_nest <- all_profile |> 
  pivot_longer(
    -c(
      seqn,
      imp
    )
  ) |> 
  arrange(seqn) |> 
  group_by(
    seqn,
    imp
  ) |>
  filter(
    value == max(value)
  ) |> 
  select(
    profile = name,
    profile_prob = value
  ) |> 
  ungroup(seqn) |> 
  nest()

model_all_prob5 <- cat_map(
  1:length(model_all),
  ~left_join(
    model_all[[.x]],
    all_prof_nest$data[[.x]]
  )
)

model_all_prob5[[1]] |> colnames()

# saveRDS(model_all_prob5, here::here("projects/nhanes/imputed_data_w_5profiles_each_imp.rds"))

# --------------------------------------- Pooling w/ MICE lm/glm models --------------------------------------
library(mice)
library(miceadds)

model_lpa <- datlist2mids(model_all_prob5)

# --------------------------------------- 5 Classes ------------------------------------------
with(
  model_lpa,
  glm(
    ever_45_drink_everyday ~ as.factor(profile),
    family = binomial("logit") 
  )
) |> 
  pool()

with(
  model_lpa,
  glm(
    told_liver_cond ~ as.factor(profile),
    family = binomial("logit") 
  )
) |> 
  pool()

with(
  model_lpa,
  lm(
    bmi ~ as.factor(profile)
  )
) |> 
  pool()

with(
  model_lpa,
  lm(
    waist_circumference ~ as.factor(profile)
  )
) |> 
  pool()
