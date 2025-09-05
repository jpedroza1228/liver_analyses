# 3 classes ----------------------------------------------------------------------------------

# lpa3 <- cat_map(
#   indicators,
#   ~Mclust(
#     .x,
#     G = 3
#   )
# )

# saveRDS(lpa3, here::here("projects/nhanes/lpa_model_3profiles.rds"))

lpa3 <- read_rds(here::here("projects/nhanes/lpa_model_3profiles.rds"))

map_dfr(
  1:length(lpa_bic),
  ~lpa3[[.x]]$parameters$mean |>
  as_tibble() |> 
  mutate(
    indicator = colnames(indicators[[.x]])
  ) |> 
  relocate(
    indicator, 
    .before = V1
  )
) |> 
  mutate(
    imp = rep(1:60, each = 10)
  ) |> 
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

# find the average means across all imputations for each indicator
# all_profile_mean3 <- map_dfr(
#   1:length(lpa_bic),
#   ~lpa3[[.x]]$parameters$mean |>
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
#     imp = rep(1:60, each = 10)
#   )

# saveRDS(all_profile_mean3, here::here("projects/nhanes/all_imputed_profile_means_3profiles.rds"))

all_profile_mean3 <- read_rds(here::here("projects/nhanes/all_imputed_profile_means_3profiles.rds"))

all_profile_mean3 |> 
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

# find mode of each id for all imputations
# all_profile3 <- 
#   map_dfr(
#   1:length(indicators),
#   ~lpa3[[.x]]$z |> 
#     as_tibble()
# ) |> 
#   mutate(
#     id = rep(seq(1, 938, 1), 60),
#     imp = rep(1:60, each = 938)
#   )

#saveRDS(all_profile3, here::here("projects/nhanes/all_imputed_profile_probabilities_3profiles.rds"))

all_profile3 <- read_rds(here::here("projects/nhanes/all_imputed_profile_probabilities_3profiles.rds"))

all_profile3 |> 
  pivot_longer(
    -c(
      id,
      imp
    )
  ) |> 
  arrange(id) |> 
  group_by(
    id,
    imp
  ) |>
  filter(
    value == max(value)
  )

max_profile3 <- all_profile3 |> 
  pivot_longer(
    -c(
      id,
      imp
    )
  ) |> 
  arrange(id) |> 
  group_by(
    id,
    imp
  ) |>
  filter(
    value == max(value)
  ) |> 
  ungroup(imp) |> 
  count(
    name
  ) |> 
  filter(
    n == max(n)
  ) |> 
  ungroup()

# saveRDS(max_profile3, here::here("projects/nhanes/most_common_profile3.rds"))

max_profile3 <- read_rds(here::here("projects/nhanes/most_common_profile3.rds"))
mean_profile3 <- read_rds(here:::here("projects/nhanes/average_profile3.rds"))

# prof3_dupes <- max_profile3 |> 
#   count(id) |> 
#   filter(
#     n > 1
#   ) |> 
#   pull(id)
# 
# set.seed(12345)
# max_profile3_resp <- max_profile3 |> 
#   filter(
#     id %in% c(prof3_dupes)
#   ) |> 
#   select(
#     -n
#   ) |> 
#   group_by(
#     id
#   ) |> 
#   slice_sample(n = 1)
# 
# max_profile3 <- max_profile3 |> 
#   filter(
#     !id %in% c(prof3_dupes)
#   ) |> 
#   full_join(max_profile3_resp)
#   
# 
# mean_profile3 <- 
# all_profile3 |>
#   pivot_longer(
#     -c(
#       id,
#       imp
#     )
#   ) |> 
#   group_by(
#     id,
#     name
#   ) |>
#   summarize(
#     avg = mean(value)
#   ) |>
#   ungroup(name) |>
#   filter(
#     avg == max(avg)
#   ) |> 
#   ungroup()
# 
# mean_profile3 <- mean_profile3 |> 
#   mutate(
#     name = case_when(
#       name == "V1" ~ "1",
#       name == "V2" ~ "2",
#       name == "V3" ~ "3"
#     )
#   ) |> 
#   select(
#     -avg
#   )
# 
# max_profile3 <- max_profile3 |> 
#   mutate(
#     name = case_when(
#       name == "V1" ~ "1",
#       name == "V2" ~ "2",
#       name == "V3" ~ "3"
#     )
#   ) |> 
#   select(
#     -n
#   )
# 
# mean_profile3 |> 
#   full_join(max_profile3, by = "id") |> 
#   mutate(
#     same = (name.x == name.y)
#   ) |> 
#   count(same)

# saveRDS(mean_profile3, here:::here("projects/nhanes/average_profile3.rds"))

all_prof_nest3 <- all_profile3 |> 
  pivot_longer(
    -c(
      id,
      imp
    )
  ) |> 
  arrange(id) |> 
  group_by(
    id,
    imp
  ) |>
  filter(
    value == max(value)
  ) |> 
    select(
      profile3 = name,
      profile_prob3 = value
    ) |> 
    ungroup(id) |> 
    nest()



model_all_prob3 <- cat_map(
  1:length(model_all_prob),
  ~cbind(
    model_all[[.x]],
    all_prof_nest3$data[[.x]]
  )
)

# model_all_prob[[1]] |> glimpse()

# saveRDS(model_all_prob3, here::here("projects/nhanes/imputed_data_w_3profiles_each_imp.rds"))

# --------------------------------------- 3 Classes ------------------------------------------
with(
  model_lpa,
  glm(
    ever_45_drink_everyday ~ lpa3,
    family = binomial("logit") 
  )
) |> 
  pool()

with(
  model_lpa,
  glm(
    told_liver_cond ~ lpa3,
    family = binomial("logit") 
  )
) |> 
  pool()

with(
  model_lpa,
  lm(
    bmi ~ lpa3
  )
) |> 
  pool()

with(
  model_lpa,
  lm(
    waist_circumference ~ lpa3
  )
) |> 
  pool()
