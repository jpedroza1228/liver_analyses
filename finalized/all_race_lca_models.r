library(tidyverse)
library(poLCA)

theme_set(theme_light())

cat_map <- purrr::map
select <- dplyr::select

mycolor <- "seagreen"

model_imp <- read_rds(here::here("RDS Files/all_race_imputed_data.rds"))

set.seed(12345)
model_all <- mice::complete(model_imp, "all")
# model_long <- mice::complete(model_imp, "long")

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

# indicators <- 
#   cat_map(
#   model_all,
#   ~.x |> 
#     select(
#       seqn,
#       # age,
#       # sex,
#       matches("_bi$"),
#       -total_chol_bi
#     )
# )

# lca_func <- cbind(
#   albumin_bi,
#   alp_bi,
#   alt_bi,
#   ast_bi,
#   ggt_bi,
#   hdl_bi,
#   ldl_bi,
#   bilirubin_bi,
#   trigly_bi
# ) ~ 1

# set.seed(12345)
# lca_models <- cat_map(
#   1:5,
#   ~poLCA(
#     lca_func,
#     data = indicators[[1]][, -1],
#     nclass = .x,
#     maxiter = 10000,
#     graphs = FALSE,
#     nrep = 20
#   )
# )
# # class 5 did not find maximum likelihood

# lrt_func <- function(model, first_class, second_class){
#   first_idx <- as.numeric(first_class)
#   second_idx <- as.numeric(second_class)

#   tidyLPA::calc_lrt(
#     model[[first_idx]]$N,
#     model[[first_idx]]$llik,
#     model[[first_idx]]$npar,
#     length(model[[first_idx]]$P),
#     model[[second_idx]]$llik,
#     model[[second_idx]]$npar,
#     length(model[[second_idx]]$P)
#   )
# }

# lrt_func(lca_models, 1, 2)
# lrt_func(lca_models, 2, 3)
# lrt_func(lca_models, 3, 4)

# tibble(
#   model = c(1:5),
#   log_lik = map_dbl(1:5, ~lca_models[[.x]]$llik),
#   aic = map_dbl(1:5, ~lca_models[[.x]]$aic),
#   bic = map_dbl(1:5, ~lca_models[[.x]]$bic)
# ) |> 
#   mutate(
#     log_lik_lag = lag(log_lik),
#     aic_lag = lag(aic),
#     bic_lag = lag(bic),
#     log_lik_diff = log_lik_lag - log_lik,
#     aic_diff = aic_lag - aic,
#     bic_diff = bic_lag - bic
#   ) |> 
#   select(
#     -matches(
#       "lag"
#     )
#   )

# map_dfr(
#   1:length(lca_models),
#   ~tibble(
#     model = .x,
#     model_entropy = poLCA.entropy(lca_models[[.x]]),
#     max_entropy = log(prod(sapply(lca_models[[.x]]$probs,ncol))),
#   )
# )

# cat_map(lca_models, ~.x$P)

# model_plot <- cat_map(
#   lca_models,
#   ~reshape2::melt(.x$probs, level = 2) |>
#   ggplot(
#     aes(
#       L2,
#       value,
#       fill = Var2
#     )
#   ) +
#   geom_bar(
#     stat = "identity",
#     position = "stack"
#   ) +
#   geom_hline(
#     yintercept = .5,
#     linetype = 2,
#     lwd = 1.25,
#     color = "red"
#     ) +
#   # coord_flip() +
#   facet_wrap(
#     ~Var1,
#     ncol = 1
#   ) +
#   scale_y_continuous(
#     limits = c(0, 1.05),
#     breaks = seq(0, 1, .1)
#   ) +
#   see::scale_fill_okabeito() +
#   theme_light() +
#   theme(
#     axis.text.x = element_text(
#       angle = 45,
#       vjust = 0.5,
#       hjust = 1
#     ),
#     strip.background = element_rect(
#       fill = "black",
#       color = "white"
#     ),
#     strip.text = element_text(
#       size = 20
#     )
#   )
# )
# model_plot[[2]]

# lca_func2 <- cbind(
#   # albumin_bi,
#   # alp_bi,
#   alt_bi,
#   ast_bi,
#   ggt_bi,
#   hdl_bi,
#   ldl_bi,
#   # bilirubin_bi,
#   # total_chol_bi,
#   trigly_bi
# ) ~ 1

# set.seed(12345)
# lca_models2 <- cat_map(
#   1:5,
#   ~poLCA(
#     lca_func2,
#     data = indicators[[1]][, -1],
#     nclass = .x,
#     maxiter = 10000,
#     graphs = FALSE,
#     nrep = 20
#   )
# )

# lrt_func(lca_models2, 1, 2)
# lrt_func(lca_models2, 2, 3)
# lrt_func(lca_models2, 3, 4)

# tibble(
#   model = c(1:5),
#   log_lik = map_dbl(1:5, ~lca_models2[[.x]]$llik),
#   aic = map_dbl(1:5, ~lca_models2[[.x]]$aic),
#   bic = map_dbl(1:5, ~lca_models2[[.x]]$bic)
# ) |> 
#   mutate(
#     log_lik_lag = lag(log_lik),
#     aic_lag = lag(aic),
#     bic_lag = lag(bic),
#     log_lik_diff = log_lik_lag - log_lik,
#     aic_diff = aic_lag - aic,
#     bic_diff = bic_lag - bic
#   ) |> 
#   select(
#     -matches(
#       "lag"
#     )
#   )

# map_dfr(
#   1:length(lca_models2),
#   ~tibble(
#     model = .x,
#     model_entropy = poLCA.entropy(lca_models2[[.x]]),
#     max_entropy = log(prod(sapply(lca_models2[[.x]]$probs,ncol))),
#   )
# )

# cat_map(lca_models2, ~.x$P)

# model_plot2 <- cat_map(
#   lca_models2,
#   ~reshape2::melt(.x$probs, level = 2) |>
#   ggplot(
#     aes(
#       L2,
#       value,
#       fill = Var2
#     )
#   ) +
#   geom_bar(
#     stat = "identity",
#     position = "stack"
#   ) +
#   geom_hline(
#     yintercept = .5,
#     linetype = 2,
#     lwd = 1.25,
#     color = "red"
#     ) +
#   # coord_flip() +
#   facet_wrap(
#     ~Var1,
#     ncol = 1
#   ) +
#   scale_y_continuous(
#     limits = c(0, 1.05),
#     breaks = seq(0, 1, .1)
#   ) +
#   see::scale_fill_okabeito() +
#   theme_light() +
#   theme(
#     axis.text.x = element_text(
#       angle = 45,
#       vjust = 0.5,
#       hjust = 1
#     ),
#     strip.background = element_rect(
#       fill = "black",
#       color = "white"
#     ),
#     strip.text = element_text(
#       size = 20
#     )
#   )
# )
# model_plot2[[3]]

# data wrangling
model_long |> 
  mutate(
    age_cat = case_when(
      age <= 40 ~ "forty_less",
      age > 40 & age <= 65 ~ "forty_sixtyfix",
      age > 65 ~ "sixtyfixplus"
    )
  ) |> 
  ggplot(
    aes(
      age
    )
  ) +
  geom_histogram(
    color = "black",
    aes(
      fill = age_cat
    )
  )

# model_long |> 
#   ggplot(
#     aes(
#       min_sedentary_hr
#     )
#   ) +
#   geom_histogram(
#     color = "black",
#     fill = mycolor
#   )

# model_long |> 
#   mutate(
#     sedentary_cat = case_when(
#       min_sedentary_hr <= 3 ~ "three_less",
#       min_sedentary_hr >= 3 & min_sedentary_hr <= 8 ~ "three_eight",
#       min_sedentary_hr >= 8 ~ "eight_plus"
#     )
#   ) |> 
#   count(sedentary_cat)

# model_all <-
# model_all |> 
#   mutate(
#     age_cat = case_when(
#       age <= 40 ~ "forty_less",
#       age > 40 & age <= 65 ~ "forty_sixtyfix",
#       age > 65 ~ "sixtyfixplus"
#     ),
#     across(
#       c(
#         age_cat,
#         sedentary_cat
#       ),
#       ~as.factor(.x)
#     )
#   )

model_all <- 
cat_map(
  model_all,
  ~.x |> 
    mutate(
      across(
        matches("_bi$"),
        ~.x - 1
      ),
      across(
        matches("_bi$"),
        ~as.factor(.x)
      )
    )
)

model_all <-
cat_map(
  model_all,
  ~.x |> 
  mutate(
    race4 = case_when(
      race_latino %in% c(1, 2) ~ "latino",
      race_latino == 3 ~ "white",
      race_latino == 4 ~ "black_aa",
      race_latino == 5 ~ "other"
    ),
    race4 = as.factor(race4),
    race4 = relevel(race4, ref = "white")
  )
)

glimpse(model_all[[1]])

model_all <-
  cat_map(
  model_all,
  ~.x |> 
    mutate(
      fatty_liver_z = 0.953 * log(trigly_mg_dl) + 0.139 * bmi + 0.718 * log(ggt_u_l) + 0.053 * waist_circumference - 15.745,
      fatty_liver_index = exp(fatty_liver_z)/(1 + exp(fatty_liver_z))*100,
      fatty_liver_group = case_when(
        fatty_liver_index < 30 ~ "low_risk",
        fatty_liver_index >= 30 & fatty_liver_index < 60 ~ "medium_risk", 
        fatty_liver_index >= 60 ~ "high_risk"
      ),
      fatty_liver_group = as.factor(fatty_liver_group),
      fatty_liver_group = as.factor(fatty_liver_group),
      fatty_liver_group = relevel(fatty_liver_group, ref = "low_risk"),
      fatty_liver_group = fct_relevel(
        fatty_liver_group,
        "low_risk",
        "medium_risk",
        "high_risk"
      ),
      indicator_total = 
      as.numeric(albumin_bi) +
      as.numeric(alp_bi) +
      as.numeric(alt_bi) +
      as.numeric(ast_bi) +
      as.numeric(ggt_bi) +
      as.numeric(hdl_bi) +
      as.numeric(ldl_bi) +
      as.numeric(bilirubin_bi) +
      as.numeric(trigly_bi),
      fatty_indicator_total = 
      as.numeric(albumin_bi) +
      as.numeric(alp_bi) +
      as.numeric(alt_bi) +
      as.numeric(ast_bi) +
      as.numeric(hdl_bi) +
      as.numeric(ldl_bi) +
      as.numeric(bilirubin_bi)
    )
)

library(nnet)
library(mice)
library(miceadds)

set.seed(12345)
model_mids <- datlist2mids(model_all)

set.seed(12345)
liver_cond_fit <-
  with(
  model_mids,
  glm(
    told_liver_cond ~
    age +
    male +
    covered_insurance +
    race4 +
    vig_rec_pa +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    ever_45_drink_everyday +
    indicator_total,
    family = binomial("logit")
  )
)
liver_cond_pool <- pool(liver_cond_fit)
broom::tidy(liver_cond_pool) |> 
  mutate(
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
  gt::gt()


set.seed(12345)
liver_cond_mod_fit <-
  with(
  model_mids,
  glm(
    told_liver_cond ~
    age +
    male +
    covered_insurance +
    vig_rec_pa +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    ever_45_drink_everyday +
    race4*indicator_total,
    family = binomial("logit")
  )
)
liver_cond_mod_pool <- pool(liver_cond_mod_fit)
broom::tidy(liver_cond_mod_pool) |> 
  mutate(
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
  gt::gt()

set.seed(12345)
wt_fit <-
  with(
  model_mids,
  lm(
    waist_circumference ~ 
    age +
    male +
    covered_insurance +
    vig_rec_pa +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    # ever_45_drink_everyday +
    race4 +
    indicator_total,
  )
)
wt_pool <- pool(wt_fit)
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
  gt::gt()


set.seed(12345)
wt_mod_fit <-
  with(
  model_mids,
  lm(
    waist_circumference ~ 
    age +
    male +
    covered_insurance +
    vig_rec_pa +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    # ever_45_drink_everyday +
    race4*indicator_total
  )
)
wt_mod_pool <- pool(wt_mod_fit)
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
  gt::gt()


set.seed(12345)
bmi_fit <-
  with(
  model_mids,
  lm(
    bmi ~ 
    age +
    male +
    covered_insurance +
    vig_rec_pa +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    # ever_45_drink_everyday +
    race4 +
    indicator_total
  )
)
bmi_pool <- pool(bmi_fit)
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
  gt::gt()


set.seed(12345)
bmi_mod_fit <-
  with(
  model_mids,
  lm(
    bmi ~ 
    age +
    male +
    covered_insurance +
    vig_rec_pa +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    # ever_45_drink_everyday +
    race4*indicator_total
  )
)
bmi_mod_pool <- pool(bmi_mod_fit)
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
  gt::gt()


set.seed(12345)
drink_fit <-
  with(
  model_mids,
  glm(
    ever_45_drink_everyday ~
    age +
    male +
    covered_insurance +
    vig_rec_pa +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    race4 +
    indicator_total,
    family = binomial("logit")
  )
)
drink_pool <- pool(drink_fit)
broom::tidy(drink_pool) |> 
  mutate(
    exp = exp(drink_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(drink_pool$pooled$estimate),
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
  gt::gt()


set.seed(12345)
drink_mod_fit <-
  with(
  model_mids,
  glm(
    ever_45_drink_everyday ~
    age +
    male +
    covered_insurance +
    vig_rec_pa +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    race4*indicator_total,
    family = binomial("logit")
  )
)
drink_mod_pool <- pool(drink_mod_fit)
broom::tidy(drink_mod_pool) |> 
  mutate(
    exp = exp(drink_mod_pool$pooled$estimate),
    exp_conf95_low = exp(estimate - 1.96*std.error),
    exp_conf95_high = exp(estimate + 1.96*std.error),
    prob = plogis(drink_mod_pool$pooled$estimate),
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
  gt::gt()

options(scipen = 9999)

set.seed(12345)
fatty_fit <- 
with(
  model_mids,
  multinom(
    fatty_liver_group ~
    age +
    male +
    covered_insurance +
    vig_rec_pa +
    min_sedentary_hr +
    num_ready_eat_food_30day +
    race4 +
    fatty_indicator_total
  )
)
fatty_pool <- pool(fatty_fit)
fatty_summary <- broom::tidy(fatty_pool)
fatty_summary |> 
  select(
    term:p.value
  ) |> 
  mutate(
    or = exp(estimate)
  ) |> 
  gt::gt()

# ---------------------------- EXTRA ----------------
# can't use fatty liver because it uses bmi, waist circumference, ggt, and triglycerides


# library(bnlearn)

# bn_score <- bnlearn::score

# set.seed(12345)
# model_train <- cat_map(
#   model_all,
#   ~.x |>
#     slice_sample(
#       prop = .75
#     )
# )

# model_test <- map2(
#   model_all,
#   model_train,
#   ~anti_join(
#     .x,
#     .y
#   )
# )

# model_train <- cat_map(model_train, ~data.frame(.x))
# model_test <- cat_map(model_test, ~data.frame(.x))

# # structure learning
# dag_list <- cat_map(
#   model_train,
#   ~empty.graph(colnames(.x))
# )

# sub <- model_all[[1]] |> 
#   select(
#     race_latino,
#     male,
#     ed,
#     length_us,
#     matches("_bi$"),
#     told_liver_cond,
#     ever_45_drink_everyday,
#     -total_chol_bi
#   )

# dag <- empty.graph(colnames(sub))

# bl <- matrix(
#   c(
#     "albumin_bi", "alp_bi",
#     "albumin_bi", "alt_bi",
#     "albumin_bi", "ast_bi",
#     "albumin_bi", "ggt_bi",
#     "albumin_bi", "hdl_bi",
#     "albumin_bi", "ldl_bi",
#     "albumin_bi", "bilirubin_bi",
#     "albumin_bi", "trigly_bi",
#     "albumin_bi", "race_latino",
#     "albumin_bi", "ed",
#     "albumin_bi", "length_us",
#     "albumin_bi", "male",

#     "alp_bi", "albumin_bi",
#     "alp_bi", "alt_bi",
#     "alp_bi", "ast_bi",
#     "alp_bi", "ggt_bi",
#     "alp_bi", "hdl_bi",
#     "alp_bi", "ldl_bi",
#     "alp_bi", "bilirubin_bi",
#     "alp_bi", "trigly_bi",
#     "alp_bi", "race_latino",
#     "alp_bi", "ed",
#     "alp_bi", "length_us",
#     "alp_bi", "male",

#     "alt_bi", "albumin_bi",
#     "alt_bi", "alp_bi",
#     "alt_bi", "ast_bi",
#     "alt_bi", "ggt_bi",
#     "alt_bi", "hdl_bi",
#     "alt_bi", "ldl_bi",
#     "alt_bi", "bilirubin_bi",
#     "alt_bi", "trigly_bi",
#     "alt_bi", "race_latino",
#     "alt_bi", "ed",
#     "alt_bi", "length_us",
#     "alt_bi", "male",

#     "ast_bi", "albumin_bi",
#     "ast_bi", "alp_bi",
#     "ast_bi", "alt_bi",
#     "ast_bi", "ggt_bi",
#     "ast_bi", "hdl_bi",
#     "ast_bi", "ldl_bi",
#     "ast_bi", "bilirubin_bi",
#     "ast_bi", "trigly_bi",
#     "ast_bi", "race_latino",
#     "ast_bi", "ed",
#     "ast_bi", "length_us",
#     "ast_bi", "male",

#     "ggt_bi", "albumin_bi",
#     "ggt_bi", "alp_bi",
#     "ggt_bi", "alt_bi",
#     "ggt_bi", "ast_bi",
#     "ggt_bi", "hdl_bi",
#     "ggt_bi", "ldl_bi",
#     "ggt_bi", "bilirubin_bi",
#     "ggt_bi", "trigly_bi",
#     "ggt_bi", "race_latino",
#     "ggt_bi", "ed",
#     "ggt_bi", "length_us",
#     "ggt_bi", "male",

#     "hdl_bi", "albumin_bi",
#     "hdl_bi", "alp_bi",
#     "hdl_bi", "alt_bi",
#     "hdl_bi", "ast_bi",
#     "hdl_bi", "ggt_bi",
#     "hdl_bi", "ldl_bi",
#     "hdl_bi", "bilirubin_bi",
#     "hdl_bi", "trigly_bi",
#     "hdl_bi", "race_latino",
#     "hdl_bi", "ed",
#     "hdl_bi", "length_us",
#     "hdl_bi", "male",

#     "ldl_bi", "albumin_bi",
#     "ldl_bi", "alp_bi",
#     "ldl_bi", "alt_bi",
#     "ldl_bi", "ast_bi",
#     "ldl_bi", "ggt_bi",
#     "ldl_bi", "hdl_bi",
#     "ldl_bi", "bilirubin_bi",
#     "ldl_bi", "trigly_bi",
#     "ldl_bi", "race_latino",
#     "ldl_bi", "ed",
#     "ldl_bi", "length_us",
#     "ldl_bi", "male",

#     "bilirubin_bi", "albumin_bi",
#     "bilirubin_bi", "alp_bi",
#     "bilirubin_bi", "alt_bi",
#     "bilirubin_bi", "ast_bi",
#     "bilirubin_bi", "ggt_bi",
#     "bilirubin_bi", "ldl_bi",
#     "bilirubin_bi", "hdl_bi",
#     "bilirubin_bi", "trigly_bi",
#     "bilirubin_bi", "race_latino",
#     "bilirubin_bi", "ed",
#     "bilirubin_bi", "length_us",
#     "bilirubin_bi", "male",

#     "trigly_bi", "albumin_bi",
#     "trigly_bi", "alp_bi",
#     "trigly_bi", "alt_bi",
#     "trigly_bi", "ast_bi",
#     "trigly_bi", "ggt_bi",
#     "trigly_bi", "ldl_bi",
#     "trigly_bi", "hdl_bi",
#     "trigly_bi", "bilirubin_bi",
#     "trigly_bi", "race_latino",
#     "trigly_bi", "ed",
#     "trigly_bi", "length_us",
#     "trigly_bi", "male",

#     "told_liver_cond", "race_latino",
#     "told_liver_cond", "ed",
#     "told_liver_cond", "length_us",
#     "told_liver_cond", "male",
#     "told_liver_cond", "albumin_bi",
#     "told_liver_cond", "alp_bi",
#     "told_liver_cond", "alt_bi",
#     "told_liver_cond", "ast_bi",
#     "told_liver_cond", "ggt_bi",
#     "told_liver_cond", "hdl_bi",
#     "told_liver_cond", "ldl_bi",
#     "told_liver_cond", "bilirubin_bi",
#     "told_liver_cond", "trigly_bi",

#     "ever_45_drink_everyday", "race_latino",
#     "ever_45_drink_everyday", "ed",
#     "ever_45_drink_everyday", "length_us",
#     "ever_45_drink_everyday", "male",
#     "ever_45_drink_everyday", "albumin_bi",
#     "ever_45_drink_everyday", "alp_bi",
#     "ever_45_drink_everyday", "alt_bi",
#     "ever_45_drink_everyday", "ast_bi",
#     "ever_45_drink_everyday", "ggt_bi",
#     "ever_45_drink_everyday", "hdl_bi",
#     "ever_45_drink_everyday", "ldl_bi",
#     "ever_45_drink_everyday", "bilirubin_bi",
#     "ever_45_drink_everyday", "trigly_bi"
#   ),
#   ncol = 2,
#   byrow = TRUE,
#   dimnames = list(
#     NULL,
#     c("from", "to")
#   )
# )

# wl <- matrix(
#   c(
#     "albumin_bi", "ever_45_drink_everyday",
#     "alp_bi", "ever_45_drink_everyday",
#     "alt_bi", "ever_45_drink_everyday",
#     "ast_bi", "ever_45_drink_everyday",
#     "ggt_bi", "ever_45_drink_everyday",
#     "hdl_bi", "ever_45_drink_everyday",
#     "ldl_bi", "ever_45_drink_everyday",
#     "bilirubin_bi", "ever_45_drink_everyday",
#     "trigly_bi", "ever_45_drink_everyday",

#     "albumin_bi", "told_liver_cond",
#     "alp_bi", "told_liver_cond",
#     "alt_bi", "told_liver_cond",
#     "ast_bi", "told_liver_cond",
#     "ggt_bi", "told_liver_cond",
#     "hdl_bi", "told_liver_cond",
#     "ldl_bi", "told_liver_cond",
#     "bilirubin_bi", "told_liver_cond",
#     "trigly_bi", "told_liver_cond"
#   ),
#   ncol = 2,
#   byrow = TRUE,
#   dimnames = list(
#     NULL,
#     c("from", "to")
#   )
# )

# set.seed(12345)
# hc_bn <- hc(
#   sub,
#   blacklist = bl,
#   whitelist = wl
#   )

# bn_score(hc_bn, sub, type = "loglik")

# arcs(dag) <- hc_bn$arcs

# set.seed(12345)
# hc_fit <- bn.fit(dag, data = sub, method = "bayes", iss = 10)
# hc_fit

# hc_fit$ever_45_drink_everyday |> str()
# hc_fit$told_liver_cond |> str()

# hc_fit$told_liver_cond$prob
