library(tidyverse)
library(poLCA)

cat_map <- purrr::map
select <- dplyr::select

options(scipen = 10)

react_table <- function(data){
  reactable::reactable(
    {{data}},
    filterable = TRUE,
    sortable = TRUE,
    searchable = TRUE
  )
}

latino <- read_rds(here::here("RDS Files/latino_20plus.rds"))
lca_diff <- read_rds(here::here("RDS Files/lca_classes1to5_allimputations.rds"))
fit_indices <- read_rds(here::here("RDS Files/fit_indices_classes1to5"))
lca_grouped <- read_rds(here::here("RDS Files/lca_class_combined.rds"))
lca_cov <- read_rds(here::here("RDS Files/lca_model_7indicators_cov.rds"))
lca_grouped_cov <- read_rds(here::here("RDS Files/lca_class_combined_cov.rds"))
model_all <- read_rds(here::here("RDS Files/data_w_lca_class_membership.rds"))

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


# lca3class <- 
#   cat_map(
#     1:length(lca_diff),
#     ~lca_diff[[.x]][[3]]
#   )

# latino |> count()

# psych::describe(latino)[c("mean", "sd")]

# latino |> 
#   count(sex) |> 
#   mutate(
#     perc = n/sum(n)*100
#   )

# cat_map(
#   latino |>
#   select(
#     sex,
#     race_latino,
#     birth_country,
#     citizen,
#     length_us,
#     ed,
#     annual_house_income
#   ),
#   ~count(data.frame(x=.x), x) |> 
#     drop_na() |> 
#       mutate(
#         perc = round(n/sum(n)*100, 2)
#       )
# )

# psych::describe(
#   latino |>
#     select(
#       albumin_g_dl,
#       alp_iu_l,
#       ast_u_l,
#       alt_u_l,
#       ggt_u_l,
#       total_bilirubin_mg_dl,
#       hdl_chol_mg_dl,
#       trigly_mg_dl,
#       ldl_chol_mg_dl,            
#       total_chol_mg_dl
#     ),
#     na.rm = TRUE
# )[c("mean", "sd")]

# inspectdf::inspect_na(latino)

# latino |> glimpse()


# # fit indices
# fit_indices |> 
#   react_table()

# lca_grouped |> 
#   pivot_longer(
#     -c(
#       lat_class,
#       initial_class,
#       type
#     )
#   ) |> 
#   mutate(
#     type = as.factor(type),
#     type = relevel(type, ref = "normal"),
#     name = str_to_upper(name),
#     name = case_when(
#       name == "TOTAL_CHOL" ~ "Total\nCholesterol",
#       name == "TRIGLY" ~ "Triglycerides",
#       TRUE ~ name
#     ),
#     lat_class = case_when(
#       lat_class == "class1" ~ "Healthy",
#       lat_class == "class2" ~ "Unhealthy Cholesterol",
#       lat_class == "class3" ~ "At-Risk"
#     ),
#     lat_class = fct_relevel(
#       as.factor(lat_class),
#       "Healthy",
#       "Unhealthy Cholesterol",
#       "At-Risk"
#     )
#   ) |> 
#   filter(
#     type == "abnormal"
#   ) |> 
#   ggplot(
#     aes(
#       name,
#       value
#     )
#   ) +
#   geom_col(
#     fill = "gray70",
#     color = "black"
#   ) +
#   geom_hline(
#     yintercept = 0.5,
#     linetype = 2,
#     lwd = 1.25,
#     color = "black" 
#   ) +
#   # coord_flip() +
#   facet_wrap(
#     vars(lat_class),
#     ncol = 1
#   ) +
#   scale_y_continuous(
#     limits = c(0, 1.05),
#     breaks = seq(0, 1, .1)
#   ) +
#   # scale_fill_manual(
#   #   values = c("gray30", "gray70"),
#   #   labels = c("normal", "abnormal")
#   # ) +
#   labs(
#     x = "",
#     y = "Probability"
#   ) +
#   theme_light() +
#   theme(
#     legend.position = "none",
#     axis.text = element_text(
#       size = 14,
#       color = "black"
#     ),
#     axis.title = element_text(
#       size = 20,
#       color = "black"
#     ),
#     strip.text = element_text(
#       size = 20,
#       color = "black"
#     )
#   )

#   lca_grouped_cov |> 
#     pivot_longer(
#       -c(
#         lat_class_cov,
#         initial_class_cov,
#         type
#       )
#     ) |> 
#     mutate(
#       type = as.factor(type),
#       type = relevel(type, ref = "normal"),
#       name = str_to_upper(name),
#       name = case_when(
#         name == "TOTAL_CHOL" ~ "Total\nCholesterol",
#         name == "TRIGLY" ~ "Triglycerides",
#         TRUE ~ name
#       ),
#       lat_class_cov = case_when(
#         lat_class_cov == "class1" ~ "Healthy",
#         lat_class_cov == "class2" ~ "Unhealthy Cholesterol",
#         lat_class_cov == "class3" ~ "At-Risk"
#       ),
#       lat_class_cov = fct_relevel(
#         as.factor(lat_class_cov),
#         "Healthy",
#         "Unhealthy Cholesterol",
#         "At-Risk"
#       )
#     ) |> 
#     filter(
#       type == "abnormal"
#     ) |> 
#     ggplot(
#       aes(
#         name,
#         value
#       )
#     ) +
#     geom_col(
#       fill = "gray70",
#       color = "black"
#     ) +
#     geom_hline(
#       yintercept = 0.5,
#       linetype = 2,
#       lwd = 1.25,
#       color = "black" 
#     ) +
#     # coord_flip() +
#     facet_wrap(
#       vars(lat_class_cov),
#       ncol = 1
#     ) +
#     scale_y_continuous(
#       limits = c(0, 1.05),
#       breaks = seq(0, 1, .1)
#     ) +
#     # scale_fill_manual(
#     #   values = c("gray30", "gray70"),
#     #   labels = c("normal", "abnormal")
#     # ) +
#     labs(
#       x = "",
#       y = "Probability"
#     ) +
#     theme_light() +
#     theme(
#       legend.position = "none",
#       axis.text = element_text(
#         size = 14,
#         color = "black"
#       ),
#       axis.title = element_text(
#         size = 20,
#         color = "black"
#       ),
#       strip.text = element_text(
#         size = 20,
#         color = "black"
#       )
#     )


# map_dfr(
#   model_all,
#   ~.x |> 
#     count(lat_class) |> 
#     mutate(
#       p = n/sum(n)*100,
#       p = round(p, 2)
#     )
# ) |>
#   mutate(
#     imp = rep(1:60, each = 3)
#   ) |> 
#   group_by(
#     lat_class
#   ) |> 
#   summarize(
#     avg_p = mean(p, na.rm = TRUE)
#   )

# map_dfr(
#   model_all,
#   ~.x |> 
#     count(lat_class_cov) |> 
#     mutate(
#       p = n/sum(n)*100,
#       p = round(p, 2)
#     )
# ) |>
#   mutate(
#     imp = rep(1:60, each = 3)
#   ) |> 
#   group_by(
#     lat_class_cov
#   ) |> 
#   summarize(
#     avg_p = mean(p, na.rm = TRUE)
#   )


# map_dfr(
#   model_all,
#   ~.x  |> 
#     select(
#       matches("_bi$")
#     )
# ) |> 
#   mutate(
#     imp = rep(1:60, each = 938)
#   ) |> 
#   pivot_longer(
#     -imp
#   ) |> 
#   group_by(
#     name,
#     imp
#   ) |> 
#   count(value) |> 
#   mutate(
#     percent = n/sum(n)*100,
#     percent = round(percent, 3)
#   )

# indicator_grouped <-
#   map_dfr(
#   model_all,
#   ~.x  |> 
#     select(
#       matches("_bi$")
#     )
# ) |> 
#   mutate(
#     imp = rep(1:60, each = 938)
#   ) |> 
#   pivot_longer(
#     -imp
#   ) |>
#   group_by(
#     imp,
#     name
#   ) |> 
#   count(value) |> 
#   mutate(
#     percent = round(n/sum(n)*100, 2)
#   ) |> 
#   ungroup()

# psych::describeBy(
#   indicator_grouped |> filter(str_detect(name, "albumin")) |> select(n),
#   indicator_grouped |> filter(str_detect(name, "albumin")) |> select(value)
# )

# map_dfr(
#   model_all,
#   ~.x  |> 
#     select(
#       matches("_bi$")
#     )
# ) |> 
#   mutate(
#     imp = rep(1:60, each = 938)
#   ) |> 
#   pivot_longer(
#     -imp
#   ) |>
#   group_by(
#     imp,
#     name
#   ) |> 
#   count(value) |> 
#   mutate(
#     percent = round(n/sum(n)*100, 2)
#   ) |> 
#   ungroup() |> 
#   group_by(
#     name,
#     value
#   ) |> 
#   summarize(
#     avg_n = mean(n, na.rm = TRUE),
#     avg_n = round(avg_n, 0),
#     avg_percent = mean(percent, na.rm = TRUE)
#   )




# p1 <- model_all[[1]] |> 
#   mutate(
#     fatty_liver_group = as.factor(fatty_liver_group),
#     fatty_liver_group = fct_relevel(
#       fatty_liver_group,
#       "low_risk",
#       "medium_risk",
#       "high_risk"
#     )
#   ) |> 
#   ggplot(
#     aes(
#       fatty_liver_index
#     )
#   ) +
#   geom_histogram(
#     aes(
#       fill = fatty_liver_group
#     ),
#     color = "black"
#   ) +
#   facet_wrap(
#     vars(fatty_liver_group),
#     ncol = 1
#   ) +
#   labs(
#     title = "Fatty Liver Index By Fatty Liver Group"
#   ) +
#   theme_light()

# p2 <- model_all[[1]] |> 
#   ggplot(
#     aes(
#       fatty_liver_index
#     )
#   ) +
#   geom_histogram(
#     aes(
#       fill = as.factor(lat_class)
#     ),
#     color = "black"
#   ) +
#   facet_wrap(
#     vars(lat_class),
#     ncol = 1
#   ) +
#     labs(
#       title = "Fatty Liver Index By Latent Class Analysis Group"
#     ) +
#   theme_light()

# patchwork::wrap_plots(
#   p1 + p2
# )

# lat_class_fli_compare <- cat_map(
#   model_all,
#   ~chisq.test(
#     .x$fatty_liver_group,
#     .x$lat_class
#   )
# )

# lat_class_fli_compare[[1]] |> str()

# # map_dbl(
# #   lat_class_fli_compare,
# #   ~.x$statistic
# # ) |> 
# #   as_tibble() |> 
# #   summarize(
# #     value = mean(value, na.rm = TRUE)
# #   )

# lat_class_fli_effect <- cat_map(
#   model_all,
#   ~effectsize::cramers_v(
#     .x$fatty_liver_group,
#     .x$lat_class
#   )
# )

# map_dbl(
#   lat_class_fli_effect,
#   ~.x$Cramers_v_adjusted
# ) |> 
#   as_tibble() |> 
#   summarize(
#     value = mean(value, na.rm = TRUE)
#   )



# lca7_covariates <- data.frame(
#   est_2vs1 = map_dfr(lca_cov, ~.x$coeff[, 1]),
#   se_2vs1 = map_dfr(lca_cov, ~.x$coeff.se[, 1]),
#   est_3vs1 = map_dfr(lca_cov, ~.x$coeff[, 2]),
#   se_3vs1 = map_dfr(lca_cov, ~.x$coeff.se[, 2])
# ) |> 
#   janitor::clean_names() |> 
#   mutate(
#     z_2vs1_intercept = est_2vs1_intercept/se_2vs1_intercept,
#     z_2vs1_sex2 = est_2vs1_sex2/se_2vs1_sex2,
#     z_2vs1_age = est_2vs1_age/se_2vs1_age,
#     z_2vs1_covered_insurance2 = est_2vs1_covered_insurance2/se_2vs1_covered_insurance2,
#     z_3vs1_intercept = est_3vs1_intercept/se_3vs1_intercept,
#     z_3vs1_sex2 = est_3vs1_sex2/se_3vs1_sex2,
#     z_3vs1_age = est_3vs1_age/se_3vs1_age,
#     z_3vs1_covered_insurance2 = est_3vs1_covered_insurance2/se_3vs1_covered_insurance2,
#     p_2vs1_intercept = 2 * (1 - pnorm(abs(z_2vs1_intercept))),
#     p_2vs1_sex2 = 2 * (1 - pnorm(abs(z_2vs1_sex2))),
#     p_2vs1_age = 2 * (1 - pnorm(abs(z_2vs1_age))),
#     p_2vs1_covered_insurance2 = 2 * (1 - pnorm(abs(z_2vs1_covered_insurance2))),
#     p_3vs1_intercept = 2 * (1 - pnorm(abs(z_3vs1_intercept))),
#     p_3vs1_sex2 = 2 * (1 - pnorm(abs(z_3vs1_sex2))),
#     p_3vs1_age = 2 * (1 - pnorm(abs(z_3vs1_age))),
#     p_3vs1_covered_insurance2 = 2 * (1 - pnorm(abs(z_3vs1_covered_insurance2)))
#   ) |> 
#   as_tibble() |> 
#   mutate(
#     imp = seq(1, 60, 1)
#   ) |> 
#   relocate(
#     imp,
#     .before = est_2vs1_intercept
#   )
# 
# lca7_covariates 

# saveRDS(lca7_covariates, here::here("projects/nhanes/lca_covariate_associations.rds"))


# model_all[[1]] |> glimpse()

# model_all[[1]] |> count(race_latino)
# model_all[[1]] |> count(citizen)

# cat_map(
#   model_all,
#   ~chisq.test(
#     .x$race_latino,
#     .x$lat_class
#   )
# )

# chisq_compare <- cat_map(
#   model_all,
#   ~effectsize::cramers_v(
#     .x$race_latino,
#     .x$lat_class
#   )
# )

# map_dbl(
#   chisq_compare,
#   ~.x$Cramers_v_adjusted
# ) |> 
#   as_tibble() |> 
#   summarize(
#     value = mean(value, na.rm = TRUE)
#   )
# # medium effect size between race and citizenship

# chisq_compare <- cat_map(
#   model_all,
#   ~effectsize::cramers_v(
#     .x$race_latino,
#     .x$lat_class
#   )
# )

# map_dbl(
#   chisq_compare,
#   ~.x$Cramers_v_adjusted
# ) |> 
#   as_tibble() |> 
#   summarize(
#     value = mean(value, na.rm = TRUE)
#   )

# Chi-Square & Cramer V for All Indicator Associations
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

# albumin_bi
# alp_bi
# alt_bi
# ast_bi
# ggt_bi
# hdl_bi
# ldl_bi
# bilirubin_bi
# total_chol_bi
# trigly_bi

model_all[[1]] |> select(matches("bi$")) |> names()

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


library(mice)
library(miceadds)
library(nnet)

set.seed(12345)
model_mids <- datlist2mids(model_all)

set.seed(12345)
control_fit <- with(
  model_mids,
  multinom(
    lat_class ~ age + sex + covered_insurance
  )
)

control_pool <- pool(control_fit)
broom::tidy(control_pool) |> 
  mutate(
    # conf95_low = estimate - 1.96*std.error,
    # conf95_high = estimate + 1.96*std.error,
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



# Liver Damage
# liver_cond_fit <- with(
#   model_mids,
#   glm(
#     told_liver_cond ~ lat_class,
#       #age + sex + covered_insurance +
#       #min_sedentary + vig_rec_pa +
#       #waist_circumference +
#       #num_ready_eat_food_30day +
#       #ever_45_drink_everyday +
#       #hep_a_anti + hep_b_core_anti + hep_c_rna,
#       family = binomial("logit")
#   )
# )

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
    lat_class
    ,
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
# as you get older, more likely to be told you have a liver condition
# class 3 is more likely to be told they have a liver condition

# waist circumference
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


# latent class 3 significantly higher waist circumference
# more time sedentary, greater waist circumference (not by much)
# more pa, lower waist circumference
# male greater waist circumference
# age goes up, waist circumference goes up

#BMI Outcome
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

# being male, lower BMI
# having insurance, lower BMI
# engaging pa, lower BMI
# more sedentary, higher BMI
# class 3 greater BMI than class 1

# excessive drinking
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
# greater age, more likely to excessive drink
# being male, more likely to excessive drink

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
# age more likely to be told liver condition
# no differences between fatty liver index groups

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
# no differences between fatty liver groups
# greater age more likely to excessive drink
# male more likely to excessive drink


# diabetes
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
# no significant findings

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
# fatty liver high risk group more likely to be at risk for diabetes than low risk group

