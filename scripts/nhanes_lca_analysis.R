library(tidyverse)
library(poLCA)

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

indicators[[1]] |> colnames()

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


# 6 indicators
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

reshape2::melt(lca_diff[[1]][[3]]$probs, level = 2) |>
  # mutate(
  #   Var2 = case_when(
  #     Var2 == "Pr(1)" ~ "Normal",
  #     Var2 == "Pr(2)" ~ "Abnormal"
  #   ),
  #   Var2 = as.factor(Var2),
  #   Var2 = fct_relevel(
  #     Var2,
  #     "Normal",
  #     "Abnormal"
  #   ),
  #   L2 = as.factor(L2),
  #   L2 = fct_relevel(
  #     L2,
  #     # "albumin_bi",
  #     # "alp_bi",
  #     "alt_bi",
  #     "ast_bi",
  #     "ggt_bi",
  #     "hdl_bi",
  #     "ldl_bi",
  #     # "bilirubin_bi",
  #     "total_chol_bi",
  #     "trigly_bi"
  #   )
  # ) |>
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

# The smallest n class (group 2) is what I'm determining is class 3 (most at risk)
# largest n class (group 1) is class 1 (most healthy)
# middle n class (group 3) is class 2 (somewhat at risk)

saveRDS(lca_diff, here::here("RDS Files/lca_classes1to5_allimputations_nototal.rds"))

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

saveRDS(fit_indices, here::here("RDS Files/fit_indices_classes1to5_nototal.RDS"))

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

lca2class[[1]]$probs$alt_bi[,1] |> as_tibble()
lca2class[[1]]$probs$ast_bi
lca2class[[1]]$probs$ggt_bi

# 2 classes is best fitting

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

lca_group1
lca_group2


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

saveRDS(lca_grouped, here::here("RDS Files/lca_class_combined_nototal.rds"))

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

#3 = 1, 1 = 2, 2 = 3; old = new
#this is just dobule checking that the posterior match the indicator values
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

# trying new latent classes

lca_grouped

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


saveRDS(model_all, here::here("RDS Files/data_w_lca_class_membership_nototal.RDS"))


# # run lca with covariates
# indicator_cov <- 
#   cat_map(
#   model_all,
#   ~.x |> 
#     select(
#       seqn,
#       age,
#       sex,
#       covered_insurance,
#       matches("_bi$")
#     )
# )

# lca_func_cov <- cbind(
#   # albumin_bi,
#   # alp_bi,
#   alt_bi,
#   ast_bi,
#   ggt_bi,
#   hdl_bi,
#   ldl_bi,
#   # bilirubin_bi,
#   total_chol_bi,
#   trigly_bi
# ) ~ sex + age + covered_insurance

# set.seed(12345)
# lca7_cov <- cat_map(
#   indicator_cov,
#   ~poLCA(
#     lca_func_cov,
#     .x[, -1],
#     nclass = 3,
#     nrep = 10
#   )
# )

# lca7_cov[[1]]

# saveRDS(lca7_cov, here::here("projects/nhanes/lca_model_7indicators_cov.rds"))

# # lca7_covariates <- data.frame(
# #   est_2vs1 = map_dfr(lca7_cov, ~.x$coeff[, 1]),
# #   se_2vs1 = map_dfr(lca7_cov, ~.x$coeff.se[, 1]),
# #   est_3vs1 = map_dfr(lca7_cov, ~.x$coeff[, 2]),
# #   se_3vs1 = map_dfr(lca7_cov, ~.x$coeff.se[, 2])
# # ) |> 
# #   janitor::clean_names() |> 
# #   mutate(
# #     z_2vs1_intercept = est_2vs1_intercept/se_2vs1_intercept,
# #     z_2vs1_sex2 = est_2vs1_sex2/se_2vs1_sex2,
# #     z_2vs1_age = est_2vs1_age/se_2vs1_age,
# #     z_2vs1_covered_insurance2 = est_2vs1_covered_insurance2/se_2vs1_covered_insurance2,
# #     z_3vs1_intercept = est_3vs1_intercept/se_3vs1_intercept,
# #     z_3vs1_sex2 = est_3vs1_sex2/se_3vs1_sex2,
# #     z_3vs1_age = est_3vs1_age/se_3vs1_age,
# #     z_3vs1_covered_insurance2 = est_3vs1_covered_insurance2/se_3vs1_covered_insurance2,
# #     p_2vs1_intercept = 2 * (1 - pnorm(abs(z_2vs1_intercept))),
# #     p_2vs1_sex2 = 2 * (1 - pnorm(abs(z_2vs1_sex2))),
# #     p_2vs1_age = 2 * (1 - pnorm(abs(z_2vs1_age))),
# #     p_2vs1_covered_insurance2 = 2 * (1 - pnorm(abs(z_2vs1_covered_insurance2))),
# #     p_3vs1_intercept = 2 * (1 - pnorm(abs(z_3vs1_intercept))),
# #     p_3vs1_sex2 = 2 * (1 - pnorm(abs(z_3vs1_sex2))),
# #     p_3vs1_age = 2 * (1 - pnorm(abs(z_3vs1_age))),
# #     p_3vs1_covered_insurance2 = 2 * (1 - pnorm(abs(z_3vs1_covered_insurance2)))
# #   ) |> 
# #   as_tibble() |> 
# #   mutate(
# #     imp = seq(1, 60, 1)
# #   ) |> 
# #   relocate(
# #     imp,
# #     .before = est_2vs1_intercept
# #   )
# # 
# # lca7_covariates 

# # saveRDS(lca7_covariates, here::here("projects/nhanes/lca_coariate_associations.rds"))

# # latent class membership with covariates included
# lca_cov[[1]]$posterior |> 
#   head()
# lca_cov[[1]]$predclass |> 
#   as_tibble() |> 
#   count(value)


# reshape2::melt(lca_cov[[1]]$probs, level = 2) |>
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

# posterior_counts_cov <- 
#   cat_map(
#   1:length(lca_cov),
#   ~lca_cov[[.x]]$posterior |> 
#     as_tibble() |> 
#     mutate(
#       seqn = indicators[[.x]][,1]
#     ) |> 
#       relocate(
#         seqn,
#         .before = V1
#       ) |>
#       pivot_longer(
#         -c(
#           seqn
#         )
#       ) |> 
#       group_by(
#         seqn
#       ) |> 
#       filter(
#         value == max(value)
#       ) |> 
#       ungroup(seqn) |> 
#       count(name)
# ) 

# posterior_counts_cov[[1]]

# class_membership_cov <- 
#   cat_map(
#   1:length(lca_cov),
#   ~lca_cov[[.x]]$posterior |> 
#     as_tibble() |> 
#     mutate(
#       seqn = indicators[[.x]][,1]
#     ) |> 
#       relocate(
#         seqn,
#         .before = V1
#       ) |>
#       pivot_longer(
#         -c(
#           seqn
#         )
#       ) |> 
#       group_by(
#         seqn
#       ) |> 
#       filter(
#         value == max(value)
#       ) |> 
#       ungroup() |> 
#       left_join(
#         posterior_counts[[.x]]
#       ) |> 
#       mutate(
#         latent_class = case_when(
#           n < 100 ~ "class3",
#           n > 500 ~ "class1",
#           n >= 100 & n <= 500 ~ "class2"
#         )
#       )
# )

# lca_cov[[1]]$probs$total_chol_bi

# total_chol_cov <- map_dfr(1:60, ~lca_cov[[.x]]$probs$total_chol_bi[,1] |> as_tibble()) |> 
#   mutate(
#     imp = rep(1:60, each = 3),
#     initial_class = rep(1:3, 60)
#   ) |> 
#   rename(
#     total_chol_norm_cov = value
#   ) |> 
#   group_by(
#     imp
#   ) |> 
#   mutate(
#     total_chol_ab_cov = 1 - total_chol_norm_cov,
#     lat_class_cov = case_when(
#       total_chol_norm_cov > .8 ~ "class1",
#       total_chol_norm_cov <= .8 & total_chol_norm_cov >= .2 ~ "class3",
#       total_chol_norm_cov < .2 ~ "class2"
#     )
#   ) |>
#   ungroup() |> 
#   select(
#     imp,
#     initial_class,
#     lat_class_cov,
#     total_chol_norm_cov,
#     total_chol_ab_cov
#   )
# total_chol_cov

# lca_cov[[1]]$probs$alt_bi

# alt_cov <- map_dfr(1:60, ~lca_cov[[.x]]$probs$alt_bi[,1] |> as_tibble()) |> 
#   mutate(
#     imp = rep(1:60, each = 3),
#     initial_class = rep(1:3, 60)
#   ) |> 
#   rename(
#     alt_norm_cov = value
#   ) |> 
#   group_by(
#     imp
#   ) |> 
#   mutate(
#     alt_ab_cov = 1 - alt_norm_cov
#   ) |> 
#   ungroup()

# lca_cov[[1]]$probs$ast_bi

# ast_cov <- 
#   map_dfr(1:60, ~lca_cov[[.x]]$probs$ast_bi[,1] |> as_tibble()) |> 
#   mutate(
#     imp = rep(1:60, each = 3),
#     initial_class = rep(1:3, 60)
#   ) |> 
#   rename(
#     ast_norm_cov = value
#   ) |> 
#   group_by(
#     imp
#   ) |> 
#   mutate(
#     ast_ab_cov = 1 - ast_norm_cov
#   ) |> 
#   ungroup()

# lca_cov[[1]]$probs$ggt_bi

# ggt_cov <- 
#   map_dfr(1:60, ~lca_cov[[.x]]$probs$ggt_bi[,1] |> as_tibble()) |> 
#   mutate(
#     imp = rep(1:60, each = 3),
#     initial_class = rep(1:3, 60)
#   ) |> 
#   rename(
#     ggt_norm_cov = value
#   ) |> 
#   group_by(
#     imp
#   ) |> 
#   mutate(
#     ggt_ab_cov = 1 - ggt_norm_cov
#   ) |> 
#   ungroup()

# lca_cov[[1]]$probs$hdl_bi

# hdl_cov <- 
#   map_dfr(1:60, ~lca_cov[[.x]]$probs$hdl_bi[,1] |> as_tibble()) |> 
#   mutate(
#     imp = rep(1:60, each = 3),
#     initial_class = rep(1:3, 60)
#   ) |> 
#   rename(
#     hdl_norm_cov = value
#   ) |> 
#   group_by(
#     imp
#   ) |> 
#   mutate(
#     hdl_ab_cov = 1 - hdl_norm_cov
#   ) |> 
#   ungroup()

# lca_cov[[1]]$probs$ldl_bi

# ldl_cov <- 
#   map_dfr(1:60, ~lca_cov[[.x]]$probs$ldl_bi[,1] |> as_tibble()) |> 
#   mutate(
#     imp = rep(1:60, each = 3),
#     initial_class = rep(1:3, 60)
#   ) |> 
#   rename(
#     ldl_norm_cov = value
#   ) |> 
#   group_by(
#     imp
#   ) |> 
#   mutate(
#     ldl_ab_cov = 1 - ldl_norm_cov
#   ) |> 
#   ungroup()

# lca_cov[[1]]$probs$trigly_bi

# trigly_cov <- 
#   map_dfr(1:60, ~lca_cov[[.x]]$probs$trigly_bi[,1] |> as_tibble()) |> 
#   mutate(
#     imp = rep(1:60, each = 3),
#     initial_class = rep(1:3, 60)
#   ) |> 
#   rename(
#     trigly_norm_cov = value
#   ) |> 
#   group_by(
#     imp
#   ) |> 
#   mutate(
#     trigly_ab_cov = 1 - trigly_norm_cov
#   ) |> 
#   ungroup()

# lca_group_cov <-
#   total_chol_cov |> 
#   left_join(alt_cov) |> 
#   left_join(ast_cov) |> 
#   left_join(ggt_cov) |> 
#   left_join(hdl_cov) |> 
#   left_join(ldl_cov) |> 
#   left_join(trigly_cov)

# lca_group_cov

# lca_group1_cov <-
#   lca_group_cov |> 
#   group_by(
#     lat_class_cov
#   ) |> 
#   summarize(
#     across(
#       -c(
#         imp
#       ),
#       ~mean(.x, na.rm = TRUE)
#     )
#   ) |> 
#   ungroup() |> 
#   select(
#     lat_class_cov,
#     matches("_ab_cov$")
#   ) |> 
#   mutate(
#     initial_class_cov = case_when(
#       lat_class_cov == "class2" ~ "1",
#       lat_class_cov == "class3" ~ "2",
#       lat_class_cov == "class1" ~ "3"
#     )
#   ) |> 
#   relocate(
#     initial_class_cov,
#     .after = lat_class_cov
#   )

# lca_group2_cov <-
#   lca_group_cov |> 
#   group_by(
#     lat_class_cov
#   ) |> 
#   summarize(
#     across(
#       -c(
#         imp
#       ),
#       ~mean(.x, na.rm = TRUE)
#     )
#   ) |> 
#   ungroup() |> 
#   select(
#     lat_class_cov,
#     matches("_norm_cov$")
#   ) |> 
#   mutate(
#     initial_class_cov = case_when(
#       lat_class_cov == "class2" ~ "1",
#       lat_class_cov == "class3" ~ "2",
#       lat_class_cov == "class1" ~ "3"
#     )
#   ) |> 
#   relocate(
#     initial_class_cov,
#     .after = lat_class_cov
#   )

# lca_grouped_cov <- rbind(
#   lca_group1_cov |>
#   rename_with(
#     ~str_remove(
#       .x,
#       "_ab_cov$"
#     )
#   ),
#   lca_group2_cov |>
#   rename_with(
#     ~str_remove(
#       .x,
#       "_norm_cov$"
#     )
#   )
# ) |> 
#   mutate(
#     type = c(rep("abnormal", 3), rep("normal", 3))
#   )

# saveRDS(lca_grouped_cov, here::here("projects/nhanes/lca_class_combined_cov.rds"))

# lca_grouped_cov |> 
#   pivot_longer(
#     -c(
#       lat_class_cov,
#       initial_class_cov,
#       type
#     )
#   ) |> 
#   mutate(
#     type = as.factor(type),
#     type = relevel(type, ref = "normal")
#   ) |> 
#   ggplot(
#     aes(
#       name,
#       value
#     )
#   ) +
#   geom_col(
#     aes(
#       fill = type
#     ),
#     color = "black"
#   ) +
#   geom_hline(
#     yintercept = 0.6,
#     linetype = 2,
#     lwd = 1.25,
#     color = "red" 
#   ) +
#   coord_flip() +
#   facet_wrap(
#     vars(lat_class_cov),
#     ncol = 1
#   ) +
#   scale_y_continuous(
#     limits = c(0, 1.05),
#     breaks = seq(0, 1, .1)
#   ) +
#   see::scale_fill_okabeito() +
#   theme_light() +
#   theme(
#     legend.position = "top",
#     axis.text = element_text(
#       size = 14
#     )
#   )


#   model_all <- 
#     cat_map(
#     1:length(model_all),
#     ~left_join(
#       model_all[[.x]],
#       class_membership_cov[[.x]] |> 
#       select(
#         seqn,
#         lat_class_prob_cov = value,
#         lat_class_size_cov = n,
#         lat_class_cov = latent_class
#       )
#     )
#   )
  
#   model_all <-
#     cat_map(
#       model_all,
#       ~.x |> 
#         mutate(
#           lat_class_cov = as.factor(lat_class_cov),
#           lat_class_cov = relevel(lat_class_cov, ref = "class1")
#         )
#   )
  
#   # model_all[[1]] |> 
#   #   group_by(
#   #     seqn
#   #   ) |> 
#   #   mutate(
#   #     compare_class = case_when(
#   #       lat_class == lat_class_cov ~ TRUE,
#   #       TRUE ~ FALSE
#   #     )
#   #   ) |> 
#   #   ungroup() |> 
#   #   select(
#   #     seqn,
#   #     compare_class
#   #   ) |> 
#   #   count(compare_class)