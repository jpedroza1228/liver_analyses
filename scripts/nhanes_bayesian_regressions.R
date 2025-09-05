library(tidyverse)
# library(mice)
# library(miceadds)
library(rstanarm)
library(posterior)
library(bayesplot)
library(bayestestR)

cat_map <- purrr::map

theme_set(theme_light())

react_table <- function(data){
  reactable::reactable(
    {{data}},
    filterable = TRUE,
    sortable = TRUE,
    searchable = TRUE
  )
}

# model_all_prob5 <- read_rds(here::here("projects/nhanes/imputed_data_w_3profiles_each_imp.rds"))
# 
# model_all_prob5 <- cat_map(
#   model_all_prob5,
#   ~.x |> 
#     mutate(
#       ever_45_drink_everyday = if_else(ever_45_drink_everyday == 1, 1, 0),
#       told_liver_cond = if_else(told_liver_cond == 1, 1, 0),
#       profile3 = as.factor(profile3)
#     )
# )


model_all_prob5 <- read_rds(here::here("projects/nhanes/imputed_data_w_5profiles_each_imp.rds"))

model_all_prob5 <- cat_map(
  model_all_prob5,
  ~.x |> 
    mutate(
      ever_45_drink_everyday = if_else(ever_45_drink_everyday == 1, 1, 0),
      told_liver_cond = if_else(told_liver_cond == 1, 1, 0),
      profile = as.factor(profile)
    )
)

# --------------------------------------- Using Bayesian Inference --------------------------------------
model_lpa <- datlist2mids(model_all_prob5)

q25 <- function(y) quantile(y, 0.25)
q75 <- function(y) quantile(y, 0.75)
q10 <- function(y) quantile(y, 0.1)
q90 <- function(y) quantile(y, 0.9)

# --------------------------------------- Drink 4-5 drinks everyday --------------------------------------
# drink_stan_model <- with(
#   model_lpa,
#   stan_glm(
#     ever_45_drink_everyday ~ as.factor(profile),
#     family = binomial("logit"),
#     prior_intercept = normal(0, 10),
#     prior = normal(0, 10),
#     seed = 01102025
#     )
# )

# drink_model <-
#   cat_map(
#   model_all_prob5,
#   ~stan_glm(
#   ever_45_drink_everyday ~ profile,
#   family = binomial("logit"),
#   prior_intercept = normal(0, 10),
#   prior = normal(0, 10),
#   data = .x,
#   seed = 01102025
#   )
# )

# saveRDS(drink_model, here::here("projects/nhanes/ever_45_drink_everyday_model.rds"))

drink_model <- read_rds(here::here("projects/nhanes/ever_45_drink_everyday_model.rds"))

color_scheme_set("viridis")

summary(drink_model[[1]])
posterior_vs_prior(drink_model[[1]])

drink_model_pp_check <- cat_map(
  drink_model,
  ~pp_check(.x)
)

drink_model_pp_check[[round(rnorm(1, 30, 10))]]

drink_model_mcmc_trace <- cat_map(
  drink_model,
  ~mcmc_trace(.x)
)

drink_model_mcmc_trace[[round(rnorm(1, 30, 10))]]



# Posterior Sampling
drink_post_sample <- 
  map_dfr(
    drink_model,
    ~.x$stanfit |> 
      as.data.frame()
)

drink_post_sample <- 
  drink_post_sample |> 
  as_tibble() |>
  mutate(
    imp = rep(1:100, each = 4000)
  )

# saveRDS(drink_post_sample, here::here("projects/nhanes/ever_45_drink_everyday_posterior_samples.rds"))

drink_post_sample <- read_rds(here::here("projects/nhanes/ever_45_drink_everyday_posterior_samples.rds"))

drink_post_sample_para <- drink_post_sample |> 
  select(
    -c(
      mean_PPD,
      `log-posterior`
    )
  ) |> 
  rename(
    intercept = `(Intercept)`
  ) |> 
  mutate(
    across(
      -imp,
      ~round(.x, 1)
    )
  ) |> 
  relocate(
    imp,
    .before = intercept
  )

drink_post_sample_para |> 
  group_by(imp) |> 
  summarize(
    across(
      everything(),
      list(
        mean = ~mean(.x),
        sd = ~sd(.x),
        q10 = ~q10(.x),
        q25 = ~q25(.x),
        q75 = ~q75(.x),
        q90 = ~q90(.x)
      )
    )
  ) |> 
  ungroup()

drink_post_sample_para |> 
  summarize(
    across(
      -imp,
      list(
        mean = ~mean(.x),
        sd = ~sd(.x)
        #q10 = ~q10(.x),
        #q25 = ~q25(.x),
        #q75 = ~q75(.x),
        #q90 = ~q90(.x)
      )
    )
  ) |> 
  pivot_longer(
    everything()
  ) |> 
  separate(
    col = name,
    into = c(
      "parameter", "stat"
    ),
    sep = "_"
  ) |> 
  ggplot(
    aes(
      parameter,
      value
    )
  ) +
  geom_col(
    position = position_dodge(),
    aes(
      fill = stat
    ),
    color = "black"
  ) +
  coord_flip() +
  labs(
    x = "",
    y = ""
  ) +
  theme(
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

ci(drink_post_sample_para[, -1], ci = .89)

drink_post_sample_para |> 
  summarize(
    across(
      -imp,
      ~mean(.x)
    ),
    across(
      -imp,
      ~round(.x, 2)
    )
  ) |> 
  pivot_longer(
    everything()
  ) |> 
  mutate(
    ci95_low = ci(drink_post_sample_para[, -1])$CI_low,
    ci95_upper = ci(drink_post_sample_para[, -1])$CI_high,
    or = exp(value),
    prob = plogis(value)
  )


# Posterior Predictions
drink_yrep <- cat_map(
  drink_model,
  ~posterior_predict(.x)
)

# saveRDS(drink_yrep, here::here("projects/nhanes/ever_45_drink_everyday_posterior_predictions.rds"))

drink_yrep <- read_rds(here::here("projects/nhanes/ever_45_drink_everyday_posterior_predictions.rds"))

ppc_bars(
  model_all_prob5[[50]]$ever_45_drink_everyday,
  drink_yrep[[50]]
)

drink_counts <- 
  map_dfr(
  1:length(drink_yrep),
  ~ppc_bars(
    model_all_prob5[[.x]]$ever_45_drink_everyday,
    drink_yrep[[.x]]
  )$data
)

drink_counts |> 
  mutate(
    imp = rep(1:100, each = 2)
  ) |> 
  group_by(
    x
  ) |> 
  summarize(
    y_obs = mean(y_obs)
  ) |> 
  ungroup() |> 
  ggplot(
    aes(
      x,
      y_obs
    )
  ) +
  geom_col(
    fill = NA,
    color = "black"
  ) +
  geom_point(
    data = drink_counts |>
      mutate(
        imp = rep(1:100, each = 2)
    ),
    aes(
      x = x,
      y = m,
      color = as.factor(imp)
    )
  ) +
  geom_errorbar(
    data = drink_counts |> 
      mutate(
        imp = rep(1:100, each = 2)
      ),
    aes(
      x = x,
      ymin = l,
      ymax = h,
      color = as.factor(imp)
    )
  ) +
  theme(
    legend.position = "none"
  )


drink_ppc_mean <- cat_map(
  1:length(drink_yrep),
  ~ppc_stat(
    model_all_prob5[[.x]]$ever_45_drink_everyday,
    drink_yrep[[.x]],
    stat = "mean"
  )$data |> 
    mutate(
      y_fit = mean(model_all_prob5[[.x]]$ever_45_drink_everyday),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)

map_dfr(
  drink_ppc_mean,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )


drink_ppc_sd <- cat_map(
  1:length(drink_yrep),
  ~ppc_stat(
    model_all_prob5[[.x]]$ever_45_drink_everyday,
    drink_yrep[[.x]],
    stat = "sd"
  )$data |> 
    mutate(
      y_fit = sd(model_all_prob5[[.x]]$ever_45_drink_everyday),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)

map_dfr(
  drink_ppc_sd,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )


# --------------------------------------- Ever told Liver Condition --------------------------------------
# liver_cond_model <-
#   cat_map(
#   model_all_prob5,
#   ~stan_glm(
#   told_liver_cond ~ profile,
#   family = binomial("logit"),
#   prior_intercept = normal(0, 10),
#   prior = normal(0, 10),
#   data = .x,
#   seed = 01102025
#   )
# )

# saveRDS(liver_cond_model, here::here("projects/nhanes/told_liver_condition_model.rds"))

liver_cond_model <- read_rds(here::here("projects/nhanes/told_liver_condition_model.rds"))

summary(liver_cond_model[[1]])
prior_summary(liver_cond_model[[1]])
posterior_vs_prior(liver_cond_model[[1]])

liver_cond_model_pp_check <- cat_map(
  liver_cond_model,
  ~pp_check(.x)
)

liver_cond_model_pp_check[[round(rnorm(1, 50, 10))]]

liver_cond_model_mcmc_trace <- cat_map(
  liver_cond_model,
  ~mcmc_trace(.x)
)

liver_cond_model_mcmc_trace[[round(rnorm(1, 50, 10))]]


# Posterior Sampling
liver_cond_post_sample <- 
  map_dfr(
    liver_cond_model,
    ~.x$stanfit |> 
      as.data.frame()
)

liver_cond_post_sample <- 
  liver_cond_post_sample |> 
  as_tibble() |>
  mutate(
    imp = rep(1:100, each = 4000)
  )

# saveRDS(liver_cond_post_sample, here::here("projects/nhanes/liver_condition_posterior_samples.rds"))

liver_cond_post_sample <- read_rds(here::here("projects/nhanes/liver_condition_posterior_samples.rds"))

liver_cond_post_sample_para <- liver_cond_post_sample |> 
  select(
    -c(
      mean_PPD,
      `log-posterior`
    )
  ) |> 
  rename(
    intercept = `(Intercept)`
  ) |> 
  mutate(
    across(
      -imp,
      ~round(.x, 1)
    )
  ) |> 
  relocate(
    imp,
    .before = intercept
  )

liver_cond_post_sample_para |> 
  # group_by(imp) |> 
    summarize(
      across(
        -imp,
        list(
          mean = ~mean(.x),
          sd = ~sd(.x),
          q10 = ~q10(.x),
          q25 = ~q25(.x),
          q75 = ~q75(.x),
          q90 = ~q90(.x)
        )
      )
    ) |> 
  pivot_longer(
    everything()
  ) |> 
  separate(
    name,
    into = c(
      "parameter",
      "stat"
    ),
    sep = "_"
  ) |> 
  pivot_wider(
    names_from = stat,
    values_from = value
  )

liver_cond_post_sample_para |> 
  summarize(
    across(
      -imp,
      list(
        mean = ~mean(.x),
        sd = ~sd(.x)
      )
    )
  ) |> 
  pivot_longer(
    everything()
  ) |> 
  separate(
    col = name,
    into = c(
      "parameter", "stat"
    ),
    sep = "_"
  ) |> 
  ggplot(
    aes(
      parameter,
      value
    )
  ) +
  geom_col(
    position = position_dodge(),
    aes(
      fill = stat
    ),
    color = "black"
  ) +
  coord_flip() +
  labs(
    x = "",
    y = ""
  ) +
  theme(
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

ci(liver_cond_post_sample_para[, -1], ci = .89)

liver_cond_post_sample_para |> 
  summarize(
    across(
      -imp,
      ~mean(.x)
    ),
    across(
      -imp,
      ~round(.x, 2)
    )
  ) |> 
  pivot_longer(
    everything()
  ) |> 
  mutate(
    ci95_low = ci(liver_cond_post_sample_para[, -1])$CI_low,
    ci95_upper = ci(liver_cond_post_sample_para[, -1])$CI_high,
    or = exp(value),
    prob = plogis(value)
  )

# saveRDS(liver_cond_yrep, here::here("projects/nhanes/liver_condition_posterior_predictions.rds"))

liver_cond_yrep <- read_rds(here::here("projects/nhanes/liver_condition_posterior_predictions.rds"))


# Posterior Predictions
liver_cond_yrep <- cat_map(
  liver_cond_model,
  ~posterior_predict(.x)
)

liver_counts <- 
  map_dfr(
  1:length(liver_cond_yrep),
  ~ppc_bars(
    model_all_prob5[[.x]]$told_liver_cond,
    liver_cond_yrep[[.x]]
  )$data
)

liver_counts |> 
  mutate(
    imp = rep(1:100, each = 2)
  ) |> 
  group_by(
    x
  ) |> 
  summarize(
    y_obs = mean(y_obs)
  ) |> 
  ungroup() |> 
  ggplot(
    aes(
      x,
      y_obs
    )
  ) +
  geom_col(
    fill = NA,
    color = "black"
  ) +
  geom_point(
    data = liver_counts |>
      mutate(
        imp = rep(1:100, each = 2)
    ),
    aes(
      x = x,
      y = m,
      color = as.factor(imp)
    )
  ) +
  geom_errorbar(
    data = liver_counts |> 
      mutate(
        imp = rep(1:100, each = 2)
      ),
    aes(
      x = x,
      ymin = l,
      ymax = h,
      color = as.factor(imp)
    )
  ) +
  theme(
    legend.position = "none"
  )


ppc_stat(
  model_all_prob5[[1]]$told_liver_cond,
  liver_cond_yrep[[1]],
  stat = "mean"
)

liver_cond_ppc_mean <- cat_map(
  1:length(liver_cond_yrep),
  ~ppc_stat(
    model_all_prob5[[.x]]$told_liver_cond,
    liver_cond_yrep[[.x]],
    stat = "mean"
  )$data |> 
    mutate(
      y_fit = mean(model_all_prob5[[.x]]$told_liver_cond),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)

map_dfr(
  liver_cond_ppc_mean,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )


ppc_stat(
  model_all_prob5[[1]]$told_liver_cond,
  liver_cond_yrep[[1]],
  stat = "sd"
)

liver_cond_ppc_sd <- cat_map(
  1:length(liver_cond_yrep),
  ~ppc_stat(
    model_all_prob5[[.x]]$told_liver_cond,
    liver_cond_yrep[[.x]],
    stat = "sd"
  )$data |> 
    mutate(
      y_fit = sd(model_all_prob5[[.x]]$told_liver_cond),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)

map_dfr(
  liver_cond_ppc_sd,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )


# --------------------------------------- BMI --------------------------------------
# bmi_model <-
#   cat_map(
#   model_all_prob5,
#   ~stan_glm(
#   bmi ~ profile,
#   family = gaussian(),
#   prior_intercept = normal(0, 100),
#   prior = normal(0, 10),
#   prior_aux = normal(0, 10),
#   data = .x,
#   seed = 01102025
#   )
# )

# saveRDS(bmi_model, here::here("projects/nhanes/bmi_model.rds"))

bmi_model <- read_rds(here::here("projects/nhanes/bmi_model.rds"))


summary(bmi_model[[1]])
prior_summary(bmi_model[[1]])
posterior_vs_prior(bmi_model[[1]])

bmi_model_pp_check <- cat_map(
  bmi_model,
  ~pp_check(.x)
)

bmi_model_pp_check[[round(rnorm(1, 50, 10))]]

bmi_model_mcmc_trace <- cat_map(
  bmi_model,
  ~mcmc_trace(.x)
)

bmi_model_mcmc_trace[[round(rnorm(1, 50, 10))]]


# Posterior Sampling
bmi_post_sample <- 
  map_dfr(
    bmi_model,
    ~.x$stanfit |> 
      as.data.frame()
)

bmi_post_sample <- 
  bmi_post_sample |> 
  as_tibble() |>
  mutate(
    imp = rep(1:100, each = 4000)
  )

# saveRDS(bmi_post_sample, here::here("projects/nhanes/bmi_posterior_samples.rds"))

bmi_post_sample <- read_rds(here::here("projects/nhanes/bmi_posterior_samples.rds"))

bmi_post_sample_para <- bmi_post_sample |> 
  select(
    -c(
      mean_PPD,
      `log-posterior`
    )
  ) |> 
  rename(
    intercept = `(Intercept)`
  ) |> 
  mutate(
    across(
      -imp,
      ~round(.x, 1)
    )
  ) |> 
  relocate(
    imp,
    .before = intercept
  )

bmi_post_sample_para |> 
  # group_by(imp) |> 
    summarize(
      across(
        -imp,
        list(
          mean = ~mean(.x),
          sd = ~sd(.x),
          q10 = ~q10(.x),
          q25 = ~q25(.x),
          q75 = ~q75(.x),
          q90 = ~q90(.x)
        )
      )
    ) |> 
  pivot_longer(
    everything()
  ) |> 
  separate(
    name,
    into = c(
      "parameter",
      "stat"
    ),
    sep = "_"
  ) |> 
  pivot_wider(
    names_from = stat,
    values_from = value
  )

bmi_post_sample_para |> 
  summarize(
    across(
      -imp,
      list(
        mean = ~mean(.x),
        sd = ~sd(.x)
      )
    )
  ) |> 
  pivot_longer(
    everything()
  ) |> 
  separate(
    col = name,
    into = c(
      "parameter", "stat"
    ),
    sep = "_"
  ) |> 
  ggplot(
    aes(
      parameter,
      value
    )
  ) +
  geom_col(
    position = position_dodge(),
    aes(
      fill = stat
    ),
    color = "black"
  ) +
  coord_flip() +
  labs(
    x = "",
    y = ""
  ) +
  theme(
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

ci(bmi_post_sample_para[, -1], ci = .89)

bmi_post_sample_para |> 
  summarize(
    across(
      -imp,
      ~mean(.x)
    ),
    across(
      -imp,
      ~round(.x, 2)
    )
  ) |> 
  pivot_longer(
    everything()
  ) |> 
  mutate(
    ci95_low = ci(bmi_post_sample_para[, -1])$CI_low,
    ci95_upper = ci(bmi_post_sample_para[, -1])$CI_high
  )



# Posterior Predictions
bmi_yrep <- cat_map(
  bmi_model,
  ~posterior_predict(.x)
)

# saveRDS(bmi_yrep, here::here("projects/nhanes/bmi_posterior_predictions.rds"))

bmi_yrep <- read_rds(here::here("projects/nhanes/bmi_posterior_predictions.rds"))

ppc_intervals(
  as.numeric(model_all_prob5[[1]]$bmi),
  bmi_yrep[[1]][, 1:938]
) +
  coord_flip()

bmi_intervals <- 
  map_dfr(
  1:length(bmi_yrep),
  ~ppc_intervals(
    as.numeric(model_all_prob5[[.x]]$bmi),
    bmi_yrep[[.x]]
  )$data
)

bmi_intervals |> 
  mutate(
    imp = rep(1:100, each = 938)
  ) |> 
  group_by(
    x,
    # imp
  ) |> 
  summarize(
    across(
      c(
        y_obs, 
        m,
        ll,
        hh
      ),
      ~mean(.x)
    ),
    .groups = "drop"
  ) |> 
  pivot_longer(
    c(
      y_obs,
      m
    )
  ) |> 
  ggplot(
    aes(
      x,
      value
    )
  ) +
  geom_point(
    aes(
      color = name
    )
  ) +
  geom_errorbar(
    aes(
      ymin = ll,
      ymax = hh
    ),
    alpha = .3,
    color = "#ffa056"
  ) +
  coord_flip() +
  see::scale_color_okabeito() +
  theme(
    legend.position = "top"
  )


ppc_stat(
  as.numeric(model_all_prob5[[1]]$bmi),
  bmi_yrep[[1]],
  stat = "mean"
)

bmi_ppc_mean <- cat_map(
  1:length(bmi_yrep),
  ~ppc_stat(
    as.numeric(model_all_prob5[[.x]]$bmi),
    bmi_yrep[[.x]],
    stat = "mean"
  )$data |> 
    mutate(
      y_fit = mean(model_all_prob5[[.x]]$bmi),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)

map_dfr(
  bmi_ppc_mean,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )


ppc_stat(
  as.numeric(model_all_prob5[[1]]$bmi),
  bmi_yrep[[1]],
  stat = "sd"
)

bmi_ppc_sd <- cat_map(
  1:length(bmi_yrep),
  ~ppc_stat(
    as.numeric(model_all_prob5[[.x]]$bmi),
    bmi_yrep[[.x]],
    stat = "sd"
  )$data |> 
    mutate(
      y_fit = sd(model_all_prob5[[.x]]$bmi),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)

map_dfr(
  bmi_ppc_sd,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )

ppc_stat(
  as.numeric(model_all_prob5[[1]]$bmi),
  bmi_yrep[[1]],
  stat = "q25"
)
  
bmi_ppc_q25 <- cat_map(
  1:length(bmi_yrep),
  ~ppc_stat(
    as.numeric(model_all_prob5[[.x]]$bmi),
    bmi_yrep[[.x]],
    stat = "q25"
  )$data |> 
    mutate(
      y_fit = q25(model_all_prob5[[.x]]$bmi),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)
  
map_dfr(
  bmi_ppc_q25,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )


ppc_stat(
  as.numeric(model_all_prob5[[1]]$bmi),
  bmi_yrep[[1]],
  stat = "q75"
)
    
bmi_ppc_q75 <- cat_map(
  1:length(bmi_yrep),
  ~ppc_stat(
    as.numeric(model_all_prob5[[.x]]$bmi),
    bmi_yrep[[.x]],
    stat = "q75"
  )$data |> 
    mutate(
      y_fit = q75(model_all_prob5[[.x]]$bmi),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)
    
map_dfr(
  bmi_ppc_q75,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )


# --------------------------------------- Waist Circumference --------------------------------------
# waist_model <-
#   cat_map(
#   model_all_prob5,
#   ~stan_glm(
#   waist_circumference ~ profile,
#   family = gaussian(),
#   prior_intercept = normal(0, 100),
#   prior = normal(0, 10),
#   prior_aux = normal(0, 10),
#   data = .x,
#   seed = 01102025
#   )
# )
# 
# saveRDS(waist_model, here::here("projects/nhanes/waist_model.rds"))

waist_model <- read_rds(here::here("projects/nhanes/waist_model.rds"))

summary(waist_model[[1]])
prior_summary(waist_model[[1]])
posterior_vs_prior(waist_model[[1]])

waist_model_pp_check <- cat_map(
  waist_model,
  ~pp_check(.x)
)

waist_model_pp_check[[round(rnorm(1, 50, 10))]]

waist_model_mcmc_trace <- cat_map(
  waist_model,
  ~mcmc_trace(.x)
)

waist_model_mcmc_trace[[round(rnorm(1, 50, 10))]]


# Posterior Sampling
waist_post_sample <- 
  map_dfr(
    waist_model,
    ~.x$stanfit |> 
      as.data.frame()
)

waist_post_sample <- 
  waist_post_sample |> 
  as_tibble() |>
  mutate(
    imp = rep(1:100, each = 4000)
  )

# saveRDS(waist_post_sample, here::here("projects/nhanes/waist_circumference_posterior_samples.rds"))

waist_post_sample <- read_rds(here::here("projects/nhanes/waist_circumference_posterior_samples.rds"))

waist_post_sample_para <- waist_post_sample |> 
  select(
    -c(
      mean_PPD,
      `log-posterior`
    )
  ) |> 
  rename(
    intercept = `(Intercept)`
  ) |> 
  mutate(
    across(
      -imp,
      ~round(.x, 1)
    )
  ) |> 
  relocate(
    imp,
    .before = intercept
  )

waist_post_sample_para |> 
  # group_by(imp) |> 
    summarize(
      across(
        -imp,
        list(
          mean = ~mean(.x),
          sd = ~sd(.x),
          q10 = ~q10(.x),
          q25 = ~q25(.x),
          q75 = ~q75(.x),
          q90 = ~q90(.x)
        )
      )
    ) |> 
  pivot_longer(
    everything()
  ) |> 
  separate(
    name,
    into = c(
      "parameter",
      "stat"
    ),
    sep = "_"
  ) |> 
  pivot_wider(
    names_from = stat,
    values_from = value
  )

waist_post_sample_para |> 
  summarize(
    across(
      -imp,
      list(
        mean = ~mean(.x),
        sd = ~sd(.x)
      )
    )
  ) |> 
  pivot_longer(
    everything()
  ) |> 
  separate(
    col = name,
    into = c(
      "parameter", "stat"
    ),
    sep = "_"
  ) |> 
  ggplot(
    aes(
      parameter,
      value
    )
  ) +
  geom_col(
    position = position_dodge(),
    aes(
      fill = stat
    ),
    color = "black"
  ) +
  coord_flip() +
  labs(
    x = "",
    y = ""
  ) +
  theme(
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

ci(waist_post_sample_para[, -1], ci = .89)

waist_post_sample_para |> 
  summarize(
    across(
      -imp,
      ~mean(.x)
    ),
    across(
      -imp,
      ~round(.x, 2)
    )
  ) |> 
  pivot_longer(
    everything()
  ) |> 
  mutate(
    ci95_low = ci(waist_post_sample_para[, -1])$CI_low,
    ci95_upper = ci(waist_post_sample_para[, -1])$CI_high
  )



# Posterior Predictions
waist_yrep <- cat_map(
  waist_model,
  ~posterior_predict(.x)
)


# saveRDS(waist_yrep, here::here("projects/nhanes/waist_circumference_posterior_predictions.rds"))

waist_yrep <- read_rds(here::here("projects/nhanes/waist_circumference_posterior_predictions.rds"))

ppc_intervals(
  as.numeric(model_all_prob5[[1]]$waist_circumference),
  waist_yrep[[1]][, 1:938]
) +
  coord_flip()

waist_intervals <- 
  map_dfr(
  1:length(waist_yrep),
  ~ppc_intervals(
    as.numeric(model_all_prob5[[.x]]$waist_circumference),
    waist_yrep[[.x]]
  )$data
)

waist_intervals |> 
  mutate(
    imp = rep(1:100, each = 938)
  ) |> 
  group_by(
    x,
    # imp
  ) |> 
  summarize(
    across(
      c(
        y_obs, 
        m,
        ll,
        hh
      ),
      ~mean(.x)
    ),
    .groups = "drop"
  ) |> 
  pivot_longer(
    c(
      y_obs,
      m
    )
  ) |> 
  ggplot(
    aes(
      x,
      value
    )
  ) +
  geom_point(
    aes(
      color = name
    )
  ) +
  geom_errorbar(
    aes(
      ymin = ll,
      ymax = hh
    ),
    alpha = .3,
    color = "#ffa056"
  ) +
  coord_flip() +
  see::scale_color_okabeito() +
  theme(
    legend.position = "top"
  )


ppc_stat(
  as.numeric(model_all_prob5[[1]]$waist_circumference),
  waist_yrep[[1]],
  stat = "mean"
)

waist_ppc_mean <- cat_map(
  1:length(waist_yrep),
  ~ppc_stat(
    as.numeric(model_all_prob5[[.x]]$waist_circumference),
    waist_yrep[[.x]],
    stat = "mean"
  )$data |> 
    mutate(
      y_fit = mean(model_all_prob5[[.x]]$waist_circumference),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)

map_dfr(
  waist_ppc_mean,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )


ppc_stat(
  as.numeric(model_all_prob5[[1]]$waist_circumference),
  waist_yrep[[1]],
  stat = "sd"
)

waist_ppc_sd <- cat_map(
  1:length(waist_yrep),
  ~ppc_stat(
    as.numeric(model_all_prob5[[.x]]$waist_circumference),
    waist_yrep[[.x]],
    stat = "sd"
  )$data |> 
    mutate(
      y_fit = sd(model_all_prob5[[.x]]$waist_circumference),
      over_under = case_when(
        value > y_fit ~ 1,
        TRUE ~ 0
      )
    ) |> 
    summarize(
      prob = mean(over_under)
    )
)

map_dfr(
  waist_ppc_sd,
  ~.x
) |> 
  summarize(
    avg = round(mean(prob), 2),
    sd = round(sd(prob), 2)
  )
