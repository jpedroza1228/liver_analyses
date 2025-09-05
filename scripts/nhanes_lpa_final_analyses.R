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

model_all <- read_rds(here::here("projects/nhanes/data_w_lca_class_membership.rds"))

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

lpa_bic <- cat_map(
  indicators,
  ~mclustBIC(.x)
)

bic_values <- 
  map_dfr(
  1:length(lpa_bic),
  ~lpa_bic[[.x]][1:9, 1:14] |> 
    as_tibble()
) |> 
  mutate(
    across(
      everything(),
      ~round(.x, 2)
     ),
    id = rep(seq(1, 9, 1), 60),
    imp = rep(1:60, each = 9)
  ) |> 
  pivot_longer(
    -c(
      id,
      imp
    )
  ) |> 
  group_by(
    id,
    name
  ) |> 
  summarize(
    value = mean(value, na.rm = TRUE),
    value = round(value, 2)
  ) |> 
  ungroup() |> 
  ggplot(
    aes(
      as.factor(id),
      value,
      group = as.factor(name)
    )
  ) +
  geom_line(
    aes(
      color = as.factor(name)
    )
  ) +
  geom_point() +
  NULL

plotly::ggplotly(bic_values)

lpa <- cat_map(
  indicators,
  ~Mclust(
    .x,
    G = 3
  )
)

summary(lpa[[1]])

lpa[[1]]$parameters$mean |> 
  as_tibble() |> 
  mutate(
    indicator = colnames(indicators[[1]])
  ) |> 
  pivot_longer(-indicator) |> 
  ggplot(
    aes(
      indicator,
      value
    )
  ) +
  geom_col(
    aes(
      fill = as.factor(name)
    ),
    color = "black",
    position = position_dodge()
  ) +
  coord_flip() +
  see::scale_fill_okabeito() +
  theme_light()

# map_dfr(
#   1:length(lpa),
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
#     imp = rep(1:60, each = 10)
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
#     )



indicators7 <- 
  cat_map(
  model_all,
  ~.x |> 
    select(
      # albumin_g_dl,
      # alp_iu_l,
      ast_u_l,
      alt_u_l,
      ggt_u_l,
      # total_bilirubin_mg_dl,
      hdl_chol_mg_dl,
      trigly_mg_dl,
      ldl_chol_mg_dl,            
      total_chol_mg_dl
    )
)
    
lpa_bic7 <- cat_map(
  indicators,
  ~mclustBIC(.x)
)
    
bic_values7 <- 
  map_dfr(
  1:length(lpa_bic),
  ~lpa_bic[[.x]][1:9, 1:14] |> 
    as_tibble()
) |> 
  mutate(
    across(
      everything(),
      ~round(.x, 2)
     ),
    id = rep(seq(1, 9, 1), 60),
    imp = rep(1:60, each = 9)
  ) |> 
  pivot_longer(
    -c(
      id,
      imp
    )
  ) |> 
  group_by(
    id,
    name
  ) |> 
  summarize(
    value = mean(value, na.rm = TRUE),
    value = round(value, 2)
  ) |> 
  ungroup() |> 
  ggplot(
    aes(
      as.factor(id),
      value,
      group = as.factor(name)
    )
  ) +
  geom_line(
    aes(
      color = as.factor(name)
    )
  ) +
  geom_point() +
  NULL

plotly::ggplotly(bic_values7)
    
lpa7 <- cat_map(
  indicators7,
  ~Mclust(
    .x,
    G = 3
  )
)

summary(lpa7[[1]])

lpa7[[1]]$parameters$mean |> 
  as_tibble() |> 
  mutate(
    indicator = colnames(indicators7[[1]])
  ) |> 
  pivot_longer(-indicator) |> 
  ggplot(
    aes(
      indicator,
      value
    )
  ) +
  geom_col(
    aes(
      fill = as.factor(name)
    ),
    color = "black",
    position = position_dodge()
  ) +
  facet_wrap(
    vars(
      indicator
    ),
    scales = "free"
  ) +
  see::scale_fill_okabeito() +
  theme_light()


lpa7[[4]]$parameters$mean

lpa7[[2]]$parameters$mean |> 
  as_tibble() |> 
  mutate(
    indicator = colnames(indicators7[[1]])
  ) |>
  pivot_longer(
    -indicator
  ) |> 
  filter(
    str_detect(indicator, "ast") |
      str_detect(indicator, "alt") |
      str_detect(indicator, "ggt")
  ) |> 
  group_by(indicator) |> 
  mutate(
    lat_profile = case_when(
      value == max(value) ~ "profile3",
      value == min(value) ~ "profile1",
      TRUE ~ "profile2"
    )
  ) |> 
  ungroup()
