# start here with data
library(tidyverse)
library(mclust)
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

latino <- read_rds(here::here("projects/nhanes/latino_20plus.rds"))

glimpse(latino)

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


psych::describe(latino$ggt_u_l, na.rm = TRUE)

# albumin_g_dl           # 3.5 - 5.5 g/dl
# alp_iu_l               # 44 - 147 iu/l
# alt_u_l                # 7-56 u/l
# ast_u_l                # 8-33 u/l, 14-20 u/l for men (ucsf)
# ggt_u_l                # below 50 u/l, 5-40 u/l for men (ucsf)
# HDL                    # 60 + = normal
# LDL                    # Under 100 = normal
# total_bilirubin_mg_dl  # 0.2-1.3 mg/dl
# Total Cholesterol      # Under 200 = normal
# trigly                 # under 150 = normal

latino |> 
  mutate(
    albumin_bi = case_when(
      albumin_g_dl > 5.5 ~ 1,
      albumin_g_dl < 3.5 ~ 1,
      TRUE ~ 0
    ),
    alp_bi = case_when(
      alp_iu_l > 147 ~ 1,
      alp_iu_l < 44 ~ 1,
      TRUE ~ 0
    ),
    alt_bi = case_when(
      alt_u_l > 56 ~ 1,
      alt_u_l < 7 ~ 1,
      TRUE ~ 0
    ),
    ast_bi = case_when(
      ast_u_l > 33 ~ 1,
      ast_u_l < 8 ~ 1,
      TRUE ~ 0
    ),
    ggt_bi = case_when(
      ggt_u_l > 50 ~ 1,
      TRUE ~ 0
    ),
    hdl_bi = case_when(
      hdl_chol_mg_dl < 60 ~ 1,
      TRUE ~ 0
    ),
    ldl_bi = case_when(
      ldl_chol_mg_dl > 100 ~ 1,
      TRUE ~ 0
    ),
    bilirubin_bi = case_when(
      total_bilirubin_mg_dl > 1.3 ~ 1,
      total_bilirubin_mg_dl < 0.2 ~ 1,
      TRUE ~ 0
    ),
    total_chol_bi = case_when(
      total_chol_mg_dl > 200 ~ 1,
      TRUE ~ 0
    ),
    trigly_bi = case_when(
      trigly_mg_dl > 150 ~ 1,
      TRUE ~ 0
    )
  ) |> 
    select(
      albumin_g_dl,
      alp_iu_l,
      alt_u_l,
      ast_u_l,
      ggt_u_l,
      total_bilirubin_mg_dl,
      hdl_chol_mg_dl,
      ldl_chol_mg_dl,
      total_chol_mg_dl,
      trigly_mg_dl,
      matches("_bi$")
    ) |> 
  pivot_longer(
    cols = matches("_bi$")
  ) |> 
  filter(
    str_detect(name, "trigly")
  ) |> 
  ggplot(
    aes(
      trigly_mg_dl
    )
  ) +
  geom_histogram(
    aes(
      fill = as.factor(value)
    )
  )

# albumin_g_dl           # 3.5 - 5.5 g/dl
# only have lower levels that are outside normal range

# alp_iu_l               # 44 - 147 iu/l
# have mostly lower than normal ranges, also some that are much higher (may want to remove higher values)

# alt_u_l                # 7-56 u/l
# only have higher levels

# ast_u_l                # 8-33 u/l, 14-20 u/l for men (ucsf)
# only have higher levels

# ggt_u_l                # below 50 u/l, 5-40 u/l for men (ucsf)
# only have higher values

# HDL                    # 60 + = normal


# LDL                    # Under 100 = normal


# total_bilirubin_mg_dl  # 0.2-1.3 mg/dl


# Total Cholesterol      # Under 200 = normal


# trigly                 # under 150 = normal



model_imp <- read_rds(here::here("projects/nhanes/imputation_model_more_var.rds"))

set.seed(12345)
model_all <- mice::complete(model_imp, "all")

# male only data
# male_model <- cat_map(
#   model_all,
#   ~.x |> 
#     filter(
#       sex == 1
#     )
# )

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

# mutate(
#   albumin_bi = case_when(
#     albumin_g_dl > 5.5 ~ 3,
#     albumin_g_dl < 3.5 ~ 2,
#     TRUE ~ 1
#   ),
#   alp_bi = case_when(
#     alp_iu_l > 147 ~ 3,
#     alp_iu_l < 44 ~ 2,
#     TRUE ~ 1
#   ),
#   alt_bi = case_when(
#     alt_u_l > 56 ~ 3,
#     alt_u_l < 7 ~ 2,
#     TRUE ~ 1
#   ),
#   ast_bi = case_when(
#     ast_u_l > 33 ~ 3,
#     ast_u_l < 8 ~ 2,
#     TRUE ~ 1
#   ),
#   ggt_bi = case_when(
#     ggt_u_l > 50 ~ 3,
#     TRUE ~ 1
#   ),
#   hdl_bi = case_when(
#     hdl_chol_mg_dl < 60 ~ 2,
#     TRUE ~ 1
#   ),
#   ldl_bi = case_when(
#     ldl_chol_mg_dl > 100 ~ 3,
#     TRUE ~ 1
#   ),
#   bilirubin_bi = case_when(
#     total_bilirubin_mg_dl > 1.3 ~ 3,
#     total_bilirubin_mg_dl < 0.2 ~ 2,
#     TRUE ~ 1
#   ),
#   total_chol_bi = case_when(
#     total_chol_mg_dl > 200 ~ 3,
#     TRUE ~ 1
#   ),
#   trigly_bi = case_when(
#     trigly_mg_dl > 150 ~ 3,
#     TRUE ~ 1
#   )
# )

# set.seed(12345)
# model_long <- mice::complete(model_imp, "long") |> 
#     mutate(
#       albumin_bi = case_when(
#         albumin_g_dl > 5.5 ~ 3,
#         albumin_g_dl < 3.5 ~ 2,
#         TRUE ~ 1
#       ),
#       alp_bi = case_when(
#         alp_iu_l > 147 ~ 3,
#         alp_iu_l < 44 ~ 2,
#         TRUE ~ 1
#       ),
#       alt_bi = case_when(
#         alt_u_l > 56 ~ 3,
#         alt_u_l < 7 ~ 2,
#         TRUE ~ 1
#       ),
#       ast_bi = case_when(
#         ast_u_l > 33 ~ 3,
#         ast_u_l < 8 ~ 2,
#         TRUE ~ 1
#       ),
#       ggt_bi = case_when(
#         ggt_u_l > 50 ~ 3,
#         TRUE ~ 1
#       ),
#       hdl_bi = case_when(
#         hdl_chol_mg_dl < 60 ~ 2,
#         TRUE ~ 1
#       ),
#       ldl_bi = case_when(
#         ldl_chol_mg_dl > 100 ~ 3,
#         TRUE ~ 1
#       ),
#       bilirubin_bi = case_when(
#         total_bilirubin_mg_dl > 1.3 ~ 3,
#         total_bilirubin_mg_dl < 0.2 ~ 2,
#         TRUE ~ 1
#       ),
#       total_chol_bi = case_when(
#         total_chol_mg_dl > 200 ~ 3,
#         TRUE ~ 1
#       ),
#       trigly_bi = case_when(
#         trigly_mg_dl > 150 ~ 3,
#         TRUE ~ 1
#       )
#     )

# colnames(model_long)
# 
# model_long |> 
#   ggplot(
#     aes(
#       alp_iu_l
#     )
#   ) +
#   geom_histogram(
#     aes(
#       fill = as.factor(alp_bi)
#     )
#   )



# LCA instead of LPA -----------------------------------------------------------
indicators <- 
  cat_map(
  model_all,
  ~.x |> 
    select(
      seqn,
      # age,
      # sex,
      # citizen,
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
  total_chol_bi,
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

lca_models[[3]]

lca_models[[3]]$probs.start

final_probs <- cat_map(lca_models, ~reshape2::melt(.x$probs, level = 2))

final_probs[[3]]

lca_models[[3]]$probs

lca_models[[3]]$posterior |> 
  as_tibble() |> 
  mutate(
    id = seq(1, 938, 1)
  ) |> 
  pivot_longer(
    -id
  ) |> 
  group_by(
    id
  ) |> 
  filter(
    value == max(value)
  ) |> 
  ungroup()

plots <- cat_map(
  final_probs,
  ~.x |>
  mutate(
    Var2 = case_when(
      Var2 == "Pr(1)" ~ "Normal",
      Var2 == "Pr(2)" ~ "Abnormal"
    ),
    Var2 = as.factor(Var2),
    Var2 = fct_relevel(
      Var2,
      "Normal",
      "Abnormal"
    ),
    L2 = as.factor(L2),
    L2 = fct_relevel(
      L2,
      "albumin_bi",
      "alp_bi",
      "alt_bi",
      "ast_bi",
      "ggt_bi",
      "hdl_bi",
      "ldl_bi",
      "bilirubin_bi",
      "total_chol_bi",
      "trigly_bi"
    )
  ) |>
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
  #geom_hline(
  #  yintercept = .7,
  #  linetype = 2
  #  ) +
  coord_flip() +
  facet_wrap(
    ~Var1
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, .1)
  ) +
  see::scale_fill_okabeito() +
  theme_light()
)
plots[[3]]

# final_probs[[3]]

# plots2 <- cat_map(
#   final_probs,
#   ~.x |>
#   mutate(
#     Var2 = case_when(
#       Var2 == "Pr(1)" ~ "Normal",
#       Var2 == "Pr(2)" ~ "Low",
#       Var2 == "Pr(3)" ~ "High",
#     ),
#     L2 = as.factor(L2),
#     L2 = fct_relevel(
#       L2,
#       "albumin_bi",
#       "alp_bi",
#       "alt_bi",
#       "ast_bi",
#       "ggt_bi",
#       "hdl_bi",
#       "ldl_bi",
#       "bilirubin_bi",
#       "total_chol_bi",
#       "trigly_bi"
#     )
#   ) |>
#     ggplot(
#       aes(
#         L2,
#         value,
#         fill = Var2
#       )
#     ) +
#     geom_col(
#       position = position_dodge(),
#       color = "black"
#     ) +
#     #coord_flip() +
#       facet_wrap(
#         ~Var1,
#         ncol = 1
#       ) +
#       scale_y_continuous(
#         limits = c(0, 1.05),
#         breaks = seq(0, 1, .1)
#       ) +
#       see::scale_fill_okabeito() +
#       theme_light()
# )

# plots2[[3]]


# reorder for all multiple imputed datasets
# group 1 = some issues, high total chol, high ldl, low hdl
# group 2 = many issues, high alt, ast, low hdl, high ldl, somewhat high total cholesterol
# group 3 = mostly healthy, low hdl, roughly 50% high ldl
# healthy comparison group, somewhat at risk, at risk



set.seed(12345)
lca <- cat_map(
  indicators,
  ~poLCA(
    lca_func,
    .x[, -1],
    nclass = 3,
    nrep = 10
  )
)

lca[[1]]

saveRDS(lca, here::here("projects/nhanes/lca_model_10indicators.rds"))

prob <- cat_map(lca, ~reshape2::melt(.x$probs, level = 2))

prob

plot <- 
  cat_map(
    prob,
    ~ .x |>
      mutate(
        Var2 = case_when(
          Var2 == "Pr(1)" ~ "Normal",
          Var2 == "Pr(2)" ~ "Abnormal"
        ),
        Var2 = as.factor(Var2),
        Var2 = fct_relevel(
          Var2,
          "Normal",
          "Abnormal"
        ),
        L2 = as.factor(L2),
        L2 = fct_relevel(
          L2,
          "albumin_bi",
          "alp_bi",
          "alt_bi",
          "ast_bi",
          "ggt_bi",
          "hdl_bi",
          "ldl_bi",
          "bilirubin_bi",
          "total_chol_bi",
          "trigly_bi"
        )
      ) |>
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
      #geom_hline(
      #  yintercept = .7,
      #  linetype = 2
      #  ) +
      coord_flip() +
      facet_wrap(
        ~Var1
      ) +
      scale_y_continuous(
        limits = c(0, 1.05),
        breaks = seq(0, 1, .1)
      ) +
      see::scale_fill_okabeito() +
      theme_light()
  )

# new_prob_start <-
#   list(
#     poLCA.reorder(lca[[1]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[2]]$probs.start, c(3, 1, 2)),
#     poLCA.reorder(lca[[3]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[4]]$probs.start, c(2, 3, 1)),
#     poLCA.reorder(lca[[5]]$probs.start, c(1, 2, 3)),
#     poLCA.reorder(lca[[6]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[7]]$probs.start, c(2, 3, 1)),
#     poLCA.reorder(lca[[8]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[9]]$probs.start, c(3, 1, 2)),
#     poLCA.reorder(lca[[10]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[11]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[12]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[13]]$probs.start, c(2, 3, 1)),
#     poLCA.reorder(lca[[14]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[15]]$probs.start, c(3, 1, 2)),
#     poLCA.reorder(lca[[16]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[17]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[18]]$probs.start, c(1, 2, 3)),
#     poLCA.reorder(lca[[19]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[20]]$probs.start, c(1, 2, 3)),
#     poLCA.reorder(lca[[21]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[22]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[23]]$probs.start, c(2, 3, 1)),
#     poLCA.reorder(lca[[24]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[25]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[26]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[27]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[28]]$probs.start, c(2, 3, 1)),
#     poLCA.reorder(lca[[29]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[30]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[31]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[32]]$probs.start, c(2, 3, 1)),
#     poLCA.reorder(lca[[33]]$probs.start, c(3, 1, 2)),
#     poLCA.reorder(lca[[34]]$probs.start, c(2, 3, 1)),
#     poLCA.reorder(lca[[35]]$probs.start, c(1, 2, 3)),
#     poLCA.reorder(lca[[36]]$probs.start, c(3, 1, 2)),
#     poLCA.reorder(lca[[37]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[38]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[39]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[40]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[41]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[42]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[43]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[44]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[45]]$probs.start, c(3, 1, 2)),
#     poLCA.reorder(lca[[46]]$probs.start, c(3, 1, 2)),
#     poLCA.reorder(lca[[47]]$probs.start, c(1, 2, 3)),
#     poLCA.reorder(lca[[48]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[49]]$probs.start, c(2, 3, 1)),
#     poLCA.reorder(lca[[50]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[51]]$probs.start, c(1, 2, 3)),
#     poLCA.reorder(lca[[52]]$probs.start, c(1, 2, 3)),
#     poLCA.reorder(lca[[53]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[54]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[55]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[56]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[57]]$probs.start, c(1, 3, 2)),
#     poLCA.reorder(lca[[58]]$probs.start, c(2, 1, 3)),
#     poLCA.reorder(lca[[59]]$probs.start, c(3, 2, 1)),
#     poLCA.reorder(lca[[60]]$probs.start, c(2, 1, 3))
# )

# new_prob_start

# set.seed(12345)
# lca_new_start <- cat_map(
#   1:length(lca),
#   ~poLCA(
#     lca_func,
#     indicators[[.x]][, -1],
#     nclass = 3,
#     nrep = 10,
#     probs.start = new_prob_start[[.x]]
#   )
# )

# lca_new_start[[10]]

# saveRDS(lca_new_start, here::here("projects/nhanes/lca_w_new_starting points.rds"))


# prob_loop <- cat_map(lca_new_start, ~reshape2::melt(.x$probs, level = 2))

# prob_loop

# plot_loop <- 
#   cat_map(
#     prob_loop,
#     ~.x |>
#       mutate(
#         Var2 = case_when(
#           Var2 == "Pr(1)" ~ "Normal",
#           Var2 == "Pr(2)" ~ "Abnormal"
#         ),
#         Var2 = as.factor(Var2),
#         Var2 = fct_relevel(
#           Var2,
#           "Normal",
#           "Abnormal"
#         ),
#         L2 = as.factor(L2),
#         L2 = fct_relevel(
#           L2,
#           "albumin_bi",
#           "alp_bi",
#           "alt_bi",
#           "ast_bi",
#           "ggt_bi",
#           "hdl_bi",
#           "ldl_bi",
#           "bilirubin_bi",
#           "total_chol_bi",
#           "trigly_bi"
#         )
#       ) |>
#       ggplot(
#         aes(
#           L2,
#           value,
#           fill = Var2
#         )
#       ) +
#       geom_bar(
#         stat = "identity",
#         position = "stack"
#       ) +
#       #geom_hline(
#       #  yintercept = .7,
#       #  linetype = 2
#       #  ) +
#       coord_flip() +
#       facet_wrap(
#         ~Var1
#       ) +
#       scale_y_continuous(
#         limits = c(0, 1.05),
#         breaks = seq(0, 1, .1)
#       ) +
#       see::scale_fill_okabeito() +
#       theme_light()
#   )

# plot_loop <- cat_map(
#   prob_loop,
#   ~.x |>
#   mutate(
#     Var2 = case_when(
#       Var2 == "Pr(1)" ~ "Normal",
#       Var2 == "Pr(2)" ~ "Low",
#       Var2 == "Pr(3)" ~ "High",
#     ),
#     L2 = as.factor(L2),
#     L2 = fct_relevel(
#       L2,
#       "albumin_bi",
#       "alp_bi",
#       "alt_bi",
#       "ast_bi",
#       "ggt_bi",
#       "hdl_bi",
#       "ldl_bi",
#       "bilirubin_bi",
#       "total_chol_bi",
#       "trigly_bi"
#     )
#   ) |>
#     ggplot(
#       aes(
#         L2,
#         value,
#         fill = Var2
#       )
#     ) +
#     geom_col(
#       position = position_dodge(),
#       color = "black"
#     ) +
#     #coord_flip() +
#       facet_wrap(
#         ~Var1,
#         ncol = 1
#       ) +
#       scale_y_continuous(
#         limits = c(0, 1.05),
#         breaks = seq(0, 1, .1)
#       ) +
#       see::scale_fill_okabeito() +
#       theme_light()
# )

plot_loop[[1]]

# use total cholesterol for checking group 1 (healthy), group 2 (slightly unhealthy)
# use ast and alt for group 3 (unhealthy)

lca[[1]]$probs$total_chol_bi
lca

lca_new_start[[1]]$probs$albumin_bi

lca_new_start[[1]]$probs$albumin_bi

lca_new_start[[1]]$probs$albumin_bi[1,]

cat_map(1:60, ~lca_new_start[[.x]]$probs$albumin_bi[1,])

map_dfr(1:60, ~lca[[.x]]$probs$albumin_bi[,1] |> as_tibble())

lca_new_start[[1]]$probs |> str()



# tibble(
#   lat_class = rep(1:3, 60),
#   imp = rep(1:60, each = 3),
#   albumin_norm = map_dfr(1:60, ~lca[[.x]]$probs$albumin_bi[,1] |> as_tibble()),
#   albumin_low = map_dfr(1:60, ~lca[[.x]]$probs$albumin_bi[,2] |> as_tibble()),
#   alp_norm = map_dfr(1:60, ~lca[[.x]]$probs$alp_bi[,1] |> as_tibble()),
#   alp_low = map_dfr(1:60, ~lca[[.x]]$probs$alp_bi[,2] |> as_tibble()),
#   alp_high = map_dfr(1:60, ~lca[[.x]]$probs$alp_bi[,3] |> as_tibble()),
#   alt_norm = map_dfr(1:60, ~lca[[.x]]$probs$alt_bi[,1] |> as_tibble()),
#   alt_high = map_dfr(1:60, ~lca[[.x]]$probs$alt_bi[,3] |> as_tibble()),
#   ast_norm = map_dfr(1:60, ~lca[[.x]]$probs$ast_bi[,1] |> as_tibble()),
#   ast_high = map_dfr(1:60, ~lca[[.x]]$probs$ast_bi[,3] |> as_tibble()),
#   ggt_norm = map_dfr(1:60, ~lca[[.x]]$probs$ggt_bi[,1] |> as_tibble()),
#   ggt_high = map_dfr(1:60, ~lca[[.x]]$probs$ggt_bi[,3] |> as_tibble()),
#   hdl_norm = map_dfr(1:60, ~lca[[.x]]$probs$hdl_bi[,1] |> as_tibble()),
#   hdl_low = map_dfr(1:60, ~lca[[.x]]$probs$hdl_bi[,2] |> as_tibble()),
#   ldl_norm = map_dfr(1:60, ~lca[[.x]]$probs$ldl_bi[,1] |> as_tibble()),
#   ldl_high = map_dfr(1:60, ~lca[[.x]]$probs$ldl_bi[,3] |> as_tibble()),
#   bilirubin_norm = map_dfr(1:60, ~lca[[.x]]$probs$bilirubin_bi[,1] |> as_tibble()),
#   bilirubin_low = map_dfr(1:60, ~lca[[.x]]$probs$bilirubin_bi[,2] |> as_tibble()),
#   bilirubin_high = map_dfr(1:60, ~lca[[.x]]$probs$bilirubin_bi[,3] |> as_tibble()),
#   total_chol_norm = map_dfr(1:60, ~lca[[.x]]$probs$total_chol_bi[,1] |> as_tibble()),
#   total_chol_high = map_dfr(1:60, ~lca[[.x]]$probs$total_chol_bi[,3] |> as_tibble()),
#   trigly_norm = map_dfr(1:60, ~lca[[.x]]$probs$trigly_bi[,1] |> as_tibble()),
#   trigly_high = map_dfr(1:60, ~lca[[.x]]$probs$trigly_bi[,3] |> as_tibble()),
# ) |> 
#   mutate(
#   across(
#     -c(
#       lat_class,
#       imp
#     ),
#     ~unlist(.x)
#   )
#   ) |> 
#   group_by(
#     lat_class
#   ) |> 
#   summarize(
#     across(
#       -imp,
#       ~mean(.x, na.rm = TRUE)
#     )
#   ) |> 
#   ungroup() |> 
#   pivot_longer(
#     -lat_class
#   ) |> 
#   ggplot(
#     aes(
#       name,
#       value
#     )
#   ) +
#   geom_col(
#     aes(
#       fill = as.factor(lat_class)
#     ),
#     position = position_dodge()
#   ) +
#   geom_hline(
#     yintercept = .5,
#     color = "red",
#     linetype = 2,
#     lwd = 1.25
#   ) +
#   coord_flip() +
#   scale_y_continuous(
#     limits = c(0, 1.05),
#     breaks = seq(0, 1, .1)
#   ) +
#   labs(
#     x = "",
#     y = "probability"
#   ) +
#   see::scale_fill_okabeito() +
#   theme_light() +
#   theme(
#     legend.position = "bottom",
#     axis.text = element_text(
#       size = 14
#     )
#   ) +
#   NULL


class_member <- cat_map(
  1:length(lca_new_start),
  ~lca_new_start[[.x]]$posterior |> 
    as_tibble() |> 
    mutate(
      seqn = indicators[[.x]][,1]
    ) |> 
      pivot_longer(
        -seqn
      ) |> 
      group_by(
        seqn
      ) |> 
      filter(
        value == max(value)
      ) |> 
      ungroup() |> 
      rename(
        latent_class = name
      ) |> 
    mutate(
      latent_class = as.factor(latent_class)
    )
)

class_member[[1]] |> 
  count(seqn) |> 
  filter(
    n > 1
  )

left_join(indicators[[1]], class_member[[1]], by = "seqn")

membership <- 
  cat_map(
  1:length(lca),
  ~left_join(indicators[[.x]], class_member[[.x]], by = "seqn") |> 
    select(
      seqn,
      latent_class,
      prob = value
    )
)

membership[[1]]

model_all <- cat_map(
  1:length(model_all),
  ~left_join(
    model_all[[.x]],
    membership[[.x]],
    by = "seqn"
  )
)

model_all[[1]] |> glimpse()

library(mice)
library(miceadds)
library(nnet)

model_mids <- datlist2mids(model_all)

liver_cond_fit <- with(
  model_mids,
  multinom(
    as.factor(latent_class) ~ age + sex + covered_insurance +
      min_sedentary + vig_rec_pa +
      waist_circumference +
      num_ready_eat_food_30day +
      ever_45_drink_everyday +
      hep_a_anti + hep_b_core_anti + hep_c_rna
  )
)

liver_cond_pool <- pool(liver_cond_fit)

liver_cond_pool
summary(liver_cond_pool)
exp(liver_cond_pool$pooled$estimate)
plogis(liver_cond_pool$pooled$estimate)

glimpse()
















# LPA

lpa_bic <- cat_map(
  indicators,
  ~mclustBIC(.x)
)

# saveRDS(lpa_bic, here::here("projects/nhanes/lpa_model_selection_less_indicators.rds"))

bic_values <- map_dfr(
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


Mclust(indicators[[1]], G = 5) |> summary()

Mclust(indicators[[1]], G = 5)$parameters$mean |> 
  as_tibble() |>  
  mutate(
    across(everything(), ~round(.x, 1)),
    indicator = colnames(indicators[[1]])
  ) |> 
  react_table()

# ast_u_l                # 8-33 u/l
# alt_u_l                # 7-56 u/l
# ggt_u_l                # below 50 u/l
# HDL                    # 60 + = normal
# trigly                 # under 150 = normal
# LDL                    # Under 100 = normal
# Total Cholesterol      # Under 200 = normal

lpa <- cat_map(
  indicators,
  ~Mclust(
    .x,
    G = 2
  )
)

# saveRDS(lpa, here::here("projects/nhanes/lpa_model_more_var.rds"))

# all_profile_mean <- 
  map_dfr(
  1:length(lpa),
  ~lpa[[.x]]$parameters$mean |>
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
    imp = rep(1:60, each = 12)
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
    )

# alt_u_l                # 7-56 u/l
# ast_u_l                # 8-33 u/l
# ggt_u_l                # below 50 u/l
# HDL                    # 60 + = normal
# LDL                    # Under 100 = normal
# Total Cholesterol      # Under 200 = normal
# trigly                 # under 150 = normal




male_ind <- 
  cat_map(
  model_all,
  ~.x |> 
    filter(
      sex == 1
    ) |> 
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

lpa_bic_male <- cat_map(
  male_ind,
  ~mclustBIC(.x)
)

# saveRDS(lpa_bic_male, here::here("projects/nhanes/lpa_model_selection.rds"))

male_bic_values <- 
  map_dfr(
  1:length(lpa_bic_male),
  ~lpa_bic_male[[.x]][1:9, 1:14] |> 
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

plotly::ggplotly(male_bic_values)

Mclust(male_ind[[1]], G = 3)$parameters$mean |> 
  as_tibble() |>  
  mutate(
    across(everything(), ~round(.x, 1)),
    indicator = colnames(male_ind[[1]])
  ) |> 
  react_table()

# ast_u_l                # 8-33 u/l
# alt_u_l                # 7-56 u/l
# ggt_u_l                # below 50 u/l
# HDL                    # 60 + = normal
# LDL                    # Under 100 = normal
# Total Cholesterol      # Under 200 = normal
# trigly                 # under 150 = normal

lpa_male <- cat_map(
  male_ind,
  ~Mclust(
    .x,
    G = 3
  )
)

lpa_male[[1]]$parameters$mean
lpa_male[[1]]$parameters$variance$sigma |> 
  as_tibble()

map_dfr(
  1:length(lpa_male),
  ~lpa_male[[.x]]$parameters$mean |>
  as_tibble() |> 
  mutate(
    indicator = colnames(male_ind[[.x]])
  ) |> 
  relocate(
    indicator, 
    .before = V1
  )
) |> 
  mutate(
    imp = rep(1:60, each = 7)
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
    )




female_ind <- 
  cat_map(
  model_all,
  ~.x |> 
    filter(
      sex == 2
    ) |> 
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

lpa_bic_female <- cat_map(
  female_ind,
  ~mclustBIC(.x)
)

map_dfr(
  1:length(lpa_bic_female),
  ~lpa_bic_female[[.x]][1:9, 1:14] |> 
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
  mutate(
    diff = VVE - lag(VVE, default = first(VVE)),
    diff = round(diff, 2)
  ) |> 
  filter(
    id %in% c(3, 4, 5, 6)
  ) |> 
  react_table()

map_dfr(
  1:length(lpa_bic_female),
  ~lpa_bic_female[[.x]][1:9, 1:14] |> 
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