
# bayes_parameter_plot <- function(
#   bayes_model,
#   parameter,
#   stat = c("mean", "sd", "q25", "q75", "q10", "q90")
# ){ 
#   if(stat == "mean"){
#     {{bayes_model}} |>
#       group_by(
#         imp
#       ) |> 
#       summarize(
#         variable = mean({{parameter}}),
#         lower10 = mean(!!sym(glue("{parameter}_lower10"))),
#         upper10 = mean(!!sym(glue("{parameter}_upper10")))
#       ) |> 
#       ggplot(
#         aes(
#           imp,
#           variable
#         )
#       ) +
#       geom_point() +
#       geom_errorbar(
#         aes(
#           ymin = lower10,
#           ymax = upper10
#         ),
#         color = "dodgerblue",
#         alpha = .7
#       ) +
#       geom_hline(
#         yintercept = mean({{bayes_model}}[[{{parameter}}]]),
#         linetype = 2
#       ) +
#       coord_flip()
#   }
# }

# bayes_parameter_plot(
#   drink_post_sample_para,
#   profileV2,
#   stat = "mean"
# )


# drink_model_parameters <-
#   map_dfr(
#   drink_model,
#   ~bayestestR::describe_posterior(.x, "mean") |> as_tibble()
# ) |> 
#   janitor::clean_names() |> 
#   mutate(
#     imp = rep(1:100, each = 5),
#     or = exp(mean),
#     prob = plogis(mean)
#   ) |>
#   relocate(
#     imp,
#     .before = parameter
#   ) |> 
#   relocate(
#     or,
#     .after = ci_high
#   ) |> 
#   relocate(
#     prob,
#     .after = or
#   )
# 
# drink_model_parameters |> 
#   ggplot(
#     aes(
#       rhat
#     )
#   ) +
#   geom_histogram(
#     fill = "dodgerblue"
#   ) +
#   geom_vline(
#     xintercept = 1.1,
#     linetype = 2
#   )
# 
# # Average Parameters Across All Imputations
# drink_model_parameters |> 
#   pivot_longer(
#     c(
#       mean,
#       ci_low,
#       ci_high,
#       or,
#       prob
#     )
#   ) |> 
#   group_by(
#     parameter,
#     name
#   ) |> 
#   summarize(
#     value = mean(value)
#   ) |> 
#   pivot_wider(
#     names_from = name,
#     values_from = value
#   ) |> 
#   select(
#     parameter,
#     mean,
#     ci_low,
#     ci_high,
#     or,
#     prob
#   )
# 
# drink_model_parameters |> 
#   select(
#     imp:parameter,
#     mean,
#     ci_low,
#     ci_high
#   ) |>  
#   ggplot(
#     aes(
#       imp,
#       mean
#     )
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = ci_low,
#       ymax = ci_high
#     )
#   ) +
#   geom_point(
#     aes(
#       color = as.factor(parameter)
#     ),
#     size = 3
#   ) +
#   coord_flip() +
#   facet_wrap(
#     vars(parameter),
#     scales = "free"
#   ) +
#   theme(
#     legend.position = "bottom"
#   )
# 
# # Odds Ratio
# drink_model_parameters |> 
#   select(
#     imp:parameter,
#     or
#   ) |>
#   ggplot(
#     aes(
#       imp,
#       or
#     )
#   ) +
#   geom_point(
#     aes(
#       color = as.factor(parameter)
#     ),
#     size = 3
#   ) +
#   geom_hline(
#     yintercept = 1,
#     linetype = 2
#   ) +
#   coord_flip() +
#   facet_wrap(
#     vars(parameter),
#     scales = "free"
#   ) +
#   theme(
#     legend.position = "bottom"
#   )
# 
# # Probability
# drink_model_parameters |> 
#   select(
#     imp:parameter,
#     prob
#   ) |>
#   ggplot(
#     aes(
#       imp,
#       prob
#     )
#   ) +
#   geom_point(
#     aes(
#       color = as.factor(parameter)
#     ),
#     size = 3
#   ) +
#   geom_hline(
#     yintercept = .5,
#     linetype = 2
#   ) +
#   coord_flip() +
#   facet_wrap(
#     vars(parameter),
#     scales = "free"
#   ) +
#   theme(
#     legend.position = "bottom"
#   )