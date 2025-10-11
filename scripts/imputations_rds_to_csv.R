library(tidyverse)
library(mice)
library(miceadds)

instance <- read_rds(here::here("RDS Files/cardio_cog_indicator_imputation.rds"))

instance

# plot(instance, layout = c(10, 10))
densityplot(instance)
bwplot(instance)
xyplot(instance, hdl_mg_dl ~ ldl_mg_dl)

set.seed(12345)
model_all <- mice::complete(instance, "all")
model_long <- mice::complete(instance, "long")

# write_csv(model_long, here::here("data/nhanes_2011_2013_cardio_cog_imputations_long.csv"))
