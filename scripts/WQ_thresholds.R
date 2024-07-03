library(tidyverse)
thresh <- read.csv("./data/NETN_WQ_thresholds.csv", na.strings = "NA") |> select(-SiteCode)

NETN_WQ_thresh <- full_join(getSites() |> select(SiteCode, SiteName),
                    thresh,
                    by = "SiteName")
usethis::use_data(NETN_WQ_thresh)
