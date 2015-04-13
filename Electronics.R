# setwd("~/Documents/Uni work/Masters/E4990 MSD/msd_proj")
# Set your own working directory here

elec <- read.csv("Electronics.csv", quote = "",
                 stringsAsFactors=F)

library(dplyr)
library(magrittr)

elec %<>% rename(product_id=product.productId) %>%
    rename(product_name=product.title) %>%
    rename(price=product.price) %>%
    rename(user_id=review.userId) %>%
    select(., -contains("profile")) %>%
    rename(helpful=review.helpfulness) %>%
    rename(user_score=review.score) %>%
    rename(time=review.time) %>%
    rename(summary=review.summary) %>%
    rename(text=review.text)

n <- dim(elec)[1]

strs <- strsplit(elec$helpful, "\\/") %>%
    unlist(.) %>%
    as.numeric(.) %>%
    matrix(., nrow=2, ncol=n) %>%
    t(.)

elec %<>% mutate(., help_score=strs[,1]) %>%
    mutate(., help_total=strs[,2])

