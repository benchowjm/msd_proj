#setwd("~/Documents/Uni work/Masters/E4990 MSD/msd_proj")
# Set your own working directory here

rm(list=ls())
cell <- read.csv("Cellphones.csv", quote = "",
                 stringsAsFactors=F, na.strings="")

library(dplyr)
library(magrittr)
library(scales)
library(ggplot2)

#####
wordcount <- function(x){
    unlist(lapply(strsplit(x, " "), length))    
}


##### Formatting Variables ###

# Extract out text separately #
text <- cell$review.text

### Renaming original variables ###
cell %<>% rename(product_id=product.productId) %>%
    rename(product_name=product.title) %>%
    rename(price=product.price) %>%
    rename(user_id=review.userId) %>%
    dplyr::select(., -contains("profile")) %>%
    rename(helpful=review.helpfulness) %>%
    rename(user_score=review.score) %>%
    rename(time=review.time) %>%
    rename(summary=review.summary) %>%
    mutate(., review.text=NULL)

### Parse out helpfulness scores ###
strs <- strsplit(cell$helpful, "/") %>%
    unlist(.) %>%
    as.numeric(.) %>%
    matrix(., nrow=2, ncol=dim(cell)[1]) %>%
    t(.)

### Helpfulness scores plus a couple of other things ###
cell %<>% mutate(., help_score=strs[,1]) %>%
    mutate(., help_total=strs[,2]) %>%
    mutate(., help_pct=help_score/help_total) %>%
    mutate(., help_agg=2*help_score-help_total) %>%
    mutate(., rev_length=wordcount(text))
rm(strs)

### Products dataset ###
products <- cell %>%
    group_by(product_id, product_name, price) %>%
    summarize(num_reviews=n(), 
              score_1=sum(user_score==1), 
              score_2=sum(user_score==2),
              score_3=sum(user_score==3),
              score_4=sum(user_score==4),
              score_5=sum(user_score==5),
              mean_score=mean(user_score))

### Generate type variables ###
products %<>%
    mutate(., type_headset= regexpr("[Hh]ead\\s*[Ss]et", product_name)>0 | 
                            regexpr("[Hh]ead\\s*[Pp]hone", product_name)>0 |
                            regexpr("[Ee]ar\\s*[Bb]ud", product_name)>0) %>%
    mutate(., type_battery= regexpr("[Cc]harg[ie][rn]", product_name)>0 | 
                            regexpr("[Bb]atter", product_name)>0) %>%
    mutate(., type_covers = regexpr("[Cc]ase", product_name)>0 |
                            regexpr("[Ff]ace\\s*[Pp]late", product_name)>0 |
                            regexpr("[Hh]ol[ds]t*er", product_name)>0) %>% # Separate?
    as.data.frame(.) 

View(filter(products, type_covers==T))
### To see what is going on try:
# x <- c("Headset", "HeadSet", "headset", "head set", "head Set")
# regexpr("Headset", x)
# regexpr("[Hh]eadset", x)
# regexpr("[Hh]ead[Ss]et", x)
# regexpr("[Hh]ead\\s*[Ss]et", x)

### Find products that correspond to more than one type ###
products$dup.type <- products %>%
    dplyr::select(., starts_with("type_")) %>%
    rowSums(.)>1

View(filter(products, dup.type==T))

########## SAVED HERE ##########

##### Visualizations & Summaries #####
# Old measure vs New measure
summary(cell$help_agg)
ggplot(cell,aes(x=help_pct, y=help_agg)) +
    geom_jitter(position=position_jitter(width=0.005, height=0.5), size = 2, alpha = 0.2) +
    coord_cartesian(ylim=c(-50, 100))


# Number of reviews against sd (Can update using 'products' instead)
cell %>%
    group_by(product_id) %>%
    summarize(mean.score=mean(user_score), tot.score=sum(user_score), sd.score=sd(user_score)) %>%
    mutate(num_rev=tot.score/mean.score) %>%
    
ggplot(products, aes(x=num_rev, y=sd.score, color=mean.score)) + 
    geom_point(size=2, shape=16, alpha = 0.8) +
    coord_cartesian(xlim=c(0, 200)) +
    scale_color_gradient(limits=c(2, 4.5), low = "firebrick2", high="navyblue")

### Some other statistics to look into ###
# number of reviews by user (filtered out more than 1)
temp <- cell %>%
    group_by(user_id) %>%
    summarize(user_count=n()) %>%
    filter(., user_count>1) %>%
    left_join(., cell, by = "user_id")

# number of user-product combinations (filtered out more than 1 again)
temp2 <- cell %>%
    group_by(product_name, user_id) %>%
    summarize(prod.user.count=n()) %>%
    filter(., prod.user.count>1)


temp <- cell %>% group_by(help_score, help_total) %>%
    summarize(mean_score=mean(user_score)) %>%
    as.data.frame(.)


##### Text Mining Stuff (To Update) #####
library(tm)
library(Matrix)

text <- Corpus(VectorSource(elec2$text))
text %<>% 
    tm_map(., stripWhitespace) %>%
    tm_map(., content_transformer(tolower)) %>%
    tm_map(., function(x) removePunctuation(x, T)) %>%
    tm_map(., removeNumbers) %>%
    tm_map(., removeWords, stopwords("en")) %>%
    DocumentTermMatrix(.) 
text.matrix <- as.matrix(text) 
dim(text.matrix)

elec2 <- cbind(elec2, text.matrix)
View(head(elec2))

dtm_to_sparse <- function(dtm) {
 sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dims=c(dtm$nrow, dtm$ncol), dimnames=dtm$dimnames)
}

text.sparse <- dtm_to_sparse(text)

save.image("MSD_Proj.Rdata")
