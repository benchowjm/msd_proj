#setwd("~/Documents/Uni work/Masters/E4990 MSD/msd_proj")
# Set your own working directory here

rm(list=ls())
cell <- read.csv("./Cellphones.csv", quote = "",
                 stringsAsFactors=F, na.strings="")

library(dplyr)
library(magrittr)
library(scales)
library(ggplot2)
library(glmnet)
library(tm)
library(SnowballC)

#####
wordcount <- function(x){
    unlist(lapply(strsplit(x, " "), length))    
}

str.to.var <- function(text, strings){
    d <- length(strings)
    n <- length(text)
    words <- matrix(0, nrow=n, ncol=d)
    colnames(words) <- strings
    for(i in 1:d){
        words[,i] <- sapply(text, function(x) length(grep(strings[i], x)))
    }
    out <- rowSums(words)
    return(list(wordcount=words, score=out))
}

num.caps <- function(string){
    temp <- strsplit(string, "(\\s|\\.|\\!|\\,|\\;|\\:|\\/)")
    sapply(temp, function(x) length(grep("^[A-Z]{2,}(\\'|$)", x)))
}

num.exc <- function(string){
    sapply(gregexpr("\\!", string), length)
}

rbinder <- function(results.list){
    n <- length(results.list)
    out <- data.frame()
    for(i in 1:n) out <- rbind(out, results.list[[i]])
    return(out)
}

cbinder <- function(results.list){
    n <- length(results.list)
    out <- results.list[[1]]
    for(i in 2:n) out <- cbind(out, results.list[[i]])
    return(out)
}

cm <- function(model, data, ref){
    confusionMatrix(ifelse(predict(model, data)>0.5, "Not Helpful", "Helpful"), ref)    
}

get_informative_words <- function(crossval, num=20, dictionary=dictionary){
  coefs <- coef(crossval, s="lambda.min")
  coefs <- round(as.data.frame(as.matrix(coefs)), 3)
  names(coefs) <- "weight"
  coefs$word <- row.names(coefs)
  row.names(coefs) <- NULL
  w <- subset(coefs, weight != 0)
  low <- w[with(w, order(weight))[1:num], ]
  high <- w[with(w, order(-weight))[1:num], ]
  low %<>% left_join(., dictionary, by = "word")
  high %<>% left_join(., dictionary, by = "word")
  return(cbind(low, high))
}


accuracy <- function(cm){
    sum(diag(cm))/sum(cm)
}

make.corpus=function(text){
    if(!require("SnowballC")){install.packages("SnowballC")}
    library(SnowballC)
  corpus=VCorpus(VectorSource(text)); print("1/8")
  corpus=tm_map(corpus, tolower); print("2/8")
  corpus=tm_map(corpus,removePunctuation); print("3/8")
  corpus=tm_map(corpus,removeWords, stopwords("english")); print("4/8")
  corpus=tm_map(corpus,stemDocument); print("5/8")
  corpus=tm_map(corpus,stripWhitespace); print("6/8")
  corpus=tm_map(corpus,removeNumbers); print("7/8")
  saveRDS(corpus, "corpus.rds"); print("8/8")
  return(corpus)
}

make.dictionary=function(corpus){
    words=vector("list",length(corpus))
        for(i in 1:length(corpus)){
            words[i]=strsplit(sub("^\\s+", "", corpus[[i]]), " +")
            if(i%%10000==0) print(paste(i,"/",length(corpus)))
        }
    data=unlist(words)
    tabulate.dic=tabulate(factor(data))
    dictionary=unique(data)
    dictionary=sort(dictionary)
    dictionary.df=data.frame(word = dictionary, count = tabulate.dic)
    dictionary.df=arrange(dictionary.df,desc(count))
    return(list(dictionary.df,words))
}

make.dtm=function(dictionary,words,count.level){
    dictionary=dictionary[dictionary$count>count.level,]
    num.infiles=length(words)
    num.words=dim(dictionary)[1]
    dtm=matrix(0,num.infiles,num.words)
    colnames(dtm)=sort(dictionary$word)
    word=data.frame(dictionary$word)
    for (i in 1:num.infiles){
        num.words.infile=length(words[[i]])
        if(i%%100==0) print(paste(i,"/",num.infiles))
        file=table(as.factor(words[[i]]))
        file.dic=data.frame(file)
        dtm[i,]=t(merge(x=word,y=file.dic,by.x="dictionary.word",by.y="Var1",all=F,all.x=T,all.y=F)[,2])
    }
    dtm[is.na(dtm)]=0
    return(dtm)
}
#####
interact <- function(formula, data, col.ndx){
    lmobj <- lm(as.formula(formula), data=data)
    vars <- names(data)[col.ndx]
    d <- length(vars)
    n <- d*(d+1)/2
    out1 <- matrix(0, nrow=n, ncol=4)
    out2 <- as.vector(rep(0, n))
    m <- 1
    for(i in 1:d){
        for(j in 1:i){
            int <- paste(vars[i], vars[j], sep = ":")
            lm.temp <- update(lmobj, as.formula(paste("~ . + ", int, sep = "")))
            z <- dim(summary(lm.temp)$coef)[1]
            out2[m] <- int
            out1[m,] <- summary(lm.temp)$coef[z,]
            m <- m + 1
        }
    }
    out1 <- as.data.frame(out1)
    return(list(results=out1, names=out2))
}

##### Formatting Variables #####

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


### Subsetting stage 1: get out only cellphone-related observations ###
ndx <- grep("phone.*(T-Mobile|Sprint|Verizon|AT&T).*", cell$product_name, ignore.case=T)
phone <- cell[ndx,]
phone.text <- text[ndx]
# Left with 9197 observations #

### Subseting stage 2: Get out only observations with at least 3 helpful votes ###
ndx2 <- phone$help_total>=3
phone <- phone[ndx2,]
phone.text <- phone.text[ndx2]
# Left with 6466 observations #
rm(ndx, ndx2)
# sample size: 9197-2731=6466

apply(phone[,9:13], 2, mean)

##### 1. Create Category Variables #####
dictionary <- list()
dictionary$physical <- c("screen", "-inch", " inch", "size", "width", "thickness", "height", "plate", "color", "colour", "weight", "durable", "power button", "compact", "fits well", "heavy", "clunky")
dictionary$camera <- c("camera", "megapixel", "photo", "front-facing", "selfie", "flash", "image quality", "image resolution", "video record", "record video", "record a video") 
dictionary$software <- c("memory", "storage", "processor", "processing speed", "computing", "operating system", "ios", "software", "slow", "fast")
dictionary$display <- c("display", "screen resolution", "keys", "keyboard", "dial", "bright", "dull", "screensaver", "visibilit", "lighting", " ppi", "lcd", "retina")
dictionary$battery <- c("battery", "charge", "charging", "last long", "lasts long", "dies quickly", "die quickly", "power save")
dictionary$phone <- c("reception", "signal", "static", "sound quality", "call", "speaker", "texting", "text messag", "volume", "loud", "soft", "carrier")
dictionary$web <- c("3G", "4G", "wireless", "wifi", "wi-fi", "internet", "cellular", "web", "browse", "browsing", "connection", "surf", "safari", "chrome")
dictionary$cost <- c("expensive", "value", "cost", "price", "affordable", "cheap", "pricy", "$", "money")
dictionary$easyuse <- c("user-friendl", "user friendl", "convenien", "interface", "intuitive", "naviga", "easy to use", "difficult to use", "ease of use", "hands-free", "handsfree", "hands free", "menu", "drop down", "drop-down", "dropdown", "confusing")
dictionary$apps <- c("game", "application", "media", "ringtone", "apps", "voice recorder", "bluetooth", "you-tube", "youtube", "functions", "functionalit", "mp3", "entertainment", "app store", "media")
dictionary$style <- c("style", "fun", "simple", "sleek", "basic", "stylish", "flexible", "attractive", "ugly", "premier", "good looking", "good-looking", "bad looking", "bad-looking", "appearance")
dictionary$gengood <- c("incredible", "good", "great", "awesome", "fantastic", "nice", "rocks", "best", "impress", "beaut", "love", "pleased")
dictionary$genbad <- c("awful", "crap", "terrible", "horrible", "sucks", "bad", "shit", "hate", "blows", "ugh", "sigh", "irritat", "mistake", "avoid")
dictionary$procon <- c("pros:", "cons:", "pros ", "cons ", "benefit", "drawback", "good point", "bad point")

cat <- list()
cat$physical <- str.to.var(tolower(phone.text), dictionary$physical)[[2]]
cat$camera   <- str.to.var(tolower(phone.text), dictionary$camera)[[2]]
cat$software <- str.to.var(tolower(phone.text), dictionary$software)[[2]]
cat$display  <- str.to.var(tolower(phone.text), dictionary$display)[[2]]
cat$battery  <- str.to.var(tolower(phone.text), dictionary$battery)[[2]]
cat$phone    <- str.to.var(tolower(phone.text), dictionary$phone)[[2]]
cat$web      <- str.to.var(tolower(phone.text), dictionary$web)[[2]]
cat$cost     <- str.to.var(tolower(phone.text), dictionary$cost)[[2]]
cat$easyuse  <- str.to.var(tolower(phone.text), dictionary$easyuse)[[2]]
cat$apps     <- str.to.var(tolower(phone.text), dictionary$apps)[[2]]
cat$style    <- str.to.var(tolower(phone.text), dictionary$style)[[2]]
cat$gengood  <- str.to.var(tolower(phone.text), dictionary$gengood)[[2]]
cat$genbad   <- str.to.var(tolower(phone.text), dictionary$genbad)[[2]]
cat$procon   <- str.to.var(tolower(phone.text), dictionary$procon)[[2]]

categories <- as.data.frame(matrix(unlist(cat), nrow=length(phone.text)))
colnames(categories) <- paste("cat_", names(cat), sep = "")
categories <- 100*categories/phone$rev_length

apply(categories, 2, mean)
##### 2. Create: a) number of caps b) number of ! c) binary variable #####
phone %<>%
    mutate(., num_caps=100*num.caps(phone.text)/rev_length) %>%
    mutate(., num_exc=100*num.exc(phone.text)/rev_length) %>%
    mutate(., helpful=ifelse(help_pct>=0.5, 1, 0))

# Save current dataset #
write.csv(phone, "Cellphone_features.csv", row.names=F)

##### 3. Make dictionary and dtm #####
corpus <- make.corpus(phone.text)
corpus=readRDS("corpus.rds")
temp <- make.dictionary(corpus)
dictionary <- temp[[1]]
words <- temp[[2]]

## Remove words that don't appear at least once in every 20 reviews ##
# Shrunk from 43134 to 429 words #
dict <- make.dtm(dictionary, words, floor(0.05*nrow(phone))) %>%
    as.data.frame(.)
dictionary$word <- paste("word_", dictionary$word, sep = "")
# Prefix columns of word dictionary
colnames(dict) <- paste("word_", colnames(dict), sep = "")
phone %<>% cbind(., categories, dict)
rm(dict, categories, words, temp)

##### Elasticnet Regressions #####
# Set up shells to collect results
glm <- list()
cm <- list()
predictions <- list()

# Choose samples for training set #
set.seed(5)
ndx <- base::sample(nrow(phone), floor(0.25*nrow(phone)))
training <- phone[-ndx,]
testing <- phone[ndx,]

### 6 Models: last 3 are the same as the first 3, but add help_total (total number of helpfulness votes) as an additional predictor ###

### Model 1: Only individual words ###
set.seed(5)
glm[[1]] <- training %>%
    dplyr::select(., starts_with("word_")) %>%
    as.matrix(.) %>%
    cv.glmnet(., training$helpful, family = "binomial", type.measure = "auc", alpha = 1)
predictions[[1]] <- testing %>% dplyr::select(., starts_with("word_")) %>% as.matrix(.) %>%
    predict(glm[[1]], ., type = "class", s=glm[[1]]$lambda.min)
cm[[1]] <- table(predictions[[1]], testing$helpful) 
cm[[1]]
accuracy(cm[[1]])
get_informative_words(glm[[1]], 20, dictionary=dictionary)

### Model 2: Only categories & exclamation marks & review length ###
set.seed(5)
glm[[2]] <- training[,(-1):(-12)] %>%
    dplyr::select(., -starts_with("word_")) %>%
    as.matrix(.) %>%
    cv.glmnet(., training$helpful, family = "binomial", type.measure = "auc", alpha = 1)
predictions[[2]] <- testing[,(-1):(-12)] %>% dplyr::select(., -starts_with("word_")) %>% 
    as.matrix(.) %>%
    predict(glm[[2]], ., type = "class", s=glm[[2]]$lambda.min)
cm[[2]] <- table(predictions[[2]], testing$helpful)
cm[[2]]
accuracy(cm[[2]])
get_informative_words(glm[[2]], 10, dictionary=dictionary)

### Model 3: Use all variables (words + categories) ###
# This is an experimental category - we wondered what would happen if we added in all the variables
set.seed(5)
glm[[3]] <- training[,(-1):(-12)] %>%
    as.matrix(.) %>%
    cv.glmnet(., training$helpful, family = "binomial", type.measure = "auc", alpha = 1)
predictions[[3]] <- testing[,(-1):(-12)] %>% as.matrix(.) %>%
    predict(glm[[3]], ., type = "class", s=glm[[3]]$lambda.min)
cm[[3]] <- table(predictions[[3]], testing$helpful)
cm[[3]]
accuracy(cm[[3]])
get_informative_words(glm[[3]], 20, dictionary=dictionary)

### Model 4: Model 1 + helpfulness total ###
glm[[4]] <- training %>%
    dplyr::select(., starts_with("word_")) %>%
    cbind(., training$help_total) %>%
    as.matrix(.) %>%
    cv.glmnet(., training$helpful, family = "binomial", type.measure = "auc", alpha = 1)
predictions[[4]] <- testing %>% dplyr::select(., starts_with("word_")) %>% 
    cbind(., testing$help_total)  %>% as.matrix(.) %>%
    predict(glm[[4]], ., type = "class", s=glm[[4]]$lambda.min)
cm[[4]] <- table(predictions[[4]], testing$helpful)
cm[[4]]
accuracy(cm[[4]])

### Model 5: Model 2 + helpfulness total ###
set.seed(5)
glm[[5]] <- training[,(-1):(-12)] %>%
    dplyr::select(., -starts_with("word_")) %>%
    cbind(., training$help_total) %>%
    as.matrix(.) %>%
    cv.glmnet(., training$helpful, family = "binomial", type.measure = "auc", alpha = 1)
predictions[[5]] <- testing[,(-1):(-12)] %>% dplyr::select(., -starts_with("word_")) %>% 
    cbind(., testing$help_total)  %>% as.matrix(.) %>%
    predict(glm[[5]], ., type = "class", s=glm[[5]]$lambda.min)
cm[[5]] <- table(predictions[[5]], testing$helpful)
cm[[5]]
accuracy(cm[[5]])
get_informative_words(glm[[5]], 20, dictionary=dictionary)

### Model 6: Use all variables (words + categories) ###
# This is an experimental category - as per model 3 #
set.seed(5)
glm[[6]] <- training[,(-1):(-12)] %>% 
    cbind(., training$help_total) %>% as.matrix(.) %>% 
    cv.glmnet(., training$helpful, family = "binomial", type.measure = "auc", alpha = 1)
predictions[[6]] <- testing[,(-1):(-12)] %>% 
    cbind(., testing$help_total) %>% as.matrix(.) %>% 
    predict(glm[[6]], ., type = "class", s=glm[[6]]$lambda.min)
cm[[6]] <- table(predictions[[6]], testing$helpful)
cm[[6]]
accuracy(cm[[6]])
get_informative_words(glm[[6]], 20, dictionary=dictionary)


### Look at consistency of models ###
results <- matrix(unlist(predictions[4:6]), nrow=nrow(testing)) %>%
    cbind(testing$helpful) %>%
    as.data.frame(.) %>%
    mutate(., score=apply(., 1, function(x) sum(x[1:3]==x[4])))

table(results$score)
# Can see that the majority of observations are either correctly classified or wrongly classified by all three models

## To see the reviews which were wrongly classified by all three specifications
#View(phone.text[ndx][results$score==0])

pred1 <- testing %>% dplyr::select(., starts_with("word_")) %>% 
    cbind(., testing$help_total)  %>% as.matrix(.) %>%
    predict(glm[[4]], ., s=glm[[4]]$lambda.min, type="response") %>%
    prediction(., testing$helpful)
perf1 <- performance(pred1, measure='tpr', x.measure='fpr')
roc_df1 <- data.frame(fpr=unlist(perf1@x.values), tpr=unlist(perf1@y.values))

pred2 <- testing[,(-1):(-12)] %>% dplyr::select(., -starts_with("word_")) %>% 
    cbind(., testing$help_total)  %>% as.matrix(.) %>%
    predict(glm[[5]], ., s=glm[[5]]$lambda.min, type = "response") %>%
    prediction(., testing$helpful)
perf2 <- performance(pred2, measure='tpr', x.measure='fpr')
roc_df2 <- data.frame(fpr=unlist(perf2@x.values), tpr=unlist(perf2@y.values))


# Final Values #
performance(pred1, 'auc')@y.values
performance(pred2, 'auc')@y.values
cm[4:5]
lapply(cm[4:5], accuracy)
500-sum(is.na(get_informative_words(glm[[4]], 500, dictionary)[,1]))-1
500-sum(is.na(get_informative_words(glm[[5]], 500, dictionary)[,1]))-1

### Visualizations ###
theme_set(theme_gray())
ggplot(phone, aes(x=help_score, y=num_caps)) +
    xlab("Helpfulness Score") + ylab("% of words in Caps") +
    geom_point(size=2, color="navyblue") +
    ggtitle("CAPITALIZED WORDS")
ggsave(file="01_Numcaps.pdf", width = 7, height = 7)

ggplot(phone, aes(x=help_score, y=num_exc)) +
    xlab("Helpfulness Score") + 
    geom_point(size=2, color="navyblue") +
    ylab("Ratio of ! to words") +
    ggtitle("!!!!!")
ggsave(file="02_Numexcmks.pdf", width = 7, height = 7)

theme_set(theme_classic())
roc_df1 %>%
    cbind(., Model="Model 1") %>%
    rbind(., cbind(roc_df2, Model="Model 2")) %>%
    mutate(., Model=factor(Model)) %>%
ggplot(., aes(x=fpr, y=tpr, color = Model)) + 
    geom_line() +
    geom_abline(a=1, b=0, linetype = 2) +
    scale_x_continuous(labels=percent, lim=c(0, 1)) +
    scale_y_continuous(labels=percent, lim=c(0, 1)) +
    scale_color_manual(values=c("navyblue", "firebrick")) +
    ggtitle("ROC Curves")
ggsave(file="03_Final_ROC.pdf", width=8, height=6)

write.csv(phone[,1:29], "Cellphone_features.csv", row.names=F)
save.image("MSD_Proj.Rdata")
