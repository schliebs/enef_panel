
## get commune

data$commune <- data$AINS2BIS %>% as.character() %>% as.numeric


# Merge circonscription 

# Circonscriptions: 
library(stringr)
dir()

setwd("C:/Users/Schliebs/OneDrive/5B11/projekte/data")
circ <- read.csv("toxicode_circonscriptions_legislatives.csv",stringsAsFactors = FALSE)

code_all <- circ$communes %>% paste(collapse = "-")
code_all_vector <- code_all %>% strsplit(split = "-") %>% unlist() %>% as.vector() 

unique <- unique(code_all_vector)
unique <- unique  [str_length(unique) > 1 ]


unique_circs <- NA
for (q in 1:length(unique)) {
  
  unique_circs[q] <- paste(circ$code_circonscription[stringr::str_detect(circ$communes,unique[q]) ],collapse = "*>-<*")
  
}

df <- data.frame(unique,unique_circs) %>% mutate(str_length(unique_circs))

# simpliefied: take first always

df2 <- df
df2$unique_circ_short <- df2$unique_circs %>% str_sub(1,5)
df2$circ_unique_final <- df2$unique_circ_short %>% as.character () %>% as.numeric()
df2$commune <- df2$unique %>% as.character() %>%  as.numeric()

df3 <- select(df2,commune,circ_unique_final)

saveRDS(df3,"convertion.rds")
saveRDS(data,"convertion.rds")


data2$circ_unique_final %>% table()


#test
# dataA <- data[data$commune %in% c(33003,33096,33397,33293,78297),]
# dataA$commune
# df3a <- df3 [1:19,]
# data2 <- left_join(dataA,df3a,by = "commune")
# data2$circ_unique_final

data %>% dim()

data$commune %>% .[100]
df2$commune %>% .[1:10]

getwd()