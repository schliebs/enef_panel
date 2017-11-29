## R-Script Marcel Schliebs | Martial Foucault

rm(list=ls())

## loading of packages

library(plyr)
library(tidyverse)
library(haven)
## Helper-Functions and Files

source('R/codebook.R')


#source('syntax/functions.R')

# Own Functions

`%nin%` <- function(x, o) return(!(x %in% o)) # returns opposite of %in%
num <- function(x) {x %>% as.character() %>% as.numeric()}

# helpers

partycolors = c(fn = "brown",
                lr = "dodgerblue",
                ps = "red")

## Load Data

# Vague 1
# path_v1 <- "C:/Users/Schliebs/Dropbox/17_cevipof/enef/data/Vague1/Data/ENEF2017_Vague1_DataStata_TOTAL18253/ENEF2017_Vague1_DataStata_TOTAL.dta"
# vague_1 <- read_dta(file = path_v1,encoding = "UTF-8")
# vague_1$id <- vague_1$ID12 %>% num()


# Vague 8: 
# path_v8 <- "C:/Users/Schliebs/Dropbox/17_cevipof/enef/data/Vague8/Data/ENEF2017_Vague8/ENEF2017_Vague8.dta"
# vague_8 <- read_dta(file = path_v8,encoding = "UTF-8")
# vague_8$id <- vague_8$ID12 %>% num()

# Vague 11: 
# path_v11 <- "C:/Users/Schliebs/Dropbox/17_cevipof/enef/data/Vague11/Data/ENEF2017_Vague 11/ENEF2017_Vague 11.dta"
# vague_11 <- read_dta(file = path_v11,encoding = "UTF-8")
# vague_11$id <- vague_11$ID12 %>% num()

# Vague 11bis: 
#path_v11b <- "C:/Users/Schliebs/Dropbox/17_cevipof/enef/data/Vague11b/Data/ENEF2017_Vague 11bis.dta"
#vague_11b <- read_dta(file = path_v11b,encoding = "UTF-8")
#vague_11b$id <- vague_11b$ID12 %>% num()

# Vague 12bis: 
# path_v12b <- "C:/Users/Schliebs/Dropbox/17_cevipof/enef/data/Vague12b/Data/ENEF2017_Vague 12bis.dta"
# vague_12b <- read_dta(file = path_v12b,encoding = "UTF-8")
# vague_12b$id <- vague_12b$ID12 %>% num()

# Vague 13: 
#path_v13 <- "C:/Users/Schliebs/Dropbox/17_cevipof/enef/data/Vague13/Vague13/Data/ENEF2017_Vague13.dta"
#vague_13 <- read_dta(file = path_v13,encoding = "UTF-8")
#vague_13$id <- vague_13$ID12 %>% num()

# Vague 14: 
path_v14 <- "data/offline/ENEF2017/Vague14/Data/ENEF2017_Vague 14/ENEF2017_Vague 14.dta"
vague_14 <- read_dta(file = path_v14,encoding = "UTF-8")
#vague_14$id <- vague_14$ID12 %>% num()
names(vague_14)

# Vague 15: 
path_v15 <- "data/offline/ENEF2017/Vague15/Data/ENEF2017_Vague 15_Extraction.dta"
vague_15 <- read_dta(file = path_v15,encoding = "UTF-8")
#vague_15$id <- vague_15$ID12 %>% num()
names(vague_15)

# Vague 16: 
path_v16 <- "data/offline/ENEF2017/Vague16/Data/ENEF2017_Vague 16_Extraction.dta"
vague_16 <- read_dta(file = path_v16,encoding = "UTF-8")
#vague_16$id <- vague_16$ID12 %>% num()

data1 <- left_join(vague_14,vague_15,by = "ID12")
data <- left_join(data1,vague_16,by = "ID12")

rm(vague_14)
rm(vague_15)
rm(vague_16)
rm(data1)


names(data)
# Codebooks

code_wide <- make_dictionary(data,format = "wide")
code_long <- make_dictionary(data,format = "long")

######### Weights: 
# library(survey)
# 
# Weight alt
# data$weight <- data$LBPPUB
# data$weight2 <- data$LBPPUB4
# data$weight_sd <- data$LBPSDD
# data$weight_ID <- data$LBPSDPRCST


# Weight final vague
# library(ggplot2)
# ggplot() + geom_point(aes(x = data$LBPPUB4,
#                           y = data$MPPUB)) + 
#   geom_line(aes(x = c(0,4),y = c(0,4)), col = "red")
# 
# 
# ggplot( ) + 
#   geom_density(aes(x = data$MPPUB), col = "red")+
#   geom_density(aes(x = data$MPPUB19), col = "green")+
#   geom_density(aes(x = data$MPSDD), col = "blue")+
#   geom_density(aes(x = data$MPSDPRCST), col = "orange")
# 
# table(is.na(data$MPPUB))
# 
# ggplot(data = data ) + 
#   geom_density(aes(x = MPPUB,col = factor(as.numeric(MQ1O)) ))
# 
# 
# data$weight_FINAL <- data$MPPUB

#   
# small.w <- svydesign(ids = ~1, data = data [!is.na(data$weight),], weights = data$weight[!is.na(data$weight)])
# 
# 

# ### 
# 
# svytable(~vote12bis,design = small.w) %>% prop.table()
# 
# KPSD
# KPSDD
# KPSDP
# KPSDPR
# KPSDDPR
# KPSDPRPS
# KPIPSD
# KPSDPRCST




#########




# Experimentations

data$HPART %>% table()
data$HVOTSTRAT1 %>% table()  # OK 100 is the control group apparently

a <- data %>% arrange(desc(HPART)) %>% select(HPART,HX1,HY1,HZ1,HVOTSTRAT1A,HX2,HY2,HZ2,HVOTSTRAT1B,HVOTSTRAT1C,HVOTSTRAT1D)
b <- data %>% arrange(desc(HVOTSTRAT1)) %>% select(HVOTSTRAT1,HX1,HY1,HZ1,HVOTSTRAT1A,HX2,HY2,HZ2,HVOTSTRAT1B,HVOTSTRAT1C,HVOTSTRAT1D) # grouping (identical values)


# Grouping id (1)

data$HPART %>% table(useNA = "always")
data$exp_id1 <- data$HPART
data$exp_id1 [is.nan(data$HPART)] <- NA

data$exp_id1 %>% table(useNA = "always")
data$exp_id1 %>% table() %>% prop.table()


# Grouping id (2)

data$HVOTSTRAT1 %>% table(useNA = "always")
data$exp_id2 <- data$HVOTSTRAT1
data$exp_id2 [is.nan(data$HVOTSTRAT1)] <- NA


data$exp_id2 %>% table(useNA = "always")
data$exp_id2 %>% table() %>% prop.table()


# X1. Résultats attendus pour le 1er tour - Score Candidat PS

data$HX1 %>% table(useNA = "always")
data$ps1 <- data$HX1
data$ps1 [is.nan(data$HX1)] <- NA

# Y1. Résultats attendus pour le 1er tour - Score Candidat LR

data$HY1 %>% table(useNA = "always")
data$lr1 <- data$HY1
data$lr1 [is.nan(data$HY1)] <- NA


# Z1. Résultats attendus pour le 1er tour - Score Candidat FN

data$HZ1 %>% table(useNA = "always")
data$fn1 <- data$HZ1
data$fn1 [is.nan(data$HZ1)] <- NA


# X2. Résultats attendus pour le 2nd tour Hyp. PS-LR - Score Candidat PS

data$HX2 %>% table(useNA = "always")
data$ps_vs_lr2 <- data$HX2
data$ps_vs_lr2 [is.nan(data$HX2)] <- NA


# Y2. Résultats attendus pour le 2nd tour Hyp. LR-FN - Score Candidat LR

data$HY2 %>% table(useNA = "always")
data$lr_vs_fn2 <- data$HY2
data$lr_vs_fn2 [is.nan(data$HY2)] <- NA


# Z2. Résultats attendus pour le 2nd tour Hyp. FN-PS - Score Candidat FN

data$HZ2 %>% table(useNA = "always")
data$fn_vs_ps2 <- data$HZ2
data$fn_vs_ps2 [is.nan(data$HZ2)] <- NA


#VOTSTRAT1a. Résultats attendus pour le 1er tour : PS - LR - FN

data$HVOTSTRAT1A %>% table(useNA = "always")
data$sondage1_all <- data$HVOTSTRAT1A
data$sondage1_all [is.nan(data$HVOTSTRAT1A)] <- NA
data$sondage1_all [data$HVOTSTRAT1A %in% " "] <- NA

data$sondage1_all %>% table(useNA = "always")


#VOTSTRAT1b. Résultats attendus pour le 2nd tour : PS - LR

data$HVOTSTRAT1B %>% table(useNA = "always")
data$sondage2_pslr <- data$HVOTSTRAT1B
data$sondage2_pslr [is.nan(data$HVOTSTRAT1B)] <- NA
data$sondage2_pslr [data$HVOTSTRAT1B %in% " "] <- NA


#VOTSTRAT1C. Résultats attendus pour le 2nd tour : LR - FN

data$HVOTSTRAT1C %>% table(useNA = "always")
data$sondage2_lrfn <- data$HVOTSTRAT1C
data$sondage2_lrfn [is.nan(data$HVOTSTRAT1C)] <- NA
data$sondage2_lrfn [data$HVOTSTRAT1C %in% " "] <- NA

#VOTSTRAT1D. Résultats attendus pour le 2nd tour : FN - PS

data$HVOTSTRAT1D %>% table(useNA = "always")
data$sondage2_fnps <- data$HVOTSTRAT1D
data$sondage2_fnps [is.nan(data$HVOTSTRAT1D)] <- NA
data$sondage2_fnps [data$HVOTSTRAT1D %in% " "] <- NA

#STRAT1A. Imaginez qu'une élection ait lieu dans votre circonscription où seuls 3 candidats sont en concurrence au 1er tour : un candidat du PS, un candidat LR (ex-UMP), un candidat du FN. Dans cette hypothèse, quel serait votre choix au 1er tour ?
# Control Group 

data$HSTRATT1A %>% table(useNA = "always")
data$vote_control <- c()
data$vote_control [data$HSTRATT1A %in% c(1)] <- "PS"
data$vote_control [data$HSTRATT1A %in% c(2)] <- "LR"
data$vote_control [data$HSTRATT1A %in% c(3)] <- "FN"
data$vote_control [data$HSTRATT1A %in% c(4)] <- "Blanc/Nul"
data$vote_control [data$HSTRATT1A %in% c(5)] <- "Abstention"
data$vote_control [is.nan(data$HSTRATT1A)] <- NA


data$vote_control %>% table(useNA = "always")
data$vote_control %>% table() %>% prop.table() %>% round(2)


#STRAT1B. Imaginez [...]. Tous les sondages d'intentions de vote [...]. Dans cette hypothèse, quel serait votre choix au 1er tour ?

data$HSTRATT1B %>% table(useNA = "always")
data$vote_stimulus <- c()
data$vote_stimulus [data$HSTRATT1B %in% c(1)] <- "PS"
data$vote_stimulus [data$HSTRATT1B %in% c(2)] <- "LR"
data$vote_stimulus [data$HSTRATT1B %in% c(3)] <- "FN"
data$vote_stimulus [data$HSTRATT1B %in% c(4)] <- "Blanc/Nul"
data$vote_stimulus [data$HSTRATT1B %in% c(5)] <- "Abstention"
data$vote_stimulus [is.nan(data$HSTRATT1B)] <- NA


data$vote_stimulus %>% table(useNA = "always") 
data$vote_stimulus %>% table() %>% prop.table() %>% round(2)


## Merging of Vote into one variable: 

# id-groupingvariable

data$group <- c()
data$group [data$HVOTSTRAT1 == 100] <- "Control_Group"
data$group [data$HVOTSTRAT1 != 100] <- "Stimulus_Group"

data$group_num <- NA
data$group_num [data$group %in% "Control_Group" ] <- 0
data$group_num [data$group %in% "Stimulus_Group" ] <- 1

data$group_num %>% table()

table(data$vote_stimulus,data$group)
table(data$vote_control,data$group)

data$vote_experiment <- c()
data$vote_experiment [data$group %in% c("Control_Group")] <- data$vote_control [data$group %in% c("Control_Group")]
data$vote_experiment [data$group %in% c("Stimulus_Group")] <- data$vote_stimulus [data$group %in% c("Stimulus_Group")]

data$vote_experiment %>% table(data$group) # Check with above: new variable successful

# Vague1

# Vote 1 Tour 2012
data$APRES1 %>% table(useNA = "always")

data$vote_p2012_1 <- c()
data$vote_p2012_1 [data$APRES1 %in% c(1)] <- "Arthaud"
data$vote_p2012_1 [data$APRES1 %in% c(2)] <- "Poutou"
data$vote_p2012_1 [data$APRES1 %in% c(3)] <- "Melenchon"
data$vote_p2012_1 [data$APRES1 %in% c(4)] <- "Hollande"
data$vote_p2012_1 [data$APRES1 %in% c(5)] <- "Joly"
data$vote_p2012_1 [data$APRES1 %in% c(6)] <- "Bayrou"
data$vote_p2012_1 [data$APRES1 %in% c(7)] <- "Sarkozy"
data$vote_p2012_1 [data$APRES1 %in% c(8)] <- "Dupont-Aignan"
data$vote_p2012_1 [data$APRES1 %in% c(9)] <- "Le Pen"
data$vote_p2012_1 [data$APRES1 %in% c(10)] <- "Cheminade"
data$vote_p2012_1 [data$APRES1 %in% c(11)] <- "Blanc/Nul"
data$vote_p2012_1 [data$APRES1 %in% c(12)] <- "Abstention"
data$vote_p2012_1 [is.nan(data$APRES1)] <- NA

data$vote_p2012_1 %>% table(useNA = "always")
data$vote_p2012_1 %>% table() %>% prop.table()

# Vote 2nd Tour 2012
data$APRES2 %>% table(useNA = "always")

data$vote_p2012_2 <- c()
data$vote_p2012_2 [data$APRES2 %in% c(1)] <- "Hollande"
data$vote_p2012_2 [data$APRES2 %in% c(2)] <- "Sarkozy"
data$vote_p2012_2 [data$APRES2 %in% c(3)] <- "Blanc/Nul"
data$vote_p2012_2 [data$APRES2 %in% c(4)] <- "Abstention"
data$vote_p2012_2 [is.nan(data$APRES2)] <- NA

data$vote_p2012_2 %>% table(useNA = "always")
data$vote_p2012_2 %>% table() %>% prop.table()


# Q10 Parti Proche

data$proche_v1 <- c()
data$proche_v1 [data$AQ10 %in% c(1)] <- "Lutte Ouvriere"
data$proche_v1 [data$AQ10 %in% c(2)] <- "Nuveau Parti Anticapitaliste"
data$proche_v1 [data$AQ10 %in% c(3)] <- "Parti Communiste Francaise"
data$proche_v1 [data$AQ10 %in% c(4)] <- "Parti de Gauche"
data$proche_v1 [data$AQ10 %in% c(5)] <- "Nouvelle Donne"
data$proche_v1 [data$AQ10 %in% c(6)] <- "Parti Socialiste"
data$proche_v1 [data$AQ10 %in% c(7)] <- "Parti radical de gauche"
data$proche_v1 [data$AQ10 %in% c(8)] <- "Europe Ecologie - Les verts"
data$proche_v1 [data$AQ10 %in% c(9)] <- "Autre Ecologie"
data$proche_v1 [data$AQ10 %in% c(10)] <- "Le MoDem"
data$proche_v1 [data$AQ10 %in% c(11)] <- "UDI"
data$proche_v1 [data$AQ10 %in% c(12)] <- "Les Republicains"
data$proche_v1 [data$AQ10 %in% c(13)] <- "Debout la France"
data$proche_v1 [data$AQ10 %in% c(14)] <- "Front National"
data$proche_v1 [data$AQ10 %in% c(15)] <- "Autre Parti"
data$proche_v1 [is.nan(data$AQ10)] <- NA

data$proche_v1 %>% table(useNA = "always")
data$proche_v1 %>% table() %>% prop.table()

# PS LEAD
data$ps1_lead <- c()
data$ps1_lead [data$ps1 > data$lr1 & data$ps1 > data$fn1 ] <- 1
data$ps1_lead [data$ps1 <= data$lr1 | data$ps1 <= data$fn1 ] <- 0

data$ps1_lead %>% table(useNA = "always")

# LR Lead
data$lr1_lead <- c()
data$lr1_lead [data$lr1 > data$ps1 & data$lr1 > data$fn1 ] <- 1
data$lr1_lead [data$lr1 <= data$ps1 | data$lr1 <= data$fn1 ] <- 0

data$lr1_lead %>% table(useNA = "always")


# FN LEAD

data$fn1_lead <- c()
data$fn1_lead [data$fn1 > data$ps1 & data$fn1 > data$lr1 ] <- 1
data$fn1_lead [data$fn1 <= data$ps1 | data$fn1 <= data$lr1 ] <- 0

data$fn1_lead %>% table(useNA = "always")

# FN LEAD

data$tie1 <- c()
data$tie1 [data$ps1_lead == 1 | data$lr1_lead == 1 | data$fn1_lead ==1] <- 0
data$tie1 [data$ps1_lead != 1 & data$lr1_lead != 1 & data$fn1_lead !=1] <- 1

data$tie1 %>% table(useNA = "always")

# Experimentation 



## EXPERIMENTATION 1: 

data$vote_exp1 <- NA
data$vote_exp1 [data$AEXP1 %in% c(1)] <- "PS"
data$vote_exp1 [data$AEXP1 %in% c(2)] <- "LR"
data$vote_exp1 [data$AEXP1 %in% c(3)] <- "FN"
data$vote_exp1 [data$AEXP1 %in% c(4)] <- "Blanc/Nul"
data$vote_exp1 [data$AEXP1 %in% c(5)] <- "Abstention"

data$vote_exp1 %>% table(useNA = "always")

data$vote_exp2 <- NA
data$vote_exp2 [data$AEXP2 %in% c(1)] <- "PS"
data$vote_exp2 [data$AEXP2 %in% c(2)] <- "LR"
data$vote_exp2 [data$AEXP2 %in% c(3)] <- "FN"
data$vote_exp2 [data$AEXP2 %in% c(4)] <- "Blanc/Nul"
data$vote_exp2 [data$AEXP2 %in% c(5)] <- "Abstention"

data$vote_exp2 %>% table(useNA = "always")


# Hypotheses Exp 1

data$resultat1 <- NA
data$resultat1 <- data$AEXP2HYP
data$resultat1 [data$AEXP2HYP == " " | is.na(data$AEXP2HYP)] <- NA 
data$resultat1 %>% table()

data$res1_ps <- NA
data$res1_ps <- data$AEXP2SCPS
data$res1_ps [data$AEXP2SCPS == " " | is.na(data$AEXP2SCPS)] <- NA 
data$res1_ps %>% table()

data$res1_lr <- NA
data$res1_lr <- data$AEXP2SCLR
data$res1_lr [data$AEXP2SCLR == " " | is.na(data$AEXP2SCLR)] <- NA 
data$res1_lr %>% table()

data$res1_fn <- NA
data$res1_fn <- data$AEXP2SCFN
data$res1_fn [data$AEXP2SCFN == " " | is.na(data$AEXP2SCFN)] <- NA 
data$res1_fn %>% table()

####### Droit-Gauche conjoint

# data$gauchedroite <- NA
# data$gauchedroite <- data$CIDEOCONJ 
# data$gauchedroite [data$CIDEOCONJ %in% c(99)] <- NA
# data$gauchedroite [is.nan(data$CIDEOCONJ)] <- NA
# 
# data$gauchedroite %>% table(useNA = "always")
# data$gauchedroite %>% hist()



## Droit-Gauche Partis Vague 1


data$leftright_ps1 <- as.numeric(data$AQ6_0)
data$leftright_ps1 [data$AQ6_0 %in% c(98,99)] <- NA

data$leftright_lr1 <- as.numeric(data$AQ6_1)
data$leftright_lr1 [data$AQ6_1 %in% c(98,99)] <- NA

data$leftright_fn1 <- as.numeric(data$AQ6_2)
data$leftright_fn1 [data$AQ6_2 %in% c(98,99)] <- NA

data$leftright_modem1 <- as.numeric(data$AQ6_3)
data$leftright_modem1 [data$AQ6_3 %in% c(98,99)] <- NA

data$leftright_verts1 <- as.numeric(data$AQ6_4)
data$leftright_verts1 [data$AQ6_4 %in% c(98,99)] <- NA

data$leftright_udi1 <- as.numeric(data$AQ6_5)
data$leftright_udi1 [data$AQ6_5 %in% c(98,99)] <- NA

data$leftright_pcf1 <- as.numeric(data$AQ6_6)
data$leftright_pcf1 [data$AQ6_6 %in% c(98,99)] <- NA

data$leftright_pdgauche1 <- as.numeric(data$AQ6_7)
data$leftright_pdgauche1 [data$AQ6_7 %in% c(98,99)] <- NA

data$leftright_lutte1 <- as.numeric(data$AQ6_8)
data$leftright_lutte1 [data$AQ6_8 %in% c(98,99)] <- NA

data$leftright_debout1 <- as.numeric(data$AQ6_11)
data$leftright_debout1 [data$AQ6_11 %in% c(98,99)] <- NA

data$leftright_self1 <- as.numeric(data$AQ7)
data$leftright_self1[data$AQ6_9 %in% c(98,99)] <- NA

## Droit-Gauche Partis Vague 4

data$leftright_ps4 <- as.numeric(data$DQ6_0)
data$leftright_ps4 [data$DQ6_0 %in% c(98,99)] <- NA

data$leftright_lr4 <- as.numeric(data$DQ6_1)
data$leftright_lr4 [data$DQ6_1 %in% c(98,99)] <- NA

data$leftright_fn4 <- as.numeric(data$DQ6_2)
data$leftright_fn4 [data$DQ6_2 %in% c(98,99)] <- NA

data$leftright_modem4 <- as.numeric(data$DQ6_3)
data$leftright_modem4 [data$DQ6_3 %in% c(98,99)] <- NA

data$leftright_verts4 <- as.numeric(data$DQ6_4)
data$leftright_verts4 [data$DQ6_4 %in% c(98,99)] <- NA

data$leftright_udi4 <- as.numeric(data$DQ6_5)
data$leftright_udi4 [data$DQ6_5 %in% c(98,99)] <- NA

data$leftright_pcf4 <- as.numeric(data$DQ6_6)
data$leftright_pcf4 [data$DQ6_6 %in% c(98,99)] <- NA

data$leftright_pdgauche4 <- as.numeric(data$DQ6_7)
data$leftright_pdgauche4 [data$DQ6_7 %in% c(98,99)] <- NA

data$leftright_lutte4 <- as.numeric(data$DQ6_8)
data$leftright_lutte4 [data$DQ6_8 %in% c(98,99)] <- NA

data$leftright_debout4 <- as.numeric(data$DQ6_11)
data$leftright_debout4 [data$DQ6_11 %in% c(98,99)] <- NA

data$leftright_self4 <- as.numeric(data$DQ7)
data$leftright_self4 [data$DQ7 %in% c(98,99)] <- NA

# Distance 4 

data$distance_fn4 <- data$leftright_self4 - data$leftright_fn4
data$distance_lr4 <- data$leftright_self4 - data$leftright_lr4
data$distance_ps4 <- data$leftright_self4 - data$leftright_ps4


# loss of ties
table(data$distance_fn4 == data$distance_lr4)
table(data$distance_fn4 == data$distance_ps4)
table(data$distance_ps4 == data$distance_lr4)


# Ranking: 

# unique
# data$closest_fn <- ifelse(abs(data$distance_fn4) < pmin(abs(data$distance_lr4) ,abs(data$distance_ps4),na.rm = TRUE),1,0)
# data$closest_lr <- ifelse(abs(data$distance_lr4) < pmin(abs(data$distance_ps4) ,abs(data$distance_fn4),na.rm = TRUE),1,0)
# data$closest_ps <- ifelse(abs(data$distance_ps4) < pmin(abs(data$distance_lr4) ,abs(data$distance_fn4),na.rm = TRUE),1,0)
# 
# data$closest <- NA
# data$closest [data$closest_fn %in% c(1)] <- "FN"
# data$closest [data$closest_lr %in% c(1)] <- "LR"
# data$closest [data$closest_ps %in% c(1)] <- "PS"

distances <- data.frame(FN = abs(data$distance_fn4),LR = abs(data$distance_lr4),PS = abs(data$distance_ps4))
minDistance=t(apply(distances,1,function(x) x==min(x,na.rm=TRUE)))
minDistance2=t(apply(minDistance,1,function(x) x=x/sum(x,na.rm=TRUE)))

data$closest <- (apply(minDistance2,1,function(x) colnames(minDistance2) [which(x ==1)][1]))

maxDistance=t(apply(distances,1,function(x) x==max(x,na.rm=TRUE)))
maxDistance2=t(apply(maxDistance,1,function(x) x=x/sum(x,na.rm=TRUE)))
data$thirdclosest <- (apply(maxDistance2,1,function(x) colnames(maxDistance2) [which(x ==1)][1]))

middleDistance=t(apply(distances,1,function(x) x!=max(x,na.rm=TRUE) & x!=min(x,na.rm=TRUE)))
middleDistance2=t(apply(middleDistance,1,function(x) x=x/sum(x,na.rm=TRUE)))
data$secondclosest <- (apply(middleDistance2,1,function(x) colnames(middleDistance2) [which(x ==1)][1]))


data.frame(data$closest,data$secondclosest,data$thirdclosest)

data$vote <- relevel(factor(data$vote_experiment), ref = "LR")
data$closest <- relevel(factor(data$closest), ref = "LR")

# Experimentation (nacher raus)
table(data$vote,data$closest) %>% prop.table(2) %>% round(2)

data$sincere <- ifelse(data$vote == data$closest,1,0)

table(sincere = data$sincere,stimulus = data$group_num) %>% prop.table(2)

write.csv2(data.frame(data$sincere,
                      data$vote,
                      data$closest,
                      data$secondclosest,
                      data$thirdclosest,
                      data$leftright_self4,
                      data$leftright_fn4,
                      data$leftright_lr4,
                      data$leftright_ps4),"vote_exp2.csv")

# ties maybe noch: 

# Parti Proche: (sentez proche)

data$proche_1 <- NA
data$proche_1 [data$AQ8 %in% c(1)] <- 1
data$proche_1 [data$AQ8 %in% c(2)] <- 0

# Parti moins-eloignee: (if proche non)

data$proche_2 <- NA
data$proche_2 [data$AQ9 %in% c(1)] <- 1
data$proche_2 [data$AQ9 %in% c(2)] <- 0

# Together

data$proche <- NA
data$proche [data$proche_1 == 1 | data$proche_2 == 1] <- 1
data$proche [data$proche_2 == 0] <- 0

data$proche %>% table(useNA = "always")





# Which party

data$parti_proche <- NA
data$parti_proche [data$AQ10 %in% c(1)] <- "lutte"
data$parti_proche [data$AQ10 %in% c(2)] <- "anticapitaliste"
data$parti_proche [data$AQ10 %in% c(3)] <- "pcf"
data$parti_proche [data$AQ10 %in% c(4)] <- "pdgauche"
data$parti_proche [data$AQ10 %in% c(5)] <- "nouvelledonne"
data$parti_proche [data$AQ10 %in% c(6)] <- "ps"
data$parti_proche [data$AQ10 %in% c(7)] <- "radicaldegauche"
data$parti_proche [data$AQ10 %in% c(8)] <- "verts"
data$parti_proche [data$AQ10 %in% c(9)] <- "autresecologies"
data$parti_proche [data$AQ10 %in% c(10)] <- "modem"
data$parti_proche [data$AQ10 %in% c(11)] <- "udi"
data$parti_proche [data$AQ10 %in% c(12)] <- "lr"
data$parti_proche [data$AQ10 %in% c(13)] <- "debout"
data$parti_proche [data$AQ10 %in% c(14)] <- "fn"
data$parti_proche [data$AQ10 %in% c(15)] <- "autre"

data$parti_proche %>% table(useNA = "always")




# Cote d'Amour

# Political Sophistication

# Party du président de la region (Vague1)

data$soph1 <- NA
data$soph1 [data$AR13BM %in% c(1)] <- 1
data$soph1 [data$AR13BM %in% c(2,99)] <- 0

data$soph1 %>% table(useNA = "always")


# Read Verbatims Vague11 

verbatim11 <- read.csv2("C:/Users/Schliebs/Dropbox/17_cevipof/enef/data/Vague11/Data/verbatim11.csv",encoding = "UTF-8")
nrow(verbatim11)

data$id <- data$ID12
verbatim11$id <- verbatim11$X.U.FEFF.ID

data <- left_join(data,verbatim11,by = "id")

library(stringr)
# Extract number from Verbatim

# true if contians augme
data$effectifs <- NA
data$effectifs <- str_detect(string = str_to_lower(as.character(data$SOPO1B)), pattern = "augme") 

# true if contains majoritaire
data$maj <- str_detect(string = str_to_lower(as.character(data$SOPO2B)), pattern = "majoritaire")

# true if guess between 500 and 600
data$rsa_guess <- as.numeric(gsub("([0-9]+).*$", "\\1", as.character(data$SOPO3B)))
data$rsa <- NA
data$rsa [data$KSPLIT3 %in% c(2) & data$rsa_guess >= 500 & data$rsa_guess <= 600] <- 1
data$rsa [data$KSPLIT3 %in% c(2) & (data$rsa_guess <= 500 | data$rsa_guess >= 600)] <- 0


# Effectifs, vague 11 (1 correct, 0 not correct answer)

data$soph_effectifs <- NA 
data$soph_effectifs [data$KSOPO1A %in% c(1,2,4,5)] <- 0
data$soph_effectifs [data$KSOPO1A %in% c(3)] <- 1

# verbatim group
data$soph_effectifs [data$KSPLIT1 %in% c(2) & data$effectifs == TRUE] <- 1

# echelle 0-10, 6-10 counts as right

data$soph_effectifs [data$KSOPO1C %in% c(6:10)] <- 1
data$soph_effectifs [data$KSOPO1C %in% c(0:5,99)] <- 0

data$soph_effectifs %>% table(useNA = "always")

# Scrutin, vague 11 (1 correct, 0 not correct answer)

data$scrutin <- NA 
data$scrutin [data$KSOPO2A %in% c(1,3,4,5)] <- 0
data$scrutin [data$KSOPO2A %in% c(2)] <- 1

# verbatim group
data$scrutin [data$KSPLIT2 %in% c(2) & data$maj == TRUE] <- 1

# echelle 0-10, 6-10 counts as right

data$scrutin [data$KSOPO2C %in% c(0:4)] <- 1
data$scrutin [data$KSOPO2C %in% c(5:10,99)] <- 0

data$scrutin %>% table(useNA = "always")



# RSA, vague 11 (1 correct, 0 not correct answer)

data$rsa <- NA 
data$rsa [data$KSOPO3A %in% c(1,2,4,5)] <- 0
data$rsa [data$KSOPO3A %in% c(3)] <- 1

# verbatim group
data$rsa [data$KSPLIT3 %in% c(2) & data$rsa == TRUE] <- 1

# echelle 0-10, 6-10 counts as right

data$rsa [data$KSOPO2C %in% c(0:4)] <- 1
data$rsa [data$KSOPO2C %in% c(5:10,99)] <- 0

data$rsa %>% table(useNA = "always")


data$right <- rowMeans(data[,c("effectifs","scrutin","rsa")],na.rm = TRUE)
data$right [is.nan(data$right)] <- NA

data$right %>% table(useNA = "always")


# Interet politique

data$interet <- NA
data$interet [data$AQ1 %in% c(99) | is.nan(data$AQ1)] <- NA
data$interet [data$AQ1 %in% c(1) ] <- 4
data$interet [data$AQ1 %in% c(2) ] <- 3
data$interet [data$AQ1 %in% c(3) ] <- 2
data$interet [data$AQ1 %in% c(4) ] <- 1

data$interet %>% table(useNA = "always")

cor.test(data$right,data$interet)

library(hrbrthemes)
library(ggplot2)

ggplot(data = data,aes(x = interet,y = right)) + 
  geom_jitter() + 
  geom_smooth(method = "lm")+
  theme_ipsum(grid = "X")


# Vote presidentielle 

# IMMER: 

# Niveau d'interet


# Intention d'aller voter

data$intention2 <- data$BQ0BIS
data$intention2 [is.nan(data$BQ0BIS) ] <- NA

data$intention3 <- data$CQ0BIS
data$intention3 [is.nan(data$CQ0BIS) ] <- NA

data$intention4 <- data$DQ0BIS
data$intention4 [is.nan(data$DQ0BIS) ] <- NA

data$intention6 <- data$FQ0BIS
data$intention6 [is.nan(data$FQ0BIS) ] <- NA

data$intention8 <- data$HQ0BIS
data$intention8 [is.nan(data$HQ0BIS) ] <- NA

data$intention9 <- data$IQ0BIS
data$intention9 [is.nan(data$IQ0BIS) ] <- NA

data$intention10 <- data$JQ0BIS
data$intention10 [is.nan(data$JQ0BIS) ] <- NA

data$intention11 <- data$KQ0BIS
data$intention11 [is.nan(data$KQ0BIS) ] <- NA

data$intention11b <- data$KBQ0BIS
data$intention11b [is.nan(data$KBQ0BIS) ] <- NA

data$intention11b %>% table(useNA = "always")

# Probabilité d'un jour voter

# Vague 2

data$probLutte2 <- as.numeric(data$BQ2_0)
data$probLutte2 [data$BQ2_0 %in% c(99) | is.nan(data$BQ2_0)] <- NA

data$probNpa2 <- as.numeric(data$BQ2_1)
data$probNpa2 [data$BQ2_1 %in% c(99) | is.nan(data$BQ2_1)] <- NA

data$probPcf2 <- as.numeric(data$BQ2_2)
data$probPcf2 [data$BQ2_2 %in% c(99) | is.nan(data$BQ2_2)] <- NA

data$probPdgauche2 <- as.numeric(data$BQ2_3)
data$probPdgauche2 [data$BQ2_3 %in% c(99) | is.nan(data$BQ2_3)] <- NA

data$probNouvelledonne2 <- as.numeric(data$BQ2_4)
data$probNouvelledonne2 [data$BQ2_4 %in% c(99) | is.nan(data$BQ2_4)] <- NA

data$probVerts2 <- as.numeric(data$BQ2_5)
data$probVerts2 [data$BQ2_5 %in% c(99) | is.nan(data$BQ2_5)] <- NA

data$probPs2 <- as.numeric(data$BQ2_6)
data$probPs2 [data$BQ2_6 %in% c(99) | is.nan(data$BQ2_6)] <- NA

data$probModem2 <- as.numeric(data$BQ2_8)
data$probModem2 [data$BQ2_8 %in% c(99) | is.nan(data$BQ2_8)] <- NA

data$probUpi2 <- as.numeric(data$BQ2_9)
data$probUpi2 [data$BQ2_9 %in% c(99) | is.nan(data$BQ2_9)] <- NA

data$probLr2 <- as.numeric(data$BQ2_10)
data$probLr2 [data$BQ2_10 %in% c(99) | is.nan(data$BQ2_10)] <- NA

data$probDebout2 <- as.numeric(data$BQ2_11)
data$probDebout2 [data$BQ2_11 %in% c(99) | is.nan(data$BQ2_11)] <- NA

data$probFn2 <- as.numeric(data$BQ2_12)
data$probFn2 [data$BQ2_12 %in% c(99) | is.nan(data$BQ2_12)] <- NA


# Vague 8

data$probLutte8 <- as.numeric(data$HQ2_0)
data$probLutte8 [data$HQ2_0 %in% c(99) | is.nan(data$HQ2_0)] <- NA

data$probPcf8 <- as.numeric(data$HQ2_2)
data$probPcf8 [data$HQ2_2 %in% c(99) | is.nan(data$HQ2_2)] <- NA

data$probPdgauche8 <- as.numeric(data$HQ2_3)
data$probPdgauche8 [data$HQ2_3 %in% c(99) | is.nan(data$HQ2_3)] <- NA

data$probVerts8 <- as.numeric(data$HQ2_5)
data$probVerts8 [data$HQ2_5 %in% c(99) | is.nan(data$HQ2_5)] <- NA

data$probPs8 <- as.numeric(data$HQ2_6)
data$probPs8 [data$HQ2_6 %in% c(99) | is.nan(data$HQ2_6)] <- NA

data$probModem8 <- as.numeric(data$HQ2_8)
data$probModem8 [data$HQ2_8 %in% c(99) | is.nan(data$HQ2_8)] <- NA

data$probUpi8 <- as.numeric(data$HQ2_9)
data$probUpi8 [data$HQ2_9 %in% c(99) | is.nan(data$HQ2_9)] <- NA

data$probLr8 <- as.numeric(data$HQ2_10)
data$probLr8 [data$HQ2_10 %in% c(99) | is.nan(data$HQ2_10)] <- NA

data$probDebout8 <- as.numeric(data$HQ2_11)
data$probDebout8 [data$HQ2_11 %in% c(99) | is.nan(data$HQ2_11)] <- NA

data$probFn8 <- as.numeric(data$HQ2_12)
data$probFn8 [data$HQ2_12 %in% c(99) | is.nan(data$HQ2_12)] <- NA

table(data$probFn8, data$probLr8)
# Max Probs

probs <- data.frame(FN = data$probFn8,LR = data$probLr8,PS = data$probPs8)

minProb=t(apply(probs,1,function(x) x==min(x,na.rm=TRUE)))
minProb2=t(apply(minProb,1,function(x) x=x/sum(x,na.rm=TRUE)))
data$minProb <- (apply(minProb2,1,function(x) colnames(minProb2) [which(x != 0)]))
data$minProb_numeric <- (apply(probs,1,function(x) min(x,na.rm=TRUE)))

data$min_ps <- unlist(lapply(X = data$minProb,FUN = function (x) ifelse(any(x == c("PS")),1,0)))
data$min_lr <- unlist(lapply(X = data$minProb,FUN = function (x) ifelse(any(x == c("LR")),1,0)))
data$min_fn <- unlist(lapply(X = data$minProb,FUN = function (x) ifelse(any(x == c("FN")),1,0)))

maxProb=t(apply(probs,1,function(x) x==max(x,na.rm=TRUE)))
maxProb2=t(apply(maxProb,1,function(x) x=x/sum(x,na.rm=TRUE)))
data$maxProb <- (apply(maxProb2,1,function(x) colnames(maxProb2) [which(x != 0)]))
data$maxProb_numeric <- (apply(probs,1,function(x) max(x,na.rm=TRUE)))

data$middleProb_numeric <- (apply(probs,1,function(x) sort(x)[2]))



data$strictly_monotonous2 <- apply(probs,1,FUN = function (x) ifelse(length(unique(x)) == 3 & !any(is.na(x)),1,0))
data$strictly_monotonous2 %>% table(useNA = "always")


#test
test <- data.frame(FN = data$probFn8,
           LR = data$probLr8,
           PS = data$probPs8,
           data$maxProb_numeric,
           data$minProb_numeric,
           data$middleProb_numeric,
           data$strictly_monotonous2)


data$max_ps <- unlist(lapply(X = data$maxProb,FUN = function (x) ifelse(any(x == c("PS")),1,0)))
data$max_lr <- unlist(lapply(X = data$maxProb,FUN = function (x) ifelse(any(x == c("LR")),1,0)))
data$max_fn <- unlist(lapply(X = data$maxProb,FUN = function (x) ifelse(any(x == c("FN")),1,0)))

middleProb=t(apply(probs,1,function(x) x!=max(x,na.rm=TRUE) & x!=min(x,na.rm=TRUE)))
middleProb2=t(apply(middleProb,1,function(x) x=x/sum(x,na.rm=TRUE)))
data$middleProb <- (apply(middleProb2,1,function(x) colnames(middleProb2) [which(x ==1)][1]))
data$middle_ps <- ifelse(data$middleProb %in% c("PS"),1,0)
data$middle_lr <- ifelse(data$middleProb %in% c("LR"),1,0)
data$middle_fn <- ifelse(data$middleProb %in% c("FN"),1,0)

# Delete all with same score for three
data$no_maxprob <- ifelse(data$probPs8 == data$probLr8 & data$probLr8 == data$probFn8,1,0)
data$no_maxprob %>% table(useNA = "always")

# Vote for ONE of 2 preferred parties 
data$vote_1a <- NA
data$vote_1a [data$max_ps == 1 & data$vote == "PS" & data$no_maxprob == 0] <- 1
data$vote_1a [data$max_lr == 1 & data$vote == "LR" & data$no_maxprob == 0] <- 1
data$vote_1a [data$max_fn == 1 & data$vote == "FN" & data$no_maxprob == 0] <- 1

data$vote_1a [data$max_ps == 1 & data$vote != "PS" & data$no_maxprob == 0] <- 0
data$vote_1a [data$max_lr == 1 & data$vote != "LR" & data$no_maxprob == 0] <- 0
data$vote_1a [data$max_fn == 1 & data$vote != "FN" & data$no_maxprob == 0] <- 0

data$vote_1a %>% table(useNA = "always")

# Vote for ONLY Highest preferred party: 

# party if there is only one max preferred
data$maxProb_Unique <- (data$maxProb [ifelse(unlist(lapply(X = data$maxProb, FUN = function(x) length(x))) == 1,1,NA) == 1])
data$maxProb_Unique[sapply(data$maxProb_Unique, is.null)] <- NA
data$maxProb_Unique <- unlist(data$maxProb_Unique)

# sincere vote if only one max
data$vote_1b <- NA
data$vote_1b [data$max_ps == 1 & data$max_fn == 0 & data$max_lr == 0 & data$vote == "PS" & data$no_maxprob == 0] <- 1
data$vote_1b [data$max_lr == 1 & data$max_ps == 0 & data$max_fn == 0 & data$vote == "LR" & data$no_maxprob == 0] <- 1
data$vote_1b [data$max_fn == 1 & data$max_ps == 0 & data$max_lr == 0 & data$vote == "FN" & data$no_maxprob == 0] <- 1

data$vote_1b [data$max_ps == 1 & data$max_fn == 0 & data$max_lr == 0 & data$vote != "PS" & data$no_maxprob == 0] <- 0
data$vote_1b [data$max_lr == 1 & data$max_ps == 0 & data$max_fn == 0 & data$vote != "LR" & data$no_maxprob == 0] <- 0
data$vote_1b [data$max_fn == 1 & data$max_ps == 0 & data$max_lr == 0 & data$vote != "FN" & data$no_maxprob == 0] <- 0

data$vote_1b %>% table(useNA = "always")


# for all cases where not possible via probability proxy-score (vague 6)

write.csv2(data.frame(data$sincere,
                      data$vote,
                      data$minProb,
                      data$secondclosest,
                      data$thirdclosest,
                      data$leftright_self4,
                      data$leftright_fn4,
                      data$leftright_lr4,
                      data$leftright_ps4),"vote_exp2.csv")


# Vague 10

data$probLutte10 <- as.numeric(data$JQ2_0)
data$probLutte10 [data$JQ2_0 %in% c(99) | is.nan(data$JQ2_0)] <- NA

data$probNpa10 <- as.numeric(data$JQ2_1)
data$probNpa10 [data$JQ2_1 %in% c(99) | is.nan(data$JQ2_1)] <- NA

data$probPcf10 <- as.numeric(data$JQ2_2)
data$probPcf10 [data$JQ2_2 %in% c(99) | is.nan(data$JQ2_2)] <- NA

data$probPdgauche10 <- as.numeric(data$JQ2_3)
data$probPdgauche10 [data$JQ2_3 %in% c(99) | is.nan(data$JQ2_3)] <- NA

data$probNouvelledonne10 <- as.numeric(data$JQ2_4)
data$probNouvelledonne10 [data$JQ2_4 %in% c(99) | is.nan(data$JQ2_4)] <- NA

data$probVerts10 <- as.numeric(data$JQ2_5)
data$probVerts10 [data$JQ2_5 %in% c(99) | is.nan(data$JQ2_5)] <- NA

data$probPs10 <- as.numeric(data$JQ2_6)
data$probPs10 [data$JQ2_6 %in% c(99) | is.nan(data$JQ2_6)] <- NA

data$probModem10 <- as.numeric(data$JQ2_8)
data$probModem10 [data$JQ2_8 %in% c(99) | is.nan(data$JQ2_8)] <- NA

data$probUpi10 <- as.numeric(data$JQ2_9)
data$probUpi10 [data$JQ2_9 %in% c(99) | is.nan(data$JQ2_9)] <- NA

data$probLr10 <- as.numeric(data$JQ2_10)
data$probLr10 [data$JQ2_10 %in% c(99) | is.nan(data$JQ2_10)] <- NA

data$probDebout10 <- as.numeric(data$JQ2_11)
data$probDebout10 [data$JQ2_11 %in% c(99) | is.nan(data$JQ2_11)] <- NA

data$probFn10 <- as.numeric(data$JQ2_12)
data$probFn10 [data$JQ2_12 %in% c(99) | is.nan(data$JQ2_12)] <- NA

data$probEnmarche10 <- as.numeric(data$JQ2_13)
data$probEnmarche10 [data$JQ2_13 %in% c(99) | is.nan(data$JQ2_13)] <- NA


data %>% 
  group_by(probFn2) %>% 
  summarise(mean (vote11bis == "Le_Pen",na.rm = TRUE))

# Vague 2

# 2 Hollande/Sarko avec Bayrou

data$vote2_HolSarBay <- NA
data$vote2_HolSarBay [data$BQ1A %in% c(1)] <- "Arthaud"
data$vote2_HolSarBay [data$BQ1A %in% c(2)] <- "Poutou"
data$vote2_HolSarBay [data$BQ1A %in% c(3)] <- "Melenchon"
data$vote2_HolSarBay [data$BQ1A %in% c(4)] <- "Duflot"
data$vote2_HolSarBay [data$BQ1A %in% c(5)] <- "Hollande"
data$vote2_HolSarBay [data$BQ1A %in% c(6)] <- "Bayrou"
data$vote2_HolSarBay [data$BQ1A %in% c(7)] <- "Sarkozy"
data$vote2_HolSarBay [data$BQ1A %in% c(8)] <- "Dupont_Aignan"
data$vote2_HolSarBay [data$BQ1A %in% c(9)] <- "Le_Pen"
data$vote2_HolSarBay [data$BQ1A %in% c(97)] <- "Abstention"
data$vote2_HolSarBay [data$BQ1A %in% c(98)] <- "Blanc_Nul"
data$vote2_HolSarBay [data$BQ1A %in% c(99)] <- NA


# 2 Hollande/Juppe avec Bayrou

data$vote2_HolJupBay <- NA
data$vote2_HolJupBay [data$BQ1B %in% c(1)] <- "Arthaud"
data$vote2_HolJupBay [data$BQ1B %in% c(2)] <- "Poutou"
data$vote2_HolJupBay [data$BQ1B %in% c(3)] <- "Melenchon"
data$vote2_HolJupBay [data$BQ1B %in% c(4)] <- "Duflot"
data$vote2_HolJupBay [data$BQ1B %in% c(5)] <- "Hollande"
data$vote2_HolJupBay [data$BQ1B %in% c(6)] <- "Bayrou"
data$vote2_HolJupBay [data$BQ1B %in% c(7)] <- "Juppe"
data$vote2_HolJupBay [data$BQ1B %in% c(8)] <- "Dupont_Aignan"
data$vote2_HolJupBay [data$BQ1B %in% c(9)] <- "Le_Pen"
data$vote2_HolJupBay [data$BQ1B %in% c(97)] <- "Abstention"
data$vote2_HolJupBay [data$BQ1B %in% c(98)] <- "Blanc_Nul"
data$vote2_HolJupBay [data$BQ1B %in% c(99)] <- NA

# 3 Hollande/Fillon avec Bayrou

data$vote2_HolFilBay <- NA
data$vote2_HolFilBay [data$BQ1C %in% c(1)] <- "Arthaud"
data$vote2_HolFilBay [data$BQ1C %in% c(2)] <- "Poutou"
data$vote2_HolFilBay [data$BQ1C %in% c(3)] <- "Melenchon"
data$vote2_HolFilBay [data$BQ1C %in% c(4)] <- "Duflot"
data$vote2_HolFilBay [data$BQ1C %in% c(5)] <- "Hollande"
data$vote2_HolFilBay [data$BQ1C %in% c(6)] <- "Bayrou"
data$vote2_HolFilBay [data$BQ1C %in% c(7)] <- "Fillon"
data$vote2_HolFilBay [data$BQ1C %in% c(8)] <- "Dupont_Aignan"
data$vote2_HolFilBay [data$BQ1C %in% c(9)] <- "Le_Pen"
data$vote2_HolFilBay [data$BQ1C %in% c(97)] <- "Abstention"
data$vote2_HolFilBay [data$BQ1C %in% c(98)] <- "Blanc_Nul"
data$vote2_HolFilBay [data$BQ1C %in% c(99)] <- NA


# Vague 3


# 3 Hollande/Sarko avec Bayrou

data$vote3_HolSarBay <- NA
data$vote3_HolSarBay [data$CQ1A %in% c(1)] <- "Arthaud"
data$vote3_HolSarBay [data$CQ1A %in% c(2)] <- "Poutou"
data$vote3_HolSarBay [data$CQ1A %in% c(3)] <- "Melenchon"
data$vote3_HolSarBay [data$CQ1A %in% c(4)] <- "Duflot"
data$vote3_HolSarBay [data$CQ1A %in% c(5)] <- "Hollande"
data$vote3_HolSarBay [data$CQ1A %in% c(6)] <- "Bayrou"
data$vote3_HolSarBay [data$CQ1A %in% c(7)] <- "Sarkozy"
data$vote3_HolSarBay [data$CQ1A %in% c(8)] <- "Dupont_Aignan"
data$vote3_HolSarBay [data$CQ1A %in% c(9)] <- "Le_Pen"
data$vote3_HolSarBay [data$CQ1A %in% c(97)] <- "Abstention"
data$vote3_HolSarBay [data$CQ1A %in% c(98)] <- "Blanc_Nul"
data$vote3_HolSarBay [data$CQ1A %in% c(99)] <- NA

# 3 Hollande/Juppe avec Bayrou

data$vote3_HolJupBay <- NA
data$vote3_HolJupBay [data$CQ1B %in% c(1)] <- "Arthaud"
data$vote3_HolJupBay [data$CQ1B %in% c(2)] <- "Poutou"
data$vote3_HolJupBay [data$CQ1B %in% c(3)] <- "Melenchon"
data$vote3_HolJupBay [data$CQ1B %in% c(4)] <- "Duflot"
data$vote3_HolJupBay [data$CQ1B %in% c(5)] <- "Hollande"
data$vote3_HolJupBay [data$CQ1B %in% c(6)] <- "Bayrou"
data$vote3_HolJupBay [data$CQ1B %in% c(7)] <- "Juppe"
data$vote3_HolJupBay [data$CQ1B %in% c(8)] <- "Dupont_Aignan"
data$vote3_HolJupBay [data$CQ1B %in% c(9)] <- "Le_Pen"
data$vote3_HolJupBay [data$CQ1B %in% c(97)] <- "Abstention"
data$vote3_HolJupBay [data$CQ1B %in% c(98)] <- "Blanc_Nul"
data$vote3_HolJupBay [data$CQ1B %in% c(99)] <- NA

# 3 Hollande/Juppe SANS Bayrou

data$vote3_HolJup <- NA
data$vote3_HolJup [data$CQ1D %in% c(1)] <- "Arthaud"
data$vote3_HolJup [data$CQ1D %in% c(2)] <- "Poutou"
data$vote3_HolJup [data$CQ1D %in% c(3)] <- "Melenchon"
data$vote3_HolJup [data$CQ1D %in% c(4)] <- "Duflot"
data$vote3_HolJup [data$CQ1D %in% c(5)] <- "Hollande"
data$vote3_HolJup [data$CQ1D %in% c(6)] <- "Juppe"
data$vote3_HolJup [data$CQ1D %in% c(7)] <- "Dupont_Aignan"
data$vote3_HolJup [data$CQ1D %in% c(8)] <- "Le_Pen"
data$vote3_HolJup [data$CQ1D %in% c(97)] <- "Abstention"
data$vote3_HolJup [data$CQ1D %in% c(98)] <- "Blanc_Nul"
data$vote3_HolJup [data$CQ1D %in% c(99)] <- NA

# Vague 4

# 4 Hollande/Sarko avec Bayrou

data$vote4_HolSarBay <- NA
data$vote4_HolSarBay [data$DQ1A %in% c(1)] <- "Arthaud"
data$vote4_HolSarBay [data$DQ1A %in% c(2)] <- "Poutou"
data$vote4_HolSarBay [data$DQ1A %in% c(3)] <- "Melenchon"
data$vote4_HolSarBay [data$DQ1A %in% c(4)] <- "Duflot"
data$vote4_HolSarBay [data$DQ1A %in% c(5)] <- "Hollande"
data$vote4_HolSarBay [data$DQ1A %in% c(6)] <- "Bayrou"
data$vote4_HolSarBay [data$DQ1A %in% c(7)] <- "Sarkozy"
data$vote4_HolSarBay [data$DQ1A %in% c(8)] <- "Dupont_Aignan"
data$vote4_HolSarBay [data$DQ1A %in% c(9)] <- "Le_Pen"
data$vote4_HolSarBay [data$DQ1A %in% c(10)] <- "Cheminade"
data$vote4_HolSarBay [data$DQ1A %in% c(97)] <- "Abstention"
data$vote4_HolSarBay [data$DQ1A %in% c(98)] <- "Blanc_Nul"
data$vote4_HolSarBay [data$DQ1A %in% c(99)] <- NA

# 4 Hollande/Juppe


data$vote4_HolJup <- NA
data$vote4_HolJup [data$DQ1D %in% c(1)] <- "Arthaud"
data$vote4_HolJup [data$DQ1D %in% c(2)] <- "Poutou"
data$vote4_HolJup [data$DQ1D %in% c(3)] <- "Melenchon"
data$vote4_HolJup [data$DQ1D %in% c(4)] <- "Duflot"
data$vote4_HolJup [data$DQ1D %in% c(5)] <- "Hollande"
data$vote4_HolJup [data$DQ1D %in% c(6)] <- "Juppe"
data$vote4_HolJup [data$DQ1D %in% c(7)] <- "Dupont_Aignan"
data$vote4_HolJup [data$DQ1D %in% c(8)] <- "Le_Pen"
data$vote4_HolJup [data$DQ1D %in% c(9)] <- "Cheminade"
data$vote4_HolJup [data$DQ1D %in% c(97)] <- "Abstention"
data$vote4_HolJup [data$DQ1D %in% c(98)] <- "Blanc_Nul"
data$vote4_HolJup [data$DQ1D %in% c(99)] <- NA


# Vague 6

# 6 Hollande/Sarko avec Bayrou

data$vote6_HolSarBay <- NA
data$vote6_HolSarBay [data$FQ1A %in% c(1)] <- "Arthaud"
data$vote6_HolSarBay [data$FQ1A %in% c(2)] <- "Poutou"
data$vote6_HolSarBay [data$FQ1A %in% c(3)] <- "Melenchon"
data$vote6_HolSarBay [data$FQ1A %in% c(4)] <- "Duflot"
data$vote6_HolSarBay [data$FQ1A %in% c(5)] <- "Hollande"
data$vote6_HolSarBay [data$FQ1A %in% c(6)] <- "Bayrou"
data$vote6_HolSarBay [data$FQ1A %in% c(7)] <- "Sarkozy"
data$vote6_HolSarBay [data$FQ1A %in% c(8)] <- "Dupont_Aignan"
data$vote6_HolSarBay [data$FQ1A %in% c(9)] <- "Le_Pen"
data$vote6_HolSarBay [data$FQ1A %in% c(10)] <- "Cheminade"
data$vote6_HolSarBay [data$FQ1A %in% c(97)] <- "Abstention"
data$vote6_HolSarBay [data$FQ1A %in% c(98)] <- "Blanc_Nul"
data$vote6_HolSarBay [data$FQ1A %in% c(99)] <- NA

# 6 Hollande/Juppe


data$vote6_HolJup <- NA
data$vote6_HolJup [data$FQ1D %in% c(1)] <- "Arthaud"
data$vote6_HolJup [data$FQ1D %in% c(2)] <- "Poutou"
data$vote6_HolJup [data$FQ1D %in% c(3)] <- "Melenchon"
data$vote6_HolJup [data$FQ1D %in% c(4)] <- "Duflot"
data$vote6_HolJup [data$FQ1D %in% c(5)] <- "Hollande"
data$vote6_HolJup [data$FQ1D %in% c(6)] <- "Juppe"
data$vote6_HolJup [data$FQ1D %in% c(7)] <- "Dupont_Aignan"
data$vote6_HolJup [data$FQ1D %in% c(8)] <- "Le_Pen"
data$vote6_HolJup [data$FQ1D %in% c(9)] <- "Cheminade"
data$vote6_HolJup [data$FQ1D %in% c(97)] <- "Abstention"
data$vote6_HolJup [data$FQ1D %in% c(98)] <- "Blanc_Nul"
data$vote6_HolJup [data$FQ1D %in% c(99)] <- NA

# 6 Valls/Juppe

data$vote6_HolSarMacBay <- NA
data$vote6_HolSarMacBay [data$FQ1E %in% c(1)] <- "Arthaud"
data$vote6_HolSarMacBay [data$FQ1E %in% c(2)] <- "Poutou"
data$vote6_HolSarMacBay [data$FQ1E %in% c(3)] <- "Melenchon"
data$vote6_HolSarMacBay [data$FQ1E %in% c(4)] <- "Duflot"
data$vote6_HolSarMacBay [data$FQ1E %in% c(5)] <- "Hollande"
data$vote6_HolSarMacBay [data$FQ1E %in% c(6)] <- "Macron"
data$vote6_HolSarMacBay [data$FQ1E %in% c(7)] <- "Bayrou"
data$vote6_HolSarMacBay [data$FQ1E %in% c(8)] <- "Sarkozy"
data$vote6_HolSarMacBay [data$FQ1E %in% c(9)] <- "Dupont_Aignan"
data$vote6_HolSarMacBay [data$FQ1E %in% c(10)] <- "Le_Pen"
data$vote6_HolSarMacBay [data$FQ1E %in% c(11)] <- "Cheminade"
data$vote6_HolSarMacBay [data$FQ1E %in% c(97)] <- "Abstention"
data$vote6_HolSarMacBay [data$FQ1E %in% c(98)] <- "Blanc_Nul"
data$vote6_HolSarMacBay [data$FQ1E %in% c(99)] <- NA

# 6 Valls/Juppe avec MACRON

data$vote6_ValJupMac <- NA
data$vote6_ValJupMac [data$FQ1F %in% c(1)] <- "Arthaud"
data$vote6_ValJupMac [data$FQ1F %in% c(2)] <- "Poutou"
data$vote6_ValJupMac [data$FQ1F %in% c(3)] <- "Melenchon"
data$vote6_ValJupMac [data$FQ1F %in% c(4)] <- "Duflot"
data$vote6_ValJupMac [data$FQ1F %in% c(5)] <- "Valls"
data$vote6_ValJupMac [data$FQ1F %in% c(6)] <- "Macron"
data$vote6_ValJupMac [data$FQ1F %in% c(7)] <- "Juppe"
data$vote6_ValJupMac [data$FQ1F %in% c(8)] <- "Dupont_Aignan"
data$vote6_ValJupMac [data$FQ1F %in% c(9)] <- "Le_Pen"
data$vote6_ValJupMac [data$FQ1F %in% c(10)] <- "Cheminade"
data$vote6_ValJupMac [data$FQ1F %in% c(97)] <- "Abstention"
data$vote6_ValJupMac [data$FQ1F %in% c(98)] <- "Blanc_Nul"
data$vote6_ValJupMac [data$FQ1F %in% c(99)] <- NA

data$vote6_ValJupMac %>% table() %>% prop.table()



# Vague 8

# 8 Hollande/Sarko avec Bayrou

data$vote8_HolSar <- NA
data$vote8_HolSar [data$HQ1A %in% c(1)] <- "Arthaud"
data$vote8_HolSar [data$HQ1A %in% c(2)] <- "Poutou"
data$vote8_HolSar [data$HQ1A %in% c(3)] <- "Melenchon"
data$vote8_HolSar [data$HQ1A %in% c(4)] <- "Jadot"
data$vote8_HolSar [data$HQ1A %in% c(5)] <- "Hollande"
data$vote8_HolSar [data$HQ1A %in% c(6)] <- "Bayrou"
data$vote8_HolSar [data$HQ1A %in% c(7)] <- "Sarkozy"
data$vote8_HolSar [data$HQ1A %in% c(8)] <- "Dupont_Aignan"
data$vote8_HolSar [data$HQ1A %in% c(9)] <- "Le_Pen"
data$vote8_HolSar [data$HQ1A %in% c(10)] <- "Cheminade"
data$vote8_HolSar [data$HQ1A %in% c(97)] <- "Abstention"
data$vote8_HolSar [data$HQ1A %in% c(98)] <- "Blanc_Nul"
data$vote8_HolSar [data$HQ1A %in% c(99)] <- NA

# 8 Hollande/Juppe


data$vote8_HolJup <- NA
data$vote8_HolJup [data$HQ1D %in% c(1)] <- "Arthaud"
data$vote8_HolJup [data$HQ1D %in% c(2)] <- "Poutou"
data$vote8_HolJup [data$HQ1D %in% c(3)] <- "Melenchon"
data$vote8_HolJup [data$HQ1D %in% c(4)] <- "Jadot"
data$vote8_HolJup [data$HQ1D %in% c(5)] <- "Hollande"
data$vote8_HolJup [data$HQ1D %in% c(6)] <- "Sarkozy"
data$vote8_HolJup [data$HQ1D %in% c(7)] <- "Dupont_Aignan"
data$vote8_HolJup [data$HQ1D %in% c(8)] <- "Le_Pen"
data$vote8_HolJup [data$HQ1D %in% c(9)] <- "Cheminade"
data$vote8_HolJup [data$HQ1D %in% c(97)] <- "Abstention"
data$vote8_HolJup [data$HQ1D %in% c(98)] <- "Blanc_Nul"
data$vote8_HolJup [data$HQ1D %in% c(99)] <- NA

# 8 Valls/Sarko

data$vote8_ValSar <- NA
data$vote8_ValSar [data$HQ1G %in% c(1)] <- "Arthaud"
data$vote8_ValSar [data$HQ1G %in% c(2)] <- "Poutou"
data$vote8_ValSar [data$HQ1G %in% c(3)] <- "Melenchon"
data$vote8_ValSar [data$HQ1G %in% c(4)] <- "Jadot"
data$vote8_ValSar [data$HQ1G %in% c(5)] <- "Valls"
data$vote8_ValSar [data$HQ1G %in% c(6)] <- "Bayrou"
data$vote8_ValSar [data$HQ1G %in% c(7)] <- "Sarkozy"
data$vote8_ValSar [data$HQ1G %in% c(8)] <- "Dupont_Aignan"
data$vote8_ValSar [data$HQ1G %in% c(9)] <- "Le_Pen"
data$vote8_ValSar [data$HQ1G %in% c(10)] <- "Cheminade"
data$vote8_ValSar [data$HQ1G %in% c(97)] <- "Abstention"
data$vote8_ValSar [data$HQ1G %in% c(98)] <- "Blanc_Nul"
data$vote8_ValSar [data$HQ1G %in% c(99)] <- NA

# 8 Valls/Juppe

data$vote8_ValJup <- NA
data$vote8_ValJup [data$HQ1H %in% c(1)] <- "Arthaud"
data$vote8_ValJup [data$HQ1H %in% c(2)] <- "Poutou"
data$vote8_ValJup [data$HQ1H %in% c(3)] <- "Melenchon"
data$vote8_ValJup [data$HQ1H %in% c(4)] <- "Jadot"
data$vote8_ValJup [data$HQ1H %in% c(5)] <- "Valls"
data$vote8_ValJup [data$HQ1H %in% c(6)] <- "Juppe"
data$vote8_ValJup [data$HQ1H %in% c(7)] <- "Dupont_Aignan"
data$vote8_ValJup [data$HQ1H %in% c(8)] <- "Le_Pen"
data$vote8_ValJup [data$HQ1H %in% c(9)] <- "Cheminade"
data$vote8_ValJup [data$HQ1H %in% c(97)] <- "Abstention"
data$vote8_ValJup [data$HQ1H %in% c(98)] <- "Blanc_Nul"
data$vote8_ValJup [data$HQ1H %in% c(99)] <- NA

data$vote8_ValJup %>% table() %>% prop.table()


# 8 Moyen pour candidats partis

# test <- 
#   data %>% 
#   select (vars())


# Vague 9 (MONTEBOURG COMME PROXY HAMON)

data$vote9 <- NA
data$vote9 [data$IQ1N %in% c(1)] <- "Arthaud"
data$vote9 [data$IQ1N %in% c(2)] <- "Poutou"
data$vote9 [data$IQ1N %in% c(3)] <- "Melenchon"
data$vote9 [data$IQ1N %in% c(4)] <- "Jadot"
data$vote9 [data$IQ1N %in% c(5)] <- "Montebourg"
data$vote9 [data$IQ1N %in% c(6)] <- "Pinel"
data$vote9 [data$IQ1N %in% c(7)] <- "Macron"
data$vote9 [data$IQ1N %in% c(8)] <- "Fillon"
data$vote9 [data$IQ1N %in% c(9)] <- "Dupont_Aignan"
data$vote9 [data$IQ1N %in% c(10)] <- "Le_Pen"
data$vote9 [data$IQ1N %in% c(11)] <- "Cheminade"
data$vote9 [data$IQ1N %in% c(97)] <- "Abstention"
data$vote9 [data$IQ1N %in% c(98)] <- "Blanc_Nul"
data$vote9 [data$IQ1N %in% c(99)] <- NA

data$vote9 %>% table() %>% prop.table()


#Vague 10

data$vote10 <- NA
data$vote10 [data$JQ1O %in% c(1)] <- "Arthaud"
data$vote10 [data$JQ1O %in% c(2)] <- "Poutou"
data$vote10 [data$JQ1O %in% c(3)] <- "Melenchon"
data$vote10 [data$JQ1O %in% c(4)] <- "Jadot"
data$vote10 [data$JQ1O %in% c(5)] <- "Hamon"
data$vote10 [data$JQ1O %in% c(6)] <- "Macron"
data$vote10 [data$JQ1O %in% c(7)] <- "Fillon"
data$vote10 [data$JQ1O %in% c(8)] <- "Dupont_Aignan"
data$vote10 [data$JQ1O %in% c(9)] <- "Le_Pen"
data$vote10 [data$JQ1O %in% c(10)] <- "Cheminade"
data$vote10 [data$JQ1O %in% c(97)] <- "Abstention"
data$vote10 [data$JQ1O %in% c(98)] <- "Blanc_Nul"
data$vote10 [data$JQ1O %in% c(99)] <- NA

data$vote10 %>% table() %>% prop.table()

#Vague 11

data$vote11 <- NA
data$vote11 [data$KQ1O %in% c(1)] <- "Arthaud"
data$vote11 [data$KQ1O %in% c(2)] <- "Poutou"
data$vote11 [data$KQ1O %in% c(3)] <- "Melenchon"
data$vote11 [data$KQ1O %in% c(4)] <- "Jadot"
data$vote11 [data$KQ1O %in% c(5)] <- "Hamon"
data$vote11 [data$KQ1O %in% c(6)] <- "Macron"
data$vote11 [data$KQ1O %in% c(7)] <- "Fillon"
data$vote11 [data$KQ1O %in% c(8)] <- "Dupont_Aignan"
data$vote11 [data$KQ1O %in% c(9)] <- "Le_Pen"
data$vote11 [data$KQ1O %in% c(10)] <- "Cheminade"
data$vote11 [data$KQ1O %in% c(97)] <- "Abstention"
data$vote11 [data$KQ1O %in% c(98)] <- "Blanc_Nul"
data$vote11 [data$KQ1O %in% c(99)] <- NA

data$vote11 %>% table() %>% prop.table()


# Vague 11bis

data$vote11bis <- NA
data$vote11bis [data$KBQ1O %in% c(1)] <- "Arthaud"
data$vote11bis [data$KBQ1O %in% c(2)] <- "Poutou"
data$vote11bis [data$KBQ1O %in% c(3)] <- "Melenchon"
data$vote11bis [data$KBQ1O %in% c(4)] <- "Jadot"
data$vote11bis [data$KBQ1O %in% c(5)] <- "Hamon"
data$vote11bis [data$KBQ1O %in% c(6)] <- "Macron"
data$vote11bis [data$KBQ1O %in% c(7)] <- "Fillon"
data$vote11bis [data$KBQ1O %in% c(8)] <- "Dupont_Aignan"
data$vote11bis [data$KBQ1O %in% c(9)] <- "Le_Pen"
data$vote11bis [data$KBQ1O %in% c(10)] <- "Cheminade"
data$vote11bis [data$KBQ1O %in% c(97)] <- "Abstention"
data$vote11bis [data$KBQ1O %in% c(98)] <- "Blanc_Nul"
data$vote11bis [data$KBQ1O %in% c(99)] <- NA


data$vote11bis %>% table() %>% prop.table()


# Vague 12

data$vote12 <- NA
data$vote12 [data$LQ1O %in% c(1)] <- "Arthaud"
data$vote12 [data$LQ1O %in% c(2)] <- "Poutou"
data$vote12 [data$LQ1O %in% c(3)] <- "Melenchon"
data$vote12 [data$LQ1O %in% c(4)] <- "Jadot"
data$vote12 [data$LQ1O %in% c(5)] <- "Hamon"
data$vote12 [data$LQ1O %in% c(6)] <- "Macron"
data$vote12 [data$LQ1O %in% c(7)] <- "Fillon"
data$vote12 [data$LQ1O %in% c(8)] <- "Dupont_Aignan"
data$vote12 [data$LQ1O %in% c(9)] <- "Le_Pen"
data$vote12 [data$LQ1O %in% c(10)] <- "Cheminade"
data$vote12 [data$LQ1O %in% c(97)] <- "Abstention"
data$vote12 [data$LQ1O %in% c(98)] <- "Blanc_Nul"
data$vote12 [data$LQ1O %in% c(99)] <- NA


data$vote12 %>% table() %>% prop.table()

# Vague 12bis

data$vote12bis <- NA
data$vote12bis [data$LBQ1O %in% c(1)] <- "Arthaud"
data$vote12bis [data$LBQ1O %in% c(2)] <- "Poutou"
data$vote12bis [data$LBQ1O %in% c(3)] <- "Melenchon"
data$vote12bis [data$LBQ1O %in% c(4)] <- "Jadot"
data$vote12bis [data$LBQ1O %in% c(5)] <- "Hamon"
data$vote12bis [data$LBQ1O %in% c(6)] <- "Macron"
data$vote12bis [data$LBQ1O %in% c(7)] <- "Fillon"
data$vote12bis [data$LBQ1O %in% c(8)] <- "Dupont_Aignan"
data$vote12bis [data$LBQ1O %in% c(9)] <- "Le_Pen"
data$vote12bis [data$LBQ1O %in% c(10)] <- "Cheminade"
data$vote12bis [data$LBQ1O %in% c(11)] <- "Asselineau"
data$vote12bis [data$LBQ1O %in% c(12)] <- "Lasalle"

data$vote12bis [data$LBQ1O %in% c(97)] <- "Abstention"
data$vote12bis [data$LBQ1O %in% c(98)] <- "Blanc_Nul"
data$vote12bis [data$LBQ1O %in% c(99)] <- NA


data$vote12bis %>% table() %>% prop.table()

# lala


data$vote12bisSEC <- NA
data$vote12bisSEC [data$LBQ1OTER %in% c(1)] <- "Arthaud"
data$vote12bisSEC [data$LBQ1OTER %in% c(2)] <- "Poutou"
data$vote12bisSEC [data$LBQ1OTER %in% c(3)] <- "Melenchon"
data$vote12bisSEC [data$LBQ1OTER %in% c(4)] <- "Jadot"
data$vote12bisSEC [data$LBQ1OTER %in% c(5)] <- "Hamon"
data$vote12bisSEC [data$LBQ1OTER %in% c(6)] <- "Macron"
data$vote12bisSEC [data$LBQ1OTER %in% c(7)] <- "Fillon"
data$vote12bisSEC [data$LBQ1OTER %in% c(8)] <- "Dupont_Aignan"
data$vote12bisSEC [data$LBQ1OTER %in% c(9)] <- "Le_Pen"
data$vote12bisSEC [data$LBQ1OTER %in% c(10)] <- "Cheminade"
data$vote12bisSEC [data$LBQ1OTER %in% c(11)] <- "Asselineau"
data$vote12bisSEC [data$LBQ1OTER %in% c(12)] <- "Lasalle"

data$vote12bisSEC [data$LBQ1OTER %in% c(97)] <- "Abstention"
data$vote12bisSEC [data$LBQ1OTER %in% c(98)] <- "Blanc_Nul"
data$vote12bisSEC [data$LBQ1OTER %in% c(99)] <- NA


data$vote12bisSEC %>% table() %>% prop.table()

#kaka

#11b
data$vote11bis2tourEMLP <- NA 
data$vote11bis2tourEMLP [data$KIVPRE2B %in% c(1)] <- "Macron"
data$vote11bis2tourEMLP [data$KIVPRE2B %in% c(2)] <- "Le_Pen"
data$vote11bis2tourEMLP [data$KIVPRE2B %in% c(97)] <- "Abstention"
data$vote11bis2tourEMLP [data$KIVPRE2B %in% c(98)] <- "Blanc_Nul"
data$vote11bis2tourEMLP [data$KIVPRE2B %in% c(99)] <- NA

data$vote11bis2tourEMFF <- NA 
data$vote11bis2tourEMFF [data$KIVPRE2A %in% c(1)] <- "Macron"
data$vote11bis2tourEMFF [data$KIVPRE2A %in% c(2)] <- "Le_Pen"
data$vote11bis2tourEMFF [data$KIVPRE2A %in% c(97)] <- "Abstention"
data$vote11bis2tourEMFF [data$KIVPRE2A %in% c(98)] <- "Blanc_Nul"
data$vote11bis2tourEMFF [data$KIVPRE2A %in% c(99)] <- NA

data$vote11bis2tourLPFF <- NA 
data$vote11bis2tourLPFF [data$KIVPRE2C %in% c(1)] <- "Fillon"
data$vote11bis2tourLPFF [data$KIVPRE2C %in% c(2)] <- "Le_Pen"
data$vote11bis2tourLPFF [data$KIVPRE2C %in% c(97)] <- "Abstention"
data$vote11bis2tourLPFF [data$KIVPRE2C %in% c(98)] <- "Blanc_Nul"
data$vote11bis2tourLPFF [data$KIVPRE2C %in% c(99)] <- NA

#12
data$vote122tourEMLP <- NA 
data$vote122tourEMLP [data$LIVPRE2B %in% c(1)] <- "Macron"
data$vote122tourEMLP [data$LIVPRE2B %in% c(2)] <- "Le_Pen"
data$vote122tourEMLP [data$LIVPRE2B %in% c(97)] <- "Abstention"
data$vote122tourEMLP [data$LIVPRE2B %in% c(98)] <- "Blanc_Nul"
data$vote122tourEMLP [data$LIVPRE2B %in% c(99)] <- NA

data$vote122tourEMFF <- NA 
data$vote122tourEMFF [data$LIVPRE2A %in% c(1)] <- "Macron"
data$vote122tourEMFF [data$LIVPRE2A %in% c(2)] <- "Le_Pen"
data$vote122tourEMFF [data$LIVPRE2A %in% c(97)] <- "Abstention"
data$vote122tourEMFF [data$LIVPRE2A %in% c(98)] <- "Blanc_Nul"
data$vote122tourEMFF [data$LIVPRE2A %in% c(99)] <- NA

data$vote122tourLPFF <- NA 
data$vote122tourLPFF [data$LIVPRE2C %in% c(1)] <- "Fillon"
data$vote122tourLPFF [data$LIVPRE2C %in% c(2)] <- "Le_Pen"
data$vote122tourLPFF [data$LIVPRE2C %in% c(97)] <- "Abstention"
data$vote122tourLPFF [data$LIVPRE2C %in% c(98)] <- "Blanc_Nul"
data$vote122tourLPFF [data$LIVPRE2C %in% c(99)] <- NA

# 12bis
data$vote12bis2tourEMLP <- NA 
data$vote12bis2tourEMLP [data$LBIVPRE2B %in% c(1)] <- "Macron"
data$vote12bis2tourEMLP [data$LBIVPRE2B %in% c(2)] <- "Le_Pen"
data$vote12bis2tourEMLP [data$LBIVPRE2B %in% c(97)] <- "Abstention"
data$vote12bis2tourEMLP [data$LBIVPRE2B %in% c(98)] <- "Blanc_Nul"
data$vote12bis2tourEMLP [data$LBIVPRE2B %in% c(99)] <- NA

data$vote12bis2tourEMFF <- NA 
data$vote12bis2tourEMFF [data$LBIVPRE2A %in% c(1)] <- "Macron"
data$vote12bis2tourEMFF [data$LBIVPRE2A %in% c(2)] <- "Le_Pen"
data$vote12bis2tourEMFF [data$LBIVPRE2A %in% c(97)] <- "Abstention"
data$vote12bis2tourEMFF [data$LBIVPRE2A %in% c(98)] <- "Blanc_Nul"
data$vote12bis2tourEMFF [data$LBIVPRE2A %in% c(99)] <- NA

data$vote12bis2tourLPFF <- NA 
data$vote12bis2tourLPFF [data$LBIVPRE2C %in% c(1)] <- "Fillon"
data$vote12bis2tourLPFF [data$LBIVPRE2C %in% c(2)] <- "Le_Pen"
data$vote12bis2tourLPFF [data$LBIVPRE2C %in% c(97)] <- "Abstention"
data$vote12bis2tourLPFF [data$LBIVPRE2C %in% c(98)] <- "Blanc_Nul"
data$vote12bis2tourLPFF [data$LBIVPRE2C %in% c(99)] <- NA


### 13

data$vote132tourEMLP <- NA 
data$vote132tourEMLP [data$MIVPRE2B %in% c(1)] <- "Macron"
data$vote132tourEMLP [data$MIVPRE2B %in% c(2)] <- "Le_Pen"
data$vote132tourEMLP [data$MIVPRE2B %in% c(97)] <- "Abstention"
data$vote132tourEMLP [data$MIVPRE2B %in% c(98)] <- "Blanc_Nul"
data$vote132tourEMLP [data$MIVPRE2B %in% c(99)] <- NA

data$vote132tourEMFF <- NA 
data$vote132tourEMFF [data$MIVPRE2A %in% c(1)] <- "Macron"
data$vote132tourEMFF [data$MIVPRE2A %in% c(2)] <- "Le_Pen"
data$vote132tourEMFF [data$MIVPRE2A %in% c(97)] <- "Abstention"
data$vote132tourEMFF [data$MIVPRE2A %in% c(98)] <- "Blanc_Nul"
data$vote132tourEMFF [data$MIVPRE2A %in% c(99)] <- NA

data$vote132tourLPFF <- NA 
data$vote132tourLPFF [data$MIVPRE2C %in% c(1)] <- "Fillon"
data$vote132tourLPFF [data$MIVPRE2C %in% c(2)] <- "Le_Pen"
data$vote132tourLPFF [data$MIVPRE2C %in% c(97)] <- "Abstention"
data$vote132tourLPFF [data$MIVPRE2C %in% c(98)] <- "Blanc_Nul"
data$vote132tourLPFF [data$MIVPRE2C %in% c(99)] <- NA

data$vote132tourMEEM <- NA 
data$vote132tourMEEM [data$MIVPRE2D %in% c(1)] <- "Melenchon"
data$vote132tourMEEM [data$MIVPRE2D %n% c(2)] <- "Macron"
data$vote132tourMEEM [data$MIVPRE2D %in% c(97)] <- "Abstention"
data$vote132tourMEEM [data$MIVPRE2D %in% c(98)] <- "Blanc_Nul"
data$vote132tourMEEM [data$MIVPRE2D %in% c(99)] <- NA

data$vote132tourMELP <- NA 
data$vote132tourMELP [data$MIVPRE2E %in% c(1)] <- "Melenchon"
data$vote132tourMELP [data$MIVPRE2E %in% c(2)] <- "Le_Pen"
data$vote132tourMELP [data$MIVPRE2E %in% c(97)] <- "Abstention"
data$vote132tourMELP [data$MIVPRE2E %in% c(98)] <- "Blanc_Nul"
data$vote132tourMELP [data$MIVPRE2E %in% c(99)] <- NA

data$vote132tourMEFF <- NA 
data$vote132tourMEFF [data$MIVPRE2F %in% c(1)] <- "Melenchon"
data$vote132tourMEFF [data$MIVPRE2F %in% c(2)] <- "Fillon"
data$vote132tourMEFF [data$MIVPRE2F %in% c(97)] <- "Abstention"
data$vote132tourMEFF [data$MIVPRE2F %in% c(98)] <- "Blanc_Nul"
data$vote132tourMEFF [data$MIVPRE2F %in% c(99)] <- NA


table(data$vote132tourEMLP) %>% prop.table()


data$vote13 <- NA
data$vote13 [data$MQ1O %in% c(1)] <- "Arthaud"
data$vote13 [data$MQ1O %in% c(2)] <- "Poutou"
data$vote13 [data$MQ1O %in% c(3)] <- "Melenchon"
data$vote13 [data$MQ1O %in% c(4)] <- "Jadot"
data$vote13 [data$MQ1O %in% c(5)] <- "Hamon"
data$vote13 [data$MQ1O %in% c(6)] <- "Macron"
data$vote13 [data$MQ1O %in% c(7)] <- "Fillon"
data$vote13 [data$MQ1O %in% c(8)] <- "Dupont_Aignan"
data$vote13 [data$MQ1O %in% c(9)] <- "Le_Pen"
data$vote13 [data$MQ1O %in% c(10)] <- "Cheminade"
data$vote13 [data$MQ1O %in% c(11)] <- "Asselineau"
data$vote13 [data$MQ1O %in% c(12)] <- "Lasalle"

data$vote13 [data$MQ1O %in% c(97)] <- "Abstention"
data$vote13 [data$MQ1O %in% c(98)] <- "Blanc_Nul"
data$vote13 [data$MQ1O %in% c(99)] <- NA


data$vote13 %>% table() %>% prop.table()

# lala


data$vote13SEC <- NA
data$vote13SEC [data$MQ1OTER %in% c(1)] <- "Arthaud"
data$vote13SEC [data$MQ1OTER %in% c(2)] <- "Poutou"
data$vote13SEC [data$MQ1OTER %in% c(3)] <- "Melenchon"
data$vote13SEC [data$MQ1OTER %in% c(4)] <- "Jadot"
data$vote13SEC [data$MQ1OTER %in% c(5)] <- "Hamon"
data$vote13SEC [data$MQ1OTER %in% c(6)] <- "Macron"
data$vote13SEC [data$MQ1OTER %in% c(7)] <- "Fillon"
data$vote13SEC [data$MQ1OTER %in% c(8)] <- "Dupont_Aignan"
data$vote13SEC [data$MQ1OTER %in% c(9)] <- "Le_Pen"
data$vote13SEC [data$MQ1OTER %in% c(10)] <- "Cheminade"
data$vote13SEC [data$MQ1OTER %in% c(11)] <- "Asselineau"
data$vote13SEC [data$MQ1OTER %in% c(12)] <- "Lasalle"

data$vote13SEC [data$MQ1OTER %in% c(97)] <- "Abstention"
data$vote13SEC [data$MQ1OTER %in% c(98)] <- "Blanc_Nul"
data$vote13SEC [data$MQ1OTER %in% c(99)] <- NA


data$vote13SEC %>% table() %>% prop.table()

table(second = data$vote13SEC,first= data$vote13) %>% prop.table(2) %>% round(2)


## Vote Probs

data$melenchon13 <- data$MQ40_0 %>% as.numeric()
data$macron13 <- data$MQ40_1 %>% as.numeric()
data$lepen13 <- data$MQ40_9 %>% as.numeric()
data$hamon13 <- data$MQ40_11 %>% as.numeric()
data$fillon13 <- data$MQ40_12 %>% as.numeric()
data$arthaud13 <- data$MQ40_13 %>% as.numeric()
data$poutou13 <- data$MQ40_14 %>% as.numeric()
data$dupont_aignan13 <- data$MQ40_15 %>% as.numeric()
data$lasalle13 <- data$MQ40_16 %>% as.numeric()
data$cheminade13 <- data$MQ40_17 %>% as.numeric()
data$asselineau13 <- data$MQ40_18 %>% as.numeric()

ggplot(data) + 
  geom_density(aes(x = melenchon13),col = "turquoise")+
  geom_density(aes(x = macron13),col = "salmon")+
  geom_density(aes(x = lepen13),col = "black")+
  geom_density(aes(x = fillon13),col = "darkblue")+
  geom_density(aes(x = hamon13),col = "red")


### vote probs 12


data$melenchon12 <- data$LQ40_0 %>% as.numeric()
data$macron12 <- data$LQ40_1 %>% as.numeric()
data$lepen12 <- data$LQ40_9 %>% as.numeric()
data$hamon12 <- data$LQ40_11 %>% as.numeric()
data$fillon12 <- data$LQ40_12 %>% as.numeric()
data$arthaud12 <- data$LQ40_13 %>% as.numeric()
data$poutou12 <- data$LQ40_14 %>% as.numeric()
data$dupont_aignan12 <- data$LQ40_15 %>% as.numeric()
#data$lasalle12 <- data$LQ40_16 %>% as.numeric()
#data$cheminade12 <- data$LQ40_17 %>% as.numeric()
#data$asselineau12 <- data$LQ40_18 %>% as.numeric()


# Regionale Komponenten: 

data$region <- data$AREG13

regional_key_old <- data.frame(region = c(44,75,84,28,27,53,24,94,11,76,32,52,93),
                           name = c("Alsace, Champagne-Ardenne et Lorraine",
                                    "Aquitaine, Limousin et Poitou-Charentes",
                                    "Auvergne et Rhône-Alpes", 
                                    "Basse-Normandie et Haute-Normandie", 
                                    "Bourgogne et Franche-Comté",
                                    "Bretagne",   
                                    "Centre",
                                    "Corse",   
                                    "Ile-de-France",  
                                    "Languedoc-Roussillon et Midi-Pyrénées", 
                                    "Nord - Pas-de-Calais et Picardie",  
                                    "Pays de la Loire",       
                                    "Provence-Alpes-Côte d'Azur"))

regional_key <- data.frame(region = c(44,75,84,28,27,53,24,94,11,76,32,52,93),
                               name = c("Grand-Est",
                                        "Nouvelle-Aquitaine",
                                        "Auvergne-Rhone-Alpes", 
                                        "Normandie", 
                                        "Bourgogne-Franche-Comte",
                                        "Bretagne",   
                                        "Centre-Val de Loire",
                                        "Corse",   
                                        "Ile-de-France",  
                                        "Occitanie", 
                                        "Hauts-de-France",  
                                        "Pays-de-la-Loire",       
                                        "Provence-Alpes-Cote d Azur"))

data$region %>% unique()
data <- data %>% left_join(regional_key, by = "region")

data$name

# Eigentlich neues Dokument: 


data$pslrfn_2a <- ifelse({data$ps1 > data$lr1 &  data$lr1 > data$fn1},1,0)
data$psfnlr_2a <- ifelse({data$ps1 > data$fn1 &  data$fn1> data$lr1},1,0)
data$lrfn_tie_second_2a <- ifelse({data$ps1 > data$fn1 &  data$fn1 == data$lr1},1,0)
data$pslr_tie_first_2a <- ifelse({data$ps1 > data$fn1 &  data$ps1 == data$lr1},1,0)
data$psfn_tie_first_2a <- ifelse({data$ps1 == data$fn1 &  data$ps1 > data$lr1},1,0)

data$lead_ps_2a <- ifelse({
  data$psfnlr_2a == 1 |
    data$pslrfn_2a == 1 |
    data$pslr_tie_first_2a == 1 |
    data$lrfn_tie_second_2a == 1|
    data$psfn_tie_first_2a
},1,0)


data$lead_pslr_2a <- data$ps1 - data$lr1
data$lead_psfn_2a <- data$ps1 - data$fn1

# LR Leading
data$lrpsfn_2a <- ifelse({data$lr1 > data$ps1 &  data$ps1> data$fn1},1,0)
data$lrfnps_2a <- ifelse({data$lr1 > data$fn1 &  data$fn1> data$ps1},1,0)
data$lrfn_tie_first_2a <- ifelse({data$ps1 < data$fn1 &  data$fn1 == data$lr1},1,0)
data$lrps_tie_first_2a <- ifelse({data$ps1 > data$fn1 &  data$ps1 == data$lr1},1,0)
data$psfn_tie_second_2a <- ifelse({data$ps1 == data$fn1 &  data$fn1 < data$lr1},1,0)


data$lead_lr_2a <- ifelse({
  data$lrpsfn_2a == 1 |
    data$lrfnps_2a == 1|
    data$lrfn_tie_first_2a == 1 |
    data$lrps_tie_first_2a == 1 |
    data$psfn_tie_second_2a == 1
},1,0)


data$lead_lrps_2a <- data$lr1 - data$ps1
data$lead_lrfn_2a <- data$lr1 - data$fn1

# FN Leading
data$fnlrps_2a <- ifelse({data$fn1 > data$lr1 &  data$lr1 > data$ps1},1,0)
data$fnpslr_2a <- ifelse({data$fn1 > data$ps1 &  data$ps1 > data$lr1},1,0)
data$pslr_tie_second_2a <- ifelse({data$ps1 == data$lr1 &  data$fn1 > data$lr1},1,0)


data$lead_fn_2a <- ifelse({
  data$fnlrps_2a == 1 | 
    data$fnlrps_2a == 1 | 
    data$pslr_tie_second_2a ==1 | 
    data$lrfn_tie_first_2a ==1 |
    data$psfn_tie_first_2a == 1
},1,0)

# Amount of lead
data$lead_fnps_2a <- data$fn1 - data$ps1
data$lead_fnlr_2a <- data$fn1 - data$lr1


# 2nd tour

data$lrfn_2b <- ifelse(data$lr_vs_fn2 > 50,1,0)
data$fnlr_2b <- ifelse(data$lr_vs_fn2 < 50,1,0)

data$pslr_2b <- ifelse(data$ps_vs_lr2 > 50,1,0)
data$lrps_2b <- ifelse(data$ps_vs_lr2 < 50,1,0)

data$fnps_2b <- ifelse(data$fn_vs_ps2 > 50,1,0)
data$psfn_2b <- ifelse(data$fn_vs_ps2 < 50,1,0)

data$fnps_lrfn_2b <- ifelse({data$fnps_2b == 1 & data$lrfn_2b == 1},1,0)
data$psfn_fnlr_2b <- ifelse({data$psfn_2b == 1 & data$fnlr_2b == 1},1,0)

data$psfn_lrfn_2b <- ifelse({data$psfn_2b == 1 & data$lrfn_2b == 1},1,0)
data$fnps_fnlr_2b <- ifelse({data$fnps_2b == 1 & data$fnlr_2b == 1},1,0)

# Neu 2nd tour: 

data$secondtour_fn_2b <- NA 
data$secondtour_fn_2b [data$lrfn_2b == 0 & data$fnps_2b == 1] <- "FN > LR & FN > PS"
data$secondtour_fn_2b [data$lrfn_2b == 0 & data$fnps_2b == 0] <- "FN > LR & FN < PS"
data$secondtour_fn_2b [data$lrfn_2b == 1 & data$fnps_2b == 0] <- "FN < LR & FN < PS"
data$secondtour_fn_2b [data$lrfn_2b == 1 & data$fnps_2b == 1] <- "FN < LR & FN > PS"

data$secondtour_ps_2b <- NA 
data$secondtour_ps_2b [data$pslr_2b == 0 & data$fnps_2b == 0] <- "PS < LR & FN < PS"
data$secondtour_ps_2b [data$pslr_2b == 0 & data$fnps_2b == 1] <- "PS < LR & FN > PS"
data$secondtour_ps_2b [data$pslr_2b == 1 & data$fnps_2b == 0] <- "PS > LR & FN < PS"
data$secondtour_ps_2b [data$pslr_2b == 1 & data$fnps_2b == 1] <- "PS > LR & FN > PS"

data$secondtour_fn_2b


# Combinaisons dichotome

# FN VS other two
data$FNsecondtour_fn_fn <- ifelse(data$lrfn_2b == 0 & data$fnps_2b == 1, 1,0)
data$FNsecondtour_fn_ps <- ifelse(data$lrfn_2b == 0 & data$fnps_2b == 0, 1,0)
data$FNsecondtour_lr_ps <- ifelse(data$lrfn_2b == 1 & data$fnps_2b == 0, 1,0)
data$FNsecondtour_lr_fn <- ifelse(data$lrfn_2b == 1 & data$fnps_2b == 1, 1,0)

# LR VS other two
data$LRsecondtour_fn_lr <- ifelse(data$lrfn_2b == 0 & data$lrps_2b == 1, 1,0)
data$LRsecondtour_fn_ps <- ifelse(data$lrfn_2b == 0 & data$lrps_2b == 0, 1,0)
data$LRsecondtour_lr_ps <- ifelse(data$lrfn_2b == 1 & data$lrps_2b == 0, 1,0)
data$LRsecondtour_lr_lr <- ifelse(data$lrfn_2b == 1 & data$lrps_2b == 1, 1,0)

# PS VS other two
data$PSsecondtour_lr_fn <- ifelse(data$pslr_2b == 0 & data$fnps_2b == 1, 1,0)
data$PSsecondtour_lr_ps <- ifelse(data$pslr_2b == 0 & data$fnps_2b == 0, 1,0)
data$PSsecondtour_ps_ps <- ifelse(data$pslr_2b == 1 & data$fnps_2b == 0, 1,0)
data$PSsecondtour_ps_fn <- ifelse(data$pslr_2b == 1 & data$fnps_2b == 1, 1,0)


# Distances between prob
data$distancePSFN <- data$probPs8 - data$probFn8
data$distanceFNPS <- data$probFn8 - data$probPs8

data$distancePSLR <- data$probPs8 - data$probLr8
data$distanceLRPS <- data$probLr8 - data$probPs8

data$distanceLRFN <- data$probLr8 - data$probFn8
data$distanceFNLR <- data$probFn8 - data$probLr8

# Generalized 

data$probDif_first_second_2a <- data$maxProb_numeric-data$middleProb_numeric


# Closeness of the race logged: 

# PS - LR 

data$log_ps_lr_2a <- NA
data$log_ps_lr_2a <- log(abs(data$ps1-data$lr1) + 1) 
data$log_ps_lr_2a <- ifelse(data$ps1 - data$lr1 >= 0,data$log_ps_lr_2a,data$log_ps_lr_2a*-1)

data$log_lr_fn_2a <- NA
data$log_lr_fn_2a <- log(abs(data$lr1-data$fn1) + 1) 
data$log_lr_fn_2a <- ifelse(data$lr1 - data$fn1 >= 0,data$log_lr_fn_2a,data$log_lr_fn_2a*-1)

data$log_fn_ps_2a <- NA
data$log_fn_ps_2a <- log(abs(data$fn1-data$ps1) + 1) 
data$log_fn_ps_2a <- ifelse(data$fn1 - data$ps1 >= 0,data$log_fn_ps_2a,data$log_fn_ps_2a*-1)

#write.csv2 (names(data),"variables.csv")
# Saving of Data Frame





######
# Vague 14

# Vote legislatives

data$NIVLEG1P %>% table()

data$leg14 <- c()
data$leg14 [data$NIVLEG1P %in% c(1)] <- "Lutte Ouvriere"
data$leg14 [data$NIVLEG1P %in% c(2)] <- "Nuveau Parti Anticapitaliste"
data$leg14 [data$NIVLEG1P %in% c(3)] <- "Front de Gauche"
data$leg14 [data$NIVLEG1P %in% c(4)] <- "Parti Socialiste"
data$leg14 [data$NIVLEG1P %in% c(5)] <- "Europe Ecologie - Les verts"
data$leg14 [data$NIVLEG1P %in% c(6)] <- "En Marche"
data$leg14 [data$NIVLEG1P %in% c(7)] <- "Le MoDem"
data$leg14 [data$NIVLEG1P %in% c(8)] <- "UDI"
data$leg14 [data$NIVLEG1P %in% c(9)] <- "Les Republicains"
data$leg14 [data$NIVLEG1P %in% c(10)] <- "Debout la France"
data$leg14 [data$NIVLEG1P %in% c(11)] <- "Front National"
data$leg14 [data$NIVLEG1P %in% c(12)] <- "Autre Parti"
data$leg14 [data$NIVLEG1P %in% c(97)] <- "Abstention"
data$leg14 [data$NIVLEG1P %in% c(98)] <- "Blanc_Nul"

data$leg14 [is.nan(data$NIVLEG1P)] <- NA

data$leg14 %>% table()




##

data<- data %>% as.data.frame() %>% .[,which(colnames(data) == "weight"):ncol(data)]

#####

# Make The 2a variables 0 for Control Group

vec2a <- data%>% select(contains("2a"))%>% colnames()
vec2b <- data%>% select(contains("_2b"))%>% colnames()
vec2c <- data%>% select(contains("secondtour"))%>% colnames()

vec2b <- vec2b [2:length(vec2b)]

for (c in c(vec2a,vec2b,vec2c)) {
  print(c)
  for (r in 1:nrow(data)) {
    if(data$group_num[r] == 0 & !is.na(data$group_num[r])) {
      eval(parse(text = paste0("data$",c,"[",r,"] <- 0")))
    }
    
  }
  
}
############

# expectations <- read.csv2("data/expectations.csv",header = TRUE)
# expectations$Cas <- expectations$Cas %>% as.numeric()
# 
# data <- 
#   left_join(data,expectations, by = c("exp_id2" = "Cas"))

saveRDS(data,"data/data14.rds")


