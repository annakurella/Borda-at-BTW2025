# 
#
#
# 2025 Nachwahlbefragung Bordavote
#
#
######################################
rm(list = ls())

library(haven)
library(basicspace)
library(survival)
library(nnet)
library(readxl)
library(gridExtra)
library(stargazer)
library(ggplot2)

library(boot)


# Arbeitspfad festelegn
setwd("C:/Users/akurella/Filr/Meine Dateien/PaperProjects/BTW 2025 Bordawahl")


# 
# Datensatz einlesen
load("d2025_borda.RData")     # Borda Data April 2025

library(RColorBrewer)
mycol <- brewer.pal(n=5, name="Dark2")


d$pol_wissen <- ifelse(d$Q7==2, 1, 0)
d$male <- ifelse(d$Q1==1, 1, 0)
d$age <- d$AgeRec
d$ost <- ifelse(d$Q3==4 | 
                  d$Q3==5 |
                  d$Q3==9 |
                  d$Q3>14 , 1, 0)

d$edu <- d$Q4Rec
d$polint <- ifelse(d$Q5<99, -1*d$Q5+5, NA)
d$satisdem <- ifelse(d$Q6<99, -1*d$Q6+5, NA)
d$nonvoter <- ifelse(d$Q8==1, 1, 0)
d$borda7 <- ifelse(d$Q23Rec==2, 1, 0)
d$borda3 <- ifelse(d$Q23Rec==1, 1, 0)



partylab <- c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW")
partycol <- c("black", "red", "gold", "darkgreen", "magenta", "cornflowerblue", "purple")



##################################################################################
#
# Stimmenverteilung unter Borda mit 3 Rängen
#

# Erststimme
d$vote1 <- ifelse(d$vote==1, "cdu", NA)
d$vote1[d$vote==3] <- "spd"
d$vote1[d$vote==4] <- "fdp"
d$vote1[d$vote==5] <- "gru"
d$vote1[d$vote==6] <- "lin"
d$vote1[d$vote==7] <- "afd"
d$vote1[d$vote==8] <- "bsw"

d$FPvote_cdu <- ifelse(d$vote==1, 1, 0)
d$FPvote_spd <- ifelse(d$vote==3, 1, 0)
d$FPvote_fdp <- ifelse(d$vote==4, 1, 0)
d$FPvote_gru <- ifelse(d$vote==5, 1, 0)
d$FPvote_lin <- ifelse(d$vote==6, 1, 0)
d$FPvote_afd <- ifelse(d$vote==7, 1, 0)
d$FPvote_bsw <- ifelse(d$vote==8, 1, 0)


# erster Rang
d$borda3_1 <- ifelse(d$Q23AQ23A_1==1, "cdu", NA)
d$borda3_1[d$Q23AQ23A_2==1] <- "spd"
d$borda3_1[d$Q23AQ23A_3==1] <- "fdp"
d$borda3_1[d$Q23AQ23A_4==1] <- "gru"
d$borda3_1[d$Q23AQ23A_5==1] <- "lin"
d$borda3_1[d$Q23AQ23A_6==1] <- "afd"
d$borda3_1[d$Q23AQ23A_7==1] <- "bsw"


table(d$borda3_1)

table(d$vote1[d$Q23Rec==1 & !is.na(d$borda1)])


# zweiter Rang
d$borda3_2 <- ifelse(d$Q23AQ23A_1==2, "cdu", NA)
d$borda3_2[d$Q23AQ23A_2==2] <- "spd"
d$borda3_2[d$Q23AQ23A_3==2] <- "fdp"
d$borda3_2[d$Q23AQ23A_4==2] <- "gru"
d$borda3_2[d$Q23AQ23A_5==2] <- "lin"
d$borda3_2[d$Q23AQ23A_6==2] <- "afd"
d$borda3_2[d$Q23AQ23A_7==2] <- "bsw"

table(d$borda3_2)


# dritter Rang
d$borda3_3 <- ifelse(d$Q23AQ23A_1==3, "cdu", NA)
d$borda3_3[d$Q23AQ23A_2==3] <- "spd"
d$borda3_3[d$Q23AQ23A_3==3] <- "fdp"
d$borda3_3[d$Q23AQ23A_4==3] <- "gru"
d$borda3_3[d$Q23AQ23A_5==3] <- "lin"
d$borda3_3[d$Q23AQ23A_6==3] <- "afd"
d$borda3_3[d$Q23AQ23A_7==3] <- "bsw"

table(d$borda3_3)

summary(d$borda3_3[d$borda3==1])



bmat <- matrix(rbind(table(d$borda3_1), 
                     table(d$borda3_2), 
                     table(d$borda3_3)), 
               byrow=FALSE, ncol=7)


mycol <-c( "#CD1076FF", "#6497B1FF","#FFB04FFF", "#679C35FF")

specol <- rev(brewer.pal("Spectral", n=7))[1:3]
#pdf(file = "freq_1-3.pdf", width=10)

par(mfrow=c(1,1))
barplot(bmat[,c(3,7,4,5,6,1,2)], beside=TRUE, main="", 
        col=specol[1:3], border="white")
axis(1, labels=c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW"), at=seq(2,29, by=4))
legend("top", horiz=T, col=specol, pch=15,
       legend=c("1", "2", "3"), title="Rank #", cex=1.3)


#dev.off()



d$cdu_bscore3 <- ifelse(d$Q23AQ23A_1==1, 3, 0)
d$cdu_bscore3[d$Q23AQ23A_1==2] <- 2
d$cdu_bscore3[d$Q23AQ23A_1==3] <- 1

d$spd_bscore3 <- ifelse(d$Q23AQ23A_2==1, 3, 0)
d$spd_bscore3[d$Q23AQ23A_2==2] <- 2
d$spd_bscore3[d$Q23AQ23A_2==3] <- 1

d$fdp_bscore3 <- ifelse(d$Q23AQ23A_3==1, 3, 0)
d$fdp_bscore3[d$Q23AQ23A_3==2] <- 2
d$fdp_bscore3[d$Q23AQ23A_3==3] <- 1

d$gru_bscore3 <- ifelse(d$Q23AQ23A_4==1, 3, 0)
d$gru_bscore3[d$Q23AQ23A_4==2] <- 2
d$gru_bscore3[d$Q23AQ23A_4==3] <- 1

d$lin_bscore3 <- ifelse(d$Q23AQ23A_5==1, 3, 0)
d$lin_bscore3[d$Q23AQ23A_5==2] <- 2
d$lin_bscore3[d$Q23AQ23A_5==3] <- 1

d$afd_bscore3 <- ifelse(d$Q23AQ23A_6==1, 3, 0)
d$afd_bscore3[d$Q23AQ23A_6==2] <- 2
d$afd_bscore3[d$Q23AQ23A_6==3] <- 1

d$bsw_bscore3 <- ifelse(d$Q23AQ23A_7==1, 3, 0)
d$bsw_bscore3[d$Q23AQ23A_7==2] <- 2
d$bsw_bscore3[d$Q23AQ23A_7==3] <- 1

###
# 3er Borda Ergebnis
#

cdu_b3 <- sum(d$cdu_bscore3)
spd_b3 <- sum(d$spd_bscore3)
fdp_b3 <- sum(d$fdp_bscore3)
gru_b3 <- sum(d$gru_bscore3)
lin_b3 <- sum(d$lin_bscore3)
afd_b3 <- sum(d$afd_bscore3)
bsw_b3 <- sum(d$bsw_bscore3)
total_b3 <- sum(cdu_b3, spd_b3, gru_b3, fdp_b3, lin_b3, afd_b3, bsw_b3)

# borda 3 ergebnis
borda3_erg <- c(
cdu_b3/total_b3,
spd_b3/total_b3,
fdp_b3/total_b3,
gru_b3/total_b3,
lin_b3/total_b3,
afd_b3/total_b3,
bsw_b3/total_b3)



#### Bootstrap confidence intervals ####

# sample weights based on official vote result
off_votes <- c(0.164, 0.286, 0.116, 0.043, 0.208, 0.088, 0.05)
b3sample_votes <- c(0.19, 0.239, 0.114, 0.042, 0.257,0.093,0.065)
b3weights <- off_votes/b3sample_votes

d$b3_weights <- 0
d$b3_weights[d$vote1=="spd"] <- b3weights[1]
d$b3_weights[d$vote1=="cdu"] <- b3weights[2]
d$b3_weights[d$vote1=="gru"] <- b3weights[3]
d$b3_weights[d$vote1=="fdp"] <- b3weights[4]
d$b3_weights[d$vote1=="afd"] <- b3weights[5]
d$b3_weights[d$vote1=="lin"] <- b3weights[6]
d$b3_weights[d$vote1=="bsw"] <- b3weights[7]


db3 <- d[d$borda3==1,]

borda3scores <- function(data, indices){ 
  
  d <- data[indices,]
  w <- d$b3_weights
  
# party totals
  cdu_b3 <- sum(d$cdu_bscore3*w)
  spd_b3 <- sum(d$spd_bscore3*w)
  fdp_b3 <- sum(d$fdp_bscore3*w)
  gru_b3 <- sum(d$gru_bscore3*w)
  lin_b3 <- sum(d$lin_bscore3*w)
  afd_b3 <- sum(d$afd_bscore3*w)
  bsw_b3 <- sum(d$bsw_bscore3*w)
  total_b3 <- sum(cdu_b3, spd_b3, gru_b3, fdp_b3, lin_b3, afd_b3, bsw_b3)
  
# final Borda-3 result (shares)
  c(
    cdu_b3/total_b3,
    spd_b3/total_b3,
    fdp_b3/total_b3,
    gru_b3/total_b3,
    lin_b3/total_b3,
    afd_b3/total_b3,
    bsw_b3/total_b3)
}

FP3scores <- function(data, indices){
  
 d <- data[indices,]
 w <- d$b3_weights
 
  # Erststimme  party totals
  cdu_fpb3 <- sum(d$FPvote_cdu*w, na.rm=TRUE)
  spd_fpb3 <- sum(d$FPvote_spd *w, na.rm=TRUE)
  fdp_fpb3 <- sum(d$FPvote_fdp *w, na.rm=TRUE)
  gru_fpb3 <- sum(d$FPvote_gru *w, na.rm=TRUE)
  lin_fpb3 <- sum(d$FPvote_lin *w, na.rm=TRUE)
  afd_fpb3 <- sum(d$FPvote_afd *w, na.rm=TRUE)
  bsw_fpb3 <- sum(d$FPvote_bsw *w, na.rm=TRUE)
  total_fpb3 <- sum(cdu_fpb3, spd_fpb3, gru_fpb3, fdp_fpb3, lin_fpb3, afd_fpb3, bsw_fpb3)
  
  # final Borda-3 result (shares)
  c(
    cdu_fpb3/total_fpb3,
    spd_fpb3/total_fpb3,
    fdp_fpb3/total_fpb3,
    gru_fpb3/total_fpb3,
    lin_fpb3/total_fpb3,
    afd_fpb3/total_fpb3,
    bsw_fpb3/total_fpb3)
  
}

library(boot)

bootFP3 <- boot(db3, FP3scores, R=10000, sim="ordinary")
FP3_point <- apply(bootFP3$t,2,median)
FP3_ci <- matrix(NA, nrow=7, ncol=2)
for(i in 1:7){
FP3_ci[i,] <- boot.ci(bootFP3, index=i, conf = c(0.95),
        type = c("norm"))$normal[2:3]}



bootB3 <- boot(db3, borda3scores, R=10000, sim="ordinary")
B3_point <- apply(bootB3$t,2,median)
B3_ci <- matrix(NA, nrow=7, ncol=2)
for(i in 1:7){
  B3_ci[i,] <- boot.ci(bootB3, index=i, conf = c(0.95),
                       type = c("norm"))$normal[2:3]}








################################################################################

#
# Stimmenverteilung unter Borda mit 7 Rängen
#

# erster Rang
d$borda7_1 <- ifelse(d$Q23BQ23B_1==1, "cdu", NA)
d$borda7_1[d$Q23BQ23B_2==1] <- "spd"
d$borda7_1[d$Q23BQ23B_3==1] <- "fdp"
d$borda7_1[d$Q23BQ23B_4==1] <- "gru"
d$borda7_1[d$Q23BQ23B_5==1] <- "lin"
d$borda7_1[d$Q23BQ23B_6==1] <- "afd"
d$borda7_1[d$Q23BQ23B_7==1] <- "bsw"


table(d$borda7_1)

table(d$vote1[d$Q23Rec==2 & !is.na(d$borda7_1)])


# zweiter Rang
d$borda7_2 <- ifelse(d$Q23BQ23B_1==2, "cdu", NA)
d$borda7_2[d$Q23BQ23B_2==2] <- "spd"
d$borda7_2[d$Q23BQ23B_3==2] <- "fdp"
d$borda7_2[d$Q23BQ23B_4==2] <- "gru"
d$borda7_2[d$Q23BQ23B_5==2] <- "lin"
d$borda7_2[d$Q23BQ23B_6==2] <- "afd"
d$borda7_2[d$Q23BQ23B_7==2] <- "bsw"

table(d$borda7_2)


# dritter Rang
d$borda7_3 <- ifelse(d$Q23BQ23B_1==3, "cdu", NA)
d$borda7_3[d$Q23BQ23B_2==3] <- "spd"
d$borda7_3[d$Q23BQ23B_3==3] <- "fdp"
d$borda7_3[d$Q23BQ23B_4==3] <- "gru"
d$borda7_3[d$Q23BQ23B_5==3] <- "lin"
d$borda7_3[d$Q23BQ23B_6==3] <- "afd"
d$borda7_3[d$Q23BQ23B_7==3] <- "bsw"

table(d$borda7_3)

# vierter Rang
d$borda7_4 <- ifelse(d$Q23BQ23B_1==4, "cdu", NA)
d$borda7_4[d$Q23BQ23B_2==4] <- "spd"
d$borda7_4[d$Q23BQ23B_3==4] <- "fdp"
d$borda7_4[d$Q23BQ23B_4==4] <- "gru"
d$borda7_4[d$Q23BQ23B_5==4] <- "lin"
d$borda7_4[d$Q23BQ23B_6==4] <- "afd"
d$borda7_4[d$Q23BQ23B_7==4] <- "bsw"

table(d$borda7_4)


# fünfter Rang
d$borda7_5 <- ifelse(d$Q23BQ23B_1==5, "cdu", NA)
d$borda7_5[d$Q23BQ23B_2==5] <- "spd"
d$borda7_5[d$Q23BQ23B_3==5] <- "fdp"
d$borda7_5[d$Q23BQ23B_4==5] <- "gru"
d$borda7_5[d$Q23BQ23B_5==5] <- "lin"
d$borda7_5[d$Q23BQ23B_6==5] <- "afd"
d$borda7_5[d$Q23BQ23B_7==5] <- "bsw"

table(d$borda7_5)


# sechster Rang
d$borda7_6 <- ifelse(d$Q23BQ23B_1==6, "cdu", NA)
d$borda7_6[d$Q23BQ23B_2==6] <- "spd"
d$borda7_6[d$Q23BQ23B_3==6] <- "fdp"
d$borda7_6[d$Q23BQ23B_4==6] <- "gru"
d$borda7_6[d$Q23BQ23B_5==6] <- "lin"
d$borda7_6[d$Q23BQ23B_6==6] <- "afd"
d$borda7_6[d$Q23BQ23B_7==6] <- "bsw"

table(d$borda7_6)


# siebter Rang
d$borda7_7 <- ifelse(d$Q23BQ23B_1==7, "cdu", NA)
d$borda7_7[d$Q23BQ23B_2==7] <- "spd"
d$borda7_7[d$Q23BQ23B_3==7] <- "fdp"
d$borda7_7[d$Q23BQ23B_4==7] <- "gru"
d$borda7_7[d$Q23BQ23B_5==7] <- "lin"
d$borda7_7[d$Q23BQ23B_6==7] <- "afd"
d$borda7_7[d$Q23BQ23B_7==7] <- "bsw"

table(d$borda7_7)

bmat <- matrix(rbind(table(d$borda7_1), 
                     table(d$borda7_2), 
                     table(d$borda7_3), 
                     table(d$borda7_4), 
                     table(d$borda7_5), 
                     table(d$borda7_6), 
                     table(d$borda7_7)), byrow=FALSE, ncol=7)

specol <- rev(brewer.pal("Spectral", n=7))


pdf(file = "freq_1-7.pdf", width=12)
barplot(bmat[,c(3,7,4,5,6,1,2)], beside=TRUE, main="", col=specol, 
        ylim=c(0,350), border="white")
axis(1, labels=c("CDU", "SPD", "FDP", "Grüne", "Linke", "AfD", "BSW"), at=seq(4,59, by=8))
legend("top", horiz=TRUE, col=specol, pch=15,  title="Rank #", legend=c(1:7))
dev.off()



d$cdu_bscore7 <- ifelse(d$Q23BQ23B_1==1, 6, 0)
d$cdu_bscore7[d$Q23BQ23B_1==2] <- 5
d$cdu_bscore7[d$Q23BQ23B_1==3] <- 4
d$cdu_bscore7[d$Q23BQ23B_1==4] <- 3
d$cdu_bscore7[d$Q23BQ23B_1==5] <- 2
d$cdu_bscore7[d$Q23BQ23B_1==6] <- 1

d$spd_bscore7 <- ifelse(d$Q23BQ23B_2==1, 6, 0)
d$spd_bscore7[d$Q23BQ23B_2==2] <- 5
d$spd_bscore7[d$Q23BQ23B_2==3] <- 4
d$spd_bscore7[d$Q23BQ23B_2==4] <- 3
d$spd_bscore7[d$Q23BQ23B_2==5] <- 2
d$spd_bscore7[d$Q23BQ23B_2==6] <- 1

d$fdp_bscore7 <- ifelse(d$Q23BQ23B_3==1, 6, 0)
d$fdp_bscore7[d$Q23BQ23B_3==2] <- 5
d$fdp_bscore7[d$Q23BQ23B_3==3] <- 4
d$fdp_bscore7[d$Q23BQ23B_3==4] <- 3
d$fdp_bscore7[d$Q23BQ23B_3==5] <- 2
d$fdp_bscore7[d$Q23BQ23B_3==6] <- 1

d$gru_bscore7 <- ifelse(d$Q23BQ23B_4==1, 6, 0)
d$gru_bscore7[d$Q23BQ23B_4==2] <- 5
d$gru_bscore7[d$Q23BQ23B_4==3] <- 4
d$gru_bscore7[d$Q23BQ23B_4==4] <- 3
d$gru_bscore7[d$Q23BQ23B_4==5] <- 2
d$gru_bscore7[d$Q23BQ23B_4==6] <- 1

d$lin_bscore7 <- ifelse(d$Q23BQ23B_5==1, 6, 0)
d$lin_bscore7[d$Q23BQ23B_5==2] <- 5
d$lin_bscore7[d$Q23BQ23B_5==3] <- 4
d$lin_bscore7[d$Q23BQ23B_5==4] <- 3
d$lin_bscore7[d$Q23BQ23B_5==5] <- 2
d$lin_bscore7[d$Q23BQ23B_5==6] <- 1

d$afd_bscore7 <- ifelse(d$Q23BQ23B_6==1, 6, 0)
d$afd_bscore7[d$Q23BQ23B_6==2] <- 5
d$afd_bscore7[d$Q23BQ23B_6==3] <- 4
d$afd_bscore7[d$Q23BQ23B_6==4] <- 3
d$afd_bscore7[d$Q23BQ23B_6==5] <- 2
d$afd_bscore7[d$Q23BQ23B_6==6] <- 1

d$bsw_bscore7 <- ifelse(d$Q23BQ23B_7==1, 6, 0)
d$bsw_bscore7[d$Q23BQ23B_7==2] <- 5
d$bsw_bscore7[d$Q23BQ23B_7==3] <- 4
d$bsw_bscore7[d$Q23BQ23B_7==4] <- 3
d$bsw_bscore7[d$Q23BQ23B_7==5] <- 2
d$bsw_bscore7[d$Q23BQ23B_7==6] <- 1

###
# 7er Borda Ergebnis
#

cdu_b7 <- sum(d$cdu_bscore7)
spd_b7 <- sum(d$spd_bscore7)
fdp_b7 <- sum(d$fdp_bscore7)
gru_b7 <- sum(d$gru_bscore7)
lin_b7 <- sum(d$lin_bscore7)
afd_b7 <- sum(d$afd_bscore7)
bsw_b7 <- sum(d$bsw_bscore7)
total_b7 <- sum(cdu_b7, spd_b7, gru_b7, fdp_b7, lin_b7, afd_b7, bsw_b7)

# borda 7 ergebnis
borda7_erg <- c(
  cdu_b7/total_b7,
  spd_b7/total_b7,
  fdp_b7/total_b7,
  gru_b7/total_b7,
  lin_b7/total_b7,
  afd_b7/total_b7,
  bsw_b7/total_b7)




#### Bootstrap confidence intervals ####

# sample weights based on official vote result
off_votes <- c(0.164, 0.286, 0.116, 0.043, 0.208, 0.088, 0.05)
b7sample_votes <- c(0.179, 0.191, 0.084, 0.048, 0.219,0.077,0.056)
b7weights <- off_votes/b7sample_votes

d$b7_weights <- 0
d$b7_weights[d$vote1=="spd"] <- b7weights[1]
d$b7_weights[d$vote1=="cdu"] <- b7weights[2]
d$b7_weights[d$vote1=="gru"] <- b7weights[3]
d$b7_weights[d$vote1=="fdp"] <- b7weights[4]
d$b7_weights[d$vote1=="afd"] <- b7weights[5]
d$b7_weights[d$vote1=="lin"] <- b7weights[6]
d$b7_weights[d$vote1=="bsw"] <- b7weights[7]


db7 <- d[d$borda7==1,]

borda7scores <- function(data, indices){ 
  
  d <- data[indices,]
  w <- d$b7_weights
  
  # party totals
  cdu_b7 <- sum(d$cdu_bscore7*w)
  spd_b7 <- sum(d$spd_bscore7*w)
  fdp_b7 <- sum(d$fdp_bscore7*w)
  gru_b7 <- sum(d$gru_bscore7*w)
  lin_b7 <- sum(d$lin_bscore7*w)
  afd_b7 <- sum(d$afd_bscore7*w)
  bsw_b7 <- sum(d$bsw_bscore7*w)
  total_b7 <- sum(cdu_b7, spd_b7, gru_b7, fdp_b7, lin_b7, afd_b7, bsw_b7)
  
  # final Borda-3 result (shares)
  c(
    cdu_b7/total_b7,
    spd_b7/total_b7,
    fdp_b7/total_b7,
    gru_b7/total_b7,
    lin_b7/total_b7,
    afd_b7/total_b7,
    bsw_b7/total_b7)
}

FP7scores <- function(data, indices){
  
  d <- data[indices,]
  w <- d$b7_weights
  
  # Erststimme  party totals
  cdu_fpb7 <- sum(d$FPvote_cdu*w, na.rm=TRUE)
  spd_fpb7 <- sum(d$FPvote_spd *w, na.rm=TRUE)
  fdp_fpb7 <- sum(d$FPvote_fdp *w, na.rm=TRUE)
  gru_fpb7 <- sum(d$FPvote_gru *w, na.rm=TRUE)
  lin_fpb7 <- sum(d$FPvote_lin *w, na.rm=TRUE)
  afd_fpb7 <- sum(d$FPvote_afd *w, na.rm=TRUE)
  bsw_fpb7 <- sum(d$FPvote_bsw *w, na.rm=TRUE)
  total_fpb7 <- sum(cdu_fpb7, spd_fpb7, gru_fpb7, fdp_fpb7, lin_fpb7, afd_fpb7, bsw_fpb7)
  
  # final Borda-3 result (shares)
  c(
    cdu_fpb7/total_fpb7,
    spd_fpb7/total_fpb7,
    fdp_fpb7/total_fpb7,
    gru_fpb7/total_fpb7,
    lin_fpb7/total_fpb7,
    afd_fpb7/total_fpb7,
    bsw_fpb7/total_fpb7)
  
}


bootFP7 <- boot(db7, FP7scores, R=10000, sim="ordinary")
FP7_point <- apply(bootFP7$t,2,median)
FP7_ci <- matrix(NA, nrow=7, ncol=2)
for(i in 1:7){
  FP7_ci[i,] <- boot.ci(bootFP7, index=i, conf = c(0.95),
                        type = c("norm"))$normal[2:3]}



bootB7 <- boot(db7, borda7scores, R=10000, sim="ordinary")
B7_point <- apply(bootB7$t,2,median)
B7_ci <- matrix(NA, nrow=7, ncol=2)
for(i in 1:7){
  B7_ci[i,] <- boot.ci(bootB7, index=i, conf = c(0.95),
                       type = c("norm"))$normal[2:3]}






n <- length(d$vote1[d$Q23Rec==2 & !is.na(d$vote1)])
vote1_ergB <- table(d$vote1[d$Q23Rec==2])[c(3,7,4,5,6,1,2)]   / n





##################################################
#
# Election result barplots
#


df <- data.frame(
  party = c("CDU", "SPD", "FDP", "GRU", "LIN", "AFD", "BSW"),
  estimate = FP3_point,
  lower = FP3_ci[,1],
  upper = FP3_ci[,2]
)

fp3plot <-  ggplot(df, aes(x = reorder(party, -estimate), y = estimate)) +
  geom_col(fill = partycol, width = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2,
    linewidth = 0.8,
    color="grey"
  ) + 
  # point estimates above bars
  geom_text(
    aes(label = sprintf("%.2f", estimate)),
    vjust = -4,
    size = 3.5
  ) +
  labs(
    x = NULL,
    y = "Vote share"
  ) +
  # box around plot, no grid
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  # give headroom for labels
  ylim(0, 0.35)



df <- data.frame(
  party = c("CDU", "SPD", "FDP", "GRU", "LIN", "AFD", "BSW"),
  estimate = FP7_point,
  lower = FP7_ci[,1],
  upper = FP7_ci[,2]
)

fp7plot <-  ggplot(df, aes(x = reorder(party, -estimate), y = estimate)) +
  geom_col(fill = partycol, width = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2,
    linewidth = 0.8,
    color="grey"
  ) + 
  # point estimates above bars
  geom_text(
    aes(label = sprintf("%.2f", estimate)),
    vjust = -4,
    size = 3.5
  ) +
  labs(
    x = NULL,
    y = "Vote share"
  ) +
  # box around plot, no grid
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  # give headroom for labels
  ylim(0, 0.35)




df <- data.frame(
  party = c("CDU", "SPD", "FDP", "GRU", "LIN", "AFD", "BSW"),
  estimate = B3_point,
  lower = B3_ci[,1],
  upper = B3_ci[,2]
)


b3plot <-  ggplot(df, aes(x = reorder(party, -estimate), y = estimate)) +
  geom_col(fill = partycol, width = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2,
    linewidth = 0.8,
    color="grey"
  ) + 
  # point estimates above bars
  geom_text(
    aes(label = sprintf("%.2f", estimate)),
    vjust = -3.5,
    size = 3.5
  ) +
  labs(
    x = NULL,
    y = "Vote share"
  ) +
  # box around plot, no grid
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  # give headroom for labels
  ylim(0, 0.35)


df <- data.frame(
  party = c("CDU", "SPD", "FDP", "GRU", "LIN", "AFD", "BSW"),
  estimate = B7_point,
  lower = B7_ci[,1],
  upper = B7_ci[,2]
)


b7plot <-  ggplot(df, aes(x = reorder(party, -estimate), y = estimate)) +
  geom_col(fill = partycol, width = 0.7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2,
    linewidth = 0.8,
    color="grey"
  ) + 
  # point estimates above bars
  geom_text(
    aes(label = sprintf("%.2f", estimate)),
    vjust = -3.5,
    size = 3.5
  ) +
  labs(
    x = NULL,
    y = "Vote share"
  ) +
  # box around plot, no grid
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  # give headroom for labels
  ylim(0, 0.35)



pdf(file = "results_PR_Borda.pdf", width=10, height=8)
grid.arrange(
  fp3plot +
    ggtitle("Borda−3 Sample: PR Vote") +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")),
  
  b3plot +
    ggtitle("Borda−3 Sample: Borda Vote") +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")),
  
  fp7plot +
    ggtitle("Borda−7 Sample: PR Vote") +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")),
  
  b7plot +
    ggtitle("Borda−7 Sample: Borda Vote") +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")),
  
  ncol = 2
)
dev.off()

######################################################################




#############
#
# Eine Variable für die ersten 3 Ränge 
#

d$borda_1 <- ifelse(d$borda7==1, d$borda7_1, d$borda3_1)
d$borda_2 <- ifelse(d$borda7==1, d$borda7_2, d$borda3_2)
d$borda_3 <- ifelse(d$borda7==1, d$borda7_3, d$borda3_3)
d$borda_4 <- ifelse(d$borda7==1, d$borda7_4, NA)
d$borda_5 <- ifelse(d$borda7==1, d$borda7_5, NA)
d$borda_6 <- ifelse(d$borda7==1, d$borda7_6, NA)



d$partial_complete3_1 <- ifelse(d$borda3==1 & !is.na(d$borda3_1) & is.na(d$borda3_2)  & is.na(d$borda3_3), 1, 0)
d$partial_complete3_2 <- ifelse(d$borda3==1 & !is.na(d$borda3_1) & !is.na(d$borda3_2) & is.na(d$borda3_3), 1, 0)
d$partial_complete3_3 <- ifelse(d$borda3==1 & !is.na(d$borda3_1) & !is.na(d$borda3_2) & !is.na(d$borda3_3) , 1, 0)


d$partial_complete7_1 <- ifelse(d$borda7==1 & !is.na(d$borda7_1) & is.na(d$borda7_2)& is.na(d$borda7_3)& is.na(d$borda7_4)
                                            & is.na(d$borda7_5) & is.na(d$borda7_6), 1, 0)
d$partial_complete7_2 <- ifelse(d$borda7==1 & !is.na(d$borda7_1) & !is.na(d$borda7_2)& is.na(d$borda7_3)& is.na(d$borda7_4)
                                & is.na(d$borda7_5) & is.na(d$borda7_6), 1, 0)
d$partial_complete7_3 <- ifelse(d$borda7==1& !is.na(d$borda7_1) & !is.na(d$borda7_2)& !is.na(d$borda7_3)& is.na(d$borda7_4)
                                & is.na(d$borda7_5) & is.na(d$borda7_6), 1, 0)
d$partial_complete7_4 <- ifelse(d$borda7==1 & !is.na(d$borda7_1) & !is.na(d$borda7_2)& !is.na(d$borda7_3)& !is.na(d$borda7_4)
                                & is.na(d$borda7_5) & is.na(d$borda7_6), 1, 0)
d$partial_complete7_5 <- ifelse(d$borda7==1 & !is.na(d$borda7_1) & !is.na(d$borda7_2)& !is.na(d$borda7_3)& !is.na(d$borda7_4)
                                & !is.na(d$borda7_5) & is.na(d$borda7_6), 1, 0)
d$partial_complete7_6 <- ifelse(d$borda7==1 & !is.na(d$borda7_1) & !is.na(d$borda7_2)& !is.na(d$borda7_3)& !is.na(d$borda7_4)
                                & !is.na(d$borda7_5) & !is.na(d$borda7_6), 1, 0)


d$complete_ballot <- ifelse((d$partial_complete7_6==1 & d$borda7==1 )| 
                              (d$partial_complete3_3==1 & d$borda3==1), 1, 0)

# Frequencies of partially complete ballots
table(d$partial_complete3_1)
table(d$partial_complete3_2)
table(d$partial_complete3_3)

table(d$partial_complete7_1)
table(d$partial_complete7_2)
table(d$partial_complete7_3)
table(d$partial_complete7_4)
table(d$partial_complete7_5)
table(d$partial_complete7_6)




### Inspecting skalometer values of ranked und unranked parties



# what is the highest thermometer score for a respondent with incomplete ballot among the set of unranked parties?

skalo <- cbind(d$skalo_cdu,
               d$skalo_spd,
               d$skalo_fdp,
               d$skalo_gru,
               d$skalo_lin,
               d$skalo_afd,
               d$skalo_bsw)

### Borda 3 ###
skalo_unranked_b3 <-as.data.frame(matrix(NA, ncol=7, nrow=1028)) 
skalo_ranked_b3 <-as.data.frame(matrix(NA, ncol=7, nrow=1028)) 
iz<- 0
for(i in c(d$id[d$borda3==1])){
  iz <- iz+1
  ranked_i <- c(d$borda3_1[d$id==i], 
                d$borda3_2[d$id==i],
                d$borda3_3[d$id==i])
  unranked_i <- rep(FALSE,7)
  
  unranked_i[1] <- ifelse(any(ranked_i=="cdu", na.rm=TRUE), FALSE, TRUE)
  unranked_i[2] <- ifelse(any(ranked_i=="spd", na.rm=TRUE), FALSE, TRUE)
  unranked_i[3] <- ifelse(any(ranked_i=="fdp", na.rm=TRUE), FALSE, TRUE)
  unranked_i[4] <- ifelse(any(ranked_i=="gru", na.rm=TRUE), FALSE, TRUE)
  unranked_i[5] <- ifelse(any(ranked_i=="lin", na.rm=TRUE), FALSE, TRUE)
  unranked_i[6] <- ifelse(any(ranked_i=="afd", na.rm=TRUE), FALSE, TRUE)
  unranked_i[7] <- ifelse(any(ranked_i=="bsw", na.rm=TRUE), FALSE, TRUE)
  
  skalo_i <- skalo[d$id==i,]
  skalo_unranked_b3[iz,1:length(skalo_i[unranked_i])] <-  sort(skalo_i[unranked_i], na.last=TRUE)
  if(7-length(skalo_i[unranked_i])>0){
    skalo_ranked_b3[iz,1:c(7-length(skalo_i[unranked_i]))] <-  rev(sort(skalo_i[!(unranked_i)], na.last=TRUE))
  }}

# average skalo scores for ranked and unranked parties
apply(skalo_ranked_b3,2,mean,na.rm=TRUE)
apply(skalo_unranked_b3,2,mean,na.rm=TRUE)


## Borda 7 ###

skalo_unranked_b7 <-as.data.frame(matrix(NA, ncol=7, nrow=1034)) 
skalo_ranked_b7 <-as.data.frame(matrix(NA, ncol=7, nrow=1034)) 

iz<- 0
for(i in c(d$id[d$borda7==1])){
  iz <- iz+1
  ranked_i <- c(d$borda7_1[d$id==i], 
                d$borda7_2[d$id==i],
                d$borda7_3[d$id==i],
                d$borda7_4[d$id==i],
                d$borda7_5[d$id==i],
                d$borda7_6[d$id==i],
                d$borda7_7[d$id==i])
  unranked_i <- rep(FALSE,7)
  
  unranked_i[1] <- ifelse(any(ranked_i=="cdu", na.rm=TRUE), FALSE, TRUE)
  unranked_i[2] <- ifelse(any(ranked_i=="spd", na.rm=TRUE), FALSE, TRUE)
  unranked_i[3] <- ifelse(any(ranked_i=="fdp", na.rm=TRUE), FALSE, TRUE)
  unranked_i[4] <- ifelse(any(ranked_i=="gru", na.rm=TRUE), FALSE, TRUE)
  unranked_i[5] <- ifelse(any(ranked_i=="lin", na.rm=TRUE), FALSE, TRUE)
  unranked_i[6] <- ifelse(any(ranked_i=="afd", na.rm=TRUE), FALSE, TRUE)
  unranked_i[7] <- ifelse(any(ranked_i=="bsw", na.rm=TRUE), FALSE, TRUE)
  
  skalo_i <- skalo[d$id==i,]
  if(length(skalo_i[unranked_i])>0){
  skalo_unranked_b7[iz,1:length(skalo_i[unranked_i])] <-  sort(skalo_i[unranked_i], na.last=TRUE)}
  if(7-length(skalo_i[unranked_i])>0){
    skalo_ranked_b7[iz,1:c(7-length(skalo_i[unranked_i]))] <-  rev(sort(skalo_i[!(unranked_i)], na.last=TRUE))
  }}

# average skalo scores for ranked and unranked parties
apply(skalo_ranked_b7,2,mean,na.rm=TRUE)
apply(skalo_unranked_b7,2,mean,na.rm=TRUE)


# Histogramm of skalometer value for the best evaluated non-ranked party

table(skalo_ranked_b3[,3])
table(skalo_unranked_b3[,5])


pdf("strategic.pdf", width=8, height=8)
par(mfrow = c(2,2), mar = c(4,4,2,1))
scores <- c(-5:5)


ranked <- as.vector(table(skalo_ranked_b3[,2]))
unranked <- as.vector(table(skalo_unranked_b3[,6]))

barplot(ranked, names.arg = scores, ylim = c(0,190),
        main = "a) 2nd-ranked parties (Borda-3)",
        xlab = "Thermometer score (−5 to +5)",
        col = "grey70", border = NA)

barplot(unranked, names.arg = scores, ylim = c(0,190),
        main = "b) 2nd-unranked parties (Borda-3)",
        xlab = "Thermometer score (−5 to +5)",
        col = "grey70", border = NA)


ranked <- as.vector(table(skalo_ranked_b3[,3]))
unranked <- as.vector(table(skalo_unranked_b3[,5]))


barplot(ranked, names.arg = scores, ylim = c(0,190),
        main = "c) 3rd-ranked parties (Borda-3)",
        xlab = "Thermometer score (−5 to +5)",
        col = "grey70", border = NA)

barplot(unranked, names.arg = scores, ylim = c(0,190),
        main = "d) 3rd-unranked parties (Borda-3)",
        xlab = "Thermometer score (−5 to +5)",
        col = "grey70", border = NA)

dev.off()

table(skalo_ranked_b3[,2])


# N of respondents who report ballot of length 1:
sum(as.vector(table(skalo_unranked_b3[,6])))
# frequency of best ratings of their unranked parties:
table(skalo_unranked_b3[,6])
# N=36 did rate any of the 6 unranked parties positive, 90 did not


# N of respondents who report ballot of length 2:
sum(as.vector( table(skalo_unranked_b3[,5][ is.na(skalo_unranked_b3[,6]) ] )))
# frequency of best ratings of their unranked parties:
table(skalo_unranked_b3[,5][ is.na(skalo_unranked_b3[,6]) ] )
# N=67 did rate any of the 5 unranked parties positive, 245 did not


hist(skalo_unranked_b7[,5])
hist(skalo_ranked_b7[,3])





pdf("strategicB7.pdf", width=8, height=8)
par(mfrow = c(2,2), mar = c(4,4,2,1))
scores <- c(-5:5)




ranked <- as.vector(table(skalo_ranked_b7[,3]))
unranked <- as.vector(table(skalo_unranked_b7[,5]))


barplot(ranked, names.arg = scores, ylim = c(0,220),
        main = "a) 3rd-ranked parties (Borda-7)",
        xlab = "Thermometer score (−5 to +5)",
        col = "grey70", border = NA)

barplot(unranked, names.arg = scores, ylim = c(0,220),
        main = "b) 3rd-unranked parties (Borda-7)",
        xlab = "Thermometer score (−5 to +5)",
        col = "grey70", border = NA)





ranked <- as.vector(table(skalo_ranked_b7[,4]))
unranked <- as.vector(table(skalo_unranked_b7[,6]))

barplot(ranked, names.arg = scores, ylim = c(0,190),
        main = "c) 4th-ranked parties (Borda-7)",
        xlab = "Thermometer score (−5 to +5)",
        col = "grey70", border = NA)

barplot(unranked, names.arg = scores, ylim = c(0,190),
        main = "d) 4th-unranked parties (Borda-7)",
        xlab = "Thermometer score (−5 to +5)",
        col = "grey70", border = NA)

dev.off()




################


# Congruence, Golder and Stramski p.96: Absolute Citizen Congruence
#
# 1/N*sum(abs(voter-weighted mean(parties)))

partypos_econ <- c(d$econ_cdu_am[1],
                   d$econ_spd_am[1],
                   d$econ_fdp_am[1],
                   d$econ_gru_am[1],
                   d$econ_lin_am[1],
                   d$econ_afd_am[1],
                   d$econ_bsw_am[1])
partypos_immi <- c(d$immi_cdu_am[1],
                   d$immi_spd_am[1],
                   d$immi_fdp_am[1],
                   d$immi_gru_am[1],
                   d$immi_lin_am[1],
                   d$immi_afd_am[1],
                   d$immi_bsw_am[1])
partypos_klima <- c(d$klima_cdu_am[1],
                    d$klima_spd_am[1],
                    d$klima_fdp_am[1],
                    d$klima_gru_am[1],
                    d$klima_lin_am[1],
                    d$klima_afd_am[1],
                    d$klima_bsw_am[1])


# Plots for policy issue preference data
d$comb_weights <- ifelse(d$borda3==1, d$b3_weights, d$b7_weights)

pdf(file = "policypositions.pdf", width=8, height=10)
par(mfrow=c(3,1), cex=0.9)
plot(density(d$econ_ego_am, na.rm=TRUE, weights=d$comb_weights),
     main="a) More social benefits vs. lower taxes", xlim=c(-1.8,1.8))
points(partypos_econ, rep(0,7), col=partycol, pch=19)
text(partypos_econ, rep(0,7), c("C", "S", "F", "G", "L", "A", "B"), pos=3, cex=0.7)
plot(density(d$klima_ego_am, na.rm=TRUE, weights=d$comb_weights),
     main="b) More vs. less climate change mitigation", xlim=c(-1.8,1.8))
points(partypos_klima, rep(0,7), col=partycol, pch=19)
text(partypos_klima, rep(0,7), c("C", "S", "F", "G", "L", "A", "B"), pos=3, cex=0.7)
plot(density(d$immi_ego_am, na.rm=TRUE, weights=d$comb_weights),
     main="c) Facilitate vs. restrict immigration", xlim=c(-1.8,1.8))
points(partypos_immi, rep(0,7), col=partycol, pch=19)
text(partypos_immi, rep(0,7), c("C", "S", "F", "G", "L", "A", "B"), pos=3, cex=0.7)
dev.off()



#####################################################################
#
# Many to many congruence: difference between cumulative density curves 
#
# use bootstrap to account for sampling bias + uncertainty

weighted_boot <- function(data, congruence, R = 1000, weight_var, ppos, res) {
  n <- nrow(data)
  boot_stats <- matrix(NA, nrow=R, ncol=3)  # 3 KS statistics
  
  
  for (i in 1:R) {
    indices <- sample(1:n, size = n, replace = TRUE, prob = data[[weight_var]])
    boot_stats[i,] <- congruence(data[indices, ], ppos, res)
  }
  
  return(boot_stats)
}



congruence <- function(df,ppos, res){
  
  party_econ <- c(rep(ppos[1,], round(630*res,0)))
  party_immi <- c(rep(ppos[2,], round(630*res,0)))
  party_klima <- c(rep(ppos[3,], round(630*res,0)))
  
  # many to many congruence econ
  c(ks.test(df$econ_ego_am, party_econ)$statistic,
    ks.test(df$immi_ego_am, party_immi)$statistic,
    ks.test(df$klima_ego_am, party_klima)$statistic)
}


partypos <- rbind(partypos_econ,partypos_immi,partypos_klima)

congruence(db3, ppos=partypos, res=FP3_point)

KS_PR3 <- weighted_boot(db3, congruence, R = 1000, 
              weight_var="b3_weights",
              ppos=partypos, 
              res=FP3_point)

# KS test statistic and 95% confidence intervals
apply(KS_PR3,2,median)
apply(KS_PR3,2,quantile, , probs = c(0.025, 0.975))



KS_Borda3 <- weighted_boot(db3, congruence, R = 1000, 
                           weight_var="b3_weights",
                           ppos=partypos, 
                           res=B3_point)

# KS test statistic and 95% confidence intervals
apply(KS_Borda3,2,median)
apply(KS_Borda3,2,quantile, , probs = c(0.025, 0.975))



KS_PR7 <- weighted_boot(db7, congruence, R = 1000, 
                        weight_var="b7_weights",
                        ppos=partypos, 
                        res=FP7_point)

# KS test statistic and 95% confidence intervals
apply(KS_PR7,2,median)
apply(KS_PR7,2,quantile, , probs = c(0.025, 0.975))


KS_Borda7 <- weighted_boot(db7, congruence, R = 1000, 
                           weight_var="b7_weights",
                           ppos=partypos, 
                           res=B7_point)

# KS test statistic and 95% confidence intervals
apply(KS_Borda7,2,median)
apply(KS_Borda7,2,quantile, , probs = c(0.025, 0.975))


 
            
library(ggplot2)
#plot Econ
# example data
df <- data.frame(x = d$econ_ego_am, w = d$comb_weights)

# sort by x
df <- df[order(df$x), ]

# compute cumulative sum of weights, normalized to 1
df$ecdf_w <- cumsum(df$w) / sum(df$w)


dfecon <- data.frame(
  x = c(d$econ_ego_am, dist_PR_econ, dist_Borda3_econ, dist_Borda7_econ),
  g = c(rep("voters", length(d$econ_ego_am)), rep("PR", 630), 
        rep("Borda3", 630), rep("Borda7", 631))
)
pecon <- ggplot(dfecon, aes(x, colour = g)) +
  stat_ecdf() +
  labs(
    title = "Economy",
    x = "Economic policy",
    y = "Cumulative Density",
    colour = NULL
  ) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )


#Plot immi
dfimmi <- data.frame(
  x = c(d$immi_ego_am, dist_PR_immi, dist_Borda3_immi, dist_Borda7_immi),
  g = c(rep("voters", length(d$immi_ego_am)), rep("PR", 630), 
        rep("Borda3", 630), rep("Borda7", 631))
)


pimmi <- ggplot(dfimmi, aes(x, colour = g)) +
  stat_ecdf() +
  labs(
    x = "Immigration policy",
    y = "Cumulative Density",
    colour = NULL
  ) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

#plot Klima
dfklima <- data.frame(
  x = c(d$klima_ego_am, dist_PR_klima, dist_Borda3_klima, dist_Borda7_klima),
  g = c(rep("voters", length(d$klima_ego_am)), rep("PR", 630), 
        rep("Borda3", 630), rep("Borda7", 631))
)

pklima <- ggplot(dfklima, aes(x, colour = g)) +
  stat_ecdf() +
  labs(
    x = "CLimate policy",
    y = "Cumulative Density",
    colour = NULL
  ) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )
    

pimmi <- pimmi +
  labs(title = "Immigration") +
  theme(legend.position = "none")

pklima <- pklima +
  labs(title = "Climate") +
  theme(legend.position = "none")

library(patchwork)

pdf(file = "congruence_borda.pdf", width=8, height=4)
(pecon | pimmi | pklima) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
dev.off()


# many to many congruence econ
ks.test(d$econ_ego_am, dist_PR_econ)
ks.test(d$econ_ego_am, dist_Borda3_econ)
ks.test(d$econ_ego_am, dist_Borda7_econ)

# many to many congruence immi
ks.test(d$immi_ego_am, dist_PR_immi)
ks.test(d$immi_ego_am, dist_Borda3_immi)
ks.test(d$immi_ego_am, dist_Borda7_immi)

# many to many congruence klima
ks.test(d$klima_ego_am, dist_PR_klima)
ks.test(d$klima_ego_am, dist_Borda3_klima)
ks.test(d$klima_ego_am, dist_Borda7_klima)





############
#
#
# Evaluation Borda
#


hist(d$Q24)

b3 <- table(d$Q24[d$borda3==1])/1028
b7 <- table(d$Q24[d$borda7==1])/1034
names(b3) <- c("Very bad", "Bad", "Mixed", "Good", "Very good")
names(b7) <- c("Very bad", "Bad", "Mixed", "Good", "Very good")

b  <- rbind(b3,b7)


pdf(file = "eval_borda.pdf", width=8, height=4)
par(mfrow=c(1,1))
bp <-  barplot(b, col=mycol[1:2], beside = TRUE, border="white", ylim=c(0,0.5),
             main=" " )
box()
legend("topright", legend=c("Borda-3", "Borda-7"), fill=mycol[1:2], border="white",
        horiz=TRUE, bty="n")
dev.off()

d$borda_pos <- ifelse(d$Q24<3, 1, 0)
d$borda_neg <- ifelse(d$Q24>3, 1, 0)

d$borda_eval <- ifelse(d$Q24<3, "positive", "neutral")
d$borda_eval[d$Q24>3] <- "negative"




library(nnet)

# Stelle sicher, dass die abhängige Variable ein Faktor ist
d$borda_eval <- as.factor(d$borda_eval)
d$borda_eval <-relevel(d$borda_eval, ref = "neutral")


d$votersOF <-relevel(as.factor(d$vote1), ref = "gru")




# Schätze ein multinomiales Logit-Modell
model_p3a <- multinom(borda_eval ~   pol_wissen + polint + 
                      edu + age + satisdem + male + ost + votersOF,
                      data = d[d$borda3==1,])

summary(model_p3a)

model_p3b <- multinom(borda_eval ~  
                        #borda3 + 
                        votersOF,
                      data = d[d$borda3==1,])

summary(model_p3b)
model_p7a <- multinom(borda_eval ~   pol_wissen + polint +
                       edu + age + satisdem + male + ost ,
                     data = d[d$borda7==1,])

summary(model_p7a)
model_p7b <- multinom(borda_eval ~   
                          votersOF,
                      data = d[d$borda7==1,])

summary(model_p7b)

library(stargazer)

stargazer(
  model_p3a,  model_p7a, 
  type = "latex",
  title = "Multinomial logit models of Borda evaluation",
  label = "tab:borda_multinom",
  column.labels = c("Borda-3 a)", "Borda-3 b)", "Borda-7 a)", "Borda-7 b)"),
  dep.var.labels.include = FALSE,  # Remove dependent variable label if unnecessary
  omit.stat = c("theta"),     # Optional: remove extra stats if you want
  star.cutoffs = c( 0.05, 0.01, 0.001),
  single.row = FALSE,               # Combine coefficient and SE in one row
  float.env = "table",
  no.space = TRUE,                  # Remove extra spaces for a tighter table
  header = TRUE,
  digits=2
)


stargazer(
    model_p3b,  model_p7b,
  type = "latex",
  title = "Multinomial logit models of Borda evaluation",
  label = "tab:borda_multinom",
  column.labels = c("Borda-3 ", "Borda-7 "),
  dep.var.labels.include = FALSE,  # Remove dependent variable label if unnecessary
  omit.stat = c("LL","theta"),     # Optional: remove extra stats if you want
  star.cutoffs = c( 0.05, 0.01, 0.001),
  single.row = FALSE,               # Combine coefficient and SE in one row
  float.env = "table",
  no.space = TRUE,                  # Remove extra spaces for a tighter table
  header = TRUE,
  digits=2
)






# Schätze ein multinomiales Logit-Modell
model_soc <- multinom(borda_eval ~  pol_wissen + polint +
                    edu + age + satisdem + male + ost +
                    borda3,
                  data = d)

summary(model_soc)

library(stargazer)
stargazer(model_soc)


# OLS
model_ols <- lm(-1*(Q24-5) ~  pol_wissen + polint +
                        edu + age + satisdem + male + ost +
                        borda3,
                      data = d)

summary(model_ols)

# OLS
d$fvote <- as.factor(d$vote1)
d$fvote <- relevel(d$fvote, ref="cdu")
model_ols2 <- lm(-1*(Q24-5) ~ pol_wissen + polint +
                   edu + age + satisdem + male + ost +
                   borda3 +fvote,
                data = d)

summary(model_ols2)

stargazer(model_ols, model_ols2, digits=2, style="apsr")

eval <- table(-1*(d$Q24-5))
eval_cdu <- table(-1*(d$Q24[d$vote1=="cdu"]-5))
eval_spd <- table(-1*(d$Q24[d$vote1=="spd"]-5))
eval_fdp <- table(-1*(d$Q24[d$vote1=="fdp"]-5))
eval_gru <- table(-1*(d$Q24[d$vote1=="gru"]-5))
eval_lin <- table(-1*(d$Q24[d$vote1=="lin"]-5))
eval_afd <- table(-1*(d$Q24[d$vote1=="afd"]-5))
eval_bsw <- table(-1*(d$Q24[d$vote1=="bsw"]-5))



pdf(file = "Eval_borda.pdf", width=10, height=8)
par(mfrow = c(2, 4))
barplot(eval, main="all")
barplot(eval_cdu, col="black", main="CDU")
barplot(eval_spd, col="red", main="SPD")
barplot(eval_fdp, col="gold", main="FDP")
barplot(eval_gru, col="darkgreen", main="Greens")
barplot(eval_lin, col="magenta", main="Linke")
barplot(eval_afd, col="cornflowerblue", main="AfD")
barplot(eval_bsw, col="purple", main="BSW")
dev.off()


###################################################################################
#
#
#  Transformation ins long Format
#
#

# Datensatz auf non-missings in der vote Variable reduzieren
X <- as.data.frame(d[!is.na(d$vote),])

party <- c("cdu", "spd", "fdp", "gru", "lin", "afd", "bsw")

long <- reshape(X, direction="long", idvar="id", timevar="party",
                times=party,
                varying=list(
                  c("skalo_cdu", "skalo_spd", "skalo_fdp", "skalo_gru",
                    "skalo_lin", "skalo_afd", "skalo_bsw"),
                 c("skalo_merz", "skalo_scholz", "skalo_lindner", "skalo_habeck",
                   "skalo_bartsch", "skalo_weidel", "skalo_wagenknecht"),
                  c("econ_cdu_am", "econ_spd_am", "econ_fdp_am", "econ_gru_am",
                    "econ_lin_am", "econ_afd_am", "econ_bsw_am"),
                  c("immi_cdu_am", "immi_spd_am", "immi_fdp_am", "immi_gru_am",
                    "immi_lin_am", "immi_afd_am", "immi_bsw_am"),
                c("klima_cdu_am", "klima_spd_am", "klima_fdp_am", "klima_gru_am",
                  "klima_lin_am", "klima_afd_am", "klima_bsw_am")))


# Vote Choice als Dummy

long$choice <- 0
long$choice[long$vote==1 & long$party=="cdu"] <- 1
long$choice[long$vote==3 & long$party=="spd"] <- 1
long$choice[long$vote==4 & long$party=="fdp"] <- 1
long$choice[long$vote==5 & long$party=="gru"] <- 1
long$choice[long$vote==6 & long$party=="lin"] <- 1
long$choice[long$vote==7 & long$party=="afd"] <- 1
long$choice[long$vote==8 & long$party=="bsw"] <- 1



# 1st borda rank

long$borda_r1 <- 0
long$borda_r1[long$borda_1=="cdu" & long$party=="cdu"] <- 1
long$borda_r1[long$borda_1=="spd" & long$party=="spd"] <- 1
long$borda_r1[long$borda_1=="fdp" & long$party=="fdp"] <- 1
long$borda_r1[long$borda_1=="gru" & long$party=="gru"] <- 1
long$borda_r1[long$borda_1=="lin" & long$party=="lin"] <- 1
long$borda_r1[long$borda_1=="afd" & long$party=="afd"] <- 1
long$borda_r1[long$borda_1=="bsw" & long$party=="bsw"] <- 1



# 2nd borda rank

long$borda_r2 <- 0
long$borda_r2[long$borda_2=="cdu" & long$party=="cdu"] <- 1
long$borda_r2[long$borda_2=="spd" & long$party=="spd"] <- 1
long$borda_r2[long$borda_2=="fdp" & long$party=="fdp"] <- 1
long$borda_r2[long$borda_2=="gru" & long$party=="gru"] <- 1
long$borda_r2[long$borda_2=="lin" & long$party=="lin"] <- 1
long$borda_r2[long$borda_2=="afd" & long$party=="afd"] <- 1
long$borda_r2[long$borda_2=="bsw" & long$party=="bsw"] <- 1

long$borda_r2[long$borda_r1==1] <- NA


# 3rd borda rank

long$borda_r3 <- 0
long$borda_r3[long$borda_3=="cdu" & long$party=="cdu"] <- 1
long$borda_r3[long$borda_3=="spd" & long$party=="spd"] <- 1
long$borda_r3[long$borda_3=="fdp" & long$party=="fdp"] <- 1
long$borda_r3[long$borda_3=="gru" & long$party=="gru"] <- 1
long$borda_r3[long$borda_3=="lin" & long$party=="lin"] <- 1
long$borda_r3[long$borda_3=="afd" & long$party=="afd"] <- 1
long$borda_r3[long$borda_3=="bsw" & long$party=="bsw"] <- 1

long$borda_r3[long$borda_r1==1] <- NA
long$borda_r3[long$borda_r2==1] <- NA


# 4th borda rank

long$borda_r4 <- 0
long$borda_r4[long$borda_4=="cdu" & long$party=="cdu"] <- 1
long$borda_r4[long$borda_4=="spd" & long$party=="spd"] <- 1
long$borda_r4[long$borda_4=="fdp" & long$party=="fdp"] <- 1
long$borda_r4[long$borda_4=="gru" & long$party=="gru"] <- 1
long$borda_r4[long$borda_4=="lin" & long$party=="lin"] <- 1
long$borda_r4[long$borda_4=="afd" & long$party=="afd"] <- 1
long$borda_r4[long$borda_4=="bsw" & long$party=="bsw"] <- 1

long$borda_r4[long$borda_r1==1] <- NA
long$borda_r4[long$borda_r2==1] <- NA
long$borda_r4[long$borda_r3==1] <- NA

# 5th borda rank

long$borda_r5 <- 0
long$borda_r5[long$borda_5=="cdu" & long$party=="cdu"] <- 1
long$borda_r5[long$borda_5=="spd" & long$party=="spd"] <- 1
long$borda_r5[long$borda_5=="fdp" & long$party=="fdp"] <- 1
long$borda_r5[long$borda_5=="gru" & long$party=="gru"] <- 1
long$borda_r5[long$borda_5=="lin" & long$party=="lin"] <- 1
long$borda_r5[long$borda_5=="afd" & long$party=="afd"] <- 1
long$borda_r5[long$borda_5=="bsw" & long$party=="bsw"] <- 1

long$borda_r5[long$borda_r1==1] <- NA
long$borda_r5[long$borda_r2==1] <- NA
long$borda_r5[long$borda_r3==1] <- NA
long$borda_r5[long$borda_r4==1] <- NA

# 6th borda rank

long$borda_r6 <- 0
long$borda_r6[long$borda_6=="cdu" & long$party=="cdu"] <- 1
long$borda_r6[long$borda_6=="spd" & long$party=="spd"] <- 1
long$borda_r6[long$borda_6=="fdp" & long$party=="fdp"] <- 1
long$borda_r6[long$borda_6=="gru" & long$party=="gru"] <- 1
long$borda_r6[long$borda_6=="lin" & long$party=="lin"] <- 1
long$borda_r6[long$borda_6=="afd" & long$party=="afd"] <- 1
long$borda_r6[long$borda_6=="bsw" & long$party=="bsw"] <- 1

long$borda_r6[long$borda_r1==1] <- NA
long$borda_r6[long$borda_r2==1] <- NA
long$borda_r6[long$borda_r3==1] <- NA
long$borda_r6[long$borda_r4==1] <- NA
long$borda_r6[long$borda_r5==1] <- NA


long$borda_ro <-  long$borda_r1 
long$borda_ro[long$borda_r2==1 & !is.na(long$borda_r2)] <- 2
long$borda_ro[long$borda_r3==1 & !is.na(long$borda_r3)] <- 3

# policy distance zwischen W?hlerinnen und Parteien

long$dist2econ <- sqrt((long$econ_ego_am - long$econ_cdu_am)^2)
long$dist2immi <- sqrt((long$immi_ego_am - long$immi_cdu_am)^2)
long$dist2klima <- sqrt((long$klima_ego_am - long$klima_cdu_am)^2)


long$kanzlerkand <- ifelse(long$party=="cdu" |
                               long$party=="spd" |
                               long$party=="gru" |
                               long$party=="afd" , long$skalo_merz, 0)

long$skalo_cand <- long$skalo_merz
long$skalo_party <- long$skalo_cdu
####################################################################################
#  Conditional Logit Analyse
    


        # Erststimme
        model_vote1 <- clogit(choice ~ skalo_cand+  party + 
                            dist2econ +
                            dist2immi +
                             dist2klima +
                            strata(id), data=long)
        summary(model_vote1)
        #
        
        
        
        
        
        # 1. Borda Rang
        model_borda1 <- clogit(borda_r1 ~ skalo_cand+  party + 
                                 dist2econ +
                                 dist2immi +
                                 dist2klima +
                                 strata(id), data=long)
        summary(model_borda1)
        # 
        
        
        # 2. Borda Rang
        model_borda2 <- clogit(borda_r2 ~ skalo_cand+  party + 
                                 dist2econ +
                                 dist2immi +
                                 dist2klima +
                                 strata(id), data=long)
        summary(model_borda2)
        #  
        
        
        # 3. Borda Rang
        model_borda3 <- clogit(borda_r3 ~ skalo_cand+   party +
                                 dist2econ +
                                 dist2immi +
                                 dist2klima +
                                 strata(id), data=long)
        summary(model_borda3)
        #  
        
        
        
        # 4. Borda Rang
        model_borda4 <- clogit(borda_r4 ~ skalo_cand+   party +
                                 dist2econ +
                                 dist2immi +
                                 dist2klima +
                                 strata(id), data=long)
        summary(model_borda4)
        #  
        # 5. Borda Rang
        model_borda5 <- clogit(borda_r5 ~ skalo_cand+   party +
                                 dist2econ +
                                 dist2immi +
                                 dist2klima +
                                 strata(id), data=long)
        summary(model_borda5)
        #  
        
        # 6. Borda Rang
        model_borda6 <- clogit(borda_r6 ~ skalo_cand+   party +
                                 dist2econ +
                                 dist2immi +
                                 dist2klima +
                                 strata(id), data=long)
        summary(model_borda6)
        #  
        
        
        
        
        stargazer(model_borda1, 
                  model_borda2,
                  model_borda3,
                  model_borda4,
                  model_borda5,
                  model_borda6, digits=2 )
        
        
        
        
        cof1 <- model_borda1$coefficients[c(1,8:10)]
        cof1[2:4] <- -1*cof1[2:4]
        cof1_sd <- coef(summary(model_borda1))[c(1,8:10),3]
        cof2 <- model_borda2$coefficients[c(1,8:10)]
        cof2[2:4] <- -1*cof2[2:4]
        cof2_sd <- coef(summary(model_borda2))[c(1,8:10),3]
        cof3 <- model_borda3$coefficients[c(1,8:10)]
        cof3[2:4] <- -1*cof3[2:4]
        cof3_sd <- coef(summary(model_borda3))[c(1,8:10),3]
        cof4 <- model_borda4$coefficients[c(1,8:10)]
        cof4[2:4] <- -1*cof4[2:4]
        cof4_sd <- coef(summary(model_borda4))[c(1,8:10),3]
        cof5 <- model_borda5$coefficients[c(1,8:10)]
        cof5[2:4] <- -1*cof5[2:4]
        cof5_sd <- coef(summary(model_borda5))[c(1,8:10),3]
        cof6 <- model_borda6$coefficients[c(1,8:10)]
        cof6[2:4] <- -1*cof6[2:4]
        cof6_sd <- coef(summary(model_borda6))[c(1,8:10),3]
        
        cof <- rbind(cof1,cof2,cof3,cof4,cof5,cof6)
        cof_sd <- rbind(cof1_sd,cof2_sd,cof3_sd,cof4_sd,cof5_sd,cof6_sd)
        
        lower <- cof-1.96*cof_sd
        upper <- cof+1.96*cof_sd
        
        
        library(paletteer)
        
        mycol <-c("#6497B1FF", "#CD1076FF", "#FFB04FFF", "#679C35FF")
        
        
        pdf(file = "coefplot_spatialborda.pdf", width=10)
        par(mfrow=c(2,2), cex=0.9)
        
        plot( rev(cof[,1]),c(1:6), pch=19, xlim=c(0,0.6), ylim=c(0,7),
               ylab="Rank No.",
              xlab="Effect size" , main="Candidate evaluation", axes=F)
        axis(1)
        axis(2, at=c(1:6), labels = c(6:1), las=2)
        arrows(lower[,1], c(6:1), upper[,1],c(6:1),  length=0)
        lines(x=c(0,0),y=c(0,7), col="gray")
        
        plot( cof[,2],c( 6:1), pch=19, xlim=c(-1,2),ylim=c(0,7),
              ylab="Rank No.",
              xlab="Effect size" , main="Spatial distance: economy", axes=F)
        axis(1)
        axis(2, at=c(1:6), labels = c(6:1), las=2)
        arrows(lower[,2], c( 6:1), upper[,2], c( 6:1),length=0)
        lines(x=c(0,0),y=c(0,7), col="gray")
        
        plot(  cof[,3],c( 6:1 ),pch=19, xlim=c(-1,2), ylim=c(0,7),
               ylab="Rank No.",
              xlab="Effect size" , main="Spatial distance: immigration", axes=F)
        axis(1)
        axis(2, at=c(1:6), labels = c(6:1), las=2)
        arrows( lower[,3],c( 6:1 ),  upper[,3],c( 6:1 ), length=0)
        lines(x=c(0,0),y=c(0,7), col="gray")
        
        plot(cof[,4],c( 6:1 ),pch=19, xlim=c(-1,2), ylim=c(0,7),
             ylab="Rank No.",
              xlab="Effect size" , main="Spatial distance: climate", axes=F)
        axis(1)
        axis(2, at=c(1:6), labels = c(6:1), las=2)
        arrows( lower[,4], c( 6:1 ),   upper[,4],c( 6:1 ), length=0)
        lines(x=c(0,0),y=c(0,7), col="gray")
        
       dev.off()
        
        
        barplot(cof1, col=mycol, border="white", main="1. Rang",
                names.arg=c("Kandidat*in", "Wirtschaft", "Immigration", "Klima"),
                ylab="Effektgröße")
        barplot(cof2, col=mycol, border="white", main="2. Rang",
                names.arg=c("Kandidat*in", "Wirtschaft", "Immigration", "Klima"),
                ylab="Effektgröße")
        barplot(cof3, col=mycol, border="white", main="3. Rang",
                names.arg=c("Kandidat*in", "Wirtschaft", "Immigration", "Klima"),
                ylab="Effektgröße")
        barplot(cof4, col=mycol, border="white", main="4. Rang",
                names.arg=c("Kandidat*in", "Wirtschaft", "Immigration", "Klima"),
                ylab="Effektgröße")
        barplot(cof5, col=mycol, border="white", main="5. Rang",
                names.arg=c("Kandidat*in", "Wirtschaft", "Immigration", "Klima"),
                ylab="Effektgröße")
        barplot(cof6, col=mycol, border="white", main="6. Rang",
                names.arg=c("Kandidat*in", "Wirtschaft", "Immigration", "Klima"),
                ylab="Effektgröße")
        
       # Rank ordered logit
        library(ROlogit)
          
        # Modellschätzung (mit individuumspezifischen Kovariaten)
        mod <- rologit(yvar="borda_ro", evar=c("skalo_cand", "party",
                                                 "dist2econ",
                                                 "dist2immi",
                                                 "dist2klima"),
                       svar="id", dat = long)
        summary(mod)
        
        
        
        
        library(mlogit)
        long_mlogit <- mlogit.data(long, shape="long", choice="borda_ro",  ranked=TRUE, 
                                   alt.var="party", id.var="ID")