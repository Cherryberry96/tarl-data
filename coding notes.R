f <- "~/Desktop/Repos/tarl-data/TARL Project - Data.xlsx"
library(readxl)
TD <- read_excel(f, sheet = 7, col_names = TRUE)
head(TD)
TD
View(TD)

TD1 <- filter(TD, Age=="Adult" | Age=="Adult1" | Age=="Adult2" | Age=="OA")


library(dplyr)
library(ggplot2)


library(skimr)
library(kableExtra)
c <- skim(TD1)
c <- skim_to_wide(TD1)
c %>% select(-type, -missing, -complete, -n, -empty, -n_unique) %>% kable() %>% kable_styling(font_size = 10)



library(summarytools)
s1 <- descr(TD1, style = "rmarkdown", transpose = TRUE)
s1 %>% summarytools::view()
s1 <- dfSummary(TD1, style = "grid", plain.ascii = FALSE)
s1 %>% summarytools::view()




par(mfrow = c(1, 2)) 
attach(TD1)  
hist(log(LLC), freq = FALSE, col = "red", main = "Hist of LLC (using Log)", xlab = "log(LLC)", 
     ylab = "density", ylim = c(0, 3))
abline(v = mean(log(LLC), na.rm = TRUE))


hist(log(RLC), freq = FALSE, col = "red", main = "Hist of RLC (using Log)", xlab = "log(RLC)", 
     ylab = "density", ylim = c(0, 3))
abline(v = mean(log(RLC), na.rm = TRUE))


EW <- filter(TD1, Site=="Ernest Witte")
View(EW)
table(EW$Abscesses)
table(EW$Caries)
table(EW$LEH)

table(EW$LEH,EW$Caries)
table(EW$LEH,EW$Abscesses)
chisq.test(EW$LEH,EW$Abscesses)
EWA <-chisq.test(EW$LEH,EW$Abscesses)
EWA$observed
EWA$expected

chisq.test(EW$LEH,EW$Caries)
EWC <- chisq.test(EW$LEH,EW$Caries)
EWC$observed
EWC$expected
EWC



EWM <- filter(EW, Sex=="M")
table(EWM$Abscesses)
table(EWM$Caries)
chisq.test(EWM$LEH,EWM$Abscesses)
EWMA <-chisq.test(EWM$LEH,EWM$Abscesses)
EWMA$observed
EWMA$expected

chisq.test(EWM$LEH,EWM$Caries)
EWMC <- chisq.test(EWM$LEH,EWM$Caries)
EWMC$observed
EWMC$expected


EWF <- filter(EW, Sex=="F")
table(EWF$Abscesses)
table(EWF$Caries)
chisq.test(EWF$LEH,EWF$Abscesses)
EWFA <-chisq.test(EWF$LEH,EWF$Abscesses)
EWFA$observed
EWFA$expected

chisq.test(EWF$LEH,EWF$Caries)
EWFC <- chisq.test(EWF$LEH,EWF$Caries)
EWFC$observed
EWFC$expected


EWI <- filter(EW, Sex=="I")
table(EWI$Abscesses)
table(EWI$Caries)
chisq.test(EWI$LEH,EWI$Abscesses)
EWIA <-chisq.test(EWI$LEH,EWI$Abscesses)
EWIA$observed
EWIA$expected

chisq.test(EWI$LEH,EWI$Caries)
EWIC <- chisq.test(EWI$LEH,EWI$Caries)
EWIC$observed
EWIC$expected







M <- filter(TD1, Site=="Morhiss")
View(M)
table(M$Abscesses)
table(M$Caries)
table(M$LEH)

table(M$LEH,M$Caries)
table(M$LEH,M$Abscesses)
chisq.test(M$LEH,M$Abscesses)
MA <-chisq.test(M$LEH,M$Abscesses)
MA$observed
MA$expected

chisq.test(M$LEH,M$Caries)
MC <- chisq.test(M$LEH,M$Caries)
MC$observed
MC$expected
MC



MM <- filter(M, Sex=="M")
View(MM)
table(MM$Abscesses)
table(MM$Caries)
table(MM$LEH)

table(MM$LEH,MM$Caries)
table(MM$LEH,MM$Abscesses)
chisq.test(MM$LEH,MM$Abscesses)
MMA <-chisq.test(MM$LEH,MM$Abscesses)
MMA$observed
MMA$expected

chisq.test(MM$LEH,MM$Caries)
MMC <- chisq.test(MM$LEH,MM$Caries)
MMC$observed
MMC$expected
MMC



MF <- filter(M, Sex=="F")
View(MF)
table(MF$Abscesses)
table(MF$Caries)
table(MF$LEH)

table(MF$LEH,MF$Caries)
table(MF$LEH,MF$Abscesses)
chisq.test(MF$LEH,MF$Abscesses)
MFA <-chisq.test(MF$LEH,MF$Abscesses)
MFA$observed
MFA$expected

chisq.test(MF$LEH,MF$Caries)
MFC <- chisq.test(MF$LEH,MF$Caries)
MFC$observed
MFC$expected
MFC

MI <- filter(M, Sex=="I")
View(MI)
table(MI$Abscesses)
table(MI$Caries)
table(MI$LEH)

table(MI$LEH,MI$Caries)
table(MI$LEH,MI$Abscesses)
chisq.test(MI$LEH,MI$Abscesses)
MIA <-chisq.test(MI$LEH,MI$Abscesses)
MIA$observed
MIA$expected

chisq.test(MI$LEH,MI$Caries)
MIC <- chisq.test(MI$LEH,MI$Caries)
MIC$observed
MIC$expected
MIC













d <- filter(TD, Site=="Ernest Witte")
View(d)
table(d$Abscesses)
table(d$Caries)
table(d$LEH)
summary(d$Caries,d$LEH)
table(d$LEH,d$Caries)
table(d$LEH,d$Abscesses)
chisq.test(d$LEH,d$Abscesses)
chisq <-chisq.test(d$LEH,d$Abscesses)
chisq$observed
chisq$expected
chisq.test(d$LEH,d$Caries)
chisq.test(d$LEH,d$Caries) -> s
s$observed
s$expected
s




m <- filter(TD, Site=="Morhiss")
View(m)
table(m$LEH, m$Abscesses)
chisq1 <- chisq.test(m$LEH, m$Abscesses)
chisq1
chisq1$observed
chisq1$expected
summary(chisq1)

chisq2 <- chisq.test(m$LEH, m$Caries)
chisq2
chisq2$observed
chisq2$expected

chisq.test(m$LEH,m$Caries)
chisq.test(m$LEH,m$Caries) -> t
t$observed
t$expected
t

EW1 <- filter(EW, LEH=="1")

par(mfrow = c(1, 2)) 
attach(EW1)  
hist(log(RUI1), freq = FALSE, col = "red", main = "Hist of RUI1", xlab = "log(RUI1)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(RUI1), na.rm = TRUE))


hist(log(LUI1), freq = FALSE, col = "red", main = "Hist of LUI1", xlab = "log(LUI1)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(LUI1), na.rm = TRUE))


hist(log(RUI2), freq = FALSE, col = "red", main = "Hist of RUI2", xlab = "log(RUI2)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(RUI2), na.rm = TRUE))


hist(log(LUI2), freq = FALSE, col = "red", main = "Hist of LUI2", xlab = "log(LUI2)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(LUI2), na.rm = TRUE))


hist(log(RUC), freq = FALSE, col = "red", main = "Hist of RUC", xlab = "log(RUC)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(RUC), na.rm = TRUE))


hist(log(LUC), freq = FALSE, col = "red", main = "Hist of LUC", xlab = "log(LUC)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(LUC), na.rm = TRUE))

hist(log(RLI1), freq = FALSE, col = "red", main = "Hist of RLI1", xlab = "log(RLI1)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(RLI1), na.rm = TRUE))

hist(log(LLI1), freq = FALSE, col = "red", main = "Hist of LLI1", xlab = "log(LLI1)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(LLI1), na.rm = TRUE))


hist(log(RLI2), freq = FALSE, col = "red", main = "Hist of RLI2", xlab = "log(RLI2)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(RLI2), na.rm = TRUE))


hist(log(LLI2), freq = FALSE, col = "red", main = "Hist of LLI2", xlab = "log(LLI2)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(LLI2), na.rm = TRUE))

hist(log(RLC), freq = FALSE, col = "red", main = "Hist of RLC", xlab = "log(RLC)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(RLC), na.rm = TRUE))


hist(log(LLC), freq = FALSE, col = "red", main = "Hist of LLC", xlab = "log(LLC)", 
     ylab = "density", ylim = c(0, 5))
abline(v = mean(log(LLC), na.rm = TRUE))



par(mfrow = c(1, 2))
plot(as.factor(EW$Abscesses), EW$RUI1, xlab = "LEH", ylab = "RUI1", col = "salmon")
plot(as.factor(EW$Abscesses), EW$LUI1, xlab = "LEH", ylab = "LUI1", col = "pink")
logistic <- glm(data = EW1, Abscesses ~ RUI1, family = "binomial")
summary(logistic)

par(mfrow = c(1, 2))
plot(as.factor(EW1$Abscesses), EW1$RLC, xlab = "Abscesses", ylab = "RLC", col = "lightgreen")
plot(as.factor(EW1$Abscesses), EW1$LLC, xlab = "Abscesses", ylab = "LLC", col = "green")


logistic <- glm(data = EW1, Abscesses ~ RLC, family = "binomial")
summary(logistic)


EW1 <- filter(EW, LEH=="1")

par(mfrow = c(1, 2))
plot(as.factor(EW$LEH), EW$RLC, xlab = "LEH", ylab = "RLC", col = "salmon")
plot(as.factor(EW$LEH), EW$LLC, xlab = "LEH", ylab = "LLC", col = "pink")


par(mfrow = c(1, 2))
plot(as.factor(EW1$Abscesses), EW1$RLC, xlab = "Abscesses", ylab = "RLC", col = "lightgreen")
plot(as.factor(EW1$Abscesses), EW1$LLC, xlab = "Abscesses", ylab = "LLC", col = "green")


logistic <- glm(data = EW1, Abscesses ~ RLC, family = "binomial")
summary(logistic)