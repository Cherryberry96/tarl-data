f <- "~/Desktop/Repos/tarl-data/TARL Project - Data.xlsx"
library(readxl)
TD <- read_excel(f, sheet = 1, col_names = TRUE)
head(TD)
TD <- read_excel(f, sheet = 6, col_names = TRUE)
head(TD)


library(dplyr)
library(ggplot2)


library(skimr)
library(kableExtra)
c <- skim(TD)
c <- skim_to_wide(TD)
c %>% select(-type, -missing, -complete, -n, -empty, -n_unique) %>% kable() %>% kable_styling(font_size = 10)



library(summarytools)
s1 <- descr(TD, style = "rmarkdown", transpose = TRUE)
s1 %>% summarytools::view()
s1 <- dfSummary(TD, style = "grid", plain.ascii = FALSE)
s1 %>% summarytools::view()



library(tidyr)
tidyr::gather()
d_long <- gather(TD, key = variable, value = value, c("Pathology", "Sex", "Site"))
p <- ggplot(data = d_long, aes(x = factor(0), y = value)) + geom_boxplot(na.rm = TRUE, 
                                                                         outlier.shape = NA) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
                                                                                                     axis.ticks.x = element_blank()) + geom_dotplot(binaxis = "y", stackdir = "center", 
                                                                                                                                                    stackratio = 0.2, alpha = 0.3, color = NA, fill = "red", na.rm = TRUE) + 
  facet_grid(. ~ variable) + geom_rug(sides = "l")
p


par(mfrow = c(1, 2)) 
attach(TD)  
hist(log(LLC), freq = FALSE, col = "red", main = "Hist of LLC (using Log)", xlab = "log(LLC)", 
     ylab = "density", ylim = c(0, 3))
abline(v = mean(log(LLC), na.rm = TRUE))


hist(log(RLC), freq = FALSE, col = "red", main = "Hist of RLC (using Log)", xlab = "log(RLC)", 
     ylab = "density", ylim = c(0, 3))
abline(v = mean(log(RLC), na.rm = TRUE))




table(TD$Site, TD$Pathology)
chisq.test(TD$Site, TD$Pathology)
chisq.test(TD$Site, TD$Pathology, correct = FALSE)
chisq <- chisq.test(TD$Site,TD$Pathology)
chisq$observed
round(chisq$expected,2)

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
