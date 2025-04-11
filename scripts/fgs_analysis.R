#Load necessary packages
library(readxl)
library(gtsummary)
library(tidyverse)
library(lmtest)
library(stringr)
library(see)
library(broom)
library(purrr)
library(parameters)
library(MASS)
library(nnet)

#Import data
fgs <- read_excel("~/fgs_analysis/fgs_data.xlsx")
glimpse(fgs)

#Summary stats
summary(fgs$Age)

#Association testing
fgs %>%
  select(`Sub-county`, `Mode of contact with stagnant water`) %>%
  table() %>%
  fisher.test(workspace = 2e7)

fgs %>%
  select(`Sub-county`, `Source of water for domestic use`) %>%
  table() %>%
  fisher.test(workspace = 2e7)

fgs %>%
  select(Wards, `Source of water for domestic use`) %>%
  table() %>%
  fisher.test(workspace = 2e7)

#Result concordance
fgs_concordance2 <- fgs %>%
  select(StudyNo, `Sub-county`, Wards, `FGS Status`, `Presence of S. haematobium eggs in urine`)

write.csv(fgs_concordance2, file = "fgs_concordance2.csv")

#Infection intensity concordance
intensity_concordance <- fgs %>%
  select(StudyNo, `PCR infection intensity`, `Microscopy infection intensity`, `Ct value`, `Mean egg count`)
  
write.csv(intensity_concordance, file = "sh_intensity_concordance.csv")

#Making summary tables
#Prevalence table
fgs %>%
  select(`FGS Status`, `Sub-county`, Wards) %>%
  tbl_summary(by = `FGS Status`)

#Urinalysis table
fgs %>%
  select(Haematuria, Nitrites, Leucocytes, `Presence of S. haematobium eggs in urine`, `FGS Status`) %>%
  tbl_summary(by = `FGS Status`) %>%
  add_p()

#Urinalysis by sub-county
fgs %>%
  select(`Sub-county`, Haematuria, Nitrites, Leucocytes, `Presence of S. haematobium eggs in urine`) %>%
  tbl_summary(by = `Sub-county`) %>%
  add_p()

#Characteristics associated with FGS
fgs %>%
  select(`Sub-county`, Wards, Age, `Microscopy infection intensity`, `Age group`, Occupation, `Education level`, `Source of water for domestic use`, `Contact with stagnant water`, `Mode of contact with stagnant water`, `Frequency of contact with stagnant water`, `History of bilharzia`, `Received bilharzia treatment`, `Bleeding outside menses in the last 6 months`, `Dysuria in the last 3 months`, `Bleeding after intercourse`, `Abnormal vaginal discharge in the last 3 months`, `Treated for UTI in the last 3 months`, `UTI recurrence`, `Presence of S. haematobium eggs in urine`, `Mean egg count`,`FGS Status`) %>%
  tbl_summary(by = `FGS Status`, statistic = list(all_continuous() ~ "{mean} ({median})", all_categorical() ~ "{n} / {N} ({p}%)")) %>%
  add_p()
fgs %>%
  select(`Sub-county`, Wards, `Microscopy infection intensity`, `Age group`, Occupation, `Education level`, `Source of water for domestic use`, `Contact with stagnant water`, `Mode of contact with stagnant water`, `Frequency of contact with stagnant water`, `History of bilharzia`, `Received bilharzia treatment`, `Bleeding outside menses in the last 6 months`, `Dysuria in the last 3 months`, `Bleeding after intercourse`, `Abnormal vaginal discharge in the last 3 months`, `Treated for UTI in the last 3 months`, `UTI recurrence`, `Presence of S. haematobium eggs in urine`) %>%
  tbl_summary()

#Characteristics by sub-county
fgs %>%
  select(`Sub-county`, `Microscopy infection intensity`, `Education level`, `Source of water for domestic use`, `Occupation`, `Contact with stagnant water`, `Mode of contact with stagnant water`, `Frequency of contact with stagnant water`, `History of bilharzia`, `Received bilharzia treatment`, `Mean egg count`) %>%
  tbl_summary(by = `Sub-county`, statistic = list(all_continuous() ~ "{mean} ({median})", all_categorical() ~ "{n} / {N} ({p}%)")) %>%
  add_p()

#Data viz
#Prevalence of FGS by sub-county
fgs %>%
  ggplot(aes(x = `Sub-county`, color = `FGS Status`, fill = `FGS Status`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  theme_minimal()

#Prevalence of FGS by wards
fgs %>%
  ggplot(aes(Wards, color = `FGS Status`, fill = `FGS Status`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  theme_bw()

#Haematuria in relation to FGS Status
fgs %>%
  ggplot(aes(`FGS Status`, color = Haematuria, fill = Haematuria)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  theme_bw()

#Haematuria by sub-county
fgs %>%
  ggplot(aes(`Sub-county`, color = Haematuria, fill = Haematuria)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  theme_bw()

#Sh eggs and FGS status
fgs %>%
  ggplot(aes(`FGS Status`, color = `Presence of S. haematobium eggs in urine`, fill = `Presence of S. haematobium eggs in urine`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  theme_bw()

#Boxplot of age and FGS status
fgs %>%
  ggplot(aes(x = `FGS Status`, y = Age, color = `FGS Status`, fill = `FGS Status`)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  theme_bw() +
  labs(x = "FGS Status", y = "Age")

#Boxplot of age and infection intensity
fgs %>%
  drop_na(`FGS Status`) %>%
  filter(`Microscopy infection intensity` %in% c("Low", "High")) %>%
  ggplot(aes(x = `Microscopy infection intensity`, y = Age, color = `Microscopy infection intensity`, fill = `Microscopy infection intensity`)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Low" = "green", "High" = "chartreuse3")) +
  theme_bw() +
  labs(x = "Microscopy infection intensity", y = "Age")

#Occurence of dysuria in the last 3 months
fgs %>%
  ggplot(aes(x =  `FGS Status`, color = `Dysuria in the last 3 months`, fill = `Dysuria in the last 3 months`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  theme_bw()

#Contact with stagnant water
fgs %>%
  ggplot(aes(x = `FGS Status`, color = `Contact with stagnant water`, fill = `Contact with stagnant water`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  theme_bw()

#Age group and FGS status
fgs %>%
  ggplot(aes(`Age group`, color = `FGS Status`, fill = `FGS Status`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  theme_bw()

#Mode of contact with stagnant water by FGS status
fgs %>%
  filter(`Mode of contact with stagnant water` %in% c("None", "When bathing", "When walking", "When washing/fetching water", "When washing/fetching water and when bathing", "When washing/fetching water and when walking")) %>%
  ggplot(aes(`FGS Status`, color = `Mode of contact with stagnant water`, fill = `Mode of contact with stagnant water`)) +
  geom_bar(color = "black") +
  theme_bw()

#History of bilharzia
fgs %>%
  ggplot(aes(`FGS Status`, color = `History of bilharzia`, fill = `History of bilharzia`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  theme_bw()

#Received bilharzia treatment
fgs %>%
  ggplot(aes(`FGS Status`, color = `Received bilharzia treatment`, fill = `Received bilharzia treatment`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  theme_bw()

#Bleeding outside menses and fgs status
fgs %>%
  ggplot(aes(`FGS Status`, color = `Bleeding outside menses in the last 6 months`, fill = `Bleeding outside menses in the last 6 months`)) +
  geom_bar(color = "black") +
  theme_bw()

#Scatter plot of mean egg count and FGS status
fgs %>%
  filter(`Mean egg count` > 0.3) %>%
  ggplot(aes(x = Age, y = `Mean egg count`, color = `FGS Status`, fill = `FGS Status`)) +
  geom_point(size = 5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  scale_fill_manual(values = c("Negative" = "aquamarine", "Positive" = "darkturquoise")) +
  theme_bw()
fgs %>%
  filter(`Mean egg count` > 0.3) %>%
  ggplot(aes(x = Age, y = `Mean egg count`, color = `FGS Status`, fill = `FGS Status`)) +
  geom_point(size = 5, alpha = 0.5) +
  scale_fill_manual(values = c("Negative" = "aquamarine", "Positive" = "darkturquoise")) +
  theme_bw()

#Microscopy intensity and FGS status
fgs %>%
  ggplot(aes(`FGS Status`, color = `Infection intensity`, fill = `Infection intensity`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Negative" = "red", "High" = "blue", "Low" = "cyan")) +
  theme_bw()

#Correlation between mean egg count and age
#Assess normality of the variables
shapiro.test(fgs$`Mean egg count`)
shapiro.test(fgs$Age)
shapiro.test(fgs$`Ct value`)

#Correlation analysis between age and mean egg count
cor.test(fgs$`Mean egg count`, fgs$Age, method = "spearman")

#Correlation analysis between mean egg count and Ct values
cor.test(fgs$`Mean egg count`, fgs$`Ct value`, method = "spearman")

#Mode of contact with stagnant water by sub-county
fgs %>%
  filter(`Mode of contact with stagnant water` %in% c("None", "When bathing", "When walking", "When washing/fetching water", "When washing/fetching water and when bathing", "When washing/fetching water and when walking")) %>%
  ggplot(aes(`Sub-county`, color = `Mode of contact with stagnant water`, fill = `Mode of contact with stagnant water`)) +
  geom_bar(color = "black") +
  theme_bw()

#Occupation by sub-county
fgs %>%
  ggplot(aes(`Sub-county`, color = Occupation, fill = Occupation)) +
  geom_bar(color = "black")+
  theme_bw()
glimpse(fgs)

#Frequency of contact with stagnant water by sub-county
fgs %>%
  drop_na(`FGS Status`) %>%
  filter(`Frequency of contact with stagnant water` %in% c("None", "Less than twice a week", "More than twice a week", "Seasonally")) %>%
  ggplot(aes(`Sub-county`, color = `Frequency of contact with stagnant water`, fill = `Frequency of contact with stagnant water`)) +
  geom_bar(color = "black") +
  theme_bw()

#Source of water by sub-county
fgs %>%
  ggplot(aes(`Sub-county`, color = `Source of water for domestic use`, fill = `Source of water for domestic use`)) +
  geom_bar(color = "black") +
  theme_bw()
glimpse(fgs)

#Source of water by ward
fgs %>%
  ggplot(aes(Wards, color = `Source of water for domestic use`, fill = `Source of water for domestic use`)) +
  geom_bar(color = "black") +
  theme_bw()

#Received bilharzia treatment
fgs %>%
  ggplot(aes(Wards, color = `Received bilharzia treatment`, fill = `Received bilharzia treatment`)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  theme_bw()

#Scatter plot with a polynomial linear regression line: mean egg count and Ct value
fgs %>%
  ggplot(aes(x = `Ct value`, y = `Mean egg count`, color = `FGS Status`, fill = `FGS Status`)) +
  geom_point(size = 5, alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ poly(x,2), col = "blue") +
  theme_bw() +
  labs(x = "Ct value", y = "Mean egg count")
fgs %>%
  ggplot(aes(x = `Ct value`, y = `Mean egg count`, color = `Presence of S. haematobium eggs in urine`, fill = `Presence of S. haematobium eggs in urine`)) +
  geom_point(size = 5, alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ poly(x,2), col = "blue") +
  theme_bw() +
  labs(x= "Ct value", y = "Mean egg count")

#Scatter plot of mean egg count and Ct value by PCR infection intensity 
fgs %>%
  filter(`PCR infection intensity` %in% c("High", "Low", "None")) %>%
  ggplot(aes(x = `Ct value`, y = `Mean egg count`, color = `PCR infection intensity`, fill = `PCR infection intensity`)) +
  geom_point(size = 5, alpha = 0.5) +
  theme_bw() +
  labs(x = "Ct value", y = "Mean egg count")


#Scatter plot of mean egg count and Ct value by microscopy infection intensity
fgs <- fgs %>%
  rename("Infection intensity" = "Microscopy infection intensity") %>%
  glimpse

fgs %>%
  ggplot(aes(x = `Ct value`, y = `Mean egg count`, color = `Infection intensity`, fill = `Infection intensity`)) +
  geom_point(size = 5, alpha = 0.5) +
  theme_bw() +
  labs(x = "Ct value", y = "Mean egg count")


#Pattern of egg shedding 
fgs_data <- data.frame(
  Location = c('Overall', 'Rabai', 'Magarini', 'Sabaki', 'Mleji', 'Jimba', 'Burangi', 'Garashi', 'Mwangatini'),
  Total = c(94, 29, 65, 17, 10, 19, 24, 4, 20),
  Eggs_Detected = c(26, 7, 19, 1, 1, 6, 8, 2, 8),
  No_Eggs_Detected = c(68, 22, 46, 16, 9, 13, 16, 2, 12)
)
print(fgs_data)
print(fgs_data_long)
rm(fgs_data)
rm(fgs_data_long)

#Melt data 
fgs_data_long <- fgs_data %>%
  tidyr::pivot_longer(cols = c(Eggs_Detected, No_Eggs_Detected), names_to = "Status", values_to = "Count")

#Bar chart of egg shedding
fgs_data_long %>%
  ggplot(aes(x = Location, y = Count, fill = Status)) +
  geom_bar(color = "black", stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Eggs_Detected" = "red", "No_Eggs_Detected" = "blue")) +
  theme_bw()

#Regression
#Polynomial regression
lm_model_poly <- lm(`Mean egg count` ~ poly(`Ct value`, 2), data = fgs)
summary(lm_model_poly)

#Recode variables
fgs <- fgs %>%
  mutate(`Sub-county` = recode(`Sub-county`, "Rabai" = 1, "Magarini" = 2))
head(fgs)
glimpse(fgs)

fgs <- fgs %>%
  mutate(`Mode of contact with stagnant water` = recode(`Mode of contact with stagnant water`, "None" = 1, "When bathing" = 2, "When walking" = 3, "When washing/fetching water" = 4, "When washing/fetching water and when bathing" = 5, "When washing/fetching water and when walking" = 6))

fgs <- fgs %>%
  mutate(`Bleeding outside menses in the last 6 months` = recode(`Bleeding outside menses in the last 6 months`, "None" = 1, "More than 6 months ago" = 2, "Presently" = 3, "Within the last 6 months" = 4))

fgs <- fgs %>%
  mutate(`Age group` = recode(`Age group`, "Age 15-25" = 1, "Age 26-35" = 2, "Age 36-45" = 3, "Age 46-55" = 4))

fgs <- fgs %>%
  mutate(`Microscopy infection intensity` = recode(`Microscopy infection intensity`, "Negative" = 1, "Low" = 2, "High" = 3))

fgs <- fgs %>%
  mutate(`Frequency of contact with stagnant water` = recode(`Frequency of contact with stagnant water`, "None" = 1, "Seasonally" = 2, "Less than twice a week" = 3, "More than twice a week" = 4))

fgs <- fgs %>%
  mutate(`Source of water for domestic use` = recode(`Source of water for domestic use`, "Piped water" = 1, "Piped water and pond" = 2, "Piped water and river" = 3, "Piped water and well" = 4, "Pond" = 5, "River" = 6, "River and pond" = 7, "Well" = 8, "Well and river" = 9))

fgs <- fgs %>%
  mutate(Occupation = recode(Occupation, "Casual labourer" = 1, "Business" = 2, "Farmer" = 3, "Housewife" = 4))

fgs <- fgs %>%
  mutate(`Presence of S. haematobium eggs in urine` = recode(`Presence of S. haematobium eggs in urine`, "Negative" = 1, "Positive" = 2))

fgs <- fgs %>%
  mutate(`FGS Status` = recode(`FGS Status`, "Negative" = 1, "Positive" = 0))

#Convert relevant columns to character type
fgs <- fgs %>%
  mutate(across(.cols = any_of(c(explanatory_vars, "FGS Status")), .fns = as.character))

#Define the explanatory variables
explanatory_vars <- c("Haematuria", "Village", "Age group", "Sub-county", "Presence of S.haematobium eggs in urine", "Mean egg count", "Contact with stagnant water", "Dysuria in the last 3 months", "Mode of contact with stagnant water", "Source of water for domestic use", "Frequency of contact with stagnant water", "Occupation", "History of bilharzia", "Received bilharzia treatment", "Bleeding outside menses in the last 6 months", "Microscopy infection intensity")
fgs <- fgs %>%
  drop_na(any_of(c("FGS Status", explanatory_vars)))

#Convert data into 1s and 0s
fgs <- fgs %>%
  drop_na(`FGS Status`) %>%
  mutate(across(.cols = any_of(c(explanatory_vars, "FGS Status")), .fns = ~case_when(. %in% c("Yes", "Negative") ~ 1, . %in% c("No", "Positive") ~ 0 , TRUE ~ as.numeric(.))))
head(fgs)
rm(fgs)
glimpse(fgs)

#Univariate regression
fgs <- fgs %>%
  drop_na(`FGS Status`) %>%
  mutate(`FGS Status` = recode(`FGS Status`, "Negative" = 1, "Positive" = 0))

fgs_model2 <- glm(`FGS Status` ~ `Age group`, family = "binomial", data = fgs)
summary(fgs_model2)

#Binomial logistic regression
fgs_model <- glm(`FGS Status` ~ `Microscopy infection intensity` + `Mode of contact with stagnant water` + Haematuria + `Frequency of contact with stagnant water` + Occupation + `Source of water for domestic use` + `Age group` + `Sub-county` + Village + `Dysuria in the last 3 months` + `History of bilharzia` + `Received bilharzia treatment` + `Bleeding outside menses in the last 6 months` + `Presence of S. haematobium eggs in urine` + `Mean egg count`, family = "binomial", data = fgs)
summary(fgs_model)

fgs_model <- glm(`FGS Status` ~ Contactstagnant_Stats, family = "binomial", data = fgs)
summary(fgs_model)

#Summary table
mv_tab <- tbl_regression(fgs_model, exponentiate = TRUE)
mv_tab

#Kruskal-wallis test: Ct value and infection intensity
kruskal_fgs <- kruskal.test(`Ct value` ~ `Microscopy infection intensity`, data = fgs)
print(kruskal_fgs)

#Multinomial logistic regression
fgs$`Microscopy infection intensity`[fgs$`Mean egg count` == 0] <- "Negative"
fgs$`Microscopy infection intensity` <- factor(fgs$`Microscopy infection intensity`, levels = c("Negative", "Low", "High"), ordered = TRUE)
table(fgs$`Microscopy infection intensity`)
multinom_fgs <- multinom(`Microscopy infection intensity` ~ `Ct value`, data = fgs)
summary(multinom_fgs)

#Extract coefficients and standard errors
coefficients <- summary(multinom_fgs)$coefficients
coefficients
std_errors <- summary(multinom_fgs)$standard.errors
std_errors

#Compute z-scores
z_scores <- coefficients / std_errors
z_scores

#Compute p-values
p_values <- 2 * (1 - pnorm(abs(z_scores)))
p_values

#Exponentiate the odds ratios
exp_coef_low <- exp(0.03871101)
exp_coef_high <- exp(0.04813929)
exp_coef_low
exp_coef_high


  
#GIS mapping
#Load and install the necessary packages
install.packages("sf")
install.packages("tmap")
library(sf)
library(tmap)

#View the shape file
kilifi_sf <- st_read("~/fgs_analysis/KilifiCounty.shp")
print(kilifi_sf)
