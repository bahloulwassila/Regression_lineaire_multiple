#Exercice 2:

#Installation des packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")

#Appel des librairies
library(readxl)
library(dplyr)
library(ggplot2)
library(car)

#Ouverture de la base de données
mtcars <- read_excel("C:/Users/wassi/OneDrive - Universite de Montreal/semestre4/STT2400/devoirs/devoir2/Mtcars.xlsx")
mtcars <- mtcars %>%
  mutate(across(c(mpg, disp, drat, wt, qsec), as.numeric))

str(mtcars)
head(mtcars)

# Q1: 

evaluate_models <- function(combinations) {
  results <- data.frame(variables = character(), r_squared = numeric(), stringsAsFactors = FALSE)
  for (i in 1:ncol(combinations)) {
    formula <- as.formula(paste("mpg ~", paste(combinations[, i], collapse = "+")))
    model <- lm(formula, data = mtcars)
    results <- rbind(results, data.frame(
      variables = paste(combinations[, i], collapse = "+"),
      r_squared = summary(model)$r.squared
    ))
  }
  return(results)
}

all_vars <- names(mtcars)[2:11]
combinations <- combn(all_vars, 2)
best_models <- evaluate_models(combinations) %>%
  arrange(desc(r_squared)) %>%
  head(4)

# impression des summary et graphiques des 4 modeles 
for (i in 1:nrow(best_models)) {
  formula <- as.formula(paste("mpg ~", best_models$variables[i]))
  model <- lm(formula, data = mtcars)
  print(summary(model))
  plot(model)
}

# Identification des meilleurs models comportant wt
best_models_with_wt <- best_models %>%
  filter(grepl("wt", variables))

for (i in 1:nrow(best_models_with_wt)) {
  formula <- as.formula(paste("mpg ~", best_models_with_wt$variables[i]))
  model <- lm(formula, data = mtcars)
  print(summary(model))
  plot(model)
}

# Q2: 
model_wt_qsec <- lm(mpg ~ wt + qsec, data = mtcars)
avPlots(model_wt_qsec, terms = "wt", main = "Partial Regression Plot for wt")

# Q3: 
crPlots(model_wt_qsec, terms = "wt", main = "Partial Residual Plot for wt")

# Comparaison des modeles avec et sans un terme quadratique en wt
model_quad <- lm(mpg ~ wt + I(wt^2) + qsec, data = mtcars)
AIC(model_wt_qsec, model_quad)

# Q4: 
plot(model_wt_qsec)
influenceIndexPlot(model_wt_qsec, main = "Influence Plot")

# Q5: 
ncvTest(model_wt_qsec)

# Q6: 
par(mfrow = c(2, 2))
plot(model_wt_qsec)
influenceIndexPlot(model_wt_qsec)
leveragePlots(model_wt_qsec)


#Exercice 3:
#Q1:
execsal2 <- read.table("C:/Users/wassi/OneDrive - Universite de Montreal/semestre4/STT2400/devoirs/devoir2/execsal2.txt", header=TRUE)

colnames(execsal2) <- c("row_number", "log_annual_salary", "experience_years", "education_years", "gender",
                        "num_employees_supervised", "corporate_assets_millions", "board_member", "age_years",
                        "company_profits_millions", "has_international_responsibility", "company_total_sales_millions")

print(colnames(execsal2))

#Q2:
regression_model <- lm(log_annual_salary~ . - row_number, data=execsal2)
summary(regression_model)

#Q3:
#La variable la moins significative est celle correspondante à la p-value la plus elevée. 

regression_model2 <- lm(log_annual_salary ~ . - row_number - company_total_sales_millions, data=execsal2)
summary(regression_model2)

#Q4:
regression_model3 <- lm(log_annual_salary ~ . - row_number - company_total_sales_millions - age_years, data=execsal2)
summary(regression_model3)

regression_model4 <- lm(log_annual_salary ~ . - row_number - company_total_sales_millions - age_years - company_profits_millions, data=execsal2)
summary(regression_model4)

regression_model5 <- lm(log_annual_salary ~ . - row_number - company_total_sales_millions - age_years - company_profits_millions - board_member, data=execsal2)
summary(regression_model5)

regression_model6 <- lm(log_annual_salary ~ . - row_number - company_total_sales_millions - age_years - company_profits_millions - board_member - has_international_responsibility, data=execsal2)
summary(regression_model6)


#Exercice 4:

#Q1:
# Lire les données
usure <- read.table("C:/Users/wassi/OneDrive - Universite de Montreal/semestre4/STT2400/devoirs/devoir2/usure.txt", header=TRUE)

# Afficher les premières lignes des données
head(usure)

#Q2:
# Calculer les moyennes pour chaque combinaison de (temp, miles)
usure$mean_usure <- ave(usure$usure, list(usure$temp, usure$miles))

# Calculer les résidus par rapport aux moyennes (erreurs pures)
usure$residus_purs <- usure$usure - usure$mean_usure

# Calculer la somme des carrés des résidus purs
erreur_pure <- sum(usure$residus_purs^2)
print(paste("Erreur pure : ", erreur_pure))

# Créer un jeu de données avec les moyennes pour chaque combinaison de (temp, miles)
mean_data <- aggregate(usure ~ temp + miles, data=usure, FUN=mean)

# Ajuster le modèle linéaire I
modele <- lm(usure ~ temp + miles, data=mean_data)
summary(modele)

# Calculer les résidus du modèle ajusté
residus_modele <- residuals(modele)

# Calculer l'erreur du modèle
erreur_modele <- sum(residus_modele^2)

# Calculer l'erreur totale
erreur_totale <- sum((usure$usure - mean(usure$usure))^2)

# Degrés de liberté
ddl_totale <- nrow(usure) - 1
ddl_modele <- length(coef(modele)) - 1
ddl_erreur_pure <- ddl_totale - ddl_modele

# Calculer la statistique F
F_stat <- ((erreur_totale - erreur_modele) / ddl_modele) / (erreur_pure / ddl_erreur_pure)

# Calculer la p-valeur
p_value <- pf(F_stat, ddl_modele, ddl_erreur_pure, lower.tail = FALSE)

print(paste("Statistique F pour le modèle I: ", F_stat))
print(paste("P-value pour le modèle I: ", p_value))

#Q3:

# Calculer les moyennes pour chaque combinaison de (temp, miles)
usure$mean_usure <- ave(usure$usure, list(usure$temp, usure$miles))

# Calculer les résidus par rapport aux moyennes (erreurs pures)
usure$residus_purs <- usure$usure - usure$mean_usure

# Calculer la somme des carrés des résidus purs
erreur_pure <- sum(usure$residus_purs^2)
print(paste("Erreur pure : ", erreur_pure))

# Créer un jeu de données avec les moyennes pour chaque combinaison de (temp, miles)
mean_data <- aggregate(usure ~ temp + miles, data=usure, FUN=mean)

# Ajouter les termes supplémentaires au modèle
mean_data$x1x2 <- mean_data$temp * mean_data$miles
mean_data$x2_squared <- mean_data$miles^2

# Ajuster le modèle linéaire II
modele_II <- lm(usure ~ temp + miles + x1x2 + x2_squared, data=mean_data)
summary(modele_II)

# Calculer les résidus du modèle ajusté
residus_modele_II <- residuals(modele_II)

# Calculer l'erreur du modèle
erreur_modele_II <- sum(residus_modele_II^2)

# Calculer l'erreur totale
erreur_totale <- sum((usure$usure - mean(usure$usure))^2)

# Degrés de liberté
ddl_totale <- nrow(usure) - 1
ddl_modele_II <- length(coef(modele_II)) - 1
ddl_erreur_pure <- ddl_totale - ddl_modele_II

# Calculer la statistique F
F_stat_II <- ((erreur_totale - erreur_modele_II) / ddl_modele_II) / (erreur_pure / ddl_erreur_pure)

# Calculer la p-valeur
p_value_II <- pf(F_stat_II, ddl_modele_II, ddl_erreur_pure, lower.tail = FALSE)

print(paste("Statistique F pour le modèle II : ", F_stat_II))
print(paste("P-value pour le modèle II : ", p_value_II))


