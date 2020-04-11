# Install package sem
#install.packages("sem")
library(sem)

# Example from lecture notes for the data in Table 14.1 of Rencher
#d = read.table("d:/Work/Columbia/HUDM6122/Datasets/T14_1_Grades.dat", header = F)
d = read.table(file.choose(), header = F)
colnames(d) = c("Lab", "HW", "PopQuiz", "Exam1", "Exam2", "FinalExam")
head(d)

# Assume 2 factors: 
# f1 = "Daily effort" which affects Lab, HW, and PopQuiz
# f2 = "Knowledge mastery" which affects PopQuiz, Exam1, Exam2, and FinalExam
# Assume further HW has loading 1 wrt to f1 and FinalExam has loading 1 wrt to f2
# That is q = 14 total parameters.

# The model:
mod2 <- c("Daily     -> Lab, lambda11, NA",
         "Daily     -> HW, NA, 1",
         "Daily     -> PopQuiz, lambda31, NA",
         "Knowledge     -> PopQuiz, lambda32, NA",
         "Knowledge  -> Exam1, lambda4, NA",
         "Knowledge  -> Exam2, lambda5, NA",
         "Knowledge  -> FinalExam, NA, 1",
         
         "Lab        <-> Lab, psi1, NA",
         "HW        <-> HW, psi2, NA",
         "PopQuiz        <-> PopQuiz, psi3, NA",
         "Exam1        <-> Exam1, psi4, NA",
         "Exam2         <-> Exam2, psi5, NA",
         "FinalExam         <-> FinalExam, psi6, NA",
         
         "Daily    <-> Daily, phi11, NA",
         "Daily    <-> Knowledge, phi12, NA",
         "Knowledge <-> Knowledge, phi22, NA")
writeLines(mod2, con = "knowledge_model.txt")

# Read model from file (function specifyModel reads file or accepts character string)
(knowledge_model <- specifyModel(file = "knowledge_model.txt"))

knowledge_sem <- sem(knowledge_model, S = cov(d), nrow(d))
summary(knowledge_sem)

# The Ho: model fits the data has p-value 0.09 so it is not rejected!

# Fit indeces:
# Bentler's CFI (equation 14.20 from Rencher)
summary(knowledge_sem, fit.indices = "CFI")
# Should be > 0.95 so model fits well

# Example on p. 206 of Everitt & Hothorn
# Get the data correlation matrix 
ab <- c(0.73,
        0.70, 0.68,
        0.58, 0.61, 0.57,      
        0.46, 0.43, 0.40, 0.37,      
        0.56, 0.52, 0.48, 0.41, 0.72)      
ability <- diag(6) / 2
ability[upper.tri(ability)] <- ab
ability <- ability + t(ability)  
rownames(ability) <- colnames(ability) <- 
  c("SCA","PPE","PTE","PFE","EA","CP")
# SCA: self-concept of ability;
# PPE: perceived parental evaluation; 
# PTE: perceived teacher evaluation; 
# PFE: perceived friend’s evaluation; 
# EA: educational aspiration;
# CP: college plans.

# The model:
mod <- c("Ability     -> SCA, lambda1, NA",
         "Ability     -> PPE, lambda2, NA",
         "Ability     -> PTE, lambda3, NA",
         "Ability     -> PFE, lambda4, NA",
         "Aspiration  -> EA, lambda5, NA",
         "Aspiration  -> CP, lambda6, NA",
         "Ability    <-> Aspiration, rho, NA",
         "SCA        <-> SCA, theta1, NA",
         "PPE        <-> PPE, theta2, NA",
         "PTE        <-> PTE, theta3, NA",
         "PFE        <-> PFE, theta4, NA",
         "EA         <-> EA, theta5, NA",
         "CP         <-> CP, theta6, NA",
         "Ability    <-> Ability, NA, 1",
         "Aspiration <-> Aspiration, NA, 1")
# Note the factor variances are fixed at 1 for identifiability 
# Save the file as text:
writeLines(mod, con = "ability_model.txt")

ability_model <- specifyModel(file = "ability_model.txt")
ability_sem <- sem(ability_model, ability, 556)

summary(ability_sem)
# All parameters are significant
# Ability and aspiration are highly correlated at 0.666

summary(ability_sem, fit.indices = c("CFI", "GFI"))

#install.packages("DiagrammeR")
pathDiagram(ability_sem)
residuals(ability_sem)

#install.packages("lavaan")
library(lavaan)
?PoliticalDemocracy
model <- '
  # measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
fit <- sem(model, data=PoliticalDemocracy)
summary(fit, standardized=TRUE)

#install.packages("lavaanPlot")
library(lavaanPlot)
lavaanPlot(model = fit, coef = TRUE)