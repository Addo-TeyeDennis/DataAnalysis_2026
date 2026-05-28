#--------------------start-------------------------------
# Get current working directory
getwd()
#----------------read dataset--------------------------
data_for_analysis<-read.csv("data_for_analysis.csv")

#-----------descriptive statistics------------------
summary(data_for_analysis)
#-----------for publication tables-----------------
#---------------Creating a custom table--------------
# Homework: Creating a custom table with descriptive statistics results
#install.packages("gtsummary")
#install.packages(c("cardx", "cards"))
#library(cardx)
#library(gtsummary)

#tbl_summary(data_for_analysis)  # Automatic table
#tbl_summary(data_for_analysis, by = outcome)  # By groups

#--------------Statistical Tests---------------------
value_outcome1<-data_for_analysis[data_for_analysis$outcome=="1",]$lipids1
hist(value_outcome1, col = "lightblue")

qqnorm(value_outcome1, main = "Q-Q Plot")
qqline(value_outcome1, col = "red", lwd = 2)

# Shapiro-Wilk test (for n < 5000)
shapiro.test(value_outcome1)


value_outcome0<-data_for_analysis[data_for_analysis$outcome=="0",]$lipids1
hist(value_outcome0, col = "lightgreen")

qqnorm(value_outcome0, main = "Q-Q Plot")
qqline(value_outcome0, col = "red", lwd = 2)

# Shapiro-Wilk test (for n < 5000)
shapiro.test(value_outcome0)

#-------Levene's Test for Homogeneity of Variance--------------
install.packages("car")
library(car)
str(data_for_analysis)
data_for_analysis$outcome<- as.factor(data_for_analysis$outcome)
car::leveneTest(lipids1 ~ outcome, data = data_for_analysis)
#---------------Application of the Brunner-Munzel test----------
install.packages("lawstat")
library(lawstat)
group1 <- data_for_analysis$lipids1[data_for_analysis$outcome == "0"]
group2 <- data_for_analysis$lipids1[data_for_analysis$outcome == "1"]

brunner.munzel.test(group1, group2)
#-------------comparison of results with other tests--------------
t.test(group1, group2)
wilcox.test(group1, group2)

#----------------------------EDA----------------------------------
install.packages("DataExplorer")
library(DataExplorer)
create_report(data_for_analysis)  # Generates HTML report with graphs and statistics
create_report(
  data = data_for_analysis,
  output_file = "EDA_Report.html",  
  output_dir = getwd(),                
  report_title = "EDA Report"          
)