#--------------------start-------------------------------
# Get current working directory
getwd()
# create path relative to project root
#data_path <- "/cloud/project/data/DataSet_No_Details.csv"
data_path <- "C:\\Users\\DauntlessD\\Desktop\\DataSet_No_Details.csv"
#----------------read dataset--------------------------
df <- read.csv(data_path)
# Display structure with variable types
str(df)
# Beautiful summary with histograms for numeric variables
install.packages("skimr")
library(skimr)
skim(df) 
#---------------data set preparation------------------
library(dplyr)
# Delete a few columns 
cols_to_remove <- c("h_index_34", "h_index_56", "hormone10_1", "hormone10_2","an_index_23","outcome","factor_eth","factor_h","factor_pcos","factor_prl")
MD_df <- df %>% select(-any_of(cols_to_remove))
factor_df <- df %>% select (record_id, outcome, factor_eth, factor_h,factor_pcos,factor_prl)
str(MD_df)
summary(factor_df)

#--------------Identify Missing Values-----------------
sum(is.na(MD_df))               # Total NAs in entire dataset
colSums(is.na(MD_df))           # NA counts per column
skim(MD_df)
na_stats <- colMeans(is.na(MD_df)) * 100 # % missing data
na_stats
na_stats_filtered <- na_stats[na_stats <= 35] #  missing data <=35 %
# result as a table
data.frame(
  Column = names(na_stats_filtered),
  NA_Percent = na_stats_filtered,
  row.names = NULL
)

na_stats_filtered_1 <- na_stats[na_stats > 35] # missing data >35 %
# result as a table
data.frame(
  Column = names(na_stats_filtered_1),
  NA_Percent = na_stats_filtered_1,
  row.names = NULL
)

#-------------------Visualizing Missing Data Patterns------------------
install.packages("visdat")
library(visdat)
vis_miss(MD_df)  # Visualizes NA patterns

install.packages("naniar")
library(naniar)
gg_miss_var(MD_df)  # Barplot of missingness per variable
#------------------ Analyzing the Impact of Missing Data--------------
# Delete a few columns 
library(dplyr)
cols_to_remove1 <- c("hormone9", "hormone11", "hormone12", "hormone13","hormone14")
handle_MD_df <- MD_df %>% select(-any_of(cols_to_remove1))
str(handle_MD_df)

#------------------Performing Little's MCAR Test----------------------
#Homework!!!
#Hypotheses:
#  H₀ (Null Hypothesis): Data is MCAR.

#H₁ (Alternative Hypothesis): Data is not MCAR (either MAR or MNAR).

#If p-value > 0.05, we fail to reject H₀ (data is likely MCAR).
#If p-value ≤ 0.05, we reject H₀ (data is likely not MCAR).
#------------------Imputation with MICE-------------------------------
# Install packages if they are not already installed
install.packages(c("mice", "ggplot2", "naniar"))
# Load the packages
library(mice)
library(ggplot2)
library(naniar)

# Perform Multiple Imputation
imputed_handle_MD_df <- mice(handle_MD_df, m=5, method='pmm', print=FALSE)
# Perform Multiple Imputation 
#Random Forest method 
#------For complex nonlinear relationships between variables------------

imputed_handle_MD_df <- mice(handle_MD_df[, !names(handle_MD_df) %in% "New"], method="rf")  
  imputed_handle_MD_df_final <- complete(imputed_handle_MD_df)  # generate full data
# Density plots 
ggplot(handle_MD_df, aes(x=hormone10_generated, fill="Original")) +
  geom_density(alpha=0.5) +
  geom_density(data=imputed_handle_MD_df_final, aes(x=hormone10_generated, fill="Imputed"), alpha=0.5) +
  labs(title="Density Plot of hormone10_generated: Original vs. Imputed")+
  scale_x_continuous(limits = c(0, 2))

#Predictive Mean Matching 
#------default for numeric data------------
#Homework!!!
imputed_handle_MD_df1 <- mice(handle_MD_df[, !names(handle_MD_df) %in% "New"], method="pmm")  
imputed_handle_MD_df_final1 <- complete(imputed_handle_MD_df)  # generate full data
# Density plots 
ggplot(handle_MD_df, aes(x=hormone10_generated, fill="Original")) +
  geom_density(alpha=0.5) +
  geom_density(data=imputed_handle_MD_df_final1, aes(x=hormone10_generated, fill="Imputed"), alpha=0.5) +
  labs(title="Density Plot of hormone10_generated: Original vs. Imputed")+
  scale_x_continuous(limits = c(0, 2))
                     
#----------------Outlier Detection Methods------------------------
library(ggplot2)
library(tidyr)
library(dplyr)
outliers_data <- imputed_handle_MD_df_final %>%
  select(lipids1, lipids2, lipids3, lipids4, lipids5) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

# build a graph 
ggplot(outliers_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Outlier Detection",
       x = "variables",
       y = "value") +
  theme_minimal()
#build a graph for all dataset
imputed_handle_MD_df_final %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Boxplots for Outlier Detection")

#FOR EXTRA POINTS !!!
# 1. Install and load dbscan
if (!require("dbscan")) install.packages("dbscan")
library(dbscan)

# 2. Prepare the data
# We select numeric columns AND remove any rows that still have NAs
numeric_only <- imputed_handle_MD_df_final %>% 
  select(where(is.numeric)) %>%
  na.omit() 

# 3. Calculate LOF factors
# Now that we've used na.omit(), this will work!
lof_scores <- lof(numeric_only, k = 10)

# 4. Add the scores back (Note: we use numeric_only here because it matches the lof_scores length)
lof_df <- numeric_only %>%
  mutate(lof_score = lof_scores,
         is_outlier = ifelse(lof_score > 1.5, "Yes", "No"))

#------------------ Visualizing Results ------------------

# A. Histogram of LOF factors
# Most scores should be around 1.0. Values > 1.5 are suspicious.
ggplot(lof_df, aes(x = lof_score)) +
  geom_histogram(bins = 30, fill = "cadetblue", color = "white") +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red") +
  labs(title = "Distribution of LOF Factors",
       subtitle = "Values significantly greater than 1.5 indicate multivariate outliers",
       x = "LOF Score", y = "Frequency") +
  theme_minimal()

# B. Bivariate Scatterplot (e.g., lipids1 vs lipids2)
# This highlights outliers that might not look weird on one axis, but look weird combined.
ggplot(lof_df, aes(x = lipids1, y = lipids2, color = is_outlier, size = lof_score)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("No" = "grey70", "Yes" = "firebrick")) +
  labs(title = "Bivariate Scatterplot with LOF Outliers",
       subtitle = "Red points represent multivariate outliers (LOF > 1.5)",
       x = "Lipids 1", y = "Lipids 2", color = "Outlier?") +
  theme_minimal()

