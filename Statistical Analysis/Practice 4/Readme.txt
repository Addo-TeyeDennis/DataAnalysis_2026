THE ASSIGNMENT
The assignment was to handle the missingness in the dataset concerning the Hormonal and Biochemical Profile in Patients and to perform a comprehensive statistical analysis of the relationship between lipid markers and clinical outcomes.

DATA DESCRIPTION
The dataset contained clinical identifiers (record_id), a hormonal profile (hormone1 to hormone10), multiple lipid variables (lipids1 to lipids5), and various categorical factors (ethnicity, PCOS status, and binary clinical outcomes).

ENVIRONMENT AND REQUIREMENTS
R Version: 4.5.x or higher recommended.

Core Libraries:

tidyverse (dplyr, ggplot2, tidyr): Data manipulation and visualization.

skimr: Summary statistics.

mice: Multiple Imputation by Chained Equations.

naniar & visdat: Missing data visualization.

pROC: Model quality assessment (ROC and AUC).

dbscan: Local Outlier Factor (LOF) calculation.

PROCEEDURE
Exploratory Data Analysis (EDA)

Initial screening using skim() to understand the distributions of lipid markers.

Variables exceeding 35% missingness were evaluated to maintain data integrity.

Missing Data Analysis

Visualization: Used vis_miss() and gg_miss_var() to identify patterns of missingness.

Hypothesis Testing: Applied Little’s MCAR test to determine if the data was Missing Completely at Random (MCAR) to justify the imputation strategy.

Multiple Imputation (MICE)

Two methods were compared for filling in missing values:

Predictive Mean Matching (PMM): Used for numeric lipid data.

Random Forest (RF): Captured non-linear relationships between hormones and lipids.

Validation: Density plots were generated to ensure the distribution of imputed values matched the original dataset.

Statistical Testing and Modeling

Normality & Correlation: Tested for normality via shapiro.test(). Spearman’s rank correlation was used to assess relationships between lipid variables, incorporating exact = FALSE to handle tied ranks.

Outlier Detection: Implemented the Local Outlier Factor (LOF) algorithm to identify multivariate anomalies that standard boxplots often miss.