# 🏡 Ames Housing Dataset - Exploratory Data Analysis (EDA)

This repository contains the final project for the **Exploratory Data Analysis (EDA)** course, focused on understanding the key factors influencing house sale prices in Ames, Iowa. Using the **Ames Housing Dataset**, which includes 1,460 observations and 81 variables, this project explores housing trends, performs feature engineering, and builds statistical models using R.

## 📌 Project Objectives

- Identify and visualize key variables that influence house prices.
- Explore real estate dynamics in Ames, Iowa (2006–2010), including the impact of renovations, location, condition, seasonality, and more.
- Engineer meaningful features to enrich the analysis.
- Build linear regression models to predict house sale prices.
- Interpret results and offer practical recommendations.

---

## 📊 Dataset Overview

- **Source**: Ames Assessor’s Office (2006–2010)
- **Observations**: 1,460 residential properties
- **Features**: 81 variables (e.g., lot size, year built, neighborhood, garage area, condition, quality, sale price)

A full data dictionary is available in the `data_description.txt` file.

---

## 🛠 Tools and Technologies

- **Language**: R
- **IDE**: RStudio
- **Packages Used**: `ggplot2`, `dplyr`, `tidyr`, `reshape2`, `corrplot`, `plotly`, `leaflet`, `linea regression`

---

## 🧹 Data Cleaning and Preprocessing

- **Missing Value Handling**: Imputation for numerical variables (e.g., `GarageYrBlt`, `MasVnrArea`) and categorical variables (e.g., `PoolQC`, `Fence`) using logical domain-specific replacements.
- **Data Transformations**:
  - Created new features like `TotalSF`, `IsNew`, `WasRenovated`, and `AgeSinceRemodel`.
  - Normalized skewed variables using logarithmic transformations (e.g., `SalePrice`).
- **Exported Clean Data**: Saved as `amesclean_train_data.csv`.

---

## 📈 Key Explorations

### 🔍 Feature Impact Analyses
Visual and statistical explorations addressed these questions:
1. How do proximity to roads, lot size, and landscape features impact sale prices?
2. What is the effect of **renovations** and **modern utilities** on pricing?
3. How do **neighborhood amenities** like parks and schools influence value?
4. What role do **seasonality**, **quality**, **garage features**, and **house size** play?

### 🏘 Neighborhood & Property Factors
- **Highest priced neighborhoods**: NridgHt, NoRidge, StoneBr.
- **Renovated homes**: Yielded higher prices especially in affluent areas.
- **Seasonality**: Spring and summer sales peak, winter dips observed.
- **Garage type**: Attached or built-in garages have higher price premiums.

---

## 📐 Feature Engineering

Key engineered features:
- `TotalSF`: Total square footage including basement and floors.
- `IsNew`: Binary indicator for houses built within 5 years of sale.
- `WasRenovated`: Derived from remodel and build year.
- `AgeSinceRemodel`: Categorized age since last renovation.

Each was visualized using advanced bar, violin, scatter, and box plots to uncover trends.

---

## 📉 Regression Modeling

Developed and evaluated 4 models:

| Model                          | RMSE ($) | R²     |
|-------------------------------|----------|--------|
| `SalePrice ~ OverallCond`     | 79,174   | 0.006  |
| `SalePrice ~ OverallQual`     | **48,589** | **0.626** |
| `SalePrice ~ GarageArea`      | 62,093   | 0.389  |
| `SalePrice ~ GrLivArea`       | 56,034   | 0.502  |

- **Best Model**: `OverallQual` showed the strongest predictive power.
- **Log Transformation** of Sale Price improved model linearity and reduced heteroscedasticity.

---

## ✅ Key Takeaways

- **Overall Quality** is the strongest predictor of sale price.
- **Renovations**, **lot area**, **garage size**, and **energy efficiency** positively impact price.
- **Seasonal and neighborhood effects** are highly relevant for pricing strategies.
- **Feature engineering** greatly enhances model interpretability and performance.

---

## 📚 References

- [City of Ames Zoning Regulations](https://www.cityofames.org/government/departments-divisions-a-h/community-development/planning-zoning)
- [U.S. Energy Info Administration](https://www.eia.gov/consumption/residential/)
- [The Financial Crisis Inquiry Report](https://www.govinfo.gov/content/pkg/GPO-FCIC/pdf/GPO-FCIC.pdf)
- [Journal of Urban Economics](https://www.journals.elsevier.com/journal-of-urban-economics)

For more, see full references in the final report.

---

## 📂 File Structure

