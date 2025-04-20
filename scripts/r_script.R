## Loading required libraries
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(reshape2)
library(RColorBrewer)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(scales)
library(broom)
library(modelsummary)
library(performance)
library(car)
library(lmtest)
library(plotly)

setwd("/Users/birajkc_docx/Github_Repository/git_repository/eda_final_assignment")

## Loading Dataset and Performing Intial Exploration
ameshous_train_data <- read.csv("datasets/train.csv")
ameshous_test_data <- read.csv("datasets/test.csv")
summary(ameshous_train_data)
str(ameshous_train_data)
head(ameshous_train_data)
dim(ameshous_train_data)
colnames(ameshous_train_data)
view(ameshous_train_data)


## Handling and looking for Missing Values in the loaded dataset
missing_values <- ameshous_train_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "MissingCount") %>%
  filter(MissingCount > 0) %>%
  arrange(desc(MissingCount)) 

missing_values <- as.data.frame(missing_values)
missing_values <- missing_values[missing_values$MissingCount > 0, ]
missing_data_df <- data.frame(Variable = missing_values$Variable, MissingCount = missing_values$MissingCount)

print(missing_values)



## Visualisation of Missing Values in the Dataset
ggplot(missing_data_df, aes(x = reorder(Variable, -MissingCount), y = MissingCount)) +
  geom_bar(stat = "identity", fill = "blue", color = "white") +
  geom_text(aes(label = MissingCount), vjust = -0.3, color = "black", size = 3.5) +
  scale_fill_gradient(low = "lightblue", high = "blue", name = "Missing Count") +
  labs(
    title = "Missing Data Counts by Variable",
    subtitle = "Total counts of missing entries for each variable in the dataset",
    x = "Variable",
    y = "Number of Missing Values"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "gray50"),
    axis.text.y = element_text(size = 10, color = "gray50"),
    legend.position = "none",
    plot.margin = unit(c(12, 12, 12, 12), "pt")
  )


## Treating the Missing Values
features_none = c("Alley", "MasVnrType", "BsmtQual", "BsmtCond", "BsmtExposure",
                  "BsmtFinType1", "BsmtFinType2", "FireplaceQu", "GarageType", 
                  "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature","LotFrontage")

for (feature in features_none) {
  ameshous_train_data[[feature]][is.na(ameshous_train_data[[feature]])] <- "None"
}

ameshous_train_data$MasVnrArea[is.na(ameshous_train_data$MasVnrArea)] <- 0

ameshous_train_data$GarageYrBlt[is.na(ameshous_train_data$GarageYrBlt)] <- ameshous_train_data$YearBuilt[is.na(ameshous_train_data$GarageYrBlt)]

mode_electrical <- names(which.max(table(ameshous_train_data$Electrical)))
ameshous_train_data$Electrical[is.na(ameshous_train_data$Electrical)] <- mode_electrical

missing_values_summary <- sapply(ameshous_train_data, function(x) sum(is.na(x)))
missing_columns <- names(missing_values_summary)[missing_values_summary > 0]
missing_values_df <- ameshous_train_data[, missing_columns]
print(missing_values_summary)

write.csv(ameshous_train_data, "datasets/amesclean_train_data.csv", row.names = FALSE)


## Understanding Dataset
ames_housing <- read.csv("datasets/amesclean_train_data.csv")
ames_numeric <- ames_housing[sapply(ames_housing, is.numeric)]

cor_matrix <- cor(ames_numeric, use = "pairwise.complete.obs")

cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3, vjust = 1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1), name="Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray95"),
    plot.background = element_rect(color = "gray95", fill = "gray95")
  ) +
  labs(title = "Correlation Matrix of Housing Features", subtitle = "Numeric features of the Ames Housing dataset")


## Top 10 highly correlated variables with the target variable "Sale Price"
sale_price_correlations <- cor_matrix[,"SalePrice", drop = FALSE]
sorted_correlations <- sort(sale_price_correlations[,1], decreasing = TRUE)

top_correlations <- head(sorted_correlations[-1], 10)

cor_data <- data.frame(
  Variable = names(top_correlations),
  Correlation = top_correlations
)

cor_melted <- melt(cor_data, id.vars = "Variable")

ggplot(cor_data, aes(x = Variable, y = factor(1, levels = "SalePrice"), fill = Correlation)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 5, vjust = 0.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name="Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(title = "Top 10 Variables Correlated with SalePrice", x = "Variables", y = "")


## Distribution of Sale Price by Frequency (No. of Houses)
ggplot(ames_housing, aes(x = SalePrice)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(count * 30), color = "Density"), fill = "lightblue", alpha = 0.3) +
  labs(title = "Distribution of Sale Prices", x = "Sale Price", y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Density" = "red")) +
  guides(color = guide_legend(title = "Overlay"))

mean_sale_price <- mean(ames_housing$SalePrice)
print(mean_sale_price)


## Distribution of Sale Price, showing outliers by using Boxplot
`ggplot(ames_housing, aes(y = SalePrice)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(aes(x = 1), color = "blue", alpha = 0.3, width = 0.1) +
  labs(y = "Sale Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 14)
  ) +
  ggtitle("Distribution of Sale Price")`


## Correlation Matrix for Physical Features of House by Overall Quality, Overall Cond, and Year Built
ames_housing %>%
  select(OverallQual, OverallCond, YearBuilt, RoofStyle, Exterior1st, Exterior2nd) %>%
  summary()

ggplot(ames_housing, aes(x = OverallQual)) +
  geom_histogram(binwidth = 1, fill = "blue") +
  labs(title = "Distribution of Overall Quality Ratings")

physical_features <- ames_housing %>% 
  select(OverallQual, OverallCond, YearBuilt)
cor_physical <- cor(physical_features, use = "complete.obs")
ggplot(melt(cor_physical), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "blue", high = "red", mid = "white") +
  theme_minimal() +
  labs(title = "Correlation Matrix for Physical House Features")


## Exploratory Data Analysis (EDA) - Distribution of Sales Prices
ggplot(ames_housing, aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "cornflowerblue", color = "black") +
  labs(title = "Distribution of Sale Prices",
       x = "Sale Price",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


## Comparing Lot Area with Sale Price
ggplot(ames_housing, aes(x = LotArea, y = SalePrice)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE, fill = "lightblue", alpha = 0.2) +
  labs(title = "Sale Price vs. Lot Area",
       x = "Lot Area (sq feet)",
       y = "Sale Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


## Density of Sale Prices by Major Roadway Proximity (Condition1)
ggplot(ames_housing, aes(x = SalePrice, fill = Condition1)) +
  geom_density(alpha = 0.7) +
  labs(title = "Density of Sale Prices by Major Roadway Proximity (Condition1)",
       x = "Sale Price",
       y = "Density") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


## Sale Prices by Condition1 and Condition2
colors <- brewer.pal(9, "Set1")

ggplot(ames_housing, aes(x = Condition1, y = SalePrice, fill = Condition1)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  facet_wrap(~ Condition2, scales = "free_x", nrow = 2, labeller = label_both) +
  labs(title = "Sale Prices by Condition1 and Condition2",
       x = "Condition1",
       y = "Sale Price") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold")
  )


## Boxplot Sale Prices by Proximity to Major Roadwasy (Condition1)
ggplot(ames_housing, aes(x = Condition1, y = SalePrice)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Sale Prices by Proximity to Major Roadways (Condition1)",
       x = "Condition",
       y = "Sale Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


## Interaction of House Style, Condition 1 and Sale Price of House
ggplot(ames_housing, aes(x = Condition1, y = SalePrice, color = HouseStyle)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(group = HouseStyle), method = "lm", se = FALSE) +
  labs(title = "Interaction of House Style and Condition1 on Sale Prices",
       x = "Condition1",
       y = "Sale Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


## Effects do renovations have on the Sale Price of a house
ames_housing$Renovated <- ifelse(ames_housing$YearRemodAdd > ames_housing$YearBuilt, "Renovated", "Not Renovated")

renovation_summary <- ames_housing %>%
  group_by(Renovated) %>%
  summarise(
    Count = n(),
    Mean = mean(SalePrice, na.rm = TRUE),
    Median = median(SalePrice, na.rm = TRUE),
    SD = sd(SalePrice, na.rm = TRUE)
  )
print(renovation_summary)


## Density Plot of Sale Price by Renovation Status of House
ggplot(ames_housing, aes(x = SalePrice, fill = Renovated)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Sale Prices by Renovation Status",
       x = "Sale Price",
       y = "Density",
       fill = "Renovated") +
  scale_fill_manual(values = c("lightblue", "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


t_test_result <- t.test(SalePrice ~ Renovated, data = ames_housing)
print(t_test_result)


## Sale Price based on the age of house during renovation
ames_housing$AgeAtSale <- ames_housing$YrSold - ames_housing$YearBuilt

ggplot(ames_housing, aes(x = AgeAtSale, y = SalePrice, color = Renovated)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, size = 1, aes(group = Renovated)) +
  labs(title = "Sale Price vs. Age at Sale by Renovation Status",
       x = "Age at Sale (Years)",
       y = "Sale Price") +
  scale_color_manual(values = c("Not Renovated" = "blue", "Renovated" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

## Sale Price of House by Neighborhood and Renovation Status
ggplot(ames_housing, aes(x = Neighborhood, y = SalePrice, fill = Renovated)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  coord_flip() +
  labs(title = "Sale Price by Neighborhood and Renovation Status",
       x = "Neighborhood",
       y = "Sale Price",
       fill = "Renovated") +
  scale_fill_manual(values = c("Not Renovated" = "lightblue", "Renovated" = "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


## Comparison of Sale Prices: High vs Low Heating Quality
high_efficiency <- ames_housing %>% filter(HeatingQC == "Ex")
low_efficiency <- ames_housing %>% filter(HeatingQC == "Po")

ggplot() +
  geom_density(data = high_efficiency, aes(x = SalePrice, fill = "High"), alpha = 0.5) +
  geom_density(data = low_efficiency, aes(x = SalePrice, fill = "Low"), alpha = 0.5) +
  labs(title = "Comparison of Sale Prices: High vs Low Heating Quality",
       x = "Sale Price", y = "Density",
       fill = "Heating Quality") +
  scale_fill_manual(values = c("High" = "green", "Low" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

## Median Sale Price of House by Pool
ames_housing$HasPool <- factor(ifelse(ames_housing$PoolQC %in% c("Ex", "Gd", "TA", "Fa"), "Yes", "No"),
                               levels = c("No", "Yes"),
                               labels = c("No Pool", "Has Pool"))

# Plot
ggplot(ames_housing, aes(x = factor(HasPool), y = SalePrice, fill = factor(HasPool))) +
  stat_summary(fun = median, geom = "bar", position = position_dodge(width = 0.8), width = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.8), width = 0.2) +
  labs(title = "Median Sale Prices by Pool Presence",
       x = "Has Pool", y = "Median Sale Price") +
  scale_fill_manual(values = c("No Pool" = "lightblue", "Has Pool" = "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

## Density of Sale Price by availability pool in the House
ggplot(ames_housing, aes(x = SalePrice, fill = factor(HasPool))) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = median(SalePrice)), color = "black", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = quantile(SalePrice, 0.25)), color = "red", linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = quantile(SalePrice, 0.75)), color = "blue", linetype = "dashed", size = 0.8) +
  facet_wrap(~ HasPool) +
  labs(title = "Density of Sale Prices by Pool Presence",
       x = "Sale Price", y = "Density", fill = "Has Pool") +
  scale_fill_manual(values = c("No Pool" = "lightblue", "Has Pool" = "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


## Sale Price of House by availability of Pool
ggplot(ames_housing, aes(x = factor(HasPool), y = SalePrice, fill = factor(HasPool))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "red") +
  stat_summary(fun.data = function(x) {
    quantiles <- quantile(x, c(0.25, 0.75))
    data.frame(y = quantiles, ymin = quantiles[1], ymax = quantiles[2])
  }, geom = "errorbar", width = 0.2, color = "blue") +
  labs(title = "Sale Prices by Pool Presence",
       x = "Has Pool", y = "Sale Price") +
  scale_fill_manual(values = c("No Pool" = "lightblue", "Has Pool" = "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


## Sale Price vs Lot Area of House by presence of Pool
ggplot(ames_housing, aes(x = LotArea, y = SalePrice, color = factor(HasPool))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "blue") +  
  labs(title = "Sale Price vs. Lot Area by Pool Presence",
       x = "Lot Area (sq feet)", y = "Sale Price",
       color = "Has Pool") +  # Add legend title
  scale_color_manual(values = c("No Pool" = "red", "Has Pool" = "green")) +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"  
  )


## Median Sale Price of House among top 10 Neighbourhood
my_colors <- RColorBrewer::brewer.pal(10, "Set3")  
if (length(my_colors) < 10) {
  my_colors <- colorRampPalette(my_colors)(10)  
}

top_neighborhoods <- ames_housing %>%
  group_by(Neighborhood) %>%
  summarize(MedianSalePrice = median(SalePrice, na.rm = TRUE), .groups = 'drop') %>%
  top_n(10, MedianSalePrice) %>%
  arrange(desc(MedianSalePrice)) %>%
  pull(Neighborhood)

ames_housing_top <- ames_housing %>%
  filter(Neighborhood %in% top_neighborhoods)

ggplot(ames_housing_top, aes(x = reorder(Neighborhood, SalePrice, FUN = median), y = SalePrice, fill = Neighborhood)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  
  stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "red") +  
  stat_summary(fun.data = function(x) {
    quantiles <- quantile(x, c(0.25, 0.75))
    data.frame(y = quantiles, ymin = quantiles[1], ymax = quantiles[2])
  }, geom = "errorbar", width = 0.2, color = "blue") +  
  scale_fill_manual(values = my_colors) +
  labs(title = "Median Sale Prices Across Top 10 Neighborhoods",
       x = "Neighborhood", y = "Median Sale Price") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

## Density of Sale Price in top 10 Neighborhood
ggplot(ames_housing_top, aes(x = SalePrice, fill = Neighborhood)) +
  geom_density(alpha = 0.6, color = "black") +  
  geom_vline(aes(xintercept = median(SalePrice)), color = "red", linetype = "dashed", size = 1) +  
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(ames_housing_top$SalePrice), sd = sd(ames_housing_top$SalePrice)),
    aes(x = SalePrice),  # explicitly define x
    inherit.aes = FALSE,  # prevent it from using 'fill = Neighborhood'
    color = "blue", 
    linetype = "dotted"
  ) +  
  scale_fill_manual(values = my_colors) +
  labs(title = "Density of Sale Prices Across Top 10 Neighborhoods",
       x = "Sale Price", y = "Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


## Sale Prices Across Top 10 Neighborhoods Over Time
ggplot(ames_housing_top, aes(x = reorder(Neighborhood, SalePrice, FUN = median), y = SalePrice, color = as.factor(YrSold))) +
  geom_jitter(alpha = 0.6, width = 0.3) +
  geom_boxplot(alpha = 0, outlier.shape = NA, width = 0.2) +  
  labs(title = "Sale Prices Across Top 10 Neighborhoods Over Time",
       x = "Neighborhood", y = "Sale Price",
       color = "Year Sold") +
  scale_color_discrete(name = "Year Sold") +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 12)
  )


## Time Series of Median Sale Price in Top 10 Neighborhood
neighborhood_yearly_top <- ames_housing_top %>%
  group_by(Neighborhood, YrSold) %>%
  summarize(MedianSalePrice = median(SalePrice, na.rm = TRUE), .groups = 'drop')

ggplot(neighborhood_yearly_top, aes(x = factor(YrSold), y = MedianSalePrice, group = Neighborhood, color = Neighborhood)) +
  geom_line() +
  scale_color_brewer(type = "qual", palette = "Paired") +  
  labs(title = "Time Series of Median Sale Prices by Top 10 Neighborhoods",
       x = "Year Sold", y = "Median Sale Price",
       color = "Neighborhood") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 12)
  )


## 2D Density Map of Sale Prices and Lot Area by Top 10 Neighborhoods
ggplot(ames_housing_top, aes(x = LotArea, y = SalePrice, color = Neighborhood)) +
  geom_point(alpha = 0.6) +
  geom_density_2d_filled(contour_var = "ndensity", aes(fill = ..level..)) +
  scale_color_manual(values = my_colors) +  
  scale_fill_manual(values = my_colors) +  
  labs(title = "2D Density Map of Sale Prices and Lot Area by Top 10 Neighborhoods",
       x = "Lot Area", y = "Sale Price") +
  theme_minimal()

## Plotting Maps of Top 10 Neighborhood where Sale Price of House is at Maximum
median_prices <- ames_housing %>%
  group_by(Neighborhood) %>%
  summarize(MedianSalePrice = median(SalePrice, na.rm = TRUE), .groups = 'drop')

neighborhoods_from_plot <- c("Blmngtn", "ClearCr", "CollgCr", "Crawfor", "NoRidge", 
                             "NridgHt", "Somrst", "StoneBr", "Timber", "Veenker")

filtered_data <- median_prices %>%
  filter(Neighborhood %in% neighborhoods_from_plot)

# Manually inputing coordinates for Ames Neighborhood
neighborhood_coords <- data.frame(
  Neighborhood = neighborhoods_from_plot,
  Latitude = c(42.05905, 41.6668, 42.02109528, 42.020579, 42.05055618, 
               42.05963516, 41.6449, 42.06128, 41.72098, 42.02369),
  Longitude = c(-93.63793, -93.6668, -93.68562317, -95.3811884, -93.62717438, 
                -93.65499878, -91.48731, -93.63313, -91.47446, -93.64669)
)

full_data <- merge(neighborhood_coords, filtered_data, by = "Neighborhood")

pal <- colorNumeric(palette = "Viridis", domain = full_data$MedianSalePrice)
final_map <- leaflet(full_data) %>%
  addTiles() %>%  
  setView(lng = -93.6250, lat = 42.0308, zoom = 12)

final_map <- final_map %>%
  addAwesomeMarkers(
    ~Longitude, ~Latitude,
    icon = makeAwesomeIcon(
      icon = 'home', 
      markerColor = ~ifelse(MedianSalePrice > 200000, 'red', 'blue')
    ),
    popup = ~paste(Neighborhood, "<br> Median Sale Price: $", format(MedianSalePrice, big.mark=",", scientific=FALSE))
  )

top_neighborhoods <- full_data %>%
  top_n(3, MedianSalePrice) %>%
  arrange(desc(MedianSalePrice))
final_map <- final_map %>%
  addPolylines(
    lng = top_neighborhoods$Longitude,
    lat = top_neighborhoods$Latitude,
    color = "red",
    weight = 5,
    opacity = 0.7
  )

final_map <- final_map %>%
  addPolygons(
    lng = c(min(top_neighborhoods$Longitude) - 0.01, max(top_neighborhoods$Longitude) + 0.01, 
            max(top_neighborhoods$Longitude) + 0.01, min(top_neighborhoods$Longitude) - 0.01),
    lat = c(min(top_neighborhoods$Latitude) - 0.01, min(top_neighborhoods$Latitude) - 0.01,
            max(top_neighborhoods$Latitude) + 0.01, max(top_neighborhoods$Latitude) + 0.01),
    fillColor = "#ff7800",
    fillOpacity = 0.5,
    weight = 3,
    color = "orange",
    opacity = 0.8
  )

final_map <- final_map %>%
  addLayersControl(
    overlayGroups = c("Price Markers", "Top Priced Route"),
    options = layersControlOptions(collapsed = FALSE)
  )

final_map


## Average Sale Price Over time by Year
if (!"YrSold" %in% names(ames_housing) | !"MoSold" %in% names(ames_housing)) {
  stop("YrSold and/or MoSold columns are missing")
}

ames_housing$DateSold <- as.Date(paste(ames_housing$YrSold, ames_housing$MoSold, "01", sep = "-"), format = "%Y-%m-%d")

ames_housing$Year <- factor(ames_housing$YrSold)

daily_avg_prices <- ames_housing %>%
  group_by(DateSold, Year) %>%
  summarize(AveragePrice = mean(SalePrice, na.rm = TRUE), .groups = 'drop')

color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique(ames_housing$Year)))

ggplot(daily_avg_prices, aes(x = DateSold, y = AveragePrice, group = Year, color = Year)) +
  geom_line(size = 1.5, alpha = 0.8) +  
  scale_color_manual(values = color_palette) +  
  labs(title = "Average Sale Prices Over Time by Year",
       x = "Date Sold", y = "Average Sale Price") +
  theme_minimal() +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 14, face = "bold"),  
    legend.text = element_text(size = 12),  
    axis.text = element_text(size = 12),  
    axis.title = element_text(size = 14)  
  )

## Monthly Average Sales Price
ames_housing$MonthYear <- as.Date(paste(ames_housing$YrSold, ames_housing$MoSold, "01", sep = "-"), "%Y-%m-%d")

monthly_prices <- ames_housing %>%
  group_by(MonthYear) %>%
  summarize(AveragePrice = mean(SalePrice, na.rm = TRUE))

ggplot(monthly_prices, aes(x = MonthYear, y = AveragePrice)) +
  geom_line(color = "dodgerblue", size = 1.2) +  
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +  
  labs(title = "Monthly Average Sale Prices",
       x = "Month-Year", y = "Average Sale Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),  
    legend.position = "none"  
  )

## Heatmap of Average Sales Price by Month and Year
monthly_prices$Year <- year(monthly_prices$MonthYear)
monthly_prices$Month <- factor(month(monthly_prices$MonthYear, label = TRUE), levels = month.abb)

ggplot(monthly_prices, aes(x = Month, y = Year, fill = AveragePrice)) +
  geom_tile(color = "white") +  
  scale_fill_gradient(low = "lightblue", high = "darkred") +  
  labs(title = "Heatmap of Average Sale Prices by Month and Year",
       x = "Month", y = "Year") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),  
    panel.grid = element_blank()  
  )

## Seasonal Trends in Sales Price
ames_housing$Season <- factor(
  cut(ames_housing$MoSold, breaks = c(0, 3, 6, 9, 12), labels = c("Winter", "Spring", "Summer", "Fall")),
  levels = c("Winter", "Spring", "Summer", "Fall")
)

ggplot(ames_housing, aes(x = Season, y = SalePrice, fill = Season)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +  
  geom_jitter(width = 0.2, size = 2, alpha = 0.5, color = "black") +  
  scale_fill_brewer(palette = "Set2") +  
  labs(title = "Seasonal Trends in Sale Prices", x = "Season", y = "Average Sale Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.position = "none"  
  )

## Density of Sales Price of House by Season
ggplot(ames_housing, aes(x = SalePrice, fill = Season)) +
  geom_density(alpha = 0.6) +
  scale_fill_brewer(palette = "Set2") +  
  labs(title = "Density of Sale Prices by Season",
       x = "Sale Price", y = "Density") +
  theme_minimal() +
  theme(
    legend.position = "top",  
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12)  
  )

## Violin Plots of Sales Price by Season
ggplot(ames_housing, aes(x = Season, y = SalePrice, fill = Season)) +
  geom_violin(trim = FALSE, alpha = 0.8) +  
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  
  labs(title = "Violin Plots of Sale Prices by Season",
       x = "Season", y = "Sale Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12), 
    legend.position = "none"  
  )

seasonal_stats <- ames_housing %>%
  group_by(Season) %>%
  summarize(
    Average = mean(SalePrice, na.rm = TRUE),
    Median = median(SalePrice, na.rm = TRUE),
    Variance = var(SalePrice, na.rm = TRUE),
    SD = sd(SalePrice, na.rm = TRUE)
  )

print(seasonal_stats)


## Seasonal Trends affect in Sale Price of Houses in Neighborhood
ames_housing$Season <- cut(ames_housing$MoSold,
                           breaks = c(1, 3, 6, 9, 12),
                           labels = c("Winter", "Spring", "Summer", "Fall"),
                           right = FALSE)

median_prices_by_season <- ames_housing %>%
  group_by(Neighborhood, Season) %>%
  summarize(MedianSalePrice = median(SalePrice, na.rm = TRUE), .groups = 'drop')

ggplot(median_prices_by_season, aes(x = Neighborhood, y = MedianSalePrice, fill = Season)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Seasonal Trends in House Sale Prices by Neighborhood",
       x = "Neighborhood",
       y = "Median Sale Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")  

## Boxplot of Sales Price by Quality and Condition of House
palette15 <- colorRampPalette(brewer.pal(9, "Set3"))(15)

ggplot(ames_housing, aes(x = as.factor(OverallQual), y = SalePrice, fill = as.factor(OverallCond))) +
  geom_boxplot() +
  scale_fill_manual(values = palette15) +
  labs(title = "Boxplot of Sale Prices by Quality and Condition",
       x = "Overall Quality", y = "Sale Price", fill = "Overall Condition") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12), 
    legend.position = "right"  
  )


## Violin Plots of Sales Price by Overall Quality of House
ggplot(ames_housing, aes(x = as.factor(OverallQual), y = SalePrice, fill = as.factor(OverallCond))) +
  geom_violin(trim = FALSE, alpha = 0.8) +  
  scale_fill_manual(values = palette15) +  
  labs(title = "Violin Plots of Sale Prices by Quality and Condition",
       x = "Overall Quality", y = "Sale Price",
       fill = "Overall Condition") +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"), 
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.position = "right"  
  )


## Scatter Plot of Sales Price by Overall Quality of House
ggplot(ames_housing, aes(x = as.factor(OverallQual), y = SalePrice, color = as.factor(OverallCond))) +
  geom_jitter(alpha = 0.6, shape = 16, width = 0.2) +  
  scale_color_manual(values = palette15) +  
  labs(title = "Scatter Plot of Sale Prices by Quality and Condition",
       x = "Overall Quality", y = "Sale Price",
       color = "Overall Condition") +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12)  
  )

quality_condition_stats <- ames_housing %>%
  group_by(OverallQual, OverallCond) %>%
  summarize(
    Count = n(),
    Average = mean(SalePrice, na.rm = TRUE),
    Median = median(SalePrice, na.rm = TRUE),
    Variance = var(SalePrice, na.rm = TRUE),
    SD = sd(SalePrice, na.rm = TRUE),
    .groups = 'drop'
  )

print(quality_condition_stats)


## Overall Quality and Overall Condition of Houses in Neighborhood
neighborhood_quality_stats <- ames_housing %>%
  group_by(Neighborhood) %>%
  summarize(
    AvgQuality = mean(OverallQual, na.rm = TRUE),
    HouseCount = n(),
    AvgSalePrice = mean(SalePrice, na.rm = TRUE),
    .groups = 'drop'  
  ) %>%
  arrange(desc(AvgQuality))  

ggplot(neighborhood_quality_stats, aes(x = Neighborhood, y = AvgQuality, size = HouseCount, color = AvgSalePrice)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "blue", high = "red") +  
  scale_size(range = c(3, 12), name = "House Count") +  
  labs(title = "Neighborhood Quality, Volume, and Value",
       x = "Neighborhood",
       y = "Average Quality",
       color = "Average Sale Price",
       size = "House Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "right"
  )


## Impact of Garage on Sale Price of House
ggplot(ames_housing, aes(x = as.factor(GarageCars), y = SalePrice, fill = GarageType)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Impact of Garage Cars and Type on Sale Price",
       x = "Number of Cars in Garage", y = "Sale Price", fill = "Garage Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.position = "bottom"  
  )

## Violin and Scatter Plot of Sale Price by GarageType
ggplot(ames_housing, aes(x = as.factor(GarageCars), y = SalePrice, fill = GarageType)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  
  geom_jitter(width = 0.1, alpha = 0.5, color = "black", size = 2) +  
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Violin and Scatter Plot of Sale Prices by Garage Cars and Type",
       x = "Number of Cars in Garage", y = "Sale Price",
       fill = "Garage Type") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),  
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12)  
  )

garage_stats <- ames_housing %>%
  group_by(GarageType, GarageCars) %>%
  summarize(
    Count = n(),
    Average = mean(SalePrice, na.rm = TRUE),
    Median = median(SalePrice, na.rm = TRUE),
    Variance = var(SalePrice, na.rm = TRUE),
    SD = sd(SalePrice, na.rm = TRUE),
    .groups = 'drop'
  )

print(garage_stats)

## Garage Area and Neighborhood
garage_by_neighborhood <- ames_housing %>%
  group_by(Neighborhood, GarageType) %>%
  summarize(
    TotalGarages = n(),  
    AverageCars = mean(GarageCars, na.rm = TRUE),  
    .groups = 'drop'
  ) %>%
  arrange(desc(TotalGarages))  

print(garage_by_neighborhood)

ggplot(garage_by_neighborhood, aes(x = Neighborhood, y = TotalGarages, fill = GarageType)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Garage Availability by Neighborhood and Type",
       x = "Neighborhood",
       y = "Total Garages",
       fill = "Garage Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "Set1")


## Sale Price of House by its age of remodeling
ames_housing$AgeSinceRemodel <- ifelse(
  is.na(ames_housing$YearRemodAdd),
  ames_housing$YrSold - ames_housing$YearBuilt,  
  ames_housing$YrSold - ames_housing$YearRemodAdd  
)

ames_housing$AgeCategory <- cut(
  ames_housing$AgeSinceRemodel,
  breaks = c(-Inf, 0, 5, 10, 15, 20, Inf),  
  labels = c("Newly remodeled", "1-5 years", "6-10 years", "11-15 years", "16-20 years", "Over 20 years"),
  include.lowest = TRUE  
)

ggplot(ames_housing, aes(x = AgeCategory, y = SalePrice, fill = AgeCategory, color = as.factor(OverallQual))) +
  geom_violin(trim = FALSE) +
  labs(title = "Sale Price of Houses by Age Since Remodel and Overall Quality",
       x = "Age Since Remodel Category",
       y = "Sale Price",
       color = "Overall Quality") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") 

## Sale Price of House in Top 5 Neighborhood by the age of Remodeling
top_neighborhoods <- ames_housing %>%
  group_by(Neighborhood) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count)) %>%
  top_n(5, Count) %>%
  pull(Neighborhood)

filtered_data <- ames_housing %>%
  filter(Neighborhood %in% top_neighborhoods)

ggplot(filtered_data, aes(x = AgeCategory, y = SalePrice, fill = AgeCategory, color = as.factor(OverallQual))) +
  geom_violin(trim = FALSE) +
  facet_wrap(~Neighborhood, scales = "free_y") +  
  labs(title = "Sale Price of Houses by Age Since Remodel and Overall Quality in Top 5 Neighborhoods",
       x = "Age Since Remodel Category",
       y = "Sale Price",
       color = "Overall Quality") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 8, face = "bold"),
        legend.position = "right") 


## Sale Price of House based on its built year and house style across different Neighborhoods
ames_housing$IsNew <- ifelse(
  ames_housing$YearBuilt >= (ames_housing$YrSold - 5) & !is.na(ames_housing$YearBuilt), 
  1,  
  ifelse(
    !is.na(ames_housing$YearBuilt),  
    0,
    NA  
  )
)

new_houses_prices_type <- ames_housing %>%
  filter(IsNew == 1) %>%
  group_by(Neighborhood, HouseStyle, OverallQual) %>%
  summarise(AverageSalePrice = mean(SalePrice, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Neighborhood, desc(AverageSalePrice))

neighborhoods_with_new_houses <- new_houses_prices_type %>%
  filter(AverageSalePrice > 0) %>%
  pull(Neighborhood)

ggplot(new_houses_prices_type, aes(x = Neighborhood, y = AverageSalePrice, fill = HouseStyle)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~OverallQual, scales = "free", labeller = label_both) +  
  labs(title = "Average Sale Price of New Houses by Neighborhood, House Type, and Overall Quality",
       x = "Neighborhood",
       y = "Average Sale Price") +
  scale_fill_brewer(palette = "Set2", name = "House Type") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 8, face = "bold")) +
  guides(fill = guide_legend(title = "House Type"))


## Sale Price of House by Renovation Status accross Neighborhoods
ames_housing$WasRenovated <- ifelse(
  !is.na(ames_housing$YearRemodAdd) & !is.na(ames_housing$YearBuilt),  
  ifelse(
    ames_housing$YearRemodAdd > ames_housing$YearBuilt,  
    1,  
    0   
  ),
  NA  
)

sale_prices_by_reno_status <- ames_housing %>%
  group_by(Neighborhood, WasRenovated, OverallQual) %>%
  summarise(AverageSalePrice = mean(SalePrice, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Neighborhood, desc(AverageSalePrice))

ggplot(sale_prices_by_reno_status, aes(x = Neighborhood, y = AverageSalePrice, fill = as.factor(WasRenovated))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~OverallQual, scales = "free", labeller = label_both) +  
  scale_fill_manual(values = c("0" = "red", "1" = "green"), labels = c("0" = "Not Renovated", "1" = "Renovated")) +
  labs(title = "Average Sale Price of Houses by Renovation Status, Overall Quality, and Neighborhood",
       x = "Neighborhood",
       y = "Average Sale Price",
       fill = "Renovation Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 8, face = "bold")) +
  guides(fill = guide_legend(title = "Renovation Status"))


## Sale Price of House based on House Style in Neighborhood
ames_housing$TotalSF <- ifelse(
  !is.na(ames_housing$X1stFlrSF) & !is.na(ames_housing$X2ndFlrSF) & !is.na(ames_housing$TotalBsmtSF),
  ames_housing$X1stFlrSF + ames_housing$X2ndFlrSF + ames_housing$TotalBsmtSF,
  NA  
)

small_threshold <- 1000  
medium_threshold <- 2500  
ames_housing$HouseAreaCategory <- cut(ames_housing$TotalSF, 
                                      breaks = c(0, small_threshold, medium_threshold, Inf), 
                                      labels = c("Small", "Medium", "Large"),
                                      include.lowest = TRUE)

overall_neighborhood_avg_price <- ames_housing %>%
  group_by(Neighborhood) %>%
  summarise(AverageSalePrice = mean(SalePrice, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(AverageSalePrice)) %>%
  slice_head(n = 10)

top_neighborhoods_data <- ames_housing %>%
  filter(Neighborhood %in% overall_neighborhood_avg_price$Neighborhood)

neighborhood_size_avg_price <- top_neighborhoods_data %>%
  group_by(Neighborhood, HouseAreaCategory, OverallQual) %>%
  summarise(AverageSalePrice = mean(SalePrice, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Neighborhood, desc(AverageSalePrice))

p <- ggplot(neighborhood_size_avg_price, aes(x = reorder(Neighborhood, -AverageSalePrice), y = AverageSalePrice, fill = HouseAreaCategory)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~OverallQual, scales = "free", labeller = label_both) +  
  labs(title = "Average Sale Price by House Size and Overall Quality in Top 10 Neighborhoods",
       x = "Neighborhood",
       y = "Average Sale Price",
       fill = "House Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 8, face = "bold"),
        legend.position = "right")

p_plotly <- ggplotly(p) %>% 
  layout(title = "Average Sale Price by House Size and Overall Quality in Top 10 Neighborhoods",
         xaxis = list(title = "Neighborhood"),
         yaxis = list(title = "Average Sale Price"),
         legend = list(title = list(text = "House Size")),
         hovermode = "closest")

p_plotly


## Model 1: Sale Price vs OverallCond
p <- ggplot(ames_housing, aes(OverallCond, SalePrice)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title="Sale Price vs. Overall Condition",
       x="Overall Condition",
       y="Sale Price ($)")

m1_cond <- lm(SalePrice ~ OverallCond, data = ames_housing)

predictions <- predict(m1_cond, ames_housing)
residuals <- ames_housing$SalePrice - predictions
rmse <- sqrt(mean(residuals^2))

p + labs(subtitle = paste("RMSE:", round(rmse, 2)))
tidy(m1_cond)


## Model2: Sale Price vs OverallQual
p <- ggplot(ames_housing, aes(OverallQual, SalePrice)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title="Sale Price vs. Overall Quality",
       x="Overall Quality",
       y="Sale Price ($)")

m2_qual <- lm(SalePrice ~ OverallQual, data = ames_housing)

predictions <- predict(m2_qual, ames_housing)
residuals <- ames_housing$SalePrice - predictions
rmse <- sqrt(mean(residuals^2))

p + labs(subtitle = paste("RMSE:", round(rmse, 2)))
tidy(m2_qual)


## Model3: Sale Price vs Garage Area
p <- ggplot(ames_housing, aes(GarageArea, SalePrice)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title="Sale Price vs. Garage Area",
       x="Garage Area (sq. ft.)",
       y="Sale Price ($)")

m3_garage <- lm(SalePrice ~ GarageArea, data = ames_housing)

predictions <- predict(m3_garage, ames_housing)
residuals <- ames_housing$SalePrice - predictions
rmse <- sqrt(mean(residuals^2))

p + labs(subtitle = paste("RMSE:", round(rmse, 2)))
tidy(m3_garage)


## Model 4: Sale Price vs Living Area
p <- ggplot(ames_housing, aes(GrLivArea, SalePrice)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title="Sale Price vs. Living Area",
       x="Living Area (sq. ft.)",
       y="Sale Price ($)")

m4_living <- lm(SalePrice ~ GrLivArea, data = ames_housing)

predictions <- predict(m4_living, ames_housing)
residuals <- ames_housing$SalePrice - predictions
rmse <- sqrt(mean(residuals^2))

p + labs(subtitle = paste("RMSE:", round(rmse, 2)))
tidy(m4_living)


## Predicting Sale Prices of Properties (Houses) described by Test Dataset
m1_cond <- lm(SalePrice ~ OverallCond, data = ames_housing)
m2_qual <- lm(SalePrice ~ OverallQual, data = ames_housing)
m3_garage <- lm(SalePrice ~ GarageArea, data = ames_housing)
m4_living <- lm(SalePrice ~ GrLivArea, data = ames_housing)

ameshous_test_data <- ameshous_test_data %>%
  mutate(
    Pred_SalePrice_Cond = predict(m1_cond, newdata = ameshous_test_data),
    Pred_SalePrice_Qual = predict(m2_qual, newdata = ameshous_test_data),
    Pred_SalePrice_Garage = predict(m3_garage, newdata = ameshous_test_data),
    Pred_SalePrice_Living = predict(m4_living, newdata = ameshous_test_data)
  )

ameshous_test_data %>%
  select(OverallCond, OverallQual, GarageArea, GrLivArea, 
         Pred_SalePrice_Cond, Pred_SalePrice_Qual, 
         Pred_SalePrice_Garage, Pred_SalePrice_Living) %>%
  head()


## Visualising the Predicted Sale Prices of Properties in Test Dataset
plot_data <- ameshous_test_data %>%
  select(OverallCond, GarageArea, GrLivArea, OverallQual, Pred_SalePrice_Cond, Pred_SalePrice_Qual, Pred_SalePrice_Garage, Pred_SalePrice_Living) %>%
  pivot_longer(cols = starts_with("Pred"), names_to = "Model", values_to = "PredictedPrice")

plot_data$Model <- factor(plot_data$Model, levels = c("Pred_SalePrice_Cond", "Pred_SalePrice_Qual", "Pred_SalePrice_Garage", "Pred_SalePrice_Living"),
                          labels = c("Condition", "Quality", "Garage Area", "Living Area"))

ggplot(plot_data, aes(x = Model, y = PredictedPrice, color = Model, shape = Model)) +
  geom_point(alpha = 0.6, size = 3) +  
  scale_color_brewer(palette = "Set1") +  
  geom_smooth(method = "lm", se = FALSE, aes(group = Model), linetype = "dashed") +
  labs(title = "Predicting Sale Prices of properties using Developed Models in Testdata",
       x = "Models",
       y = "Predicted Sale Price",
       color = "Model",
       shape = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 5)),
         shape = guide_legend(override.aes = list(size = 5)))

rsquared <- plot_data %>%
  group_by(Model) %>%
  summarise(Rsquared = summary(lm(PredictedPrice ~ OverallQual))$r.squared, .groups = "keep")

cat("R-squared values for each model:\n")
print(rsquared)
cat("\n")

best_model <- rsquared$Model[which.max(rsquared$Rsquared)]
cat("Best model (Highest R-squared):", best_model, "\n")


## Evaluating the model - Model Assessment
### Fit the models using the predicted sale prices (e.g., Pred_SalePrice_Cond, etc.)
m1_cond_pred <- lm(Pred_SalePrice_Cond ~ OverallCond, data = ameshous_test_data)
m2_qual_pred <- lm(Pred_SalePrice_Qual ~ OverallQual, data = ameshous_test_data)
m3_garage_pred <- lm(Pred_SalePrice_Garage ~ GarageArea, data = ameshous_test_data)
m4_living_pred <- lm(Pred_SalePrice_Living ~ GrLivArea, data = ameshous_test_data)


### Store the models in a list
models_pred <- list(
  "Sale Price ~ Overall Condition" = m1_cond_pred,
  "Sale Price ~ Overall Quality" = m2_qual_pred,
  "Sale Price ~ Garage Area" = m3_garage_pred,
  "Sale Price ~ Living Area" = m4_living_pred
)

### Summarize the models
modelsummary(models_pred)


## Model Diagnostics- Performing Residual Diagnostics-Model 1: Sale Price vs OverallCond
m1_cond_pred <- lm(Pred_SalePrice_Cond ~ OverallCond, data = ameshous_test_data)
# Fit the model using the predicted sale prices (e.g., Pred_SalePrice_Cond)
m1_cond_pred <- lm(Pred_SalePrice_Cond ~ OverallCond, data = ameshous_test_data)

# Augment the model with residuals and fitted values
m1_aug_cond_pred <- augment(m1_cond_pred)

# Residual vs. Fitted Plot
ggplot(data = m1_aug_cond_pred, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals") +
  theme_minimal()

# Histogram of Residuals
ggplot(data = m1_aug_cond_pred, aes(x = .resid)) +
  geom_histogram(color = 'red', fill = 'skyblue', bins = 30) +
  xlab("Residuals") +
  theme_minimal()

# Model diagnostics: check for heteroscedasticity
check_model(m1_cond_pred)
check_heteroscedasticity(m1_cond_pred)

# Compute RMSE (Root Mean Squared Error) based on residuals
rmse_m1_cond_pred <- sqrt(mean(m1_aug_cond_pred$.resid^2))
print(paste("RMSE:", rmse_m1_cond_pred))


## Model 2: Sale Price vs Overall Quality
# Fit the model using SalePrice ~ OverallQual
m2_qual <- lm(Pred_SalePrice_Qual ~ OverallQual, data = ameshous_test_data)

# Augment the model with residuals and fitted values
m2_aug_qual <- augment(m2_qual)

# Residual vs. Fitted Plot
ggplot(data = m2_aug_qual, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals") +
  theme_minimal()

# Histogram of Residuals
ggplot(data = m2_aug_qual, aes(x = .resid)) +
  geom_histogram(color = 'red', fill = 'skyblue', bins = 30) +
  xlab("Residuals") +
  theme_minimal()

# Model diagnostics: check for heteroscedasticity
check_model(m2_qual)
check_heteroscedasticity(m2_qual)

# Compute RMSE (Root Mean Squared Error) based on residuals
rmse_m2_qual <- sqrt(mean(m2_aug_qual$.resid^2))
print(paste("RMSE:", rmse_m2_qual))


## Model 3: Sale Price vs GarageArea
m3_garage <- lm(Pred_SalePrice_Garage ~ GarageArea, data = ameshous_test_data)

m3_aug_garage <- augment(m3_garage)

ggplot(data = m3_garage, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals") +
  theme_minimal()

ggplot(data = m3_garage, aes(x = .resid)) +
  geom_histogram(color = 'red', fill = 'skyblue', bins = 30) +
  xlab("Residuals") +
  theme_minimal()

check_model(m3_garage)
check_heteroscedasticity(m3_garage)

rmse_m3_garage <- sqrt(mean(m3_aug_garage$.resid^2))
print(paste("RMSE:", rmse_m3_garage))


## Model 4: Sale Price vs GrLivArea (Living Area)
m4_living <- lm(Pred_SalePrice_Living ~ GrLivArea, data = ameshous_test_data)

m4_aug_living <- augment(m4_living)

ggplot(data = m4_living, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals") +
  theme_minimal()

ggplot(data = m4_living, aes(x = .resid)) +
  geom_histogram(color = 'red', fill = 'skyblue', bins = 30) +
  xlab("Residuals") +
  theme_minimal()

check_model(m4_living)
check_heteroscedasticity(m4_living)

rmse_m4_living <- sqrt(mean(m4_aug_living$.resid^2))
print(paste("RMSE:", rmse_m4_living))


## Additional Diagnostics in Models
bptest(m1_cond)
bptest(m2_qual)
bptest(m3_garage)
bptest(m4_living)


## Normalisation of Target Variable "Sale Price" using log
ames_housing <- ames_housing %>% 
  mutate(sale_ames = log(SalePrice))

ggplot(ames_housing) +
  geom_histogram(aes(sale_ames), color = "black", fill="orange")

ames_housing %>%
  summarise(cor(OverallQual, sale_ames))


m5 <- lm(sale_ames ~ OverallQual, data = ames_housing)
tidy(m5)
glance(m5)

m5_aug <- augment(m5)

ggplot(data = m5_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals")+
  theme_minimal()


ggplot(data = m5_aug, aes(x = .resid)) +
  geom_histogram(color = 'red', fill = 'skyblue', binwidth = 0.05) + 
  xlab("Residuals") +
  theme_minimal()


check_model(m5)
check_heteroscedasticity(m5)