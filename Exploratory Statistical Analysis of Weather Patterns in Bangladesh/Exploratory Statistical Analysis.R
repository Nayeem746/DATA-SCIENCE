
dataset <- read.csv("C:/Users/nayee/Downloads/AIUB/bangladesh_weather_dataset.csv")


dim(dataset)      
str(dataset)       
head(dataset)       
sum(is.na(dataset))  




# 1. Pearson Correlation
pearson_result <- cor.test(dataset$Temperature, dataset$Humidity, method = "pearson")
print("Pearson Correlation (Temperature vs. Humidity):")
print(pearson_result)


# 2. Spearman Correlation
spearman_result <- cor.test(dataset$Temperature, dataset$Humidity, method = "spearman")
print("\nSpearman Correlation (Temperature vs. Humidity):")
print(spearman_result)


# 3. Kendall Correlation
kendall_result <- cor.test(dataset$Temperature, dataset$Humidity, method = "kendall")
print("\nKendall Correlation (Temperature vs. Humidity):")
print(kendall_result)



# 4. Chi-Squared Test
table_location_rainfall <- table(dataset$Location, dataset$RainfallCategory)
chisq_result <- chisq.test(table_location_rainfall)
print("\nChi-Squared Test (Location vs. RainfallCategory):")
print(chisq_result)


# 5. ANOVA
anova_result <- aov(Temperature ~ Location, data = dataset)
print("\nANOVA (Temperature vs. Location):")
print(summary(anova_result))
