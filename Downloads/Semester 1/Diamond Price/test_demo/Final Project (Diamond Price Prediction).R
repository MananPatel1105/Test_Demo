# Final Project 

# Diamond Price Prediction 

# Importing the Data-set

library(readr)
df = read.csv("C:/Users/manan/Downloads/Semester 1/data.csv")
View(df)

#Now let's Check the shape of the Data-set
dim(df)

# To Check data types and sample values.
str(df)
#As we can see that the datatype of Messurements is character we will change that later on


# Now let's check summary of our dataset
summary(df)

# Checking for the Null Values 
sum(is.na(df))                 # As we can see that there are no null values in our dataset

# Name of all the columns from our data-set
colnames(df)


# Data Cleaning

# Creating 3 New Columns using the Messurements column and dropping the data Url Column
# 1) Length
# 2) Width
# 3) Depth


library(tidyr)
library(dplyr)

# Splitting the 'Measurements' column into 'Length' and 'WidthXDepth'
df <- df %>% separate(Messurements, into = c("Length", "WidthXDepth"), sep = "-", remove = TRUE)

# Splitting 'WidthXDepth' into 'Width' and 'Depth'
df <- df %>% separate(WidthXDepth, into = c("Width", "Depth"), sep = "×", remove = TRUE)

# Dropping the 'Data Url' column
df <- df %>% select(-Data.Url)




# Now 2 of the columns in Width and Height have Nan Values and Length has those values so manually replacing those values in the dataset
# [1073, 1948]
df[nchar(df$Length) != 4, ]

# Let's split them manually
df[c(1073, 1948), c("Length", "Width", "Depth")] <- c("5.10", "5.06", "3.15")


#Same applies for Width and Height column we have values splitting with X
df[nchar(df$Width) != 4, ]

#Finding this location and then replacing it
for (j in 1:nrow(df)) {
  if (nchar(df$Width[j]) > 4) {
    df$Width[j] <- substr(df$Width[j], 1, 4)
    df$Depth[j] <- substr(df$Width[j], 6, nchar(df$Width[j]))
  }
}


# Converting Datatype of Length, Width and Height to float
# Convert 'Length', 'Width', and 'Depth' columns to numeric type
df$Length <- as.numeric(df$Length)
df$Width <- as.numeric(df$Width)
df$Depth <- as.numeric(df$Depth)





# Exploratory Data Analysis (EDA)

# Correlation Matrix 


# Install and load the packages
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)
# Select only numeric columns
numeric_df <- df %>% select_if(is.numeric)

# Calculate the correlation matrix
cor_matrix <- cor(numeric_df, use = "complete.obs")

# Melt the correlation matrix for ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "", y = "")

#Observation: -

# 1) Price is not related with this numerical columns
# 2) But most of the columns are related to each other like Height, Width, Length and Weight



# Length Vs Price

# Load necessary libraries
library(ggplot2)
library(patchwork)

# Scatter plot
scatter_plot <- ggplot(df, aes(x = Length, y = Price)) +
  geom_point(color = "green") +
  ggtitle("Scatter Plot: Length vs Price") +
  theme_minimal()

# Line plot
line_plot <- ggplot(df, aes(x = Length, y = Price)) +
  geom_line(color = "green") +
  scale_x_continuous(breaks = c(3.5, 4.0, 4.5, 5.0, 5.5)) +
  ggtitle("Line Plot: Length vs Price") +
  theme_minimal()

# Combine plots side by side
combined_plot <- scatter_plot + line_plot +
  plot_annotation(title = "Length VS Price")

# Display the combined plot
print(combined_plot)


# Observation: -
# 1)There is no Linear relationship between Length and Price as seen in Heatmap
# 2)But we can see high decrease in price when Length between (4.75-5.00)
# 3)Also Price is increasing after approx 5.25
# 4)Highest Price is at Aprox 4.6-4.7
# 5)There are Outliers too we will deal with them later


# Depth Vs Price 


# Scatter plot of Depth vs Price
scatter_plot <- ggplot(df, aes(x = Depth, y = Price)) +
  geom_point(color = "red") +
  ggtitle("Scatter Plot: Depth vs Price") +
  theme_minimal()

# Line plot of Depth vs Price
line_plot <- ggplot(df, aes(x = Depth, y = Price)) +
  geom_line(color = "red") +
  ggtitle("Line Plot: Depth vs Price") +
  theme_minimal()

# Combine the plots side by side
combined_plot <- scatter_plot + line_plot +
  plot_annotation(title = "Depth VS Price")

# Display the combined plot
print(combined_plot)

# Observation: -
# 1)Same as Length there is no Linear relationship between Depth and Price
# 2)But we also see high increase in Price after 3.3
# 3)Highest Price is at approx 3.3-3.4
# 4)There are Outliers too we will deal with them later


# Width Vs Price 

# Scatter plot of Width vs Price
scatter_plot <- ggplot(df, aes(x = Width, y = Price)) +
  geom_point(color = "blue") +
  ggtitle("Scatter Plot: Width vs Price") +
  theme_minimal()

# Line plot of Width vs Price
line_plot <- ggplot(df, aes(x = Width, y = Price)) +
  geom_line(color = "blue") +
  ggtitle("Line Plot: Width vs Price") +
  theme_minimal()

# Combine the plots side by side with a shared title
combined_plot <- scatter_plot + line_plot +
  plot_annotation(title = "Width VS Price")

# Display the combined plot
print(combined_plot)

# Observation: -
# 1)Again there is no Linear relationship between Width and Price
# 2)But we also see high increase in Price after 5.25 width
# 3)Highest Price is at approx 4.8-4.85 and 5.1
# 4)There are Outliers too we will deal with them later


# Weight Vs Price (in Carat)


# Line plot of Weight vs Price
line_plot <- ggplot(df, aes(x = Weight, y = Price)) +
  geom_line(color = "blue") +
  theme_minimal() +
  ggtitle("Line Plot: Weight vs Price")

# Display the Line plot
print(line_plot)

# Observation: -

# 1)Again there is no Linear relationship between Weight and Price
# 2)But we also see high increase in Price after 0.55 weigth
# 3)Highest Price is at approx 0.45 and 0.51-0.52
# 4)There are Outliers too we will deal with them later


# Countplot for Shape
# Convert "Shape" to a factor (if not already)


# Create the count plot
ggplot(df, aes(x = Shape)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Shape", x = "Shape", y = "Count")

#Observation: -
#The dataset is of Round Shape Diamonds so all of them are Round we will drop this column afterwards



# Clarity

library(ggplot2)
library(dplyr)
library(patchwork)



# Count plot of Clarity
count_plot <- ggplot(df, aes(x = Clarity)) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 0) +  # Add bar labels
  theme_minimal() +
  labs(title = "Count Plot: Clarity", x = "Clarity", y = "Count")

# Box plot of Clarity vs. Price
box_plot <- ggplot(df, aes(x = Clarity, y = Price)) +
  geom_boxplot(fill = "lightblue", color = "blue") +  # Box plot with color
  theme_minimal() +
  labs(title = "Box Plot: Clarity vs Price", x = "Clarity", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine the plots side by side with a shared title
combined_plot <- count_plot + box_plot +
  plot_annotation(title = "Clarity Analysis")

# Display the combined plot
print(combined_plot)

#IF – Internally Flawless
#VVS1 – Very Very Small Inclusions 1
#VVS2 – Very Very Small Inclusions 2
#VS1 – Very Small Inclusions 1
#VS2 – Very Small Inclusions 2
#SI1 – Small Inclusions 1
#SI2 – Small Inclusions 2
#I1 – Inclusions 1

# Observation: -

# 1) More than Half of Diamonds are of Clarity "Very Small Inclusions 1" and also have highest price too
# 2)Least one is "Inclusions 1" with only 2 Counts
# 3)Price for most of the diamond are same apart from l1 but it only have 2 counts so not including that
# 4)We will Combine the least ones to create a 'Other' as feature



# Colour

# Create the count plot
count_data <- df %>%
  count(Colour)

count_plot <- ggplot(count_data, aes(x = Colour, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), vjust = 0) +
  theme_minimal() +
  labs(title = "Count Plot: Colour", x = "Colour", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the box plot
box_plot <- ggplot(df, aes(x = Colour, y = Price)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  theme_minimal() +
  labs(title = "Box Plot: Colour vs Price", x = "Colour", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine the plots side by side
combined_plot <- count_plot + box_plot +
  plot_annotation(title = "Colour Analysis")

# Display the combined plot
print(combined_plot)




# Observation: -

# 1)More than Half of Diamonds are of Colour "E" and "F"
# 2)Least one are N, M, Q, R, O, P, U, V with only 2 Counts
# 3)Price for most of the diamond are same but the data is not more for half of them so we cannot compare prices with them
# 4)Highest price is for F Color
# 5)We will Combine the least ones to create a 'Other' as feature



# Cut
# Create the count plot
count_data <- df %>%
  count(Cut)
count_plot <- ggplot(count_data, aes(x = Cut, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), vjust = 0) +
  theme_minimal() +
  labs(title = "Count Plot: Cut", x = "Cut", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the box plot
box_plot <- ggplot(df, aes(x = Cut, y = Price)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  theme_minimal() +
  labs(title = "Box Plot: Cut vs Price", x = "Cut", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine the plots side by side with a shared title
combined_plot <- count_plot + box_plot +
  plot_annotation(title = "Cut Analysis")
# Display the combined plot
print(combined_plot)


# Observation: -

# 1)More than Half of Diamonds are of Cut "Excellent"
# 2)Least one is F with 8 counts
# 3)Price decreases with the Cut it makes sense as Excellent cut diamonds have more price than the other ones
# 4)Highest price is for EX Cut diamonds
# 5)We will Combine the least ones to create a 'Other' as 



# Polish
# Create the count plot
count_data <- df %>%
  count(Polish)

count_plot <- ggplot(count_data, aes(x = Polish, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +  # Add labels above the bars
  ylim(0, max(count_data$n) * 1.2) +  # Extend y-axis limits for visibility
  theme_minimal() +
  labs(title = "Count Plot: Polish", x = "Polish", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the box plot
box_plot <- ggplot(df, aes(x = Polish, y = Price)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  theme_minimal() +
  labs(title = "Box Plot: Polish vs Price", x = "Polish", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine the plots side by side with a shared title
combined_plot <- count_plot + box_plot +
  plot_annotation(title = "Polish Analysis")
# Display the combined plot
print(combined_plot)

#Observation: -


# 1)More than Half of Diamonds are of "Excellent" Polish
# 2)Least one is F with just 1 count
# 3)Price decreases with the Polish it makes sense as Excellent Polished diamonds have more price than the other ones
# 4)Highest price is for EX Polish diamonds
# 5)We will Combine the least ones to create a 'Other' as feature



# Symmetry
# Create the count plot
count_data <- df %>%
  count(Symmetry)

count_plot <- ggplot(count_data, aes(x = Symmetry, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), vjust = 0.1, size = 4) +  # Add count labels above the bars
  ylim(0, max(count_data$n) * 1.2) +  # Extend y-axis limits for label visibility
  theme_minimal() + labs(title = "Count Plot: Symmetry", x = "Symmetry", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the box plot
box_plot <- ggplot(df, aes(x = Symmetry, y = Price)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  theme_minimal() +
  labs(title = "Box Plot: Symmetry vs Price", x = "Symmetry", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine the plots side by side with a shared title
combined_plot <- count_plot + box_plot +
  plot_annotation(title = "Symmetry Analysis")
# Display the combined plot
print(combined_plot)


# Observation: -


# 1)More than Half of Diamonds are of "Excellent" Symmetry
# 2)Least one is FR with just 13 counts
# 3)Price decreases with the Symmetry it makes sense as Excellent Symmetry diamonds have more price than the other ones
# 4)Highest price is for EX Symmetry diamonds
# 5)We will Combine the least ones to create a 'Other' as feature



# Fluoresence
# Create the count plot
count_data <- df %>%
  count(Fluorescence)

count_plot <- ggplot(count_data, aes(x = Fluorescence, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +  # Add count labels above the bars
  ylim(0, max(count_data$n) * 1.2) +  # Extend y-axis limits for label visibility
  theme_minimal() + labs(title = "Count Plot: Fluorescence", x = "Fluorescence", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the box plot
box_plot <- ggplot(df, aes(x = Fluorescence, y = Price)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  theme_minimal() +
  labs(title = "Box Plot: Fluorescence vs Price", x = "Fluorescence", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine the plots side by side with a shared title
combined_plot <- count_plot + box_plot +
  plot_annotation(title = "Fluorescence Analysis")
# Display the combined plot
print(combined_plot)


# Observation: -
# 1)More than Half of Diamonds are of "N" Fluorescence
# 2)Least one are ST and M with just 49 and 38 counts respectively
# 3)Highest price is for N Fluorescence diamonds
# 4)We will Combine the least ones to create a 'Other' as feature





#Combining the features with minimum count as 'Other' in this columns
#Colour,Clarity,Cut,Polish,Symmetry


# Load necessary library
library(dplyr)

# Colour
df$Colour <- ifelse(df$Colour %in% c("N", "M"), "Other", df$Colour)
df$Colour <- ifelse(df$Colour %in% c("O-P", "U-V"), "Other", df$Colour)
df$Colour <- ifelse(df$Colour == "Q-R", "Other", df$Colour)

# Clarity
df$Clarity <- ifelse(df$Clarity %in% c("VVS1", "VVS2"), "VVS", df$Clarity)
df$Clarity <- ifelse(df$Clarity %in% c("SI1", "SI2"), "SI", df$Clarity)
df$Clarity <- ifelse(df$Clarity %in% c("VS1", "VS2"), "VS", df$Clarity)
df$Clarity <- ifelse(df$Clarity %in% c("IF", "I1"), "Other", df$Clarity)

# Cut (Only 8 counts of F so replacing Fair with Very Good)
df$Cut <- ifelse(df$Cut == "F", "GD", df$Cut)

# Polish (Only 1 counts of F so replacing Fair with Very Good)
df$Polish <- ifelse(df$Polish == "F", "GD", df$Polish)

# Symmetry (Only 13 counts of F so replacing Fair with Very Good)
df$Symmetry <- ifelse(df$Symmetry == "FR", "GD", df$Symmetry)




# Function to create pie chart
windows(width = 1080, height = 720)  # For Windows
# Set margins to increase plot area (bottom, left, top, right)
par(mfrow = c(2, 3), mar = c(3, 3, 3, 3))

create_pie_chart <- function(column, title) {
  counts <- table(column)
  labels <- names(counts)
  percentages <- round(100 * counts / sum(counts), 1)
  pie_labels <- paste(percentages, "%")
  
  # Create the pie chart with increased size
  pie(counts,
      labels = pie_labels,
      col = rainbow(length(counts)),
      main = title,
      cex = 0.9,        # Increase label size
      cex.main = 2,     # Increase title size
      edges = 500,      # Increase the smoothness of pie edges
      clockwise = TRUE,
      init.angle = 90,
      border = "black")
}


# Create pie charts for each category
create_pie_chart(df$Colour, "Colour")
create_pie_chart(df$Clarity, "Clarity")
create_pie_chart(df$Cut, "Cut")
create_pie_chart(df$Polish, "Polish")
create_pie_chart(df$Symmetry, "Symmetry")

# Reset plotting layout
par(mfrow = c(1, 1))






# Dealing with the Outliers

# Identify numerical and categorical columns
numerical_columns <- c("Weight", "Length", "Width", "Depth")
categorical_columns <- setdiff(colnames(df), c(numerical_columns, "Id", "Price"))

# Function to create Window
windows(width = 1080, height = 720)  # For Windows
# Create individual box plots using `coef` for whisker length
boxplot1 <- ggplot(df, aes(y = Weight)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, coef = 3) +
  labs(title = "Box Plot of Weight") +
  theme_minimal()

boxplot2 <- ggplot(df, aes(y = Length)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, coef = 3) +
  labs(title = "Box Plot of Length") +
  theme_minimal()

boxplot3 <- ggplot(df, aes(y = Width)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, coef = 3) +
  labs(title = "Box Plot of Width") +
  theme_minimal()

boxplot4 <- ggplot(df, aes(y = Depth)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, coef = 3) +
  labs(title = "Box Plot of Depth") +
  theme_minimal()

# Arrange the box plots in a 2x2 grid
grid.arrange(boxplot1, boxplot2, boxplot3, boxplot4, ncol = 2, top = "Box Plots")



# Function to create Window
windows(width = 1080, height = 720)  # For Windows
# Calculate z-scores for numerical columns
z_scores <- as.data.frame(scale(df[numerical_columns]))

# Define the threshold for z-scores
threshold <- 3

# Identify rows with any z-score greater than the threshold
outliers <- apply(z_scores, 1, function(x) any(abs(x) > threshold))

# Filter the data to remove outliers
df_clean <- df[!outliers, ]

# Create individual box plots after removing outliers
boxplot1 <- ggplot(df_clean, aes(y = Weight)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, coef = 3) +
  labs(title = "Box Plot of Weight (After Outlier Removal)") +
  theme_minimal()

boxplot2 <- ggplot(df_clean, aes(y = Length)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, coef = 3) +
  labs(title = "Box Plot of Length (After Outlier Removal)") +
  theme_minimal()

boxplot3 <- ggplot(df_clean, aes(y = Width)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, coef = 3) +
  labs(title = "Box Plot of Width (After Outlier Removal)") +
  theme_minimal()

boxplot4 <- ggplot(df_clean, aes(y = Depth)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, coef = 3) +
  labs(title = "Box Plot of Depth (After Outlier Removal)") +
  theme_minimal()

# Arrange the box plots in a 2x2 grid
grid.arrange(boxplot1, boxplot2, boxplot3, boxplot4, ncol = 2, top = "Box Plots (After Removing Outliers)")



# Hist Plots

# Function to create Window
windows(width = 1080, height = 720)  # For Windows
# Create individual histogram plots with KDE (density curve)
hist_plot1 <- ggplot(df, aes(x = Weight)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Weight", x = "Weight", y = "Density") +
  theme_minimal()

hist_plot2 <- ggplot(df, aes(x = Length)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Length", x = "Length", y = "Density") +
  theme_minimal()

hist_plot3 <- ggplot(df, aes(x = Width)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Width", x = "Width", y = "Density") +
  theme_minimal()

hist_plot4 <- ggplot(df, aes(x = Depth)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Depth", x = "Depth", y = "Density") +
  theme_minimal()

# Arrange the histogram plots in a 2x2 grid
grid.arrange(hist_plot1, hist_plot2, hist_plot3, hist_plot4, ncol = 2, top = "Hist Plots with Density Curves")





# Feature Scaling

# Converting the Categorical Columns into Numerical Columns
# Identify categorical columns excluding "Colour"
categorical_columns <- c("Clarity", "Cut", "Polish", "Symmetry","Fluorescence")

# Apply label encoding (factor conversion) to all categorical columns except "Colour"
for (col in categorical_columns) {
  df[[col]] <- as.numeric(factor(df[[col]], levels = unique(df[[col]])))
}

library(fastDummies)

# One-hot encode using fastDummies
df <- dummy_cols(df, select_columns = "Colour", remove_selected_columns = TRUE)





# Standardization of a Numerical Columns

library(scales)
numerical_columns <- c("Weight", "Length", "Width", "Depth")

# Standardization
df[numerical_columns] <- lapply(df[numerical_columns], scale)

# Drop the columns "Id" and "Shape"
df <- df %>% select(-Id, -Shape)

# Move "Price" column to the last position
df <- df %>% select(-Price, everything(), Price)




#Correlation Matrix

# Load necessary libraries
library(corrplot)

windows(width = 1080, height = 720)  # For Windows

# Calculate the correlation matrix
cor_matrix <- cor(df, use = "complete.obs")

# Plot the heatmap
corrplot(cor_matrix, method = "color", tl.col = "black", tl.cex = 0.8, number.cex = 0.6, addCoef.col = "black", mar = c(1, 1, 1, 1))

# Observation: -
# Price is mostly related with this columns
#Cut,Polish,Symmetry,Flourescene,Colour






# Splitting the Data into Train and Test

library(caTools)

# Define the target variable and features
X <- df[, !(names(df) %in% "Price")]
y <- df$Price

# Set seed for reproducibility
set.seed(42)

# Split the data (75% training, 25% testing)
split <- sample.split(y, SplitRatio = 0.75)

# Create training and testing sets
x_train <- X[split, ]
x_test <- X[!split, ]
y_train <- y[split]
y_test <- y[!split]

# Print the dimensions of the training and testing sets
cat("Train Size: -", dim(x_train), "\n\n")
cat("Test Size: -", dim(x_test), "\n")


# All the required libraries for model devlopment
library(ggplot2)
library(caret)
library(randomForest)
library(rpart)
library(Metrics)




# Linear Regression Model
windows(width = 1080, height = 720)  # For Windows

# Combine x_train and y_train to create a training data frame
train_data <- cbind(x_train, Price = y_train)

# Fit a linear regression model
lm_model <- lm(Price ~ ., data = train_data)

summary(lm_model)

# Make predictions on the test set
pred <- predict(lm_model, newdata = x_test)

# R-squared for the test set
test_r2 <- cor(pred, y_test, use = "complete.obs")^2
cat("Test R-squared (Accuracy):", round(test_r2 * 100, 2), "%\n")

# Mean Absolute Error (MAE)
mae <- mean(abs(y_test - pred), na.rm = TRUE)
cat("Mean Absolute Error (MAE):", round(mae, 2), "\n")

# Mean Squared Error (MSE)
mse <- mean((y_test - pred)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE):", round(mse, 2), "\n")

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", round(rmse, 2), "\n")

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((y_test - pred) / y_test), na.rm = TRUE) * 100
cat("Mean Absolute Percentage Error (MAPE):", round(mape, 2), "%\n")


# Create a data frame with actual and predicted values
results_df <- data.frame(
  Price = c(y_test, pred),
  Type = rep(c("Actual Price", "Predicted Price"), each = length(y_test))
)

# Plot the density plot for actual vs predicted values
ggplot(results_df, aes(x = Price, color = Type, fill = Type)) +
  geom_density(alpha = 0.3, size = 1) +  # Density plot with transparency
  scale_color_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  scale_fill_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  labs(title = "Density Plot: Actual vs Predicted Price",
       x = "Price",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top")


# R² (R-squared): Measures the proportion of the variance explained by the model (higher is better).
# MAE: Measures the average magnitude of errors (lower is better).
# MSE: Penalizes larger errors more significantly (lower is better).
# RMSE: Provides a metric in the same units as the target variable (lower is better).
# MAPE: Provides the error as a percentage, useful for comparing across different scales (lower is better).








# Lasso Regression

x_train_matrix <- as.matrix(x_train)
x_test_matrix <- as.matrix(x_test)
y_train_vector <- as.vector(y_train)
y_test_vector <- as.vector(y_test)

# Replace missing values with the column mean
for (i in 1:ncol(x_train_matrix)) {
  x_train_matrix[is.na(x_train_matrix[, i]), i] <- mean(x_train_matrix[, i], na.rm = TRUE)
}

# For the test matrix
for (i in 1:ncol(x_test_matrix)) {
  x_test_matrix[is.na(x_test_matrix[, i]), i] <- mean(x_test_matrix[, i], na.rm = TRUE)
}

# Fit Lasso model (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(x_train_matrix, y_train_vector, alpha = 1, nfolds = 10)

# Optimal lambda
lambda_lasso <- lasso_model$lambda.min
cat("Optimal Lambda for Lasso:", lambda_lasso, "\n")

# Make predictions on the test set
lasso_pred <- predict(lasso_model, s = lambda_lasso, newx = x_test_matrix)

# Calculate R², MAE, MSE, RMSE, MAPE for Lasso
lasso_r2 <- cor(lasso_pred, y_test_vector)^2
lasso_mae <- mean(abs(y_test_vector - lasso_pred))
lasso_mse <- mean((y_test_vector - lasso_pred)^2)
lasso_rmse <- sqrt(lasso_mse)
lasso_mape <- mean(abs((y_test_vector - lasso_pred) / y_test_vector)) * 100

cat("Lasso R-squared:", round(lasso_r2, 2)*100, "\n")
cat("Lasso MAE:", round(lasso_mae, 2), "\n")
cat("Lasso MSE:", round(lasso_mse, 2), "\n")
cat("Lasso RMSE:", round(lasso_rmse, 2), "\n")
cat("Lasso MAPE:", round(lasso_mape, 2), "%\n")

# Create a data frame with actual and predicted values
results_df <- data.frame(
  Price = c(y_test_vector, lasso_pred),
  Type = rep(c("Actual Price", "Predicted Price"), each = length(y_test))
)

# Plot the density plot for actual vs predicted values
ggplot(results_df, aes(x = Price, color = Type, fill = Type)) +
  geom_density(alpha = 0.3, size = 1) +  # Density plot with transparency
  scale_color_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  scale_fill_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  labs(title = "Density Plot: Actual vs Predicted Price",
       x = "Price",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top")






#Ridge Regression
x_train_matrix <- as.matrix(x_train)
x_test_matrix <- as.matrix(x_test)
y_train_vector <- as.vector(y_train)
y_test_vector <- as.vector(y_test)

# Replace missing values with the column mean
for (i in 1:ncol(x_train_matrix)) {
  x_train_matrix[is.na(x_train_matrix[, i]), i] <- mean(x_train_matrix[, i], na.rm = TRUE)
}

# For the test matrix
for (i in 1:ncol(x_test_matrix)) {
  x_test_matrix[is.na(x_test_matrix[, i]), i] <- mean(x_test_matrix[, i], na.rm = TRUE)
}
# Fit Ridge model (alpha = 0 for Ridge)
ridge_model <- cv.glmnet(x_train_matrix, y_train_vector, alpha = 0, nfolds = 20)

# Optimal lambda
lambda_ridge <- ridge_model$lambda.min
cat("Optimal Lambda for Ridge:", lambda_ridge, "\n")

# Make predictions on the test set
ridge_pred <- predict(ridge_model, s = lambda_ridge, newx = x_test_matrix)

# Calculate R², MAE, MSE, RMSE, MAPE for Ridge
ridge_r2 <- cor(ridge_pred, y_test_vector)^2
ridge_mae <- mean(abs(y_test_vector - ridge_pred))
ridge_mse <- mean((y_test_vector - ridge_pred)^2)
ridge_rmse <- sqrt(ridge_mse)
ridge_mape <- mean(abs((y_test_vector - ridge_pred) / y_test_vector)) * 100

cat("Ridge R-squared:", round(ridge_r2, 2)*100, "\n")
cat("Ridge MAE:", round(ridge_mae, 2), "\n")
cat("Ridge MSE:", round(ridge_mse, 2), "\n")
cat("Ridge RMSE:", round(ridge_rmse, 2), "\n")
cat("Ridge MAPE:", round(ridge_mape, 2), "%\n")

# Create a data frame with actual and predicted values
results_df <- data.frame(
  Price = c(y_test_vector, ridge_pred),
  Type = rep(c("Actual Price", "Predicted Price"), each = length(y_test))
)

# Plot the density plot for actual vs predicted values
ggplot(results_df, aes(x = Price, color = Type, fill = Type)) +
  geom_density(alpha = 0.3, size = 1) +  # Density plot with transparency
  scale_color_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  scale_fill_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  labs(title = "Density Plot: Actual vs Predicted Price",
       x = "Price",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top")



# Coefficients for Lasso
lasso_coeff <- coef(lasso_model, s = lambda_lasso)
print("Lasso Coefficients:")
print(lasso_coeff)

# Coefficients for Ridge
ridge_coeff <- coef(ridge_model, s = lambda_ridge)
print("Ridge Coefficients:")
print(ridge_coeff)








# Adding Polynomial Features


windows(width = 1080, height = 720)  # For Windows

# Combine x_train and y_train to create a training data frame
train_data <- cbind(x_train, Price = y_train)

# Add polynomial features for numerical columns
train_data$Length_squared <- train_data$Length^2
train_data$Width_squared <- train_data$Width^2
train_data$Depth_squared <- train_data$Depth^2

#add interaction terms
train_data$Length_Width <- train_data$Length * train_data$Width
train_data$Width_Depth <- train_data$Width * train_data$Depth
train_data$Length_Depth <- train_data$Length * train_data$Depth

# Refit the linear regression model with polynomial features
lm_model_poly <- lm(Price ~ . + Length_squared + Width_squared + Depth_squared + Length_Width + Width_Depth + Length_Depth, data = train_data)

# View the model summary
summary(lm_model_poly)

# Add polynomial features to the test set
x_test$Length_squared <- x_test$Length^2
x_test$Width_squared <- x_test$Width^2
x_test$Depth_squared <- x_test$Depth^2
x_test$Length_Width <- x_test$Length * x_test$Width
x_test$Width_Depth <- x_test$Width * x_test$Depth
x_test$Length_Depth <- x_test$Length * x_test$Depth

# Make predictions
pred_poly <- predict(lm_model_poly, newdata = x_test)

# R-squared for the test set
test_r2_poly <- cor(pred_poly, y_test, use = "complete.obs")^2
cat("Test R-squared (Accuracy) with Polynomial Features:", round(test_r2_poly * 100, 2), "%\n")

# Mean Absolute Error (MAE)
mae_poly <- mean(abs(y_test - pred_poly), na.rm = TRUE)
cat("Mean Absolute Error (MAE):", round(mae_poly, 2), "\n")

# Mean Squared Error (MSE)
mse_poly <- mean((y_test - pred_poly)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE):", round(mse_poly, 2), "\n")

# Root Mean Squared Error (RMSE)
rmse_poly <- sqrt(mse_poly)
cat("Root Mean Squared Error (RMSE):", round(rmse_poly, 2), "\n")

# Mean Absolute Percentage Error (MAPE)
mape_poly <- mean(abs((y_test - pred_poly) / y_test), na.rm = TRUE) * 100
cat("Mean Absolute Percentage Error (MAPE):", round(mape_poly, 2), "%\n")


# Create a data frame with actual and predicted values for the polynomial model
results_df_poly <- data.frame(
  Price = c(y_test, pred_poly),
  Type = rep(c("Actual Price", "Predicted Price (Polynomial)"), each = length(y_test))
)

# Plot the density plot
ggplot(results_df_poly, aes(x = Price, color = Type, fill = Type)) +
  geom_density(alpha = 0.3, size = 1) +
  scale_color_manual(values = c("Actual Price" = "orange", "Predicted Price (Polynomial)" = "blue")) +
  scale_fill_manual(values = c("Actual Price" = "orange", "Predicted Price (Polynomial)" = "blue")) +
  labs(title = "Density Plot: Actual vs Predicted Price (Polynomial Features)",
       x = "Price",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top")





# Adding more Degree

windows(width = 1080, height = 720)  # For Windows

# Combine x_train and y_train to create a training data frame
train_data <- cbind(x_train, Price = y_train)

# Add polynomial features for numerical columns
train_data$Length_squared <- train_data$Length^2
train_data$Width_squared <- train_data$Width^2
train_data$Depth_squared <- train_data$Depth^2
train_data$Length_cube <- train_data$Length^3
train_data$Width_cube <- train_data$Width^3
train_data$Depth_cube <- train_data$Depth^3

#add interaction terms
train_data$Length_Width <- train_data$Length * train_data$Width
train_data$Width_Depth <- train_data$Width * train_data$Depth
train_data$Length_Depth <- train_data$Length * train_data$Depth

# Refit the linear regression model with polynomial features
lm_model_poly <- lm(Price ~ . + Length_squared + Width_squared + Depth_squared + Length_Width + Width_Depth + Length_Depth +Length_cube+Width_cube+Depth_cube, data = train_data)

# View the model summary
summary(lm_model_poly)

# Add polynomial features to the test set
x_test$Length_squared <- x_test$Length^2
x_test$Width_squared <- x_test$Width^2
x_test$Depth_squared <- x_test$Depth^2
x_test$Length_cube <- x_test$Length^3
x_test$Width_cube <- x_test$Width^3
x_test$Depth_cube <- x_test$Depth^3
x_test$Length_Width <- x_test$Length * x_test$Width
x_test$Width_Depth <- x_test$Width * x_test$Depth
x_test$Length_Depth <- x_test$Length * x_test$Depth

# Make predictions
pred_poly <- predict(lm_model_poly, newdata = x_test)

# R-squared for the test set
test_r2_poly <- cor(pred_poly, y_test, use = "complete.obs")^2
cat("Test R-squared (Accuracy) with Polynomial Features:", round(test_r2_poly * 100, 2), "%\n")

# Mean Absolute Error (MAE)
mae_poly <- mean(abs(y_test - pred_poly), na.rm = TRUE)
cat("Mean Absolute Error (MAE):", round(mae_poly, 2), "\n")

# Mean Squared Error (MSE)
mse_poly <- mean((y_test - pred_poly)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE):", round(mse_poly, 2), "\n")

# Root Mean Squared Error (RMSE)
rmse_poly <- sqrt(mse_poly)
cat("Root Mean Squared Error (RMSE):", round(rmse_poly, 2), "\n")

# Mean Absolute Percentage Error (MAPE)
mape_poly <- mean(abs((y_test - pred_poly) / y_test), na.rm = TRUE) * 100
cat("Mean Absolute Percentage Error (MAPE):", round(mape_poly, 2), "%\n")

# Create a data frame with actual and predicted values for the polynomial model
results_df_poly <- data.frame(
  Price = c(y_test, pred_poly),
  Type = rep(c("Actual Price", "Predicted Price (Polynomial)"), each = length(y_test))
)

# Plot the density plot
ggplot(results_df_poly, aes(x = Price, color = Type, fill = Type)) +
  geom_density(alpha = 0.3, size = 1) +
  scale_color_manual(values = c("Actual Price" = "orange", "Predicted Price (Polynomial)" = "blue")) +
  scale_fill_manual(values = c("Actual Price" = "orange", "Predicted Price (Polynomial)" = "blue")) +
  labs(title = "Density Plot: Actual vs Predicted Price (Polynomial Features)",
       x = "Price",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top")




# Degree = 4

windows(width = 1080, height = 720)  # For Windows

# Combine x_train and y_train to create a training data frame
train_data <- cbind(x_train, Price = y_train)

# Add polynomial features for numerical columns
train_data$Length_squared <- train_data$Length^2
train_data$Width_squared <- train_data$Width^2
train_data$Depth_squared <- train_data$Depth^2
train_data$Length_cube <- train_data$Length^3
train_data$Width_cube <- train_data$Width^3
train_data$Depth_cube <- train_data$Depth^3
train_data$Length_f <- train_data$Length^4
train_data$Width_f <- train_data$Width^4
train_data$Depth_f <- train_data$Depth^4

#add interaction terms
train_data$Length_Width <- train_data$Length * train_data$Width
train_data$Width_Depth <- train_data$Width * train_data$Depth
train_data$Length_Depth <- train_data$Length * train_data$Depth
train_data$Length_Depth_Width <- train_data$Length * train_data$Depth * train_data$Width
# Refit the linear regression model with polynomial features
lm_model_poly <- lm(Price ~ . + Length_squared + Width_squared + Depth_squared + Length_Width + Width_Depth + Length_Depth +Length_cube+Width_cube+Depth_cube+Length_f+Width_f+Depth_f+Length_Depth_Width, data = train_data)

# View the model summary
summary(lm_model_poly)

# Add polynomial features to the test set
x_test$Length_squared <- x_test$Length^2
x_test$Width_squared <- x_test$Width^2
x_test$Depth_squared <- x_test$Depth^2
x_test$Length_cube <- x_test$Length^3
x_test$Width_cube <- x_test$Width^3
x_test$Depth_cube <- x_test$Depth^3
x_test$Length_f <- x_test$Length^4
x_test$Width_f <- x_test$Width^4
x_test$Depth_f <- x_test$Depth^4
x_test$Length_Width <- x_test$Length * x_test$Width
x_test$Width_Depth <- x_test$Width * x_test$Depth
x_test$Length_Depth <- x_test$Length * x_test$Depth
x_test$Length_Depth_Width <- x_test$Length * x_test$Depth * x_test$Width

# Make predictions
pred_poly <- predict(lm_model_poly, newdata = x_test)

# R-squared for the test set
test_r2_poly <- cor(pred_poly, y_test, use = "complete.obs")^2
cat("Test R-squared (Accuracy) with Polynomial Features:", round(test_r2_poly * 100, 2), "%\n")

# Mean Absolute Error (MAE)
mae_poly <- mean(abs(y_test - pred_poly), na.rm = TRUE)
cat("Mean Absolute Error (MAE):", round(mae_poly, 2), "\n")

# Mean Squared Error (MSE)
mse_poly <- mean((y_test - pred_poly)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE):", round(mse_poly, 2), "\n")

# Root Mean Squared Error (RMSE)
rmse_poly <- sqrt(mse_poly)
cat("Root Mean Squared Error (RMSE):", round(rmse_poly, 2), "\n")

# Mean Absolute Percentage Error (MAPE)
mape_poly <- mean(abs((y_test - pred_poly) / y_test), na.rm = TRUE) * 100
cat("Mean Absolute Percentage Error (MAPE):", round(mape_poly, 2), "%\n")


# Create a data frame with actual and predicted values for the polynomial model
results_df_poly <- data.frame(
  Price = c(y_test, pred_poly),
  Type = rep(c("Actual Price", "Predicted Price (Polynomial)"), each = length(y_test))
)

# Plot the density plot
ggplot(results_df_poly, aes(x = Price, color = Type, fill = Type)) +
  geom_density(alpha = 0.3, size = 1) +
  scale_color_manual(values = c("Actual Price" = "orange", "Predicted Price (Polynomial)" = "blue")) +
  scale_fill_manual(values = c("Actual Price" = "orange", "Predicted Price (Polynomial)" = "blue")) +
  labs(title = "Density Plot: Actual vs Predicted Price (Polynomial Features)",
       x = "Price",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top")




# Decision Tree

# library (rpart)

windows(width = 1080, height = 720)  # For Windows

# Combine x_train and y_train to create a training data frame
train_data <- cbind(x_train, Price = y_train)

# Fit the Decision Tree model
dt_model <- rpart(Price ~ ., data = train_data, method = "anova")

# Print the model summary
print(summary(dt_model))

# Make predictions on the test set
dt_pred <- predict(dt_model, newdata = x_test)

# R-squared for the test set
dt_r2 <- cor(dt_pred, y_test, use = "complete.obs")^2
cat("Decision Tree R-squared (Accuracy):", round(dt_r2 * 100, 2), "%\n")

# Mean Absolute Error (MAE)
dt_mae <- mean(abs(y_test - dt_pred), na.rm = TRUE)
cat("Mean Absolute Error (MAE):", round(dt_mae, 2), "\n")

# Mean Squared Error (MSE)
dt_mse <- mean((y_test - dt_pred)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE):", round(dt_mse, 2), "\n")

# Root Mean Squared Error (RMSE)
dt_rmse <- sqrt(dt_mse)
cat("Root Mean Squared Error (RMSE):", round(dt_rmse, 2), "\n")

# Mean Absolute Percentage Error (MAPE)
dt_mape <- mean(abs((y_test - dt_pred) / y_test), na.rm = TRUE) * 100
cat("Mean Absolute Percentage Error (MAPE):", round(dt_mape, 2), "%\n")

windows(width = 1080, height = 720)  # For Windows
# Plot the Decision Tree
plot(dt_model, uniform = TRUE, main = "Decision Tree for Price Prediction")
text(dt_model, use.n = TRUE, all = TRUE, cex = 0.8)

# Plot the Decision Tree with a proper hierarchy
rpart.plot(dt_model, type = 2, extra = 101, fallen.leaves = TRUE, 
           main = "Decision Tree for Price Prediction")

# Create a data frame with actual and predicted values
results_df <- data.frame(
  Price = c(y_test, dt_pred),
  Type = rep(c("Actual Price", "Predicted Price"), each = length(y_test))
)

# Plot the density plot for actual vs predicted values
ggplot(results_df, aes(x = Price, color = Type, fill = Type)) +
  geom_density(alpha = 0.3, size = 1) +
  scale_color_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  scale_fill_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  labs(title = "Decision Tree: Actual vs Predicted Price",
       x = "Price",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top")




# Random Forest




windows(width = 1080, height = 720)  # For Windows
# Fit the Random Forest model
train_data <- cbind(x_train, Price = y_train)
train_data <- as.data.frame(lapply(train_data, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)  # Impute numeric columns with mean
  }
  x  # Return the column
}))
set.seed(42)  # For reproducibility
rf_model <- randomForest(Price ~ ., data = train_data, ntree = 100, mtry = 2, importance = TRUE)

# Print the model summary
print(rf_model)

# Make predictions on the test set
rf_pred <- predict(rf_model, newdata = x_test)

# R-squared for the test set
rf_r2 <- cor(rf_pred, y_test, use = "complete.obs")^2
cat("Random Forest R-squared (Accuracy):", round(rf_r2 * 100, 2), "%\n")

# Mean Absolute Error (MAE)
rf_mae <- mean(abs(y_test - rf_pred), na.rm = TRUE)
cat("Mean Absolute Error (MAE):", round(rf_mae, 2), "\n")

# Mean Squared Error (MSE)
rf_mse <- mean((y_test - rf_pred)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE):", round(rf_mse, 2), "\n")

# Root Mean Squared Error (RMSE)
rf_rmse <- sqrt(rf_mse)
cat("Root Mean Squared Error (RMSE):", round(rf_rmse, 2), "\n")

# Mean Absolute Percentage Error (MAPE)
rf_mape <- mean(abs((y_test - rf_pred) / y_test), na.rm = TRUE) * 100
cat("Mean Absolute Percentage Error (MAPE):", round(rf_mape, 2), "%\n")

# View feature importance
importance(rf_model)

# Plot feature importance
varImpPlot(rf_model, main = "Feature Importance")

# Convert a Random Forest tree into a Decision Tree format
single_tree <- randomForest::getTree(rf_model, k = 1, labelVar = TRUE)

# Plot the tree (manual conversion may be required)
rpart.plot(single_tree)

windows(width = 1080, height = 720)  # For Windows
# Create a data frame with actual and predicted values
results_df <- data.frame(
  Price = c(y_test, rf_pred),
  Type = rep(c("Actual Price", "Predicted Price"), each = length(y_test))
)

# Plot the density plot for actual vs predicted values
ggplot(results_df, aes(x = Price, color = Type, fill = Type)) +
  geom_density(alpha = 0.3, size = 1) +
  scale_color_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  scale_fill_manual(values = c("Actual Price" = "orange", "Predicted Price" = "green")) +
  labs(title = "Random Forest: Actual vs Predicted Price",
       x = "Price",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top")


