
# *********************************************
# DAMI Preprocessing Exercise R file
# Complete the codes to complete the assignment
# *********************************************

# 1. Import data for analysis to R environment
# Downloaded "Adult" dataset from UCI Machine Learning Repository
# URL http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
# Import dataset in adult_db
# Missing values are represented as "?" in data file, make sure that R read them as missing values (NAs)
# ------------------------------------------------------------------------------------------------------ #
# use read.table(), type ?read.table for help
adult_db <- read.table(file="C:\\Users\\Louanès\\Documents\\Homework1\\adult.data",header=FALSE,sep=",",na.strings="?",strip.white = TRUE)

  
  
  
  # Assign attribute names (column names) to the data we just imported
  # Attribute names are in separate file "adult.names", scroll down to the bottom of this file
  # Attribute names such as ("age", "workclass", "fnlwgt",...)
  # Last column of the dataset adult.db with values ">50K" and "<=50K" does not have name, 
  # this is the class attribute, so we just name it as "class"
  
colnames(adult_db) = c("age",
                         "workclass",
                         "fnlwgt",
                         "education",
                         "education_num",
                         "marital_status",
                         "occupation",
                         "relationship",
                         "race",
                         "sex",
                         "capital_gain",
                         "capital_loss",
                         "hours_per_week",
                         "native_country",
                         "class")


# 2. Check for missing values
# Write code to plot missingness and count missing values each attribute has
# Inorder to plot missingness, you need to first install "Amelia" package
# Hint: use "apply" function along columns of "adult.db", for each column (attribute) find how many NAs are there
# --------------------------------------------------------------------------------------------------------------- #

library(Amelia)
# plot missing values in data
missmap(adult_db,legend=TRUE,col = c("red","blue"))
# HINT: use missmap()

# count number of missing values in all attributes
vector_na_per_column<-apply(adult_db,2,function(x) sum(is.na(x))) #vector of missing values in each column
number_of_all_missing_values <- sum(vector_na_per_column) #In order to find the overall missing values in all the dataframe, we have to sum each value of the missing values vector
number_of_all_missing_values # The count of all missing values is stored in this variable

# Delete records (rows) with any missing value
adult_db_nomiss <- na.omit(adult_db)
  
####check if we still have missing values in this new dataframe
apply(adult_db_nomiss,2,function(x) sum(is.na(x)))
  
  
  # 3. We will take only small chunk of the data for our purpose.
  # So, randomly select 1000 records from among 30 thousand records in the dataset.
  # ------------------------------------------------------------------------------- #
set.seed(145)
#take 1000 record instead of 1500 which was written in the code
idx = sample(1:nrow(adult_db_nomiss),1500,replace = TRUE)
adult_db_lim = adult_db_nomiss[idx,]
row.names(adult_db_lim) <- NULL



# 3a. Examine attributes of the dataset
# Plot histogram for numeric attribute "age", with 100 (for <=50K) and 50(for >50K) breaks, 
# show main title and attribute name on the plot.
# --------------------------------------------------------------------------------------------------------
par(mar = c(5,5,3,1))

# HINT: use hist()
library(ggplot2)
#ggplot(adult_db_lim,aes(age,fill=class))+geom_histogram()



hist(adult_db_lim$age[which(adult_db_lim$class == "<=50K")], breaks = 100, 
     xlab = "age", ylab = "frequency", col = "red", main = "Age of adults")

hist(adult_db_lim$age[which(adult_db_lim$class == ">50K")], breaks = 50, 
     xlab = "age", ylab = "frequency", col = "blue", main = "Age of adults",add=T)

legend(x=70, y=40, legend = c(">50K", "<=50K"),
      col=c("blue", "red"), pch = 20, cex = 1)

# ******************************************************* #
#install.packages("ggplot2")
# 3b. Plot barchart for categorical attribute "relationship", 
# show legend, attribute name and main title for the plot.
# ******* YOUR CODE FOR BAR CHART GOES HERE ******* #
# HINT: use barplot()
# ************************************************* #
par_default <- par(no.readonly = T)
height <- table(adult_db_lim$relationship)

barplot(height=height,col=c("black", "red", "green","blue","lightblue", "pink"))


# 3c. Plot a boxplot for attribute "Age" for groups with earning "<=50K", ">50K"
# ------------------------------------------------------------------------------

boxplot(age ~ class, data = adult_db_lim, col = "red", main = "Age of adults")

# HINT: use boxplot()



# 4 Create new data set from our latest dataset with only numeric attributes
# ------------------------------------------------------------------------
adult_db_numeric <- adult_db_lim[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_cat <- adult_db_lim[,c("class")]
#head(adult_db_numeric)
#print(class_cat)

# Standardize numeric attributes in "adult_db_numeric" dataset.
# mean = 0 and sd = 1 for all numeric attributes

###### Standardize the data in order to have similar measured data (no attribute X outweighting attribute Y)
### We do this simply by calling the "scale" function
adult_db_num_std <- scale(adult_db_numeric)

#### Check if mean = 0 and sd = 1 in each column
colMeans(adult_db_num_std)
apply(adult_db_num_std,2,sd)  

head(adult_db_num_std)  
# we can check the mean and standard deviation of the standardized data
apply(adult_db_num_std, 2, mean)
apply(adult_db_num_std, 2, sd)

head(adult_db_num_std)

# 5a. Run Principal Component Analysis (PCA) on the numeric dataset from above "adult_db_num_std"
# plot the first 2 principal components
# ------------------------------------------------------------------------------------------

# ******** YOUR CODE FOR GETTING PRINCIPAL COMPONENTS GOES HERE ******** #
# HINT: use prcomp()
pr.out <- prcomp(adult_db_num_std)
names(pr.out)
head(pr.out)
class_cat
adult_db_pca <- pr.out$x
par(mfrow = c(1,1))
# ******** YOUR CODE TO PLOT FOR FIRST TWO PCs ****** #
# plot(), legend()
plot(adult_db_pca[,1:2], col = ifelse(class_cat =="<=50K" , "red","green"), pch = 20, main = "First two PC")

legend(x=5, y=4, legend = c(">50K", "<=50K"),
       col=c("green", "red"), pch = 20, cex = 0.6)
head(adult_db_pca)
class_cat

# 5b. Plot percentage of the variance explained by principal components
# ----------------------------------------------------------------------------
# write a code to show proportion of variance explained by each Principal Component
# Standard deviation are stored as "sdev"

pr.var <- (pr.out$sdev)^2
pve <- pr.var/sum(pr.var)
head(pr.var)
head(pve)

par(mfrow = c(1,2), oma = c(1,1,2,1))
plot(pve, xlab = "Principal Components", ylab = "Variance", type = "b", ylim = c(0,1),col="red")
plot(cumsum(pve), xlab = "Principal Components", ylab = "Cumulative variance", type = "b", ylim = c(0,1),col="red")
mtext("Proportion of Variance explained", outer = TRUE)
par(mfrow = c(1,1))




# 5c. write answer for this as a comment using #
# ------------------------------------------------------------------------------
# How many principal components are needed to explain 50% and 90% of variance respectively
# Answer: 3 PC's are needed to explain 50% of variance and 6 PC's are needed to explain 90% of variance


