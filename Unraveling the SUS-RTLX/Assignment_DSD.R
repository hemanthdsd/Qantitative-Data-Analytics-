setwd("E:/New folder/IS40730-Quantitative Data Analysis/Assignment 1") #setting our working directory

##             ---------- Loading the Data set-----------


#lets load the given data into the environment. This results in view of sus-rtlx in the global environment
sus_rtlx=read.csv("SusRtlx.csv") 

View(sus_rtlx)  #lets open the provided information as a data frame in a new tab

sus_rtlx #we can always print the data to view in the console if we want(not necessary)

# we can always check out the head and tail(to check whether the data is complete) instead of viewing all the values in the console using (sus_rtlx)

head(sus_rtlx)
tail(sus_rtlx)

sus_rtlx$ID = NULL# we will get rid of the column ID(since we have already have indexing)
head(sus_rtlx)
tail(sus_rtlx)

##             ------------- Data Cleaning---------------

# Check if all SUS scores are between 0 and 100
all(sus_rtlx$SUS.Score >= 0 & sus_rtlx$SUS.Score <= 100)
# since, the result was false, we know that , there are some values which are not in ideal range(between 0 and 100) for SUS values

# lets find them
# Print rows with SUS scores outside the range
print(sus_rtlx[!(sus_rtlx$SUS.Score >= 0 & sus_rtlx$SUS.Score <= 100), ])

# so we need to remove these values which are out of range 
# I will consider transforming the out-of-range scores to bring them within the valid range. transform values below 0 to 0 and values above 100 to 100, effectively bringing them within the valid range.
sus_rtlx$SUS.Score[sus_rtlx$SUS.Score < 0] = 0
sus_rtlx$SUS.Score[sus_rtlx$SUS.Score > 100] = 100

# now , we will check again , if all SUS scores are between 0 and 100
all(sus_rtlx$SUS.Score >= 0 & sus_rtlx$SUS.Score <= 100)# since the value is true, we can consider our SUS values are in ideal range

# Print rows 88 and 92
print(sus_rtlx[c(88, 92), ])# we can check whether the SUS score in rows 88 & 92  are transformed or not


# Similarly, let's check for the RTLX score
# Check if all RTLX scores are between 0 and 126
all(sus_rtlx$RTLX.Score >= 0 & sus_rtlx$RTLX.Score <= 126)
#since the value is true, we can consider our RTLX values are in ideal range


head(sus_rtlx)
tail(sus_rtlx)

##        ------------------ Descriptive Statistics--------------------

# descriptive statistics for SUS (System usability scale) scores
mean (sus_rtlx$SUS.Score)                # Calculate the mean of "SUS.Score
sd(sus_rtlx$SUS.Score)                   # Calculate the standard deviation of "SUS.Score"
max (sus_rtlx$SUS.Score)                 # Find the maximum value in "SUS.Score"
min (sus_rtlx$SUS.Score)                 # Find the minimum value in "SUS.Score"
median (sus_rtlx$SUS.Score)              # Calculate the median of "SUS.Score"
IQR (sus_rtlx$SUS.Score)                 # Calculate the interquartile range (IQR) of "SUS.Score"

# descriptive statistics for RTLX scores
mean (sus_rtlx$RTLX.Score)               # Calculate the mean of "RTLX.Score"
sd(sus_rtlx$RTLX.Score)                  # Calculate the standard deviation of "RTLX.Score"
max (sus_rtlx$RTLX.Score)                # Find the maximum value in "RTLX.Score"
min (sus_rtlx$RTLX.Score)                # Find the minimum value in "RTLX.Score"
median (sus_rtlx$RTLX.Score)             # Calculate the median of "RTLX.Score"
IQR (sus_rtlx$RTLX.Score)                # Calculate the interquartile range (IQR) of "RTLX.Score"

# Summary of SUS scores
summary(sus_rtlx$SUS.Score)

# Summary of RTLX scores
summary(sus_rtlx$RTLX.Score)


##   --------------------- Data Visualization-------------------------


# Histogram for SUS Score with specified breaks (0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
hist(sus_rtlx$SUS.Score, main = "Histogram of SUS Scores",
     xlab = "SUS Score", # x-axis label
     col = "skyblue",  # Bar color
     border = "black",  # Border color
     xlim = c(0, 100),  # Set x-axis limits
     ylim = c(0, 20),   # Set y-axis limits
     ylab = "Frequency", # y-axis label
     las = 1,  # Make y-axis labels horizontal
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))


# Histogram for RTLX Score with specified breaks (0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
hist(sus_rtlx$RTLX.Score, main = "Histogram of RTLX Scores",
     xlab = "RTLX Score", # x-axis label
     col = "lightgreen",  # Bar color
     border = "black",  # Border color
     xlim = c(0, 100),  # Set x-axis limits
     ylim = c(0, 40),   # Set y-axis limits
     ylab = "Frequency", # y-axis label
     las = 1,  # Make y-axis labels horizontal
     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))


# let's Create a Boxplot for SUS.Score 
boxplot(sus_rtlx$SUS.Score, 
        main="Boxplot of SUS Score",# title
        ylab="SUS Score",           # y-axis label
        col="lightblue",           # Change the box color
        border="darkblue")         # Change the border color
        

# Boxplot for RTLX.Score 
boxplot(sus_rtlx$RTLX.Score, 
        main="Boxplot of RTLX Score",#title
        ylab="RTLX Score",          # y-axis label
        col="lightgreen",          # Change the box color
        border="darkgreen")        # Change the border color

#  let's create a scatterplot of SUS.Score vs. RTLX.Score
plot(sus_rtlx$SUS.Score, sus_rtlx$RTLX.Score, 
     main="Scatterplot of SUS Score vs. RTLX Score",# title
     xlab="SUS Score",        # x-axis label
     ylab="RTLX Score",       # y-axis label
     pch=19,                  # Set the point character
     col="blue",# Set the point color
     xlim=c(0, 100),          # Set x-axis limits
     ylim=c(0, 75))           # Set y-axis limits
        


abline(lm(sus_rtlx$RTLX.Score ~ sus_rtlx$SUS.Score), col="red") # Add a regression line (Optional)


##  ------------------------  Statistical Analysis------------------------

#correlation test
cor.test(sus_rtlx$SUS.Score,sus_rtlx$RTLX.Score)  


#  Pearson's product-moment correlation
# 
# data:  sus_rtlx$SUS.Score and sus_rtlx$RTLX.Score
# t = 9.4335, df = 98, p-value = 2.054e-15
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5707893 0.7805206
# sample estimates:
#       cor 
# 0.6898644 

##Based on the analysis, there is a statistically significant positive correlation between system usability (SUS) and perceived workload (RTLX).
##This suggests that, in the context of using voice user interfaces, higher usability scores are associated with higher perceived workload.
##The very low p-value (2.054e-15) provides strong evidence against the null hypothesis, indicating that this correlation is not due to random chance.
##The 95% confidence interval suggests that the true correlation is likely to be between 0.571 and 0.781.








