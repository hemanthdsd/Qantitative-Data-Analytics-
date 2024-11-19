setwd("E:/Assignments/IS40730-Quantitative Data Analysis/Assignment 2") #setting our working directory


##             ---------- Loading the Data set-----------


#lets load the given data into the environment. This results in view of tetris in the global environment

tetris=read.csv("tetris.csv")

View(tetris)  #lets open the provided information as a data frame in a new tab

tetris #we can always print the data to view in the console if we want(not necessary)

tetris$ID = NULL# we will get rid of the column ID(since we have already have indexing)


# we can always check out the head and tail(to check whether the data is complete) instead of viewing all the values in the console using (tetris)

head(tetris)
tail(tetris)

##    ------------------Data Cleaning and Preparation------------------


#let us check if all the score are in range i.e., in between  0 (lowest possible score) to 100 (highest possible score). 

all(tetris$score >= 0 & tetris$score <= 100)  # If this is true, the scores are in the ideal range


#since, we have data corresponding to people played tetris in two different conditions,
#we will create different corresponding subsets for better understanding

aud_dis_score = subset(tetris, condition == "auditory") # This subset contains the scores of the people played tetris with auditory distractions
aud_dis_score # we can view that in console to check the data

length(aud_dis_score$condition) # we can see that 32 members played tetris with auditory distraction

vis_dis_score = subset(tetris, condition == "visual") # This subset contains the scores of the people played tetris with visual distractions
vis_dis_score # we can view that in console to check the data

length(vis_dis_score$condition) # we can see that 32 members played tetris with visual distraction( total 64 member participated)

# Box plots helps to  visually identify any potential outliers in the data
boxplot(aud_dis_score$score,  
        main = "Auditory Disruption Scores",
        ylab = "Score",
        col = "skyblue",            # Set box plot color
        border = "darkblue",        # Set border color
        las=1)                      # Make y-axis labels horizontal 


boxplot(vis_dis_score$score,
        main = "Visual Disruption Scores",
        ylab = "Score",
        col = "lightgreen",         # Set box plot color
        border = "darkgreen",       # Set border color
        las=1)                      # Make y-axis labels horizontal 


# since, all the score are in ideal range and no outliers found we can proceed further into descriptive statistics


##        ------------------ Descriptive Statistics--------------------


# Descriptive statistics for 'auditory' subset
mean(aud_dis_score$score)       # Calculate the mean of scores in the 'auditory' subset
sd(aud_dis_score$score)         # Calculate the standard deviation of scores in the 'auditory' subset
max(aud_dis_score$score)        # Find the maximum value in the 'auditory' subset
min(aud_dis_score$score)        # Find the minimum value in the 'auditory' subset
median(aud_dis_score$score)     # Calculate the median of scores in the 'auditory' subset
IQR(aud_dis_score$score)        # Calculate the interquartile range (IQR) of scores in the 'auditory' subset


# Descriptive statistics for 'visual' subset
mean(vis_dis_score$score)       # Calculate the mean of scores in the 'visual' subset
sd(vis_dis_score$score)         # Calculate the standard deviation of scores in the 'visual' subset
max(vis_dis_score$score)        # Find the maximum value in the 'visual' subset
min(vis_dis_score$score)        # Find the minimum value in the 'visual' subset
median(vis_dis_score$score)     # Calculate the median of scores in the 'visual' subset
IQR(vis_dis_score$score)        # Calculate the interquartile range (IQR) of scores in the 'visual' subset


# Summary of 'score' in 'auditory' subset
summary(aud_dis_score$score)


# Summary of 'score' in 'visual' subset
summary(vis_dis_score$score)


##   --------------------- Data Visualization-------------------------


# Histogram for 'score' in 'auditory' subset with specified breaks

hist(
  aud_dis_score$score,        # Data: 'score' column from 'auditory' subset
  main = "Histogram of Auditory Disruption Scores",  # Title of the histogram
  xlab = "Score",             # Label for the x-axis
  ylab = "Frequency",         # Label for the y-axis
  col = "skyblue",            # Bar color
  border = "black",           # Border color
  xlim = c(0, 100),           # Set x-axis limits
  ylim = c(0, 15),            # Set y-axis limits
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),  # Specified breaks for histogram bins
  las = 1                     # Make y-axis labels horizontal
)

# Histogram for 'score' in 'visual' subset with specified breaks

hist(
  vis_dis_score$score,        # Data: 'score' column from 'visual' subset
  main = "Histogram of Visual Disruption Scores",  # Title of the histogram
  xlab = "Score",             # Label for the x-axis
  ylab = "Frequency",         # Label for the y-axis
  col = "lightgreen",         # Bar color (changed to light green)
  border = "darkgreen",       # Border color (similar to the visual box plot)
  xlim = c(0, 100),           # Set x-axis limits
  ylim = c(0, 15),            # Set y-axis limits
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),  # Specified breaks for histogram bins
  las = 1                     # Make y-axis labels horizontal
)

library(ggplot2)           # Load the ggplot2 package for data visualization

# Create a combined data frame for both subsets
tetris$condition= factor(tetris$condition, levels = c("auditory", "visual"))

# Plotting histograms with overlay and overlapping area highlighted

ggplot(tetris, aes(x = score, fill = condition)) +                                          # Begin creating a ggplot with 'score' on the x-axis and fill by condition
  geom_histogram(binwidth = 10, alpha = 0.5, color = "black", position = "identity") +      # Add the histogram layer
  geom_vline(aes(xintercept = min(vis_dis_score$score), color = "Overlap"), linetype = "dashed") +  # Add a vertical line for overlap indication
  labs(title = "Overlay Histogram of Scores by Condition", x = "Score", y = "Frequency") +  # Label titles and axes
  scale_color_manual(values = c("black", "red"), guide = FALSE) +                           # Set color scales manually
  theme_minimal() +  # Set a minimal theme
  theme(  # Further customize the plot's appearance
    plot.title = element_text(face = "bold", size = 16),            # Title text properties
    axis.title = element_text(face = "bold", size = 12),            # Axis title text properties
    axis.text = element_text(size = 10),  # Axis text size
    panel.background = element_rect(fill = "white")                 # Background color
  )

#  ----------------xx--------------


#The independent t-test was considered appropriate because we had two distinct groups (auditory and visual), each experiencing a different condition, allowing us to compare their means reliably.


## ----------------Assumptions Check for Independent t-test-----------------

# let us check the assumptions for the above test

# Shapiro-Wilk test for normality
shapiro.test(aud_dis_score$score)
shapiro.test(vis_dis_score$score)  #Based on these results, both subsets exhibit no significant departure from normality, supporting the assumption of normality for further analysis using a t-test.

# Install the 'car' package for Levene's test
install.packages("car")  

# Load the 'car' package to use the Levene's test function
library(car)  

# Perform Levene's test to check if variance of 'score' differs between 'auditory' and 'visual' groups
# If p > 0.05, assumption of equal variances for t-test is reasonably met
# Output 'levene_result' contains the test statistics and p-value
leveneTest(score ~ condition, data = tetris) 


#we know that data, that is scores are independent
#data is interval scale

##  ------------------------  Statistical Analysis------------------------

#            -------------Performing Independent T-Test-------------

# Perform independent t-test to compare means between 'auditory' and 'visual' groups
t_test_result <- t.test(score ~ condition, data = tetris)

# Display the t-test result
t_test_result

# Interpretation:
# The t-test yielded a significant result (p < 0.001), indicating a substantial difference
# between the mean scores of participants in the 'auditory' and 'visual' conditions.
# The 95% confidence interval (15.60 to 27.03) for the difference in means does not include zero,
# further supporting the conclusion of a significant difference between the two groups.

## -------------------Visualization: Comparative Mean Scores by Condition--------------


# Calculate mean and standard deviation for 'auditory' and 'visual' subsets
aud_mean = mean(tetris$score[tetris$condition == "auditory"])
vis_mean = mean(tetris$score[tetris$condition == "visual"])

aud_sd = sd(tetris$score[tetris$condition == "auditory"])
vis_sd = sd(tetris$score[tetris$condition == "visual"])

aud_sd
vis_sd

# Calculate the standard error
n_aud = sum(tetris$condition == "auditory")
n_vis = sum(tetris$condition == "visual")

se_aud = aud_sd / sqrt(n_aud)
se_vis = vis_sd / sqrt(n_vis)

# Set the means, SDs, and standard errors in vectors
cycle.mean = c(aud_mean, vis_mean)
cycle.sd = c(aud_sd, vis_sd)
se = c(se_aud, se_vis)

# Create a barplot
# Calculate mean and standard deviation for 'auditory' and 'visual' subsets
aud_mean = mean(tetris$score[tetris$condition == "auditory"])
vis_mean = mean(tetris$score[tetris$condition == "visual"])

aud_sd = sd(tetris$score[tetris$condition == "auditory"])
vis_sd = sd(tetris$score[tetris$condition == "visual"])

# Calculate the standard error
n_aud = sum(tetris$condition == "auditory")
n_vis = sum(tetris$condition == "visual")

se_aud = aud_sd / sqrt(n_aud)
se_vis = vis_sd / sqrt(n_vis)

# Set the means, SDs, and standard errors in vectors
cycle.mean = c(aud_mean, vis_mean)
cycle.sd = c(aud_sd, vis_sd)
se = c(se_aud, se_vis)

# Create a barplot and store the x-axis positions
bp <- barplot(cycle.mean, ylim = c(0, 100), names.arg = c("Auditory", "Visual"), col = c("skyblue", "lightgreen"), xlab = "Condition", ylab = "Mean Score", las = 1, main = "Comparison of Mean Scores by Condition")

# Calculate the x-axis positions for the centers of bars
bar_centers <- bp

# Add error bars
arrows(bar_centers, cycle.mean + se, bar_centers, cycle.mean - se, angle = 90, code = 3, length = 0.1)
