This project aims to show how the student's test scores is affected by other variables such as Gender, Ethnicity, Parental level of education, Lunch and Test preparation course.


```R
require(openxlsx) #library(openxlsx)
require(ggplot2)
require(data.table)
require(skimr)
require(GGally)
require(ggcorrplot)
require(forecast)
require(dplyr)
options(repr.plot.width=12.7, repr.plot.height=8.5)

data_path='/Users/furkan.celen/IE360HW1Regression/study_performance.csv'


performance =read.csv(data_path)
str(performance)

```

    Zorunlu paket yükleniyor: dplyr
    
    
    Attaching package: ‘dplyr’
    
    
    The following objects are masked from ‘package:data.table’:
    
        between, first, last
    
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    


    'data.frame':	1000 obs. of  8 variables:
     $ gender                     : chr  "female" "female" "female" "male" ...
     $ race_ethnicity             : chr  "group B" "group C" "group B" "group A" ...
     $ parental_level_of_education: chr  "bachelor's degree" "some college" "master's degree" "associate's degree" ...
     $ lunch                      : chr  "standard" "standard" "standard" "free/reduced" ...
     $ test_preparation_course    : chr  "none" "completed" "none" "none" ...
     $ math_score                 : int  72 69 90 47 76 71 88 40 64 38 ...
     $ reading_score              : int  72 90 95 57 78 83 95 43 64 60 ...
     $ writing_score              : int  74 88 93 44 75 78 92 39 67 50 ...



```R
# numerical statistics of the data
summary_data=skim(performance)
print(summary_data)
```

    ── Data Summary ────────────────────────
                               Values     
    Name                       performance
    Number of rows             1000       
    Number of columns          8          
    _______________________               
    Column type frequency:                
      character                5          
      numeric                  3          
    ________________________              
    Group variables            None       
    
    ── Variable type: character ────────────────────────────────────────────────────
      skim_variable               n_missing complete_rate min max empty n_unique
    [90m1[39m gender                              0             1   4   6     0        2
    [90m2[39m race_ethnicity                      0             1   7   7     0        5
    [90m3[39m parental_level_of_education         0             1  11  18     0        6
    [90m4[39m lunch                               0             1   8  12     0        2
    [90m5[39m test_preparation_course             0             1   4   9     0        2
      whitespace
    [90m1[39m          0
    [90m2[39m          0
    [90m3[39m          0
    [90m4[39m          0
    [90m5[39m          0
    
    ── Variable type: numeric ──────────────────────────────────────────────────────
      skim_variable n_missing complete_rate mean   sd p0  p25 p50 p75 p100 hist 
    [90m1[39m math_score            0             1 66.1 15.2  0 57    66  77  100 ▁▁▅▇▃
    [90m2[39m reading_score         0             1 69.2 14.6 17 59    70  79  100 ▁▂▆▇▃
    [90m3[39m writing_score         0             1 68.1 15.2 10 57.8  69  79  100 ▁▂▅▇▃



```R
performance =data.table(performance)
str(performance)

ggplot(performance, aes(x=math_score)) + geom_histogram()
ggplot(performance, aes(x=reading_score)) + geom_histogram()
ggplot(performance, aes(x=writing_score)) + geom_histogram()

```

    Classes ‘data.table’ and 'data.frame':	1000 obs. of  8 variables:
     $ gender                     : chr  "female" "female" "female" "male" ...
     $ race_ethnicity             : chr  "group B" "group C" "group B" "group A" ...
     $ parental_level_of_education: chr  "bachelor's degree" "some college" "master's degree" "associate's degree" ...
     $ lunch                      : chr  "standard" "standard" "standard" "free/reduced" ...
     $ test_preparation_course    : chr  "none" "completed" "none" "none" ...
     $ math_score                 : int  72 69 90 47 76 71 88 40 64 38 ...
     $ reading_score              : int  72 90 95 57 78 83 95 43 64 60 ...
     $ writing_score              : int  74 88 93 44 75 78 92 39 67 50 ...
     - attr(*, ".internal.selfref")=<externalptr> 


    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.



    
![png](output_3_2.png)
    


    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.



    
![png](output_3_4.png)
    



    
![png](output_3_5.png)
    



```R
ggplot(performance, aes(x=math_score)) + geom_boxplot()
ggplot(performance, aes(x=reading_score)) + geom_boxplot()
ggplot(performance, aes(x=writing_score)) + geom_boxplot()




```


    
![png](output_4_0.png)
    



    
![png](output_4_1.png)
    



    
![png](output_4_2.png)
    



```R
ggpairs(performance)
```

    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.



    
![png](output_5_1.png)
    



```R
# Check for missing values
colSums(is.na(performance))

```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>gender</dt><dd>0</dd><dt>race_ethnicity</dt><dd>0</dd><dt>parental_level_of_education</dt><dd>0</dd><dt>lunch</dt><dd>0</dd><dt>test_preparation_course</dt><dd>0</dd><dt>math_score</dt><dd>0</dd><dt>reading_score</dt><dd>0</dd><dt>writing_score</dt><dd>0</dd></dl>




```R
# Check for duplicated rows
sum(duplicated(performance))


```


0



```R
# Check column data types
column_types <- sapply(performance, class)

# Check for missing values
missing_values <- colSums(is.na(performance))

# Combine into a data frame for better visualization
info_df <- data.frame(Column = names(performance),
                      DataType = column_types,
                      NumMissing = missing_values,
                      stringsAsFactors = FALSE)

# Print the information
print(info_df)
```

                                                     Column  DataType NumMissing
    gender                                           gender character          0
    race_ethnicity                           race_ethnicity character          0
    parental_level_of_education parental_level_of_education character          0
    lunch                                             lunch character          0
    test_preparation_course         test_preparation_course character          0
    math_score                                   math_score   integer          0
    reading_score                             reading_score   integer          0
    writing_score                             writing_score   integer          0



```R
 Find the number of unique values for each column
num_unique <- sapply(df, function(x) n_distinct(x, na.rm = TRUE))

# Combine into a data frame for better visualization
unique_df <- data.frame(Column = names(df),
                        NumUnique = num_unique,
                        stringsAsFactors = FALSE)

# Print the information
print(unique_df)

```


```R
#Find the number of unique values for each column
num_unique <- sapply(performance, function(x) n_distinct(x, na.rm = TRUE))

# Combine into a data frame for better visualization
unique_df <- data.frame(Column = names(performance),
                        NumUnique = num_unique,
                        stringsAsFactors = FALSE)

# Print the information
print(unique_df)

```

                                                     Column NumUnique
    gender                                           gender         2
    race_ethnicity                           race_ethnicity         5
    parental_level_of_education parental_level_of_education         6
    lunch                                             lunch         2
    test_preparation_course         test_preparation_course         2
    math_score                                   math_score        81
    reading_score                             reading_score        72
    writing_score                             writing_score        77


## Exploratory Data Analysis


```R
# Print unique categories in each variable

cat("Categories in 'gender' variable: ")
unique(performance$gender)

cat("Categories in 'race_ethnicity' variable: ")
unique(performance$race_ethnicity)

cat("Categories in 'parental_level_of_education' variable: ")
unique(performance$parental_level_of_education)

cat("Categories in 'lunch' variable: ")
unique(performance$lunch)

cat("Categories in 'test_preparation_course' variable: ")
unique(performance$test_preparation_course)
```

    Categories in 'gender' variable: 


<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>'female'</li><li>'male'</li></ol>



    Categories in 'race_ethnicity' variable: 


<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>'group B'</li><li>'group C'</li><li>'group A'</li><li>'group D'</li><li>'group E'</li></ol>



    Categories in 'parental_level_of_education' variable: 


<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>'bachelor\'s degree'</li><li>'some college'</li><li>'master\'s degree'</li><li>'associate\'s degree'</li><li>'high school'</li><li>'some high school'</li></ol>



    Categories in 'lunch' variable: 


<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>'standard'</li><li>'free/reduced'</li></ol>



    Categories in 'test_preparation_course' variable: 


<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>'none'</li><li>'completed'</li></ol>




```R
# Define numerical and categorical columns
numeric_features <- names(performance)[sapply(performance, is.numeric)]
categorical_features <- names(performance)[sapply(performance, function(x) is.factor(x) | is.character(x))]

# Print columns
cat("We have", length(numeric_features), "numerical features:", paste(numeric_features, collapse = ", "), "\n")
cat("\nWe have", length(categorical_features), "categorical features:", paste(categorical_features, collapse = ", "), "\n")

```

    We have 3 numerical features: math_score, reading_score, writing_score 
    
    We have 5 categorical features: gender, race_ethnicity, parental_level_of_education, lunch, test_preparation_course 



```R
# Calculate total score and average
performance$total_score <- performance$math_score + performance$reading_score + performance$writing_score
performance$average <- performance$total_score / 3

# View the first few rows of the updated data frame
head(performance)

```


<table class="dataframe">
<caption>A data.table: 6 × 10</caption>
<thead>
	<tr><th scope=col>gender</th><th scope=col>race_ethnicity</th><th scope=col>parental_level_of_education</th><th scope=col>lunch</th><th scope=col>test_preparation_course</th><th scope=col>math_score</th><th scope=col>reading_score</th><th scope=col>writing_score</th><th scope=col>total_score</th><th scope=col>average</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>female</td><td>group B</td><td>bachelor's degree </td><td>standard    </td><td>none     </td><td>72</td><td>72</td><td>74</td><td>218</td><td>72.66667</td></tr>
	<tr><td>female</td><td>group C</td><td>some college      </td><td>standard    </td><td>completed</td><td>69</td><td>90</td><td>88</td><td>247</td><td>82.33333</td></tr>
	<tr><td>female</td><td>group B</td><td>master's degree   </td><td>standard    </td><td>none     </td><td>90</td><td>95</td><td>93</td><td>278</td><td>92.66667</td></tr>
	<tr><td>male  </td><td>group A</td><td>associate's degree</td><td>free/reduced</td><td>none     </td><td>47</td><td>57</td><td>44</td><td>148</td><td>49.33333</td></tr>
	<tr><td>male  </td><td>group C</td><td>some college      </td><td>standard    </td><td>none     </td><td>76</td><td>78</td><td>75</td><td>229</td><td>76.33333</td></tr>
	<tr><td>female</td><td>group B</td><td>associate's degree</td><td>standard    </td><td>none     </td><td>71</td><td>83</td><td>78</td><td>232</td><td>77.33333</td></tr>
</tbody>
</table>




```R
# Count the number of students with full marks in each subject
reading_full <- sum(performance$reading_score == 100)
writing_full <- sum(performance$writing_score == 100)
math_full <- sum(performance$math_score == 100)

# Print the results
cat("Number of students with full marks in Maths:", math_full, "\n")
cat("Number of students with full marks in Writing:", writing_full, "\n")
cat("Number of students with full marks in Reading:", reading_full, "\n")

```

    Number of students with full marks in Maths: 7 
    Number of students with full marks in Writing: 14 
    Number of students with full marks in Reading: 17 



```R
# Count the number of students with less than 20 marks in each subject
reading_less_20 <- sum(performance$reading_score <= 20)
writing_less_20 <- sum(performance$writing_score <= 20)
math_less_20 <- sum(performance$math_score <= 20)

# Print the results
cat("Number of students with less than 20 marks in Maths:", math_less_20, "\n")
cat("Number of students with less than 20 marks in Writing:", writing_less_20, "\n")
cat("Number of students with less than 20 marks in Reading:", reading_less_20, "\n")

```

    Number of students with less than 20 marks in Maths: 4 
    Number of students with less than 20 marks in Writing: 3 
    Number of students with less than 20 marks in Reading: 1 


## Data Visualization


```R
# Load required libraries
library(GGally)

# Create pair plot
ggpairs(performance, aes(color = gender))

```

    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    [1m[22m`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.



    
![png](output_18_1.png)
    



```R
# Load required libraries
library(ggplot2)

# Create histograms
p1 <- ggplot(performance, aes(x = average)) +
  geom_histogram(bins = 30, color = "black", fill = "green", alpha = 0.5) +
  ggtitle("Histogram of Average Scores") +
  xlab("Average Score") +
  ylab("Frequency")

p2 <- ggplot(performance, aes(x = average, fill = gender)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Gender") +
  xlab("Average Score") +
  ylab("Frequency") +
  facet_wrap(~gender)

# Combine plots
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

```

    
    Attaching package: ‘gridExtra’
    
    
    The following object is masked from ‘package:dplyr’:
    
        combine
    
    



    
![png](output_19_1.png)
    



```R
p1 <- ggplot(performance, aes(x = total_score)) +
  geom_histogram(bins = 30, color = "black", fill = "green", alpha = 0.5) +
  ggtitle("Histogram of Total Scores") +
  xlab("Total Score") +
  ylab("Frequency")

p2 <- ggplot(performance, aes(x = total_score, fill = gender)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Total Scores by Gender") +
  xlab("Total Score") +
  ylab("Frequency") +
  facet_wrap(~gender)

# Combine plots
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
```


    
![png](output_20_0.png)
    



```R
# Create histograms
p1 <- ggplot(performance, aes(x = average, fill = lunch)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Lunch") +
  xlab("Average Score") +
  ylab("Frequency") +
  facet_wrap(~gender)

p2 <- ggplot(subset(performance, gender == "female"), aes(x = average, fill = lunch)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Lunch (Female)") +
  xlab("Average Score") +
  ylab("Frequency") 

p3 <- ggplot(subset(performance, gender == "male"), aes(x = average, fill = lunch)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Lunch (Male)") +
  xlab("Average Score") +
  ylab("Frequency")

# Combine plots
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

```


    
![png](output_21_0.png)
    



```R
# Create histograms
p1 <- ggplot(performance, aes(x = average, fill = parental_level_of_education)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Parental Level of Education") +
  xlab("Average Score") +
  ylab("Frequency") +
  facet_wrap(~gender)

p2 <- ggplot(subset(performance, gender == "male"), aes(x = average, fill = parental_level_of_education)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Parental Level of Education (Male)") +
  xlab("Average Score") +
  ylab("Frequency")

p3 <- ggplot(subset(performance, gender == "female"), aes(x = average, fill = parental_level_of_education)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Parental Level of Education (Female)") +
  xlab("Average Score") +
  ylab("Frequency")

# Combine plots
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

```


    
![png](output_22_0.png)
    



```R
# Create histograms
p1 <- ggplot(performance, aes(x = average, fill = race_ethnicity)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Race/Ethnicity") +
  xlab("Average Score") +
  ylab("Frequency") +
  facet_wrap(~gender)

p2 <- ggplot(subset(performance, gender == "male"), aes(x = average, fill = race_ethnicity)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Race/Ethnicity (Male)") +
  xlab("Average Score") +
  ylab("Frequency")

p3 <- ggplot(subset(performance, gender == "female"), aes(x = average, fill = race_ethnicity)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5) +
  ggtitle("Histogram of Average Scores by Race/Ethnicity (Female)") +
  xlab("Average Score") +
  ylab("Frequency")

# Combine plots
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

```


    
![png](output_23_0.png)
    


## Data Preprocessing


```R
l_fit = lm(average~.,data=performance)

```


```R
l_fit
```


    
    Call:
    lm(formula = average ~ ., data = performance)
    
    Coefficients:
                                     (Intercept)  
                                       1.450e-14  
                                      gendermale  
                                      -2.252e-14  
                           race_ethnicitygroup B  
                                       9.423e-15  
                           race_ethnicitygroup C  
                                      -7.895e-15  
                           race_ethnicitygroup D  
                                      -1.342e-14  
                           race_ethnicitygroup E  
                                      -2.186e-14  
    parental_level_of_educationbachelor's degree  
                                       2.432e-14  
          parental_level_of_educationhigh school  
                                       6.463e-15  
      parental_level_of_educationmaster's degree  
                                      -3.654e-15  
         parental_level_of_educationsome college  
                                      -6.282e-16  
     parental_level_of_educationsome high school  
                                       5.442e-15  
                                   lunchstandard  
                                      -1.903e-14  
                     test_preparation_coursenone  
                                       1.719e-14  
                                      math_score  
                                       3.333e-01  
                                   reading_score  
                                       3.333e-01  
                                   writing_score  
                                       3.333e-01  
                                     total_score  
                                              NA  




```R
summary(l_fit)
```


    
    Call:
    lm(formula = average ~ ., data = performance)
    
    Residuals:
           Min         1Q     Median         3Q        Max 
    -8.854e-14 -1.093e-14 -1.700e-15  5.370e-15  2.963e-12 
    
    Coefficients: (1 not defined because of singularities)
                                                   Estimate Std. Error    t value
    (Intercept)                                   1.450e-14  2.369e-14  6.120e-01
    gendermale                                   -2.252e-14  1.026e-14 -2.195e+00
    race_ethnicitygroup B                         9.424e-15  1.264e-14  7.450e-01
    race_ethnicitygroup C                        -7.895e-15  1.184e-14 -6.670e-01
    race_ethnicitygroup D                        -1.342e-14  1.223e-14 -1.097e+00
    race_ethnicitygroup E                        -2.186e-14  1.377e-14 -1.587e+00
    parental_level_of_educationbachelor's degree  2.432e-14  1.125e-14  2.162e+00
    parental_level_of_educationhigh school        6.463e-15  9.771e-15  6.610e-01
    parental_level_of_educationmaster's degree   -3.654e-15  1.451e-14 -2.520e-01
    parental_level_of_educationsome college      -6.282e-16  9.275e-15 -6.800e-02
    parental_level_of_educationsome high school   5.442e-15  1.004e-14  5.420e-01
    lunchstandard                                -1.903e-14  7.079e-15 -2.689e+00
    test_preparation_coursenone                   1.719e-14  7.517e-15  2.287e+00
    math_score                                    3.333e-01  5.814e-16  5.733e+14
    reading_score                                 3.333e-01  7.825e-16  4.260e+14
    writing_score                                 3.333e-01  8.927e-16  3.734e+14
    total_score                                          NA         NA         NA
                                                 Pr(>|t|)    
    (Intercept)                                   0.54082    
    gendermale                                    0.02840 *  
    race_ethnicitygroup B                         0.45618    
    race_ethnicitygroup C                         0.50514    
    race_ethnicitygroup D                         0.27277    
    race_ethnicitygroup E                         0.11275    
    parental_level_of_educationbachelor's degree  0.03089 *  
    parental_level_of_educationhigh school        0.50848    
    parental_level_of_educationmaster's degree    0.80127    
    parental_level_of_educationsome college       0.94601    
    parental_level_of_educationsome high school   0.58789    
    lunchstandard                                 0.00729 ** 
    test_preparation_coursenone                   0.02244 *  
    math_score                                    < 2e-16 ***
    reading_score                                 < 2e-16 ***
    writing_score                                 < 2e-16 ***
    total_score                                        NA    
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Residual standard error: 9.784e-14 on 984 degrees of freedom
    Multiple R-squared:      1,	Adjusted R-squared:      1 
    F-statistic: 1.414e+30 on 15 and 984 DF,  p-value: < 2.2e-16



Adjusted R squared equals 1 indicates the model perfectly predicts values in the target field.


```R
plot(l_fit)
```


    
![png](output_29_0.png)
    



    
![png](output_29_1.png)
    



    
![png](output_29_2.png)
    



    
![png](output_29_3.png)
    



```R
l_fit = lm(math_score~.,data=performance)

```


```R
summary(l_fit)
```


    
    Call:
    lm(formula = math_score ~ ., data = performance)
    
    Residuals:
           Min         1Q     Median         3Q        Max 
    -6.741e-13 -1.160e-14 -9.000e-16  7.700e-15  3.569e-12 
    
    Coefficients: (1 not defined because of singularities)
                                                   Estimate Std. Error    t value
    (Intercept)                                   1.564e-13  2.905e-14  5.385e+00
    gendermale                                    2.199e-14  1.258e-14  1.748e+00
    race_ethnicitygroup B                         1.569e-14  1.550e-14  1.012e+00
    race_ethnicitygroup C                         1.522e-15  1.452e-14  1.050e-01
    race_ethnicitygroup D                         3.549e-15  1.499e-14  2.370e-01
    race_ethnicitygroup E                         1.216e-14  1.688e-14  7.200e-01
    parental_level_of_educationbachelor's degree  3.470e-14  1.379e-14  2.516e+00
    parental_level_of_educationhigh school        1.481e-14  1.198e-14  1.237e+00
    parental_level_of_educationmaster's degree    3.850e-14  1.779e-14  2.164e+00
    parental_level_of_educationsome college       1.823e-15  1.137e-14  1.600e-01
    parental_level_of_educationsome high school  -1.062e-15  1.231e-14 -8.600e-02
    lunchstandard                                 4.316e-14  8.678e-15  4.973e+00
    test_preparation_coursenone                  -1.987e-14  9.216e-15 -2.156e+00
    reading_score                                -1.000e+00  1.302e-15 -7.679e+14
    writing_score                                -1.000e+00  1.555e-15 -6.430e+14
    total_score                                   1.000e+00  7.128e-16  1.403e+15
    average                                              NA         NA         NA
                                                 Pr(>|t|)    
    (Intercept)                                  9.04e-08 ***
    gendermale                                     0.0809 .  
    race_ethnicitygroup B                          0.3117    
    race_ethnicitygroup C                          0.9165    
    race_ethnicitygroup D                          0.8129    
    race_ethnicitygroup E                          0.4715    
    parental_level_of_educationbachelor's degree   0.0120 *  
    parental_level_of_educationhigh school         0.2165    
    parental_level_of_educationmaster's degree     0.0307 *  
    parental_level_of_educationsome college        0.8726    
    parental_level_of_educationsome high school    0.9312    
    lunchstandard                                7.76e-07 ***
    test_preparation_coursenone                    0.0313 *  
    reading_score                                 < 2e-16 ***
    writing_score                                 < 2e-16 ***
    total_score                                   < 2e-16 ***
    average                                            NA    
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Residual standard error: 1.199e-13 on 984 degrees of freedom
    Multiple R-squared:      1,	Adjusted R-squared:      1 
    F-statistic: 1.064e+30 on 15 and 984 DF,  p-value: < 2.2e-16




```R
plot(l_fit)
```


    
![png](output_32_0.png)
    



    
![png](output_32_1.png)
    



    
![png](output_32_2.png)
    



    
![png](output_32_3.png)
    


 ## Residuals:
The residuals represent the differences between the observed and predicted values of the dependent variable (average score). In this case, the residuals are very small, indicating that the model fits the data extremely well. The smallest residual is approximately -8.854e-14, and the largest is approximately 2.963e-12.
Coefficients:
Intercept: The intercept represents the predicted value of the dependent variable when all predictor variables are zero. In this case, the intercept is approximately 1.450e-14.
Gender (male): The coefficient for the gender variable (male) is approximately -2.252e-14. This suggests that, on average, being male is associated with a decrease of approximately -2.252e-14 in the average score, holding all other variables constant.
Race/Ethnicity (Groups B-E): The coefficients for different race/ethnicity groups represent the average change in the average score associated with each group compared to the reference group (Group A). For example, the coefficient for Group B is approximately 9.424e-15, indicating that, on average, being in Group B is associated with an increase of approximately 9.424e-15 in the average score, holding all other variables constant.
Parental Level of Education: The coefficients for different levels of parental education represent the average change in the average score associated with each level compared to the reference level (some college). For example, the coefficient for having a bachelor's degree is approximately 2.432e-14, indicating that, on average, having a bachelor's degree is associated with an increase of approximately 2.432e-14 in the average score, holding all other variables constant.
Lunch (standard): The coefficient for the lunch variable (standard) is approximately -1.903e-14. This suggests that, on average, having a standard lunch is associated with a decrease of approximately -1.903e-14 in the average score, holding all other variables constant.
Test Preparation Course (none): The coefficient for the test preparation course variable (none) is approximately 1.719e-14. This indicates that, on average, not taking a test preparation course is associated with an increase of approximately 1.719e-14 in the average score, holding all other variables constant.
Math, Reading, and Writing Scores: The coefficients for math, reading, and writing scores represent the average change in the average score associated with a one-unit increase in each score, holding all other variables constant. For example, the coefficient for math score is approximately 3.333e-01, indicating that, on average, a one-unit increase in the math score is associated with an increase of approximately 0.333 in the average score, holding all other variables constant.
Significance:
The p-values associated with each coefficient indicate the probability of observing the estimated coefficient (or more extreme) under the null hypothesis that the true coefficient is zero. Significant predictors have p-values below the chosen significance level (e.g., 0.05) and are indicated by asterisks in the "Signif. codes" column.
Residual standard error, Multiple R-squared, Adjusted R-squared, and F-statistic:
The residual standard error is approximately 9.784e-14, indicating the average distance between the observed and predicted values of the dependent variable.
The R-squared values are 1, indicating that the model explains all the variability in the average score. The Adjusted R-squared value is also 1, suggesting that the model is not overfitting and that all predictor variables are relevant.
The F-statistic is very large, indicating that the overall model is statistically significant.
In summary, the model suggests that several variables, including gender, race/ethnicity, parental level of education, lunch type, test preparation course, and individual scores in math, reading, and writing, are associated with the average score. The model fits the data extremely well, explaining all the variability in the average score.
