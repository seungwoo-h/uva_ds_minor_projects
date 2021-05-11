---
title: 'Assignment 2 : Statistical Learning'
---


Seungwoo,Hong 10879420
Ee June, Kim  11630566
Jin Hyun,Kim  11968850

### Objective of Analysis

The ClassifyRisk dataset which is available in the R liver package has 6 variables with 5 predictors. With this dataset this assignment, the customers would be classified as either “good risk” or “bad loss” by applying k-nearest neighbor algorithm.  This is important to be illustrated because the risk classification is a one of the key factors in lots of companies. 


### 1- Importing and understading the dataset (15 points)

```{r}
require(liver)
```

```{r}
library( liver ) 
```

Now, we impcrt the *ClassifyRisk* dataset as follows:
```{r}
data( classifyRisk ) 
```


We can see the structure of the dataset by using the `str` function as follows:
```{r}
str( classifyRisk )
```

```{r}
View( classifyRisk)
```



##Report a summary of the dataset by using appropriate R functions. 



##What is the number of variables? 

ㅁ6 variables




##Which type of variables are they? 

ㅁmortgage: Categorical and binary

ㅁnr_loans: Numerical and discrete

ㅁage: Numerical and discrete

ㅁmarital_status: multinomial / Categorical and nominal 

ㅁincome: Numerical and continuous

ㅁrisk: flag / Categorical and binary




##Are there any missing values?  
ㅁThere is no missing values




##What would be your strategy to deal with the missing data? 
ㅁWe will replace missing values with Mode or Mean




### 2- Partitioning the dataset (10 points)



Cross-validation is a type of technique to evaluate the results of a statistical analysis methods, which has a purpose of generating an independent dataset

By using Cross-Validation, we partition the *classifyRisk* dataset randomly into two groups as a train set (70%) and test set (30%). Here, we use the `partition` function from the *liver* package as follows:

```{r}
set.seed( 10 ) 

data_sets = partition( data = classifyRisk, prob = c( 0.7, 0.3 ) )

train_set = data_sets $ part1
test_set  = data_sets $ part2

train_label = train_set $ risk
test_label  = test_set $ risk
```


The `set.seed` function was used to create reproducible results. 


### 3- Validate the partition (15 points)

We want to validate the partition by testing whether the proportion of the variable `risk` differs between the two data sets. We use prop-test for the difference in proportions. 
`

```{r}
table(train_label)
```

```{r}
table(test_label)
```

Check whether each proportions of goodrisk from two train set and test set is equal 
```{r}
prop.test( x = c(80, 43), n = c( 93+ 80, 30 + 43), alternative = "two.sided"  )
```

The p-value is 0.09397, which is larger than 0.05. So, we can not reject the null hypothesis that proportions of good risk in both sample is equal.


### 4- Appling the KNN algorithm (20 points)

Find the k-nearest neighbor predictions for the test set using k = 2 and k = 90. 

Explain how you preparing the variables for the KNN algorithm. Which variables need to be normalized? 

The predictors that we used in the previous question, do not have the same scale. For example, variable `income` change between `r min( classifyRisk $ income )` and `r max( classifyRisk $ income )`, whereas variable `mortgage` is categorical. In this case, the values of variable `income` will overwhelm the contribution of `mortgage`. To avoid this situation we use *normalization*. So, we use *min-max normalization* and transfer the predictors. 

Which variables need dummy variable?
Because there is only yes or no in mortgage variable, this variable needs dummy variable.

```{r}
knn_2  = kNN( risk ~ ., train = train_set, test = test_set, transform = "minmax" ,k = 2 )

knn_90 = kNN( risk ~ ., train = train_set, test = test_set,transform = "minmax" ,k = 90 )

```


To have an overview of the prediction result, we could use the `table` function as follows: 
```{r}
table( knn_2 , test_label )
table( knn_90, test_label )
```


### 5- Evaluate the accuracy of the predictions (10 points)

Evaluate the accuracy of the predictions using the Mean Square Error (MSE). You could use the `mse` function from the R package **liver**. 

```{r}
MSE_2  = mse( test_label, knn_2  )
MSE_2

MSE_90 = mse( test_label, knn_90 )
MSE_90

```

MSE is a common method of evaluation for accuracy of models for continuous target variables. Lower MSE means the model predicts better. When the k is increased from 2 to 90, we can that the MSE has increased.  Conclusion can be that model with k= 2 is better than the one with k=90.

### 6- Optimal value of k for the kNN algorithm (15 points)

In the previous questions, to find the k-nearest neighbor for the test set, we set k = 2 and k = 90. But why 2 or 90? Find out the optimal value of `k`.

```{r}
k_list   = 1:90
mse_list = vector( length = length( k_list )  )

for( k in k_list ){
    knn_k = kNN( risk ~ ., train = train_set, test = test_set, transform = "minmax", k = k )
    mse_list[ k ] = mse( test_label, knn_k )
}

plot( x = k_list, y = mse_list, type = "o" )
```

From the graph above, it can be seen that the mse is the lowest when the value of k is 4. This can be verified through comparing the mse below

```{r}
knn_4 = kNN( risk ~ ., train = train_set, test = test_set,transform = "minmax" ,k = 4 )

MSE_2  = mse( test_label, knn_2  )
MSE_2

MSE_90 = mse( test_label, knn_90 )
MSE_90

MSE_4 = mse( test_label, knn_4 )
MSE_4
```


### 7- Writing a report (15 points)

To sum up, there were several steps and codes to illustrate the result of applying the k-neaest neighbor(knn) algorithm to organize the customer. First the partitioning of the ClassifyRist dataset randomly divided the data into two groups which are train set and test set. The validation was tested by prop-test to find the proportion of variable 'risk' difference between the groups. In the prop-test, the null hypothesis which is "the proportion of good risk in both sample is equal" was not rejected.

Before applying the Knn algorithm some variables such as the variable income and mortgage need the normalization to avoid the inaccurate results. After coding the predictions, with 'MSE' the accuracy of the predictions was evaluated. This codes showed that the model with k = 2 is more accurate than the model with k =90. Finally the optimal value of the K was founded which is 4. 



### 8- Bonus question (30 points)

## Applying KNN algorithm for analyizing bank dataset

### Import libraries

```{r}

library(liver)
library(ggplot2)
library(dplyr)

data(bank)
```

### Understanding the dataset

```{r}
str(bank)
```

```{r}
summary(bank)
```

The summary of the dataset shows there are unknown values we need to consider: education, contact and poutcome.

### Discovering relationship between numerical variables and a target variable

#### Correlation matrix

```{r}
bank_numerical <- bank %>% select(age, balance, day, duration, campaign, pdays, previous, deposit)
bank_numerical$deposit <- as.integer(bank_numerical$deposit) -1

cor(bank_numerical)
```

First to find out a relationship between numerical variables and a target variable, correlation analysis was performed.
Since a target variable is coded in "yes" or "no", we decided to recode values to integers 0 and 1.
From the matrix above, only the variable duration shows a moderate to high correlation (0.4) with a target variable deposit.

#### Boxplot to compare means

```{r}
ggplot(bank, aes(x = deposit, y = age)) +
  geom_boxplot()

ggplot(bank, aes(x = deposit, y = balance)) +
  geom_boxplot()

ggplot(bank, aes(x = deposit, y = duration)) +
  geom_boxplot()

ggplot(bank, aes(x = deposit, y = campaign)) +
  geom_boxplot()

ggplot(bank, aes(x = deposit, y = pdays)) +
  geom_boxplot()

ggplot(bank, aes(x = deposit, y = previous)) +
  geom_boxplot()
```

Above boxplots show graphical confirmation that only in a variable duration, there is a difference between means among two deposit groups.

### Discovering relationship between categorical variables and a target variable

#### Normalized histograms
```{r}
ggplot(bank, aes(x = job, fill = deposit)) +
  geom_histogram(stat = "count", position = "fill") +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ))

ggplot(bank, aes(x = marital, fill = deposit)) +
  geom_histogram(stat = "count", position = "fill")

ggplot(bank, aes(x = education, fill = deposit)) +
  geom_histogram(stat = "count", position = "fill")

ggplot(bank, aes(x = default, fill = deposit)) +
  geom_histogram(stat = "count", position = "fill")

ggplot(bank, aes(x = housing, fill = deposit)) +
  geom_histogram(stat = "count", position = "fill")

ggplot(bank, aes(x = loan, fill = deposit)) +
  geom_histogram(stat = "count", position = "fill")

ggplot(bank, aes(x = contact, fill = deposit)) +
  geom_histogram(stat = "count", position = "fill")

ggplot(bank, aes(x = poutcome, fill = deposit)) +
  geom_histogram(stat = "count", position = "fill")

```

From histograms above, varaibles job, marital, education, housing, loan, poutcome shows clear differences on the ratio of deposit.

### Feature Selection

From information above, we choose job, marital, education, housing, loan, poutcome, and duration variables to predict a target variable deposit.

### Train Test Split

```{r}
set.seed(10)

data_sets = partition(data = bank, prob = c(0.7, 0.3))
data_sets_fs = data_sets
data_sets_fs$part1 = data_sets_fs$part1 %>% select(job,
                                                   marital,
                                                   education,
                                                   housing,
                                                   loan,
                                                   duration,
                                                   poutcome,
                                                   deposit)

data_sets_fs$part2 = data_sets_fs$part2 %>% select(job,
                                                   marital,
                                                   education,
                                                   housing,
                                                   loan,
                                                   duration,
                                                   poutcome,
                                                   deposit)

data_sets_fs$part1$duration = scale(data_sets_fs$part1$duration)
data_sets_fs$part2$duration = scale(data_sets_fs$part2$duration)

train_set = data_sets$part1
test_set  = data_sets$part2
train_set_fs = data_sets_fs$part1
test_set_fs = data_sets_fs$part2

train_label = train_set$deposit
test_label  = test_set$deposit
train_label_fs = train_set_fs$deposit
test_label_fs = test_set_fs$deposit
```

For the feature-selected-model, we standardized a numerical variable for a better performance.

### Split validation

```{r}
t.test( x = train_set $ duration, y = test_set $ duration )
```

P-value (0.62) is greater than 0.05 therefore, we do not reject that there is no mean difference between train and test data.
So we assume our split is valid.

### Label distribution

```{r}
table(train_label)
table(test_label)

table(train_label_fs)
table(test_label_fs)
```

### Applying KNN algorithm

We will apply KNN algorithms both to baseline model and our new feature-selected-model to compare how our feature selection has improved performance.

#### Baseline model (with all features)

```{r}
k_list   = 1:90
mse_list = vector( length = length( k_list )  )

for( k in k_list ){
    knn_k = kNN( deposit ~ ., train = train_set, test = test_set, transform = "minmax", k = k )
    mse_list[ k ] = mse( test_label, knn_k )
}

plot( x = k_list, y = mse_list, type = "o" )
```

#### Feature-selected model

```{r}
mse_list_fs = vector(length = length(k_list))

for (k in k_list) {
  knn_k_fs = kNN(
    deposit ~ .,
    train = train_set_fs,
    test = test_set_fs,
    transform = "minmax",
    k = k
  )
  mse_list_fs[k] = mse(test_label_fs, knn_k_fs)
}

plot(x = k_list, y = mse_list_fs, type = "o")
```

### MSE at optimal K

```{r}
cat("MSE of baseline model : ", min(mse_list), "|| Optimal K: ", which.min(mse_list), "\n")
cat("MSE of feature-selected-model : ", min(mse_list_fs), "|| Optimal K: ", which.min(mse_list_fs))
```

The result show that our new model has improved in decreased in MSE by 0.0042766 compared to the original model.
