---
title: "Assignment 1 Exploratory Data Analysis"
---

Jin Hyun,Kim  11968850
Ee June, Kim  11630566
Seungwoo,Hong 10879420


The codes illustrate the relationship among adult dataset which is available in the R liver package via exploratory data analysis (EDA). The adult dataset contains 15 variables. Through this assignment, each data would be discussed and visualized. 

## Load 'ggplot2', 'liver', 'ROCR'  packages
```{r message = FALSE}
require( ggplot2 )   # load the package "ggplot2"
require( liver   )   # load the package "liver"
require( ROCR    )   # load the package "ROCR"

library(ggplot2)
library(plyr)
library(ROCR)
```


#load data
```{r}
data("adult")
```


###EDA of the dependent variable

The original dataset contains a distribution of 24.08% entries labeled with >50k and 75.91% entries labeled with <=50k. The following graphs and statistics pertain to the orignal dataset.

```{r}
barplot(table(adult$income),main = 'Income Classification',col='blue',ylab ='No. of people')

```

### The summary of the dataset 
```{r}
summary( adult )
```


### The structure of the data

```{r}
str( adult )   
```

### Explanations of variables

- age: The age of an individual
  (Type: Numerical and discrete) 

- workclass: A general term to represent the employment status of an individual.
  (Type: Categorical and nominal)

- education: The highest level of education achieved by an individual.
  (Type: Categorical and nominal)

- education.num: The highest level of education achieved in numerical form.
  (Type: Numerial and discrete)

- marital.status: Marital status of an individual. Married.civ.spouse corresponds to a civilian spouse while Married.AF.spouse is a spouse in the Armed Forces.
  (Type: Categorical and nominal)

- occupation: the general type of occupation of an individual
  (Type: Categorical and nominal)

- relationship: Represents what this individual is relative to others. For example an individual could be a Husband. Each entry only has one relationship attribute and is somewhat redundant with marital status. We might not make use of this attribute at all
  (Type: Categorical and nominal )

- race: Descriptions of an individual’s race
  (Type: Categorical and nominal )

- sex: the biological sex of the individual
  (Type: Categorical and binary )

- capital.gain: capital gains for an individual
  (Type: Numerical and continuous)

- capital.loss: capital loss for an individual
  (Type: Numerical and continuous)

- hours.per.week: the hours an individual has reported to work per week
  (Type: Numerial and discrete)

- native.country: country of origin for an individual
  (Type: Categorical and nominal )

- ?  : missing variables.
  

- the label: whether or not an individual makes more than $50,000 annually.

           * <=50k
           * >50k
  (Type: Categorical and binary )


### Concluding a few things before applying the various classification algorithms

  Before investigating the adult dataset, 2 variables which are education and relationship were eliminated. The education_num variable is a continuous variable which shows the number of total education years. While, variable education is the discrete variable which solely represents the highest level of education. Therefore, the variable education could be substituted by the variable education_num. The relationship variable demonstrates the respondent's role in their family which could be explained with the variable gender and marital status. Since both two variables could be replaced by other variables, those were deleted for simplicity.  


```{r}

adult$educatoin <- NULL
adult$relationship <- NULL

```


###Checking income with respect to age


```{r}
ggplot(adult) + aes(x=as.numeric(age), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')+
  labs(x="Age",y="Count",title = "Income w.r.t Age")+ 
  scale_fill_manual( values = c( "blue", "red" ) ) 
```


```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = as.numeric( age ),
                 fill = factor( income ) ),
            position = "fill" ) + 
  scale_x_discrete( "age" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "Income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) 
```

 As we notice total incomes mostly come from 20- 40 ages. On the other hand, It can be checked that middle aged people show the highest proportion of 50k> income earnings.  We can conclude that age has a correlation with  income. Before middle age, age has a positive impact. However after that, age has a negative effect on income. 
The reason behind this rise and fall could be  promotion on workplace and  retirement 



###Checking income with respect to gender group

```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( sex ),
                 fill = factor( income ) ),
            position = "stack" ) + 
  scale_x_discrete( "sex" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "income " ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) +
  labs(x="Age",y="Count",title = "Income w.r.t gender group")
```

It is noticed that the average income of male is about twice larger than that of female.


```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( sex ),
                 fill = factor( income ) ),
            position = "fill" ) + 
  scale_x_discrete( "sex" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) )  +
  labs(x="Age",y="Count",title = "Income w.r.t gender group")
```

 Addtionally, it can be checked that proportion of male  who earns more than 50k is higher than that of female. So, we can conclude that sex has an effect on  income.  Although working conditions for women is generally getting better and better than before, we can still  see that males are getting paid more than women. Possible reasons for this gap could be preference of male in workplace, disadvatage from childbirth, etc


###Checking income with respect to workclass


### Not normalized

```{r}

ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( workclass ),
                 fill = factor( income ) ),
            position = "stack" ) + 
  scale_x_discrete( "Workclass" ) + 
  scale_y_continuous( "No. of people" ) + 
  guides( fill = guide_legend( title = " income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Age",y="Count",title = "Income w.r.t workclass")
```

From graph above, it can be seen that the average income of private job is the highest.


### Normalized
```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( workclass ),
                 fill = factor( income ) ),
            position = "fill" ) + 
  scale_x_discrete( "Workclass" ) + 
  scale_y_continuous( "No. of people" ) + 
  guides( fill = guide_legend( title = " income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Age",y="Count",title = "Income w.r.t workclass")
```

On the contrary to the total income, the proportion of  private workclass was the lowest among eight types of job.  Being self-emp-inc leads to higher income the most. Over 50% of self employed people earns more than 50k. Although there is not very clear difference in 50k> income earnings, we can see that self-emo-inc job and Federal-gov job has advantage in earning lots of money.  
 
Private job has the worst negative effect on income among job types. However, this type of job show highet total income. The big reason for this contradiction could be scale of private job market. These jobs are general types of job, so the large number of job could be the reason for the highest total income.

### Education_num

# Not normalized

```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( education.num ),
                 fill = factor( income ) ),
            position = "stack" ) + 
  scale_x_discrete( "education num" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "income " ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) +
  labs(x="Age",y="Count",title = "Income w.r.t education")
```


# Normalized

```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( education.num ),
                 fill = factor( income ) ),
            position = "fill" ) + 
  scale_x_discrete( "education num" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "Income " ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) +
  labs(x="Age",y="Count",title = "Income w.r.t education")
```

It can be seen that the total incomes are highest when the education num is between 9 and 10.  However, it is shown that a longer education duration leads to higher income from the second graph.

The reason behind this correlation difference could be that general job requirements are usually fulfilled with  9- 10 years education levels. Although jobs that requires long education period allow high earnings, the number of these special jobs is generally not that high. The less number of special job probably has caused less total income amount.

Briefly, education num has a positive correlation with income. 



### Occupation

```{r}
summary(adult$occupation)
```


For simplicity of the model, occupation is also blocked into several groups, namely Blue-Collar, Professional, Sales, Service, and White-Collar. Because of the small number of people who are service members, they are combined with Unknowns to form an Unknown/Other group.

```{r}
levels(adult$occupation)[1] <- 'Unknown'
adult$occupation <- gsub('Adm-clerical', 'White-Collar', adult$occupation)
adult$occupation <- gsub('Craft-repair', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Exec-managerial', 'White-Collar', adult$occupation)
adult$occupation <- gsub('Farming-fishing', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Handlers-cleaners', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Machine-op-inspct', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Other-service', 'Service', adult$occupation)
adult$occupation <- gsub('Priv-house-serv', 'Service', adult$occupation)
adult$occupation <- gsub('Prof-specialty', 'Professional', adult$occupation)
adult$occupation <- gsub('Protective-serv', 'Service', adult$occupation)
adult$occupation <- gsub('Tech-support', 'Service', adult$occupation)
adult$occupation <- gsub('Transport-moving', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Unknown', 'Other/Unknown', adult$occupation)
adult$occupation <- gsub('Armed-Forces', 'Other/Unknown', adult$occupation)
adult$occupation <- as.factor(adult$occupation)
summary(adult$occupation)
```


# Create a dataframe
```{r}

df2 <- data.frame(table(adult$income, adult$occupation))
names(df2) <- c('income', 'occupation', 'count')
df2
```

```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( occupation ),
                 fill = factor( income ) ),
            position = "stack" ) + 
  scale_x_discrete( "Occupation" ) + 
  scale_y_continuous( "Income" ) + 
  guides( fill = guide_legend( title = "income " ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) +
  labs(x="Age",y="Count",title = "Income w.r.t occupation")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

From this graph, it can be seen that the income from Blue-Collar is the highest. The professional job shows the least amount of total income.


```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( occupation ),
                 fill = factor( income ) ),
            position = "fill" ) + 
  scale_x_discrete( "Occupation" ) + 
  scale_y_continuous( "Income" ) + 
  guides( fill = guide_legend( title = "income " ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) +
  labs(x="Age",y="Count",title = "Income w.r.t occupation")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

From the second graph, it is shown that Blue- Collar occupation shows the lowest rate of 50k > income. On the other hand the professional job illustrates the highest positive correlation with high income. 

The possible reason behind this rank change could be the number of job. The number of Blue- Collar job is usually the highest among 6 types of occupation. This job  number might be the reason for the largest total income despite the lowest income rate (50k>)

Briefly,  occupations are correlated with income.



### Marital_status

marital_status is a categorical variable with 7 categories indicating the marital status of observations. In fact, it can be blocked into a few categories as well.

```{r}
summary(adult$marital.status)
```

```{r}
adult$marital.status <- gsub('Married-AF-spouse', 'Married', adult$marital.status)
adult$marital.status <- gsub('Married-civ-spouse', 'Married', adult$marital.status)
adult$marital.status <- gsub('Married-spouse-absent', 'Married', adult$marital.status)
adult$marital.status <- gsub('Never-married', 'Single', adult$marital.status)

```


## Not normalized

```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( marital.status ),
                 fill = factor( income ) ),
            position = "stack")+ 
  scale_x_discrete( "Marital status" ) + 
  scale_y_continuous( "Income" ) + 
  guides( fill = guide_legend( title = "income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) +
  labs(x="Age",y="Count",title = "Income w.r.t marital status")
```

It is shown that income from married couple is the largest among five groups.

##Normalized
```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( marital.status ),
                 fill = factor( income ) ),
            position = "fill" ) + 
  scale_x_discrete( "Marital status" ) + 
  scale_y_continuous( "Income" ) + 
  guides( fill = guide_legend( title = "income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) +
  labs(x="Age",y="Count",title = "Income w.r.t marital status")

```
 
From barplots above, variable "marital status" seems to have an association with the target variable "income". Nearly half of the observations that are currently married have an income more than 50K, while observations with other status show that more than 80 % of people have an income of less than 50K. The proportion of people having an income of 50K or more was shown smallest in a group of no marriage experience.

The high income from married couples can be explained with the possibility of married couples  working together. 


### Race


## Not normalized

```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( race ),
                 fill = factor( income ) ),
            position = "stack") + 
  scale_x_discrete( "race" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) +
  labs(x="Age",y="Count",title = "Income w.r.t race")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


```

It can be seen that total income from White is the largest among all races. The distribution of variable 'race' explains that there are 5 unique categories, and most of them are "white" which is roughly 85.5% with the second major race "black" (9.59%). It is important to notice that the dataset is biased toward the specific race "white". 

## Normalized

```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( race ),
                 fill = factor( income ) ),
            position = "fill" ) + 
  scale_x_discrete( "race" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

It is illustrated that ‘Asian- Pac- islander’ and ‘White’ equally have the highest proportion of over 50k income earner. Since the plot shows that there are clear income gap between races, variable ‘race’ has an impact on a dependent variable.


### Capital_gain & Capital_loss


#### Scatter Plot
```{r}
ggplot(adult, aes(x = income, y = capital.gain)) +
  geom_point()
```

# histogram of capital_gain
```{r}

ggplot(adult) + aes(x=as.numeric(capital.gain), group=income, fill=income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Income w.r.t Capital Gain')+ 
  scale_fill_manual( values = c( "blue", "red" ) ) 
```

#### Re-labeling & Bar plots
```{r}
capital.gain.median <-
  median(subset(adult, capital.gain != 0)$capital.gain)
adult$capital.gain <-
  cut(
    adult$capital.gain,
    breaks = c(-Inf, 0, capital.gain.median,+Inf),
    labels = c("Zero", "Low", "High")
  )
tab <- table(adult$capital.gain, adult$income)
barplot(tab,
        beside = TRUE,
        col = topo.colors(nrow(tab)),
        legend = rownames(tab))
```

### Capital-loss and income

#### Scatter Plot

```{r}

ggplot(adult, aes(x = income, y = capital.loss)) +
  geom_point()
```


# histogram of capital_loss
```{r}

ggplot(adult) + aes(x=as.numeric(capital.loss), group=income, fill=income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Income w.r.t Capital Loss')+ 
  scale_fill_manual( values = c( "blue", "red" ) ) 
```

#### Re-labeling & Bar plots
```{r}
capital.loss.median <-
  median(subset(adult, capital.loss != 0)$capital.loss)
adult$capital.loss <-
  cut(
    adult$capital.loss,
    breaks = c(-Inf, 0, capital.loss.median,+Inf),
    labels = c("Zero", "Low", "High")
  )
tab <- table(adult$capital.loss, adult$income)
barplot(tab,
        beside = TRUE,
        col = topo.colors(nrow(tab)),
        legend = rownames(tab))
```

Most of the observed values of capital gains and losses are accumulated at 0 for both income groups. 

In the re-labeled boxplot, the capital gain was high in the group with income higher than 50k, but the capital loss was also higher in the same group.

Therefore, with these insights, we conclude that both variable "capital gain" and "capital loss" are not in association with the target variable.

```{r}
adult$capital.gain <- NULL
adult$capital.loss <- NULL
```


### Income with hours per week

```{r}
ggplot(adult) + aes(x=as.numeric(hours.per.week), group=income, fill=income, position = "fill")+ 
  geom_histogram(bins=10, color='black') + ggtitle('Income w.r.t hours per week')+ 
  scale_fill_manual( values = c( "blue", "red" ) ) 
```

Most people work 30 to 40 hours per week


```{r}
tab <- table(adult$hours.per.week, adult$income)
tab
```

#### Barplot
```{r}
barplot(tab, beside=TRUE, col=topo.colors(nrow(tab)))
```

Median of the "hours per week" seems higher in the income group >50K. In the group with income >50K, it seems to choose a slightly more flexible working hours, but the variable does not seem to be directly associated to the target variable income.

# Normalized

```{r}
ggplot() +
  geom_histogram( data = adult,
            aes( x    = as.numeric( hours.per.week ),
                 fill = factor( income ) ),
            position = "fill" ) + 
  scale_x_discrete( "hours.per.week" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "income " ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) ) + 
  geom_histogram(bins=40, color='black')+ ggtitle('Income w.r.t hours per week')
```



### Native country


```{r}
summary(adult$native.country)
```


```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( native.country ),
                 fill = factor( income ) ),
            position = "stack" ) + 
  scale_x_discrete( "native.country" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "Income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) )+ ggtitle('Income w.r.t country') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```
 
 It can be seen that the total income from U.S is the extremly highest among selected countries.


```{r}
ggplot() +
  geom_bar( data = adult,
            aes( x    = factor( native.country ),
                 fill = factor( income ) ),
            position = "fill" ) + 
  scale_x_discrete( "native.country" ) + 
  scale_y_continuous( "income" ) + 
  guides( fill = guide_legend( title = "Income" ) ) + 
  scale_fill_manual( values = c( "blue", "red" ) )+ ggtitle('Income w.r.t country') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

 It can be seen that there is very week corelation between native country and income from the second graph. Native_country displays high skewness as most observations are from the United States. Also, distribution of income groups varies between native countries. Therefore, there is no dependency on target variable.


### Summary

 This assignment dealt with the adult dataset which is available in R liver. The adult dataset has 15 variables which were age, workclass, education, education_num, matrial_status, occupation, relationship, race, sex, caplital_gain, caplital_loss, hours.per.week, native_country, and missing variables. 


 This dataset was examined through exploratory data analysis(EDA). Before analysing the dataset, 3 variables which were education,and relationship were deleted for simplification. Through the EDA, the relationships between income and other variables were founded. The income of adults relates to the variable age, sex, workclass, education_num, race, matrial_status, occupation  and hours per week. Before middle age the income has positive correlation while after middle age it has negative correlation. Females earn less income than male. There are some significant workclass which earn more income than other work classes. 'Asian- Pac- islander’ and ‘White’ equally have the highest proportion of over 50k income earners. Longer education year leads to higher income. Marriage status influences income. Blue-Collar receive higher income compared to other occupations. People from the U.S have higher salaries.  However, the variable native.country,caplital_gain and caplital_loss  did not have any relationship with income. 


 With these relationships between income and variables above, it could be concluded that the EDA helps to interpret the data by  analysing and visualizing each variable from the data.   



