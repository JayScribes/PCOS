
---
  ![](https://www.mayoclinic.org/-/media/kcms/gbs/patient-consumer/images/2013/08/26/10/42/ds00423_im04151_mcdc7_polycystic_ovarythu_jpg.jpg)


## Background

Polycystic ovary syndrome (PCOS) is a hormonal disorder common among women of reproductive age. Women with PCOS may have infrequent or prolonged menstrual periods or excess male hormone (androgen) levels. The ovaries may develop numerous small collections of fluid (follicles) and fail to regularly release eggs.

This dataset was compiled by Prasoon Kottarathi from data across 10 different hospitals in Kerala, India. It can be found on kaggle by following this link:
  https://www.kaggle.com/datasets/prasoonkottarathil/polycystic-ovary-syndrome-pcos


### Loading Up Libraries
```{r Libraries}
library(skimr)
library(ranger)
library(tidyverse)
library(tidymodels)
library(GGally)
options(scipen=999)
library(doParallel)
library(themis)
library(DescTools)
library(baguette)
```

### Loading the Data

```{r Loading Data, warning = FALSE}
PCOS <- read_csv("C:/Users/Dell/Desktop/Data Projects/Portfolio/ML Projects/PCOS/PCOS1.csv", 
                 col_types = cols(Age = col_number(), 
                                  Weight = col_number(), Height = col_number(), 
                                  BMI = col_number(), Pulse = col_number(), 
                                  BreathingRate = col_number(), Hb = col_number(), 
                                  Cyclelength = col_number(), Married = col_number(), 
                                  Abortions = col_number(), `b-HCG` = col_number(), 
                                  `b-HCG-2` = col_number(), FSH = col_number(), 
                                  LH = col_number(), `FSH/LH` = col_number(), 
                                  Hip = col_number(), Waist = col_number(), 
                                  WaistHipRation = col_number(), TSH = col_number(), 
                                  AMH = col_number(), PRL = col_number(), 
                                  VitD3 = col_number(), PRG = col_number(), 
                                  RBS = col_number(), DiastolicBP = col_number(), 
                                  LFolNum = col_number(), RFolNum = col_number(), 
                                  LFolSize = col_number(), RFolSize = col_number(), 
                                  Endometrium = col_number()))
```

### Exploratory Data Analysis and Cleaning

```{r}
skim(PCOS)
```

Skimming the data for missing values seems to have returned a nearly fully complete data set, let's start creating pair plots to look at interactions between variables and PCOS. However, many variables
seem to have extreme values, which may obfuscate effects and interactions.

### Winsorizing Extreme Values in Key Variables

```{r}
LH_winsorized <-  Winsorize(PCOS$LH, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$LH <- LH_winsorized ## adding winsorized information into the data frame
remove(LH_winsorized)

FSH_winsorized <-  Winsorize(PCOS$FSH, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$FSH <- FSH_winsorized ## adding winsorized information into the data frame
remove(FSH_winsorized)

bb_win <-  Winsorize(PCOS$`b-HCG`, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$`b-HCG` <- bb_win ## adding winsorized information into the data frame
remove(bb_win)

bb_win_2 <-  Winsorize(PCOS$`b-HCG-2`, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$`b-HCG` <- bb_win_2 ## adding winsorized information into the data frame
remove(bb_win_2)

fl_win <-  Winsorize(PCOS$`FSH/LH`, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$`FSH/LH` <- fl_win ## adding winsorized information into the data frame
remove(fl_win)

TSH_winsorized <-  Winsorize(PCOS$TSH, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$TSH <- TSH_winsorized ## adding winsorized information into the data frame
remove(TSH_winsorized)

AMH_winsorized <-  Winsorize(PCOS$AMH, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = TRUE, type =1)
PCOS$AMH <- AMH_winsorized ## adding winsorized information into the data frame
remove(AMH_winsorized)

PCOS$AMH[is.na(PCOS$AMH)] <- mean(PCOS$AMH, na.rm = TRUE)

PRL_winsorized <-  Winsorize(PCOS$PRL, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$PRL <- PRL_winsorized ## adding winsorized information into the data frame
remove(PRL_winsorized)

VitD3_winsorized <-  Winsorize(PCOS$VitD3, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$VitD3 <- VitD3_winsorized ## adding winsorized information into the data frame
remove(VitD3_winsorized)

PRG_winsorized <-  Winsorize(PCOS$PRG, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$PRG <- PRG_winsorized ## adding winsorized information into the data frame
remove(PRG_winsorized)

RBS_winsorized <-  Winsorize(PCOS$RBS, minval = NULL, maxval = NULL, probs = c(0.04,0.96), na.rm = FALSE, type =1)
PCOS$RBS <- RBS_winsorized ## adding winsorized information into the data frame
remove(RBS_winsorized)
```


### Pairs Plots for Key Numerical Variables
```{r, warning = FALSE}
PCOS$PCOS <- as.factor(PCOS$PCOS)

PCOS %>% 
  select(PCOS, Age, BMI, Pulse, BreathingRate, Hb, Cyclelength, Married) %>% 
  ggpairs(columns = 2:8, aes(color = PCOS, alpha = 0.5))

PCOS %>% 
  select(PCOS, Abortions, 'b-HCG', 'b-HCG-2', FSH, LH, 'FSH/LH') %>% 
  ggpairs(columns = 2:7, aes(color = PCOS, alpha = 0.5))


PCOS %>% 
  select(PCOS, TSH, AMH, PRL, VitD3, PRG, RBS) %>% 
  ggpairs(columns = 2:7, aes(color = PCOS, alpha = 0.5))

PCOS %>% 
  select(PCOS, SystolicBP,DiastolicBP,LFolNum,RFolNum,LFolSize,RFolSize,Endometrium) %>% 
  ggpairs(columns = 2:8, aes(color = PCOS, alpha = 0.5))
```

### Pairs Plots for Key Categorical Variables

```{r, warning = FALSE}
PCOS %>% 
  select(PCOS, WeightGain, Hirsutism, SkinDarkening, HairLoss, Pimples, FastFood, Exercise) %>% 
  pivot_longer(WeightGain:Exercise) %>% 
  ggplot(aes(y = value, fill = PCOS))+
  geom_bar(position = "fill")+
  facet_wrap(vars(name), scale = "free")+
  labs(x = NULL, y= NULL, fil = NULL)
```


## Preparing a Dataset for Modeling 
```{r}
PCOS.Data <- PCOS %>% 
  select(PCOS, Age, BMI, Cyclelength, 'b-HCG', 'b-HCG-2', FSH, LH, 'FSH/LH', AMH, PRL, VitD3, RBS, LFolNum, RFolNum, LFolSize, RFolSize, FastFood, HairLoss, Hirsutism, Pimples, SkinDarkening, WeightGain)

PCOS.Data <- edit(PCOS.Data)

skim(PCOS.Data)
```

### Splitting Data, Creating Model, and Fitting 


```{r Splitting Data}
set.seed(123)
data_split <- initial_split(PCOS.Data)
data_train <- training(data_split)
data_test <- testing(data_split)
```

```{r Folds}
PCOS_folds <- vfold_cv(data_train, v = 5, strata = PCOS)
PCOS_folds

PCOS_metrics <- metric_set(accuracy, sensitivity, specificity, recall)
```

```{r Recipe}
PCOS_recipe <- recipe(PCOS ~ ., data = data_train) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), - all_outcomes())

PCOS_recipe
```

```{r Generating Model}
bag_spec <-
  bag_tree(min_n = 10) %>% 
  set_engine("rpart", times = 25) %>% 
  set_mode("classification")
```

```{r Fitting Model}
imb_wf <- workflow() %>% 
  add_recipe(PCOS_recipe) %>% 
  add_model(bag_spec)

var.imp.t <- fit(imb_wf, data = data_train)
```

### Fitting Non-Balanced Resamples

```{r}
doParallel::registerDoParallel()
set.seed(123)
imb_results <- fit_resamples(
  imb_wf,
  resamples = PCOS_folds,
  metrics = PCOS_metrics
)

collect_metrics(imb_results)
```
 
### Fitting Balanced Resamples

```{r}
bal_rec <- PCOS_recipe %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_smote(PCOS)


bal_wf <- workflow() %>% 
  add_recipe(bal_rec) %>% 
  add_model(bag_spec)

set.seed(123)
bal_results <- fit_resamples(
  bal_wf,
  resamples = PCOS_folds,
  metrics = PCOS_metrics,
  control = control_resamples(save_pred = TRUE))


collect_metrics(bal_results)

bal_results %>% 
  conf_mat_resampled()
```

### Fitting Model onto Test Data

```{r}
PCOS_final <- bal_wf %>% 
  last_fit(data_split)

collect_metrics(PCOS_final)


collect_predictions(PCOS_final) %>% 
  conf_mat(PCOS, .pred_class)
```

### Variable Importance Table - Test Fitting
```{r}
var.imp.t
```

### ROC Curve - Test Fitting

```{r}
PCOS_final %>% 
  collect_predictions() %>% 
  group_by(id) %>% 
  roc_curve(PCOS, .pred_0) %>% 
  ggplot(aes(1 - specificity, sensitivity, color = id))+
  geom_abline(lty = 2, color = "gray90", size = 1.5)+
  geom_path(show.legend = FALSE, alpha = 0.6, size =1.2)+
  coord_equal()+theme_classic()
```


### Confusion Matrix Graph - Test Fitting

```{r}
collect_predictions(PCOS_final) %>% 
  conf_mat(PCOS, .pred_class) %>% 
  autoplot(cm, type = "heatmap")
```
 
