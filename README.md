# Datathon Pet Insurance

  In the past two decades, the Australian pet insurance market has grown rapidly driven by a cultural shift towards treating pets as real family members. As costs for pets have risen due to medical care such as surgeries, diagnostics, vaccinations, and 
dental care, most pet owners have sought financial protection to deal with these issues. Therefore, pet insurance has become a desirable choice. Faced with such a competitive pet insurance market, how to make a success is a tough task. We aim to deliver
competitive premiums, maintain an optimal loss ratio, and ensure long-term sustainability in a crowded market.  This project, based on collected historical data, provides an introduction to the GLM pricing method for pet insurance and identifies
key factors that significantly impact pet insurance. Finally, it performs pricing estimation on the test set.

## Table of Contents
- [Installation](#installation)
- [Usage](#usage)


## Installation

1. **Clone the Repository**:
 git clone  https://github.com/zhaojiezhu123/Datathon
2. **Install Dependencies**:
For R projects, open R or RStudio and run:
   install.packages(c("boot", "dplyr", "tidyr"，"glmnet","forcats","gbm","lubridate","ggplot2","gridExtra","pscl","MASS"))
## Usage
1. **Data cleaning and processing**
![image](https://github.com/user-attachments/assets/117ac8bc-e779-4119-84ef-0dd391dff72e)


2.**EDA for the data**
  find significant features for pricing and also try to calculate claims paid by the company based on the policy condition and claim amount by the policyholder

Age:

![image](https://github.com/user-attachments/assets/8fa551a6-190d-410f-bb66-4724ecc9949b)

Breed name:

![image](https://github.com/user-attachments/assets/a270e1a9-29e6-4fe9-92d4-3e9c207787ee)

By incorporating a loading factor into the calculations, we can account for cases where a large portion of policy claim amounts may be lower than the values calculated (possibly due to exclusion clauses). This approach seeks a strong symmetrical relationship
to balance the calculation method from claim amount to claim paid. Using this method directly for estimation can ensure that the expected error is zero.

```r
earned_with_claim_info_cal <- earned_with_claim_info %>%
  group_by(exposure_id, condition_category) %>%
  arrange(claim_start_date) %>%
  
  # Step 1: Identify the occurrence (first, second, etc.)
  mutate(occurrence = row_number()) %>%
  mutate(paid = ifelse(occurrence == 1,
                      ifelse(total_claim_amount > nb_excess, 0.92 * (total_claim_amount - nb_excess / 1.1) * nb_contribution / 100, 0), 0)) %>%
  mutate(previous_paid = 0)

for (i in 2:max(earned_with_claim_info_cal$occurrence)) {
  earned_with_claim_info_cal <- earned_with_claim_info_cal %>%
    mutate(previous_paid = ifelse(occurrence == i, cummax(paid), previous_paid)) %>%
    mutate(paid = ifelse(occurrence == i,
                        ifelse(total_claim_amount > nb_excess,
                               ifelse(previous_paid > 0, 0.92 * total_claim_amount * nb_contribution / 100,
                                      0.92 * (total_claim_amount - nb_excess / 1.1) * nb_contribution / 100),
                               paid), paid))
}

earned_with_claim_info_cal$paid_erro <- earned_with_claim_info_cal$paid - earned_with_claim_info_cal$claim_paid
earned_with_claim_info_cal <- filter(earned_with_claim_info_cal, paid_erro != "NA")
summary(earned_with_claim_info_cal$paid_erro)
qqnorm(earned_with_claim_info_cal$paid_erro)
qqline(earned_with_claim_info_cal$paid_erro, col = "red")

# Delete the outlier
earned_with_claim_info_cal <- earned_with_claim_info_cal[earned_with_claim_info_cal$paid_erro < 2000,]
earned_with_claim_info_cal <- earned_with_claim_info_cal[earned_with_claim_info_cal$paid_erro > -2000,]
qqnorm(earned_with_claim_info_cal$paid_erro)
qqline(earned_with_claim_info_cal$paid_erro, col = "red")
```


![Screenshot 2024-11-06 005504](https://github.com/user-attachments/assets/863c178d-d6a8-4566-a572-7e48fcbfb88c)






3. **Frequency model (claim count)**:
 
This model aims to predict the number of claims within the same exposure unit.
we compared the Poisson, Negative Binomial, and lasso model and selected the lasso Poisson. Although the Poisson distribution is not a perfect fit for this data (as the mean is significantly smaller than the variance), due to the variable selection enabled by Lasso regularization for the breed_name variable, we achieve high accuracy with Lasso. Even though the overall data distribution aligns more closely with a Negative Binomial (NB) distribution, we still choose Lasso as our final target model.

![image](https://github.com/user-attachments/assets/4ac12fcd-be48-4816-9c60-d500c9e7ba7b)

 
4. **Severity model (Average claim amount)**:

The target variable is calculated as the total claim amount for the same exposure ID divided by the total number of claims. For the severity model, we use step-forward feature selection directly and then perform cross-validation (CV) on the resulting optimal model to evaluate its accuracy (MSE).

![Screenshot 2024-11-06 011639](https://github.com/user-attachments/assets/7b305ae6-ccb6-4299-ab25-d50be24ef786)

![Screenshot 2024-11-06 005711](https://github.com/user-attachments/assets/93aec786-e118-4cdb-b610-d400116e61ba)



5. **Final prediction**:

For the final prediction, we calculate the probabilities for different claim frequencies separately and then compute the claim paid based on the method from the second step. Due to the complexity of the calculation and the low likelihood of having more than three claims in a single month, we estimate by summing the expected values for 0, 1, and 2 claims only. Additionally, exploratory data analysis (EDA) revealed that a significant portion of pets claim repeatedly within the same month for the same reason. Therefore, we assume that for the case of 2 claims, both are for the same category and if the excess is met in the first claim, it does not need to be deducted from the second claim.

```{r}
lambda0<-predict(model_list_fre[[12]],newx=x_final_1,type="response")
E_p_0 <- dpois(0, lambda = lambda0)
E_p_1 <- dpois(1, lambda = lambda0)  
E_p_2 <- dpois(2, lambda = lambda0)  
set.seed(123)
random_amounts_1 <- rlnorm(nrow(forecast_data2), meanlog = log(mean(earned_with_claim_info3$avg_claim_amount)), sdlog = sd(log(earned_with_claim_info3$avg_claim_amount)))
random_amounts_2 <- rlnorm(nrow(forecast_data2), meanlog = log(mean(earned_with_claim_info3$avg_claim_amount)), sdlog = sd(log(earned_with_claim_info3$avg_claim_amount)))
forecast_data2$e_values<-0
for (i in 1:nrow(forecast_data2)) {
  forecast_data2$e_values[i] <- E_p_0 [i]*0+E_p_1 [i]*ifelse(random_amounts_1[i] > forecast_data2$nb_excess[i],0.92*(random_amounts_1[i] - forecast_data2$nb_excess[i] / 1.1) * forecast_data2$nb_contribution[i] / 100,0) + E_p_2 [i]*ifelse(random_amounts_1[i] > forecast_data2$nb_excess[i],0.92*(random_amounts_1[i] - forecast_data2$nb_excess[i] / 1.1) * forecast_data2$nb_contribution[i] / 100+0.92*random_amounts_2[i] * forecast_data2$nb_contribution[i] / 100,ifelse(random_amounts_2[i] > forecast_data2$nb_excess[i],0.92*(random_amounts_2[i] - forecast_data2$nb_excess[i] / 1.1) * forecast_data2$nb_contribution[i] / 100,0))
}
sum(lambda0*random_amounts_1)
sum(forecast_data2$e_values)
forecast_data<- forecast_data[!forecast_data$nb_breed_trait %in% c('siamese', 'forest cat', 'spotted cats', 'persian', 'burmese'), ]
merged_data <- forecast_data %>%
  mutate(Full_month_premium=forecast_data2$e_values)
write.csv(merged_data, "output_5305.csv", row.names = FALSE)
```








