library("Matrix")
library("arules")
library("ResourceSelection")
library(corrplot)
library(MASS)
library(pROC)
library(car)


data <-read.csv("final.csv")
write.csv(data,"final.csv")

nrow(data)

train_data<-data[1:60000,]
valid_data<-data[60001:70437,]


Donner.Model <- glm(readmitted ~ admission_source_id_9+diag3_level_15+metformin-pioglitazone+admission_source_id_13+acetohexamide+discharge_disposition_id_20+admission_source_id_11+diag3_level_8+rosiglitazone+tolbutamide+admission_source_id_25+admission_source_id_14+metformin-rosiglitazone+diag1_level_4+race_Asian+glimepiride+diag2_level_17+discharge_disposition_id_27+discharge_disposition_id_16+troglitazone+diag2_level_15+admission_source_id_17+gender+pioglitazone+miglitol+admission_source_id_22+diag3_level_6+acarbose+discharge_disposition_id_10+discharge_disposition_id_19+admission_source_id_4+diag3_level_14+admission_source_id_10+glipizide-metformin+glyburide-metformin+discharge_disposition_id_24+discharge_disposition_id_17+diag1_level_15+nateglinide+tolazamide+diag2_level_10+diag2_level_9+race_Hispanic+diag1_level_7+diag1_level_14+admission_source_id_6+diag2_level_6+diag2_level_14+diag1_level_13+chlorpropamide+race_AfricanAmerican+discharge_disposition_id_14+diag1_level_9+admission_source_id_1+A1Cresult+race_Other+diag3_level_3+discharge_disposition_id_25+diag2_level_5+diag1_level_10+admission_type_id_3+metformin+diag3_level_5+discharge_disposition_id_23+diag2_level_12+diag3_level_12+discharge_disposition_id_13+diag1_level_12+diag1_level+diag1_level_17+encounter_id+discharge_disposition_id_1+diag2_level_11+diag1_level_11+admission_source_id_2+discharge_disposition_id_8+admission_type_id_4+admission_source_id_5+discharge_disposition_id_7+diag2_level_3+diag1_level_5+diag2_level_7+discharge_disposition_id_4+admission_type_id_5+diag2_level+diag3_level_10+patient_nbr+discharge_disposition_id_9+glyburide+diag2_level_18+diag2_level_8+diag3_level_18+admission_type_id_1+diag3_level_11+discharge_disposition_id_6+discharge_disposition_id_12+diag3_level_17+admission_source_id_3+admission_source_id_20+discharge_disposition_id_18+diag3_level_7+number_outpatient+diag1_level_6+max_glu_serum+admission_source_id_7+glipizide+nummed+diag3_level_4+race_Caucasian+diag3_level_9+diag3_level_13+diag1_level_3+repaglinide+change+diag2_level_13+diag1_level_18+diag1_level_8+diag2_level_4+diag3_level+numchange+insulin+discharge_disposition_id_2+diabetesMed+number_emergency+discharge_disposition_id_15+num_lab_procedures+discharge_disposition_id_28+num_medications+number_diagnoses+age+discharge_disposition_id_5+time_in_hospital+discharge_disposition_id_3+service_utilization+discharge_disposition_id_22+number_inpatient+admission_source_id_8+num_procedures
                    
                    , data=data, family = binomial)


Diabetes.model5 <- glm(readmitted ~ diag2_level_17+discharge_disposition_id_24 +diag2_level_9+diag2_level_14+diag2_level_6+discharge_disposition_id_14+discharge_disposition_id_25+diag2_level_5+discharge_disposition_id_23+discharge_disposition_id_13+encounter_id+discharge_disposition_id_1+diag2_level_11+discharge_disposition_id_8+discharge_disposition_id_7+discharge_disposition_id_4+patient_nbr+diag2_level_8+discharge_disposition_id_6+discharge_disposition_id_18+number_outpatient+nummed+repaglinide+numchange+discharge_disposition_id_2+insulin+diabetesMed+number_emergency+discharge_disposition_id_15+num_lab_procedures+discharge_disposition_id_28+num_medications+number_diagnoses+age+discharge_disposition_id_5+discharge_disposition_id_3+service_utilization 
                       +  num_medications* num_procedures+
                         number_diagnoses*age+number_diagnoses*time_in_hospital, data=train_data, family = binomial)
summary(Diabetes.model5)
pchisq(35749, df = 59957, lower.tail = F)



library(ResourceSelection)
hoslem.test(train_data$readmitted, fitted(Diabetes.model5), g=10)






predicted <- predict(Diabetes.model5, newdata = train_data, type = "response")
predicted <- as.numeric(predicted>0.5)
table(predicted,train_data$readmitted)



predicted <- predict(Diabetes.model5, newdata = valid_data, type = "response")
predicted <- as.numeric(predicted>0.5)
table(predicted,valid_data$readmitted)


54305/(54305+43)
28/(28+5624)



library(car)
influenceIndexPlot(Diabetes.model4, id.n = 5)
cutoff1 <- 1
# # Cutoff  =  4/(n-k-1) 
cutoff2 <- 4/((nrow(data)-length(Diabetes.model3$coefficients)-2)) 
plot(cooks.distance(Diabetes.model3)) # Plot Cooks D for every obs
abline(h = cutoff2, lty = 2) # Add cutoff line

library(car)

influencePlot(Diabetes.model4, identify = "auto", main="Influence Plot")






library(pROC)
predicted <- predict(Diabetes.model4, newdata = data, type ="response")
roccurve <- roc(data$readmitted ~ predicted) 
                     plot(roccurve)
                     
                     auc(roccurve)
