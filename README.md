# Hospital-Readmissions

An EDA and Predictive Modeling of Diabetic Patient Readmissions

Overview
My goals were twofold: first, to identify any interesting cohorts within patient groups that might increase the probability of readmission, and second, to create a machine learning model capable of identifying patients most likely to be readmitted.

This analysis is important because hospital readmissions negatively impact patient quality of life, place undue strain on medical staff, and can result in insurance and government supplementals denying payments if a patient is readmitted within 30 days of discharge. Moreover, with the cost of readmissions ranging from $10,900 to $15,200, these instances also present a significant financial burden to both patients and hospitals.

Data
Diabetes Hospital Info: This data includes 7500 diabetic patients' data for EDA and model training. It comprises 16 variables including a unique identifier, and consists of information gathered at the hospital such as the number of lab procedures and diagnosis descriptions.
Diabetes Medicine Info: This data includes 7500 diabetic patients' medicine information for EDA and model training. It comprises 23 variables such as whether or not the patient is taking a specific drug like Metformin.
Diabetes Patient Info: This data includes 7500 diabetic patients' demographic and insurance information for EDA and model training. This dataset contains the dependent variable readmitted_y which is a Boolean indicator of whether the patient was readmitted within 30 days of hospital discharge. Other variables include race, gender, age, weight, and how the patient paid for services.

