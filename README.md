# Model structure
This is an SEIS model stratified by age and risk: children (0-17), adults (18-64), seniors (65+), and high-risk adults (18-64). Waned/naive individuals start in S1, those with partially waned vaccinal immunity or a recent infection start in S2, and those who have been recently boosted or had 2+ recent infections start in S3. Immune escape moves individuals in S3 and S2 to S3e and S2e respectively at a rate of 1/365 days, with a median 35% reduction in protection against infection, but no reduction in protection against severe disease. 

<img width="993" alt="CompartmentalDiagram" src="https://github.com/user-attachments/assets/740e9320-2417-4611-b0a2-ed8e2dd57d34" />

Contacts are informed by the fourth phase of the Berkeley Interpersonal Contact Study [1]. Contact proportions for each age group are given below; 65+ are assumed to have half the total contacts of children and adults: 

Age Group | 0-17 contact proportion | 18-64 contact proportion | 65+ contact proportion | High risk 18-64 contact proportion
--- | --- | --- | --- |--- 
0-17 | 0.5 | 0.27 | 0.1 | 0.13 
--- | --- | --- | --- |--- 
18-64 | 0.35 | 0.42 | 0.1 | 0.13
--- | --- | --- | --- |--- 
65+ | 0.2 | 0.17 | 0.5 | 0.13
--- | --- | --- | --- |--- 
High risk 18-64 | 0.35 | 0.42 | 0.1 | 0.13 




# Seasonal Forcing
A cyclic cubic spline basis was used to capture changes in overall contacts and the risk of infection given contact during the calibration and projection periods. Six seasonal parameters were calibrated for each state; the basis for Illinois is shown below.

![Seasonality](https://github.com/user-attachments/assets/9fd602c3-bb5b-48c9-89f4-eadcbf0bfa1d)

[1] Feehan, D. M., & Mahmud, A. S. (2021). Quantifying population contact patterns in the United States during the COVID-19 pandemic. Nature communications, 12(1), 893.
