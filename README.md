# IMMCYC model structure
This is an SEIS model stratified by age and risk: children (0-17), adults (18-64), seniors (65+), and high-risk adults (18-64). Waned/naive individuals start in S1, those with partially waned vaccinal immunity or a recent infection start in S2, and those who have been recently boosted or had 2+ recent infections start in S3. Immune escape moves individuals in S3 and S2 to S3e and S2e respectively at a rate of 1/365 days, with a median 35% reduction in protection against infection, but no reduction in protection against severe disease. 

<img width="993" alt="CompartmentalDiagram" src="https://github.com/user-attachments/assets/740e9320-2417-4611-b0a2-ed8e2dd57d34" />

# Parameters

We assume a latent period of 3 days, recovery rate of 7 days, and a duration of immunity of 150 days. The infection hospitalization rate for waned/naive adults is assumed to be 0.225%, based on the midpoint of latest adult IHRs reported by [1]. High risk adults are assumed to have 4.5x this risk [2]. Contacts are informed by the fourth phase of the Berkeley Interpersonal Contact Study [3]. Contact proportions for each age group are given below; 65+ are assumed to have half the total contacts of children and adults: 

Age Group | 0-17 contact proportion | 18-64 contact proportion | 65+ contact proportion | High risk 18-64 contact proportion
--- | --- | --- | --- |--- 
0-17 | 0.5 | 0.27 | 0.1 | 0.13 
18-64 | 0.35 | 0.42 | 0.1 | 0.13
65+ | 0.2 | 0.17 | 0.5 | 0.13
High risk 18-64 | 0.35 | 0.42 | 0.1 | 0.13 

# Seasonal Forcing
A cyclic cubic spline basis was used to capture changes in overall contacts and the risk of infection given contact during the calibration and projection periods. Six seasonal parameters were calibrated for each state.

<img width="769" alt="Screenshot 2025-05-08 at 2 54 13 PM" src="https://github.com/user-attachments/assets/c8ffe6df-a528-4366-b3fd-4cef7f3faaac" />

# Calibration
Seasonal parameters (above), hospitalization fatality rate, and relative risks of hospitalization for 65+/children are calibrated using maximum likelihood. 

# References
[1] Ward, T., Fyles, M., Glaser, A., Paton, R. S., Ferguson, W., & Overton, C. E. (2024). The real-time infection hospitalisation and fatality risk across the COVID-19 pandemic in England. Nature Communications, 15(1), 4633.

[2] Ko, J. Y., Danielson, M. L., Town, M., Derado, G., Greenlund, K. J., Kirley, P. D., ... & Kim, L. (2021). Risk factors for coronavirus disease 2019 (COVID-19)–associated hospitalization: COVID-19–associated hospitalization surveillance network and behavioral risk factor surveillance system. Clinical Infectious Diseases, 72(11), e695-e703.

[3] Feehan, D. M., & Mahmud, A. S. (2021). Quantifying population contact patterns in the United States during the COVID-19 pandemic. Nature communications, 12(1), 893.

