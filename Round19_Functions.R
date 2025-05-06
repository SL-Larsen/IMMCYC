library(deSolve)
library(tidyverse)
library(mgcv)

#### SEIR model ####

seir <- function(t, y, pars){
  
  # Parameters
  epsilon <- pars["epsilon"] # latent period
  omega <- pars["omega"] # waning of immunity
  gamma <- pars["gamma"] # recovery rate
  
  contact_cc <- pars["contact_cc"] # child contacts child
  contact_ca <- pars["contact_ca"] # child contacts adult
  contact_co <- pars["contact_co"] # child contacts old
  contact_chr <- pars["contact_chr"] # child contacts high risk
  
  
  contact_ac <- pars["contact_ac"] # adult contacts child
  contact_aa <- pars["contact_aa"] # adult contacts adult
  contact_ao <- pars["contact_ao"] # adult contacts old
  contact_ahr <- pars["contact_ahr"] # adult contacts high risk
  
  contact_oc <- pars["contact_oc"] # old contacts child
  contact_oa <- pars["contact_oa"] # old contacts adult
  contact_oo <- pars["contact_oo"] # old contacts old
  contact_ohr <- pars["contact_ohr"] # old contacts high risk
  
  contact_hrc <- pars["contact_hrc"] # old contacts child
  contact_hra <- pars["contact_hra"] # old contacts adult
  contact_hro <- pars["contact_hro"] # old contacts old
  contact_hrhr <- pars["contact_hrhr"] # old contacts high risk
  
  eta <- pars["eta"] # risk of hospitalization given infection
  alpha <- pars["alpha"] # risk of death given hospitalization
  
  VE_i <- pars["VE_i"] # booster efficacy against infection
  VE_h <- pars["VE_h"] # booster efficacy against severe disease
  
  wane_VE_i <- pars["wane_VE_i"] ## booster efficacy against infection, post waning
  wane_VE_h <- pars["wane_VE_h"] ## booster efficacy against severe disease, post waning
  
  risk_old_i <- pars["risk_old_i"]
  risk_old_h <- pars["risk_old_h"]

  risk_child_i <- pars["risk_child_i"]
  risk_child_h <- pars["risk_child_h"]
  
  risk_hr_i <- pars["risk_hr_i"]
  risk_hr_h <- pars["risk_hr_i"]
  
  immesc <- pars["immesc"]
  immesc_prop <- pars["immesc_prop"]

  # State variables
  
  # Children
  # Waned immunity
  S_1_child <- y[1]
  E_1_child <- y[2]
  I_1_child <- y[3]
  H_1_child <- y[4]
  D_1_child <- y[5]
  
  # Recent immunity
  S_2_child <- y[6]
  E_2_child <- y[7]
  I_2_child <- y[8]
  H_2_child <- y[9]
  D_2_child <- y[10]
  
  # Recent immunity with escape
  S_2E_child <- y[11]
  E_2E_child <- y[12]
  I_2E_child <- y[13]
  H_2E_child <- y[14]
  D_2E_child <- y[15]
  
  # Partial immunity
  S_3_child <- y[16]
  E_3_child <- y[17]
  I_3_child <- y[18]
  H_3_child <- y[19]
  D_3_child <- y[20]
  
  # Partial immunity with escape
  S_3E_child <- y[21]
  E_3E_child <- y[22]
  I_3E_child <- y[23]
  H_3E_child <- y[24]
  D_3E_child <- y[25]
  
  # Cumulative infections
  Inf_1_child <- y[26]
  Inf_2_child <- y[27]
  Inf_3_child <- y[28]
  
  # Cumulative hospitalizations
  Hosp_1_child <- y[29]
  Hosp_2_child <- y[30]
  Hosp_3_child <- y[31]
  
  
  # Adults
  # Waned immunity
  S_1_adult <- y[32]
  E_1_adult <- y[33]
  I_1_adult <- y[34]
  H_1_adult <- y[35]
  D_1_adult <- y[36]
  
  # Recent immunity
  S_2_adult <- y[37]
  E_2_adult <- y[38]
  I_2_adult <- y[39]
  H_2_adult <- y[40]
  D_2_adult <- y[41]
  
  # Recent immunity - escape
  S_2E_adult <- y[42]
  E_2E_adult <- y[43]
  I_2E_adult <- y[44]
  H_2E_adult <- y[45]
  D_2E_adult <- y[46]
  
  # Partial immunity
  S_3_adult <- y[47]
  E_3_adult <- y[48]
  I_3_adult <- y[49]
  H_3_adult <- y[50]
  D_3_adult <- y[51]
  
  # Partial immunity - escape
  S_3E_adult <- y[52]
  E_3E_adult <- y[53]
  I_3E_adult <- y[54]
  H_3E_adult <- y[55]
  D_3E_adult <- y[56]
  
  # Cumulative infections
  Inf_1_adult <- y[57]
  Inf_2_adult <- y[58]
  Inf_3_adult <- y[59]
  
  # Cumulative hospitalizations
  Hosp_1_adult <- y[60]
  Hosp_2_adult <- y[61]
  Hosp_3_adult <- y[62]
  
  # Elderly
  # Waned immunity
  S_1_old <- y[63]
  E_1_old <- y[64]
  I_1_old <- y[65]
  H_1_old <- y[66]
  D_1_old <- y[67]
  
  # Recent immunity
  S_2_old <- y[68]
  E_2_old <- y[69]
  I_2_old <- y[70]
  H_2_old <- y[71]
  D_2_old <- y[72]
  
  # Recent immunity - escape
  S_2E_old <- y[73]
  E_2E_old <- y[74]
  I_2E_old <- y[75]
  H_2E_old <- y[76]
  D_2E_old <- y[77]
  
  # Partial immunity
  S_3_old <- y[78]
  E_3_old <- y[79]
  I_3_old <- y[80]
  H_3_old <- y[81]
  D_3_old <- y[82]  
  
  # Partial immunity - escape
  S_3E_old <- y[83]
  E_3E_old <- y[84]
  I_3E_old <- y[85]
  H_3E_old <- y[86]
  D_3E_old <- y[87]  
  
  # Cumulative infections
  Inf_1_old <- y[88]
  Inf_2_old <- y[89]
  Inf_3_old <- y[90]
  
  # Cumulative hospitalizations
  Hosp_1_old <- y[91]
  Hosp_2_old <- y[92]
  Hosp_3_old <- y[93]
  
  # High risk
  # Waned immunity
  S_1_hr <- y[94]
  E_1_hr <- y[95]
  I_1_hr <- y[96]
  H_1_hr <- y[97]
  D_1_hr <- y[98]
  
  # Recent immunity
  S_2_hr <- y[99]
  E_2_hr <- y[100]
  I_2_hr <- y[101]
  H_2_hr <- y[102]
  D_2_hr <- y[103]
  
  # Recent immunity - escape
  S_2E_hr <- y[104]
  E_2E_hr <- y[105]
  I_2E_hr <- y[106]
  H_2E_hr <- y[107]
  D_2E_hr <- y[108]
  
  # Partial immunity
  S_3_hr <- y[109]
  E_3_hr <- y[110]
  I_3_hr <- y[111]
  H_3_hr <- y[112]
  D_3_hr <- y[113]  
  
  # Partial immunity - escape
  S_3E_hr <- y[114]
  E_3E_hr <- y[115]
  I_3E_hr <- y[116]
  H_3E_hr <- y[117]
  D_3E_hr <- y[118]  
  
  # Cumulative infections
  Inf_1_hr <- y[119]
  Inf_2_hr <- y[120]
  Inf_3_hr <- y[121]
  
  # Cumulative hospitalizations
  Hosp_1_hr <- y[122]
  Hosp_2_hr <- y[123]
  Hosp_3_hr <- y[124]
  
  
  ## force of infection
  
  sumI_child <- I_1_child + I_2_child + I_3_child + I_2E_child + I_3E_child + H_1_child + H_2_child + H_3_child + H_2E_child + H_3E_child
  sumI_adult <- I_1_adult + I_2_adult + I_3_adult + I_2E_adult + I_3E_adult + H_1_adult + H_2_adult + H_3_adult + H_2E_adult + H_3E_adult
  sumI_old <- I_1_old + I_2_old + I_3_old + I_2E_old + I_3E_old + H_1_old + H_2_old + H_3_old + H_2E_old + H_3E_old
  sumI_hr <- I_1_hr + I_2_hr + I_3_hr + I_2E_hr + I_3E_hr + H_1_hr + H_2_hr + H_3_hr + H_2E_hr + H_3E_hr
  
  pop_child <- S_1_child + E_1_child + I_1_child + H_1_child + 
    S_2_child + E_2_child + I_2_child + H_2_child + 
    S_3_child + E_3_child + I_3_child + H_3_child + 
    S_2E_child + E_2E_child + I_2E_child + H_2E_child + 
    S_3E_child + E_3E_child + I_3E_child + H_3E_child
  pop_adult <- S_1_adult + E_1_adult + I_1_adult + H_1_adult + 
    S_2_adult + E_2_adult + I_2_adult + H_2_adult + 
    S_3_adult + E_3_adult + I_3_adult + H_3_adult + 
    S_2E_adult + E_2E_adult + I_2E_adult + H_2E_adult + 
    S_3E_adult + E_3E_adult + I_3E_adult + H_3E_adult
  pop_old <- S_1_old + E_1_old + I_1_old + H_1_old + 
    S_2_old + E_2_old + I_2_old + H_2_old + 
    S_3_old + E_3_old + I_3_old + H_3_old + 
    S_2E_old + E_2E_old + I_2E_old + H_2E_old + 
    S_3E_old + E_3E_old + I_3E_old + H_3E_old
  pop_hr <- S_1_hr + E_1_hr + I_1_hr + H_1_hr + 
    S_2_hr + E_2_hr + I_2_hr + H_2_hr + 
    S_3_hr + E_3_hr + I_3_hr + H_3_hr + 
    S_2E_hr + E_2E_hr + I_2E_hr + H_2E_hr + 
    S_3E_hr + E_3E_hr + I_3E_hr + H_3E_hr

  
  sumI <- sumI_child + sumI_adult + sumI_old + sumI_hr
  pop <- pop_child + pop_adult + pop_old + pop_hr
  
  lambda_child <- risk_child_i*mu(t)*(contact_cc*sumI_child/pop_child + contact_ca*sumI_adult/pop_adult + contact_co*sumI_old/pop_old + contact_chr*sumI_hr/pop_hr)
  lambda_adult <- mu(t)*(contact_ac*sumI_child/pop_child + contact_aa*sumI_adult/pop_adult + contact_ao*sumI_old/pop_old + contact_ahr*sumI_hr/pop_hr)
  lambda_old <- risk_old_i*mu(t)*(contact_oc*sumI_child/pop_child + contact_oa*sumI_adult/pop_adult + contact_oo*sumI_old/pop_old + contact_ohr*sumI_hr/pop_hr)
  lambda_hr <- risk_hr_i*mu(t)*(contact_hrc*sumI_child/pop_child + contact_hra*sumI_adult/pop_adult + contact_hro*sumI_old/pop_old + contact_hrhr*sumI_hr/pop_hr)

  
  ## Vaccination rates
  
  vacc_child_rate <- min(vacc_child(t)*pop_child/(S_1_child + S_3_child + S_3E_child), 1)
  vacc_adult_rate <- min(vacc_adult(t)*pop_adult/(S_1_adult + S_3_adult + S_3E_adult),1)
  vacc_hr_rate <- min(vacc_hr(t)*pop_hr/(S_1_hr + S_3_hr + S_3E_hr),1)
  vacc_old_rate <- min(vacc_old(t)*pop_old/(S_1_old + S_3_old + S_3E_old),1)
  
  
  
  ## Updates
  ## Children
  ## Waned
  dS_1_child <- - lambda_child*S_1_child + omega*S_3_child + omega*S_3E_child - vacc_child_rate*S_1_child
  dE_1_child <- + lambda_child*S_1_child - epsilon*E_1_child 
  dI_1_child <- + (1-eta*risk_child_h)*epsilon*E_1_child - gamma*I_1_child
  dH_1_child <- + eta*risk_child_h*epsilon*E_1_child - gamma*H_1_child
  dD_1_child <- + alpha*gamma*H_1_child
  
  ## Boosted
  dS_2_child <- - (1-(1-VE_i))*lambda_child*S_2_child - omega*S_2_child + vacc_child_rate*S_1_child + vacc_child_rate*S_3_child + 
    vacc_child_rate*S_3E_child + (1-alpha)*gamma*H_3_child + gamma*I_3_child + (1-alpha)*gamma*H_3E_child + 
    gamma*I_3E_child + (1-alpha)*gamma*H_2_child + gamma*I_2_child + gamma*I_2E_child + (1-alpha)*gamma*H_2E_child - immesc*S_2_child
  dE_2_child <- + (1-(1-VE_i))*lambda_child*S_2_child - epsilon*E_2_child
  dI_2_child <- + (1-VE_h*eta*risk_child_h)*epsilon*E_2_child - gamma*I_2_child
  dH_2_child <- + VE_h*eta*risk_child_h*epsilon*E_2_child - gamma*H_2_child
  dD_2_child <- + alpha*gamma*H_2_child
  
  ## Boosted, immune escape
  dS_2E_child <- - (1-(1-VE_i)*immesc_prop)*lambda_child*S_2E_child - omega*S_2E_child + immesc*S_2_child
  dE_2E_child <- + (1-(1-VE_i)*immesc_prop)*lambda_child*S_2E_child - epsilon*E_2E_child
  dI_2E_child <- + (1-VE_h*eta*risk_child_h)*epsilon*E_2E_child - gamma*I_2E_child
  dH_2E_child <- + VE_h*eta*risk_child_h*epsilon*E_2E_child - gamma*H_2E_child
  dD_2E_child <- + alpha*gamma*H_2E_child
  
  ## Partial
  dS_3_child <- - (1-(1-wane_VE_i))*lambda_child*S_3_child - omega*S_3_child + omega*S_2_child + (1-alpha)*gamma*H_1_child +
    gamma*I_1_child - vacc_child_rate*S_3_child - immesc*S_3_child
  dE_3_child <- + (1-(1-wane_VE_i))*lambda_child*S_3_child - epsilon*E_3_child
  dI_3_child <- + (1-wane_VE_h*eta*risk_child_h)*epsilon*E_3_child - gamma*I_3_child
  dH_3_child <- + wane_VE_h*eta*risk_child_h*epsilon*E_3_child - gamma*H_3_child
  dD_3_child <- + alpha*gamma*H_3_child
  
  ## Partial, immune escape
  dS_3E_child <- - (1-(1-wane_VE_i)*immesc_prop)*lambda_child*S_3E_child - omega*S_3E_child + omega*S_2E_child + immesc*S_3_child - vacc_child_rate*S_3E_child
  dE_3E_child <- + (1-(1-wane_VE_i)*immesc_prop)*lambda_child*S_3E_child - epsilon*E_3E_child
  dI_3E_child <- + (1-wane_VE_h*eta*risk_child_h)*epsilon*E_3E_child - gamma*I_3E_child
  dH_3E_child <- + wane_VE_h*eta*risk_child_h*epsilon*E_3E_child - gamma*H_3E_child
  dD_3E_child <- + alpha*gamma*H_3E_child
  
  dInf_1_child <- epsilon*E_1_child
  dInf_2_child <- epsilon*E_2_child + epsilon*E_2E_child
  dInf_3_child <- epsilon*E_3_child + epsilon*E_3E_child
  
  dHosp_1_child <- eta*risk_child_h*epsilon*E_1_child
  dHosp_2_child <- VE_h*eta*risk_child_h*epsilon*E_2_child + VE_h*eta*risk_child_h*epsilon*E_2E_child
  dHosp_3_child <- wane_VE_h*eta*risk_child_h*epsilon*E_3_child + wane_VE_h*eta*risk_child_h*epsilon*E_3E_child
  
  
  ## Adults
  ## Waned
  dS_1_adult <- - lambda_adult*S_1_adult + omega*S_3_adult + omega*S_3E_adult - vacc_adult_rate*S_1_adult
  dE_1_adult <- + lambda_adult*S_1_adult - epsilon*E_1_adult 
  dI_1_adult <- + (1-eta)*epsilon*E_1_adult - gamma*I_1_adult
  dH_1_adult <- + eta*epsilon*E_1_adult - gamma*H_1_adult
  dD_1_adult <- + alpha*gamma*H_1_adult
  
  ## Boosted
  dS_2_adult <- - (1-(1-VE_i))*lambda_adult*S_2_adult - omega*S_2_adult + vacc_adult_rate*S_1_adult + vacc_adult_rate*S_3_adult + 
    vacc_adult_rate*S_3E_adult + (1-alpha)*gamma*H_3_adult + gamma*I_3_adult + (1-alpha)*gamma*H_3E_adult + 
    gamma*I_3E_adult + (1-alpha)*gamma*H_2_adult + gamma*I_2_adult + gamma*I_2E_adult + (1-alpha)*gamma*H_2E_adult - immesc*S_2_adult
  dE_2_adult <- + (1-(1-VE_i))*lambda_adult*S_2_adult - epsilon*E_2_adult
  dI_2_adult <- + (1-VE_h*eta)*epsilon*E_2_adult - gamma*I_2_adult
  dH_2_adult <- + VE_h*eta*epsilon*E_2_adult - gamma*H_2_adult
  dD_2_adult <- + alpha*gamma*H_2_adult
  
  ## Boosted, immune escape
  dS_2E_adult <- - (1-(1-VE_i)*immesc_prop)*lambda_adult*S_2E_adult - omega*S_2E_adult + immesc*S_2_adult
  dE_2E_adult <- + (1-(1-VE_i)*immesc_prop)*lambda_adult*S_2E_adult - epsilon*E_2E_adult
  dI_2E_adult <- + (1-VE_h*eta)*epsilon*E_2E_adult - gamma*I_2E_adult
  dH_2E_adult <- + VE_h*eta*epsilon*E_2E_adult - gamma*H_2E_adult
  dD_2E_adult <- + alpha*gamma*H_2E_adult
  
  ## Partial
  dS_3_adult <- - (1-(1-wane_VE_i))*lambda_adult*S_3_adult - omega*S_3_adult + omega*S_2_adult + (1-alpha)*gamma*H_1_adult +
    gamma*I_1_adult - vacc_adult_rate*S_3_adult - immesc*S_3_adult
  dE_3_adult <- + (1-(1-wane_VE_i))*lambda_adult*S_3_adult - epsilon*E_3_adult
  dI_3_adult <- + (1-wane_VE_h*eta)*epsilon*E_3_adult - gamma*I_3_adult
  dH_3_adult <- + wane_VE_h*eta*epsilon*E_3_adult - gamma*H_3_adult
  dD_3_adult <- + alpha*gamma*H_3_adult
  
  ## Partial, immune escape
  dS_3E_adult <- - (1-(1-wane_VE_i)*immesc_prop)*lambda_adult*S_3E_adult - omega*S_3E_adult + omega*S_2E_adult + immesc*S_3_adult - vacc_adult_rate*S_3E_adult
  dE_3E_adult <- + (1-(1-wane_VE_i)*immesc_prop)*lambda_adult*S_3E_adult - epsilon*E_3E_adult
  dI_3E_adult <- + (1-wane_VE_h*eta)*epsilon*E_3E_adult - gamma*I_3E_adult
  dH_3E_adult <- + wane_VE_h*eta*epsilon*E_3E_adult - gamma*H_3E_adult
  dD_3E_adult <- + alpha*gamma*H_3E_adult
  
  dInf_1_adult <- epsilon*E_1_adult
  dInf_2_adult <- epsilon*E_2_adult + epsilon*E_2E_adult
  dInf_3_adult <- epsilon*E_3_adult + epsilon*E_3E_adult
  
  dHosp_1_adult <- eta*epsilon*E_1_adult
  dHosp_2_adult <- VE_h*eta*epsilon*E_2_adult + VE_h*eta*epsilon*E_2E_adult
  dHosp_3_adult <- wane_VE_h*eta*epsilon*E_3_adult + wane_VE_h*eta*epsilon*E_3E_adult
  
  ## Old
  ## Waned
  dS_1_old <- - lambda_old*S_1_old + omega*S_3_old + omega*S_3E_old - vacc_old_rate*S_1_old
  dE_1_old <- + lambda_old*S_1_old - epsilon*E_1_old 
  dI_1_old <- + (1-eta*risk_old_h)*epsilon*E_1_old - gamma*I_1_old
  dH_1_old <- + eta*risk_old_h*epsilon*E_1_old - gamma*H_1_old
  dD_1_old <- + alpha*gamma*H_1_old
  
  ## Boosted
  dS_2_old <- - (1-(1-VE_i))*lambda_old*S_2_old - omega*S_2_old + vacc_old_rate*S_1_old + vacc_old_rate*S_3_old + 
    vacc_old_rate*S_3E_old + (1-alpha)*gamma*H_3_old + gamma*I_3_old + (1-alpha)*gamma*H_3E_old + 
    gamma*I_3E_old + (1-alpha)*gamma*H_2_old + gamma*I_2_old + gamma*I_2E_old + (1-alpha)*gamma*H_2E_old - immesc*S_2_old
  dE_2_old <- + (1-(1-VE_i))*lambda_old*S_2_old - epsilon*E_2_old
  dI_2_old <- + (1-VE_h*eta*risk_old_h)*epsilon*E_2_old - gamma*I_2_old
  dH_2_old <- + VE_h*eta*risk_old_h*epsilon*E_2_old - gamma*H_2_old
  dD_2_old <- + alpha*gamma*H_2_old
  
  ## Boosted, immune escape
  dS_2E_old <- - (1-(1-VE_i)*immesc_prop)*lambda_old*S_2E_old - omega*S_2E_old + immesc*S_2_old
  dE_2E_old <- + (1-(1-VE_i)*immesc_prop)*lambda_old*S_2E_old - epsilon*E_2E_old
  dI_2E_old <- + (1-VE_h*eta*risk_old_h)*epsilon*E_2E_old - gamma*I_2E_old
  dH_2E_old <- + VE_h*eta*risk_old_h*epsilon*E_2E_old - gamma*H_2E_old
  dD_2E_old <- + alpha*gamma*H_2E_old
  
  ## Partial
  dS_3_old <- - (1-(1-wane_VE_i))*lambda_old*S_3_old - omega*S_3_old + omega*S_2_old + (1-alpha)*gamma*H_1_old +
    gamma*I_1_old - vacc_old_rate*S_3_old - immesc*S_3_old
  dE_3_old <- + (1-(1-wane_VE_i))*lambda_old*S_3_old - epsilon*E_3_old
  dI_3_old <- + (1-wane_VE_h*eta*risk_old_h)*epsilon*E_3_old - gamma*I_3_old
  dH_3_old <- + wane_VE_h*eta*risk_old_h*epsilon*E_3_old - gamma*H_3_old
  dD_3_old <- + alpha*gamma*H_3_old
  
  ## Partial, immune escape
  dS_3E_old <- - (1-(1-wane_VE_i)*immesc_prop)*lambda_old*S_3E_old - omega*S_3E_old + omega*S_2E_old + immesc*S_3_old - vacc_old_rate*S_3E_old
  dE_3E_old <- + (1-(1-wane_VE_i)*immesc_prop)*lambda_old*S_3E_old - epsilon*E_3E_old
  dI_3E_old <- + (1-wane_VE_h*eta*risk_old_h)*epsilon*E_3E_old - gamma*I_3E_old
  dH_3E_old <- + wane_VE_h*eta*risk_old_h*epsilon*E_3E_old - gamma*H_3E_old
  dD_3E_old <- + alpha*gamma*H_3E_old
  
  dInf_1_old <- epsilon*E_1_old
  dInf_2_old <- epsilon*E_2_old + epsilon*E_2E_old
  dInf_3_old <- epsilon*E_3_old + epsilon*E_3E_old
  
  dHosp_1_old <- eta*risk_old_h*epsilon*E_1_old
  dHosp_2_old <- VE_h*eta*risk_old_h*epsilon*E_2_old + VE_h*eta*risk_old_h*epsilon*E_2E_old
  dHosp_3_old <- wane_VE_h*eta*risk_old_h*epsilon*E_3_old + wane_VE_h*eta*risk_old_h*epsilon*E_3E_old
  

  ## High risk
  ## Waned
  dS_1_hr <- - lambda_hr*S_1_hr + omega*S_3_hr + omega*S_3E_hr - vacc_hr_rate*S_1_hr
  dE_1_hr <- + lambda_hr*S_1_hr - epsilon*E_1_hr 
  dI_1_hr <- + (1-eta*risk_hr_h)*epsilon*E_1_hr - gamma*I_1_hr
  dH_1_hr <- + eta*risk_hr_h*epsilon*E_1_hr - gamma*H_1_hr
  dD_1_hr <- + alpha*gamma*H_1_hr
  
  ## Boosted
  dS_2_hr <- - (1-(1-VE_i))*lambda_hr*S_2_hr - omega*S_2_hr + vacc_hr_rate*S_1_hr + vacc_hr_rate*S_3_hr + 
    vacc_hr_rate*S_3E_hr + (1-alpha)*gamma*H_3_hr + gamma*I_3_hr + (1-alpha)*gamma*H_3E_hr + 
    gamma*I_3E_hr + (1-alpha)*gamma*H_2_hr + gamma*I_2_hr + gamma*I_2E_hr + (1-alpha)*gamma*H_2E_hr - immesc*S_2_hr
  dE_2_hr <- + (1-(1-VE_i))*lambda_hr*S_2_hr - epsilon*E_2_hr
  dI_2_hr <- + (1-VE_h*eta*risk_hr_h)*epsilon*E_2_hr - gamma*I_2_hr
  dH_2_hr <- + VE_h*eta*risk_hr_h*epsilon*E_2_hr - gamma*H_2_hr
  dD_2_hr <- + alpha*gamma*H_2_hr
  
  ## Boosted, immune escape
  dS_2E_hr <- - (1-(1-VE_i)*immesc_prop)*lambda_hr*S_2E_hr - omega*S_2E_hr + immesc*S_2_hr
  dE_2E_hr <- + (1-(1-VE_i)*immesc_prop)*lambda_hr*S_2E_hr - epsilon*E_2E_hr
  dI_2E_hr <- + (1-VE_h*eta*risk_hr_h)*epsilon*E_2E_hr - gamma*I_2E_hr
  dH_2E_hr <- + VE_h*eta*risk_hr_h*epsilon*E_2E_hr - gamma*H_2E_hr
  dD_2E_hr <- + alpha*gamma*H_2E_hr
  
  ## Partial
  dS_3_hr <- - (1-(1-wane_VE_i))*lambda_hr*S_3_hr - omega*S_3_hr + omega*S_2_hr + (1-alpha)*gamma*H_1_hr +
    gamma*I_1_hr - vacc_hr_rate*S_3_hr - immesc*S_3_hr
  dE_3_hr <- + (1-(1-wane_VE_i))*lambda_hr*S_3_hr - epsilon*E_3_hr
  dI_3_hr <- + (1-wane_VE_h*eta*risk_hr_h)*epsilon*E_3_hr - gamma*I_3_hr
  dH_3_hr <- + wane_VE_h*eta*risk_hr_h*epsilon*E_3_hr - gamma*H_3_hr
  dD_3_hr <- + alpha*gamma*H_3_hr
  
  ## Partial, immune escape
  dS_3E_hr <- - (1-(1-wane_VE_i)*immesc_prop)*lambda_hr*S_3E_hr - omega*S_3E_hr + omega*S_2E_hr + immesc*S_3_hr - vacc_hr_rate*S_3E_hr
  dE_3E_hr <- + (1-(1-wane_VE_i)*immesc_prop)*lambda_hr*S_3E_hr - epsilon*E_3E_hr
  dI_3E_hr <- + (1-wane_VE_h*eta*risk_hr_h)*epsilon*E_3E_hr - gamma*I_3E_hr
  dH_3E_hr <- + wane_VE_h*eta*risk_hr_h*epsilon*E_3E_hr - gamma*H_3E_hr
  dD_3E_hr <- + alpha*gamma*H_3E_hr
  
  dInf_1_hr <- epsilon*E_1_hr
  dInf_2_hr <- epsilon*E_2_hr + epsilon*E_2E_hr
  dInf_3_hr <- epsilon*E_3_hr + epsilon*E_3E_hr
  
  dHosp_1_hr <- eta*risk_hr_h*epsilon*E_1_hr
  dHosp_2_hr <- VE_h*eta*risk_hr_h*epsilon*E_2_hr + VE_h*eta*risk_hr_h*epsilon*E_2E_hr
  dHosp_3_hr <- wane_VE_h*eta*risk_hr_h*epsilon*E_3_hr + wane_VE_h*eta*risk_hr_h*epsilon*E_3E_hr
  
  
  # Return list of gradients
  list(c(dS_1_child, dE_1_child, dI_1_child, dH_1_child, dD_1_child,
         dS_2_child, dE_2_child, dI_2_child, dH_2_child, dD_2_child,
         dS_2E_child, dE_2E_child, dI_2E_child, dH_2E_child, dD_2E_child,
         dS_3_child, dE_3_child, dI_3_child, dH_3_child, dD_3_child,
         dS_3E_child, dE_3E_child, dI_3E_child, dH_3E_child, dD_3E_child,
         dInf_1_child, dInf_2_child, dInf_3_child, dHosp_1_child, dHosp_2_child, dHosp_3_child,
         dS_1_adult, dE_1_adult, dI_1_adult, dH_1_adult, dD_1_adult,
         dS_2_adult, dE_2_adult, dI_2_adult, dH_2_adult, dD_2_adult,
         dS_2E_adult, dE_2E_adult, dI_2E_adult, dH_2E_adult, dD_2E_adult,
         dS_3_adult, dE_3_adult, dI_3_adult, dH_3_adult, dD_3_adult,
         dS_3E_adult, dE_3E_adult, dI_3E_adult, dH_3E_adult, dD_3E_adult,
         dInf_1_adult, dInf_2_adult, dInf_3_adult, dHosp_1_adult, dHosp_2_adult, dHosp_3_adult,
         dS_1_old, dE_1_old, dI_1_old, dH_1_old, dD_1_old,
         dS_2_old, dE_2_old, dI_2_old, dH_2_old, dD_2_old,
         dS_2E_old, dE_2E_old, dI_2E_old, dH_2E_old, dD_2E_old,
         dS_3_old, dE_3_old, dI_3_old, dH_3_old, dD_3_old,
         dS_3E_old, dE_3E_old, dI_3E_old, dH_3E_old, dD_3E_old,
         dInf_1_old, dInf_2_old, dInf_3_old, dHosp_1_old, dHosp_2_old, dHosp_3_old,
         dS_1_hr, dE_1_hr, dI_1_hr, dH_1_hr, dD_1_hr,
         dS_2_hr, dE_2_hr, dI_2_hr, dH_2_hr, dD_2_hr,
         dS_2E_hr, dE_2E_hr, dI_2E_hr, dH_2E_hr, dD_2E_hr,
         dS_3_hr, dE_3_hr, dI_3_hr, dH_3_hr, dD_3_hr,
         dS_3E_hr, dE_3E_hr, dI_3E_hr, dH_3E_hr, dD_3E_hr,
         dInf_1_hr, dInf_2_hr, dInf_3_hr, dHosp_1_hr, dHosp_2_hr, dHosp_3_hr
         ))
}

#### Vax function ####
# vacc_function <- function(t){
#   return(0.005)
# }

# This function takes in a set of vaccination data, and an ego (race/ethnicity). 
# It creates a spline function using the daily vaccination rate for each day (NOT cumulative)
vacc_function <- function(data_vax, ego_string){
  dfv <- data_vax %>% 
    filter(Type == ego_string) %>%
    arrange(day)
  vaxF <- splinefun(x = dfv$day, y = dfv$Rate)
  return(vaxF)
}

#### Seasonal function ####
seas_matrix <- function(mu_janfeb, mu_marapr, mu_mayjun, mu_julaug, mu_septoct, mu_novdec){
  n <- 365
  x <- 0:(n-1)/(n-1);
  k<- 0:6/6
  matrix <- cSplineDes(x, k, ord = 4, derivs=0) %>%
    as.data.frame() %>%
    mutate(day = 1:365) %>%
    rename(JanFeb = V1, MarApr = V2, MayJun = V3, JulAug = V4, SeptOct = V5, NovDec = V6) %>%
    pivot_longer(JanFeb:NovDec, values_to = "Value", names_to = "Season") %>%
    mutate(spline_scalar = case_when(
      Season == "JanFeb" ~ mu_janfeb,
      Season == "MarApr" ~ mu_marapr,
      Season == "MayJun" ~ mu_mayjun,
      Season == "JulAug" ~ mu_julaug,
      Season == "SeptOct" ~ mu_septoct,
      Season == "NovDec" ~ mu_novdec
    )) %>%
    mutate(Value = spline_scalar*Value) %>%
    group_by(day) %>% 
    summarise(Value = mean(Value)) %>%
    ungroup() %>%
    mutate(day = if_else(day == 365, 0, day))
  return(matrix)
}

seas_function <- function(t, matrix){
  matrix <- matrix %>%
    filter(day == t %% 365)
  mu <- matrix$Value[1]
    return(mu)
}


#### loglik ####
loglik <- function(
    mu_janfeb, mu_marapr, mu_mayjun, 
    mu_julaug, mu_septoct, mu_novdec,
    alpha,
    risk_old_h, risk_child_h,
    sig_dist
){
  
  # First, feed in standard parms
  paras <- run_parms
  # update death rate, hospitalization risks
  paras["alpha"] <- alpha
  paras["risk_old_h"] <- risk_old_h
  paras["risk_child_h"] <- risk_child_h
  # set seasonality
  set_mat <- seas_matrix(mu_janfeb, mu_marapr, mu_mayjun, 
                         mu_julaug, mu_septoct, mu_novdec)
  ## make sure mu is defined globally so that it actually passes into the seir function
  mu <<- function(t){
    seas_function(t, matrix = set_mat)
  }
  
  # set initial conditions
  init <- run_init
 
  # Set times
  times <- 92:481
  
  # vec_print <- c(mu_janfeb, mu_marapr, mu_mayjun, 
  #                mu_julaug, mu_septoct, mu_novdec,
  #                alpha,
  #                sig_dist)
  # 
  # print("------- PARAS --------")
  # print(vec_print)
  
  # Run the model 
  out_calib1 <- ode(y=init, func = seir, times=times, parms = paras, method = "euler") %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(time, 
           Hosp_1_child, Hosp_2_child,Hosp_3_child,
           D_1_child, D_2_child, D_2E_child, D_3_child, D_3E_child,
           Hosp_1_adult, Hosp_2_adult,Hosp_3_adult,
           D_1_adult, D_2_adult, D_2E_adult, D_3_adult, D_3E_adult,
           Hosp_1_old, Hosp_2_old,Hosp_3_old,
           D_1_old, D_2_old, D_2E_old, D_3_old, D_3E_old,
           Hosp_1_hr, Hosp_2_hr, Hosp_3_hr,
           D_1_hr, D_2_hr, D_2E_hr, D_3_hr, D_3E_hr
           ) %>%
    group_by(time) %>%
    reframe(
            `inc hosp_0-64` = Hosp_1_adult + Hosp_2_adult + Hosp_3_adult + Hosp_1_child + Hosp_2_child + Hosp_3_child + Hosp_1_hr + Hosp_2_hr + Hosp_3_hr,
            `inc nhsn_0-17` = Hosp_1_child + Hosp_2_child + Hosp_3_child,
            `inc nhsn_18-64` = Hosp_1_adult + Hosp_2_adult + Hosp_3_adult + Hosp_1_hr + Hosp_2_hr + Hosp_3_hr,
            `inc hosp_65-130` = Hosp_1_old + Hosp_2_old + Hosp_3_old,
            `inc nhsn_65-130` = Hosp_1_old + Hosp_2_old + Hosp_3_old,
            `inc hosp_0-130` = `inc hosp_0-64` + `inc hosp_65-130`,
            `inc death_0-64` = D_1_adult + D_2_adult + D_2E_adult + D_3_adult + D_3E_adult + D_1_child + D_2_child + D_2E_child +  D_3_child + D_3E_child + D_1_hr + D_2_hr + D_2E_hr + D_3_hr + D_3E_hr,
            `inc death_65-130` = D_1_old + D_2_old + D_2E_old +  D_3_old + D_3E_old,
            `inc death_0-130` = `inc death_0-64` + `inc death_65-130`
            ) %>%
    ungroup() %>%
    pivot_longer(-time) %>%
    separate(name, into = c("target", "age_group"), sep = "_") %>%
    group_by(target, age_group) %>%
    arrange(time) %>%
    mutate(value = value - lag(value, 7)) %>% ## weekly
    ungroup() %>%
    mutate(date = as.Date("01-01-2024", "%m-%d-%Y") + time) %>%
    select(-time) %>%
    right_join(data, by = c("target", "age_group", "date")) %>%
  # Normalize the data. Here, I'm using the maximum in the observed data for each target and 
  # age combination. This step is very important. Without it, the MLE weights targets and age differently,
  # and doesn't converge. 
    group_by(target, age_group) %>%
    arrange(date) %>%
    mutate(max_normal = max(observation, na.rm = T),
           observation = observation/max_normal,
           value = value/max_normal) %>%
    filter(!is.na(observation), !is.na(value)) %>%
    ungroup()
  
  # Now get the negative log likelihood. Note we are concurrently fitting the sd
  ll <- -sum(dnorm(x=out_calib1$value,mean=out_calib1$observation,sd=sig_dist,log=TRUE))
  
  # This code ensures that if the output of the log likelihood is NA, the MLE won't stop. Instead it will return
  # an extremely large, positive value of the negative log likelihood and keep iterating.
  ll_final = if_else(is.na(ll), 10^6, ll)
  
  return(ll_final)
}


#### profile ####

profile <- function(i){
  ## grab the parameter estimate from the maximum likelihood
  parm_test = dt_profile$Estimate[i]
  
  # mu_janfeb = 0, 
  # mu_marapr = 0, 
  # mu_mayjun = 0, 
  # mu_julaug = 0, 
  # mu_septoct = 0, 
  # mu_novdec = 0, 
  # alpha = 1/50,
  # risk_old_h = 2,
  # risk_child_h = 0.05,
  # sig_dist = 0.05
  
  ## set up the start and upper/lowers for the first mle. If any parameters are on the boundary, shift slightly so that the start parms are not boundary parms. 
  start1 = list(
    mu_janfeb =  dt_run$Estimate[1], mu_marapr =  dt_run$Estimate[2],
    mu_mayjun =  dt_run$Estimate[3], mu_julaug =  dt_run$Estimate[4],
    mu_septoct = dt_run$Estimate[5], mu_novdec = dt_run$Estimate[6],  
    alpha = dt_run$Estimate[7], risk_old_h = dt_run$Estimate[8],
    risk_child_h = dt_run$Estimate[9],
    sig_dist = dt_run$Estimate[10])
  upper_list1 = upper_limit1
  lower_list1 = lower_limit1
  fixed_list1 = list(
    mu_janfeb = parm_test, mu_marapr = parm_test, 
    mu_mayjun = parm_test, mu_julaug = parm_test,
    mu_septoct = parm_test, mu_novdec = parm_test, 
    alpha = parm_test, risk_old_h = parm_test,
    risk_child_h = parm_test,
    sig_dist = parm_test
  )
  
  startA <- start1[-i] ## remove the one we are fixing
  upper_listA <- upper_list1[-i]
  lower_listA <- lower_list1[-i]
  fixed_listA <- fixed_list1[i] ## make a list containing only the param we are fixing
  
  conf_threshold <- out_ll_A - 1.92 ## grab the confidence threshold for log likelihood
  
  ## get right threshold
  ## First, set the starting steps away from the parameter
  precision = 0.1
  parm_test_right = dt_profile$Estimate[i] + precision ## start at the true parameter value plus a step
  upper_limit_var <- dt_profile$upper[i]
  
  print("Running right MLE")
  
  if (round(dt_profile$Estimate[i],4) == round(dt_profile$upper[i],4)){
    parm_test_right = dt_profile$Estimate[i]
    new_ll_right = -10^3
  }
  else{
    while (precision > 10e-5) {
      ## IF the test param has exceeded the allowed window for the parameter, we will not run the mle, and instead go back a step and adjust precision (treat it like it is < conf_threshold)
      
      if (parm_test_right >= upper_limit_var){
        new_ll_right <- -10^3 ## return dummy log likelihood that will always be outside the threshold
      } ## OTHERWISE we will run the mle
      else{
        #parm_test <- parm_test + precision
        ## update the list of fixed parms
        fixed_listA = list(#mu = parm_test_right,
          mu_janfeb = parm_test_right, mu_marapr = parm_test_right, 
          mu_mayjun = parm_test_right, mu_julaug = parm_test_right,
          mu_septoct = parm_test_right, mu_novdec = parm_test_right, 
          alpha = parm_test_right, risk_old_h = parm_test_right,
          risk_child_h = parm_test_right,
          sig_dist = parm_test_right
        )
        fixed_listA <- fixed_listA[i]
        
        ## run mle and grab the new log likelihood
        m1_NC_A = mle2(minuslogl = loglik,
                       start = startA,
                       data = data,
                       fixed = fixed_listA,
                       method="L-BFGS-B",
                       upper= upper_listA,
                       lower = lower_listA,
                       control=list(maxit=1000, trace=TRUE, parscale=abs(unlist(startA)))
        )
        
        
        right_ll_A <- -m1_NC_A@min ## grab the base likelihood for phase 1 (remember, we fit on the negative log likelihood, so this should have a minus sign in front)
        
        new_ll_right <- right_ll_A
      }
      
      ## If it equals the confidence threshold, stop
      if (new_ll_right == conf_threshold){
        break
      }
      ## if it's above the threshold, shift the param again to the right
      else if(new_ll_right > conf_threshold){
        parm_test_right = parm_test_right + precision}
      ## if it's below the threshold, shift the param to the right but by less
      else if(new_ll_right < conf_threshold){
        parm_test_right = parm_test_right - precision
        precision = precision/10
        parm_test_right = parm_test_right + precision
      }
    }
    
  }
  
  
  ## First, set the starting steps away from the parameter
  precision = 0.1
  parm_test_left = dt_profile$Estimate[i] - precision ## start at the true parameter value minus a step
  lower_limit_var <- dt_profile$lower[i]
  
  print("Running left MLE")
  
  if (round(dt_profile$Estimate[i], 4) == round(dt_profile$lower[i], 4)){
    parm_test_left = dt_profile$Estimate[i]
    new_ll_left = -10^3
  }
  else{
    while (precision > 10e-5) {
      
      ## IF the test param has exceeded the allowed window for the parameter, we will not run the mle, and instead go back a step and adjust precision (treat it like it is < conf_threshold)
      
      if (parm_test_left <= lower_limit_var){
        new_ll_left <- -10^3 ## return dummy log likelihood that will always be outside the threshold
      } ## OTHERWISE we will run the mle
      else{
        
        
        #parm_test <- parm_test + precision
        ## update the list of fixed parms
        fixed_listA = list(#mu = parm_test_right,
          mu_janfeb = parm_test_left, mu_marapr = parm_test_left, 
          mu_mayjun = parm_test_left, mu_julaug = parm_test_left,
          mu_septoct = parm_test_left, mu_novdec = parm_test_left, 
          alpha = parm_test_left, risk_old_h = parm_test_left,
          risk_child_h = parm_test_left,
          sig_dist = parm_test_left
        )
        fixed_listA <- fixed_listA[i]
        
        
        ## run mle and grab the new log likelihood
        ## run m1_A for starting param
        m1_NC_A = mle2(minuslogl = loglik,
                       start = startA,
                       data = data,
                       fixed = fixed_listA,
                       method="L-BFGS-B",
                       upper= upper_listA,
                       lower = lower_listA,
                       control=list(maxit=1000, trace=TRUE, parscale=abs(unlist(startA)))
        )
        
        left_ll_A <- -m1_NC_A@min ## grab the base likelihood for phase 1 (remember, we fit on the negative log likelihood, so this should have a minus sign in front)
        new_ll_left <- left_ll_A
        
      }
      
      ## If it equals the confidence threshold, stop
      if (new_ll_left == conf_threshold){
        break
      }
      ## if it's above the threshold, shift the param again to the right
      else if(new_ll_left > conf_threshold){
        parm_test_left = parm_test_left - precision}
      ## if it's below the threshold, shift the param to the right but by less
      else if(new_ll_left < conf_threshold){
        parm_test_left = parm_test_left + precision
        precision = precision/10
        parm_test_left = parm_test_left - precision
      }
    }
  }
  
  out_data <- tibble(param = dt_profile$prof_param[i],
                     loglik = out_ll_A,
                     ll_05 = new_ll_left,
                     ll_95 = new_ll_right,
                     conf_05 = parm_test_left,
                     conf_95 = parm_test_right)
  
  write_csv(out_data, paste("../data/profile/", dt_profile$prof_param[i], "Geography_", dt_profile$Geography[i], ".csv"))
  
}



#### Cyclic Basis Example ####

example_seasonal_matrix <- seas_matrix(1.0401255195228700, 1.4683874657557000, 0.7828333130748770, 0.46777116618316600, 1.9445103085801600, 0.7934362529100850)

n <- 365
x <- 0:(n-1)/(n-1);
k<- 0:6/6
matrix_with_basis <- cSplineDes(x, k, ord = 4, derivs=0) %>%
  as.data.frame() %>%
  mutate(day = 1:365) %>%
  as.data.frame() %>%
  mutate(JanFeb = 1.0401255195228700*V1, MarApr = 1.4683874657557000*V2, MayJun = 0.7828333130748770*V3, 
         JulAug = 0.46777116618316600*V4, SeptOct = 1.9445103085801600*V5, NovDec = 0.7934362529100850*V6) %>%
  select(-c(V1,V2,V3,V4,V5,V6))


plot_seasonality <- ggplot() + 
  geom_line(data = matrix_with_basis, aes(x = day, y = JanFeb), color = "blue", alpha = 0.3, size = 1) + 
  geom_line(data = matrix_with_basis, aes(x = day, y = MarApr), color = "blue", alpha = 0.3, size = 1) + 
  geom_line(data = matrix_with_basis, aes(x = day, y = MayJun), color = "blue", alpha = 0.3, size = 1) + 
  geom_line(data = matrix_with_basis, aes(x = day, y = JulAug), color = "blue", alpha = 0.3, size = 1) + 
  geom_line(data = matrix_with_basis, aes(x = day, y = SeptOct), color = "blue", alpha = 0.3, size = 1) + 
  geom_line(data = matrix_with_basis, aes(x = day, y = NovDec), color = "blue", alpha = 0.3, size = 1) + 
  geom_line(data = example_seasonal_matrix, aes(x = day, y = Value), size = 1.25) + 
  ylab("Seasonal forcing") + xlab("Day") + 
  ggtitle("Illinois") + 
  theme_bw() + 
  theme(
    aspect.ratio = 1
  )
plot_seasonality

ggsave("Seasonality.jpg", width = 5, height = 5)

