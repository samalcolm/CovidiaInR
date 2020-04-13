Covidia538 <- function(Ro_uncontrolled = 2.7,
                       Ro_intermediate = 1.4,
                       Ro_lockdown = 0.7,
                       Cluster_value = "Yes, slightly",
                       Begin_intermediate = 11,
                       Begin_lockdown = 15,
                       Pct_asy = 0.3,
                       Pct_mild = 0.6,
                       Zero_date = "01/01/2020",
                       Serial = 5,
                       Population = 10000000,
                       Initial_cases = 1,
                       Faux_severe = 0.001,
                       Faux_mild = 0.025,
                       Desire_severe = 1,
                       Desire_mild = 0.5,
                       Desire_asy = 0.02,
                       Initial_tests = 1000,
                       Ramp_period = 3,
                       Test_gowth_rate = 0.5,
                       Tests_max = 10000000,
                       Rationed_tests = 0.75,
                       False_negative = 0.2,
                       False_positive = 0.005,
                       Delay = 2,
                       generations = 36) {
  
  # Source: https://fivethirtyeight.com/features/coronavirus-case-counts-are-meaningless/
  #         https://fivethirtyeight.com/wp-content/uploads/2020/04/covidia_1.05_updated.xlsx
  Cluster = c(
    "No" = 0,
    "Yes, slightly" = 0.5,
    "Yes, moderately" = 1,
    "Yes, substantailly" = 2
  )
  Version = 1.05
  
  # saves list of parameters before any other values enter the environment
  params = as.list(environment())
  
  # put in some error checking
  ARG_ERROR_FLAG = FALSE
  if (Ro_uncontrolled < 0 | Ro_intermediate < 0 | Ro_lockdown < 0) {
    print("Ro_ values must not be negative")
    ARG_ERROR_FLAG = TRUE
  }
  if (Pct_mild > 1 | Pct_mild < 0) {
    print("Pct_mild must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (Pct_asy > 1 | Pct_asy < 0) {
    print("Pct_asy must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (1 - Pct_mild - Pct_asy > 1 | 1 - Pct_mild - Pct_asy < 0) {
    print("1-Pct_mild-Pct_asy must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (False_positive > 1 | False_positive < 0) {
    print("False_positive must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (False_negative > 1 | False_negative < 0) {
    print("False_negative must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (Faux_severe > 1 | Faux_severe < 0) {
    print("Faux_severe must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (Faux_mild > 1 | Faux_mild < 0) {
    print("Faux_mild must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (Desire_severe > 1 | Desire_severe < 0) {
    print("Desire_severe must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (Desire_mild > 1 | Desire_mild < 0) {
    print("Desire_mild must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (Desire_asy > 1 | Desire_asy < 0) {
    print("Desire_asy must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (Rationed_tests > 1 | Rationed_tests < 0) {
    print("Rationed_tests must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (mortality > 1 | mortality < 0) {
    print("mortality must be between 0 and 1")
    ARG_ERROR_FLAG = TRUE
  }
  if (!(Cluster_value %in% names(Cluster))) {
    print(paste(
      "Cluster_value must be between one of",
      paste0("'", names(Cluster), "'", collapse = ",")
    ))
    ARG_ERROR_FLAG = TRUE
  }
  if (!(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$",Zero_date))) {
    print("Zero_date must be of form mm/dd/yyyy")
    ARG_ERROR_FLAG = TRUE
  }
  
  if (ARG_ERROR_FLAG)
    stop("Errors in arguments listed above")
  
  # change Zero_date string to Date type
  Zero_date = as.Date(Zero_date, format = "%m/%d/%Y")
  # since we have a Generation 0
  gens = generations + 1
  
  Date = rep(0, gens)
  Actual_R = rep(0, gens)
  Doubling_time = rep(0, gens) # not currently implemented
  New_Infections = rep(0, gens)
  Cumulative_Infections = rep(0, gens)
  Pop_severe = rep(0, gens)
  Pop_mild = rep(0, gens)
  Pop_asy = rep(0, gens)
  Deaths = rep(0, gens)
  Cumulative_Deaths = rep(0, gens)
  Actual_severe = rep(0, gens)
  Actual_mild = rep(0, gens)
  Actual_asy = rep(0, gens)
  
  Tests_available = rep(0, gens)
  Rationed_tests_available = rep(0, gens)
  On_demand_tests = rep(0, gens)
  
  Desiring_tests_severe = rep(0, gens)
  Desiring_tests_mild = rep(0, gens)
  Desiring_tests_asy = rep(0, gens)
  
  Rationed_tests_severe = rep(0, gens)
  Rationed_tests_mild = rep(0, gens)
  Rationed_tests_asy = rep(0, gens)
  
  Unmet_demand_tests_severe = rep(0, gens)
  Unmet_demand_tests_mild = rep(0, gens)
  Unmet_demand_tests_asy = rep(0, gens)
  Unmet_demand_total = rep(0, gens)
  
  On_demand_tests_severe = rep(0, gens)
  On_demand_tests_mild = rep(0, gens)
  On_demand_tests_asy = rep(0, gens)
  
  Tests_conducted_severe = rep(0, gens)
  Tests_conducted_mild = rep(0, gens)
  Tests_conducted_asy = rep(0, gens)
  Total_tests_conducted = rep(0, gens)
  Cumulative_tests = rep(0, gens)
  
  Target_Ro = rep(0, gens)
  
  Share_positive_severe = rep(0, gens)
  Share_positive_mild = rep(0, gens)
  Share_positive_asy = rep(0, gens)
  
  Positive_tests_severe = rep(0, gens)
  Positive_tests_mild = rep(0, gens)
  Positive_tests_asy = rep(0, gens)
  
  Reported_new_positives = rep(0, gens)
  Nominal_R = rep(0, gens)
  Share_positive = rep(0, gens)
  New_share_positive = rep(0, gens)
  
  New_detected_cases = rep(0, gens)
  Cumulative_detected_cases = rep(0, gens)
  
  Detection_rate = rep(0, gens)
  New_tests_reported = rep(0, gens)
  Share_positive = rep(0, gens)
  
  Susceptible = rep(0, gens)
  
  Susceptible[1] = Population - Initial_cases
  New_Infections[1] = Initial_cases
  Cumulative_Infections[1] = Initial_cases
  
  for (i in c(1:gens)) {
    Date[i] = Zero_date + Serial * (i - 1)
    
    if (i > 1) { 
      sir = singleCompartment(New_Infections[i-1], Cumulative_Infections[i-1], Susceptible[i-1], Target_Ro[i-1], Cluster[Cluster_value], Population)
      New_Infections[i] = sir$New_Infections
      Cumulative_Infections[i] = sir$Cumulative_Infections
      Susceptible[i] = sir$Susceptible
    }
      
    Actual_severe[i] = round(New_Infections[i] * (1 - Pct_asy - Pct_mild))
    Actual_mild[i]   = round(New_Infections[i] * Pct_mild)
    Actual_asy[i]    = New_Infections[i] - Actual_severe[i] - Actual_mild[i]
    
    Pop_severe[i] = round((Population - (
      Actual_severe[i] + Actual_mild[i] + Actual_asy[i]
    )) * Faux_severe) + Actual_severe[i]
    Pop_mild[i]   = round((Population - (
      Actual_severe[i] + Actual_mild[i] + Actual_asy[i]
    )) * Faux_mild)   + Actual_mild[i]
    Pop_asy[i]    = Population - Pop_severe[i] - Pop_mild[i]
    
    Share_positive_severe[i] = Actual_severe[i] / Pop_severe[i]
    Share_positive_mild[i]   = Actual_mild[i] / Pop_mild[i]
    Share_positive_asy[i]    = Actual_asy[i] / Pop_asy[i]
    
    if (i > Ramp_period) {
      Tests_available[i] = round(min(Tests_max, (1 + Test_gowth_rate) * Tests_available[i - 1]))
    } else if (i > 1) {
      Tests_available[i] = Initial_tests
    } else {
      Tests_available[i] = 0
    }
    
    Rationed_tests_available[i] = round(Rationed_tests * Tests_available[i])
    On_demand_tests[i] = Tests_available[i] - Rationed_tests_available[i]
    
    Desiring_tests_severe[i] = round(Pop_severe[i] * Desire_severe * (1 - Cumulative_tests[i] / Population))
    Desiring_tests_mild[i]   = round(Pop_mild[i]  * Desire_mild  * (1 - Cumulative_tests[i] / Population))
    Desiring_tests_asy[i]    = round(Pop_asy[i]   * Desire_asy   * (1 - Cumulative_tests[i] / Population))
    
    Rationed_tests_severe[i] = min(Rationed_tests_available[i], Desiring_tests_severe[i])
    Rationed_tests_mild[i]   = min(Rationed_tests_available[i] - Rationed_tests_severe[i], Desiring_tests_mild[i])
    Rationed_tests_asy[i]    = min(Rationed_tests_available[i] - Rationed_tests_severe[i] - Rationed_tests_mild[i], Desiring_tests_asy[i])
    
    Unmet_demand_tests_severe[i] = Desiring_tests_severe[i] - Rationed_tests_severe[i]
    Unmet_demand_tests_mild[i]   = Desiring_tests_mild[i]   - Rationed_tests_mild[i]
    Unmet_demand_tests_asy[i]    = Desiring_tests_asy[i]    - Rationed_tests_asy[i]
    Unmet_demand_total[i] = Unmet_demand_tests_severe[i] + Unmet_demand_tests_mild[i] + Unmet_demand_tests_asy[i]
    
    if (Unmet_demand_total[i] > 0) {
      On_demand_tests_severe[i] = round(On_demand_tests[i] * Unmet_demand_tests_severe[i] /
                                          Unmet_demand_total[i])
      On_demand_tests_mild[i]   = round(On_demand_tests[i] * Unmet_demand_tests_mild[i] /
                                          Unmet_demand_total[i])
      On_demand_tests_asy[i]    = round(On_demand_tests[i] * Unmet_demand_tests_asy[i] /
                                          Unmet_demand_total[i])
    }
    
    Tests_conducted_severe[i] = On_demand_tests_severe[i] + Rationed_tests_severe[i]
    Tests_conducted_mild[i]   = On_demand_tests_mild[i]   + Rationed_tests_mild[i]
    Tests_conducted_asy[i]    = On_demand_tests_asy[i]    + Rationed_tests_asy[i]
    Total_tests_conducted[i] = Tests_conducted_severe[i] + Tests_conducted_mild[i] + Tests_conducted_asy[i]
    if (i < gens)
      Cumulative_tests[i + 1] = Cumulative_tests[i] + Total_tests_conducted[i]
    
    Target_Ro[i] = Ro_lockdown
    if (i <= Begin_intermediate) {
      Target_Ro[i] = Ro_uncontrolled
    } else if (i > Begin_intermediate & i <= Begin_lockdown) {
      Target_Ro[i] = Ro_intermediate
      #     Target_Ro[i] = Ro_uncontrolled + (i-Begin_intermediate)/(Begin_lockdown-Begin_intermediate)*(Ro_lockdown - Ro_uncontrolled)
    }
    
    Positive_tests_severe[i] =
      round(Tests_conducted_severe[i] * Share_positive_severe[i] * (1 - False_negative)) +
      round(Tests_conducted_severe[i] * (1 - Share_positive_severe[i]) * False_positive)
    Positive_tests_mild[i] = 
      round(Tests_conducted_mild[i] * Share_positive_mild[i] *(1 - False_negative)) + 
      round(Tests_conducted_mild[i] * (1 - Share_positive_mild[i]) * False_positive)
    Positive_tests_asy[i]  = 
      round(Tests_conducted_asy[i] * Share_positive_asy[i]  * (1 - False_negative)) + 
      round(Tests_conducted_asy[i] * (1 - Share_positive_asy[i]) * False_positive)
    #no lag
    Reported_new_positives[i] = Positive_tests_severe[i] + Positive_tests_mild[i] + Positive_tests_asy[i]
    if (Total_tests_conducted[i] > 0)
      Share_positive[i] = Reported_new_positives[i] / Total_tests_conducted[i]
    
    if (i > Delay)
      New_detected_cases[i] = Reported_new_positives[i - Delay]
    if (i > Delay)
      Cumulative_detected_cases[i] = New_detected_cases[i] + Cumulative_detected_cases[i - 1]
    
    Detection_rate[i] = Cumulative_detected_cases[i] / Cumulative_Infections[i]
    if (i > Delay)
      New_tests_reported[i] = Total_tests_conducted[i - Delay]
    if (New_tests_reported[i] > 0)
      New_share_positive[i] = New_detected_cases[i] / New_tests_reported[i]
  }
  
  for (i in c(1:(gens - 1))) {
    if (New_Infections[i] > 0)
      Actual_R[i] = New_Infections[i + 1] / New_Infections[i]
    if (Reported_new_positives[i] > 0)
      Nominal_R[i + 1] = Reported_new_positives[i + 1] / Reported_new_positives[i]
  }
  
  Date = as.Date(Date, origin = "1970-01-01")
  output = list(
    params = params,
    output = data.frame(
      Date,
      Actual_R,
      New_Infections,
      Cumulative_Infections,
      New_detected_cases,
      Cumulative_detected_cases,
      Detection_rate,
      New_tests_reported,
      New_share_positive,
      Target_Ro,
      Susceptible,
      Actual_severe,
      Actual_mild,
      Actual_asy,
      Pop_severe,
      Pop_mild,
      Pop_asy,
      Desiring_tests_severe,
      Desiring_tests_mild,
      Desiring_tests_asy,
      Tests_available,
      Rationed_tests_available,
      On_demand_tests,
      Rationed_tests_severe,
      Rationed_tests_mild,
      Rationed_tests_asy,
      Unmet_demand_tests_severe,
      Unmet_demand_tests_mild,
      Unmet_demand_tests_asy,
      Unmet_demand_total,
      On_demand_tests_severe,
      On_demand_tests_mild,
      On_demand_tests_asy,
      Tests_conducted_severe,
      Tests_conducted_mild,
      Tests_conducted_asy,
      Share_positive_severe,
      Share_positive_mild,
      Share_positive_asy,
      Positive_tests_severe,
      Positive_tests_mild,
      Positive_tests_asy,
      Reported_new_positives,
      Nominal_R,
      Share_positive
    )
  )
}

# Susceptible/Infected compartment for single generation. This function can be used if
# model is to account for interegional movement. Also for testing alternate infection dynamics.
# One could also add Removals.
singleCompartment <- function(New_Infections, Cumulative_Infections, Susceptible, Target_Ro, Cluster, Population) {
  New_Infections =
    round(Susceptible *
            (1 -
               (1 -
                  (
                    (
                      Target_Ro *
                        (1 - Cumulative_Infections / Population) ^ Cluster
                    ) / Population
                  )) ^ New_Infections))
  
  Cumulative_Infections = Cumulative_Infections + New_Infections
  Susceptible = Population - Cumulative_Infections
  output = list(New_Infections=New_Infections, Cumulative_Infections=Cumulative_Infections, Susceptible=Susceptible)
}
