#-----------------------------------------------------------------------------#
# Budget distrib. calculations                                             ----
#-----------------------------------------------------------------------------#

# 1 definition of global fns and vars          --------------------------------
startYr <- 2021
histYr <- 1995
endYr <- 2050

# determine global budget (700 gt in starting year based on Williges et al.
# 2021, minus global emissions in intervening years)
global_budget <- 700 - 36.9 - 37.7 - 37.9 - 35.6 - 37.5


tot_budget <- global_budget

#annChgLvl = 0.07
# Define tons of CO2 per unit of K stock
k_value <- 0.000426357

# Define HDI threshold
threshold <- 0.75

# 1.1 function calcEndEmi ---------------------------------------------------
# Define function to calcuate the end emissions per capita for the world
# under CAC (as this will change depending on the assumptions, e.g. not a 
# single target number under all circumstances)

calcEndEmi <- function(time_start, time_end, emi_start, b_tot){
  emi_end = ((2*b_tot) - (emi_start * (time_end - time_start))) / 
    (time_end - time_start)
}

# 1.2 function calcNatB    --------------------------------------------------
# Define function to calcuate the total budget for a country, given the
# emissions per capita goal for the world at 2050

calcNatB <- function(time_start, time_end, n_emi_start, n_emi_end){
  bNat <- 0.5 * (time_end - time_start) * (n_emi_start - n_emi_end) + 
    (time_end - time_start) * n_emi_end
}

# 2 Budget distribution functions -------------------------------------------   
# EPC -----------------------------------------------------------------------#
# EPC Simple -----
epc_s <- function(rDataInput, budget_remaining = tot_budget) {
  world_budget <- budget_remaining # / sum(rDataInput$pop_21)
  
  epc_s <- rDataInput
  # takes avg country pop from start and end periods,
  # and divides by average global pop for same periods
  epc_s <- mutate(epc_s, 
                  epc_s = world_budget * (((pop_21 + pop_50) / 2) / 
                                        ((sum(rDataInput$pop_21, na.rm = TRUE) +
                                         sum(rDataInput$pop_50, na.rm = TRUE))/2)))
  epc_s <- arrange(epc_s, desc(hdi))
}

# EPC-H qual(1995) ----
epc_h <- function(rDataInput, budget_remaining = tot_budget){
  world_budget <- budget_remaining + sum(rDataInput$e_hist, na.rm = TRUE)
  
  epc_h <- rDataInput
  epc_h <- mutate(epc_h, epc_h = world_budget * (((pop_95 + pop_50) / 2) /
                               ((sum(rDataInput$pop_95, na.rm = TRUE) +
                                sum(rDataInput$pop_50, na.rm = TRUE)) / 2)) - 
                    e_hist)  
  epc_h <- arrange(epc_h, desc(hdi))
}

# EPC-B qual ----
# Emissions due to 2015 K stock added to total budget is to compensate for emi 
# embodied in K stock,
epc_b <- function(rDataInput, budget_remaining = tot_budget){
  world_budget <- ((budget_remaining + (sum(rDataInput$k_19, na.rm = TRUE) * 
                                    k_value)))
  
  epc_b <- rDataInput
  # number subtracted is the embodied emission per unit of K 
  epc_b <- mutate(epc_b, epc_b = world_budget * (((pop_21 + pop_50) / 2) / 
                                ((sum(rDataInput$pop_21, na.rm = TRUE) +
                                 sum(rDataInput$pop_50, na.rm = TRUE)) / 2)) - 
                     (k_19 *  k_value))
  epc_b <- arrange(epc_b, desc(hdi))
}

# EPC-N qual -----
epc_n <- function(rDataInput, budget_remaining = tot_budget){
  # split countries into 2 datasets
  c_above <- filter(rDataInput, hdi >= threshold)
  c_below <- filter(rDataInput, hdi < threshold)
  
  # determine expected emissions per capita at the threshold
  c_below <- arrange(c_below, desc(hdi))
  
  # define threshold epc
  hdiMinEpc <- as.numeric(c_below[1,"threshold_epc"])
  
  # assign new starting emissions 
  c_below$e_21 <- c_below$pop_21 * hdiMinEpc
  
  # recombine the split sets
  hdiGoalStart <- bind_rows(c_above, c_below)
  
  # calculate the budget as if all countries below threshold were at threshold-
  # determined emissions per capita; only keep the budget for countries below
  # the threshold
  s9_emi_end <- calcEndEmi(startYr, endYr, (sum(rDataInput$e_21, na.rm = TRUE)),
                           budget_remaining)
  s9_epc_end <- s9_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  hdiGoalStart <- mutate(hdiGoalStart, budget = (calcNatB(startYr, endYr, 
                                                 hdiGoalStart$e_21, 
                                          (s9_epc_end * hdiGoalStart$pop_50))))
  
  # split dataset again, to set budget of countries > t at 0
  c_above <- filter(hdiGoalStart, hdi >= threshold)
  c_below <- filter(hdiGoalStart, hdi < threshold)
  c_above$budget <- 0
  
  # recombine the split sets
  hdiBudgetBelow <- bind_rows(c_above, c_below)
  
  # calculate the initial budget for countries below threshold
  hdiBudgetNeeded <- sum(hdiBudgetBelow$budget, na.rm = TRUE)
  print(hdiBudgetNeeded)
  
  # now find the remainder to be distributed as per usual
  world_budget <- (budget_remaining - hdiBudgetNeeded) 
  print(budget_remaining - hdiBudgetNeeded)
  epc_n <- hdiBudgetBelow
  epc_n <- mutate(epc_n, epc_n = budget + world_budget * (((pop_21 + pop_50) / 2) / 
                                        ((sum(rDataInput$pop_21, na.rm = TRUE) +
                                         sum(rDataInput$pop_50, na.rm = TRUE))/2)))
  epc_n <- arrange(epc_n, desc(hdi))
}

# EPC-N new (using poverty headcounts) ----
epc_n_pov <- function(rDataInput, poverty, budget_remaining = tot_budget){

  world_budget <- (budget_remaining - sum(poverty$bn_emi)) 
  print(budget_remaining - sum(poverty$bn_emi))
  epc_n_pov <- mutate(poverty, epc_n_pov = bn_emi + world_budget * 
                    (((pop_21 + pop_50)/2) / ((sum(poverty$pop_21, na.rm = TRUE) +
                                            sum(poverty$pop_50, na.rm = TRUE)))/2))
  epc_n_pov <- arrange(epc_n_pov, desc(hdi))
}

# EPC-NHB qual -----
epc_nhb <- function(rDataInput, budget_remaining = tot_budget){
  # split countries into 2 datasets
  c_above <- filter(rDataInput, hdi >= threshold)
  c_below <- filter(rDataInput, hdi < threshold)
  
  # determine expected emissions per capita at the threshold
  c_below <- arrange(c_below, desc(hdi))
  
  # define threshold epc
  hdiMinEpc <- as.numeric(c_below[1,"threshold_epc"])
  
  # assign new starting emissions 
  c_below$e_21 <- c_below$pop_21 * hdiMinEpc
  
  # recombine the split sets
  hdiGoalStart <- bind_rows(c_above, c_below)
  
  # calculate the budget as if all countries below threshold were at threshold-
  # determined emissions per capita; only keep the budget for countries below
  # the threshold
  s13_emi_end <- calcEndEmi(startYr, endYr, (sum(rDataInput$e_21, na.rm = TRUE)), budget_remaining)
  s13_epc_end <- s13_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  hdiGoalStart <- mutate(hdiGoalStart, budget = (calcNatB(startYr, endYr, 
                                                          hdiGoalStart$e_21, 
                                                          (s13_epc_end * 
                                                      hdiGoalStart$pop_50))))
  
  # split dataset again, to set budget of countries > t at 0
  c_above <- filter(hdiGoalStart, hdi >= threshold)
  c_below <- filter(hdiGoalStart, hdi < threshold)
  c_above$budget <- 0
  
  # recombine the split sets
  hdiBudgetBelow <- bind_rows(c_above, c_below)
  
  # calculate the initial budget for countries below threshold
  hdiBudgetNeeded <- sum(hdiBudgetBelow$budget, na.rm = TRUE)
  print(hdiBudgetNeeded)
  # now find the remainder to be distributed as per usual
  
  world_budget <- (budget_remaining - hdiBudgetNeeded + (sum(rDataInput$k_95, 
                                                       na.rm = TRUE) * 
                               k_value)  + sum(rDataInput$e_hist, na.rm = TRUE))
  
  epc_nhb <- hdiBudgetBelow
  # number subtracted is the embodied emission per unit of K 
  epc_nhb <- mutate(epc_nhb, epc_nhb = budget + world_budget * 
                      (((pop_95 + pop_50) / 2) /
                         ((sum(rDataInput$pop_95, na.rm = TRUE) +
                             sum(rDataInput$pop_50, na.rm = TRUE)) / 2)) -
                      (k_95 * k_value) - e_hist)
  epc_nhb <- arrange(epc_nhb, desc(hdi))
}

# EPC-NHBC (Limiting burden on high emitters) ----
epc_nhbc <- function(rDataInput, annChgLvl = 0.069, budget_remaining = tot_budget){
  
  # find global rate of reduction to meet an average emissions per capita of
  # 0.5 tons at 2050. Then using that rate, calculate the budget from present
  # to 2050 for all countries (minimum budget). For countries with a min budget
  # > their EPC simple budget, allocate to them the difference between the two, 
  # AFTER allocating for HDI considerations
  
  # 1. Determining global rate of reduction (assume 0.5 tons per person in 
  # 2050)
  # goalEmi2050 <- (sum(rDataInput$pop_50, na.rm = TRUE)) * 0.5
  # annualChange <- ((goalEmi2050/(sum(rDataInput$e_21, na.rm = TRUE)))^(1/(2050-2016)))-1
  
  # we use a 6.9% threshold from Rockström paper
  annualChange <- -annChgLvl
  
  # line below was just a test to see if the number was right
  # ((1+annualChange)^(34)) * (sum(rDataInput$e_21, na.rm = TRUE))
  
  # 2. Calculate country minimum budget assuming a single rate of reduction 
  # equal to the global rate from 1. above
  maxEffort <- rDataInput
  maxEffort <- arrange(maxEffort, desc(hdi))
  maxEffort$endEmi <- maxEffort$e_21 * ((1+annualChange)^(2050-2016))
  maxEffort$maxPossible <- (0.5 * (2050-2016) * (maxEffort$e_21 - 
                                                   maxEffort$endEmi)) +
    (maxEffort$endEmi * (2050-2016))
  
  # 3. Calculate the difference between EPC simple and min budget from 2.,
  # and store those values only for countries with "overly-burdensome" EPC
  # simple targets; for other countries just say zero
  maxEffort$epcAllow <- epc_s(rDataInput)$epc_s
  maxEffort$burdenSize <- maxEffort$epcAllow - maxEffort$maxPossible
  
  maxEffort$BurdenBuget <- case_when(maxEffort$burdenSize < 0 ~ 
                                       abs(maxEffort$burdenSize),
                                     maxEffort$burdenSize >= 0 ~ 0)
  
  # 4. Now calculate the budgets as in S16, but include also as a second step
  # the extra portions from 3. above, after calculating HDI pre-allocation
  
  # split countries into 2 datasets
  c_above <- filter(rDataInput, hdi >= threshold)
  c_below <- filter(rDataInput, hdi < threshold)
  
  # determine expected emissions per capita at the threshold
  c_below <- arrange(c_below, desc(hdi))
  
  # define threshold epc; [1.12] is the location of the threshold_epc 
  # SHOULD ALWAYS BE THAT LOCATION
  hdiMinEpc <- as.numeric(c_below[1, "threshold_epc"])
  
  # assign new starting emissions 
  c_below$e_21 <- c_below$pop_21 * hdiMinEpc
  
  # recombine the split sets
  hdiGoalStart <- bind_rows(c_above, c_below)
  
  # calculate the budget as if all countries below threshold were at threshold-
  # determined emissions per capita; only keep the budget for countries below
  # the threshold
  s17_emi_end <- calcEndEmi(startYr, endYr, (sum(rDataInput$e_21, na.rm = TRUE)), budget_remaining)
  s17_epc_end <- s17_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  hdiGoalStart <- mutate(hdiGoalStart, 
                         budget = (calcNatB(startYr, endYr, hdiGoalStart$e_21,
                                            (s17_epc_end * hdiGoalStart$pop_50))))
  
  # split dataset again, to set budget of countries > t at 0
  c_above <- filter(hdiGoalStart, hdi >= threshold)
  c_below <- filter(hdiGoalStart, hdi < threshold)
  c_above$budget <- 0
  
  # recombine the split sets
  hdiBudgetBelow <- bind_rows(c_above, c_below)
  
  # calculate the initial budget for countries below threshold
  hdiBudgetNeeded <- sum(hdiBudgetBelow$budget, na.rm = TRUE)
  
  # now find the remainder to be distributed as per usual, additional sum is for
  # emissions from hist numbers
  
  world_budget <- (budget_remaining - hdiBudgetNeeded + (sum(rDataInput$k_95, na.rm = TRUE) * k_value)  +
                     sum(rDataInput$e_hist, na.rm = TRUE) - sum(maxEffort$BurdenBuget, na.rm = TRUE))
  
  epc_nhbc <- hdiBudgetBelow
  # number subtracted is the embodied emission per unit of K 
  epc_nhbc <- arrange(epc_nhbc, desc(hdi))
  
  epc_nhbc <- mutate(epc_nhbc, epc_nhbc = budget + world_budget * 
                       (((pop_95 + pop_50) / 2) /
                          ((sum(rDataInput$pop_95, na.rm = TRUE) +
                              sum(rDataInput$pop_50, na.rm = TRUE)) / 2)) -
                      (k_95 * k_value) - e_hist + maxEffort$BurdenBuget)
  epc_nhbc <- arrange(epc_nhbc, desc(hdi))
}

# Used to balance out charts, nothing more
DUMMYs18 <- function(rDataInput){
  s18_nat <- rDataInput
  s18_nat <- epc_nhbc <- mutate(s18_nat, budget = NA)
  s18_nat <- arrange(s18_nat, desc(hdi))
  
}

# PCC ------------------------------------------------------------------------#
# PCC Simple -----
pcc_simple <- function(rDataInput, budget_remaining = tot_budget){
  s2_emi_end <- calcEndEmi(startYr, endYr, (sum(rDataInput$e_21, na.rm = TRUE)), budget_remaining)
  s2_epc_end <- s2_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  pcc_s <- rDataInput
  pcc_s <- mutate(pcc_s, pcc_s = (calcNatB(startYr, endYr, pcc_s$e_21, 
                                           (s2_epc_end * pcc_s$pop_50))))
  pcc_s <- arrange(pcc_s, desc(hdi))
}

# PCC-H qual (1995) ----
pcc_h <- function(rDataInput, budget_remaining = tot_budget){
  s4_emi_end <- calcEndEmi(histYr, endYr, (sum(rDataInput$e_95, na.rm = TRUE)), (budget_remaining + 
                                                       sum(rDataInput$e_hist, na.rm = TRUE)))
  s4_epc_end <- s4_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  pcc_h <- rDataInput
  pcc_h <- mutate(pcc_h, pcc_h = (calcNatB(histYr, endYr, pcc_h$e_95, 
                                           (s4_epc_end * pop_50))) - e_hist)
  pcc_h <- arrange(pcc_h, desc(hdi))
}

# PCC-B qual ----
# Additional sum added to total budget is to compensate for emi embodied in K
pcc_b <- function(rDataInput, budget_remaining = tot_budget){
  s7_emi_end <- calcEndEmi(startYr, endYr, (sum(rDataInput$e_21, na.rm = TRUE)), budget_remaining + 
                             (sum(rDataInput$k_19, na.rm = TRUE) * k_value))
  
  s7_epc_end <- s7_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  pcc_b <- rDataInput
  # number subtracted is the embodied emission per unit of K
  pcc_b <- mutate(pcc_b, pcc_b = (calcNatB(startYr, endYr, e_21, (s7_epc_end * 
                                                                    pop_50)))-
                    (k_19 * k_value))
  pcc_b <- arrange(pcc_b, desc(hdi))
}

# PCC-N qual -----
pcc_n <- function(rDataInput, budget_remaining = tot_budget){
  # split countries into 2 datasets
  c_above <- filter(rDataInput, hdi >= threshold)
  c_below <- filter(rDataInput, hdi < threshold)
  
  # determine expected emissions per capita at the threshold
  c_below <- arrange(c_below, desc(hdi))
  
  # define threshold epc
  hdiMinEpc <- as.numeric(c_below[1,"threshold_epc"])
  
  # assign new starting emissions 
  c_below$e_21 <- c_below$pop_21 * hdiMinEpc
  
  # recombine the split sets
  hdiGoalStart <- bind_rows(c_above, c_below)
  
  # calculate the budget as if all countries below threshold were at threshold-
  # determined emissions per capita; only keep the budget for countries below
  # the threshold
  s10_emi_end <- calcEndEmi(startYr, endYr, (sum(rDataInput$e_21, na.rm = TRUE)), budget_remaining)
  s10_epc_end <- s10_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  hdiGoalStart <- mutate(hdiGoalStart, budget = (calcNatB(startYr, endYr, 
                                                          hdiGoalStart$e_21, 
                                                          (s10_epc_end * 
                                                             hdiGoalStart$pop_50))))
  
  # split dataset again, to set budget of countries > t at 0
  c_above <- filter(hdiGoalStart, hdi >= threshold)
  c_below <- filter(hdiGoalStart, hdi < threshold)
  c_above$budget <- 0
  
  # recombine the split sets
  hdiBudgetBelow <- bind_rows(c_above, c_below)
  
  # calculate the initial budget for countries below threshold
  hdiBudgetNeeded <- sum(hdiBudgetBelow$budget, na.rm = TRUE)
  print(hdiBudgetNeeded)
  # now find the remainder to be distributed as per usual
  s10_emi_end <- calcEndEmi(startYr, endYr, (sum(rDataInput$e_21, na.rm = TRUE)), (budget_remaining - hdiBudgetNeeded - (4.3648)))
  print(budget_remaining - hdiBudgetNeeded)
  s10_epc_end <- s10_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  pcc_n <- hdiBudgetBelow
  pcc_n <- mutate(pcc_n, pcc_n = budget + (calcNatB(startYr, endYr, pcc_n$e_21, 
                                                    (s10_epc_end * pcc_n$pop_50))))
  pcc_n <- arrange(pcc_n, desc(hdi))
}

# PCC-NHB qual-----
pcc_nhb <- function(rDataInput, budget_remaining = tot_budget){
  # split countries into 2 datasets, those above / below the threshold value
  c_above <- filter(rDataInput, hdi >= threshold)
  c_below <- filter(rDataInput, hdi < threshold)
  
  # determine expected emissions per capita at the threshold
  c_below <- arrange(c_below, desc(hdi))
  
  # define threshold epc
  hdiMinEpc <- as.numeric(c_below[1,"threshold_epc"])
  
  # assign new starting emissions 
  c_below$e_21 <- c_below$pop_21 * hdiMinEpc
  
  # recombine the split sets
  hdiGoalStart <- bind_rows(c_above, c_below)
  
  # calculate the budget as if all countries below threshold were at threshold-
  # determined emissions per capita; only keep the budget for countries below
  # the threshold
  s14_emi_end <- calcEndEmi(startYr, endYr, (sum(rDataInput$e_21, na.rm = TRUE)), budget_remaining)
  s14_epc_end <- s14_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  hdiGoalStart <- mutate(hdiGoalStart, 
                         budget = (calcNatB(startYr, endYr, hdiGoalStart$e_21,
                                            (s14_epc_end * hdiGoalStart$pop_50))))
  
  # split dataset again, to set budget of countries > t at 0
  c_above <- filter(hdiGoalStart, hdi >= threshold)
  c_below <- filter(hdiGoalStart, hdi < threshold)
  c_above$budget <- 0
  
  # recombine the split sets
  hdiBudgetBelow <- bind_rows(c_above, c_below)
  
  # calculate the initial budget for countries below threshold
  hdiBudgetNeeded <- sum(hdiBudgetBelow$budget, na.rm = TRUE)
  print(hdiBudgetNeeded)
  # now find the remainder to be distributed as per usual, additional sum is for
  # emissions embodied in K stock as well as hist numbers
  s14_emi_end <- calcEndEmi(histYr, endYr, (sum(rDataInput$e_95, na.rm = TRUE)), budget_remaining - 
                              hdiBudgetNeeded + (sum(rDataInput$k_95, na.rm = TRUE) * k_value) + 
                              sum(rDataInput$e_hist, na.rm = TRUE))
  
  s14_epc_end <- s14_emi_end / (sum(rDataInput$pop_50, na.rm = TRUE))
  
  pcc_nhb <- hdiBudgetBelow
  
  # number subtracted is the embodied emission per unit of K 
  pcc_nhb <- mutate(pcc_nhb, 
                    pcc_nhb = budget + (calcNatB(histYr, endYr, e_95, 
                                                 (s14_epc_end * pop_50)))
                    - (k_95 * k_value) - e_hist)
  
  pcc_nhb <- arrange(pcc_nhb, desc(hdi))
}
# EPC-BC qual (NUTS2) ------ 
nuts2_epc_bc <- function(budgets, rDataInput, goal){

  # empty dataset to hold values
  epc_bc <- rDataInput
  
  # calculate subnational budget for each country individually (which is
  # equivalent to the "world" for calculation's sake)
  for (nation in budgets$ISO){
    # subset for only the country of interest (nation)
    budgets_temp <- budgets %>% subset(ISO == nation)
    rDataInput_temp <- epc_bc %>% subset(ISO == nation)
    
    # make function to get the correct budget value for each country from the 
    # table of all budgets, depending on which global allocation approach
    # is preferred (and passed to the function from the main script, e.g.,
    # calling b(budgets, epc_nhbc) will return only the value of the nhbc
    # allocation)
    b <- function(df, name){
      eval(substitute(name), df)
    }
    
    world_budget <- (budgets_temp[[goal]] + (sum(rDataInput_temp$k_19, 
                                                      na.rm = TRUE) * k_value))# -
    #  sum(maxEffort$BurdenBuget, na.rm = TRUE))
    
    temp <- rDataInput_temp
    # number subtracted is the embodied emission per unit of K 
    
    # calculate the budget
    temp <- mutate(temp, epc_bc = (world_budget * 
                       (((pop_21 + pop_50)/2) / ((sum(rDataInput_temp$pop_21, na.rm = TRUE) +
                                               sum(rDataInput_temp$pop_50, na.rm = TRUE))/2)) -
                       (k_19 * k_value))) #+ maxEffort$BurdenBuget)
    
    # remove unneeded variables
    temp <- temp %>% dplyr::select(NUTS_ID, epc_bc)
    
    # join to the full dataset
    epc_bc <- epc_bc %>% left_join(temp, by = 'NUTS_ID') 
    
    # since after the first join, R wants to create multiple columns, this
    # checks if that situation occurs, and combines the duplicated 
    # erroneous columns into one
    if ('epc_bc.y' %in% names(epc_bc)){
      epc_bc <- epc_bc %>% mutate(epc_bc = coalesce(epc_bc.y, epc_bc.x)) %>%
        dplyr::select(-epc_bc.y, -epc_bc.x)
    }
  }
  return(epc_bc)
}

# EPC-H qual (NUTS2) ------ 
nuts2_epc_h <- function(budgets, rDataInput, goal){
  
  # empty dataset to hold values
  epc_h <- rDataInput
  # calculate subnational budget for each country individually (which is
  # equivalent to the "world" for calculation's sake)
  for (nation in budgets$ISO){
    # subset for only the country of interest (nation)
    budgets_temp <- budgets %>% subset(ISO == nation)
    rDataInput_temp <- epc_h %>% subset(ISO == nation)
    
    # make function to get the correct budget value for each country from the 
    # table of all budgets, depending on which global allocation approach
    # is preferred (and passed to the function from the main script, e.g.,
    # calling b(budgets, epc_nhbc) will return only the value of the nhbc
    # allocation)
    b <- function(df, name){
      eval(substitute(name), df)
    }
    
    world_budget <- (budgets_temp[[goal]] + (sum(rDataInput_temp$emi_hist, 
                                                 na.rm = TRUE) / 1000000000)) # divide it to make it Gt carbon

    temp <- rDataInput_temp
    # number subtracted is the embodied emission per unit of K 
    
    # calculate the budget
    temp <- mutate(temp, epc_h = (world_budget * 
                                     (((pop_2000 + pop_50)/2) / ((sum(rDataInput_temp$pop_2000, na.rm = TRUE) +
                                                                  sum(rDataInput_temp$pop_50, na.rm = TRUE))/2)) -
                                     (emi_hist / 1000000000))) # since emi_hist is in tonnes and we need Gt
    
    # remove unneeded variables
    temp <- temp %>% dplyr::select(NUTS_ID, epc_h)
    
    # join to the full dataset
    epc_h <- epc_h %>% left_join(temp, by = 'NUTS_ID') 
    
    # since after the first join, R wants to create multiple columns, this
    # checks if that situation occurs, and combines the duplicated 
    # erroneous columns into one
    if ('epc_h.y' %in% names(epc_h)){
      epc_h <- epc_h %>% mutate(epc_h = coalesce(epc_h.y, epc_h.x)) %>%
        dplyr::select(-epc_h.y, -epc_h.x)
    }
  }
  return(epc_h)
}
 