library(tidyverse)
library(scales)
#install.packages('tidyverse')
#install.packages('scales')

#Number of years
n_years=10

# Assumptions (copied and simplified)
netscape_assumptions <- list(
  revenue_growth = rep(0.65,n_years),
  cost_of_sales_pct = rep(0.1044,n_years),
  rd_pct = rep(0.3676,n_years),
  tax_rate = rep(0.34,n_years),
  other_op_exp_pct = c(0.80,0.65,0.55,0.45,0.35,0.25,rep(0.2,4)),
  capex_pct = c(0.45,0.4,0.3,0.2,rep(0.1,6)),
  nwc_pct = rep(0,n_years),
  depreciation_pct = rep(0.055,n_years),
  beta=rep(1.5,n_years),
  rf=rep(0.0671,n_years),
  mrp=rep(0.075,n_years),
  shares_outstanding = 38000,
  terminal_growth = 0.04,
  terminal_r=0.1796
)

# Base year (1995)
netscape_base_rev <- 33250

# Projection over 10 years
get_projection <- function(assumptions,base){
  
  projection=data.frame(
      Year = 1:n_years,
      Revenue = NA,
      EBIT = NA,
      Taxes = NA,
      NOPAT = NA,
      Depreciation = NA,
      CAPEX = NA,
      NWC = NA,
      Delta_NWC = NA,
      Discount_Rate=NA)

  # Fill in projections for revenue
  for (t in 1:n_years) {
    if (t == 1) {
      projection$Revenue[t] <- base * (1 + assumptions$revenue_growth[t])
    } else {
      projection$Revenue[t] <- projection$Revenue[t - 1] * (1 + assumptions$revenue_growth[t])
    }
  }
  
  #Fill in FCF terms
  projection$EBIT <- projection$Revenue * (1 - assumptions$cost_of_sales_pct - assumptions$rd_pct - assumptions$other_op_exp_pct - assumptions$depreciation_pct)
  projection$Taxes <- projection$EBIT * assumptions$tax_rate
  projection$NOPAT <- projection$EBIT - projection$Taxes
  projection$Depreciation <- projection$Revenue * assumptions$depreciation_pct
  projection$CAPEX <- projection$Revenue * assumptions$capex_pct
  projection$NWC <- projection$Revenue*assumptions$nwc_pct
  projection$Delta_NWC <- 0
  projection$Discount_Rate <- 1/(1+(assumptions$rf+assumptions$mrp*assumptions$beta))^projection$Year
  
  return(projection)
} 

get_dcf <- function(projection){
  
  dcf=projection%>%
    mutate(FCF = NOPAT + Depreciation - CAPEX - Delta_NWC)%>%
    reframe(DCF=FCF*Discount_Rate)%>%
    pull(DCF)%>%
    sum()
  
  return(dcf)
  
}


get_terminal_value <- function(projection,r,g){

  last_fcf=projection%>%
    mutate(FCF = NOPAT + Depreciation - CAPEX - Delta_NWC)%>%
    pull(FCF)%>%
    tail(1)
  
  return(last_fcf*(1+g)/(r-g)/(1+r)^(n_years+1))
}    

get_projection(netscape_assumptions,netscape_base_rev)%>%get_dcf()
get_projection(netscape_assumptions,netscape_base_rev)%>%
  get_terminal_value(g=netscape_assumptions$terminal_growth,
                     r=netscape_assumptions$terminal_r)

get_EV <-function(assumptions,base,r,g){

  PV_FCF = get_projection(assumptions,base)%>%get_dcf()
  PV_Terminal = get_projection(assumptions,base)%>%get_terminal_value(r,g)
  
  return(PV_FCF+PV_Terminal)

  }

get_EV(netscape_assumptions,
       netscape_base_rev,
       netscape_assumptions$terminal_r,
       netscape_assumptions$terminal_growth)




# Exercise 1: changing revenue growth ------------------------------------

  # Initialize an empty dataframe
  results <- numeric(0)
  sim_assumptions <- netscape_assumptions
  n_sim=10000
  
  #Run the simulation
  for (i in 1:n_sim){

    # Simulate revenue growth for 10 years from a normal distribution
    rev_growth <- rnorm(n_years, mean = 0.65, sd = 0.1)
    
    # Modify assumptions with simulated growth
    sim_assumptions$revenue_growth <- rev_growth
    
    # Compute EV for this simulation
    ev <- get_EV(sim_assumptions,
                 netscape_base_rev,
                 sim_assumptions$terminal_r,
                 sim_assumptions$terminal_growth)
    
    results[i] <- ev
  }
  

  #Create a ggplot histogram
  results%>%
    as.tibble()%>%
    ggplot(aes(x = value)) +
    geom_histogram(fill = "skyblue", color = "white", bins = 50)+
    scale_x_continuous(labels = scales::dollar) +
    labs(
      title = "Monte Carlo Simulation of Enterprise Value",
      subtitle = paste0('Drawing ',comma(n_sim), ' simulations.'), 
      x = "Enterprise Value (USD, Thousands)",
      y = "Frequency"
    ) +
    theme_minimal(base_size = 14)

              
# Exercise 2: Revenue and COGS --------------------------------------------

# Initialize an empty dataframe
results <- numeric(0)
sim_assumptions <- netscape_assumptions
n_sim=10000

#Run the simulation
for (i in 1:n_sim){
  
  # Simulate revenue growth for 10 years from a normal distribution
  rev_growth <- rnorm(n_years, mean = 0.65, sd = 0.1)
  cogs_perc <- rnorm(n_years, mean = 0.10, sd = 0.025)
  
  # Modify assumptions with simulated growth
  sim_assumptions$revenue_growth <- rev_growth
  sim_assumptions$cost_of_sales_pct <- cogs_perc
  
  # Compute EV for this simulation
  ev <- get_EV(sim_assumptions,
               netscape_base_rev,
               sim_assumptions$terminal_r,
               sim_assumptions$terminal_growth)
  
  results[i] <- ev
}

#Create a ggplot histogram
results%>%
  as.tibble()%>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 50)+
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Monte Carlo Simulation of Enterprise Value",
    subtitle = paste0('Drawing ',comma(n_sim), ' simulations.'), 
    x = "Enterprise Value (USD, Thousands)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)



# Exercise 3: Revenue, COGS, and CAPEX --------------------------------------------

# Initialize an empty dataframe
results <- numeric(0)
sim_assumptions <- netscape_assumptions
n_sim=1000

#Run the simulation
for (i in 1:n_sim){
  
  # Simulate revenue growth for 10 years from a normal distribution
  rev_growth <- rnorm(n_years, mean = 0.65, sd = 0.1)
  cogs_perc <- rnorm(n_years, mean = 0.10, sd = 0.025)
  capex_perc <- seq(rnorm(1, mean = 0.45, sd = 0.1),to=0.1,length.out = 10)
  
  # Modify assumptions with simulated growth
  sim_assumptions$revenue_growth <- rev_growth
  sim_assumptions$cost_of_sales_pct <- cogs_perc
  sim_assumptions$capex_pct <- capex_perc
  
  # Compute EV for this simulation
  ev <- get_EV(sim_assumptions,
               netscape_base_rev,
               sim_assumptions$terminal_r,
               sim_assumptions$terminal_growth)
  
  results[i] <- ev
}


#Create a ggplot histogram
results%>%
  as.tibble()%>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 50)+
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Monte Carlo Simulation of Enterprise Value",
    subtitle = paste0('Drawing ',comma(n_sim), ' simulations.'), 
    x = "Enterprise Value (USD, Thousands)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

#Assessing the firm value for % of scenarios
quantile(results,seq(0,1,0.1))



# Exercise 4: Different levels of terminal r and g ------------------------

# Initialize an empty dataframe
results <- numeric(0)
new_data <- data.frame()
sim_assumptions <- netscape_assumptions
n_sim=1000
g_sequence <- seq(0.05,0.03,length.out=10)
r_sequence <- seq(0.15,0.225,length.out=10)

for(s in 1:10){
  
  #Run the simulation
  for (i in 1:n_sim){
    
    # Simulate revenue growth for 10 years from a normal distribution
    rev_growth <- rnorm(n_years, mean = 0.65, sd = 0.1)
    cogs_perc <- rnorm(n_years, mean = 0.10, sd = 0.025)
    capex_perc <- seq(rnorm(1, mean = 0.45, sd = 0.1),to=0.1,length.out = 10)
    
    # Modify assumptions with simulated growth
    sim_assumptions$revenue_growth <- rev_growth
    sim_assumptions$cost_of_sales_pct <- cogs_perc
    sim_assumptions$capex_pct <- capex_perc
    sim_assumptions$terminal_growth = g_sequence[s]
    sim_assumptions$terminal_r = r_sequence[s]
    
    # Compute EV for this simulation
    ev <- get_EV(sim_assumptions,
                 netscape_base_rev,
                 sim_assumptions$terminal_r,
                 sim_assumptions$terminal_growth)
    
    results[i] <- ev
  }
  
  #Store the Pairs
  new_data=new_data%>%
    rbind(data.frame(Scenario = paste0('Scenario ', s),
                     EV=results))
          
  message(paste0('Finished simulation for pair number ',s,'.'))
}


#install.packages('ggridges')
library(ggridges)

#Create a ggplot histogram
new_data%>%
  mutate(Scenario = factor(Scenario,levels=paste0('Scenario ',10:1)))%>%
  ggplot(aes(x = EV,
             y = Scenario,
             fill=..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  scale_x_continuous(labels = scales::dollar) +
  scale_fill_viridis_c(name = "Enterprise Value", option = "plasma",labels=scales::dollar)+
  labs(
    title = "Monte Carlo Simulation of Enterprise Value",
    subtitle = paste0('Drawing ',comma(n_sim), ' simulations, best-to-worse scenarios.'), 
    x = "",
    y = "Frequency",
    fill = "Enterprise Value (USD, Thousands)")+
  theme_minimal(base_size = 14)+
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width = unit(1.5, "cm"))

#Assessing the firm value for % of scenarios
quantile(results,seq(0,1,0.1))

# Simulating independently distributed variables ----------------------------------------------------

# Initialize an empty dataframe
indep_results <- numeric(0)
sim_assumptions <- netscape_assumptions
n_sim=10000

#Run the simulation
for (i in 1:n_sim){
  
  # Simulate revenue growth for 10 years from a normal distribution
  rev_growth <- rnorm(n_years, mean = 0.65, sd = 0.1)
  cogs_perc <- rnorm(n_years, mean = 0.10, sd = 0.025)
  capex_perc <- seq(rnorm(1, mean = 0.45, sd = 0.1),to=0.1,length.out = 10)
  r_terminal <- rnorm(1, mean = 0.1796, sd = 0.015)
  g_terminal <- rnorm(1, mean = 0.03, sd = 0.005)
  
  # Modify assumptions with simulated growth
  sim_assumptions$revenue_growth <- rev_growth
  sim_assumptions$cost_of_sales_pct <- cogs_perc
  sim_assumptions$capex_pct <- capex_perc
  sim_assumptions$terminal_growth = g_terminal
  sim_assumptions$terminal_r = r_terminal
  
  # Compute EV for this simulation
  ev <- get_EV(sim_assumptions,
               netscape_base_rev,
               sim_assumptions$terminal_r,
               sim_assumptions$terminal_growth)
  
  indep_results[i] <- ev
}


#Create a ggplot histogram
indep_results%>%
  as.tibble()%>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 50)+
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Monte Carlo Simulation of Enterprise Value",
    subtitle = paste0('Drawing ',comma(n_sim), ' simulations. Terminal growth and discount assumed to be i.i.d'), 
    x = "Enterprise Value (USD, Thousands)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)


# Simulating correlated variables ----------------------------------------------------

# Initialize an empty dataframe
corr_results <- numeric(0)
sim_assumptions <- netscape_assumptions
n_sim=10000

# Means and standard deviations for r and g
mu <- c(r = 0.1796, g = 0.03)
sd_r <- 0.015
sd_g <- 0.005
rho <- -0.8  # specified correlation

# Covariance matrix
sigma <- matrix(c(sd_r^2,
                  rho * sd_r * sd_g,
                  rho * sd_r * sd_g,
                  sd_g^2
), nrow = 2)

# Simulate 10000 draws
sim_bivariate <- mvrnorm(n = n_sim, mu = mu, Sigma = sigma)

#Run the simulation
for (i in 1:n_sim){
  
  # Simulate revenue growth for 10 years from a normal distribution
  rev_growth <- rnorm(n_years, mean = 0.65, sd = 0.1)
  cogs_perc <- rnorm(n_years, mean = 0.10, sd = 0.025)
  capex_perc <- seq(rnorm(1, mean = 0.45, sd = 0.1),to=0.1,length.out = 10)

  # Modify assumptions with simulated growth
  sim_assumptions$revenue_growth <- rev_growth
  sim_assumptions$cost_of_sales_pct <- cogs_perc
  sim_assumptions$capex_pct <- capex_perc
  sim_assumptions$terminal_r = sim_bivariate[i,1]
  sim_assumptions$terminal_growth = sim_bivariate[i,2]
  
  # Compute EV for this simulation
  ev <- get_EV(sim_assumptions,
               netscape_base_rev,
               sim_assumptions$terminal_r,
               sim_assumptions$terminal_growth)
  
  corr_results[i] <- ev
}

#Create a ggplot histogram
corr_results%>%
  as.tibble()%>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 50)+
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Monte Carlo Simulation of Enterprise Value",
    subtitle = paste0('Drawing ',comma(n_sim), ' simulations. Terminal growth and discount taken from a bivariate normal distribution with rho = -0.8'), 
    x = "Enterprise Value (USD, Thousands)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

#Chart
rbind(data.frame(Model='i.i.d',Values=indep_results),
      data.frame(Model='Bivariate Normal',Values=corr_results))%>%
  ggplot(aes(x = Values,color=Model)) +
  geom_density(aes(y=after_stat(count)),size=1)+
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Monte Carlo Simulation of Enterprise Value",
    subtitle = paste0('Drawing ',comma(n_sim), ' simulations. Terminal growth and discount taken from a bivariate normal distribution with rho = -0.8'), 
    x = "Enterprise Value (USD, Thousands)",
    y = "Frequency")+
  theme_minimal(base_size = 14)+
  theme(legend.position='bottom')


# Question: givn that the stock price is $28.37, in how many potential scenarios such stock is undervalued? --------


#Create a ggplot histogram
prices=corr_results%>%
  as.tibble()%>%
  mutate(Price=value/netscape_assumptions$shares_outstanding)%>%
  mutate(Situation=ifelse(Price>=28.37,'Undervalued','Overvalued'))

prices%>%
  ggplot(aes(x = Price)) +
  geom_histogram(aes(fill = Situation), bins = 50)+
  scale_x_continuous(labels = scales::dollar)+
  scale_fill_manual(values=c('darkgreen','darkred'))+
  geom_vline(xintercept=28.37,linetype='dashed',size=1)+
  annotate(geom='text',x=50,y=150,label= paste0('True Price is higher than \nbaseline price only in ',
                                                percent(mean(prices$Price>=28.37)),
                                                ' of the cases!'))+
  labs(
    title = "Monte Carlo Simulation of Share Price",
    subtitle = paste0('Drawing ',comma(n_sim), ' simulations. Terminal growth and discount taken from a bivariate normal distribution with rho = -0.5'), 
    x = "Enterprise Value (USD, Thousands)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)+
  theme(legend.position='bottom')


  