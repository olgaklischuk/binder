.libPaths("C:/Users/1/Documents/R/win-library/4.0")
library(gEcon)
library(gEcon.iosam)
library(gEcon.estimation)

# copy the example to the current working directory####
if(Sys.info()[4]=="MacBook-Air-Ulia.local"){comp="mac"}else{comp="asus"}
if(comp=="asus"){load(paste("C:/Users/1/OneDrive/DSGE main/Result/","dsge.",d,".RData",sep=""))}else{load(paste("~/OneDrive/","DSGE main/Result/","dsge.",d,".RData",sep=""))}
if(comp=="asus"){disk="OneDrive"
path<-ifelse(disk=="OneDrive","C:/Users/1/OneDrive/DSGE main/dsge_model.gcn","J:/Model-листопад 2017/Equilibrium model/dsge_model.gcn")
info__ <- c("dsge_model", path, "2018-05-07 12:36:19")}else{disk="OneDrive"
path<-ifelse(disk=="OneDrive","~/OneDrive/Computer/DSGE main/dsge_model.gcn","~/OneDrive/Computer/DSGE main/dsge_model.gcn")
info__ <- c("dsge_model", path, "2018-05-07 12:36:19")}
if(disk=="OneDrive"){path<-c("C:/Users/1/OneDrive/DSGE main")}else{path<-"I:/9.5.Central Banks/case 2/simulations/examples"}
gsn<-ifelse(disk=="OneDrive",path,
                    "I:/9.5.Central Banks/case 2/simulations/examples")
file.copy(from = file.path(gsn, "dsge_model.gcn"), to = getwd())
setwd("C:/Users/1/OneDrive/DSGE main")

dsge_model <- gEcon::make_model("dsge_model.gcn")

# solve the model####
dsge_model <- gEcon::steady_state(dsge_model)
dsge_model <- gEcon::solve_pert(dsge_model, loglin = TRUE)

# set the stochastic shocks distribution parameters####
dsge_model <- gEcon::set_shock_distr_par(dsge_model,
                                  distr_par = list("sd( epsilon_G )" = 0.01,
                                                   "sd( epsilon_Z )" = 0.01))
gEcon::shock_info(model = dsge_model, all = TRUE)

# ###################################################################
# 2. simulate the model to obtain data for the estimation###########

# choose variables of interest####
set.seed(250)
series_length <- 150
observables <- c("Y", "G")

# simulate random path####
dsge_simulation <- gEcon::random_path(model = dsge_model,
                               sim_length = series_length,
                               variables = observables)
model_data <- gEcon::get_simulation_results(dsge_simulation)

# create data set to be used for estimation (ts object)####
estimation_data <- ts(data = t(model_data)[, observables],
                      start = c(1973, 1),
                      frequency = 4, names = observables);estimation_data

# remove mean from the data series####
mean_var <- matrix(apply(estimation_data, 2, mean),
                   byrow = TRUE,
                   nrow = nrow(estimation_data),
                   ncol = ncol(estimation_data));tail(mean_var)
estimation_data <- estimation_data  - mean_var

# ###################################################################
# 3. declare prior distribution####

dsge_prior <- gEcon.estimation::gecon_prior(
  prior_list = list(
    list(par = "sd(epsilon_Z)", type = "inv_gamma",
         mean = 0.012, sd = 0.3, lower_bound = 0.0001,
         upper_bound  = 0.9, initial = 0.0012),
    list(par = "sd(epsilon_G)", type = "inv_gamma",
         mean = 0.008, sd = 0.3, lower_bound = 0.0001,
         upper_bound  = 0.9, initial = 0.006),
    list(par = "omega", type = "normal",
         mean = 1.45, sd = 0.1, lower_bound = 1,
         upper_bound  = 2, initial = 1.5),
    list(par = "phi_G", type = "beta",
         mean = 0.88, sd = 0.03, lower_bound = 0.5,
         upper_bound  = 0.999, initial = 0.95),
    list(par = "phi_Z", type = "beta",
         mean = 0.92, sd = 0.03, lower_bound = 0.5,
         upper_bound  = 0.999, initial = 0.95)),
  model = dsge_model)

gEcon.estimation::plot_prior((dsge_prior))+ggthemes::theme_economist_white()

# ###################################################################
# 4. estimate the model (Bayesian estimation)####

estimation_result <- gEcon.estimation::bayesian_estimation(data_set = estimation_data[,1:2],
                                         optim_options_list = list(solver = "csminwel"),
                                         mcmc_options_list = list(chain_length = 1000,
                                                                  burn = 200,
                                                                  cores = 2, chains = 2,
                                                                  scale = rep(0.5, 5)),
                                         observables =  observables[1:2],
                                         model = dsge_model,
                                         prior = dsge_prior)


setwd("C:/Users/1/OneDrive/DSGE main/plots")
gEcon.estimation::plot_posterior(estimation_result)
# retrieve estimates####
#
# true model parameters were:
# sd(epsilon_Z) 0.01
# sd(epsilon_G) 0.01
# omega         1.45
# phi_G         0.9
# phi_Z         0.9
est_par <- gEcon.estimation::get_estimated_par(estimation_result)
free_par <- est_par$free_par;free_par
shock_distr_par <- est_par$shock_distr_par;shock_distr_par
estimated_dsge_model <- gEcon::set_free_par(dsge_model, free_par = free_par);estimated_dsge_model
estimated_dsge_model <- gEcon::set_shock_distr_par(estimated_dsge_model, distr_par = shock_distr_par)

estimated_dsge_model <- gEcon::steady_state(estimated_dsge_model)
estimated_dsge_model <- gEcon::solve_pert(estimated_dsge_model, loglin = TRUE)

# ###################################################################
# 5. historical shock decomposition and variable smoothing####

# find historical shock decomposition
dsge_shock_decomp <- gEcon.estimation::shock_decomposition(model = estimated_dsge_model,
                                         data_set = window(estimation_data,
                                                           start = c(2004, 1),
                                                           end = c(2010, 1),
                                                           frequency = 4),
                                         observables = observables,
                                         variables = observables)
gEcon.estimation::plot_shock_decomposition(dsge_shock_decomp)

# use Kalman smoother to obtain smoothed variables' values
dsge_smoothed_variables <- gEcon.estimation::smoother(model = estimated_dsge_model,
                                    data_set = estimation_data,
                                    observables = c("Y", "G"),
                                    variables = c("K", "I", "C"));dsge_smoothed_variables

# print smoothed shocks' values
dsge_smoothed_variables$smoothed_shock
# print smoothed variables' values
dsge_smoothed_variables$smoothed_var
# print the MSE matrix
dsge_smoothed_variables$MSE

# ###################################################################
# 6. forecast using the model#######################################

# forecast using point estimates of parameters
fc_res <- gEcon.estimation::forecast(model = estimated_dsge_model,
                   data_set = estimation_data,
                   observables = observables,
                   variables = c("Y", "G"),
                   horizon = 20)

# forecast using posterior distribution
fc_res_post <- gEcon.estimation::forecast_posterior(est_results = estimation_result,
                                  data_set = estimation_data,
                                  observables = observables,
                                  variables = c("Y", "G"),
                                  horizon = 20)

# plot forecasts
gEcon.estimation::plot_forecast(fc_res_post)
vplot_forecast(fc_res)
#####
model_data
observables
series_length
shock_distr_par
dsge_model;estimated_dsge_model
dsge_prior
dsge_shock_decomp
dsge_simulation
dsge_smoothed_variables
est_par; estimation_data; estimation_resul
fc_res;fc_res_post;free_par
mean_var
#####
d<-format(Sys.Date(),"%d.%m")
save.image(paste("C:/Users/1/OneDrive/DSGE main/Result/","dsge.",d,".RData",sep=""))
rm(list=ls())
q(save="no",1,FALSE)
q(save="yes")
