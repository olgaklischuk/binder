.libPaths("C:/Users/1/Documents/R/win-library/4.0")
if(comp="asus"){archive="zip"}else{archive="tr.gz"};install.packages(paste("http://grcon.r-forge.r-project.org/files/gEcon_1.2.0."),archive,sep=""))
install.packages(paste("http://grcon.r-forge.r-project.org/files/gEcon.iosam_0.2.0.",archive,sep=""))
install.packages(paste("http://grcon.r-forge.r-project.org/files/gEcon.estimation_0.1.0.",archive,sep=""))
library(gEcon)
library(gEcon.iosam)
library(gEcon.estimation)

# copy the example to the current working directory####
if(Sys.info()[4]=="MacBook-Air-Ulia.local"){comp="mac";OneDrive<-"~/OneDrive/"}else{comp="asus";OneDrive<-"C:/Users/1/OneDrive/"};d<-paste(lubridate::day(format(Sys.time(),"%Y-%m-%d"))-1,".0",lubridate::month(Sys.time()),sep="")
if(comp=="asus"){load(paste("C:/Users/1/OneDrive/DSGE main/Result/","SW_dsge.",d,".RData",sep=""))}else{load(paste("~/OneDrive/","DSGE main/Result/","SW_dsge.",d,".RData",sep=""))}
if(comp=="asus"){disk="OneDrive"
path<-ifelse(disk=="OneDrive","C:/Users/1/OneDrive/DSGE main/models/SW_03/SW_03.gcn","J:/Model-листопад 2017/Equilibrium model/SW_model.gcn")
info__ <- c("SW_model", path, format(Sys.time(),"%d.%m.%Y %H:%M"))}else{disk="OneDrive"
path<-ifelse(disk=="OneDrive","~/OneDrive/Computer/DSGE main/models/SW_03/SW_03.gcn","~/OneDrive/Computer/DSGE main/models/SW_03/SW_03.gcn")
info__ <- c("SW_model", path, format(Sys.time(),"%d.%m.%Y %H:%M"))}
# if(disk=="OneDrive"){path<-c("C:/Users/1/OneDrive/DSGE main/models/SW_03")}else{path<-"I:/9.5.Central Banks/case 2/simulations/examples/SW_03"}
gsn<-ifelse(disk=="OneDrive",path,
            "I:/9.5.Central Banks/case 2/simulations/examples")
if(!comp=="asus")setwd("~/OneDrive/DSGE main/models/SW_03/")else{
setwd("C:/Users/1/OneDrive/DSGE main/models/SW_03/")}
SW_dsge_model <- gEcon::make_model("SW_03.gcn")

initv <- list(z=1,z_f=1,Q=1,Q_f=1,pi=1,pi_obj=5,
epsilon_b=1,epsilon_L=1,epsilon_I=1,epsilon_a=1,epsilon_G=1,
                  r_k=0.01,r_k_f=0.01)
SW_dsge_model <- initval_var( SW_dsge_model , init_var =initv )
# solve the model####
SW_dsge_model_ss <- gEcon::steady_state(SW_dsge_model)
get_ss_values(SW_dsge_model_ss)
SW_dsge_model_pert <- gEcon::solve_pert(SW_dsge_model_ss, loglin = TRUE)
summary(SW_dsge_model_pert)
# set the stochastic shocks distribution parameters####
#shocks' variences####
a<-list(eta_b=.336^2,
        eta_L=3.52^2,
        eta_I=.085^2,
        eta_a=.598^2,
        eta_w=.6853261^2,
        eta_p=.7896512^2,
        eta_G=.325^2,
        eta_R=.081^2,
        eta_pi=.017^2)

cbind(SW_dsge_model@parameters,SW_dsge_model@parameters_free)
SW_dsge_model@shocks

SW_dsge_model_shocks <- gEcon::set_shock_distr_par(SW_dsge_model_pert,
                                         distr_par = list("sd(eta_b)" = 0.01,
                                                          "sd(eta_L)" = 0.01,
                                                          "sd(eta_I)" = 0.01,
                                                          "sd(eta_a)" = 0.01,
                                                          "sd(eta_G)" = 0.01))
SW_dsge_model_shocks_cov <- gEcon::set_shock_cov_mat(SW_dsge_model_shocks,diag(a),names(a))
gEcon::shock_info(model = SW_dsge_model_shocks_cov, all = TRUE)
SW_dsge_model_shocks_cov@shock_cov_mat;SW_dsge_model_shocks_cov@corr_mat;SW_dsge_model_shocks_cov@autocorr_mat;SW_dsge_model_shocks_cov@ref_var_corr_mat;SW_dsge_model_shocks_cov@var_dec;SW_dsge_model_shocks_cov@sdev
##moments####
 SW_dsge_model_computed_stats<-compute_model_stats(SW_dsge_model_shocks_cov)
get_model_stats(SW_dsge_model_computed_stats)
#Shocks irf of var####
SW_dsge_model_irf<-compute_irf(SW_dsge_model_computed_stats, variables=c("C","Y","K","I","L","pi"),cholesky=T, shocks=c("eta_a","eta_R","eta_p"), sim_length=48)
plot_simulation(SW_dsge_model_irf)
# ###################################################################
# 2. simulate the model to obtain data for the estimation###########

# choose variables of interest####
set.seed(250)
series_length <- 165
observables <- c("C","Y","K","I","L","pi")

# simulate random path####
SW_dsge_simulation <- gEcon::random_path(model = SW_dsge_model,
                                      sim_length = series_length,
                                      variables = observables)
model_data <- gEcon::get_simulation_results(SW_dsge_simulation)
model_data_1<-t(model_data)
# create data set to be used for estimation (ts object)####
estimation_data <- ts(data = t(model_data)[, observables],
                      start = c(2007, 1),
                      frequency = 12, names = observables);estimation_data

# remove mean from the data series####
mean_var <- matrix(apply(estimation_data, 2, mean),
                   byrow = TRUE,
                   nrow = nrow(estimation_data),
                   ncol = ncol(estimation_data));tail(mean_var)
estimation_data <- estimation_data  - mean_var

# ###################################################################
# 3. declare prior distribution####

SW_dsge_prior <- gEcon.estimation::gecon_prior(
  prior_list = list(
    ##################################################
    # list(par = "alpha", type = "inv_gamma",
    #      mean = 0.012, sd = 0.3, lower_bound = 0.0001,
    #      upper_bound  = 0.9, initial = 0.0012),
    # list(par = "beta", type = "inv_gamma",
    #      mean = 0.008, sd = 0.3, lower_bound = 0.0001,
    #      upper_bound  = 0.9, initial = 0.006),
    # list(par = "calibr_pi", type = "normal",
    #      mean = 13.92452, sd = 0.1, lower_bound = 1,
    #      upper_bound  = 2, initial = 1.5),
    # list(par = "calibr_pi_obj", type = "normal",
    #      mean = 4.88, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "gamma_w", type = "normal",
    #      mean = 1.45, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "gamma_p", type = "normal",
    #      mean = 5.00, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "h", type = "beta",
    #      mean = 0.336, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "lambda_w", type = "beta",
    #      mean = 3.52, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "lambda_p", type = "inv_gamma",
    #      mean = 0.085, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    ##################################################
    list(par = "omega", type = "inv_gamma",
         mean = 0.598, sd = 0.03, lower_bound = 0.5,
         upper_bound  = 0.999, initial = 0.95),
    list(par = "psi", type = "inv_gamma",
         mean = 0.325, sd = 0.3, lower_bound = 0.0001,
         upper_bound  = 0.999, initial = 0.006),
    list(par = "r_pi", type = "beta",
         mean = 0.081, sd = 0.03, lower_bound = 0.5,
         upper_bound  = 0.999, initial = 0.95),
    list(par = "r_Y", type = "beta",
         mean = 0.01, sd = 0.03, lower_bound = 0.5,
         upper_bound  = 0.999, initial = 0.95),
    list(par = "r_Delta_pi", type = "beta",
         mean = 0.01, sd = 0.03, lower_bound = 0.5,
         upper_bound  = 0.999, initial = 0.95),
    list(par = "r_Delta_y", type = "beta",
         mean = 0.92, sd = 0.03, lower_bound = 0.5,
         upper_bound  = 0.999, initial = 0.95)
    ###############################################
    # ,list(par = "rho_b", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "rho_L", type = "beta",
    #      mean = 1., sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "rho_I", type = "beta",
    #      mean = 0.92, sd =0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "rho_a", type = "beta",
    #      mean = 0.98, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "rho_G", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "rho", type = "beta",
    #      mean = 0.99, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "rho_pi_bar", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "sigma_c", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "sigma_l", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "tau", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "varphi", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "xi_w", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "xi_p", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "G_bar", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95),
    # list(par = "Phi", type = "beta",
    #      mean = 0.92, sd = 0.03, lower_bound = 0.5,
    #      upper_bound  = 0.999, initial = 0.95)
    #################################################
    ),
  model = SW_dsge_model)
par(mai=c(.1,.1,.1,.2),mar=c(1,1,1,1),cex=.5)
gEcon.estimation::plot_prior((SW_dsge_prior))+ggthemes::theme_economist_white()

# ###################################################################
# 4. estimate the model (Bayesian estimation)####

estimation_result <- gEcon.estimation::bayesian_estimation(data_set = estimation_data[,1:6],
                                                           optim_options_list = list(solver = "csminwel"),
                                                           mcmc_options_list = list(chain_length = 1000,
                                                                                    burn = 200,
                                                                                    cores = 2, chains = 2,
                                                                                    scale = rep(0.5, 30)),
                                                           observables =  observables[1:6],
                                                           model = SW_dsge_model,
                                                           prior = SW_dsge_prior)


setwd("C:/Users/1/OneDrive/DSGE main/plots")
gEcon.estimation::plot_posterior(estimation_result)
# retrieve estimates####
#
# true model parameters were:
#eta_b 0.855
#eta_L 0.889
#eta_R
#eta_pi
#eta_G
#eta_p
#eta_a
#eta_w
#eta_I
#rho_b
#rho_L
#rho_a
#rho_G

# sd(epsilon_Z) 0.010428485
# sd(epsilon_G) 0.008431751
# omega         1.430057680# Labour disutility parameter
# phi_G         0.858202844
# phi_Z         0.868186453
# psi           0.169       # Capacity utilisation cost parameter

est_par <- gEcon.estimation::get_estimated_par(estimation_result)
free_par <- est_par$free_par;free_par
shock_distr_par <- est_par$shock_distr_par;shock_distr_par
estimated_SW_dsge_model <- gEcon::set_free_par(SW_dsge_model, free_par = free_par);estimated_SW_dsge_model
estimated_SW_dsge_model <- gEcon::set_shock_distr_par(estimated_SW_dsge_model, distr_par = shock_distr_par)

estimated_SW_dsge_model <- gEcon::steady_state(estimated_SW_dsge_model)
estimated_SW_dsge_model <- gEcon::solve_pert(estimated_SW_dsge_model, loglin = TRUE)

# ###################################################################
# 5. historical shock decomposition and variable smoothing####

# find historical shock decomposition
SW_dsge_shock_decomp <- gEcon.estimation::shock_decomposition(model = estimated_SW_dsge_model,
                                                           data_set = window(estimation_data,
                                                                             start = c(2017, 11),
                                                                             end = c(2019, 10),
                                                                             frequency = 12),
                                                           observables = observables,
                                                           variables = observables)
gEcon.estimation::plot_shock_decomposition(SW_dsge_shock_decomp)

# use Kalman smoother to obtain smoothed variables' values
SW_dsge_smoothed_variables <- gEcon.estimation::smoother(model = estimated_SW_dsge_model,
                                                      data_set = estimation_data,
                                                      # observables = c(observables),
                                                      # variables = c("K", "I", "C","pi")
                                                      );SW_dsge_smoothed_variables

# print smoothed shocks' values
SW_dsge_smoothed_variables$smoothed_shock
# print smoothed variables' values
SW_dsge_smoothed_variables$smoothed_var
# print the MSE matrix
SW_dsge_smoothed_variables$MSE

# ###################################################################
# 6. forecast using the model#######################################

# forecast using point estimates of parameters
fc_res <- gEcon.estimation::forecast(model = estimated_SW_dsge_model,
                                     data_set = estimation_data,
                                     observables = observables,
                                     variables = c("Y", "G","pi"),
                                     horizon = 24)

# forecast using posterior distribution
fc_res_post <- gEcon.estimation::forecast_posterior(est_results = estimation_result,
                                                    data_set = estimation_data,
                                                    observables = observables,
                                                    variables = c("Y", "G","pi"),
                                                    horizon = 24)

# plot forecasts
gEcon.estimation::plot_forecast(fc_res_post)
vplot_forecast(fc_res)
#results####
model_data
observables
series_length
shock_distr_par
SW_dsge_model;estimated_SW_dsge_model
SW_dsge_prior
SW_dsge_shock_decomp
SW_dsge_simulation
SW_dsge_smoothed_variables
est_par; estimation_data; estimation_resul
fc_res;fc_res_post;free_par
mean_var
#@saving####
d<-format(Sys.Date(),"%d.%m")
save.image(paste(OneDrive,"DSGE main/Result/","SW_dsge.",d,".RData",sep=""))
rm(list=ls())
q(save="no",1,FALSE)
q(save="yes")
