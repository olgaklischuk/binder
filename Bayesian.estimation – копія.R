mcmc_options=list(chain_length = 1000,
burn = 200,
cores = 2, chains = 2,
scale = rep(0.5, 5))
mcmc_options$scale;length(get_estimated_par_names(SW_dsge_prior))
bayesian_estimation<-
function (data_set, observables, model, prior, optim_options_list = NULL,
          mcmc_options_list = NULL)
{
  real_start <- Sys.time()
  if (!is.gecon_model(model))
    stop("the model argument has to be of gecon_model class")
  if ((!is.ts(data_set) & !is.mts(data_set)))
    stop("the data_set argument has to be of (m)ts class")
  if (!is.character(observables))
    stop("the observables argument has to be a character vector")
  if (class(prior) != "gecon_prior")
    stop("the prior argument must be an object of gecon_prior class")
  val_data <- validate_data(model, data_set, observables = observables)
  obs_var_indices <- val_data$obs_var_indices
  observables <- val_data$observables
  series_start <- val_data$series_start
  series_freq <- val_data$series_freq
  data_set <- val_data$data_set
  n_obs <- val_data$n_obs
  n_var <- val_data$n_var
  validate_model(model)
  if ((!is.null(mcmc_options_list)) & (!is.list(mcmc_options_list)))
    stop("the mcmc_options_list argument has to be a list or a NULL value")
  mcmc_options <- list(chains = 2, cores = 2, burn = 200, chain_length = 500,
                       scale = rep(1, length(get_estimated_par_names(prior))),
                       output_freq = 250, avoid_model_recalculation = FALSE)
  if (!is.null(mcmc_options_list)) {
    fitting_options <- which(names(mcmc_options_list) %in%
                               names(mcmc_options))
    if (length(fitting_options) < length(mcmc_options_list)) {
      warning("unrecognised options were specified for posterior simulation")
    }
    mo <- match(names(mcmc_options_list[fitting_options]),
                names(mcmc_options))
    mcmc_options[mo] <- mcmc_options_list[fitting_options]
    if ("chains" %in% names(mcmc_options_list) & !("cores" %in%
                                                   names(mcmc_options_list))) {
      mcmc_options$cores <- min(mcmc_options_list$chains,
                                detectCores())
    }
    if ("cores" %in% names(mcmc_options_list) & !("chains" %in%
                                                  names(mcmc_options_list))) {
      mcmc_options$chains <- mcmc_options_list$cores
    }
  }
  if (length(mcmc_options$scale) != length(get_estimated_par_names(prior)))
    stop("scale parameter has to be of the same length as number of estimated parameters")
  chains <- mcmc_options$chains
  cores <- mcmc_options$cores
  burn <- round(mcmc_options$burn, 0)
  chain_length <- mcmc_options$chain_length
  scale <- mcmc_options$scale
  output_freq <- mcmc_options$output_freq
  avoid_model_recalculation <- mcmc_options$avoid_model_recalculation
  initial_trials <- 10
  est_par_names <- get_estimated_par_names(prior)
  if (any(scale <= 0))
    stop("the scale parameter must be a positive")
  if (length(scale) != length(est_par_names))
    stop("the scale parameter must be a scalar or a vector of length equal to the number of estimated parameters")
  if (chain_length < 1 || chain_length%%1 != 0)
    stop("the chain_length parameter must be a positive integer")
  if (output_freq <= 0 || output_freq%%1 != 0)
    stop("the output_freq parameter must be a positive integer")
  if (chains < 1 || chains%%1 != 0)
    stop("the chains parameter must be a positive integer")
  if (burn < 0)
    stop("the burn parameter must be a non-negative integer")
  n_var <- ncol(data_set)
  if (anyDuplicated(colnames(data_set)))
    stop("column names in data are duplicated")
  if (n_var > length(get_shock_names(model)))
    stop("you can not use for estimation more observable variables than shocks in the model")
  cat("\nFinding the posterior kernel mode... \n")
  opt_pars_len <- length(est_par_names)
  init_pars <- rep(0.5, opt_pars_len)
  init_vals <- get_initial_values(prior)
  if (!is.null(get_shock_cov_mat(model))) {
    sh_mat <- get_shock_cov_mat(model)
    sq_sh_mat <- sqrt(diag(sh_mat))
    sh_mat <- sh_mat/kronecker(sq_sh_mat, t(sq_sh_mat))
    cov_sd_ind <- get_estimated_par_types(prior)
    if (length(which(cov_sd_ind > 0))) {
      init_pars[which(cov_sd_ind > 0)] <- sh_mat[prior@shock_names_matrix]
    }
  }
  model_par <- get_par_values(model, silent = TRUE)
  init_pars[-which(cov_sd_ind > 0)] <- model_par[est_par_names[-which(cov_sd_ind >
                                                                        0)]]
  if (any(!is.na(init_vals)))
    init_pars[which(!is.na(init_vals))] <- init_vals[which(!is.na(init_vals))]
  names(init_pars) <- est_par_names
  prior_bound <- summary(prior)
  l_bound <- prior_bound$parameters["Lower bound"][[1]]
  u_bound <- prior_bound$parameters["Upper bound"][[1]]
  kernel_post <- function(pars, par_names = NULL, ...) {
    if (!is.null(par_names))
      names(pars) <- par_names
    output_text <- capture.output(suppressWarnings(model_opt_tr <- update_model(model,
                                                                                pars, cov_sd_ind)))
    if (!is.null(model_opt_tr)) {
      output_text <- capture.output(suppressWarnings(pk <- posterior(parameters = pars,
                                                                     data_set = data_set, prior = prior, model = model,
                                                                     update_model = TRUE)))
      return(-pk$log_density)
    }
    else {
      return(Inf)
    }
  }
  opt_fun_result <- maximise_function(fun = kernel_post, init_pars = init_pars,
                                      l_bound = l_bound, u_bound = u_bound, optim_options_list = optim_options_list)
  post_mode <- opt_fun_result$f_mode
  post_inv_hess <- opt_fun_result$f_inv_hess
  cat("\n")
  cat("\nMaximisation routine solution: \n")
  names(post_mode) <- est_par_names
  print(post_mode)
  cat("\nCandidate for posterior mode FOUND \n")
  if_not_pos_def <- tryCatch(expr = {
    dispersion <- t(chol(post_inv_hess))
    FALSE
  }, warning = function(w) TRUE, error = function(w) TRUE)
  result <- gecon_estimation_results(chains, prior, est_parameters = get_estimated_par_names(prior),
                                     model)
  if (if_not_pos_def | (any(is.nan(post_inv_hess))) | (any(is.na(post_inv_hess)))) {
    result <- set_optimisation_results(est_results = result,
                                       opt_result = post_mode, opt_err = sqrt(diag(post_inv_hess)),
                                       hessian_pos_def = FALSE)
    stop("the posterior kernel optimisation problem. Inverse Hessian is not positive definite. Try with different initial values")
  }
  else {
    result <- set_optimisation_results(est_results = result,
                                       opt_result = post_mode, opt_err = sqrt(diag(post_inv_hess)),
                                       hessian_pos_def = TRUE)
  }
  cat("\n")
  cat("Computing marginal density (Laplace approximation)\n")
  model <- update_model(model, post_mode, cov_sd_ind)
  lt <- marginal_density_laplace(post_mode, post_inv_hess,
                                 data_set, model, prior)
  print(lt)
  result <- set_marg_density(est_results = result, marg_density = lt)
  initial <- vector(length = chains, mode = "list")
  tmp <- generate_initial(est_results = result, data_set, post_mode,
                          dispersion, chains, initial_scale = 10, initial_trials)
  if (chains > 1) {
    result <- set_models(result, lapply(tmp, "[[",
                                        "model"))
  }
  else {
    result <- set_models(result, list(tmp$model))
  }
  for (i in 1:chains) {
    if (chains > 1) {
      initial[[i]] <- tmp[[i]]$initial
    }
    else {
      initial <- list(tmp$initial)
    }
    names(initial[[i]]) <- get_estimated_par_names(prior)
  }
  total_it <- chains * (chain_length + burn)
  burning <- TRUE
  if (burn > 0)
    cat("\nRunning burn-in phase ... \n\n")
  else {
    burning <- FALSE
    cat("\nRunning proper phase of MCMC ... \n\n")
  }
  it <- 0
  bit <- 0
  ss_failures <- re_failures <- 0
  start <- Sys.time()
  cluster <- makeCluster(cores)
  registerDoParallel(cluster, cores = cores)
  on.exit({
    stopCluster(cluster)
  })
  while (it < chain_length) {
    if (burning)
      n <- min(burn - bit, output_freq)
    else n <- min(chain_length - it, output_freq)
    tmp <- {
      foreach(i = 1:chains, .packages = c("gEcon",
                                          "gEcon.estimation")) %dopar% {
                                            mcmc_worker(model = get_models(result)[[i]],
                                                        data = data_set, prior = prior, initial = initial[[i]],
                                                        dispersion = dispersion, n = n, scale = scale)
                                          }
    }
    for (i in 1:chains) {
      result <- extend_chain(result, get_chain(result,
                                               i), tmp[[i]][["model"]], chain = i, data = tmp[[i]][["data"]],
                             acceptance = tmp[[i]][["accept"]])
      ss_failures <- ss_failures + tmp[[i]][["ss_failures"]]
      re_failures <- re_failures + tmp[[i]][["re_failures"]]
    }
    if (burning)
      bit <- bit + n
    else it <- it + n
    est_par_names <- get_estimated_par_names(get_prior(result))
    chain_list <- get_chains(result)
    for (i in 1:length(initial)) {
      initial[[i]] <- chain_list[[i]][nrow(chain_list[[i]]),
                                      ]
      names(initial[[i]]) <- est_par_names
    }
    cat("Progress:  ", format((bit + it) * chains,
                              scientific = FALSE), "/", format(total_it,
                                                               scientific = FALSE), "( ", round(((bit + it) *
                                                                                                   chains)/total_it * 100, 0), "% )\n", sep = "")
    cat("Time consumed:  ", time_difference(real_start),
        "\n", sep = "")
    cat("Estimated time left:  ", time_difference(start,
                                                  total_it/((bit + it) * chains) - 1), "\n",
        sep = "")
    cat("Acceptance rate:  ", round(get_acc_rate(result) *
                                      100, 0), "%\n", sep = "")
    cat("Steady state failures:  ", round(ss_failures,
                                          0), "\n", sep = "")
    cat("Perturbation failures:  ", round(re_failures,
                                          0), "\n", sep = "")
    cat("\n")
    if (!burning) {
      cat("Current estimates:\n\n")
      out <- as.data.frame(matrix(NA, ncol = 2, nrow = length(est_par_names)))
      colnames(out) <- c("  Mean", "  Std. dev.")
      rownames(out) <- est_par_names
      out[, 1] <- paste0("  ", format(get_estimates(result)$estimates,
                                      digits = 4))
      out[, 2] <- paste0("  ", format(get_estimates(result)$stddev,
                                      digits = 4))
      print(out, right = FALSE)
      cat("\n\n")
    }
    if (burning && bit == burn) {
      burning <- FALSE
      result <- burn_initial(est_results = result, n = burn)
      cat("Burn-in phase DONE.\n\nRunning proper phase of MCMC ... \n\n")
    }
  }
  cat("Estimation is DONE. Total time elapsed: ", time_difference(real_start),
      "\n", sep = " ")
  return(result)
}
