gEcon.estimation::gecon_prior<-
function (prior_list, model)
{
  if (length(prior_list) == 0)
    stop("prior information has not been specified for any parameter")
  if (!is.list(prior_list))
    stop("prior information should be provided as a list of lists")
  if (class(model) != "gecon_model")
    stop("model argument should be of gEcon_model class")
  p_list <- lapply(prior_list, "is.list")
  if (any(p_list == FALSE))
    stop("there is one or more prior_list element that is not a list")
  object <- new("gecon_prior")
  object@model_info <- model@model_info
  avail_dist <- c("beta", "gamma", "inv_gamma",
                  "inv_gamma2", "normal", "uniform")
  param_dim <- sapply(prior_list, "length", simplify = "array")
  max_num <- max(param_dim)
  f_names <- function(a) {
    b <- names(a)
    c(b, rep("", max_num - length(b)))
  }
  param_names <- sapply(prior_list, f_names, simplify = "character")
  f_values <- function(a) {
    b <- a
    c(b, rep("", max_num - length(b)))
  }
  param_values <- sapply(prior_list, f_values, simplify = "character")
  par_ind <- which(param_names == "par", arr.ind = TRUE)
  if (!all(c(1:length(param_dim)) %in% par_ind[, 2]))
    stop("the par element is missing in one or more lists")
  par_names <- as.character(param_values[par_ind])
  par_types <- parameter_type(model = model, parameters = par_names)
  object@parameters_tex <- par_types$parameters_tex
  object@par_type <- par_types$par_type
  object@shock_names_matrix <- par_types$shock_names_matrix
  object@model_par <- par_names
  type_ind <- which(param_names == "type", arr.ind = TRUE)
  type_names <- as.character(param_values[type_ind])
  type_ind <- match(type_names, avail_dist)
  if (all(is.na(type_ind))) {
    object@dist_type <- rep(0, length(type_ind))
  }
  else if (any(is.na(type_ind))) {
    stop(paste("the following distribution names: \n",
               paste(type_names[which(is.na(type_ind))], sep = ", "),
               "were misspelled"))
  }
  else {
    object@dist_type <- type_ind
    object@moments <- matrix(NA, length(object@model_par),
                             2)
    object@parameters <- matrix(NA, length(object@model_par),
                                2)
    mean_ind <- which(param_names == "mean", arr.ind = TRUE)
    object@moments[mean_ind[, 2], 1] <- as.numeric(param_values[mean_ind])
    std_ind <- which(param_names == "sd", arr.ind = TRUE)
    object@moments[std_ind[, 2], 2] <- as.numeric(param_values[std_ind])
    alpha_ind <- which(param_names == "a", arr.ind = TRUE)
    object@parameters[alpha_ind[, 2], 1] <- as.numeric(param_values[alpha_ind])
    beta_ind <- which(param_names == "b", arr.ind = TRUE)
    object@parameters[beta_ind[, 2], 2] <- as.numeric(param_values[beta_ind])
  }
  init_ind <- which(param_names == "initial", arr.ind = TRUE)
  object@initial <- as.numeric(rep(NA, length(object@model_par)))
  object@initial[init_ind[, 2]] <- as.numeric(param_values[init_ind])
  object@bounds <- matrix(NA, length(object@model_par), 2)
  lower_ind <- which(param_names == "lower_bound", arr.ind = TRUE)
  object@bounds[lower_ind[, 2], 1] <- as.numeric(param_values[lower_ind])
  upper_ind <- which(param_names == "upper_bound", arr.ind = TRUE)
  object@bounds[upper_ind[, 2], 2] <- as.numeric(param_values[upper_ind])
  unspec_bounds <- rowSums(is.na(object@bounds))
  if (length(intersect(which(unspec_bounds == 1), which(object@dist_type ==
                                                        1))))
    stop(paste("for beta distribution either no bounds should be specified (default values of 0 and 1 will be assumed)",
               "or both lower and upper bounds have to be specified"))
  if (length(intersect(which(unspec_bounds == 2), which(object@dist_type ==
                                                        1)))) {
    object@bounds[intersect(which(unspec_bounds == 2), which(object@dist_type ==
                                                               1)), 1] <- 0
    object@bounds[intersect(which(unspec_bounds == 2), which(object@dist_type ==
                                                               1)), 2] <- 1
  }
  if (length(intersect(which(unspec_bounds > 0), which(object@dist_type ==
                                                       2)))) {
    object@bounds[intersect(which(is.na(object@bounds[, 1])),
                            which(object@dist_type == 2)), 1] <- 0
    object@bounds[intersect(which(is.na(object@bounds[, 2])),
                            which(object@dist_type == 2)), 2] <- Inf
  }
  if (length(intersect(which(unspec_bounds > 0), which(object@dist_type ==
                                                       3)))) {
    object@bounds[intersect(which(is.na(object@bounds[, 1])),
                            which(object@dist_type == 3)), 1] <- 0
    object@bounds[intersect(which(is.na(object@bounds[, 2])),
                            which(object@dist_type == 3)), 2] <- Inf
  }
  if (length(intersect(which(unspec_bounds > 0), which(object@dist_type ==
                                                       4)))) {
    object@bounds[intersect(which(is.na(object@bounds[, 1])),
                            which(object@dist_type == 4)), 1] <- 0
    object@bounds[intersect(which(is.na(object@bounds[, 2])),
                            which(object@dist_type == 4)), 2] <- Inf
  }
  if (length(intersect(which(unspec_bounds > 0), which(object@dist_type ==
                                                       5)))) {
    object@bounds[intersect(which(is.na(object@bounds[, 1])),
                            which(object@dist_type == 5)), 1] <- -Inf
    object@bounds[intersect(which(is.na(object@bounds[, 2])),
                            which(object@dist_type == 5)), 2] <- Inf
  }
  if (length(intersect(which(unspec_bounds > 1), which(object@dist_type ==
                                                       6))))
    stop("for uniform distribution parameters both lower and upper bounds have to be specified")
  if (any(object@dist_type == 6))
    object@parameters[which(object@dist_type == 6), ] <- object@bounds[which(object@dist_type ==
                                                                               6), ]
  par_as_mom <- rowSums(is.na(object@moments)) == 0
  par_as_par <- rowSums(is.na(object@parameters)) == 0
  if (any(par_as_mom & par_as_par)) {
    stop(paste("for the following parameters of model",
               object@model_par[which(par_as_mom & par_as_par)],
               "parameters were passed both as moments and distribution parameters"))
  }
  par_as_mom <- which(par_as_mom)
  par_as_par <- which(par_as_par)

  moments_to_par <- function(a, dist_code) {
    switch(as.character(dist_code), `1` = {
      return(beta_params(mean_b = a[1], std_b = a[2], start_b = a[3],
                         end_b = a[4]))
    }, `2` = {
      return(gamma_params(mean_g = a[1], std_g = a[2],
                          start_g = a[3]))
    }, `3` = {
      return(inverted_gamma_1_parameters(mean_i = a[1],
                                         sd_i = a[2]))
    }, `4` = {
      return(inverted_gamma_2_parameters(mean_i = a[1] -
                                           a[3], sd_i = a[2]))
    }, `5` = {
      return(normal_params(mean_n = a[1], std_n = a[2]))
    }, `6` = {
      return(uniform_params(mean_u = a[1], std_u = a[2]))
    })
  }
  if (length(par_as_mom)) {
    for (i in 1:length(par_as_mom)) {
      object@parameters[par_as_mom[i], ] <- as.numeric(moments_to_par(c(object@moments[par_as_mom[i],
                                                                                       ], object@bounds[par_as_mom[i], ]), object@dist_type[par_as_mom[i]]))
    }
  }
  par_to_moments <- function(a, dist_code) {
    switch(as.character(dist_code), `1` = {
      return(beta_mom(a = a[1], b = a[2], L = a[3], U = a[4]))
    }, `2` = {
      return(gamma_mom(a = a[1], b = a[2], L = a[3]))
    }, `3` = {
      return(inverted_gamma_1_mom(a = a[1], b = a[2], L = a[3]))
    }, `4` = {
      return(inverted_gamma_2_mom(a = a[1], b = a[2], L = a[3]))
    }, `5` = {
      return(normal_mom(a = a[1], b = a[2]))
    }, `6` = {
      return(uniform_mom(a = a[1], b = a[2]))
    })
  }
  if (length(par_as_par)) {
    for (i in 1:length(par_as_par)) {
      object@moments[par_as_par[i], ] <- as.numeric(par_to_moments(c(object@parameters[par_as_par[i],
                                                                                       ], object@bounds[par_as_par[i], ]), object@dist_type[par_as_par[i]]))
    }
  }
  if (any(object@bounds[, 1] > object@bounds[, 2])) {
    low_up_bounds <- which(object@bounds[, 1] > object@bounds[,
                                                              2])
    stop(paste("for the following parameters the upper bound is lower than the lower bound:\n",
               paste(object@model_par[low_up_bounds], collapse = ", ")))
  }
  init <- which(!is.na(object@initial))
  if (any(object@initial[init] < object@bounds[init, 1] | object@initial[init] >
          object@bounds[init, 2])) {
    init_out_bounds <- which((object@initial[init] < object@bounds[init,
                                                                   1] | object@initial > object@bounds[init, 2]))
    stop(paste("the initial values for estimation of the following parameters lie outside the distribution support:\n",
               paste(object@model_par[init_out_bounds], collapse = ", ")))
  }
  return(object)
}