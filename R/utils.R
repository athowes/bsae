# Collection of auxiliary utility functions

# input: eta in [-inf, inf]
# output: rho in [0, 1]
logistic <- function(eta) exp(eta) / (1 + exp(eta))

# source: adapted from eden (or see e.g. GPML)
# input: r, distance between points; l, lengthscale; nu, smoothness parameter (in 1.5, 2.5)
# output: matern covariance
matern <- function(r, l, nu){
  
   if(!nu %in% c(1.5, 2.5)){
     errorCondition("Choose either nu = 1.5 or 2.5")
   }
  
  ifelse(nu == 1.5, (1 + sqrt(3)*r/l) * exp(-sqrt(3) * r/l),
                    (1 + sqrt(5)*r/l + 5*r^2/(3*l^2)) * exp(-sqrt(5) * r/l))
}

# source: adapted from https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R
# input: x, nb object
# returns: dataframe containing num nodes, num edges and a list of graph edges from node1 to node2.
nb_to_graph = function(x) {
  
  n <- length(x)
  n_links <- 0
  for (i in 1:n) {
    if (x[[i]][1] != 0) {
      n_links = n_links + length(x[[i]])
    }
  }
  n_edges = n_links / 2;
  node1 = vector(mode="numeric", length = n_edges);
  node2 = vector(mode="numeric", length = n_edges);
  idx = 0;
  for (i in 1:n) {
    if (x[[i]][1] > 0) {
      for (j in 1:length(x[[i]])) {
        n2 = unlist(x[[i]][j]);
        if (i < n2) {
          idx = idx + 1;
          node1[idx] = i;
          node2[idx] = n2;
        }
      }
    }
  }
  return(list("n" = n, "n_edges" = n_edges, "node1" = node1, "node2" = node2));
}

# input: nb object
# returns: scaling factor 
get_scale <- function(nb) {
  
  M <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE) # Alternatively.. adjacency matrix
  Q <- diag(rowSums(M)) - M # ICAR precision matrix, equation (2) Riebler et al. (2016)
  Q_scaled  <- as.matrix(INLA::inla.scale.model(Q, constr = list(A = matrix(1, 1, nrow(Q)), e = 0))) # Scaled ICAR precision matrix for BYM2
  return(Q[1, 1] / Q_scaled[1, 1]) # I think Q/scale is the same as Q_scaled but not 100% sure yet
}
# Note: 
# Here does it a different way https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R
# without using INLA::
# but I got (what I think is) the wrong answer using it.
# Here's the code:
#
# M <- spdep::nb2mat(x, style = "B", zero.policy = TRUE) # Adjacency matrix
# Q <- diag(rowSums(M)) - M # ICAR precision matrix, equation (2) Riebler et al. (2016)
# n <- dim(Q)[1]
# Q_pert <- Q + Diagonal(n) * max(diag(Q)) * sqrt(.Machine$double.eps) # Add jiter to the diagonal for numerical stability
# Q_inv <- inla.qinv(Q_pert, constr = list(A = matrix(1, 1, n), e = 0)) # Inversion for sparse matrix
# return(exp(mean(log(diag(Q_inv))))) # Compute the generalised variance

# input: fit, a Stan model; name, a string containing the variable name
# returns: df of posterior means, standard deviations and 2.5% and 97.5% quantiles
stan_interval <- function(fit, name) {
  
  l <- nchar(name)
  data.frame(summary(fit)$summary) %>% 
    rownames_to_column("param") %>% 
    filter(substr(param, 1, l) == name) %>%
    dplyr::select(mean, sd, lower = X2.5., upper = X97.5.)
}

# input: fit, an INLA model
# returns: df of posterior means, standard deviations and 2.5% and 97.5% quantiles
inla_interval <- function(fit){
  
  fit$summary.fitted.values %>%
    dplyr::select(mean, sd, lower = "0.025quant", upper = "0.975quant")
}

# input: fit, a string (e.g. stan_fit1)
# output: a list containing the method and model names
get_model_method <- function(fit){
  
  x <- strsplit(fit, "_")
  method <- x[[1]][1]
  lookup1 <- data.frame(string = c("stan", "inla"), model = c("Stan", "INLA"))
  method <- filter(lookup1, string == method)$model 

  model <- x[[1]][2]
  i <- as.numeric(substring(model, nchar(model)))
  lookup2 <- data.frame(index = 1:6, 
                        model = c("Indep", "Besag", "Weighted ICAR", "BYM2", "Centroid MVN", "BYM3"))
  model <- lookup2[i, 2]
  return(list(method_name = method, model_name = model))
}

# input: M, a matrix
# returns: ggplot of the matrix
matrix_plot <- function(M){
  
  ggplot(reshape2::melt(M), aes(x = Var1, y = Var2, fill = value)) +
    labs(title = paste0("Visualise matrix: ", deparse(substitute(W))), x = "", y = "", fill = "Value") +
    geom_tile()
}

# input: sf, a simple features object with some geometry
# returns: list containing border length information for each region
border_lengths <- function(sf){
  
  gm <- st_geometry(sf)
  touch <- st_touches(gm) # Adjacency information
  
  perim <- gm %>% 
    st_cast("MULTILINESTRING") %>% 
    st_length() # Region perimeters

  # Function f adapted from Sébastien Rochette Stackoverflow answer
  f <- function(from){
    
    if(length(touch[[from]]) != 0) {
      lengths <- st_intersection(gm[from], gm[touch[[from]]]) %>%
        st_length()
    
      data.frame(origin = from,
                 perimeter = perim[[from]],
                 touching = touch[[from]],
                 length = lengths,
                 weight =  lengths / perim[[from]]) # Gives non-symmetric matrix
    } else {
      return(NA)
    }
  }

  all_lengths <- lapply(1:length(touch), f)
  all_lengths
}

# input: sf, a simple features object with some geometry
# returns: ICAR precision matrix Q based upon border lengths
border_precision <- function(sf){
  
  all_lengths <- border_lengths(sf)
  n <- length(all_lengths) # Not great naming..
  Q <- matrix(data = 0, nrow = n, ncol = n) # Empty matrix
  
  for(i in 1:nrow(sf)) {
    if(!anyNA(all_lengths[[i]])) {
      
      Q[i, i] <- all_lengths[[i]]$perimeter[1]
      Q[i, all_lengths[[i]]$touching] <- -all_lengths[[i]]$length
    }
  }
  
  return(Q)
}

# input: sf, a simple features object with some geometry
# returns: nb object giving the neighbor list
neighbours <- function(sf){
  nb <- sf %>%
    as("Spatial") %>%
    spdep::poly2nb()
  nb
}

# input: nb object
# returns: ICAR precision matrix Q corresponding to the Besag model
# i.e. -1 if i~j and #{neighbours} if i=j
nb_to_precision <- function(nb){
  
  n <- length(nb)
  Q <- matrix(data = 0, nrow = n, ncol = n) # Empty matrix
  
  for(i in 1:n){
    Q[i, i] <- length(nb[[i]])
    Q[i, nb[[i]]] <- -1
  }
  
  return(Q)
}