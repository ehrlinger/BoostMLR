#' Variable Importance
#' 
#' Calculate standardized variable importance (VIMP) for each covariate or a
#' joint VIMP of multiple covariates.
#' 
#' Standardized variable importance (VIMP) is calcuated for each covariate or a
#' joint VIMP is calculated for all the covariates specified in
#' \code{xvar.names}.
#' 
#' @param Object A boosting object of class \code{(BoostMLR, predict)}.
#' @param xvar.names Names of the x-variables for which VIMP is requested. If
#' NULL, VIMP is calcuated for all the covariates.
#' @param joint Whether to estimate VIMP for each covariate from
#' \code{xvar.names} or a joint VIMP for multiple covariates?
#' @param setting_seed Set \code{setting_seed} = TRUE if you intend to
#' reproduce the result.
#' @param seed_value Seed value.
#' @return If \code{joint} = FALSE, a standardized VIMP for each covariate is
#' obtained otherwisea joint VIMP for all the covariates is obtained.  The
#' result consists of a list of length equal to the number of multivariate
#' response. Each element from the list represents a matrix with number of rows
#' equal to the number of covariates (in case of joint VIMP, the matrix will
#' have a single row) and the number of columns equal to the number of
#' overlapping time intervals + 1 where the first column contains covariate
#' main effects and all other columns contain covariate-time interaction
#' effects.
#' @author Amol Pande and Hemant Ishwaran
#' @references Pande, A., Ishwaran, H. & Blackstone, E. Boosting for
#' Multivariate Longitudinal Responses. SN COMPUT. SCI. 3, 186 (2022).
#' https://doi.org/10.1007/s42979-022-01072-6
#' 
#' Friedman J.H. Greedy function approximation: a gradient boosting machine,
#' \emph{Ann. of Statist.}, 5:1189-1232, 2001.
#' @keywords plot
#' @examples
#' 
#' \donttest{
#' ##-----------------------------------------------------------------
#' ## Calculate individual and joint VIMP
#' ##-----------------------------------------------------------------
#' 
#' # Simulate data involves 3 response and 4 covariates
#' 
#' dta <- simLong(n = 100, ntest = 100, N = 5, rho =.80, model = 1, 
#'                                   y_scale = "continuous", q_x = 0, 
#'                                   q_y = 0,type = "corCompSym")
#' dtaL <- dta$dtaL
#' trn <- dta$trn
#' # Boosting call: Raw values of covariates, B-spline for time, 
#' # no estimate of rho and phi
#' 
#' boost.grow <- BoostMLR(x = dtaL$features[trn,], tm = dtaL$time[trn], 
#'                       id = dtaL$id[trn], y = dtaL$y[trn,], 
#'                       y_scale = rep("continuous",3),intercept = FALSE,
#'                       M = 100, var_flag = FALSE)
#' 
#' boost.pred <- predictBoostMLR(Object = boost.grow, x = dtaL$features[-trn,], 
#'                                tm = dtaL$time[-trn], id = dtaL$id[-trn], 
#'                                y = dtaL$y[-trn,], importance = TRUE)
#' # Individual VIMP                               
#' Ind_vimp <- vimp.BoostMLR(boost.pred)
#' 
#' # Joint VIMP
#' Joint_vimp <- vimp.BoostMLR(boost.pred,joint = TRUE)
#' 
#' }
#' 
vimp.BoostMLR <- function(Object,
                          xvar.names = NULL,
                          joint = FALSE,
                          setting_seed = FALSE,
                          seed_value = 100L)
{
  
  if(sum(inherits(Object, c("BoostMLR", "predict"), TRUE) == c(1, 2)) != 2) 
  {
    stop("This function only works for objects of class `(BoostMLR, predict)'")
  }

  x <- Object$Data$Org_x
  P <- ncol(x)
  x_names <- Object$x_names
  y_names <- Object$y_names

  if(is.null(xvar.names))
  {
    vimp_set <- (1:P) - 1
    x_names <- x_names
    if(joint)
    {
      x_names <- "joint_vimp"
    }
  }else
  {
    n.x.names <- length(xvar.names)
    vimp_set <- ( sort( match(xvar.names,x_names) ) ) - 1
    if(any(is.na( vimp_set )))
    {
      stop("xvar.names do not match with variable names from original data")
    }
    
    if(joint)
    {
      x_names <- "joint_vimp"
    }else
    {
      x_names <- xvar.names[ na.omit(match(x_names,xvar.names)) ]
    }
  }

  if(joint)
  {
    p <- 1
  }else 
  {
    p <- length(vimp_set)  
  }

  Org_x <- Object$Data$Org_x
  Org_y <- Object$Data$Org_y
  id <- Object$Data$id
  tm <- Object$Data$tm
  id_x <- Object$Data$id_x
  tm_x <- Object$Data$tm_x
  x_Mean <- Object$Data$x_Mean
  x_Std_Error <- Object$Data$x_Std_Error
  y_Mean <- Object$Data$y_Mean
  y_Std_Error <- Object$Data$y_Std_Error

  n <- Object$Pred_Object$Dimensions$n
  ni <- Object$Pred_Object$Dimensions$ni
  ni_x <- Object$Pred_Object$Dimensions$ni_x
  N <- Object$Pred_Object$Dimensions$N
  N_x <- Object$Pred_Object$Dimensions$N_x
  L <- Object$Pred_Object$Dimensions$L
  K <- Object$Pred_Object$Dimensions$K
  Dk <- Object$Pred_Object$Dimensions$Dk
  H <- Object$Pred_Object$Dimensions$H
  G <- Object$Pred_Object$Dimensions$G
  n_unq_tm <- Object$Pred_Object$Dimensions$n_unq_tm
  n_unq_tm_x <- Object$Pred_Object$Dimensions$n_unq_tm_x

  unq_id <- Object$Pred_Object$Index$unq_id
  unq_tm <- Object$Pred_Object$Index$unq_tm
  unq_tm_x <- Object$Pred_Object$Index$unq_tm_x
  unq_x <- Object$Pred_Object$Index$unq_x
  id_index <- Object$Pred_Object$Index$id_index
  id_index_x <- Object$Pred_Object$Index$id_index_x
  tm_index <- Object$Pred_Object$Index$tm_index
  tm_index_x <- Object$Pred_Object$Index$tm_index_x
  x_index <- Object$Pred_Object$Index$x_index
  unq_x_New <- Object$Pred_Object$Index$unq_x_New
  Index_Bt <- Object$Pred_Object$Index$Index_Bt
  Index_Bt_x <- Object$Pred_Object$Index$Index_Bt_x

  Bt <- Object$Pred_Object$BS$Bt
  Bt_x <- Object$Pred_Object$BS$Bt_x
  Bxt <- Object$Pred_Object$BS$Bxt
  Bx_K <- Object$Pred_Object$BS$Bx_K
  Bt_H <- Object$Pred_Object$BS$Bt_H
  Bt_G <- Object$Pred_Object$BS$Bt_G
  Bx <- Object$Pred_Object$BS$Bx
  time_map <- Object$Pred_Object$BS$time_map

  UseRaw <- Object$Pred_Object$UseRaw
  Beta_Hat_List <- Object$Pred_Object$Beta_Hat_List
  Mopt <- Object$Mopt
  rmse <- Object$rmse
  nu <- Object$nu

  Vec_zero <- Object$Pred_Object$Vec_zero
  mu_zero_vec <- Object$Pred_Object$mu_zero_vec

  Time_Varying <- Object$Pred_Object$Time_Varying
  intercept <- Object$intercept
  y_scale_summary <- Object$y_scale_summary

  obj_C <- vimp_BoostMLR_C(Org_x,
                          Org_y,
                          tm,
                          id,
                          id_x,
                          tm_x,
                          x_Mean,
                          x_Std_Error,
                          y_Mean,
                          y_Std_Error,
                          intercept,
                          y_scale_summary,
                          n,
                          ni,
                          ni_x,
                          N,
                          N_x,
                          L,
                          K,
                          p,
                          H,
                          G,
                          Dk,
                          n_unq_tm,
                          n_unq_tm_x,
                          UseRaw,
                          id_index,
                          id_index_x,
                          tm_index,
                          tm_index_x,
                          unq_x_New,
                          Index_Bt,
                          Index_Bt_x,
                          vimp_set,
                          joint,
                          Bt,
                          Bt_x,
                          Bt_H,
                          Bt_G,
                          Bx,
                          Bxt,
                          Bx_K,
                          time_map,
                          Beta_Hat_List,
                          Mopt,
                          nu,
                          rmse,
                          Time_Varying,
                          Vec_zero,
                          mu_zero_vec,
                          setting_seed,
                          seed_value)

  vimp <- obj_C$vimp
  names(vimp) <- y_names

  # Set the covariate-interaction to NA for now
  for(l in 1 : L)
  {
    vimp[[l]][[2]] <- NA
  }

  for(l in 1:L)
  {
    names(vimp[[l]][[1]]) <- x_names
    #names(vimp[[l]][[2]]) <- x_names
  }

  obj <- vimp
  invisible(obj)

}
