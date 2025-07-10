#' Prediction for the multivariate longitudinal response
#' 
#' Function returns predicted values for the response. Also, if the response is
#' provided, function returns test set performance, optimal boosting iteration,
#' and variable importance (VIMP).
#' 
#' The predicted response and performance values are obtained for the test data
#' using the \code{Object} grown using function \command{BoostMLR} on the
#' training data.
#' 
#' @param Object A boosting object obtained using the function
#' \command{BoostMLR} on the training data.
#' @param x Data frame (or matrix) containing the test set x-values
#' (covariates).  Covariates can be time-varying or time-invariant. If \code{x}
#' is unspecified while growing the \code{Object}, it should be unspecified
#' here as well.
#' @param tm Vector of test set time values corresponding to response \code{y}.
#' If \code{tm} is unspecified while growing the \code{Object}, it should be
#' unspecified here as well.
#' @param id Vector of test set subject identifier corresponding to response
#' \code{y}. If \code{id} is unspecified while growing the \code{Object}, it
#' should be unspecified here as well.
#' @param y Data frame (or matrix) containing the test set y-values (response)
#' in case of multivariate response or a vector of y-values in case of
#' univariate response. If \code{y} is unspecified then predicted values
#' corresponding to \code{x} and \code{tm} can be obtained but no performance
#' measure such as test set error and VIMP.
#' @param id_x Vector of test set subject identifier corresponding to response
#' \code{x}.
#' @param tm_x Vector of test set time values corresponding to response
#' \code{x}.
#' @param M Number of boosting iterations. Value should be less than or equal
#' to the value specified in the \code{Object}. If unspecified, value from the
#' \code{Object} will be used.
#' @param importance Whether to calculate standardized variable importance
#' (VIMP) for each covariate?
#' @param eps Tolerance value used for determining the optimal \code{M}.
#' @param setting_seed Set \code{setting_seed} = TRUE if you intend to
#' reproduce the result.
#' @param seed_value Seed value.
#' @param time_map Use the same format as described for function
#' \command{BoostMLR}.
#' @param ... Further arguments passed to or from other methods.
#' @return \item{Data}{A list with elements \code{x}, \code{tm}, \code{id} and
#' \code{y}. Additionally, the list include mean and standard deviation of
#' \code{x} and \code{y}.} \item{x_Names}{Variable names of \code{x}.}
#' \item{y_Names}{Variable names of \code{y}.} \item{intercept}{Intercept term
#' is used in the model or now?} \item{y_scale_summary}{Using \code{y_scale},
#' we summarize if the scale of responses as a scalar. For example, if all the
#' responses are continuous, then \code{y_scale_summary} would be continuous,
#' however in case if the responses are a combination of binary and ordinal,
#' then \code{y_scale_summary} would be binary, since for the purpose of
#' modeling, ordinal response will be converted to multiple binary responses.}
#' \item{async_x}{This is used to identify if the data is asynchronous (TRUE)
#' or synchronous (FALSE).} \item{mu}{Estimate of conditional expectation of
#' \code{y} corresponding to the last boosting iteration.}
#' \item{mu_Mopt}{Estimate of conditional expectation of \code{y} corresponding
#' to the optimal boosting iteration.} \item{prob_class}{For ordinal responses,
#' this provide probabilities for each class, rather than cumulative
#' probabilities.} \item{Error_Rate}{Test set error rate for each multivariate
#' response across the boosting iterations.} \item{Mopt}{The optimal number of
#' boosting iteration.} \item{nu}{Regularization parameter.} \item{rmse}{Test
#' set standardized root mean square error (sRMSE) at the \code{Mopt}.}
#' \item{vimp}{Standardized VIMP for each covariate. This consist of a list of
#' length equal to the number of multivariate responses. Each element from the
#' list represents a list with two elements. The first element is for the
#' covariate main effect and has the same length as the number of covariates.
#' The second element is for the covariate-time interation effect and has a
#' length equal to the number of covariates. The difference from the first
#' element is that here each element of the list is a matrix with number of
#' rows equal to the number of columns of B-spline for \code{tm} and the number
#' of columns equal to the number of columns of B-spline for \code{tm_x}.}
#' \item{time_map}{Supplied or derived object.} \item{Pred_Object}{Useful for
#' internal calculation.}
#' @author Amol Pande and Hemant Ishwaran
#' @seealso \command{\link{BoostMLR}}, %\command{\link{updateBoostMLR}},
#' \command{\link{simLong}}
#' @references Pande, A., Ishwaran, H. & Blackstone, E. Boosting for
#' Multivariate Longitudinal Responses. SN COMPUT. SCI. 3, 186 (2022).
#' https://doi.org/10.1007/s42979-022-01072-6
#' 
#' Pande A., Li L., Rajeswaran J., Ehrlinger J., Kogalur U.B., Blackstone E.H.,
#' Ishwaran H. (2017).  Boosted multivariate trees for longitudinal data,
#' \emph{Machine Learning}, 106(2): 277--305.
#' 
#' Pande A. (2017).  \emph{Boosting for longitudinal data}.  Ph.D.
#' Dissertation, Miller School of Medicine, University of Miami.
#' @keywords boosting
#' @examples
#' 
#' \donttest{
#' ##-----------------------------------------------------------------
#' ## Multivariate Longitudinal Response
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
#' # Plot test set error
#' plotBoostMLR(boost.pred$Error_Rate,xlab = "m",ylab = "Test Set Error",
#'                                               legend_fraction_x = 0.2)
#' }
#' 
predictBoostMLR    <-  function(Object,
                                 x,
                                 tm,
                                 id,
                                 y,
                                 id_x = NULL,
                                 tm_x = NULL,
                                 M,
                                 importance = FALSE,
                                 eps = 1e-5,
                                 setting_seed = FALSE,
                                 seed_value = 100L,
                                 time_map = NULL,
                                 ...)
{
  
  user.option <- list(...)
  dt_Add <- is.hidden.predict.dt_Add(user.option)
  
  importance_Coef <- is.hidden.importance_Coef(user.option)
  
  if(missing(tm) && missing(x))
  {
    stop("tm and x both missing")
  }
  
  if(!missing(tm) && missing(id))
  {
    stop("id is missing")
  }
  
  CrossSectional <- FALSE
  if(missing(tm) && !missing(x) )
  {
    if(!missing(id))
    {
      if(!(length(sort(unique(id))) == nrow(x)) )
      {
        stop("tm is missing")
      }
    }else
    {
      id <- 1:nrow(x)
    }
    tm <- rep(0, length(x))
    CrossSectional <- TRUE
  }
  
  if(missing(x) && !missing(tm))
  {
    x_miss <- TRUE
    x_raw_all <- TRUE
    x <- cbind(rep(1,length(tm)))
    if(length(sort(unique(id))) == nrow(x) )
    {
      CrossSectional <- TRUE
    }
  }else
  {
    x_miss <- FALSE
  }
  
  tm_varying <- Object$tm_varying
  
  if(!missing(tm) && tm_varying == FALSE && CrossSectional == FALSE)
  {
    if(x_miss)
    {
      x <- tm
    }else
    {
      x <- x #cbind(x,tm)
    }
  }
  
  if (any(is.na(id))) 
  {
    stop("missing values encountered in id: remove observations with missing values")
  }
  
  if (!missing(y)) 
  {
    if ( any(is.na(y)) ) 
    {
      #stop("missing values encountered in y: remove observations with missing values")
    }
    testFlag <- TRUE
    y_scale <- Object$y_scale
    if(is.data.frame(y))
    {
      y <- data.matrix(y)
    }

    if(!is.vector(y) && !is.matrix(y) )
    {
      stop("y must be a vector or matrix")
    } else 
      {
      if(is.vector(y) && is.atomic(y)) {
        y <- matrix(y,ncol = 1)  
      }
    }

    y_names <- colnames(y)
    if(is.null(y_names))
    {
      y_names <- paste("y",1 : ncol(y),sep="")
    }
    if( length(y_scale) != ncol(y) )
    {
      stop("length of y_scale should be same as the number of variables in y")
    }


    if( any(y_scale == "ordinal") )
    {
      which_y_ordinal <- which( y_scale == "ordinal")

      y_names <- colnames(y)   

      temp_y_names <- c()
      temp_y <- data.frame()
      y_scale_mod <- c()

      null.obj <- lapply(1 : ncol(y), function(l) {
        if( any(which_y_ordinal == l) )
        {
          Q_set <- Object$Q_set[[l]]
          n.Q   <- length(Q_set) 
          null.obj.1 <- lapply(1 : n.Q, function(q){
            temp_y_vector <- cbind(ifelse( y[,l,drop = TRUE] <=  Q_set[q],1,0))
            if( all (dim(temp_y) == 0) )
            {
              temp_y <<- as.data.frame(temp_y_vector)
            }else
            {
              temp_y <<- as.data.frame(cbind(temp_y, temp_y_vector))
            }            
            temp_y_names <<- c(temp_y_names, paste(y_names[l],"_ord_",Q_set[q],sep=""))
            y_scale_mod <<- c(y_scale_mod, "binary") 
            NULL
          })
          rm(null.obj.1)
        }else
        {
          if( all (dim(temp_y) == 0) )
          {
            temp_y <<- as.data.frame(y[,l,drop = TRUE])
          }else
          {
            temp_y <<- as.data.frame(cbind(temp_y, y[,l,drop = FALSE]))
          }
          temp_y_names <<- c(temp_y_names, y_names[l])
          y_scale_mod <<- c(y_scale_mod, y_scale[l]) 
        }
        NULL
      })
      rm(null.obj)

      y <- temp_y
      y_names <- temp_y_names
      colnames(y) <- y_names
    }else
    {
      y_scale_mod <-  y_scale
    }

  }else
  {
    testFlag <- FALSE
    L <- Object$Grow_Object$Dimensions$L
    y <- matrix(0, nrow =  length(tm),ncol = L) 
    y_names <- paste("y",1:L,sep="")
    y_scale <- Object$y_scale
  }
  
  if(!is.matrix(y))
  {
    y <- data.matrix(y)
  }
  
  Time_Unmatch <- Object$Grow_Object$Time_Unmatch
  N <- nrow(x)
  
  tm_separate <- Object$tm_separate

  if( (is.null(id_x) && !is.null(tm_x)) || (!is.null(id_x) && is.null(tm_x)) )
  {
    stop("Either id_x or tm_x is null but not both")
  }

  if( ( !is.null(id_x) && !is.null(tm_x) )  )
  {
    if(is.null(tm_separate))
    {
      stop("tm_separate cannot be NULL when id_x and tm_x are provided")
    }
  }

  if(!is.null(id_x))
  {
    if(!all.equal( unique(id), unique(id_x) ))
    {
      stop("id doesn't match with id_x for all subjects")
    }
    if( length(id_x) != length(tm_x))
    {
      stop("lengths of id_x and tm_x not matched")
    }
    if( length(tm_x) != nrow(x) )
    {
      stop("length of tm_x does not match with number of rows of x")
    }
    async_x <- TRUE
  }else
  {
    async_x <- FALSE
    id_x <- id
    tm_x <- rep(1, length(tm))
  }

  if(!is.null(dt_Add))
  {
    if(!is.list(dt_Add))
    {
      stop("dt_Add must be a list")
    }
    K_Add <- length(dt_Add)
    nullObj <- lapply(1:K_Add,function(kk)
    {
      nc_K_Add <- ncol(dt_Add[[kk]])
      if(nc_K_Add != 3){
        stop("Each element of dt_Add must be a dataset with 3 columns arrange in order of id, time, x")
      }
      NULL
    })
    
    Ord_id_tm <- Order_Time(ID = id,Time = tm)
    id  <- id[Ord_id_tm]
    tm  <- tm[Ord_id_tm]
    x   <- x[Ord_id_tm,,drop = FALSE]
    y   <- y[Ord_id_tm,,drop = FALSE]
    x_Add_New <- matrix(NA,nrow = N,ncol = K_Add)
    x_Names_Add <- rep(NA,K_Add)
    Time_Add_New <- matrix(NA,nrow = N,ncol = K_Add)
    Time_Names_Add <- rep(NA,K_Add)
    for(kk in 1:K_Add){
      Ord_id_tm_Add <- Order_Time(ID = dt_Add[[kk]][,1],Time =  dt_Add[[kk]][,2])
      dt_Add[[kk]] <- dt_Add[[kk]][Ord_id_tm_Add,,drop = FALSE]
      id_Add <- dt_Add[[kk]][,1]
      x_Names_Add[kk] <- names(dt_Add[[kk]][,3,drop = FALSE])
      Time_Names_Add[kk] <- names(dt_Add[[kk]][,2,drop = FALSE])
      if(any(is.na(id_Add))){
        stop("Missing values observed for id in dt_Add")
      }
      unq_id_Add <- unique(id_Add)
      n_Add <- length(unq_id_Add)
      nullObj <- unlist(lapply(1:n_Add,function(i){
        Which_id <- which(unq_id_Add[i] == id)
        #ni <- length(Which_id)
        if(ni > 0){
          Which_id_Add <- which(id_Add == unq_id_Add[i])
          ni_Add <- length(Which_id_Add)
          tm_Add <- dt_Add[[kk]][Which_id_Add,2]
          x_Add <- dt_Add[[kk]][Which_id_Add,3]
          for(j in 1:ni){
            for(jj in 1:ni_Add){
              if((!is.na(tm_Add[jj]) && !is.na(tm[Which_id[j]]))){
                if(tm_Add[jj] <=  tm[Which_id[j]]){
                  x_Add_New[Which_id[j], kk] <<- x_Add[jj]
                  Time_Add_New[Which_id[j], kk] <<- tm_Add[jj]
                }
              }
            }
          }
        }
        NULL  
      }))
    }
    colnames(x_Add_New) <- x_Names_Add
    x <- cbind(x,x_Add_New)
    colnames(Time_Add_New) <- Time_Names_Add
  } else
  {
    Time_Add_New <- matrix(0,nrow = N,ncol = 1)
    colnames(Time_Add_New) <- "Time_Add"
  }
  

  #---------------------------------------------------------------------------------- 
  # Date: 12/17/2020
  
  # In the following codes, if the id is character or factor, we convert into numeric
  # without changing the values.
  #----------------------------------------------------------------------------------
   
  if(is.character(id))
  {
    id <- as.numeric(id)
  }
 
  if(is.factor(id))
  {
    id <- as.numeric(levels(id))[id]
  }

  if(is.character(id_x))
  {
    id_x <- as.numeric(id_x)
  }
 
  if(is.factor(id_x))
  {
    id_x <- as.numeric(levels(id_x))[id_x]
  }

  #---------------------------------------------------------------------------------- 
  # Date: 12/17/2020
  
  # while working on BoostMLR manuscript, I realized that the function Order_Time
  # works only when Dt_Add is non-null. We need this in every situation so that
  # I can plot beta coefficient as a function of time. This is done in the following
  # codes. Note that I have modified the Order_Time function in the utilities file.
  #----------------------------------------------------------------------------------
  sort_id <- is.hidden.sort_id(user.option)
  if(sort_id)
  {
    unq_id <- sort_unique_C_NA(id)
  } else
  {
    unq_id <- unique_C_NA(id)
  }

  n <- length(unq_id)
  ni <- unlist(lapply(1 : n, function(i){
    sum(id == unq_id[i],na.rm = TRUE)
  }))

  ni_x <- unlist(lapply(1 : n, function(i){
    sum(id_x == unq_id[i],na.rm = TRUE)
  }))

  ord_id_tm <- Order_Time_specified_id(ID = id,Time = tm,unq_id = unq_id)
  id  <- id[ord_id_tm]
  tm  <- tm[ord_id_tm]
  y   <- y[ord_id_tm,,drop = FALSE]

  if(async_x)
  {
    ord_id_tm_x <- Order_Time_specified_id(ID = id_x,Time = tm_x,unq_id = unq_id)
    id_x  <- id_x[ord_id_tm_x]
    tm_x  <- tm_x[ord_id_tm_x]
    x   <- x[ord_id_tm_x,,drop = FALSE]
  }else
  {
    x    <- x[ord_id_tm,,drop = FALSE]
    id_x <- id_x[ord_id_tm]
    tm_x <- tm_x[ord_id_tm]
  }

  if(!is.matrix(x))
  {
    x <- data.matrix(x)
  }
  
  x_names <- colnames(x)
  
  K <- ncol(x)
  
  if(is.null(x_names))
  {
    x_names <- paste("x",1:K,sep="")
  }
  
  if(!identical(x_names , Object$x_names) )
  {
    stop("Covariate from grow and predict function are not matching")
  }
  
  if(missing(M))
  {
    M <- Object$Grow_Object$Regulate$M  
  }
  
  L <- ncol(y)
  
  if(is.null(y_names))
  {
    y_names <- paste("y",1:L,sep="")
  }
  
  H  <- Object$Grow_Object$Dimensions$H
  G  <- Object$Grow_Object$Dimensions$G
  Dk <- Object$Grow_Object$Dimensions$Dk
  
  x_Mean      <- Object$Grow_Object$Data$x_Mean
  x_Std_Error <- Object$Grow_Object$Data$x_Std_Error
  y_Mean      <- Object$Grow_Object$Data$y_Mean
  y_Std_Error <- Object$Grow_Object$Data$y_Std_Error
  
  unq_tm     <- Object$Grow_Object$Index$unq_tm
  unq_tm_x   <- Object$Grow_Object$Index$unq_tm_x
  unq_x      <- Object$Grow_Object$Index$unq_x
  
  Bt     <- Object$Grow_Object$BS$Bt
  Bt_x   <- Object$Grow_Object$BS$Bt_x
  Bx     <- Object$Grow_Object$BS$Bx
  
  nu  <- Object$Grow_Object$Regulate$nu
  
  Beta             <- Object$Grow_Object$Beta_Estimate$Beta
  Beta_Hat_List    <- Object$Grow_Object$Beta_Estimate$Beta_Hat_List
  
  use_raw <- Object$use_raw
  vimpFlag <- (importance == TRUE && testFlag == TRUE)
  vimpFlag_Coef <- (importance_Coef == TRUE && testFlag == TRUE)
  intercept  <- Object$intercept
  y_scale_summary <- Object$y_scale_summary
  y_scale_ordinal <- Object$y_scale_ordinal

  id_index <- lapply(1 : n, function(i){
    which(id == unq_id[i])
  })

  id_index_x <- lapply(1 : n, function(i){
    which(id_x == unq_id[i])
  })

  tm_list <- lapply(1 : n, function(i){
    tm[ (id_index[[i]] ) ]
  })

  tm_x_list <- lapply(1 : n, function(i){
    tm_x[ (id_index_x[[i]] ) ]
  })

  if(is.null(time_map))
  {
    if(async_x)
    {
      time_map <- lapply(1 : n, function(i){
        time_map_temp <- matrix(NA,nrow = ni[i],ncol = ni_x[i])
        null.obj <- lapply(1 : ni[i], function(j){
          time_map_temp[j,] <<- ifelse(tm_x_list[[i]] <= tm_list[[i]][j],1,0)
          NULL
        })
        time_map_temp 
      })
    }else
    {
      time_map <- lapply(1 : n, function(i){
        DiagMat(rep(1,ni[i]))
      })
    }
  }

  obj_C <- predict_BoostMLR_C(x,
                              tm,
                              id,
                              y,
                              id_x,
                              tm_x,
                              x_Mean,
                              x_Std_Error,
                              y_Mean,
                              y_Std_Error,
                              intercept,
                              y_scale_summary,
                              time_map,
                              K,
                              L,
                              H,
                              G,
                              Dk,
                              unq_id,
                              unq_tm,
                              unq_tm_x,
                              unq_x,
                              Bt,
                              Bt_x,
                              Bx,
                              use_raw,
                              Time_Add_New,
                              Time_Unmatch,
                              Beta,
                              Beta_Hat_List,
                              testFlag,
                              M,
                              nu,
                              tm_varying,
                              vimpFlag,
                              vimpFlag_Coef,
                              eps,
                              setting_seed,
                              seed_value)
  
  Error_Rate <- obj_C$Error_Rate
  colnames(Error_Rate) <- y_names
  vimp <- obj_C$vimp
  vimp_Coef <- obj_C$vimp_Coef

  # Set the covariate-interaction to NA for now
  if(vimpFlag)
  {
    for(l in 1 : L)
    {
      vimp[[l]][[2]] <- NA
    }
  }

  if(vimpFlag_Coef)
  {
    for(l in 1 : L)
    {
      vimp_Coef[[l]][[2]] <- NA
    }
  }

  if(vimpFlag)
  {
    names(vimp) <- y_names
    for(l in 1:L)
    {
      names(vimp[[l]][[1]]) <- x_names
      #names(vimp[[l]][[2]]) <- x_names
    }
  }
  
  if(vimpFlag_Coef)
  {
    names(vimp_Coef) <- y_names
    for(l in 1:L)
    {
      names(vimp_Coef[[l]][[1]]) <- x_names
      #names(vimp_Coef[[l]][[2]]) <- x_names
    }
  }
  
  mu <- obj_C$Org_mu
  colnames(mu) <- y_names
  N <- nrow(mu)

  if(y_scale_summary == "binary")
  {
    prob_class <- vector("list", length(y_scale))
    null.obj <- lapply(1 : length(y_scale), function(ii){
      if( y_scale[ii] == "binary")
      {
        which_index <- which(y_scale_ordinal == paste(ii,"_binary",sep="") )
        prob_class[[ ii ]] <<- mu[,which_index,drop = FALSE]
      }else
      {
        if( y_scale[ii] == "ordinal" )
        {
          which_index <- which(y_scale_ordinal == paste(ii,"_ordinal",sep="") )
          mu_temp <- matrix(NA,nrow = N, ncol = ( length(which_index) +1 )  )
          null.obj <- lapply(1 : ncol(mu_temp), function(jj) {
            if(jj == 1)
            {
              mu_temp_temp <-  mu[ , which_index[jj]  ]
              mu_temp[,jj] <<- ifelse(mu_temp_temp < 0, 0, ifelse(mu_temp_temp > 1, 1, mu_temp_temp) )
            }else
            {
              if(jj ==  ncol(mu_temp)  )
              {
                mu_temp_temp <-  (1 - mu[ , which_index[ (jj-1) ]  ])
                mu_temp[,jj] <<- ifelse(mu_temp_temp < 0, 0, ifelse(mu_temp_temp > 1, 1, mu_temp_temp) )
              }else
              {
                mu_temp_temp <- (mu[ , which_index[jj] ] - mu[ , which_index[ (jj-1) ] ])
                mu_temp[,jj] <<- ifelse(mu_temp_temp < 0, 0, ifelse(mu_temp_temp > 1, 1, mu_temp_temp) )
              }
            }
            NULL
          })
         prob_class[[ ii ]] <<- mu_temp
        }
      }     
    })
  }else
  {
    prob_class <- NULL
  }


  if(testFlag)
  {
    mu_Mopt <- obj_C$Org_mu_Mopt
    colnames(mu_Mopt) <- y_names
  } else
  {
    mu_Mopt <- NA
  }
  
  Pred_Object <- obj_C$Pred_Object
  Pred_Object$Dimensions = obj_C$Dimensions
  Pred_Object$Index = obj_C$Index
  Pred_Object$BS = obj_C$BS
  Pred_Object$UseRaw = use_raw
  Pred_Object$Time_Varying = tm_varying
  Pred_Object$Beta_Hat_List = Beta_Hat_List
  Pred_Object$vimp_Coef = vimp_Coef

  obj <- list(Data = obj_C$Data,
              x_names = x_names,
              y_names = y_names,
              intercept = intercept,
              y_scale_summary = y_scale_summary,
              async_x = async_x,
              mu = mu,
              mu_Mopt = mu_Mopt,
              prob_class = prob_class,
              Error_Rate = Error_Rate,
              Mopt = obj_C$Mopt,
              nu = nu,
              rmse = obj_C$rmse,
              vimp = vimp,
              #vimp_Coef = vimp_Coef,
              time_map = time_map,
              Pred_Object = Pred_Object)
  class(obj) <- c("BoostMLR", "predict")
  invisible(obj)
}
