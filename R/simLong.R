#' Simulate longitudinal data
#' 
#' Simulates longitudinal data from multivariate and univariate longitudinal
#' response model.
#' 
#' Simulates longitudinal data from multivariate and univariate longitudinal
#' response model. We consider following 3 models: \enumerate{ \item
#' \emph{\code{model=1}:} Simpler linear model consist of three longitudinal
#' responses, \code{y1}, \code{y2}, and \code{y3} and four covariates
#' \code{x1}, \code{x2}, \code{x3}, and \code{x4}.  Response \code{y1} is
#' associated with \code{x1} and \code{x4}.  Response \code{y2} is associated
#' with \code{x2} and \code{x4}.  Response \code{y3} is associated with
#' \code{x3} and \code{x4}.
#' 
#' \item \emph{\code{model=2}:} Model consist of single longitudinal response
#' and four covariates. Model includes non-linear relationship between response
#' and covariates and covariate-time interaction.
#' 
#' \item \emph{\code{model=3}:} Model consist of single longitudinal response
#' and four covariates. Model includes non-linear relationship between response
#' and covariates and as well as coefficient varying with time.
#' 
#' Above models are generated for continuous responses, when \code{y_scale} is
#' continuous. In situations where user request binary responses, using
#' \command{y_scale = "binary"}, the original continuous responses are used to
#' generated probabilities (using inverse of logit transformation), and using
#' Burnolli distribution, binary responses are generated.
#' 
#' }
#' 
#' @param n Requested training sample size.
#' @param ntest Requested test sample size.
#' @param N Parameter controlling number of time points per subject.
#' @param rho Correlation parameter.
#' @param model Requested simulation model.
#' @param y_scale Scale of the response.
#' @param n_classes This is applicable for generating ordinal response, and
#' refer to the number of classes where the values ranges from 0 to the
#' n_classes. Thus, if \command{n_classes = 3}, the values are 0, 1, 2 and 3.
#' @param phi Variance of measurement error.
#' @param q_x Number of noise covariates.
#' @param q_y Number of noise responses.
#' @param type Type of correlation matrix.
#' @return An invisible list with the following components: \item{dtaL}{List
#' containing the simulated data in the following order: \code{features},
#' \code{time}, \code{id} and \code{y}.} \item{dta}{Simulated data given as a
#' data frame.} \item{trn}{Index of \code{id} values identifying the training
#' data.}
#' @author Amol Pande and Hemant Ishwaran
#' @references Pande A., Li L., Rajeswaran J., Ehrlinger J., Kogalur U.B.,
#' Blackstone E.H., Ishwaran H. (2017).  Boosted multivariate trees for
#' longitudinal data, \emph{Machine Learning}, 106(2): 277--305.
#' @keywords simulation variable selection
simLong <- function(n = 100,
                    ntest = 0,
                    N = 5,
                    rho = 0.8,
                    model = c(1, 2, 3),
                    y_scale = c("continuous","binary","ordinal"),
                    n_classes = NULL,
                    phi = 1,
                    q_x = 0,
                    q_y = 0,
                    type = c("corCompSym", "corAR1", "corSymm", "iid"))
{
  dta <- data.frame(do.call("rbind", lapply(1:(n+ntest), function(i) {
    Ni <- round(runif(1, 1, 3 * N))
    type <- match.arg(type, c("corCompSym", "corAR1", "corSymm", "iid"))
    if (type == "corCompSym") {
      corr.mat <- matrix(rho, nrow=Ni, ncol=Ni)
      diag(corr.mat) <- 1
    }
    if (type == "corAR1") {
      corr.mat <- diag(rep(1, Ni))
      if (Ni > 1) {
        for (ii in 1:(Ni - 1)) {
          corr.mat[ii, (ii + 1):Ni] <- rho^(1:(Ni - ii))
        }
        ind <- lower.tri(corr.mat)
        corr.mat[ind] <- t(corr.mat)[ind]
      }
    }
    if (type == "iid") {
      corr.mat <- diag(rep(1, Ni))
    }
    tm <- sort(sample((1:(3 * N))/N, size = Ni, replace = TRUE))
    if (model == 1) {
      x1 <- rnorm(Ni)
      x2 <- rnorm(Ni)
      x3 <- rnorm(Ni)
      x4 <- rnorm(Ni)
      x <- cbind(x1, x2, x3, x4)
      p <- ncol(x)
      if (q_x > 0) {
        xnoise <- matrix(rnorm(Ni*q_x),ncol = q_x)
        x <- cbind(x, xnoise)
      }
      eps1 <- sqrt(phi) * t(chol(corr.mat)) %*% rnorm(Ni)
      eps2 <- sqrt(phi) * t(chol(corr.mat)) %*% rnorm(Ni)
      eps3 <- sqrt(phi) * t(chol(corr.mat)) %*% rnorm(Ni)
      y1 <- 1.5 + 1.5 * x1 + 1 * x4 + eps1
      y2 <- 1.5 + 1.5 * x2 + 1 * x4 + eps2
      y3 <- 1.5 + 1.5 * x3 + 1 * x4 + eps3
      l_pred <- cbind(y1,y2,y3)
      if (q_y > 0) {
        ynoise <- matrix(rnorm(Ni*q_y),ncol = q_y)
        l_pred <- cbind(l_pred,ynoise)
      }
    }
    if (model == 2) {
      x1 <- rnorm(Ni)
      x2 <- rnorm(Ni)
      x3 <- rnorm(Ni)
      x4 <- rnorm(Ni)
      x <- cbind(x1, x2, x3, x4)
      p <- ncol(x)
      if (q_x > 0) {
        xnoise <- matrix(rnorm(Ni*q_x),ncol = q_x)
        x <- cbind(x, xnoise)
      }
      eps <- sqrt(phi) * t(chol(corr.mat)) %*% rnorm(Ni)
      b.x1 <- ifelse(tm < 1.5, 0, 1.5 * x1 )
      y1 <- 1.5 + b.x1 - 0.65 * (x2^2) * (tm^2) - 2.5 * x3 - 1.5 * exp(x4) + eps
      l_pred <- cbind(y1)
      if (q_y > 0) {
        ynoise <- matrix(rnorm(Ni*q_y),ncol = q_y)
        l_pred <- cbind(l_pred,ynoise)
      }
    }
    if (model == 3) {
      x1 <- sort(rnorm(Ni))
      x2_Temp <- sort(runif(n = Ni,min = 0,max = 1))
      x2 <- unlist(lapply(1:Ni,function(ii){ if(tm[ii] > 1 && tm[ii] < 2) -1*x2_Temp[ii] else x2_Temp[ii] }))
      x3 <- rnorm(1)
      x4 <- rnorm(1)
      x <- cbind(x1, x2, x3, x4)
      B1 <- 2
      B2 <- tm^2
      B3 <- exp(tm)
      B4 <- 1
      p <- ncol(x)
      if (q_x > 0) {
        xnoise <- matrix(rnorm(Ni*q_x),ncol = q_x)
        x <- cbind(x, xnoise)
      }
      eps <- sqrt(phi) * t(chol(corr.mat)) %*% rnorm(Ni)
      y1 <- 1.5 + B1 * x1 + B2 * x2 + B3 * x3 + B4 * x4 + eps
      l_pred <- cbind(y1)
      if (q_y > 0) {
        ynoise <- matrix(rnorm(Ni*q_y),ncol = q_y)
        l_pred <- cbind(l_pred,ynoise)
      }
    }
    cbind(x,tm, rep(i, Ni), l_pred)
  })))
  d_x <- q_x + 4
  if(model == 1){
    d_y <- q_y + 3
  }
  if(model == 2){
    d_y <- q_y + 1
  }
  if(model == 3){
    d_y <- q_y + 1
  }

  colnames(dta) <- c(paste("x", 1:d_x, sep = ""), "time", "id", paste("y", 1:d_y, sep = ""))

  if(y_scale == "binary")
  {
    mat_l_pred <- dta[,  paste("y", 1:d_y, sep = ""), drop = FALSE]
    null.obj <- lapply(1 : d_y, function(ll){
      mu <-  exp(mat_l_pred[,ll])/( 1 + exp(mat_l_pred[,ll]) )
      dta[,  paste("y", 1:d_y, sep = "")[ll] ] <<- rbinom(n = length(mu),size = 1,prob = mu)
      NULL
    })
  }

  if(y_scale == "ordinal")
  {
    if(is.null(n_classes))
    {
      stop("For the ordinal response, n_classes cannot be NULL")
    }

    mat_l_pred <- dta[,  paste("y", 1:d_y, sep = ""), drop = FALSE]
    null.obj <- lapply(1 : d_y, function(ll){
      mu <-  exp(mat_l_pred[,ll])/( 1 + exp(mat_l_pred[,ll]) )
      dta[,  paste("y", 1:d_y, sep = "")[ll] ] <<- rbinom(n = length(mu),size = n_classes,prob = mu)
      NULL
    })
  }

  dtaL <- list(features = dta[, 1:d_x], time = dta$time, id = dta$id, y = dta[, -c(1:(d_x+2)) ])
  trn <- c(1:sum(dta$id <= n))
  return(invisible(list(dtaL = dtaL, dta = dta, trn = trn)))
}
