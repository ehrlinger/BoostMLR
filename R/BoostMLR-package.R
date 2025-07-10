

#' Boosting for Multivariate Longitudinal Response
#' 
#' The primary feature of the package is to jointly model multiple longitudinal
#' responses (referred to as multivariate longitudinal response) and multiple
#' covariates and time from a longitudinal study using gradient boosting
#' approach (Pande et al., 2022). Scale of the responses can be continuous or a
#' combination of binary and ordinal.  Covariates can be time-varying or
#' time-invariant. Special cases include modeling of univariate longitudinal
#' response from a longitudinal study, and univariate or multivariate response
#' from a cross-sectional study. The estimated coefficient can be a function of
#' time (referred to as time-varying coefficient in case of a longitudinal
#' study) or a function of pre-specified covariate (in case of a longitudinal
#' or a cross-sectional study) or fixed.
#' 
#' This package allows joint modeling of a multivariate longitudinal response,
#' which is based on marginal model. Estimation is performed using gradient
#' boosting, a generic form of boosting (Friedman J. H., 2001). The boosting
#' approach use in this package is closely related to component-wise L2
#' boosting. Package can handle high dimensionalilty of covariate and response
#' when some of the covariates and responses are pure noise.
#' 
#' The package is designed to identify covariates that affect responses
#' differently as different time intervals. This idea is helpful to dissect an
#' overall effect of covariate into different time intervals. For example, some
#' covariates affect response at the beginning of the follow-up whereas others
#' at a later stage.
#' 
#' @name BoostMLR-package
#' @docType package
#' @section Package Overview: This package contains many useful functions and
#' users should read the help file in its entirety for details.  However, we
#' briefly mention several key functions that may make it easier to navigate
#' and understand the layout of the package.
#' 
#' \enumerate{ \item \command{\link{BoostMLR}}
#' 
#' This is the main entry point to the package. The model is fit using the
#' gradient boosting approach for the user specified training data.
#' 
#' % \item \command{\link{updateBoostMLR}} (\command{updateBoostMLR})
#' 
#' % This allows to update the model by specifying additional boosting
#' iteration.
#' 
#' \item \command{\link{predictBoostMLR}} (\command{predictBoostMLR})
#' 
#' Model performace can be obtained using the test set data. This function also
#' estimate variable importance (VIMP).
#' 
#' }
#' @author Amol Pande and Hemant Ishwaran
#' 
#' Maintainer: Amol Pande <amoljpande@@gmail.com>
#' @seealso \command{\link{BoostMLR}}, %\command{\link{updateBoostMLR}},
#' \command{\link{predictBoostMLR}}, \command{\link{simLong}}
#' @references Pande, A., Ishwaran, H. & Blackstone, E. Boosting for
#' Multivariate Longitudinal Responses. SN COMPUT. SCI. 3, 186 (2022).
#' https://doi.org/10.1007/s42979-022-01072-6
#' 
#' Friedman J.H. (2001). Greedy function approximation: a gradient boosting
#' machine, \emph{Ann. of Statist.}, 5:1189-1232.
#' @keywords package
NULL





#' Laboratory Data
#' 
#' The laboratory data is based on 459 patients who were listed for heart
#' transplant and were put on mechanical circulatory system through device
#' implantation from December 1991 to July 2009 at Cleveland Clinic. These
#' patients had periodic measurements of their bilirubin and creatinine levels.
#' Data from 459 patients includes 18285 measurements of bilirubin and
#' creatinine with an average of 39 measurements per patient.
#' 
#' 
#' @name Laboratory_Data
#' @docType data
#' @format Laboratory data has 4 parts: \enumerate{ \item A total of 41
#' x-variables.  \item Time points (time).  \item Patient identifier (id).
#' \item Longitudinal responses (tbili_po and creat_po).  }
#' @references Rajeswaran J., Blackstone E.H. and Bernard J. Evolution of
#' association between renal and liver function while awaiting for the heart
#' transplant: An application using bivariate multiphase nonlinear mixed effect
#' model. \emph{Statistical methods in medical research} 27(7):2216--2230,
#' 2018.
#' @keywords datasets
#' @examples
#' data(Laboratory_Data, package = "BoostMLR")
NULL



