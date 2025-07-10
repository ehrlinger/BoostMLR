#' Boosting for Multivariate Longitudinal Response
#' 
#' Function jointly models multiple longitudinal responses (collectively
#' referred to as multivariate longitudinal response) and multiple covariates
#' and time from a longitudinal study using gradient boosting approach (Pande
#' et al., 2022). Scale of the responses can be continuous or a combination of
#' binary and ordinal.  Covariates can be time-varying or time-invariant. Among
#' the time-varying covariates, we consider covariates that are measured at the
#' time of the response or are measured at time points which are different than
#' time of measurement of the responses. Study design in which both covariates
#' and responses are measured at the same time points is referred to as
#' synchronous longitudinal study, whereas when covariates and responses are
#' measured at different time points, we referred it as an asynchronous study
#' design.  Special cases include modeling of univariate longitudinal response
#' from a longitudinal study, and univariate or multivariate response from a
#' cross-sectional study. The estimated coefficient can be a function of time
#' (referred to as time-varying coefficient in case of a longitudinal study) or
#' a function of pre-specified covariate (in case of a longitudinal or a
#' cross-sectional study) or the coefficient can be fixed similar to a
#' parametric model.
#' 
#' This is a non-parametric approach for joint modeling of multivariate
#' longitudinal responses, which is based on a marginal model. Details about
#' the estimations are provided in Pande at el (2022). Estimation is performed
#' using gradient boosting, a generic form of boosting (Friedman J. H., 2001).
#' Our boosting approach is closely related to component-wise L2 boosting.
#' Approach can handle multivariate longitudinal resposes as well as
#' time-varying covariates.  Useful for fitting time-varying coefficient model
#' as well as fixed coefficient model. Special case includes data generated
#' from a cross-sectional study where coefficients can be functions of a
#' pre-selected covariate. In case of longitudinal study design, the approach
#' can handle a situation where responses and covariates are measured at
#' different time points (asynchronous longitudinal study design). Approach can
#' handle high dimensionalilty of covariate and response when some of the
#' covariates and responses are pure noise. Approach is designed to identify
#' covariates that affect responses differently as different time intervals.
#' This idea is helpful to dissect an overall effect of covariate into
#' different time intervals. For example, some covariates affect response at
#' the beginning of the follow-up whereas others at a later stage.
#' 
#' %Shrinking allows for early termination of boosting to prevent overfitting.
#' Also, it %provides a parsimonious model by shrinking coefficient for
#' non-informative %covariate-response pair to zero.
#' 
#' @param x Data frame (or matrix) containing x-values (covariates). Number of
#' rows should match with number of rows of response \code{y} in case of
#' synchronous study design but they maybe different in case of asynchronous
#' study design. Covariates can be time-varying or time-invariant. Missing
#' values are allowed, and are ignored during estimation. If unspecified, model
#' will be fitted with time alone (applicable in the situation when the
#' interest is to obtain an estimated mean response trajectory over time
#' without the influence of any covariates).
#' @param tm Vector of time values corresponding to the measurement of response
#' \code{y}, one entry for each row of the response. In the case of
#' longitudinal study, the estimated coefficient will be a function of
#' \code{tm} when \code{bm_varying} = TRUE. If unspecified, data is assumed to
#' be generated from a cross-sectional study, and the relationship between
#' \code{y} and \code{x} can be obtained. In case of a longitudinal or
#' cross-sectional study, coefficient can be a function of covariate \code{z}
#' which may or may not a part of \code{x} by using \code{z} in place of
#' \code{tm} or it can be fixed when \code{tm_varying} = FALSE.
#' @param id Vector of subject identifier with same length as the number of
#' rows of \code{y}. If \code{id} is unspecified along with \code{tm}, data is
#' assumed to be generated from a cross-sectional study, and the relationship
#' between \code{y} and \code{x} can be obtained.
#' @param y Data frame (or matrix) containing the y-values (response) in case
#' of multivariate responses or a vector of y-values in case of univariate
#' response.
#' @param id_x Vector of subject identifier with same length as the number of
#' rows of \code{x} when the data is generated from an asynchronous
#' longitudinal study design. Ignore this option when the data is generated
#' from a synchronous longitudinal study design.
#' @param tm_x Vector of time values corresponding to the measurement of
#' \code{x} when the data is generated from an asynchronous longitudinal study
#' design. Ignore this option when the data is generated from a synchronous
#' longitudinal study design.
#' @param y_scale This should be a vector with number of elements same as the
#' number of responses, identifying the scales of responses in same same order
#' as the responses ordered in \code{y}. For example, if \code{y} has 3
#' columns, and resposes are ordered as binary, ordinal, ordinal, then
#' \code{y_scale} should be \command{y_scale =
#' c("binary","ordinal","ordinal")}.
#' @param intercept TRUE or FALSE, depending on whether user wanted to model
#' intercept along with all the covariates or not. If the responses are all
#' continuous, then we standardized the responses, such that, each response
#' will have mean = 0, and thus, in this case, \code{intercept} should be
#' FALSE. If the response is binary and/or ordinal, user must set
#' \command{intercept = TRUE}. We left this option unspecified in case user may
#' wants to test this option.
#' @param tm_varying TRUE or FALSE, depending on whether the one wants to fit
#' time-varying coefficient model or a fixed coefficient model. Default is
#' TRUE, and hence time-varying coefficient model will be fitted.
#' @param tm_bs If \code{tm} is specified, should \code{tm} is mapped using
#' B-spline or use original scale of \code{tm}? Default is TRUE, which allows
#' mapping of \code{tm} using B-spline.
#' @param nknots_t If \command{tm_bs} = TRUE, specify the number of knots of
#' B-spline for \code{tm}.
#' @param d_t If \command{tm_bs} = TRUE, specify degree of polynomial of
#' B-spline for \code{tm}.
#' @param nknots_t_x If the data is generated from an asynchronous longitudinal
#' study design, then user can specify mapping of \code{tm_x} using B-spline,
#' and this option allows user to specify the number of knots of B-spline for
#' \code{tm_x}.
#' @param d_t_x If the data is generated from an asynchronous longitudinal
#' study design, then user can specify mapping of \code{tm_x} using B-spline,
#' and this option allows user to specify the degree of polynomial of B-spline
#' for \code{tm_x}.
#' @param tm_separate Ignore this option when the data is generated from a
#' synchronous longitudinal study design or use TRUE or FALSE when the data is
#' generated from an asynchronous longitudinal study design. Use TRUE when user
#' wanted to use separate B-spline mapping for \code{tm_x} and use FALSE if the
#' B-spline mapping that was used for \code{tm} should be applied for
#' \code{tm_x}. Broadly, this option is asking user if the distribution of
#' \code{tm} and \code{tm_x} is same, and hence both \code{tm} and \code{tm_x}
#' have similar time range and if so, set \command{tm_separate = TRUE}. On the
#' other hand if \code{tm} is distict from \code{tm_x}, for example when
#' responses are measured towards the end of follow-up, whereas covariates are
#' measured during the follow-up, then set \command{tm_separate = FALSE}.
#' @param x_raw_all TRUE or FALSE depending on whether to use original (or raw)
#' scale of \code{x} or map each covariate using B-spline? Default is TRUE,
#' which means original scale of \code{x} is used; if FALSE, covariates
#' measured on the continuous scale will be mapped using B-splines.
#' @param x_raw_names If \command{x_raw_all} = FALSE, specify names of the
#' covariates, measured on a continuous scale, that should be used on the
#' original scale and not mapped using B-spline. Note that, even if
#' \command{x_raw_all} = FALSE, covariates not measured on a continuous scale,
#' such as binary, nominal, and ordinal covariates will be used without
#' mapping.
#' @param nknots_x Specify the number of knots for B-spline of \code{x}. This
#' can be a vector of length equal to the number of covariates or a scalar.  If
#' scalar, the same value will be used for all covariates.
#' @param d_x Specify the degree of polynomial for B-spline of \code{x}. This
#' can be a vector of length equal to the number of covariates or a scalar.  If
#' scalar, the same value will be used for all covariates.
#' @param M Number of boosting iterations. In most cases, this should be at
#' least 500.
#' @param nu Boosting regularization parameter.  A value from the interval
#' (0,1].
#' @param mod_grad TRUE or FALSE, depending on whether a modified gradient
#' should be used? Modified gradient is a special type of gradient that is
#' independent of the correlation coefficient. Pande A. (2017) observed that
#' prediction performance increases under modified gradient.
#' @param var_flag Estimate the variance (scale parameter) and correlation
#' parameter for each \code{y}? Applicable for a longitudinal study. If
#' \code{var_flag} = FALSE, a fixed value of scale parameter = 1 and
#' correlation parameter = 0 is used.
#' @param verbose Print the current stage of boosting iteration?
#' @param trace Print the current stage of execution? Useful for identifying
#' bug in the code.
#' @param setting_seed Set \code{setting_seed} = TRUE if you intend to
#' reproduce the result.
#' @param seed_value Seed value.
#' @param time_map Ignore this option for the synchronous longitudinal data. In
#' case of asynchronous longitudinal data, this is already taken care of, and
#' hence user can ignore this in most cases. Here we are providing some details
#' so that the user can use this option if necessary. Note that in case of an
#' asynchronous data, we expect \code{tm_x} for a specific instance to be same
#' or smaller than \code{tm}. This is because we expect covariates to impact
#' the respose, and hence it should be measured prior to measurement of
#' responses or at the same time as the response. This idea is utilized in
#' \code{time_map}, which is a list with same number as the number of subjects,
#' and for each subject, there will be a matrix of dimension number of
#' measurements of \code{tm} and number of measurements of \code{tm_x}. In this
#' matrix, the cell takes value 1 if the corresponding instace of \code{tm_x}
#' is less than or equal to the corresponding instance of \code{tm}, and 0
#' othewise.
#' @param time_delta This option works along with \code{time_map}. Earlier we
#' mention the idea behind values taken within \code{time_map} to be 0 or 1.
#' Here, \code{time_delta} represents the maximum distance allowed between
#' \code{tm_x} and \code{tm}, such that the value for \code{time_map} will be 0
#' even if \code{tm_x} is smaller than \code{tm}, when the distance exceeds by
#' \code{time_delta}.
#' @param comp_x Note that our approach is based on component-wise gradient
#' boosting, where, for any given boosting iteration, only one covariate (or
#' component) is getting selected to update the model. The option \code{comp_x}
#' is a list with number of elements same as the number of covariates, and each
#' element provides a list of indices of the covariates getting selected along
#' with that specific covariate. For example, in a situation where everytime
#' our algorithm selects k'th covariate, we would also like to select p'th
#' covariate, where k are p are different, then the k'th element of
#' \code{comp_x} should have a vector with values k and p.
#' @param comp_y Same logic as described for \code{comp_x} but for the
#' responses.
#' @param int_bs Whether to use intercept term for all the B-spline mapping.
#' Default is true, and we observed better performance with this option.
#' @param Calculate_tm_Matrix Use TRUE when \code{tm} (and \code{tm_x}) is
#' binary or has a few unique values. In this case, we generate an identity
#' matrix with dimension same as number of unique values instead of using
#' B-spline.
#' @param Use_tm_Supply Use TRUE in situation where user specify their own
#' matrices for time mapping instead of we using B-spline or using idea
#' described in \code{Calculate_tm_Matrix}.
#' @param tm_Supply This is where the user should provide the matrix for
#' \code{tm} when \code{Use_tm_Supply} is TRUE.
#' @param tm_Supply_x This is where the user should provide the matrix for
#' \code{tm_x} when \code{Use_tm_Supply} is TRUE.
#' @param y_summary_supply Use TRUE in situations where the users would like to
#' supply their own summary statistics for the response matrix to be used for
#' standardizing responses?
#' @param y_mean_supply Mean of response variables to be supplied when
#' \command{y_summary_supply} = TRUE.
#' @param y_std_error_supply Squared error distance of response variables to be
#' supplied when \command{y_summary_supply} = TRUE.
#' @param ... Further arguments passed to or from other methods.
#' @return An object of class \code{(BoostMLR, grow)} with the following
#' components:
#' 
#' \item{x}{Matrix containing x-values.} \item{id}{Vector of subject identifier
#' ordered according to \code{y}.} \item{tm}{Vector of time values
#' corresponding to the measurements of \code{y}.} \item{y}{Matrix containing
#' y-values.} \item{id_x}{Vector of subject identifier ordered according to
#' \code{x}. In case of the synchronous data, this is same as \code{id}.}
#' \item{tm_x}{Vector of time values corresponding to the measurements of
#' \code{x}. In case of the synchronous data, this is vector of 1.}
#' \item{y_scale}{The scale for the \code{y} as supplied by the user. Can be a
#' vector or a scaler depending on whether \code{y} is univariate or
#' multivariate.} \item{intercept}{Intercept term is used in the model or now?}
#' \item{y_scale_summary}{Using \code{y_scale}, we summarize if the scale of
#' responses as a scalar. For example, if all the responses are continuous,
#' then \code{y_scale_summary} would be continuous, however in case if the
#' responses are a combination of binary and ordinal, then
#' \code{y_scale_summary} would be binary, since for the purpose of modeling,
#' ordinal response will be converted to multiple binary responses.}
#' \item{y_scale_ordinal}{This is used to identify if any of the response is
#' ordinal or not.} \item{use_raw}{Logical vector indicating indexes of
#' covariates which are used on the original (or raw) scale without B-spline
#' mapping.} \item{async_x}{This is used to identify if the data is
#' asynchronous (TRUE) or synchronous (FALSE).} \item{tm_separate}{Null if the
#' data is generated from a synchronous data or TRUE/FALSE for the asynchronous
#' data.} \item{x_names}{Variable names of \code{x}.} \item{y_names}{Variable
#' names of \code{y}.} \item{M}{Number of boosting iterations. If boosting
#' terminates before a pre-specified \code{M}, this indicates the last boosting
#' iteration before termination.} \item{nu}{Regularization parameter.}
#' \item{Tm_Beta}{An estimate of the parameter beta on the original scale of
#' the covariate-response pair. This consist of 3 hierarchical levels of list.
#' A list on the top has length equal to the number of multivariate response
#' (denoted by L). The list below it corresponds to covariates and has lenght
#' equal to the number of covariates (denoted by K). In the bottom of this
#' hierarchy is a list corresponds to the subjects, and had length equal to the
#' number of subjects in the data. If \code{tm_varying} = TRUE, each element
#' from the list represents a matrix with number of rows equal to the number of
#' measurements of \code{y} over time for that subject and the number of
#' columns equal to the number of measurements of \code{x} over time for the
#' same subject. In case of the synchronous data, this would be a diagonal
#' matrix. Each cell of the matrix represents an estimate of time-varying
#' coefficient for the given covariate-response pair. If \code{tm_varying} =
#' FALSE, in place of estimate of time-varying coefficient, a fixed estimate is
#' provided similar to the estimate from a parametric model. The result is
#' provided for covariates who are treated on the original scale rather than
#' using B-spline mapping; for covariates who are mapped using B-spline, the
#' estimates are difficult to interprete and therefore the output is NA.}
#' \item{Tm_Beta_Std}{An estimate of the parameter beta on the standardized
#' scale of the covariate-response pair and has the form similar to
#' \code{Tm_Beta}. These estimates are useful to compare the magnitude of
#' effects of multiple covariates on the response.} \item{mu}{Estimate of the
#' conditional expectation of \code{y} corresponding to the M'th boosting
#' iterations.} \item{prob_class}{For ordinal responses, this provide
#' probabilities for each class, rather than cumulative probabilities.}
#' \item{Q_set_list}{For ordinal responses, this provide unique values for each
#' response.} \item{Error_Rate}{Training error rate for each response across
#' the boosting iterations.} \item{Variable_Select}{Indexes of important
#' covariates that get picked-up across time and across boosting iterations.
#' Result is shown as a list with number of elements equal to M, and each
#' element of the list consist of matrix of dimension G by H, where G refers to
#' the number of parameters used for mapping of \code{tm} and H refers to the
#' number of parameters used for mapping of \code{tm_x}. Each element of the
#' matrix represents the index of the important covariate that got selected
#' during the corresponding boosting iteration.} \item{Response_Select}{Indexes
#' of important responses that get picked-up across time and across boosting
#' iterations. Result is shown as a list with number of elements equal to M,
#' and each element of the list consist of matrix of dimension G by H, where G
#' refers to the number of parameters used for mapping of \code{tm} and H
#' refers to the number of parameters used for mapping of \code{tm_x}. Each
#' element of the matrix represents the index of the important response that
#' got selected during the corresponding boosting iteration.}
#' \item{var_flag}{Whether the variance (scale parameter) and correlation are
#' estimated?} \item{tm_varying}{Whether estimates are time-varying or fixed?}
#' \item{Phi}{Matrix, having dimension M by L, representing an estimate of
#' variance (scale parameter) for each response across the boosting
#' iterations.} \item{Rho}{Matrix, having dimension M by L, represent an
#' estimate of correlation for each response across the boosting iterations.}
#' %\item{Lambda_List}{Estimate of the lambda (the L1 penaulty parameter) for
#' each boosting iterations. Useful for internal calculation.}
#' \item{comp_x}{Refer to the \code{comp_x} supplied by the user or }
#' \item{update_all_comp}{Related to \code{comp_x}; take value TRUE when we
#' update all covariates (components) during each boosting iteration, rather
#' than a single covariate.} \item{Grow_Object}{Useful for internal
#' calculation.} %\item{Time_Add_New}{If \code{dt_Add} is specified, this
#' corresponds to time measurements %from \code{dt_Add}, aligned with
#' \code{tm}.}
#' @author Amol Pande and Hemant Ishwaran
#' @seealso %\command{\link{updateBoostMLR}}, \command{\link{predictBoostMLR}},
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
#' 
#' Friedman J.H. (2001). Greedy function approximation: a gradient boosting
#' machine, \emph{Ann. of Statist.}, 5:1189-1232.
#' @keywords boosting
#' @examples
#' 
#' 
#' \donttest{
#' ##-----------------------------------------------------------------
#' ## Multivariate Continuous Longitudinal Response - synchronous data
#' ##-----------------------------------------------------------------
#' 
#' # Simulate data involves 3 response and 4 covariates
#' 
#' dta <- simLong(n = 100, N = 5, rho =.80, model = 1, 
#'                                   y_scale = "continuous", q_x = 0, 
#'                                   q_y = 0,type = "corCompSym")$dtaL
#' 
#' # Boosting call: Raw values of covariates, B-spline for time, 
#' # no estimate of rho and phi
#' 
#' boost.grow <- BoostMLR(x = dta$features, tm = dta$time, id = dta$id, 
#'                        y = dta$y,
#'                        y_scale = rep("continuous",3),intercept = FALSE, 
#'                        M = 100, var_flag = FALSE)
#' 
#' 
#' # Plot training error
#' plotBoostMLR(boost.grow$Error_Rate,xlab = "m",ylab = "Training Error")
#' 
#' 
#' ##-----------------------------------------------------------------
#' ## Multivariate Continuous Longitudinal Response - asynchronous data
#' ##-----------------------------------------------------------------
#' 
#' # Simulate data involves 3 response and 4 covariates. In order to
#' # create an asynchronous data, we are going to remove some of the
#' # observations from covariates, which will make number of measurements
#' # of covariates different from number of measurements of responses.
#' 
#' dta <- simLong(n = 100, N = 5, rho =.80, model = 1, 
#'                y_scale = "continuous", q_x = 0, 
#'                q_y = 0,type = "corCompSym")$dtaL
#' 
#' unq_id <- unique(dta$id)
#' 
#' id_index <- lapply(1 : length(unq_id), function(i){
#'   which(dta$id == unq_id[i])
#' })
#' 
#' id_index_x <- unlist(lapply(1 : length(unq_id), function(i){
#'   samp_obs <- sample( x = id_index[[i]], size = floor(0.9 * length(id_index[[i]])), replace = FALSE)
#'   if( length(samp_obs) == 0)
#'   {
#'     samp_obs <- id_index[[i]]
#'   }
#'   samp_obs
#' }))
#' 
#' dta$time_x <- dta$time
#' dta$id_x   <- dta$id
#' 
#' dta$time_x <- dta$time_x[ id_index_x ]
#' dta$id_x <- dta$id_x[ id_index_x ]
#' dta$features <- dta$features[id_index_x, ,drop = FALSE]        
#' 
#' # Boosting call: Raw values of covariates, B-spline for time, 
#' # no estimate of rho and phi
#' 
#' boost.grow <- BoostMLR(x = dta$features, tm = dta$time, id = dta$id, 
#'                        y = dta$y, id_x = dta$id_x,tm_x = dta$time_x,
#'                        y_scale = rep("continuous",3),intercept = FALSE,
#'                        tm_separate = FALSE, 
#'                        M = 100, var_flag = FALSE)
#' 
#' 
#' # Plot training error
#' plotBoostMLR(boost.grow$Error_Rate,xlab = "m",ylab = "Training Error")
#' 
#' ##-----------------------------------------------------------------
#' ## Multivariate Binary Longitudinal Response - synchronous data
#' ##-----------------------------------------------------------------
#' 
#' # Simulate data involves 3 response and 4 covariates
#' 
#' dta <- simLong(n = 100, N = 5, rho =.80, model = 1, 
#'                                   y_scale = "binary", q_x = 0, 
#'                                   q_y = 0,type = "corCompSym")$dtaL
#' 
#' # Boosting call: Raw values of covariates, B-spline for time, 
#' # no estimate of rho and phi
#' 
#' boost.grow <- BoostMLR(x = dta$features, tm = dta$time, id = dta$id, 
#'                        y = dta$y,
#'                        y_scale = rep("binary",3),intercept = TRUE, 
#'                        M = 100, var_flag = FALSE)
#' 
#' 
#' # Plot training error
#' plotBoostMLR(boost.grow$Error_Rate,xlab = "m",ylab = "Training Error")
#' 
#' ##-----------------------------------------------------------------
#' ## Multivariate Ordinal Longitudinal Response - synchronous data
#' ##-----------------------------------------------------------------
#' 
#' # Simulate data involves 1 response and 4 covariates
#' 
#' dta <- simLong(n = 100, N = 5, rho =.80, model = 2, 
#'                                   y_scale = "ordinal", n_classes = 3, 
#'                                   q_x = 0, q_y = 0,type = "corCompSym")$dtaL
#' 
#' # Boosting call: Raw values of covariates, B-spline for time, 
#' # no estimate of rho and phi
#' 
#' boost.grow <- BoostMLR(x = dta$features, tm = dta$time, id = dta$id, 
#'                        y = dta$y,
#'                        y_scale = "ordinal",intercept = TRUE, 
#'                        M = 100, var_flag = FALSE)
#' 
#' 
#' # Plot training error
#' plotBoostMLR(boost.grow$Error_Rate,xlab = "m",ylab = "Training Error")
#' 
#' ##-----------------------------------------------------------------
#' ## Univariate Continuous Longitudinal Response - synchronous data
#' ##-----------------------------------------------------------------
#' 
#' # Simulate data involves 1 response and 4 covariates
#' 
#' dta <- simLong(n = 100, N = 5, rho =.80, model = 2, 
#'                                   y_scale = "continuous", q_x = 0, 
#'                                   q_y = 0,type = "corCompSym")$dtaL
#' 
#' # Boosting call: B-spline for time and covariates,
#' # estimate of rho and phi 
#' 
#' boost.grow <- BoostMLR(x = dta$features, tm = dta$time, id = dta$id, 
#'                           y = dta$y,
#'                           y_scale = "continuous",intercept = FALSE,   
#'                           M = 100, tm_bs = TRUE,
#'                           x_raw_all = FALSE, var_flag = TRUE)
#' 
#' # Plot training error
#' plotBoostMLR(boost.grow$Error_Rate,xlab = "m",ylab = "Training Error")
#' 
#' # Plot phi
#' plotBoostMLR(boost.grow$Phi,xlab = "m",ylab = "phi")
#' 
#' # Plot rho
#' plotBoostMLR(boost.grow$Rho,xlab = "m",ylab = "rho")
#' 
#' 
#' ##-----------------------------------------------------------------
#' ## Multivariate Continuous Longitudinal Response - synchronous data
#' ##-----------------------------------------------------------------
#' 
#' # Simulate data involves 3 response and 4 covariates
#' 
#' dta <- simLong(n = 100, N = 5, rho =.80, model = 1, 
#'                                   y_scale = "continuous", q_x = 0, 
#'                                   q_y = 0,type = "corCompSym")$dtaL
#' 
#' # Boosting call: Raw values of covariates, fixed parameter estimates
#' # instead of time varying, no estimate of rho and phi
#' 
#' boost.grow <- BoostMLR(x = dta$features, tm = dta$time, id = dta$id, 
#'                        y = dta$y,
#'                        y_scale = rep("continuous",3),
#'                        intercept = FALSE, 
#'                        M = 100,tm_varying = FALSE,var_flag = FALSE)
#' 
#' # Print time-invarint parameter estimates
#' 
#' est_beta <- lapply(1 : ncol(boost.grow$y), function(l){
#'    unlist(lapply(1 : ncol(boost.grow$x), function(k){
#'      unique(unlist(lapply(1 : length(unique(boost.grow$id)), function(i){
#'        unique(diag(boost.grow$Tm_Beta[[ l ]][[ k ]][[ i ]]))
#'      })))
#'    }))
#'  })
#' est_beta
#' 
#' ##-----------------------------------------------------------------
#' ## Multivariate Continuos Response from Cross-sectional Data: Estimated 
#' ## coefficient as a function of covariate
#' ##-----------------------------------------------------------------
#' 
#' if (library("mlbench", logical.return = TRUE)) {
#' data("BostonHousing")
#' 
#' x <- BostonHousing[,c(1:7,9:12)]
#' tm <- BostonHousing[,8]
#' id <- 1:nrow(BostonHousing)
#' y <- BostonHousing[,13:14]
#' 
#' # Boosting call: Raw values of covariates, B-spline for covariate "dis", 
#' 
#' boost.grow <- BoostMLR(x = x, tm = tm, id = id, y = y,
#'                        y_scale = rep("continuous",2),
#'                        intercept = FALSE,  
#'                        M = 100,var_flag = FALSE)
#' 
#' # Plot training error
#' plotBoostMLR(boost.grow$Error_Rate,xlab = "m",ylab = "Training Error",
#'                                               legend_fraction_x = 0.2)
#' }
#' ##-----------------------------------------------------------------
#' ## Univariate Continuous Response from Cross-sectional Data: Fixed 
#' ## estimated coefficient
#' ##-----------------------------------------------------------------
#' 
#' if (library("mlbench", logical.return = TRUE)) {
#' library(mlbench)
#' data("BostonHousing")
#' 
#' x <- BostonHousing[,1:13]
#' y <- BostonHousing[,14]
#' 
#' # Boosting call: Raw values of covariates
#' 
#' boost.grow <- BoostMLR(x = x, y = y,
#'                        y_scale = "continuous",
#'                        intercept = FALSE, , M = 100)
#' 
#' # Plot training error
#' plotBoostMLR(boost.grow$Error_Rate,xlab = "m",ylab = "Training Error",
#'                                                legend_fraction_x = 0.2)
#' }
#' }
#' 
BoostMLR     <- function(x,
                         tm,
                         id,
                         y,
                         id_x = NULL,
                         tm_x = NULL,
                         y_scale,
                         intercept,
                         tm_varying = TRUE,
                         tm_bs = TRUE,
                         nknots_t = 10,
                         d_t = 3,
                         nknots_t_x = NULL,
                         d_t_x = NULL,
                         tm_separate = NULL, 
                         x_raw_all = TRUE,
                         x_raw_names,
                         nknots_x = 7,
                         d_x = 3,
                         M = 500,
                         nu = 0.05,
                         mod_grad = TRUE,
                         var_flag = TRUE,
                         verbose = TRUE,
                         trace = FALSE,
                         setting_seed = FALSE,
                         seed_value = 100L,
                         time_map = NULL,
                         time_delta = NULL,
                         comp_x = NULL,
                         comp_y = NULL,
                         int_bs = TRUE,
                         Calculate_tm_Matrix = FALSE,
                         Use_tm_Supply = FALSE,
                         tm_Supply = NULL,
                         tm_Supply_x = NULL,
                         y_summary_supply = FALSE,
                         y_mean_supply = NULL,
                         y_std_error_supply = NULL,
                         ...)
{

  shrink <- FALSE
  lower_perc <- 0.25
  upper_perc <- 0.75
  n_lambda <- 100
  lambda <- 0

  if(missing(y))
  {
    stop("y is missing")
  }

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

  if(missing(y_scale))
  {
    stop("y_scale is missing. Please provide a scaler or vector, depending on whether y is a vector or dataframe. Inputs for y_scale should be a string: continuous, binary or ordinal. Also, order of y_scale should match with the order of variables in y (if y is a dataframe)")
  }else
  {
    if( length(setdiff( unique(y_scale), c("continuous","binary","ordinal") )) > 0 )
    {
      stop("Inputs for y_scale should be a string: continuous, binary or ordinal.")
    }else
    {
      # This is to prevent modeling of mixed responses
      if( (any(y_scale == "continuous") && any(y_scale == "binary")) ||  (any(y_scale == "continuous") && any(y_scale == "ordinal")) )
      {
        stop("y_scale should be continuous or a combination of binary and ordinal but the combination of continous and binary or ordinal is not allowed")
      }
      if( length(y_scale) != ncol(y) )
      {
        stop("length of y_scale should be same as the number of variables in y")
      }
    }
  }

  Q_set_list <- lapply(1 : ncol(y), function(l){
    NA
  })

  y_scale_ordinal <- c()

  if( any(y_scale == "ordinal") )
  {
    which_y_ordinal <- which( y_scale == "ordinal")

    y_names <- colnames(y)   
    if(is.null(y_names))
    {
      y_names <- paste("y",1 : ncol(y),sep="")
    }

    temp_y_names <- c()
    temp_y <- data.frame()
    y_scale_mod <- c()
    null.obj <- lapply(1 : ncol(y), function(l) {
      if( any(which_y_ordinal == l) )
      {
        y.unq <- sort(unique(y[,l,drop = TRUE]))
        Q   <- length(y.unq)
        Q_set <- setdiff(y.unq,max(y.unq))
        n.Q  <- length(Q_set)
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
          y_scale_ordinal <<- c(y_scale_ordinal, paste(l,"_ordinal",sep="") )
          NULL
        })
        rm(null.obj.1)
        Q_set_list[[l]] <<- Q_set
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
        y_scale_ordinal <<- c(y_scale_ordinal, paste(l,"_",y_scale[l],sep="") )
      }
      NULL
    })
    rm(null.obj)

    y <- temp_y
    y_names <- temp_y_names
    colnames(y) <- y_names
  }else
  {
    y_names <- colnames(y)   
    if(is.null(y_names))
    {
      y_names <- paste("y",1 : ncol(y),sep="")
    }
    y_scale_mod <-  y_scale 
    Q_set <- NA
    n.Q  <- 1
    y_scale_ordinal <- y_scale 
  }


  if( all(y_scale_mod == "binary"))
  {
    y_scale_summary <- "binary"
  }else
  {
    if(all(y_scale_mod == "continuous"))
    {
      y_scale_summary <- "continuous"
    }else
    {
      stop("y_scale_summary must be binary or continuous")
    }
  }


  if(missing(tm) && missing(x))
  {
    stop("tm and x are missing")
  }

  if(!missing(tm) && missing(id))
  {
    stop("id is missing")
  }

  cross_sectional <- FALSE
  if( missing(tm) && !missing(x) )
  {
    if(!missing(id))
    {
      if(!(length(sort(unique(id))) == nrow(y)) )
      {
        stop("tm is missing")
      }
    } else
    {
      id <- 1:nrow(y)
    }
    tm <- rep(0, nrow(y))
    nknots_t <-  1
    d_t <- 0
    cross_sectional <- TRUE
    var_flag <- FALSE
  }
  
  if(missing(x) && !missing(tm))
  {
    x_miss <- TRUE
    x_raw_all <- TRUE
    x <- cbind(rep(1,nrow(y)))
    if(length(sort(unique(id))) == nrow(y) )
    {
      cross_sectional <- TRUE
      var_flag <- FALSE
    }
  }else
  {
    x_miss <- FALSE
  }

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
    if(tm_separate)
    {
      if( is.null(nknots_t_x) || is.null(d_t_x) )
      {
        stop("either nknots_t_x or d_t_x is null")
      }
    }else
    {
      nknots_t_x <- nknots_t
      d_t_x <- d_t
    }
    async_x <- TRUE
  }else
  {
    async_x <- FALSE
    id_x <- id
    tm_x <- rep(1, length(tm))
    nknots_t_x <- 1
    d_t_x <- 0
  }

  if(nknots_t <= 0 && tm_bs == TRUE)
  {
    stop("nknots_t must be > 0")
  }
  
  if(x_raw_all == FALSE && any(nknots_x <= 0) )
  {
    stop("nknots_x must be > 0")
  }
  
  if(missing(tm) && tm_varying == TRUE)
  {
    tm_varying <- FALSE
  }
  
  if(!tm_bs)
  {
    nknots_t <-  1
    d_t <- 0
  }
  
  if ( any(is.na(id))  ) 
  {  
    stop("missing values encountered in id: remove observations with missing values")
  }

  if (any(is.na(id_x))  ) 
  {  
    stop("missing values encountered in id_x: remove observations with missing values")
  }

  Time_Unmatch <- rep(FALSE,ncol(x))
  N <- nrow(y)
  
 user.option <- list(...)
 lambda_scale <- 1
 rho <- is.hidden.rho(user.option)
 phi <- is.hidden.phi(user.option)
 sq_residual_select <- is.hidden.sq_residual_select(user.option)

 # come back to this later
 update_all_comp <- is.hidden.update_all_comp(user.option)

 lambda_ridge <- lambda
 ridge_penalty <- FALSE
  
 dt_Add <- is.hidden.dt_Add(user.option) 
  
  if(!is.null(dt_Add))
  {
    if(!is.list(dt_Add))
    {
      stop("dt_Add must be a list")
    }
    K_Add <- length(dt_Add)
    nullObj <- lapply(1:K_Add,function(kk){
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
    for(kk in 1:K_Add)
    {
      Ord_id_tm_Add <- Order_Time(ID = dt_Add[[kk]][,1],Time =  dt_Add[[kk]][,2])
      dt_Add[[kk]] <- dt_Add[[kk]][Ord_id_tm_Add,,drop = FALSE]
      id_Add <- dt_Add[[kk]][,1]
      x_Names_Add[kk] <- names(dt_Add[[kk]][,3,drop = FALSE])
      Time_Names_Add[kk] <- names(dt_Add[[kk]][,2,drop = FALSE])
      if(any(is.na(id_Add)))
      {
        stop("Missing values observed for id in dt_Add")
      }
      unq_id_Add <- unique(id_Add)
      n_Add <- length(unq_id_Add)
      nullObj <- unlist(lapply(1:n_Add,function(i){
        Which_id <- which(unq_id_Add[i] == id)
        ni <- length(Which_id)
        if(ni > 0)
        {
          Which_id_Add <- which(id_Add == unq_id_Add[i])
          ni_Add <- length(Which_id_Add)
          tm_Add <- dt_Add[[kk]][Which_id_Add,2]
          x_Add <- dt_Add[[kk]][Which_id_Add,3]
          for(j in 1:ni)
          {
           for(jj in 1:ni_Add)
           {
              if((!is.na(tm_Add[jj]) && !is.na(tm[Which_id[j]])))
              {
                if(tm_Add[jj] <=  tm[Which_id[j]])
                {
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
    Time_Unmatch <- c(Time_Unmatch,rep(TRUE, ncol(x_Add_New)))
    colnames(Time_Add_New) <- Time_Names_Add
  } else
  {
    Time_Add_New <- matrix(0,nrow = N,ncol = 1)
    colnames(Time_Add_New) <- "Time_Add"
  }
  
  #---------------------------------------------------------------------------------- 
  # Date: 12/11/2020
  
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
  # Date: 12/11/2020
  
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
  
  if(!is.matrix(y))
  {
    y <- data.matrix(y)
  }

  K <- ncol(x)
  L <- ncol(y)

  x_names <- colnames(x)

  if(is.null(x_names))
  {
    x_names <- paste("x",1:K,sep="")
  }

  if( ( is.null(rho) && !is.null(phi) ) || ( !is.null(rho) && is.null(phi) )  )
  {
    stop("rho or phi is null")
  }
  
  if( !is.null(rho) && !is.null(phi) )
  {
    var_flag <- FALSE
    if(! ( (length(rho) == 1 || length(rho) == L) && (length(phi) == 1 || length(phi) == L) ) )
    {
      stop("Length of rho and phi must be 1 or L")
    }
    
    if( any(rho < -1) || any(rho > 1) || any(phi <= 0) )
    {
      stop("rho should be between -1 and 1 and phi should be > 0")
    }
    
    if(length(rho) == 1 && length(phi) == 1)
    {
      rho <- rep(rho,L)
      phi <- rep(phi,L)
    }
    Rho <- matrix(rep(rho,M),nrow = M,byrow = TRUE)
    Phi <- matrix(rep(phi,M),nrow = M,byrow = TRUE)
  } else 
  {
    rho <- rep(0,L)
    phi <- rep(1,L)
    Rho <- matrix(rep(rho,M),nrow = M,byrow = TRUE)
    Phi <- matrix(rep(phi,M),nrow = M,byrow = TRUE)
  }
  
  unq_x <- lapply(1:K,function(k){
    sort_unique_C_NA(x[,k,drop = TRUE])
  })
  
  n_unq_x <- unlist(lapply(1:K,function(k){
    length(unq_x[[k]])
  }))
  
  if( !(length(nknots_x) == 1 || length(nknots_x) == K) )
  {
    stop( paste("Length of nknots_x should be either 1 or",K,sep = " ") )
  }
  
  if( !(length(d_x) == 1 || length(d_x) == K) )
  {
    stop( paste("Length of d_x should be either 1 or",K,sep = " ") )
  }
  
  if(length(nknots_x) == 1)
  {
    nknots_x <- rep(nknots_x,K)
  }
  
  if(length(d_x) == 1)
  {
    d_x <- rep(d_x,K)
  }
  
  unique_limit <- unlist(lapply(1:K,function(k){
    nknots_x[k] + d_x[k]
  }))
  
  categorical_x <- x_names[which(unlist(lapply(1:K,function(k){
    (n_unq_x[k] < unique_limit[k])
  })))]
  
  if(length(categorical_x) > 0)
  { 
    if(missing(x_raw_names))
    {
      x_raw_names <- categorical_x
    }else
    {
      x_raw_names <- union(x_raw_names,categorical_x)
    }
  }
  
  if(x_raw_all)
  {
    use_raw <- rep(TRUE,K)
  }else
  {
    if(missing(x_raw_names) )
    {
      use_raw <- rep(FALSE,K)
    }else
    {
      use_raw <- (!is.na(match(x_names,x_raw_names)))
      if(all(use_raw == FALSE))
      {
        stop("x_raw_names do not match with the variables names from x")
      }
    }
  }


  if(y_summary_supply)
  {
    if( is.null(y_mean_supply) || is.null(y_std_error_supply)  )
    {
      stop("Values of y_mean_supply or y_std_error_supply can not be null")
    }else
    {
      y_mean_supply <- y_mean_supply
      y_std_error_supply <- y_std_error_supply
    }
  }else 
  {
    if( y_scale_summary == "binary" )
    {
      y_summary_supply <- TRUE
      y_mean_supply <- rep(0,L)
      y_std_error_supply <- rep(1,L)
    }else
    {
      y_mean_supply <- rep(NA,L)
      y_std_error_supply <- rep(NA,L)
    }
  }

  ProcessedData <- DataProcessing_C(x,
                                    y,
                                    id, 
                                    tm,
                                    id_x,
                                    tm_x,
                                    unq_id,
                                    x_miss,
                                    trace,
                                    y_summary_supply,
                                    y_mean_supply,
                                    y_std_error_supply)
  
  Org_x       <- ProcessedData$Data$Org_x
  Org_y       <- ProcessedData$Data$Org_y
  id          <- ProcessedData$Data$id
  tm          <- ProcessedData$Data$tm
  x           <- ProcessedData$Data$x
  y           <- ProcessedData$Data$y
  id_x        <- ProcessedData$Data$id_x
  tm_x        <- ProcessedData$Data$tm_x

  
  
  if(!is.matrix(Org_x))
  {
    Org_x <- data.matrix(Org_x)
  }

  if(!is.matrix(Org_y))
  {
    Org_y <- data.matrix(Org_y)
  }

  if(!is.matrix(x))
  {
    x <- data.matrix(x)
  }

  if(!is.matrix(y))
  {
    y <- data.matrix(y)
  }
  
  x_Mean      <- ProcessedData$Data$x_Mean
  x_Std_Error <- ProcessedData$Data$x_Std_Error
  y_Mean      <- ProcessedData$Data$y_Mean
  y_Std_Error <- ProcessedData$Data$y_Std_Error
  
  unq_id     <- ProcessedData$Index$unq_id
  id_index   <- ProcessedData$Index$id_index
  id_index_x <- ProcessedData$Index$id_index_x
  
  n     <- ProcessedData$Dimensions$n
  K     <- ProcessedData$Dimensions$K
  L     <- ProcessedData$Dimensions$L
  ni    <- ProcessedData$Dimensions$ni
  N     <- ProcessedData$Dimensions$N
  ni_x  <- ProcessedData$Dimensions$ni_x
  N_x   <- ProcessedData$Dimensions$N_x

  tm_list <- lapply(1 : n, function(i){
    tm[ (id_index[[i]] + 1) ]
  })

  tm_x_list <- lapply(1 : n, function(i){
    tm_x[ (id_index_x[[i]] + 1) ]
  })


  if(all(ni == 1))
  {
    var_flag <- FALSE
  }
  
  H <- nknots_t_x + d_t_x
  G <- nknots_t + d_t

  unq_tm <- sort_unique_C_NA(tm)
  n_unq_tm <- length(unq_tm)

  unq_tm_x <- sort_unique_C_NA(tm_x)
  n_unq_tm_x <- length(unq_tm_x)

##------------------------------------------------------------------------
# Date: 04/13/2021

# I have added two additional argument: Calculate_tm_Matrix, where if the
# tm is binary or ordinal with a few unique values, then rather than using
# B-spline matrix, calculate matrix such that there will be a binary variable
# for each unique time value.
# Alternately, user can supply a matrix similar
# to B-spline type matrix where rows correspond to unique time points.
# These arguments work only when Time_Varying = TRUE and tm_bs = FALSE.
##------------------------------------------------------------------------

  if(async_x)
  {
    if(tm_separate)
    {
      if(cross_sectional == TRUE || tm_varying == FALSE)
      {
        Bt <- cbind(rep(1,n_unq_tm));
        G <- ncol(Bt)

        Bt_x <- cbind(rep(1,n_unq_tm_x));
        H <- ncol(Bt_x)
      }else
      {
        if(tm_bs)
        {
          
          if(n_unq_tm == 1)
          {
            Bt <-  cbind(unq_tm)
            G <- ncol(Bt)
          }else
          {
            Bt <- bs(x = unq_tm,df = G,degree = d_t,intercept = int_bs)
            G <- ncol(Bt)
          }

          if( n_unq_tm_x == 1)
          {
            Bt_x <-  cbind(unq_tm_x)
            H <- ncol(Bt_x)
          }else
          {
            Bt_x <- bs(x = unq_tm_x,df = H,degree = d_t_x,intercept = int_bs)
            H <- ncol(Bt_x)
          }

        }else
        {
          if(Calculate_tm_Matrix)
          {
            Bt <- DiagMat(rep(1,n_unq_tm))
            G <- ncol(Bt)

            Bt_x <- DiagMat(rep(1,n_unq_tm_x))
            H <- ncol(Bt_x)
          }else
          {
            if(Use_tm_Supply)
            {
              Bt <- tm_Supply
              G <- ncol(Bt)

              Bt_x <- tm_Supply_x
              H <- ncol(Bt_x)
            }else
            {
              Bt <-  cbind(unq_tm)
              G <- ncol(Bt)

              Bt_x <-  cbind(unq_tm_x)
              H <- ncol(Bt_x)
            }
          }
        }
      }
    }else
    {
      unq_tm_com <- unique(c(unq_tm,unq_tm_x))
      n_unq_tm_com <- length(unq_tm_com)
      
      if( (n_unq_tm_com < H) && (tm_bs == TRUE) )
      {
        H <- n_unq_tm_com
      }

      if(tm_bs)
      {
        Bt_x <- bs(x = unq_tm_com,df = H,degree = d_t_x,intercept = int_bs)
        H <- ncol(Bt_x)

        Bt <- Bt_x
        G <- H
      }else
      {
        if(Calculate_tm_Matrix)
        {
          Bt_x <- DiagMat(rep(1,n_unq_tm_com))
          H <- ncol(Bt_x)

          Bt <- Bt_x
          G <- H
        }else
        {
          if(Use_tm_Supply)
          {
            Bt_x <- tm_Supply_x
            H <- ncol(Bt_x)

            Bt <- Bt_x
            G <- H
          }else
          {
            Bt_x <-  cbind(unq_tm_com)
            H <- ncol(Bt_x)

            Bt <- Bt_x
            G <- H
          }
        }
      }  
    }
  }else
  {
    if(cross_sectional == TRUE || tm_varying == FALSE)
    {
      Bt <- cbind(rep(1,n_unq_tm));
      G <- ncol(Bt)

      Bt_x <- cbind(rep(1,n_unq_tm_x));
      H <- ncol(Bt_x)
    }else
    {
      if(tm_bs)
      {
        Bt <- bs(x = unq_tm,df = G,degree = d_t,intercept = int_bs)
        G <- ncol(Bt)

        Bt_x <-  cbind(unq_tm_x)
        H <- ncol(Bt_x)
      }else
      {
        if(Calculate_tm_Matrix)
        {
          Bt <- DiagMat(rep(1,n_unq_tm))
          G <- ncol(Bt)

          Bt_x <- DiagMat(rep(1,n_unq_tm_x))
          H <- ncol(Bt_x)
        }else
        {
          if(Use_tm_Supply)
          {
            Bt <- tm_Supply
            G <- ncol(Bt)

            Bt_x <- tm_Supply_x
            H <- ncol(Bt_x)
          }else
          {
            Bt <-  cbind(unq_tm)
            G <- ncol(Bt)

            Bt_x <-  cbind(unq_tm_x)
            H <- ncol(Bt_x)
          }
        }
      }
    }
  }

  unq_x <- lapply(1:K,function(k){
    if(use_raw[k])
    {
      NA
    }else
    {
      sort_unique_C_NA(x[,k,drop = TRUE])
    }
  })

  n_unq_x <- unlist(lapply(1:K,function(k){
    if(use_raw[k])
    {
      NA
    }else
    {
      length(unq_x[[k]])
    }
  }))
  
  Dk <- unlist(lapply(1:K,function(k){
    if(use_raw[k])
    {
     out <- 1
    }else
    {
      Dk_Temp <- nknots_x[k] + d_x[k]
      if(n_unq_x[k] < Dk_Temp)
      {
        out <- n_unq_x[k]
      }else
      {
        out <- Dk_Temp  
      }
    }
   out  
  }))
  
  count <- 0
  Bx <- lapply(1:K,function(k){
    if(use_raw[k])
    {
      if(Time_Unmatch[k])
      {
        count <<- count + 1
        x[,k,drop = FALSE]*Time_Add_New[ , count, drop = FALSE]
      }else 
      {
        x[,k,drop = FALSE]
      }
    }else
    {
        bs(x = unq_x[[k]],df = Dk[k],degree = d_x[k],intercept = int_bs)
    }
  })

  Bx_Scale <- lapply(1:K,function(k){
    if(use_raw[k])
    {
      1
    }else
    {
      rep(1,Dk[k])
    }
  })

  #Bx_Scale_const <- sum(unlist(lapply(1 : K, function(k){
  #  Dk[k]*H
  #})),na.rm = TRUE)

  Bx_Scale_const <- 1.0
  

  Lambda_Ridge_Vec <- unlist(lapply(1:K,function(k){
    lambda_ridge
  }))

  if(is.null(time_map))
  {
    if(async_x)
    {
      time_map <- lapply(1 : n, function(i){
        time_map_temp <- matrix(NA,nrow = ni[i],ncol = ni_x[i])
        null.obj <- lapply(1 : ni[i], function(j){
          if( is.null(time_delta) )
          {
            time_map_temp[j,] <<- ifelse(tm_x_list[[i]] <= tm_list[[i]][j],1,0)
          }else
          {
            null.obj.1 <- lapply(1 : ni_x[i], function(jj){
              if( (tm_x_list[[i]][jj] <= tm_list[[i]][j]) &&  (abs( tm_list[[i]][j] - tm_x_list[[i]][jj] ) <= time_delta)  )
              {
                time_map_temp[j,jj] <<- 1
              }else
              {
                time_map_temp[j,jj] <<- 0
              }
            })           
          }          
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

  if(is.null(comp_x))
  {
    comp_x <- lapply(1 : K, function(k){
      (k - 1)
    })
  }else
  {
    if( !is.list(comp_x) ||  length(comp_x) != K)
    {
      stop("Either comp_x is not a list or length of comp_x is not equal to total number of x-variables")
    }

    null.obj <- lapply(1 : K, function(k){
      if( length(setdiff( comp_x[[k]] , 1 : K )) > 0 )
      {
        stop("Values allowed in the comp_x are between 1 : total number of x-variables")
      }
      NULL 
    })
    rm(null.obj)

    null.obj <- lapply(1 : K, function(k){
      if( !any( comp_x[[k]] == k)  )
      {
          print("The kth element for comp_x does not include index k. Are you sure about this?")
      }
    })

    comp_x <- lapply(1 : K, function(k){
      comp_x[[k]] - 1
    })
  }
  





  if(is.null(comp_y))
  {
    comp_y <- lapply(1 : L, function(l){
      (l - 1)
    })
  }else
  {
    if( !is.list(comp_y) ||  length(comp_y) != L)
    {
      stop("Either comp_y is not a list or length of comp_y is not equal to total number of y-variables")
    }

    null.obj <- lapply(1 : L, function(l){
      if( length(setdiff( comp_y[[l]] , 1 : L )) > 0 )
      {
        stop("Values allowed in the comp_y are between 1 : total number of y-variables")
      }
      NULL 
    })
    rm(null.obj)

    null.obj <- lapply(1 : L, function(l){
      if( !any( comp_y[[l]] == l)  )
      {
          print("The lth element for comp_y does not include index l. Are you sure about this?")
      }
    })

    comp_y <- lapply(1 : L, function(l){
      comp_y[[l]] - 1
    })
  }

  if(M <= 10)
  {
    verbose <- FALSE
  }

  obj_C  <- BoostMLR_C(Org_x,
                       Org_y,
                       id,
                       tm,
                       x,
                       y,
                       id_x,
                       tm_x,
                       x_Mean,
                       x_Std_Error,
                       y_Mean,
                       y_Std_Error,
                       y_scale_summary,
                       intercept,
                       time_map,
                       n,
                       K,
                       L,
                       H,
                       G,
                       Dk,
                       ni,
                       ni_x,
                       N,
                       N_x,
                       unq_id,
                       unq_tm,
                       unq_tm_x,
                       unq_x,
                       id_index,
                       id_index_x,
                       Bt,
                       Bt_x,
                       Bx,
                       Bx_Scale,
                       Bx_Scale_const,
                       Time_Add_New,
                       Time_Unmatch,
                       nu,
                       M,
                       mod_grad,
                       use_raw,
                       Lambda_Ridge_Vec,
                       ridge_penalty,
                       shrink,
                       lower_perc,
                       upper_perc,
                       lambda_scale,
                       n_lambda,
                       var_flag,
                       rho,
                       phi,
                       setting_seed,
                       seed_value,
                       sq_residual_select,
                       update_all_comp,
                       comp_x,
                       comp_y,
                       verbose,
                       trace)
  
  
  if(trace)
  {
    print("Estimation begins for Tm_Beta")
  }


  Tm_Beta <- lapply(1:obj_C$Dimensions$L,function(l){
    Out <- lapply(1:obj_C$Dimensions$K,function(k){
      if(!use_raw[k])
      {
        temp_obj <- rep(NA, obj_C$Dimensions$N)
      }else
      {
        temp_obj <- lapply(1 : obj_C$Dimensions$n, function(i){
          matrix(0,nrow = obj_C$Dimensions$ni[i],ncol = obj_C$Dimensions$ni_x[i])
        })
        
        null.obj <- lapply(1:obj_C$Dimensions$H,function(h){
          null.obj.1 <- lapply(1 : obj_C$Dimensions$G, function(g){
            obj_n <- lapply(1:obj_C$Dimensions$n,function(i){
              obj_C$Beta_Estimate$Tm_Beta_C[[k]][[1]][[h]][[g]][[l]][[i]]
            })
            temp_obj <<- Map("+",temp_obj,obj_n)
            NULL
          })
          NULL
        })
      }
      temp_obj
    })
    names(Out) <- x_names
    Out
  })



  if(trace)
  {
    print("Estimation ends for Tm_Beta")
  }

  #---------------------------------------------------------------------------------- 
  # Date: 12/11/2020
  
  # It was realized that it makes more sense to show plots of beta on the standardized
  # scale rather than on the original scale. Therefore, along with Tm_Beta, I
  # have calculated Tm_Beta_Std in the following codes.
  #----------------------------------------------------------------------------------

  Tm_Beta_Std <- lapply(1:obj_C$Dimensions$L,function(l){
    Out <- lapply(1:obj_C$Dimensions$K,function(k){
      if(!use_raw[k])
      {
        temp_obj <- rep(NA, obj_C$Dimensions$N)
      }else
      {
        temp_obj <- lapply(1 : obj_C$Dimensions$n, function(i){
          matrix(0,nrow = obj_C$Dimensions$ni[i],ncol = obj_C$Dimensions$ni_x[i])
        })
        
        null.obj <- lapply(1:obj_C$Dimensions$H,function(h){
          null.obj.1 <- lapply(1 : obj_C$Dimensions$G, function(g){
            obj_n <- lapply(1:obj_C$Dimensions$n,function(i){
              obj_C$Beta_Estimate$Tm_Beta_Std_C[[k]][[1]][[h]][[g]][[l]][[i]]
            })
            temp_obj <<- Map("+",temp_obj,obj_n)
            NULL
          })
          NULL
        })
      }
      temp_obj
    })
    names(Out) <- x_names
    Out
  })

  if(FALSE)
  {
    if(tm_varying == FALSE)
    {
      Tm_Beta <- lapply(1:obj_C$Dimensions$L,function(l){
        Tm_Beta[[l]][1,,drop = TRUE]
      })
    }
  }
  
  names(Tm_Beta) <- y_names

  #---------------------------------------------------------------------------------- 
  # Date: 12/11/2020
  
  # Added Tm_Beta_Std as a part of Beta_Estimate
  #----------------------------------------------------------------------------------

  if(FALSE)
  {
    if(tm_varying == FALSE)
    {
    Tm_Beta_Std <- lapply(1:obj_C$Dimensions$L,function(l){
      Tm_Beta_Std[[l]][1,,drop = TRUE]
    })
    }
  }


  names(Tm_Beta_Std) <- y_names
  
  Beta_Estimate <- obj_C$Beta_Estimate
  Beta_Estimate$Tm_Beta <- Tm_Beta


  #---------------------------------------------------------------------------------- 
  # Date: 12/11/2020
  
  # Added Tm_Beta_Std as a part of Beta_Estimate
  #----------------------------------------------------------------------------------
  

  Rho <- Phi <- matrix(NA,nrow = M,ncol = L)
  colnames(Phi) <- y_names
  colnames(Rho) <- y_names
  Error_Rate <- obj_C$Error_Rate
  colnames(Error_Rate) <- y_names
   
  
  x       <- obj_C$Data$Org_x
  y       <- obj_C$Data$Org_y
  id      <- obj_C$Data$id
  tm      <- obj_C$Data$tm
  M       <- obj_C$Regulate$M
  nu      <- obj_C$Regulate$nu
  mu      <- obj_C$mu_List[[M]] 
  N       <- obj_C$Dimensions$N

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
              mu_temp_temp <- mu[ , which_index[jj]  ]
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
  
  if(var_flag)
  {
    phi <- obj_C$Phi[M,]
    rho <- obj_C$Rho[M,]
  }
  
  Grow_Object <- list(Data = obj_C$Data,
                      Dimensions = obj_C$Dimensions,
                      Index = obj_C$Index,
                      BS = obj_C$BS,
                      Regulate = obj_C$Regulate,
                      Beta_Estimate = Beta_Estimate,
                      mu = obj_C$mu,
                      mu_List = obj_C$mu_List,
                      mu_zero = obj_C$mu_zero,
                      Vec_zero = obj_C$Vec_zero,
                      mod_grad = mod_grad,
                      sort_id = sort_id,
                      phi = phi,
                      rho = rho,
                      Time_Unmatch = Time_Unmatch,
                      setting_seed = setting_seed,
                      seed_value = seed_value,
                      Time_Add_New = if(is.null(dt_Add)) NULL else Time_Add_New)

  Variable_Select <- lapply(1 : M, function(m){
  obj_C$Variable_Select[[m]] + 1
  })

  Response_Select <- lapply(1 : M, function(m){
  obj_C$Response_Select[[m]] + 1
  })

  Phi <- obj_C$Phi
  Rho <- obj_C$Rho
  colnames(Phi) <- colnames(Rho) <- y_names
  
  obj <- list(x  = x,
              id = id,
              tm = tm,
              y  = y,
              id_x = id_x,
              tm_x = tm_x,
              y_scale = y_scale,
              intercept = intercept,
              y_scale_summary = y_scale_summary,
              y_scale_ordinal = y_scale_ordinal,
              use_raw = use_raw,
              async_x = async_x,
              tm_separate = tm_separate,
              x_names = x_names,
              y_names = y_names,
              M = M,
              nu = nu,
              Tm_Beta = Tm_Beta,
              Tm_Beta_Std = Tm_Beta_Std,
              mu = mu,
              prob_class = prob_class,
              Q_set_list = Q_set_list,
              Error_Rate = Error_Rate,
              Variable_Select = Variable_Select,
              Response_Select = Response_Select,
              var_flag = var_flag,
              tm_varying = tm_varying,
              Phi = Phi,
              Rho = Rho,
              comp_x = comp_x,
              comp_y = comp_y,
              update_all_comp = update_all_comp,
              #Lambda_List = obj_C$Lambda_List,
              Grow_Object = Grow_Object)
  
  class(obj) <- c("BoostMLR", "grow")
  invisible(obj)
}
