#-------------------------------------------------------------------------------------------
# Partial plot function





#' Partial plot analysis
#' 
#' Partial dependence plot of x and time against adjusted predicted y.
#' 
#' Partial dependence plot (Friedman, 2001) of x values specified by
#' \code{xvar.name} against the adjusted predicted y-values over a set of time
#' points specified by \code{tm.unq}.
#' 
#' @param Object A boosting object of class \code{(BoostMLR, grow)}.
#' @param xvar.name Name of the x-variable to be used for partial plot.
#' @param n.x Maximum number of unique points used for \code{xvar.name}. Reduce
#' this value if plotting is slow.
#' @param n.tm Maximum number of unique points used for \code{tm}. Reduce this
#' value if plotting is slow.
#' @param n.tm_x Maximum number of unique points used for \code{tm_x}. Reduce
#' this value if plotting is slow.
#' @param x.unq Unique values used for the partial plot for variable
#' \code{xvar.name}. Default is NULL in which case unique values are obtained
#' uniformaly based on the range of variable.
#' @param tm.unq Unique time points used for the partial plots of x against y.
#' Default is NULL in which case unique values are obtained uniformaly based on
#' the range of \code{tm}.
#' @param tm_x.unq Unique time points used for the partial plots of x against
#' y. Default is NULL in which case unique values are obtained uniformaly based
#' on the range of \code{tm_x}.
#' @param extract_prob_class In case of ordinal responses, extract
#' probabilities of each class rather than cumulative probabilities.
#' @param Mopt The optimal number of boosting iteration. If missing, the value
#' from the \code{Object} will be used.
#' @param plot.it Should partial plot be displayed?
#' @param path_saveplot Provide the location where plot should be saved. By
#' default the plot will be saved at temporary folder.
#' @param Verbose Display the path where the plot is saved?
#' @param ... Further arguments passed to or from other methods.
#' @return \item{x.unq}{Unique values used for the partial plot for variable
#' \code{xvar.name}} \item{tm.unq}{Unique time points from \code{tm} used for
#' the partial plots of x against y.} \item{tm_x.unq}{Unique time points from
#' \code{tm_x} used for the partial plots of x against y.} \item{pList}{List
#' with number of elements equal to number of multivariate response. Each
#' element of the list is a list with number of elements equal to the number of
#' unique x-values. Each element of this list is a matrix with number of rows
#' equal to length of \code{tm.unq}, and number of columns equal to length of
#' \code{tm_x.unq}. Values in the matrix represent predicted partial values.}
#' %\item{sList}{List with number of elements equal to number of multivariate
#' response. %Each element is a matrix with the same dimension as described in
#' \code{pList}. %Values are calculated using the local smoother (loess) for
#' \code{tm.unq} and the %i'th row %of the matrix from \code{pList}. Users are
#' encouraged to use \code{pList} to %genenrate their own \code{sList} so that
#' they will have more control over the %different arguments of local
#' smoother.}
#' @author Amol Pande and Hemant Ishwaran
#' @references Friedman J.H. Greedy function approximation: a gradient boosting
#' machine, \emph{Ann. of Statist.}, 5:1189-1232, 2001.
#' @keywords plot
#' @examples
#' 
#' \donttest{
#' ##------------------------------------------------------------
#' ## Generate partial plot for covariate x1
#' ##-------------------------------------------------------------
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
#' Partial_Plot_x1 <- partial.BoostMLR(Object = boost.grow, 
#'                                      xvar.name = "x1",
#'                                      plot.it = FALSE)
#' 
#' }
#' 
partial.BoostMLR <- function(Object,
                             xvar.name,
                             n.x = 10,
                             n.tm = 10,
                             n.tm_x = 10,
                             x.unq = NULL,
                             tm.unq = NULL,
                             tm_x.unq = NULL,
                             extract_prob_class = FALSE,
                             Mopt,
                             plot.it = TRUE,
                             path_saveplot = NULL,
                             Verbose = TRUE,
                             ...)
{
  
  if (missing(Object)) 
  {
    stop("Object is missing")
  }
  
  x  <- Object$x
  tm <- Object$tm
  id <- Object$id
  tm_x <- Object$tm_x
  id_x <- Object$id_x
  async_x <- Object$async_x
  N <- Object$Grow_Object$Dimensions$N
  N_x <- Object$Grow_Object$Dimensions$N_x
  time_map <- Object$Grow_Object$BS$time_map

  if (missing(xvar.name)) 
  {
    stop("xvar.name is missing" )
  }
  
  if(length(xvar.name) > 1)
  {
    stop("Use single covariate in xvar.name")
  }
  
  x_names <- Object$x_names
  xvar.name <- intersect(xvar.name, x_names)
  if (length(xvar.name) == 0) 
  {
    stop("xvar.name do not match original variable names")
  }
  
  user.option <- list(...)
  prob_min <- is.hidden.prob_min(user.option)
  prob_max <- is.hidden.prob_max(user.option)
  
  if( is.null(x.unq) )
  {
    xvar <- x[,xvar.name,drop = TRUE]
    n.x.unq <- length(unique(xvar))
    if(n.x.unq <= n.x)
    {
      x.unq <- sort(unique(xvar))
    } else 
    {
      x.unq <- sort(unique(xvar))[unique(as.integer(quantile(1: n.x.unq ,probs = seq(prob_min,prob_max,length.out = min(n.x,n.x.unq)   ))))]  
    }
  }
  n.x <- length(x.unq)

  if( is.null(tm.unq) )
  {
    n.tm.unq <- length(unique(tm))
    if(n.tm.unq <= n.tm)
    {
      tm.unq <- sort(unique(tm))
    } else 
    {
      tm.unq <- sort(unique(tm))[unique(as.integer(quantile(1: length(unique(tm)) ,probs = seq(0,0.9,length.out = min(n.tm,n.tm.unq) ))))]
    }
  }
  n.tm <- length(tm.unq)
  
  if( is.null(tm_x.unq) )
  {
    n.tm_x.unq <- length(unique(tm_x))
    if(n.tm_x.unq <= n.tm_x)
    {
      tm_x.unq <- sort(unique(tm_x))
    } else 
    {
      tm_x.unq <- sort(unique(tm_x))[unique(as.integer(quantile(1: length(unique(tm_x)) ,probs = seq(0,0.9,length.out = min(n.tm_x,n.tm_x.unq) ))))]
    }
  }
  n.tm_x <- length(tm_x.unq)


  if(missing(Mopt))
  {
    Mopt <- Object$M
  }
  
  L <- Object$Grow_Object$Dimensions$L
  
  if(extract_prob_class)
  {
    if( !any(Object$y_scale == "ordinal") )
    {
      stop("prob_class is applied if one of the responses is ordinal")
    }else
    {
      lth_prob_class  <- length(Object$prob_class)
      p.obj <- lapply(1 : lth_prob_class, function(k)
      {
        lapply(1 : n.x, function(i)
        {
          new_x <- x
          new_x[,xvar.name] <- x.unq[i]
          lapply(1:n.tm,function(j)
          {
            lapply(1 : n.tm_x,function(jj)
            {
              tm <- rep(tm.unq[j],N)
              if(async_x)
              {
                tm_x <- rep(tm_x.unq[jj],N_x)
                colMeans(predictBoostMLR(Object = Object,x = new_x,tm = tm,id = id,id_x = id_x, tm_x = tm_x,M = Mopt,importance = FALSE,time_map = time_map)$prob_class[[k]],na.rm = TRUE) 
              }else
              {
                colMeans(predictBoostMLR(Object = Object,x = new_x,tm = tm,id = id,M = Mopt,importance = FALSE,time_map = time_map)$prob_class[[k]],na.rm = TRUE) 
              }
            })
          }) 
        }) 
      })

      pList <- lapply(1 : lth_prob_class, function(k)
      {
        L_prob_class <- ncol(Object$prob_class[[k]])
        lapply(1 : L_prob_class, function(l)
        {
          lapply(1:n.x, function(i)
          {
            pMat <- matrix(NA,nrow = n.tm,ncol = n.tm_x)
            for(j in 1:n.tm)
            {
              for(jj in 1 : n.tm_x)
              {
                pMat[j,jj] <- p.obj[[k]][[i]][[j]][[jj]][l]
              }            
            }
            pMat
          })          
        })
      })
    }
  }else
  {
    p.obj <- lapply(1:n.x,function(i)
    {
      new_x <- x
      new_x[,xvar.name] <- x.unq[i]
      lapply(1:n.tm,function(j)
      {
        lapply(1 : n.tm_x, function(jj)
        {
          tm <- rep(tm.unq[j],N)
          if(async_x)
          {
            tm_x <- rep(tm_x.unq[jj],N_x)
            colMeans(predictBoostMLR(Object = Object,x = new_x,tm = tm,id = id,id_x = id_x, tm_x = tm_x,M = Mopt,importance = FALSE,time_map = time_map)$mu,na.rm = TRUE) 
          }else
          {
            colMeans(predictBoostMLR(Object = Object,x = new_x,tm = tm,id = id,M = Mopt,importance = FALSE,time_map = time_map)$mu,na.rm = TRUE)
          }
        })
      })    
    })
    

    pList <- lapply(1 : L, function(l)
    {
      lapply(1 : n.x, function(i)
      {
        pMat <- matrix(NA,nrow = n.tm,ncol = n.tm_x)
        for(j in 1 : n.tm)
        {
          for(jj in 1 : n.tm_x)
          {
            pMat[j,jj] <- p.obj[[i]][[j]][[jj]][l]
          }
        }
        pMat
      })
    })
  }
  
  # Code to create partial plot is pending; do it later
  plot.it <- FALSE

  if(plot.it)
  {

    if(is.null(path_saveplot))
    {
        path_saveplot <- tempdir()
    }
    pdf(file = paste(path_saveplot,"/","PartialPlot.pdf",sep=""),width = 14,height = 14)
    
    if(extract_prob_class)
    {
      for(k in 1 : lth_prob_class)
      {
        L_prob_class <- ncol(Object$prob_class[[k]])
        for(l in 1 : L_prob_class)
        {
          filled.contour(x = tm.unq,
                y = x.unq,
                z = t(pList[[k]][[l]]),
                xlim = range(tm.unq, finite = TRUE) + c(-0, 0),
                ylim = range(x.unq, finite = TRUE) + c(-0, 0),
                color.palette =
                  colorRampPalette(c("yellow", "red")),
                xlab = "Time",
                ylab = "x",
                main = "PartialPlot",
                cex.main = 2,
                cex.lab = 1.5,
                plot.axes = {
                axis(1,cex.axis = 1.5)
                axis(2,cex.axis = 1.5)})
        }
      }
    }else
    {
    for(l in 1:L)
    {
      filled.contour(x = tm.unq,
                    y = x.unq,
                    z = t(pList[[l]]),
                    xlim = range(tm.unq, finite = TRUE) + c(-0, 0),
                    ylim = range(x.unq, finite = TRUE) + c(-0, 0),
                    color.palette =
                      colorRampPalette(c("yellow", "red")),
                    xlab = "Time",
                    ylab = "x",
                    main = "PartialPlot",
                    cex.main = 2,
                    cex.lab = 1.5,
                    plot.axes = {
                    axis(1,cex.axis = 1.5)
                    axis(2,cex.axis = 1.5)})
      }
    }

    dev.off()
    if(Verbose)
    {
    cat("Plot will be saved at:",path_saveplot,sep = "")
    }
  }
  obj <- list(x.unq = x.unq,
              tm.unq = tm.unq,
              tm_x.unq = tm_x.unq,
              pList = pList)
  
  invisible(obj)
}
