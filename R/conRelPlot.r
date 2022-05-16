#' Plotting conditional reliability
#'
#' This function takes a fitted mirt-model and visualizes a conditional reliability curve. Heavily inspired by code from Phil Chalmers (author or 'mirt')
#' 
#' 
#' @param model an object of class `SingleGroupClass` returned by the function `mirt()`. 
#' @param theta_range range to be shown on the x-axis
#' @param color color of the line
#' @param title title for the plot (defaults to "Conditional Reliability")
#'
#' @return a ggplot
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import mirt
#' @export
#'
#' @examples
#' library(mirt)
#' library(ggmirt)
#' data <- expand.table(LSAT7)
#' (mod <- mirt(data, 1))
#' 
#' conRelPlot(mod)
conRelPlot <- function(model, 
                       theta_range = c(-4,4), 
                       color = "red",
                       title = "Conditional Reliability") {
  
  # helper functions
  
  computeItemtrace <- function(pars, Theta, itemloc, offterm = matrix(0L, 1L, length(itemloc)-1L),
                               CUSTOM.IND, pis = NULL){
    if(is.null(pis)){
      itemtrace <- .Call('computeItemTrace', pars, Theta, itemloc, offterm)
      if(length(CUSTOM.IND)){
        for(i in CUSTOM.IND)
          itemtrace[,itemloc[i]:(itemloc[i+1L] - 1L)] <- ProbTrace(pars[[i]], Theta=Theta)
      }
    } else {
      tmp_itemtrace <- vector('list', length(pis))
      for(g in seq_len(length(pis))){
        tmp_itemtrace[[g]] <- .Call('computeItemTrace', pars[[g]]@ParObjects$pars, Theta, itemloc, offterm)
        if(length(CUSTOM.IND)){
          for(i in CUSTOM.IND)
            tmp_itemtrace[[g]][,itemloc[i]:(itemloc[i+1L] - 1L)] <- ProbTrace(pars[[g]]@ParObjects$pars[[i]], Theta=Theta)
        }
      }
      itemtrace <- do.call(rbind, tmp_itemtrace)
    }
    return(itemtrace)
  }
  
  ExtractGroupPars <- function(x){
    if(x@itemclass < 0L) return(list(gmeans=0, gcov=matrix(1)))
    nfact <- x@nfact
    gmeans <- x@par[seq_len(nfact)]
    phi_matches <- grepl("PHI", x@parnames)
    if (x@dentype == "Davidian") {
      phi <- x@par[phi_matches]
      tmp <- x@par[-c(seq_len(nfact), which(phi_matches))]
      gcov <- matrix(0, nfact, nfact)
      gcov[lower.tri(gcov, diag=TRUE)] <- tmp
      gcov <- makeSymMat(gcov)
      return(list(gmeans=gmeans, gcov=gcov, phi=phi))
    } else {
      par <- x@par
      if(x@dentype == "mixture") par <- par[-length(par)] # drop pi
      tmp <- par[-seq_len(nfact)]
      gcov <- matrix(0, nfact, nfact)
      gcov[lower.tri(gcov, diag=TRUE)] <- tmp
      gcov <- makeSymMat(gcov)
      return(list(gmeans=gmeans, gcov=gcov))
    }
  }
  
  makeSymMat <- function(mat){
    if(ncol(mat) > 1L){
      mat[is.na(mat)] <- 0
      mat <- mat + t(mat) - diag(diag(mat))
    }
    mat
  }
  
  # Actual computation
  nfact <- model@Model$nfact
  J <- model@Data$nitems
  theta <- seq(theta_range[1],theta_range[2], by = .01)
  ThetaFull <- Theta <- thetaComb(theta, nfact)
  info <- testinfo(model, ThetaFull)
  itemtrace <- computeItemtrace(model@ParObjects$pars, ThetaFull, model@Model$itemloc,
                                CUSTOM.IND=model@Internals$CUSTOM.IND)
  mins <- model@Data$mins
  maxs <- extract.mirt(model, 'K') + mins - 1
  gp <- ExtractGroupPars(model@ParObjects$pars[[J+1]])
  score <- c()
  for(i in 1:J)
    score <- c(score, (0:(model@Data$K[i]-1) + mins[i]) * (i %in% c(1:J)))
  score <- matrix(score, nrow(itemtrace), ncol(itemtrace), byrow = TRUE)
  plt <- data.frame(cbind(info,score=rowSums(score*itemtrace),Theta=Theta))
  colnames(plt) <- c("info", "score", "Theta")
  plt$SE <- 1 / sqrt(plt$info)
  plt$rxx <- plt$info / (plt$info + 1/gp$gcov[1L,1L])

  ggplot(plt, aes(x = Theta, y = rxx)) + 
    geom_line(color = color) +
    ylim(0, 1) +
    theme_minimal() +
    labs(title = title, x = expression(theta), y = expression(r[xx](theta)))

}  
