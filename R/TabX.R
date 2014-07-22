#' Cross Tabulation
#'
#' \code{XTab} is a modified version of \code{\link{CrossTable}} (\pkg{gmodels}). 
#'
#' @param x,y The variables for the cross tabulation. 
#' @param digits Number of digits for rounding proportions. 
#' @author Jonah Gabry <jsg2201@@columbia.edu> See \code{\link{CrossTable}} for the authors
#' of the function on which \code{XTab} is based. 
#' @seealso \code{\link{plot.RD}} 
#' @export
#' @examples
#' data("GSS_2010", package = "QMSS")
#' with(GSS_2010, TabX(sex, race))

TabX <- function(x, y, digits = 3, ...){
  
  dig <- digits
  xName <- deparse(substitute(x))
  yName <- deparse(substitute(y))
  x <- factor(x)
  y <- factor(y)
  tab <- table(x, y)
  dim1 <- dimnames(tab)[[1]]
  dim2 <- dimnames(tab)[[2]]
  
  # Calcs
  Cprop <- prop.table(tab, 2)
  Tprop <- prop.table(tab)
  Obs <- sum(tab)
  Rsum <- rowSums(tab)   
  Csum <- colSums(tab)
  
  # Formatting Setup
  Ctotal.lab <- "TOTAL"
  Rtotal.lab <- "TOTAL"
  maxC <- max(2 + dig, nchar("TOTAL"), nchar(dim2), nchar(tab), nchar(Rsum), nchar(Csum))
  maxR <- max(nchar(dim1), nchar("TOTAL"))
  Ctotal.lab <- formatC(Ctotal.lab, width = maxR, format = "s")
  Rtotal.lab <- formatC(Rtotal.lab, width = maxC, format = "s")
  Lines <- paste(rep("=", 2 + maxC), collapse = "")
  inner.Lines <- paste(rep("=", 1 + maxR), collapse = "")
  Rspaces <- paste(rep(" ", maxR), collapse = "")
  Cspaces <- paste(rep(" ", maxC), collapse = "")
  outer.Column <- formatC(dim1, width = maxR, format = "s")
  
  
  
  printTheTable <- function() {
    xyNames <- abbreviate(c(xName,yName), min = 5, dot = T)
    cat(rep(Rspaces,3), "[Y]", xyNames[2], collapse = "\n")
    cat("[X]", xyNames[1], collapse = "\n")
    cat(Rspaces, 
        formatC(dim2, width = maxC, format = "s"), 
        Rtotal.lab, 
        sep = " | ", 
        collapse = "\n"
    )
    cat(inner.Lines, 
        rep(Lines, ncol(tab) + 1), 
        sep = "|", 
        collapse = "\n"
    )
    for (i in 1:nrow(tab)) {
      cat(outer.Column[i], 
          formatC(c(tab[i, ], Rsum[i]), width = maxC, format = "d"), 
          sep = " | ", 
          collapse = "\n"
      )
      cat(Rspaces, 
          formatC(Cprop[i, ], width = maxC, digits = dig, format = "f"), 
          sep = " | ", 
          collapse = "\n"
      )
      cat(Rspaces, 
          formatC(c(Tprop[i, ],Rsum[i]/Obs), width = maxC, digits = dig, format = "f"), 
          #         Cspaces, 
          sep = " | ", 
          collapse = "\n"
      )
      cat(inner.Lines, 
          rep(Lines, ncol(tab) + 1), 
          sep = "|", 
          collapse = "\n"
      )
    }
    cat(Ctotal.lab, 
        formatC(c(Csum, Obs), width = maxC, format = "d"), 
        sep = " | ", 
        collapse = "\n"
    )
    cat(Rspaces, formatC(c(Csum/Obs, sum(Rsum/Obs)), width = maxC, digits = dig, format = "f"), 
        #       Cspaces, 
        sep = " | ", 
        collapse = "\n"
    )
    cat(Rspaces, 
        formatC(c(sum(Cprop[,1]),sum(Cprop[,2])), width = maxC, digits = dig, format = "f"), 
        Cspaces, 
        sep = " | ", 
        collapse = "\n"
    )
    cat(inner.Lines, 
        rep(Lines, ncol(tab) + 1), 
        sep = "|", 
        collapse = "\n"
    )
  }
  
  # Cell contents
  cat(rep("\n", 2))
  cat("   Cell Contents\n")
  cat("|=========================|\n")
  cat("|                   Count |\n")
  cat("|    Count / Column Total |\n")
  cat("|     Count / Table Total |\n")
  cat("|=========================|\n")
  cat("\n")
  cat("Total Observations in Table: ", Obs, "\n")
  cat("\n")
  cat("X = ", xName, "\n")
  cat("Y = ", yName, "\n")
  cat(rep("\n", 2))
  
  printTheTable()
}
