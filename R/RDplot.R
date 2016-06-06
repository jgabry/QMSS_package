#' Plot regression discontinuity
#' 
#' \code{RDplot} adds a few additional features to \code{\link[rdd]{plot.RD}} 
#' from the \pkg{rdd} package. The additional arguments are
#' \code{col},\code{pts}, \code{xlab}, \code{ylab}, and \code{main}.
#' 
#' @export
#' @inheritParams rdd::plot.RD
#' @param col a vector of two colors to distinguish before and after the discontinuity
#' @param pts should points be plotted?
#' @param xlab label for the horizontal axis
#' @param ylab label for the vertical axis
#' @param main title for the plot
#' 
#' @seealso \code{\link[rdd]{plot.RD}} for the original function.
#' 
RDplot <- function (x,
                    gran = 400,
                    bins = 100,
                    which = 1,
                    range,
                    col = c("black", "black"),
                    pts = TRUE,
                    xlab = "",
                    ylab = "",
                    main = "Plot of Regression Discontinuity",
                    ...) {
  
  if (length(col) != 2)
    stop("'col' must contain two colors")
  labs <- c(xlab, ylab, main)
  frm <- FALSE
  if ("frame" %in% names(x$call))
    frm <- eval.parent(x$call$frame)
  if (!frm) {
    x$call$frame <- TRUE
    x$call$verbose <- FALSE
    x <- eval.parent(x$call)
  }
  d <- as.data.frame(x$frame)
  if (length(x$na.action) > 0)
    d <- d[-x$na.action,]
  if ("kernel" %in% names(x$call))
    kern <- eval.parent(x$call$kernel)
  else
    kern <- "triangular"
  if ("cutpoint" %in% names(x$call))
    cut <- eval.parent(x$call$cutpoint)
  else
    cut <- 0
  bw <- x$bw[1]
  if (missing(range)) {
    range <- c(cut - 10 * bw, cut + 10 * bw)
    if (range[1] < min(d$X))
      range[1] <- min(d$X)
    if (range[2] > max(d$X))
      range[2] <- max(d$X)
  }
  if (range[1] == "min")
    range[1] <- min(d$X)
  if (range[2] == "max")
    range[2] <- max(d$X)
  range <- as.double(range)
  rdplot <- function(d,
                     xlab = NA,
                     ylab = NA,
                     main = NA) {
    d.l <- data.frame(X = d$X[d$X < cut], Y = d$Y[d$X < cut])
    lval <- seq(range[1], cut, length.out = (gran %/% 2))
    lest <- vector(length = (gran %/% 2))
    llwr <- vector(length = (gran %/% 2))
    lupr <- vector(length = (gran %/% 2))
    for (i in 1:(gran %/% 2)) {
      sub <- d.l$X >= (lval[i] - bw) & d.l$X <= (lval[i] + bw)
      w <- kernelwts(
        X = d.l$X[sub],
        center = lval[i],
        bw = bw,
        kernel = kern
      )
      ly <- d.l$Y[sub]
      lx <- d.l$X[sub]
      if (length(lx) <= 2)
        pred <- rep(NA, 3)
      else
        pred <- predict(lm(ly ~ lx, weights = w),
                        interval = "confidence",
                        newdata = data.frame(lx = lval[i]))
      lest[i] <- pred[1]
      llwr[i] <- pred[2]
      lupr[i] <- pred[3]
    }
    d.r <- data.frame(X = d$X[d$X >= cut], Y = d$Y[d$X >= cut])
    rval <- seq(cut, range[2], length.out = (gran %/% 2))
    rest <- vector(length = (gran %/% 2))
    rlwr <- vector(length = (gran %/% 2))
    rupr <- vector(length = (gran %/% 2))
    for (i in 1:(gran %/% 2)) {
      sub <- d.r$X >= (rval[i] - bw) & d.r$X <= (rval[i] + bw)
      w <- kernelwts(
        X = d.r$X[sub],
        center = rval[i],
        bw = bw,
        kernel = kern
      )
      ry <- d.r$Y[sub]
      rx <- d.r$X[sub]
      if (length(rx) <= 2)
        pred <- rep(NA, 3)
      else
        pred <- predict(lm(ry ~ rx, weights = w),
                        interval = "confidence",
                        newdata = data.frame(rx = rval[i]))
      rest[i] <- pred[1]
      rlwr[i] <- pred[2]
      rupr[i] <- pred[3]
    }
    if (length(unique(d$Y)) == 2) {
      ep <- (max(d$X) - min(d$X)) / (2 * bins)
      nX <- seq(min(d$X) - ep, max(d$X) + ep, length = bins +
                  1)
      nY <- rep(NA, length(nX))
      for (i in (1:(length(nX) - 1))) {
        if (sum(!is.na(d$Y[d$X > nX[i] & d$X <= nX[i + 1]])) == 0)
          next
        nY[i] <-
          sum(d$Y[d$X > nX[i] &
                    d$X <= nX[i + 1]], na.rm = TRUE) / sum(!is.na(d$Y[d$X > nX[i] &
                                                                        d$X <= nX[i + 1]]))
      }
      sub <- nX >= range[1] & nX <= range[2]
      subl <- lval >= range[1] & lval <= range[2]
      subr <- rval >= range[1] & rval <= range[2]
      
      # added type = ifelse(pts == TRUE, "p", "n")
      plot(
        nX,
        nY,
        type = ifelse(isTRUE(pts), "p", "n"),
        pch = 20,
        cex = 0.5,
        col = "black",
        xlim = c(range[1], range[2]),
        ylim = c(min(c(llwr[subl], rlwr[subr]), na.rm = T),
                 max(c(lupr[subl], rupr[subr]), na.rm = T)),
        # added labs[]
        xlab = labs[1],
        ylab = labs[2],
        main = labs[3],
        ...
      )
    }
    else {
      subl <- lval >= range[1] & lval <= range[2]
      subr <- rval >= range[1] & rval <= range[2]
      
      # added type = ifelse(isTRUE(pts), "p", "n")
      plot(
        d$X,
        d$Y,
        type = ifelse(isTRUE(pts), "p", "n"),
        pch = 20,
        cex = 0.5,
        col = "black",
        xlim = c(range[1], range[2]),
        ylim = c(min(c(llwr[subl], rlwr[subr]), na.rm = T),
                 max(c(lupr[subl], rupr[subr]), na.rm = T)),
        # added labs[]
        xlab = labs[1],
        ylab = labs[2],
        main = labs[3],
        ...
      )
    }
    
    #added color options
    lines(
      lval,
      lest,
      lty = 1,
      lwd = 2,
      col = col[1],
      type = "l"
    )
    lines(
      lval,
      llwr,
      lty = 2,
      lwd = 1,
      col = "black",
      type = "l"
    )
    lines(
      lval,
      lupr,
      lty = 2,
      lwd = 1,
      col = "black",
      type = "l"
    )
    lines(
      rval,
      rest,
      lty = 1,
      lwd = 2,
      col = col[2],
      type = "l"
    )
    lines(
      rval,
      rlwr,
      lty = 2,
      lwd = 1,
      col = "black",
      type = "l"
    )
    lines(
      rval,
      rupr,
      lty = 2,
      lwd = 1,
      col = "black",
      type = "l"
    )
  }
  if (x$type == "sharp" | 1 %in% which) {
    rdplot(d)
    dev.flush()
  }
  if (x$type == "fuzzy" & 2 %in% which) {
    d$Y <- d$Z
    rdplot(d)
    dev.flush()
  }
}
