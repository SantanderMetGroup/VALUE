#' @title Filtered annual cycle maxima statistics
#' @description Returns the amplitude or the location of the maxima of the annual cycle
#' @author D. Maraun, J. Bedia, D. San-Martín, J.M. Gutiérrez
#' @template templateIndexParams
#' @template templateDates
#' @param peak = 1 or 2 (maximum or secondary maximum - higher order peaks are ignored)
#' @param type A character string indicating max \code{"phase"} (days, default), \code{"amp"} (amplitude) or \code{"amprel"} (relative amplitude)
#' @param thresh Threshold that defines weak and double maxima (relative to amplitude of annual cycle)
#' @return A float number corresponding to the amplitude or the (phase) day of the maxima
#' @importFrom RcppEigen fastLm
#' @importFrom stats predict
#' @export
#' @keywords internal


index.annual.cycle.filtered <- function(ts, dates, type = 'phase', peak = 1, thresh = .1) {
      type <- match.arg(type, choices = c("phase", "amp", "amprel"))
      doy <- substr(dates, 6, 10) 
      anncyc <- tapply(ts, INDEX = doy, FUN = mean, na.rm = TRUE)
      T <- length(anncyc)
      t <- 1:T
      sh1 <- sin(t/T*2*pi)
      ch1 <- cos(t/T*2*pi)
      sh2 <- sin(t/T*4*pi)
      ch2 <- cos(t/T*4*pi)
      sh3 <- sin(t/T*6*pi)
      ch3 <- cos(t/T*6*pi)
      sh4 <- sin(t/T*8*pi)
      ch4 <- cos(t/T*8*pi)
      mod4 <- fastLm(anncyc~sh1+ch1+sh2+ch2+sh3+ch3+sh4+ch4)
      ac <- predict(mod4)
      n <- length(ac)
      ## to ease identification of maxima at beginning/end of year
      acdum <- c(ac[n],ac,ac[1])
      ## amplitude and relative amplitude
      amp <- max(ac) - min(ac)
      amprel <- (max(ac) - min(ac)) / mean(ac)
      ## position (day of year), type (-1: min, 1: max) and magnitude of maxima/minima
      mmp <- mmt <- mmv <- NA
      nm <- 0
      for (i in 1 + 1:n) {
            if (acdum[i - 1] < acdum[i] & acdum[i + 1] < acdum[i]) { 
                  nm <- nm + 1
                  mmp[nm] <- i - 1
                  mmt[nm] <- 1
                  mmv[nm] <- ac[mmp[nm]]
            }
            if (acdum[i - 1] > acdum[i] & acdum[i + 1] > acdum[i]) {
                  nm <- nm + 1
                  mmp[nm] <- i - 1
                  mmt[nm] <- -1
                  mmv[nm] <- ac[mmp[nm]]
            }
      }
      ## merge double peaks
      if (nm > 2) {
            if (mmt[2] == 1 & mmt[nm] == 1) {
                  if (abs(ac[mmp[2]] - ac[mmp[nm]]) < thresh*amp & abs(mean(c(ac[mmp[2]], ac[mmp[nm]])) - ac[mmp[1]]) < thresh*amp) {
                        mmt[1] <- 1
                        mmt[nm] <- 0
                        mmt[2] <- 0
                        mmv[nm] <- 0
                        mmv[2] <- 0
                        mmp[1] <- mean(c(mmp[nm] - 366, mmp[2]))
                        if (mmp[1] < 1) {
                              mmp[1] <- mmp[1] + 366
                        }
                        mmv[1] <- mean(c(ac[mmp[2]], ac[mmp[nm]]))
                  }
            }
            for (i in 2:(nm - 1)) {
                  if (mmt[i - 1] == 1 & mmt[i + 1] == 1) {
                        if (abs(ac[mmp[i + 1]] - ac[mmp[i - 1]]) < thresh*amp & abs(mean(c(ac[mmp[i + 1]], ac[mmp[i - 1]])) - ac[mmp[i]]) < thresh*amp) {
                              mmt[i] <- 1
                              mmt[i - 1] <- 0
                              mmt[i + 1] <- 0
                              mmv[i - 1] <- 0
                              mmv[i + 1] <- 0
                              mmp[i] <- mean(c(mmp[i + 1], mmp[i - 1]))
                              mmv[i] <- mean(c(ac[mmp[i + 1]], ac[mmp[i - 1]]))
                        }
                  }
            }
            if (mmt[1] == 1 & mmp[nm - 1] == 1) {
                  if (abs(ac[mmp[1]] - ac[mmp[nm - 1]]) < thresh*amp & abs(mean(c(ac[mmp[1]], ac[mmp[nm - 1]])) - ac[mmp[nm]]) < thresh*amp) {
                        mmt[nm] <- 1
                        mmt[nm - 1] <- 0
                        mmt[1] <- 0
                        mmv[nm - 1] <- 0
                        mmv[1] <- 0
                        mmp[nm] <- mean(c(mmp[nm - 1] - 366, mmp[1]))
                        if (mmp[nm] < 1) {
                              mmp[nm] <- mmp[nm] + 366
                        }
                        mmv[nm] <- mean(c(ac[mmp[1]], ac[mmp[nm - 1]]))
                  }
            }
      }
      ## erase deleted peaks
      itn0 <- which(mmt != 0)
      mmt <- mmt[itn0]
      mmv <- mmv[itn0]
      mmp <- mmp[itn0]
      nm <- length(mmt)
      ## delete weak peaks
      if (nm > 2) {
            izero <- NA
            nzero <- 0
            if (((abs(mmv[1] - mmv[nm]) < thresh*amp) || (abs(mmv[1] - mmv[2]) < thresh*amp)) & mmv[1] != max(mmv)) {
                  mmt[1] <- 0
                  nzero <- nzero + 1
                  izero[nzero] <- 1
            }
            for (i in 2:(nm - 1)) {
                  if (((abs(mmv[i] - mmv[i - 1]) < thresh*amp) || (abs(mmv[i] - mmv[i + 1]) < thresh*amp)) & mmv[i] != max(mmv)) {
                        mmt[i] <- 0
                        nzero <- nzero + 1
                        izero[nzero] <- i
                  }
            }
            if ((abs(mmv[nm] - mmv[1]) < thresh*amp) || (abs(mmv[nm] - mmv[nm - 1]) < thresh*amp) & mmv[nm] != max(mmv)) {
                  mmt[nm] <- 0
                  nzero <- nzero + 1
                  izero[nzero] <- nm
            }
            for (i in 1:nzero) {
                  mmv[izero[i]] <- 0
            }
      }
      if (nm > 2) {
            nmax <- sum(mmt == 1)
            if (nmax >= 2) {
                  max2 <- sort(mmv[mmt == 1])[nmax - 1]
                  pmax <- mmp[which(mmv >= max2 & mmt == 1)]
                  vmax <- mmv[which(mmv >= max2 & mmt == 1)]
                  if (vmax[2] > vmax[1])
                        pmax <- pmax[c(2,1)]
                  
            } else {
                  pmax <- mmp[which(mmv == max(mmv))]
            }
      } else {
            pmax <- mmp[which(mmv == max(mmv))]
      }
      if (type == 'phase') {
            if (peak == 1) {
                  return(pmax[1])
            } else {
                  if (length(pmax) < 2) {
                        return(NA)
                  } else {
                        return(pmax[2])
                  }
            }
      } else if (type == 'amp') {
            return(amp)
      } else if (type == 'amprel') {
            return(amprel)
      }
}

