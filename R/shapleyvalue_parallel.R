#' Shapley Value Regression (parallel)
#'
#' @param y is the data set of dependent variable
#' @param x is the data set of independent variables
#'
#' @export
shapleyvalue_parallel <- function(y,x){
  k <- ncol(x)
  times_wt <- NULL
  for (i in 1:k-1){
    times_wt[i] <- choose(k-1,i)
  }
  data <- data.frame(matrix(ncol=k,nrow=2^(k-1)))
  name <- colnames(x)
  
  if (k==1){
    stop(simpleError("No need for using shapley regression!"))
  }
  else if (k==2){
    for (i in 1:k){
      n <- name[i]
      lm <- lm(y~x[,n])
      lm2 <- lm(y~x[,-i])
      lm_all <- lm(y~.,data=x)
      r1 <- summary(lm)$r.squared
      r2 <- summary(lm2)$r.squared
      r3 <- summary(lm_all)$r.squared
      data[1,i] <- r1
      data[2,i] <- r3-r2
    }
    weight <- c(1/2,1/2)
    data <- as.matrix(data)
    dat <- weight%*%data
    dat <- as.data.frame(dat)
    sum <- rowSums(dat)
    uni_value <- dat/sum
    dat <- rbind(round(dat,4),round(uni_value,4))
    colnames(dat) <- name
    rownames(dat) <- c("Shapley Value","Standardized Shapley Value")
    return(dat)
  }
  else{
    for (i in 1:k){
      num <- 1
      for (j in 1:k){
        if(j==1){
          n <- name[i]
          lm <- lm(y~x[,n])
          data[1,i] <- summary(lm)$r.squared
        }
        else if(j==k){
          xx <- combn(k, k)
          xlab <- NULL
          for(l in 1:j){
            xlab[l] <- xx[l]
            if(xlab[l]==i){
              x1 <- x[,xlab[l]]
              x_reslab <- xx[-l]
              x_res <- x[x_reslab]
              
              x_res <- cbind(x_res,x1)
              lm <- lm(y~.-x1,data=x_res)
              lm2 <- lm(y~.,data=x_res)
              
              r <- summary(lm)$r.squared
              r2 <- summary(lm2)$r.squared
            }
          }
          data[2^(k-1),i] <- r2-r
        }
        else{
          times <- choose(k-1,j-1)
          times2 <- choose(k,j)
          combine <- combn(k,j)
          n <- NULL
          nn <- combine
          for(m in 1:j){
            nn[m,] <- combine[m,]==i
          }
          for(m in 1:times2){
            n[m] <- 1 %in% nn[,m]
          }
          xx <- combine[,which(n)]
          out <- furrr::future_map_dbl(seq_len(times), \(o) {
            xlab <- NULL
            for(l in 1:j){
              xlab[l] <- xx[,o][l]
              if(xlab[l]==i){
                x1 <- x[,xlab[l]]
                x_reslab <- xx[,o][-l]
                x_res <- x[x_reslab]
                
                x_res <- cbind(x_res,x1)
                lm <- lm(y~.-x1,data=x_res)
                lm2 <- lm(y~.,data=x_res)
                
                #lm <- lm(y~.,data=x_res)
                r <- summary(lm)$r.squared
                #lm2 <- lm(y~.+x1,data=x_res)
                r2 <- summary(lm2)$r.squared
              }
            }
            r2 - r
          })
          idx_lo <- sum(choose(k - 1, seq_len(j - 1) - 1)) + 1
          idx_hi <- idx_lo + times - 1
          data[[i]][idx_lo:idx_hi] <- out
        }
      }
    }
    weight <- 1/k
    for (q in 1:k-1){
      weight <- c(weight,rep(1/k/times_wt[q],times_wt[q]))
    }
    data <- as.matrix(data)
    weight <- as.matrix(weight)
    weight <- t(weight)
    dat2 <- weight%*%data
    dat2 <- as.data.frame(dat2)
    sum <- rowSums(dat2)
    uni_value <- dat2/sum
    dat2 <- rbind(round(dat2,4),round(uni_value,4))
    colnames(dat2) <- name
    rownames(dat2) <- c("Shapley Value","Standardized Shapley Value")
    return(dat2)
  }
}
