plot.lpca <- function(x, type = c("trace", "loadings", "scores"), ...) {
  type = match.arg(type)
  
  if (type == "trace") {
    df = data.frame(Iteration = 0:x$iters,
                    NegativeLogLikelihood = x$loss_trace)
    p <- ggplot2::ggplot(df, ggplot2::aes_string("Iteration", "NegativeLogLikelihood")) +
      ggplot2::geom_line()
  } else if (type == "loadings") {
    df = data.frame(x$U)
    colnames(df) <- paste0("PC", 1:ncol(df))
    if (ncol(df) == 1) {
      df$PC2 = 0
      p <- ggplot2::ggplot(df, ggplot2::aes_string("PC1", "PC2")) + ggplot2::geom_point() + 
        ggplot2::labs(y = NULL)
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes_string("PC1", "PC2")) + ggplot2::geom_point()
    }
  } else if (type == "scores") {
    df = data.frame(x$PCs)
    colnames(df) <- paste0("PC", 1:ncol(df))
    if (ncol(df) == 1) {
      df$PC2 = 0
      p <- ggplot2::ggplot(df, ggplot2::aes_string("PC1", "PC2")) + ggplot2::geom_point() + 
        ggplot2::labs(y = NULL)
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes_string("PC1", "PC2")) + ggplot2::geom_point()
    }
  }
  
  return(p)
}

cv.lpca <- function(x, ks, ms = seq(2, 10, by = 2), folds = 5, quiet = TRUE, Ms, ...) {
  if (!missing(Ms)) {
    ms = Ms
    warning("Ms is depricated. Use ms instead.\n", 
            "Using ms in ", paste(ms, collapse = ","))
  }
  q = 2 * as.matrix(x) - 1
  q[is.na(q)] <- 0
  
  if (length(folds) > 1) {
    # does this work if factor?
    if (length(unique(folds)) <= 1) {
      stop("If inputing CV split, must be more than one level")
    }
    if (length(folds) != nrow(x)) {
      stop("if folds is a vector, it should be of same length as nrow(x)")
    }
    cv = folds
  } else {
    cv = sample(1:folds, nrow(q), replace = TRUE)
  }
  
  log_likes = matrix(0, length(ks), length(ms),
                     dimnames = list(k = ks, m = ms))
  for (k in ks) {
    for (m in ms) {
      if (!quiet) {
        cat("k =", k, "m =", m, "")
      }
      for (c in unique(cv)) {
        if (!quiet) {
          cat(".")
        }
        lpca = logisticPCA(x[c != cv, ], k = k, m = m, ...)
        pred_theta = predict(lpca, newdat = x[c == cv, ], type = "link")
        log_likes[k == ks, m == ms] = log_likes[k == ks, m == ms] +
          log_like_Bernoulli(q = q[c == cv, ], theta = pred_theta)
        #         log_likes[k == ks, m == ms] = log_likes[k == ks, m == ms] +
        #           sum(log(inv.logit.mat(q[c == cv, ] * pred_theta)))
      }
      if (!quiet) {
        cat("", -log_likes[k == ks, m == ms], "\n")
      }
    }
  }
  class(log_likes) <- c("matrix", "cv.lpca")
  which_min = which(log_likes == max(log_likes), arr.ind = TRUE)
  if (!quiet) {
    cat("Best: k =", ks[which_min[1]], "m =", ms[which_min[2]], "\n")
  }
  
  return(-log_likes)
}

plot.cv.lpca <- function(x, ...) {
  # replaces reshape2::melt(-x, value.name = "NegLogLikelihood")
  ms = type.convert(colnames(x))
  ks = type.convert(rownames(x))
  df = data.frame(k = rep(ks, times = length(ms)),
                  m = rep(ms, each = length(ks)),
                  NegLogLikelihood = as.vector(x))
  
  if (ncol(x) == 1) {
    df$m = factor(df$m)
    p <- ggplot2::ggplot(df, ggplot2::aes_string("k", "NegLogLikelihood", colour = "m")) +
      ggplot2::geom_line()
  } else {
    df$k = factor(df$k)
    p <- ggplot2::ggplot(df, ggplot2::aes_string("m", "NegLogLikelihood", colour = "k")) +
      ggplot2::geom_line()
  }
  return(p)
}

plot.lsvd <- function(x, type = c("trace", "loadings", "scores"), ...) {
  type = match.arg(type)
  
  if (type == "trace") {
    df = data.frame(Iteration = 0:x$iters,
                    NegativeLogLikelihood = x$loss_trace)
    p <- ggplot2::ggplot(df, ggplot2::aes_string("Iteration", "NegativeLogLikelihood")) + 
      ggplot2::geom_line()
  } else if (type == "loadings") {
    df = data.frame(x$B)
    colnames(df) <- paste0("PC", 1:ncol(df))
    if (ncol(df) == 1) {
      df$PC2 = 0
      p <- ggplot2::ggplot(df, ggplot2::aes_string("PC1", "PC2")) + ggplot2::geom_point() + 
        ggplot2::labs(y = NULL)
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes_string("PC1", "PC2")) + ggplot2::geom_point()
    }
  } else if (type == "scores") {
    df = data.frame(x$A)
    colnames(df) <- paste0("PC", 1:ncol(df))
    if (ncol(df) == 1) {
      df$PC2 = 0
      p <- ggplot2::ggplot(df, ggplot2::aes_string("PC1", "PC2")) + ggplot2::geom_point() + 
        ggplot2::labs(y = NULL)
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes_string("PC1", "PC2")) + ggplot2::geom_point()
    }
  }
  
  return(p)
}
