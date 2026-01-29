#' Summarize a \code{"prior.cifti"} object
#'
#' Summary method for class \code{"prior.cifti"}
#'
#' @param object Object of class \code{"prior.cifti"}.
#' @param ... further arguments passed to or from other methods.
#' @export
#' @return A list summarizing the prior: data dimensions, options used for
#'  prior estimation, etc.
#' @method summary prior.cifti
summary.prior.cifti <- function(object, ...) {
  tmean <- struct_prior(object$prior$mean, "CIFTI", object$mask_input,
    object$params, object$dat_struct, object$template_parc_table)
  tparams <- lapply(
    object$params,
    function(q) {
      if (is.null(q)) { q <- "NULL"};
      paste0(as.character(q), collapse=" ")
    }
  )
  x <- c(
    summary(tmean),
    list(has_DR="DR" %in% names(object)),
    tparams
  )

  class(x) <- "summary.prior.cifti"
  return(x)
}

#' Summarize a \code{"prior.gifti"} object
#'
#' Summary method for class \code{"prior.gifti"}
#'
#' @param object Object of class \code{"prior.gifti"}.
#' @param ... further arguments passed to or from other methods.
#' @export
#' @return A list summarizing the prior: data dimensions, options used for
#'  prior estimation, etc.
#' @method summary prior.gifti
summary.prior.gifti <- function(object, ...) {
  tparams <- lapply(
    object$params,
    function(q) {
      if (is.null(q)) { q <- "NULL"};
      paste0(as.character(q), collapse=" ")
    }
  )

  x <- c(
    list(
      nV=nrow(object$prior$mean),
      nL=ncol(object$prior$mean),
      hemisphere=object$dat_struct$hemisphere,
      hasDR="DR" %in% names(object)
    ),
    tparams
  )

  class(x) <- "summary.prior.gifti"
  return(x)
}

#' Summarize a \code{"prior.nifti"} object
#'
#' Summary method for class \code{"prior.nifti"}
#'
#' @param object Object of class \code{"prior.nifti"}.
#' @param ... further arguments passed to or from other methods.
#' @export
#' @return A list summarizing the prior: data dimensions, options used for
#'  prior estimation, etc.
#' @method summary prior.nifti
summary.prior.nifti <- function(object, ...) {
  tparams <- lapply(
    object$params,
    function(q) {
      if (is.null(q)) { q <- "NULL"};
      paste0(as.character(q), collapse=" ")
    }
  )

  x <- c(
    list(
      mask_dims=dim(object$mask_input),
      nV=nrow(object$prior$mean),
      nL=ncol(object$prior$mean),
      hasDR="DR" %in% names(object)
    ),
    tparams
  )

  class(x) <- "summary.prior.nifti"
  return(x)
}

#' Summarize a \code{"prior.matrix"} object
#'
#' Summary method for class \code{"prior.matrix"}
#'
#' @param object Object of class \code{"prior.matrix"}.
#' @param ... further arguments passed to or from other methods.
#' @export
#' @return A list summarizing the prior: data dimensions, options used for
#'  prior estimation, etc.
#' @method summary prior.matrix
summary.prior.matrix <- function(object, ...) {
  tparams <- lapply(
    object$params,
    function(q) {
      if (is.null(q)) { q <- "NULL"};
      paste0(as.character(q), collapse=" ")
    }
  )

  x <- c(
    list(
      nV=nrow(object$prior$mean),
      nL=ncol(object$prior$mean),
      hasDR="DR" %in% names(object)
    ),
    tparams
  )

  class(x) <- "summary.prior.matrix"
  return(x)
}

#' @rdname summary.prior.cifti
#' @export
#'
#' @param x The prior from \code{estimate_prior.cifti}
#' @param ... further arguments passed to or from other methods.
#' @return Nothing, invisibly.
#' @method print summary.prior.cifti
print.summary.prior.cifti <- function(x, ...) {
  # Get TR
  the_TR <- if (x$TR == "NULL") {
    "not provided"
  } else {
    paste("TR=", x$TR, "s.")
  }
  # Get highpass filter
  the_hpf <- if (x$hpf == "0") {
    "not used"
  } else {
    paste(as.character(x$hpf), "Hz")
  }

  cat("====PRIOR INFO=======================\n")
  cat("# Subjects:      ", x$num_subjects, "\n")
  cat("Temporal Res.:   ", the_TR, "\n")
  cat("Highpass filter: ", the_hpf, "\n")
  cat("Spatial scaling: ", x$scale, "\n")
  cat("Q2 and Q2_max:   ", paste0(x$Q2, ", ", x$Q2_max), "\n")
  cat("Pseudo retest:   ", x$pseudo_retest, "\n")
  cat("FC prior:        ", x$FC, "\n")
  cat("\n")

  class(x) <- "summary.xifti"
  print(x)
  invisible(NULL)
}

#' @rdname summary.prior.gifti
#' @export
#'
#' @param x The prior from \code{estimate_prior.gifti}
#' @param ... further arguments passed to or from other methods.
#' @return Nothing, invisibly.
#' @method print summary.prior.gifti
print.summary.prior.gifti <- function(x, ...) {
  # Get TR
  the_TR <- if (x$TR == "NULL") {
    "not provided"
  } else {
    paste("TR=", x$TR, "s.")
  }
  # Get highpass filter
  the_hpf <- if (x$hpf == "0") {
    "not used"
  } else {
    paste(as.character(x$hpf), "Hz")
  }

  cat("====PRIOR INFO=======================\n")
  cat("# Subjects:      ", x$num_subjects, "\n")
  cat("Temporal Res.:   ", the_TR, "\n")
  cat("Highpass filter: ", the_hpf, "\n")
  cat("Spatial scaling: ", x$scale, "\n")
  cat("Q2 and Q2_max:   ", paste0(x$Q2, ", ", x$Q2_max), "\n")
  cat("Pseudo retest:   ", x$pseudo_retest, "\n")
  cat("-------------------------------------\n")
  cat("# Locations:     ", x$nV, "\n")
  cat("# Networks:      ", x$nL, "\n")
  cat("Hemisphere:      ", x$hemisphere, "\n")
  cat("\n")

  invisible(NULL)
}

#' @rdname summary.prior.nifti
#' @export
#'
#' @param x The prior from \code{estimate_prior.nifti}
#' @param ... further arguments passed to or from other methods.
#' @return Nothing, invisibly.
#' @method print summary.prior.nifti
print.summary.prior.nifti <- function(x, ...) {
  # Get TR
  the_TR <- if (x$TR == "NULL") {
    "not provided"
  } else {
    paste("TR=", x$TR, "s.")
  }
  # Get highpass filter
  the_hpf <- if (x$hpf == "0") {
    "not used"
  } else {
    paste(as.character(x$hpf), "Hz")
  }

  cat("====PRIOR INFO=======================\n")
  cat("# Subjects:      ", x$num_subjects, "\n")
  cat("Temporal Res.:   ", the_TR, "\n")
  cat("Highpass filter: ", the_hpf, "\n")
  cat("Spatial scaling: ", x$scale, "\n")
  cat("Q2 and Q2_max:   ", paste0(x$Q2, ", ", x$Q2_max), "\n")
  cat("Pseudo retest:   ", x$pseudo_retest, "\n")
  cat("-------------------------------------\n")
  cat("Mask dims:       ", paste0(x$mask_dims, collapse=" x "), "\n")
  cat("Vectorized dims:\n")
  cat("# Locations:     ", x$nV, "\n")
  cat("# Networks:      ", x$nL, "\n")
  cat("\n")

  invisible(NULL)
}

#' @rdname summary.prior.matrix
#' @export
#'
#' @param x The prior from \code{estimate_prior.cifti}
#' @param ... further arguments passed to or from other methods.
#' @return Nothing, invisibly.
#' @method print summary.prior.matrix
print.summary.prior.matrix <- function(x, ...) {
  # Get TR
  the_TR <- if (x$TR == "NULL") {
    "not provided"
  } else {
    paste("TR=", x$TR, "s.")
  }
  # Get highpass filter
  the_hpf <- if (x$hpf == "0") {
    "not used"
  } else {
    paste(as.character(x$hpf), "Hz")
  }

  cat("====PRIOR INFO=======================\n")
  cat("# Subjects:      ", x$num_subjects, "\n")
  cat("Temporal Res.:   ", the_TR, "\n")
  cat("Highpass filter: ", the_hpf, "\n")
  cat("Spatial scaling: ", x$scale, "\n")
  cat("Q2 and Q2_max:   ", paste0(x$Q2, ", ", x$Q2_max), "\n")
  cat("Pseudo retest:   ", x$pseudo_retest, "\n")
  cat("-------------------------------------\n")
  cat("Dimensions:      \n")
  cat("# Locations:     ", x$nV, "\n")
  cat("# Networks:      ", x$nL, "\n")
  cat("\n")

  invisible(NULL)
}

#' @rdname summary.prior.cifti
#' @export
#'
#' @return Nothing, invisibly.
#' @method print prior.cifti
print.prior.cifti <- function(x, ...) {
  print.summary.prior.cifti(summary(x))
}

#' @rdname summary.prior.gifti
#' @export
#'
#' @return Nothing, invisibly.
#' @method print prior.gifti
print.prior.gifti <- function(x, ...) {
  print.summary.prior.gifti(summary(x))
}

#' @rdname summary.prior.nifti
#' @export
#'
#' @return Nothing, invisibly.
#' @method print prior.nifti
print.prior.nifti <- function(x, ...) {
  print.summary.prior.nifti(summary(x))
}

#' @rdname summary.prior.matrix
#' @export
#'
#' @return Nothing, invisibly.
#' @method print prior.matrix
print.prior.matrix <- function(x, ...) {
  print.summary.prior.matrix(summary(x))
}

#' Plot prior
#'
#' @param x The prior from \code{estimate_prior.cifti}
#' @param what The \code{"maps"} (default) on the brain, or the \code{"FC"}
#'  matrix. If both are desired, use two separate \code{plot} calls to first
#'  plot the maps and then plot the FC.
#'
#'  If \code{"FC"}, the default color scale will be from blue (-1) to red (1).
#'  This can be changed with the \code{colFUN} argument to
#'  \code{\link[fMRItools]{plot_FC_gg}}.
#' @param stat Which prior statistic to plot: the \code{"mean"} (default),
#'  \code{"sd"} for the square root of the variance template, or \code{"var"}
#'  for the variance template.
#' @param var_method \code{"non-negative"} (default) or \code{"unbiased"}, for
#'  the variance estimate of the maps. Note that FC variance estimates are
#'  always non-negative.
#' @param FC_method If \code{what=="FC"}: empirical (\code{"emp"}) (default),
#'  Inverse-Wishart (\code{"IW"}), or Cholesky (\code{"Chol"}).
#' @param ... Additional arguments to \code{\link[ciftiTools]{view_xifti}}
#'  if \code{what=="maps"}, or \code{\link[fMRItools]{plot_FC_gg}} if
#'  \code{what=="FC"}.
#' @return The plot
#' @export
#' @method plot prior.cifti
plot.prior.cifti <- function(x,
  what=c("maps", "FC"),
  stat=c("mean", "sd", "var"),
  var_method=c("non-negative", "unbiased"),
  FC_method=c("empirical", "IW", "Chol", "none"),
  ...) {

  stopifnot(inherits(x, "prior.cifti"))

  if (!requireNamespace("ciftiTools", quietly = TRUE)) {
    stop("Package \"ciftiTools\" needed to plot CIFTI data. Please install it.", call. = FALSE)
  }

  what <- match.arg(what, c("maps", "FC"))
  stat <- match.arg(stat, c("mean", "sd", "var"))
  var_method <- match.arg(var_method, c("non-negative", "unbiased"))
  FC_method <- match.arg(FC_method, c("empirical", "IW", "Chol", "none"))

  # Check `...`
  args <- list(...)
  has_title <- "title" %in% names(args)
  skip_title <- has_title && is.null(args$title)
  has_idx <- "idx" %in% names(args)
  has_fname <- "fname" %in% names(args)
  has_labs <- "labs" %in% names(args)

  # Print message saying what's happening.
  msg1 <- ifelse(has_idx || what=="FC",
    "Plotting the",
    "Plotting the first network's"
  )
  msg2 <- switch(stat,
    mean="mean",
    sd="sqrt(variance)",
    var="variance"
  )
  msg3 <- switch(what,
    maps="spatial map",
    FC="FC"
  )
  cat(msg1, msg2, msg3, "prior.\n")

  # Plot
  if (what == "FC" && is.null(x$prior$FC)) {
    stop("`what=='FC'` but there's no FC prior.")
  }

  ss <- stat
  ss2 <- ss
  if (what == "FC") { ss2 <- paste0(ss2, "_FC") }
  ssname <- if (ss == "mean") {
    ss
  } else if (what == "FC") {
    "varNN"
  } else if (var_method=="non-negative") {
    "varNN"
  } else {
    "varUB"
  }

  dat <- if (what == "maps") {
    x$prior[[ssname]]
  } else if (what == "FC") {
    x$prior$FC[[FC_method]][[switch(
      ssname, mean="mean", varNN="var", varUB=stop())]]
  } else { stop() }

  if (ss=="var" && var_method=="unbiased") { dat[] <- pmax(1e-6, dat) }
  if (ss=="sd") {
    dat <- sqrt(dat)
    ssname <- paste0("sqrt ", ssname)
  }

  if (what == "maps") {
    tss <- struct_prior(
      dat, "CIFTI", x$mask_input, x$params, x$dat_struct, x$template_parc_table)
  }

  args_ss <- args
  # Handle title and idx
  if (what == "maps") {
    ### No title: use the network names if available, and the indices if not.
    if (!has_title && !has_idx) {
      args_ss$title <- if (!is.null(rownames(x$template_parc_table))) {
        rownames(x$template_parc_table)[1]
      } else if (!is.null(x$dat_struct$meta$cifti$names)) {
        x$dat_struct$meta$cifti$names[1]
      } else {
        "First network"
      }
    } else if (!has_title) {
      args_ss$title <- if (!is.null(rownames(x$template_parc_table))) {
        rownames(x$template_parc_table)[args$idx]
      } else if (!is.null(x$dat_struct$meta$cifti$names)) {
        x$dat_struct$meta$cifti$names[args$idx]
      } else {
        paste("Network", args$idx)
      }
    }
    ### Append the statistic name.
    args_ss$title <- paste0(args_ss$title, " (", ssname, ")")
  } else {
    if (has_title) {
      ### Append the statistic name.
      args_ss$title <- paste0(args_ss$title, " (", ssname, ")")
    }
  }
  # Handle fname
  if (has_fname) {
    fext <- if (grepl("html$", args_ss$fname[1])) {
      "html"
    } else if (grepl("pdf$", args_ss$fname[1])) {
      "pdf"
    } else {
      "png"
    }
    args_ss$fname <- gsub(paste0(".", fext), "", args_ss$fname, fixed=TRUE)
    args_ss$fname <- paste0(args_ss$fname, "_", ss, ".", fext)
  }

  if (skip_title) { args_ss$title <- NULL }

  if (what == "maps") {
    out <- do.call(
      ciftiTools::view_xifti, c(list(tss), args_ss)
    )
    if (inherits(out, "htmlwidget")) { print(out) }
  } else if (what == "FC") {
    if (!has_labs) {
      net_names <- if (!is.null(x$template_parc_table)) {
        rownames(x$template_parc_table)
      } else {
        x$params$inds
      }
      args_ss$labs <- net_names
    }
    if (!("diagVal" %in% names(args_ss)) && stat!="mean") { args_ss$diagVal <- 0 }
    out <- do.call(
      fMRItools::plot_FC_gg, c(list(dat), args_ss)
    )
    print(out)
  } else { stop() }

  invisible(out)
}

#' Plot prior
#'
#' @param x The prior from \code{estimate_prior.gifti}
#' @param what The \code{"maps"} (default) on the brain, or the \code{"FC"}
#'  matrix. If both are desired, use two separate \code{plot} calls to first
#'  plot the maps and then plot the FC.
#'
#'  If \code{"FC"}, the default color scale will be from blue (-1) to red (1).
#'  This can be changed with the \code{colFUN} argument to
#'  \code{\link[fMRItools]{plot_FC_gg}}.
#' @param stat Which prior statistic to plot: the \code{"mean"} (default),
#'  \code{"sd"} for the square root of the variance template, or \code{"var"}
#'  for the variance template.
#' @param var_method \code{"non-negative"} (default) or \code{"unbiased"}, for
#'  the variance estimate of the maps. Note that FC variance estimates are
#'  always non-negative.
#' @param FC_method If \code{what=="FC"}: empirical (\code{"empirical"}) (default),
#'  Inverse-Wishart (\code{"IW"}), or Cholesky (\code{"Chol"}).
#' @param ... Additional arguments to \code{view_xifti} if \code{what=="maps"},
#'  or \code{\link[fMRItools]{plot_FC_gg}} if \code{what=="FC"}.
#' @return The plot
#' @export
#' @method plot prior.gifti
plot.prior.gifti <- function(x,
  what=c("maps", "FC"),
  stat=c("mean", "sd", "var"),
  var_method=c("non-negative", "unbiased"),
  FC_method=c("empirical", "IW", "Chol", "none"),
  ...) {

  stopifnot(inherits(x, "prior.gifti"))

  if (x$dat_struct$hemisphere == "left")  {
    y <- ciftiTools::as_cifti(cortexL=x$prior$mean[,1,drop=FALSE] * 0)
  } else {
    y <- ciftiTools::as_cifti(cortexR=x$prior$mean[,1,drop=FALSE] * 0)
  }
  y <- ciftiTools::move_from_mwall(y)
  x$dat_struct <- y; class(x) <- "prior.cifti"

  plot.prior.cifti(x,
    what=what, stat=stat, FC_method=FC_method, var_method=var_method,
    ...
  )
}

#' Plot prior
#'
#' Based on \code{oro.nifti::image}.
#'
#' Consider using \code{struct_prior} to obtain the 3D volumes to plot with a different
#'  viewer function (e.g. from \code{oro.nifti}) if desired.
#'
#' @param x The prior from \code{estimate_prior.nifti}
#' @param what The \code{"maps"} (default) on the brain, or the \code{"FC"}
#'  matrix. If both are desired, use two separate \code{plot} calls to first
#'  plot the maps and then plot the FC.
#'
#'  If \code{"FC"}, the default color scale will be from blue (-1) to red (1).
#'  This can be changed with the \code{colFUN} argument to
#'  \code{\link[fMRItools]{plot_FC_gg}}.
#' @param stat Which prior statistic to plot: the \code{"mean"} (default),
#'  \code{"sd"} for the square root of the variance template, or \code{"var"}
#'  for the variance template.
#' @param var_method \code{"non-negative"} (default) or \code{"unbiased"}, for
#'  the variance estimate of the maps. Note that FC variance estimates are
#'  always non-negative.
#' @param FC_method If \code{what=="FC"}: empirical (\code{"empirical"}) (default),
#'  Inverse-Wishart (\code{"IW"}), or Cholesky (\code{"Chol"}).
#' @param plane,n_slices,slices Anatomical plane and which slice indices to
#'  show.
#'  Default: 9 axial slices.
#' @param ... Additional arguments to \code{oro.nifti::image}
#'  if \code{what=="maps"}, or \code{\link[fMRItools]{plot_FC_gg}} if
#'  \code{what=="FC"}.
#' @return The plot
#' @export
#' @method plot prior.nifti
plot.prior.nifti <- function(x,
  what=c("maps", "FC"),
  stat=c("mean", "sd", "var"),
  FC_method=c("empirical", "IW", "Chol", "none"),
  var_method=c("non-negative", "unbiased"),
  plane=c("axial", "sagittal", "coronal"), n_slices=9, slices=NULL,
  ...) {

  stopifnot(inherits(x, "prior.nifti"))

  if (!requireNamespace("oro.nifti", quietly = TRUE)) {
    stop("Package \"oro.nifti\" needed to read NIFTI data. Please install it.", call. = FALSE)
  }

  what <- match.arg(what, c("maps", "FC"))
  stat <- match.arg(stat, c("mean", "sd", "var"))
  var_method <- match.arg(var_method, c("non-negative", "unbiased"))
  FC_method <- match.arg(FC_method, c("emp", "IW", "Chol", "none"))

  # Check `...`
  args <- list(...)
  has_title <- "title" %in% names(args)
  has_idx <- "idx" %in% names(args)
  has_fname <- "fname" %in% names(args)
  has_labs <- "labs" %in% names(args)

  # Check `idx`
  if (has_idx) {
    stopifnot(length(args$idx)==1)
    stopifnot(is.numeric(args$idx) && args$idx==round(args$idx))
    stopifnot(args$idx %in% seq(ncol(x$prior$mean)))
  } else {
    args$idx <- 1
  }
  idx <- args$idx; args$idx <- NULL

  # Print message saying what's happening.
  msg1 <- ifelse(has_idx || what=="FC",
    "Plotting the",
    "Plotting the first network's"
  )
  msg2 <- switch(stat,
    mean="mean",
    sd="sqrt(variance)",
    var="variance"
  )
  msg3 <- switch(what,
    maps="spatial map",
    FC="FC"
  )
  cat(msg1, msg2, msg3, "prior.\n")

  # Plot
  if (what == "FC" && is.null(x$prior$FC)) {
    stop("`what=='FC'` but there's no FC prior.")
  }

  ss <- stat
  ss2 <- ss
  if (what == "FC") { ss2 <- paste0(ss2, "_FC") }
  ssname <- if (ss == "mean") {
    ss
  } else if (what == "FC") {
    "varNN"
  } else if (var_method=="non-negative") {
    "varNN"
  } else {
    "varUB"
  }

  if (what == "maps") {
    plane <- match.arg(plane, c("axial", "sagittal", "coronal"))
    args$plane <- plane
    plane_dim <- switch(plane, axial=3, coronal=2, sagittal=1)
    if (is.null(slices)) {
      if (is.null(n_slices)) { warning("Using 9 slices."); n_slices <- 9 }
      n_slices <- as.numeric(n_slices)
      if (length(n_slices) > 1) { warning("Using the first entry of `slice`."); n_slices <- n_slices[1] }
      # Pick slices that are spaced out, and with many voxels.
      mask_count <- apply(x$mask_input, plane_dim, sum)
      ns_all <- length(mask_count)
      slices <- seq(ns_all)
      # Remove slices with zero voxels.
      slices <- slices[mask_count != 0]
      mask_count <- mask_count[mask_count != 0]
      ns_all <- length(mask_count)
      if (n_slices > length(slices)) {
        warning(
          "`n_slices` is larger than the number of non-empty slices (",
          length(slices), "). Showing all non-empty slices."
        )
        n_slices <- length(slices)
      }
      # Remove slices with few voxels.
      if (n_slices < (ns_all / 2)) {
        slices <- slices[mask_count > quantile(mask_count, .33)]
      }
      slices <- slices[round(seq(1, length(slices), length.out=n_slices))]
    } else {
      slices <- as.numeric(slices)
      stopifnot(all(slices %in% seq(dim(x$mask_input)[plane_dim])))
    }
  }

  dat <- if (what == "maps") {
    x$prior[[ssname]]
  } else if (what == "FC") {
    x$prior$FC[[FC_method]][[switch(
      ssname, mean="mean", varNN="var", varUB=stop())]]
  } else { stop() }

  if (ss=="var" && var_method=="unbiased") { dat[] <- pmax(0, dat) }
  if (stat=="sd") {
    dat <- sqrt(dat)
    ssname <- paste0("sqrt ", ssname)
  }

  if (what == "maps") {
    tss <- struct_prior(dat, "NIFTI", x$mask_input, x$params, x$dat_struct, x$template_parc_table)
    tss <- tss[,,,idx]

    if (plane=="axial") {
      tss <- tss[,,slices,drop=FALSE]
    } else if (plane=="coronal") {
      tss <- tss[,slices,,drop=FALSE]
    } else if (plane=="sagittal") {
      tss <- tss[slices,,,drop=FALSE]
    } else { stop() }
  }

  args_ss <- args
  if (what == "maps") {
    args_ss$plane <- plane
    if (has_title) { stop("Not supported yet.") }
    if (has_fname) { stop("Not supported yet. Call `pdf` or `png` beforehand, and then `dev.off`.") }
    out <- do.call(
      oro.nifti::image,
      c(list(oro.nifti::as.nifti(tss)), args_ss)
    )

  } else if (what == "FC") {
    # Handle title and idx
    ### No title: use the network names if available, and the indices if not.
    if (!has_title && !has_idx) {
      args_ss$title <- if (!is.null(x$dat_struct$meta$cifti$names)) {
        x$dat_struct$meta$cifti$names[1]
      } else {
        "First network"
      }
    } else if (!has_title) {
      args_ss$title <- if (!is.null(x$dat_struct$meta$cifti$names)) {
        x$dat_struct$meta$cifti$names[args$idx]
      } else {
        paste("Network", args$idx)
      }
    }
    ### Append the statistic name.
    args_ss$title <- paste0(args_ss$title, " (", ssname, ")")
    # Handle fname
    if (has_fname) {
      fext <- if (grepl("html$", args_ss$fname[1])) {
        "html"
      } else if (grepl("pdf$", args_ss$fname[1])) {
        "pdf"
      } else {
        "png"
      }
      args_ss$fname <- gsub(paste0(".", fext), "", args_ss$fname, fixed=TRUE)
      args_ss$fname <- paste0(args_ss$fname, "_", ss, ".", fext)
    }

    if (!has_labs) {
      net_names <- if (!is.null(x$template_parc_table)) {
        rownames(x$template_parc_table)
      } else {
        x$params$inds
      }
      args_ss$labs <- net_names
    }
    if (!("diagVal" %in% names(args_ss)) && stat!="mean") { args_ss$diagVal <- 0 }
    out <- do.call(
      fMRItools::plot_FC_gg, c(list(dat), args_ss)
    )
    print(out)
  } else { stop() }

  invisible(out)
}

#' Plot prior
#'
#' @param x The prior from \code{estimate_prior.matrix}
#' @param ... Additional arguments
#' @return The plot
#' @export
#' @method plot prior.matrix
plot.prior.matrix <- function(x, ...) {
  stop("Not supported yet.")
}
