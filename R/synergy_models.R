# Cell estimates are descriptive and matched inside each biological unit.
# No cell or technical replicate contributes an inferential degree of freedom.
ldj_synergy_excess <- function(data,cutoff=10) {
  if(!is.numeric(cutoff) || length(cutoff)!=1L || !is.finite(cutoff) || cutoff<=0)
    stop("El umbral descriptivo debe ser positivo.")
  d <- ldj_synergy_prepare(data)
  a <- sort(unique(d$dose_A_M[d$dose_A_M>0]))
  b <- sort(unique(d$dose_B_M[d$dose_B_M>0]))
  if(!length(a) || !length(b)) stop("Incluya al menos una dosis positiva por eje.")
  strata <- split(d,list(d$experiment_id,d$biological_replicate_id),drop=TRUE)
  do.call(rbind,lapply(strata,function(z) {
    cell <- expand.grid(dose_A_M=a,dose_B_M=b)
    lookup <- function(a,b) {
      x <- z$readout_value[z$dose_A_M==a & z$dose_B_M==b]
      if(!length(x) || !any(is.finite(x))) return(NA_real_)
      if(any(x<0 | x>100,na.rm=TRUE)) return(NA_real_)
      mean(x,na.rm=TRUE)
    }
    cell$observed <- mapply(lookup,cell$dose_A_M,cell$dose_B_M)
    cell$mono_A <- vapply(cell$dose_A_M,lookup,numeric(1),b=0)
    cell$mono_B <- vapply(cell$dose_B_M,function(y) lookup(0,y),numeric(1))
    cell$control <- lookup(0,0)
    cell$nd_reason <- ifelse(!is.finite(cell$control),"Control ausente o fuera de rango",
      ifelse(!is.finite(cell$observed),"Combinación ausente o fuera de rango",
        ifelse(!is.finite(cell$mono_A) | !is.finite(cell$mono_B),
          "Monoterapia correspondiente ausente o fuera de rango","")))
    cell$Bliss_expected <- with(cell,mono_A+mono_B-mono_A*mono_B/100)
    cell$HSA_expected <- with(cell,pmax(mono_A,mono_B))
    for(model in c("Bliss","HSA")) {
      score <- cell$observed-cell[[paste0(model,"_expected")]]
      score[!is.finite(cell$control)] <- NA_real_
      cell[[paste0(model,"_excess")]] <- score
      cell[[paste0(model,"_category")]] <- ifelse(!is.finite(score),"ND",
        ifelse(score>cutoff,"Sinergia descriptiva",ifelse(score< -cutoff,"Antagonismo descriptivo","Sin exceso relevante")))
    }
    cell$compound_A <- z$compound_A[1L]
    cell$compound_B <- z$compound_B[1L]
    cell$experiment_id <- z$experiment_id[1L]
    cell$biological_replicate_id <- z$biological_replicate_id[1L]
    cell
  }))
}

# Estimate paired effects within each biological unit, then weight each
# independent experiment equally. HSA is a descriptive reference. The
# co-primary "better than both" test is intersection-union (max of two p's).
ldj_synergy_inference <- function(cells, independent_experiments=FALSE) {
  stopifnot(is.data.frame(cells),!anyDuplicated(cells[c("dose_A_M","dose_B_M","experiment_id",
    "biological_replicate_id")]))
  cells$against_A <- cells$observed-cells$mono_A
  cells$against_B <- cells$observed-cells$mono_B
  comparable <- is.finite(cells$against_A) & is.finite(cells$against_B) & is.finite(cells$control)
  cells$against_A[!comparable] <- cells$against_B[!comparable] <- NA_real_
  cells$against_best <- pmin(cells$against_A,cells$against_B)
  contrasts <- c("against_A","against_B","against_best","Bliss_excess","HSA_excess")
  summarize <- function(d) {
    out <- lapply(contrasts,function(key) {
      vals <- d[[key]]
      finite <- is.finite(vals)
      z <- vapply(split(vals[finite],d$experiment_id[finite]),mean,numeric(1))
      n <- length(z)
      est <- if(n) mean(z) else NA_real_
      spread <- if(n>1L) stats::sd(z) else NA_real_
      valid <- isTRUE(independent_experiments) && n>=3L && is.finite(spread) && spread>0 &&
        !key %in% c("against_best","HSA_excess")
      se <- if(valid) spread/sqrt(n) else NA_real_
      margin <- if(valid) stats::qt(.975,n-1L)*se else NA_real_
      data.frame(contrast=key,estimate_pp=est,n_experiments=n,sd_between=spread,
        low=est-margin,high=est+margin,p=if(valid) stats::pt(est/se,n-1L,lower.tail=FALSE) else NA_real_)
    })
    ans <- do.call(rbind,out)
    tests <- ans$p[1:2]
    ans <- rbind(ans,data.frame(contrast="superior_to_both",estimate_pp=NA_real_,
      n_experiments=ans$n_experiments[1],sd_between=NA_real_,low=NA_real_,high=NA_real_,
      p=if(all(is.finite(tests))) max(tests) else NA_real_))
    cbind(d[rep(1L,nrow(ans)),c("dose_A_M","dose_B_M"),drop=FALSE],ans)
  }
  if(!nrow(cells)) stop("No hay pares de dosis.")
  groups <- split(seq_len(nrow(cells)),list(cells$dose_A_M,cells$dose_B_M),drop=TRUE)
  result <- do.call(rbind,lapply(groups,function(i) summarize(cells[i,,drop=FALSE])))
  result$p_holm <- stats::p.adjust(result$p,method="holm")
  rownames(result) <- NULL
  result
}
