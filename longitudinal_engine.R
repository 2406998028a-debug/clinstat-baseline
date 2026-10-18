# Longitudinal mean-score prototype 0.1.0. source() does not install/read/write.
# Complete independent-patient wide data only. No automatic model selection.
LONGITUDINAL_ENGINE_VERSION <- "0.3.0"

lt_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE))
    stop("Missing package: ", package, "; install explicitly before analysis")
}

lt_prepare_wide <- function(data, group, outcomes, id = NULL,
                            row_pairing_confirmed = FALSE,
                            source_rows = seq_len(nrow(data)) + 1L,
                            group_levels = NULL) {
  if (!is.data.frame(data) || !nrow(data) || !ncol(data) ||
      anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data)))
    stop("Require a nonempty data.frame with unique nonempty column names")
  if (length(group) != 1L || !group %in% names(data)) stop("Group column not found")
  if (length(source_rows) != nrow(data) || anyNA(source_rows) ||
      anyDuplicated(source_rows) || any(!is.finite(source_rows)) ||
      any(source_rows < 1 | source_rows != floor(source_rows)))
    stop("Source row numbers must be unique positive integers")
  groups <- as.character(data[[group]])
  if (anyNA(groups) || any(!nzchar(trimws(groups)))) stop("Missing group")
  if (is.null(group_levels)) group_levels <- sort(unique(groups))
  if (length(group_levels) < 2L || anyNA(group_levels) ||
      anyDuplicated(group_levels) || !setequal(groups, group_levels))
    stop("Confirm two or more group levels covering the observed groups exactly")
  if (is.null(id)) {
    if (!isTRUE(row_pairing_confirmed)) stop("Confirm one patient per source row")
    patient <- paste0("source_row_", source_rows)
    id_source <- "source row; one-patient-per-row confirmed by user"
  } else {
    if (length(id) != 1L || !id %in% names(data) || id == group)
      stop("Patient ID column not found or conflicts with group")
    patient <- as.character(data[[id]])
    id_source <- id
  }
  if (anyNA(patient) || any(!nzchar(trimws(patient))) || anyDuplicated(patient))
    stop("Wide-table patient IDs must be present and globally unique")
  if (!is.list(outcomes) || !length(outcomes) || is.null(names(outcomes)) ||
      any(!nzchar(names(outcomes))) || anyDuplicated(names(outcomes)))
    stop("Provide explicitly named outcome mappings")
  mapped_columns <- unlist(lapply(outcomes, `[[`, "columns"), use.names = FALSE)
  if (anyDuplicated(mapped_columns)) stop("A measurement column belongs to multiple outcomes")
  prepared <- lapply(names(outcomes), function(label) {
    spec <- outcomes[[label]]
    if (is.null(spec$type)) spec$type <- "continuous"
    if (!spec$type %in% c("continuous", "binary", "ordinal")) stop("Unknown outcome type: ",label)
    columns <- as.character(spec$columns); times <- as.character(spec$times)
    if (length(columns) < 2L || length(columns) != length(times) ||
        anyNA(times) || any(!nzchar(times)) || anyDuplicated(times) ||
        !all(columns %in% names(data)) || anyDuplicated(columns) ||
        any(columns %in% c(group, id))) stop("Invalid mapping for ", label)
    bounds <- spec$bounds
    if (!is.null(bounds) && (length(bounds) != 2L || anyNA(bounds) ||
                            any(!is.finite(bounds)) || bounds[1] >= bounds[2]))
      stop("Invalid scale bounds for ", label)
    all_values <- unlist(data[columns], use.names = FALSE)
    if (anyNA(all_values) || any(!nzchar(trimws(as.character(all_values)))))
      stop("Complete measurements required: ",label,"; no silent deletion or imputation")
    if (spec$type == "binary") {
      observed <- unique(as.character(all_values))
      if (length(observed) != 2L || length(spec$event) != 1L || !as.character(spec$event) %in% observed)
        stop("Binary outcome requires exactly two observed values and an explicit event value: ",label)
    }
    if (spec$type == "ordinal") {
      lev <- as.character(unlist(spec$levels))
      if (length(lev)<2L || anyNA(lev) || anyDuplicated(lev) || any(!nzchar(lev)) ||
          !setequal(lev,unique(as.character(all_values))))
        stop("Ordinal levels must be explicit, unique, low-to-high, and match all observed categories: ",label)
      spec$levels <- lev
    }
    for (column in columns) {
      x <- data[[column]]
      if (spec$type == "continuous" && (!is.numeric(x) || anyNA(x) || any(!is.finite(x))))
        stop("Complete finite numeric measurements required: ", column,
             "; no silent deletion or imputation")
      if (spec$type == "continuous" && !is.null(bounds) && any(x < bounds[1] | x > bounds[2]))
        stop("Out-of-range score: ", column)
      if (spec$type == "continuous" && isTRUE(spec$integer) && any(x != floor(x))) stop("Noninteger score: ", column)
    }
    d <- do.call(rbind, lapply(seq_along(columns), function(j)
      data.frame(patient = patient, group = groups, time = times[j],
                 wave = j, value = data[[columns[j]]], source_row = source_rows,
                 source_column = columns[j], stringsAsFactors = FALSE)))
    d$patient <- factor(d$patient, levels = sort(unique(patient)))
    d$group <- factor(d$group, levels = group_levels)
    d$time <- factor(d$time, levels = times)
    d$original_value <- as.character(d$value)
    if (spec$type == "binary") d$value <- as.integer(d$original_value == as.character(spec$event))
    if (spec$type == "ordinal") d$value <- ordered(d$original_value, levels = spec$levels)
    d <- d[order(d$patient, d$wave), , drop = FALSE]; rownames(d) <- NULL
    if (anyDuplicated(d[c("patient", "time")])) stop("Duplicate patient/time key")
    list(label = label, data = d, mapping = spec,
         patients = nrow(data), groups = group_levels, times = times)
  })
  names(prepared) <- names(outcomes)
  # Identical scores do not establish duplicate identities. Never deduplicate.
  extra_identical_profiles <- sum(duplicated(data[c(group, mapped_columns)]))
  list(outcomes = prepared, metadata = list(version = LONGITUDINAL_ENGINE_VERSION,
    patients = nrow(data), id_source = id_source, group_column = group,
    group_levels = group_levels, group_counts = as.list(table(factor(groups, group_levels))),
    identical_profile_extra_records = extra_identical_profiles,
    missing_policy = "complete measurements required; no deletion or imputation"))
}

lt_describe <- function(prepared_outcome) {
  d <- prepared_outcome$data; bounds <- prepared_outcome$mapping$bounds
  do.call(rbind, lapply(prepared_outcome$times, function(t)
    do.call(rbind, lapply(prepared_outcome$groups, function(g) {
      y <- d$value[d$group == g & d$time == t]
      if (is.factor(y)) return(data.frame(outcome=prepared_outcome$label,group=g,time=t,
        n=length(y),missing=0L,mean=NA_real_,sd=NA_real_,q1=NA_real_,median=NA_real_,
        q3=NA_real_,min=NA_real_,max=NA_real_,zeros=sum(as.character(y)=="0"),
        zero_fraction=mean(as.character(y)=="0"),upper_bound_fraction=NA_real_))
      q <- stats::quantile(y, c(.25, .5, .75), names = FALSE, type = 7)
      data.frame(outcome = prepared_outcome$label, group = g, time = t,
        n = length(y), missing = 0L, mean = mean(y), sd = stats::sd(y),
        q1 = q[1], median = q[2], q3 = q[3], min = min(y), max = max(y),
        zeros = sum(y == 0), zero_fraction = mean(y == 0),
        upper_bound_fraction = if (is.null(bounds)) NA_real_ else mean(y == bounds[2]),
        stringsAsFactors = FALSE)
    }))))
}

lt_distribution <- function(outcome) {
  d <- outcome$data; type <- outcome$mapping$type
  if (type == "continuous") return(data.frame())
  lev <- if(type=="ordinal") outcome$mapping$levels else sort(unique(d$original_value))
  do.call(rbind,lapply(outcome$times,function(t) do.call(rbind,lapply(outcome$groups,function(g){
    x <- d$original_value[d$time==t & d$group==g]
    data.frame(outcome=outcome$label,group=g,time=t,category=lev,
      count=as.integer(table(factor(x,levels=lev))),n=length(x),
      proportion=as.numeric(table(factor(x,levels=lev)))/length(x))
  }))))
}

lt_wald <- function(estimate, covariance) {
  covariance <- (covariance + t(covariance)) / 2
  eig <- eigen(covariance, symmetric = TRUE)
  if (any(!is.finite(covariance)) || any(!is.finite(estimate)) ||
      min(eig$values) <= max(abs(eig$values)) * 1e-10)
    stop("Joint contrast covariance is singular or unstable; no Wald P reported")
  statistic <- as.numeric(crossprod(estimate, solve(covariance, estimate)))
  c(statistic = statistic, df = length(estimate),
    p_value = stats::pchisq(statistic, df = length(estimate), lower.tail = FALSE))
}

lt_fit_gaussian <- function(prepared_outcome, mean_score_target_confirmed = FALSE,
                       corstr = c("exchangeable", "independence"), alpha = .05,
                       model = c("gee", "lmm")) {
  if (!isTRUE(mean_score_target_confirmed))
    stop("Explicitly confirm an exploratory mean-score estimand; no automatic GEE selection")
  corstr <- match.arg(corstr); model <- match.arg(model)
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1)
    stop("Invalid alpha")
  d <- prepared_outcome$data; warnings <- character()
  fit <- withCallingHandlers(if (model == "gee") {
    lt_require("geepack")
    geepack::geeglm(value ~ group * time,
      id = patient, waves = wave, data = d, family = stats::gaussian("identity"),
      corstr = corstr, std.err = "san.se")
  } else {
    lt_require("nlme")
    nlme::lme(value ~ group * time, random = ~ 1 | patient, data = d,
              method = "REML", na.action = stats::na.fail)
  }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  if (model == "gee" && fit$geese$error != 0L)
    stop("GEE convergence error: ", fit$geese$error)
  beta <- if (model == "gee") stats::coef(fit) else nlme::fixef(fit)
  V <- stats::vcov(fit)
  df_fun <- function(k) Inf
  if (model == "lmm") {
    # Containment convention matches emmeans for nlme::lme; not Kenward-Roger.
    dfx <- fit$fixDF$X[names(beta)]
    dfx[1] <- nlevels(d$patient) - 1L
    df_fun <- function(k) {
      active <- which(abs(k) > 1e-4)
      if (length(active)) min(dfx[active]) else Inf
    }
  }
  if (any(!is.finite(beta)) || any(!is.finite(V))) stop("Nonfinite GEE estimates")
  grid <- expand.grid(group = prepared_outcome$groups, time = prepared_outcome$times,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$group <- factor(grid$group, levels = prepared_outcome$groups)
  grid$time <- factor(grid$time, levels = prepared_outcome$times)
  X <- stats::model.matrix(~ group * time, grid, contrasts.arg = fit$contrasts)
  X <- X[, names(beta), drop = FALSE]
  mu <- as.numeric(X %*% beta); S <- X %*% V %*% t(X)
  if (any(diag(S) <= 0)) stop("Nonpositive mean-score variance")
  se <- sqrt(diag(S))
  mean_df <- apply(X, 1, df_fun)
  critical <- stats::qt(1 - alpha / 2, df = mean_df)
  ci_type <- if (model == "gee") "pointwise, unadjusted asymptotic normal" else
    "pointwise, unadjusted t; containment df"
  means <- data.frame(outcome = prepared_outcome$label, grid,
    category = "", quantity = "score_mean",
    estimate = mu, se = se, df = mean_df,
    ci_low = mu - critical * se, ci_high = mu + critical * se,
    ci_level = 1 - alpha, ci_type = ci_type,
    row.names = NULL)
  bounds <- prepared_outcome$mapping$bounds
  means$ci_outside_declared_scale <- if (is.null(bounds)) NA else
    means$ci_low < bounds[1] | means$ci_high > bounds[2]
  pairs <- combn(prepared_outcome$groups, 2, simplify = FALSE)
  pairwise <- do.call(rbind, lapply(prepared_outcome$times, function(t)
    do.call(rbind, lapply(pairs, function(pair) {
      c <- as.numeric(grid$group == pair[1] & grid$time == t) -
           as.numeric(grid$group == pair[2] & grid$time == t)
      effect <- sum(c * mu); variance <- as.numeric(crossprod(c, S %*% c))
      if (!is.finite(variance) || variance <= 0) stop("Nonpositive contrast variance")
      error <- sqrt(variance)
      df <- df_fun(as.numeric(c %*% X)); critical <- stats::qt(1 - alpha / 2, df)
      data.frame(outcome = prepared_outcome$label, time = t,
        group_left = pair[1], group_right = pair[2], difference = effect,
        se = error, ci_low = effect - critical * error, ci_high = effect + critical * error,
        statistic = effect / error, df = df,
        p_raw = 2 * stats::pt(-abs(effect / error), df = df),
        ci_level = 1 - alpha, ci_type = ci_type,
        stringsAsFactors = FALSE)
    }))))
  pairwise$p_holm_within_outcome <- stats::p.adjust(pairwise$p_raw, "holm")
  pairwise$effect_type <- "mean_difference"
  pairwise$effect_estimate <- pairwise$difference
  pairwise$effect_ci_low <- pairwise$ci_low
  pairwise$effect_ci_high <- pairwise$ci_high
  pairwise$within_outcome_family_size <- nrow(pairwise)
  gs <- prepared_outcome$groups; ts <- prepared_outcome$times
  cell <- function(g, t) as.numeric(grid$group == g & grid$time == t)
  group_C <- do.call(rbind, lapply(gs[-1], function(g)
    as.numeric(grid$group == g) / length(ts) - as.numeric(grid$group == gs[1]) / length(ts)))
  time_C <- do.call(rbind, lapply(ts[-1], function(t)
    as.numeric(grid$time == t) / length(gs) - as.numeric(grid$time == ts[1]) / length(gs)))
  interaction_C <- do.call(rbind, lapply(gs[-1], function(g)
    do.call(rbind, lapply(ts[-1], function(t)
      cell(g, t) - cell(gs[1], t) - cell(g, ts[1]) + cell(gs[1], ts[1])))))
  any_group_C <- do.call(rbind, lapply(ts, function(t)
    do.call(rbind, lapply(gs[-1], function(g) cell(g, t) - cell(gs[1], t)))))
  tests <- list(group_equal_time_average = group_C,
    time_equal_group_average = time_C, group_time_interaction = interaction_C,
    any_group_difference_across_times = any_group_C)
  joint <- do.call(rbind, lapply(names(tests), function(name) {
    C <- tests[[name]]
    denominator <- df_fun(colSums(abs(C %*% X)))
    ans <- tryCatch(lt_wald(as.numeric(C %*% mu), C %*% S %*% t(C)), error = identity)
    if (inherits(ans, "error")) return(data.frame(outcome = prepared_outcome$label,
      hypothesis = name, statistic = NA_real_, df = nrow(C),
      df_denominator = denominator, distribution = if (model == "gee") "chi-square" else "F",
      p_value = NA_real_,
      status = "needs_review", error = conditionMessage(ans)))
    if (model == "lmm") {
      ans[["statistic"]] <- ans[["statistic"]] / ans[["df"]]
      ans[["p_value"]] <- stats::pf(ans[["statistic"]], ans[["df"]], denominator,
                                   lower.tail = FALSE)
    }
    data.frame(outcome = prepared_outcome$label, hypothesis = name,
      statistic = ans[["statistic"]], df = ans[["df"]],
      df_denominator = denominator, distribution = if (model == "gee") "chi-square" else "F",
      p_value = ans[["p_value"]],
      status = "computed_exploratory", error = "")
  }))
  residual_diagnostics <- NULL
  if (model == "lmm") {
    residuals <- stats::residuals(fit, type = "normalized")
    sw <- if (length(residuals) >= 3 && length(residuals) <= 5000 && stats::sd(residuals) > 0)
      stats::shapiro.test(residuals)$p.value else NA_real_
    residual_diagnostics <- list(shapiro_p = sw,
      note = "Residual screening only; does not select the model or certify assumptions",
      random_intercept_sd = as.numeric(nlme::VarCorr(fit)[1, "StdDev"]),
      residual_sd = fit$sigma)
  }
  list(status = "computed_exploratory", means = means, pairwise = pairwise,
    joint = joint, descriptive = lt_describe(prepared_outcome),
    distribution = lt_distribution(prepared_outcome),
    metadata = list(outcome = prepared_outcome$label, patients = prepared_outcome$patients,
      observations = nrow(d), formula = "value ~ group * time", time_type = "categorical",
      model = model, outcome_type = "continuous", family = "gaussian", link = "identity",
      corstr = if (model == "gee") corstr else "patient random intercept; iid equal-variance conditional residuals",
      estimated_working_correlation = if (model == "gee") unname(fit$geese$alpha) else NULL,
      standard_error = if (model == "gee") "geepack san.se empirical sandwich; no finite-sample correction" else
        "model-based REML covariance; containment df, no Kenward-Roger correction",
      inference = if (model == "gee") "asymptotic normal CI / chi-square joint Wald" else
        "t CI / approximate joint F; containment denominator df",
      residual_diagnostics = residual_diagnostics,
      mean_score_target = "exploratory numeric-score mean, not ordinal odds",
      unbounded_interval_note = "Normal intervals are not clipped to scale bounds; outside-scale intervals require interpretation review",
      mean_intervals_outside_declared_scale = sum(means$ci_outside_declared_scale, na.rm = TRUE),
      alpha = alpha, warnings = unique(warnings),
      package_version = as.character(utils::packageVersion(if (model == "gee") "geepack" else "nlme"))),
    fit = fit, grid_covariance = S)
}

lt_fit_gee <- function(prepared_outcome, mean_score_target_confirmed = FALSE,
                       corstr = "exchangeable", alpha = .05)
  lt_fit_gaussian(prepared_outcome, mean_score_target_confirmed, corstr, alpha, "gee")

lt_fit_categorical <- function(outcome, model=c("gee","lmm"), alpha=.05) {
  model <- match.arg(model); type <- outcome$mapping$type; d <- outcome$data
  if(!is.finite(alpha)||alpha<=0||alpha>=1)stop("Invalid alpha")
  for(g in outcome$groups) for(t in outcome$times) {
    y <- d$value[d$group==g & d$time==t]
    if(type=="binary" && length(unique(y))<2L)
      stop("Complete separation: a group/time cell has only events or only non-events; no finite OR reported")
    if(type=="ordinal" && length(unique(y))==1L &&
       as.character(y[1]) %in% outcome$mapping$levels[c(1,length(outcome$mapping$levels))])
      stop("Boundary-only ordinal group/time cell: finite cumulative OR requires review")
  }
  warnings <- character()
  fit <- withCallingHandlers({
    if(model=="gee") {
      lt_require("geepack")
      if(type=="binary") geepack::geeglm(value~group*time,id=patient,waves=wave,
        data=d,family=stats::binomial("logit"),corstr="independence",std.err="san.se") else
        geepack::ordgee(value~group*time,id=d$patient,waves=d$wave,data=d,
          mean.link="logit",corstr="independence",rev=FALSE)
    } else if(type=="binary") {
      lt_require("lme4")
      lme4::glmer(value~group*time+(1|patient),data=d,family=stats::binomial("logit"),
        nAGQ=1L,control=lme4::glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30000)))
    } else {
      lt_require("ordinal")
      ordinal::clmm(value~group*time+(1|patient),data=d,link="logit",Hess=TRUE,nAGQ=1L)
    }
  },warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")})
  if(model=="gee") {
    code <- if(type=="binary")fit$geese$error else fit$error
    if(code!=0L)stop("Categorical GEE convergence error: ",code)
  }
  grid <- expand.grid(group=outcome$groups,time=outcome$times,
    KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  grid$group<-factor(grid$group,levels=outcome$groups)
  grid$time<-factor(grid$time,levels=outcome$times)
  mm <- stats::model.matrix(~group*time,grid)
  if(type=="binary") {
    if(model=="gee") {beta<-stats::coef(fit);V<-stats::vcov(fit)} else {
      messages <- fit@optinfo$conv$lme4$messages
      if(length(messages) && any(!grepl("singular",messages)))stop(paste(messages,collapse="; "))
      if(lme4::isSingular(fit))warnings<-c(warnings,"Random intercept is on the variance boundary; review model")
      beta<-lme4::fixef(fit);V<-as.matrix(stats::vcov(fit))
    }
    L <- mm[,names(beta),drop=FALSE]; threshold_indices<-integer()
  } else if(model=="gee") {
    beta<-fit$beta;V<-fit$vbeta;dimnames(V)<-list(names(beta),names(beta))
    threshold_indices<-grep("^Inter:",names(beta))
    slopes<-setdiff(names(beta),names(beta)[threshold_indices])
    L<-matrix(0,nrow(mm),length(beta),dimnames=list(NULL,names(beta)))
    L[,slopes]<-mm[,slopes,drop=FALSE]
  } else {
    if(!is.null(fit$optRes$convergence) && fit$optRes$convergence!=0)
      stop("Ordinal mixed model did not converge")
    if(length(fit$gradient) && max(abs(fit$gradient))>1e-3)
      stop("Ordinal mixed-model gradient too large; no inferential result reported")
    beta<-c(fit$alpha,fit$beta);V<-as.matrix(stats::vcov(fit))[names(beta),names(beta),drop=FALSE]
    threshold_indices<-seq_along(fit$alpha);slopes<-names(fit$beta)
    L<-matrix(0,nrow(mm),length(beta),dimnames=list(NULL,names(beta)))
    L[,slopes]<-mm[,slopes,drop=FALSE]
  }
  if(any(!is.finite(beta))||any(!is.finite(V))||any(diag(V)<=0)||
     any(abs(beta)>35)||any(sqrt(diag(V))>1e4))
    stop("Unstable categorical coefficients/covariance; separation or sparse categories require review")
  eta<-as.numeric(L%*%beta);S<-L%*%V%*%t(L);critical<-stats::qnorm(1-alpha/2)
  interpretation<-if(model=="gee")"population-averaged" else "subject-conditional; random intercept set to zero for probabilities"
  means<-list();q<-0L
  for(i in seq_len(nrow(grid))) {
    if(type=="binary") {
      se<-sqrt(S[i,i]);p<-stats::plogis(eta[i]);q<-q+1L
      means[[q]]<-data.frame(outcome=outcome$label,group=as.character(grid$group[i]),
        time=as.character(grid$time[i]),category=as.character(outcome$mapping$event),
        quantity="event_probability",estimate=p,se=p*(1-p)*se,df=Inf,
        ci_low=stats::plogis(eta[i]-critical*se),ci_high=stats::plogis(eta[i]+critical*se),
        ci_level=1-alpha,ci_type="pointwise, logit-scale delta Wald",ci_outside_declared_scale=FALSE)
    } else {
      K<-length(outcome$mapping$levels)
      if(length(threshold_indices)!=K-1L)stop("Ordinal threshold count mismatch")
      H<-matrix(rep(L[i,],each=K-1L),nrow=K-1L)
      if(model=="lmm")H<--H
      for(j in seq_len(K-1L))H[j,threshold_indices[j]]<-1
      cum<-stats::plogis(as.numeric(H%*%beta))
      # ordgee rev=FALSE models P(Y > threshold); clmm uses P(Y <= threshold).
      probs<-if(model=="gee") -diff(c(1,cum,0)) else diff(c(0,cum,1))
      if(any(probs<=0)||any(probs>=1)||abs(sum(probs)-1)>1e-8)
        stop("Crossed/unstable ordinal probabilities; do not silently clip")
      J<-H*(cum*(1-cum))
      for(k in seq_len(K)) {
        previous<-if(k==1L)rep(0,length(beta)) else J[k-1L,]
        next_one<-if(k==K)rep(0,length(beta)) else J[k,]
        gradient<-if(model=="gee")previous-next_one else next_one-previous
        variance<-as.numeric(crossprod(gradient,V%*%gradient))
        if(!is.finite(variance)||variance<=0)stop("Unstable ordinal probability variance")
        p<-probs[k];se<-sqrt(variance);logit_se<-se/(p*(1-p));q<-q+1L
        means[[q]]<-data.frame(outcome=outcome$label,group=as.character(grid$group[i]),
          time=as.character(grid$time[i]),category=outcome$mapping$levels[k],
          quantity="category_probability",estimate=p,se=se,df=Inf,
          ci_low=stats::plogis(stats::qlogis(p)-critical*logit_se),
          ci_high=stats::plogis(stats::qlogis(p)+critical*logit_se),
          ci_level=1-alpha,ci_type="pointwise, logit-scale delta Wald",ci_outside_declared_scale=FALSE)
      }
    }
  }
  pairs<-combn(outcome$groups,2,simplify=FALSE)
  pairwise<-do.call(rbind,lapply(outcome$times,function(t)do.call(rbind,lapply(pairs,function(pair){
    C<-as.numeric(grid$group==pair[1]&grid$time==t)-as.numeric(grid$group==pair[2]&grid$time==t)
    effect<-sum(C*eta);variance<-as.numeric(crossprod(C,S%*%C))
    if(!is.finite(variance)||variance<=0)stop("Unstable categorical contrast variance")
    se<-sqrt(variance);lo<-effect-critical*se;hi<-effect+critical*se
    data.frame(outcome=outcome$label,time=t,group_left=pair[1],group_right=pair[2],
      difference=effect,se=se,ci_low=lo,ci_high=hi,statistic=effect/se,df=Inf,
      p_raw=2*stats::pnorm(-abs(effect/se)),ci_level=1-alpha,ci_type="pointwise, unadjusted log-OR Wald",
      effect_type=if(type=="binary")"event_OR" else "higher_category_OR",
      effect_estimate=exp(effect),effect_ci_low=exp(lo),effect_ci_high=exp(hi))
  }))))
  if(any(!is.finite(pairwise$effect_ci_high)))stop("Unbounded OR interval requires review")
  pairwise$p_holm_within_outcome<-stats::p.adjust(pairwise$p_raw,"holm")
  pairwise$within_outcome_family_size<-nrow(pairwise)
  gs<-outcome$groups;ts<-outcome$times
  cell<-function(g,t)as.numeric(grid$group==g&grid$time==t)
  tests<-list(
    group_equal_time_average=do.call(rbind,lapply(gs[-1],function(g)
      (as.numeric(grid$group==g)-as.numeric(grid$group==gs[1]))/length(ts))),
    time_equal_group_average=do.call(rbind,lapply(ts[-1],function(t)
      (as.numeric(grid$time==t)-as.numeric(grid$time==ts[1]))/length(gs))),
    group_time_interaction=do.call(rbind,lapply(gs[-1],function(g)do.call(rbind,lapply(ts[-1],function(t)
      cell(g,t)-cell(gs[1],t)-cell(g,ts[1])+cell(gs[1],ts[1]))))),
    any_group_difference_across_times=do.call(rbind,lapply(ts,function(t)do.call(rbind,lapply(gs[-1],function(g)cell(g,t)-cell(gs[1],t))))))
  joint<-do.call(rbind,lapply(names(tests),function(name){C<-tests[[name]]
    a<-tryCatch(lt_wald(as.numeric(C%*%eta),C%*%S%*%t(C)),error=identity)
    failed<-inherits(a,"error")
    data.frame(outcome=outcome$label,hypothesis=name,statistic=if(failed)NA_real_ else a[["statistic"]],
      df=nrow(C),df_denominator=Inf,distribution="chi-square",p_value=if(failed)NA_real_ else a[["p_value"]],
      status=if(failed)"needs_review" else "computed_exploratory",error=if(failed)conditionMessage(a) else "")
  }))
  list(status="computed_exploratory",descriptive=lt_describe(outcome),distribution=lt_distribution(outcome),
    means=do.call(rbind,means),pairwise=pairwise,joint=joint,fit=fit,grid_covariance=S,
    metadata=list(outcome=outcome$label,outcome_type=type,model=model,patients=outcome$patients,
      observations=nrow(d),formula="group * categorical time; patient correlation",
      family=if(type=="binary")"binomial" else "cumulative proportional odds",link="logit",
      corstr=if(model=="gee")"independence working structure; patient-cluster robust variance" else "patient random intercept",
      standard_error=if(model=="gee")"empirical sandwich; no finite-sample correction" else "model-based Hessian; Laplace approximation",
      interpretation=interpretation,OR_direction=if(type=="binary")paste("event =",outcome$mapping$event) else
        paste("higher in declared order:",paste(outcome$mapping$levels,collapse=" < ")),
      proportional_odds=if(type=="ordinal")"assumed, not automatically verified; requires substantive/diagnostic review" else NULL,
      warnings=unique(warnings),alpha=alpha,
      package_version=as.character(utils::packageVersion(if(model=="gee")"geepack" else if(type=="binary")"lme4" else "ordinal"))))
}

lt_analyze <- function(prepared, mean_score_target_confirmed = FALSE,
                       corstr = "exchangeable", alpha = .05, model = c("gee", "lmm")) {
  model <- match.arg(model)
  if (!isTRUE(mean_score_target_confirmed)) stop("Confirm mean-score target explicitly")
  results <- lapply(prepared$outcomes, function(outcome) {
    selected_model <- if(is.null(outcome$mapping$model)) model else outcome$mapping$model
    tryCatch(if(outcome$mapping$type=="continuous")
      lt_fit_gaussian(outcome, TRUE, corstr, alpha, selected_model) else
      lt_fit_categorical(outcome, selected_model, alpha), error = function(e)
      list(status = "failed", error = conditionMessage(e), descriptive = lt_describe(outcome),
           distribution = lt_distribution(outcome)))
  })
  combine <- function(field) {
    parts <- lapply(results, `[[`, field)
    parts <- Filter(function(d)!is.null(d)&&is.data.frame(d)&&nrow(d)>0, parts)
    if (!length(parts)) return(data.frame())
    fields <- unique(unlist(lapply(parts,names)))
    parts <- lapply(parts,function(d){for(nm in setdiff(fields,names(d)))d[[nm]]<-NA;d[fields]})
    d <- do.call(rbind, parts); rownames(d) <- NULL; d
  }
  pairwise <- combine("pairwise"); joint <- combine("joint")
  # Families retain all planned hypotheses, even if some models fail.
  planned_pairs <- sum(vapply(prepared$outcomes, function(o)
    choose(length(o$groups), 2) * length(o$times), numeric(1)))
  if (nrow(pairwise)) {
    pairwise$p_holm_all_outcomes <- stats::p.adjust(pairwise$p_raw, "holm", n = planned_pairs)
    pairwise$all_outcome_family_size <- planned_pairs
  }
  if (nrow(joint)) {
    joint$p_holm_all_joint_tests <- stats::p.adjust(joint$p_value, "holm", n = 4L * length(results))
    joint$family_size <- 4L * length(results)
  }
  list(metadata = c(prepared$metadata, list(analysis = "exploratory longitudinal models; type-specific estimands",
      model = model,
      status = if (all(vapply(results, function(r) identical(r$status, "computed_exploratory") &&
        all(r$joint$status == "computed_exploratory"), logical(1)))) "computed_exploratory" else "partial",
      alpha = alpha, planned_pairwise_hypotheses = planned_pairs,
      correction = "Holm within each outcome and separately across all planned pairwise hypotheses",
      interval_note = "CI are pointwise unadjusted; Holm-adjusted P do not imply adjusted CI",
      across_endpoint_confirmatory_claim = FALSE)),
    descriptive = combine("descriptive"), distribution = combine("distribution"), means = combine("means"),
    pairwise = pairwise, joint = joint, results = results)
}
