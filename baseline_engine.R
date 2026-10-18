# Baseline engine 0.1.0 -- independent groups only; source() has no side effects.
# External packages are needed only for file import/export, never installed here.
BASELINE_ENGINE_VERSION <- "0.1.0"

bt_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Missing package: ", pkg, ". Install it before running this operation.")
}

bt_read <- function(path, sheet = 1) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    bt_require("readxl")
    d <- as.data.frame(readxl::read_excel(path, sheet = sheet,
                                        range = readxl::cell_limits(c(1, 1), c(NA, NA)),
                                        .name_repair = "minimal"))
  } else if (ext %in% c("csv", "tsv")) {
    d <- read.table(path, header = TRUE, sep = if (ext == "tsv") "\t" else ",",
                    check.names = FALSE, stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"),
                    comment.char = "", quote = "\"")
  } else stop("Supported input: xlsx, xls, xlsm, csv, tsv")
  # Ignore layout-only columns only when both their header and all cells are
  # blank. A named all-missing clinical variable must remain in the analysis.
  raw_column_count <- ncol(d)
  unnamed <- is.na(names(d)) | !nzchar(trimws(names(d)))
  empty <- vapply(d, function(x) all(is.na(x) | trimws(as.character(x)) == ""), logical(1))
  dropped <- which(unnamed & empty)
  if (length(dropped)) d <- d[, -dropped, drop = FALSE]
  attr(d, "import_notes") <- list(raw_column_count = raw_column_count,
                                  dropped_unnamed_empty_columns = as.integer(dropped))
  bt_check_names(d)
  d
}

bt_check_names <- function(data) {
  if (!is.data.frame(data) || !nrow(data)) stop("Input must be a nonempty data.frame")
  if (anyNA(names(data)) || any(!nzchar(trimws(names(data)))) || anyDuplicated(names(data)))
    stop("Column names must be nonempty and unique; confirm a column mapping first")
}

# Suggestions are NOT confirmations. Numeric coding cannot establish ordinality.
bt_suggest <- function(data, group) {
  bt_check_names(data)
  if (!group %in% names(data)) stop("Group column not found")
  v <- setdiff(names(data), group)
  if (!length(v)) stop("No analysis variables")
  do.call(rbind, lapply(v, function(nm) {
    x <- data[[nm]]; u <- unique(x[!is.na(x)])
    categorical <- !is.numeric(x) || length(u) <= 10
    data.frame(variable = nm, type = if (categorical) "categorical" else "continuous",
               confirmed = FALSE, label = nm, method = "auto", summary = "auto",
               digits = 1L, levels = "", stringsAsFactors = FALSE)
  }))
}

bt_capture <- function(expr) {
  warnings <- character()
  ans <- tryCatch(withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
  }), error = function(e) e)
  list(value = if (inherits(ans, "error")) NULL else ans,
       error = if (inherits(ans, "error")) conditionMessage(ans) else "",
       warnings = paste(unique(warnings), collapse = " | "))
}

bt_levels <- function(x) {
  if (is.factor(x)) return(levels(droplevels(x)))
  z <- unique(as.character(x[!is.na(x)]))
  n <- suppressWarnings(as.numeric(z))
  if (length(z) && all(is.finite(n))) z[order(n)] else sort(z)
}

# Preserve user-specified order. Alternate separators are accepted only if the
# resulting tokens cover every observed value. Never reinterpret label content
# such as "A/B" when the canonical form "A/B|C" was provided.
bt_resolve_levels <- function(spec, observed, variable) {
  clean <- function(x) trimws(chartr("\u00a0\u3000", "  ", as.character(x)))
  text <- if (length(spec) != 1 || is.na(spec)) "" else clean(spec)
  text <- gsub("\uff5c", "|", text, fixed = TRUE)
  split_keep_empty <- function(s) {
    a <- strsplit(paste0(s, "|__END__"), "|", fixed = TRUE)[[1]]
    clean(head(a, -1L))
  }
  lv <- split_keep_empty(text)
  if (!grepl("|", text, fixed = TRUE) && (!length(observed) || !all(observed %in% lv))) {
    alternate <- gsub("[/\uff0f,\uff0c;\uff1b>\uff1e<\uff1c\u2192]", "|", text)
    candidate <- split_keep_empty(alternate)
    if (all(observed %in% candidate) ||
        !any(grepl("[/\uff0f,\uff0c;\uff1b>\uff1e<\uff1c\u2192]", observed))) lv <- candidate
  }
  actual <- if (length(observed)) paste(head(observed, 20), collapse = "、") else "无非缺失观测值"
  example <- if (length(observed)) paste(observed, collapse = "|") else "1|2|3"
  prefix <- paste0("变量“", variable, "”的等级设置有误。实际取值：", actual, "。")
  if (!nzchar(text) || any(!nzchar(lv)))
    stop(prefix, "等级不能为空，也不能连续或在首尾填写分隔符；示例：", example)
  if (anyDuplicated(lv)) stop(prefix, "等级不能重复；示例：", example)
  missing <- setdiff(observed, lv)
  if (length(missing)) stop(prefix, "尚未包含：", paste(head(missing, 20), collapse = "、"),
                            "。请按研究定义确认顺序并完整填写，参考格式：", example,
                            "；支持 |、/、逗号或分号。若取值本身含这些符号，请用 | 分隔完整取值。")
  lv
}

bt_pformat <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

# Brown-Forsythe (median-centered Levene) using base R.
bt_variance_p <- function(x, g) {
  ok <- !is.na(x) & !is.na(g); x <- x[ok]; g <- droplevels(g[ok])
  if (nlevels(g) < 2 || any(table(g) < 2)) return(NA_real_)
  z <- abs(x - ave(x, g, FUN = median))
  r <- bt_capture(summary(aov(z ~ g))[[1]][["Pr(>F)"]][1])
  if (is.null(r$value) || !is.finite(r$value)) NA_real_ else r$value
}

bt_normality <- function(x, g, variable, alpha) {
  do.call(rbind, lapply(levels(g), function(gl) {
    a <- x[!is.na(g) & g == gl & !is.na(x)]
    status <- if (length(a) < 3) "too_few" else if (length(a) > 5000) "too_many" else
      if (length(unique(a)) < 2) "constant" else "ok"
    p <- if (status == "ok") shapiro.test(a)$p.value else NA_real_
    data.frame(variable = variable, group = gl, n = length(a), shapiro_p = p,
               screening = if (is.na(p)) status else if (p < alpha) "departure_flag" else "no_flag",
               stringsAsFactors = FALSE)
  }))
}

# A display recommendation, independent of the chosen hypothesis test and of
# variance screening. All groups share one display format for each variable.
bt_resolve_summary <- function(requested, diag, alpha) {
  if (requested != "auto") return(list(resolved = requested, status = "manual", reason = "manual"))
  if (any(diag$shapiro_p < alpha, na.rm = TRUE))
    return(list(resolved = "median_iqr", status = "recommended", reason = "group_departure"))
  if (anyNA(diag$shapiro_p) || !nrow(diag))
    return(list(resolved = "median_iqr", status = "needs_review", reason = "diagnostics_incomplete"))
  list(resolved = "mean_sd", status = "recommended", reason = "groups_no_flag")
}

bt_category_method <- function(x, g) {
  tb <- table(droplevels(factor(x)), droplevels(g))
  if (any(dim(tb) < 2)) return("chisq")
  e <- outer(rowSums(tb), colSums(tb)) / sum(tb)
  if (any(e < 5)) "fisher" else "chisq"
}

bt_with_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed); force(expr)
}

bt_test <- function(x, g, method, fisher_B = 100000L, seed = 20261018L) {
  ok <- !is.na(x) & !is.na(g); x <- x[ok]; g <- droplevels(g[ok])
  if (method %in% c("chisq", "chisq_nc", "fisher", "fisher_mc")) {
    tb <- table(droplevels(factor(x)), g)
    if (method == "fisher_mc") return(bt_with_seed(seed,
      fisher.test(tb, simulate.p.value = TRUE, B = fisher_B)))
    return(if (method == "fisher") fisher.test(tb) else
      chisq.test(tb, correct = method == "chisq"))
  }
  switch(method,
    anova = oneway.test(x ~ g, var.equal = TRUE),
    welch = oneway.test(x ~ g, var.equal = FALSE),
    student_t = t.test(x ~ g, var.equal = TRUE),
    welch_t = t.test(x ~ g, var.equal = FALSE),
    kruskal = kruskal.test(x ~ g),
    wilcox = wilcox.test(x ~ g, exact = FALSE, correct = TRUE),
    stop("Unknown method: ", method))
}

bt_method_name <- function(method) {
  labels <- c(anova = "One-way ANOVA (equal variance)", welch = "Welch ANOVA",
              student_t = "Student t-test (equal variance)", welch_t = "Welch t-test",
              kruskal = "Kruskal-Wallis rank-sum test",
              wilcox = "Wilcoxon rank-sum (asymptotic, continuity corrected)",
              chisq = "Pearson chi-square (Yates correction for 2x2)",
              chisq_nc = "Pearson chi-square (no continuity correction)",
              fisher = "Fisher exact test", fisher_mc = "Fisher conditional Monte Carlo test",
              none = "Not requested")
  unname(labels[method])
}

# Contract: explicit grouping and variable confirmations; independent observations.
# mode="descriptive" by default. "comparison" enables exploratory testing.
# normality_policy="review" never makes a Shapiro-only automatic decision.
# "screen" enables a documented heuristic, with all per-group diagnostics saved.
baseline_analyze <- function(data, group, config = NULL,
                             mode = c("descriptive", "comparison"),
                             normality_policy = c("review", "screen"),
                             independent = FALSE, alpha = 0.05,
                             posthoc = FALSE, p_adjust = "holm",
                             missing_codes = character(), fisher_B = 100000L,
                             seed = 20261018L) {
  mode <- match.arg(mode); normality_policy <- match.arg(normality_policy)
  bt_check_names(data)
  if (length(group) != 1 || !group %in% names(data)) stop("Confirm one group column")
  if (!is.logical(independent) || length(independent) != 1 || is.na(independent))
    stop("independent must be TRUE or FALSE")
  if (mode == "comparison" && !isTRUE(independent))
    stop("Confirm independent groups with independent=TRUE; paired/repeated/clustered data are unsupported")
  if (!p_adjust %in% p.adjust.methods) stop("Unknown p_adjust; BH is case-sensitive")
  if (length(fisher_B) != 1 || is.na(fisher_B) || fisher_B < 1000 ||
      fisher_B > .Machine$integer.max || fisher_B != floor(fisher_B)) stop("Invalid fisher_B")
  if (length(seed) != 1 || is.na(seed) || seed < 0 || seed > .Machine$integer.max ||
      seed != floor(seed)) stop("Invalid seed")
  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1)
    stop("alpha must be between 0 and 1")
  original_n <- nrow(data); duplicate_rows <- sum(duplicated(data))
  import_notes <- attr(data, "import_notes")
  # Missing conversion is explicit; original object is not modified.
  data[] <- lapply(data, function(x) {
    if (is.character(x) || is.factor(x)) {
      x <- as.character(x); x[trimws(x) == ""] <- NA_character_
    }
    if (length(missing_codes)) x[!is.na(x) & as.character(x) %in% missing_codes] <- NA
    x
  })
  missing_group <- sum(is.na(data[[group]]))
  data <- data[!is.na(data[[group]]), , drop = FALSE]
  groups <- bt_levels(data[[group]])
  if (length(groups) < 2) stop("At least two observed groups are required")
  g <- factor(as.character(data[[group]]), levels = groups)
  if (is.null(config)) config <- bt_suggest(data, group)
  needed <- c("variable", "type", "confirmed")
  if (!all(needed %in% names(config)) || !nrow(config)) stop("Config needs variable, type, confirmed")
  if (anyNA(config$variable) || anyDuplicated(config$variable) ||
      any(!config$variable %in% setdiff(names(data), group))) stop("Invalid config variables")
  config$confirmed <- as.logical(config$confirmed)
  if (anyNA(config$confirmed) || !all(config$confirmed))
    stop("Variable types must be confirmed. Use bt_suggest() to prepare a config first.")
  defaults <- list(label = config$variable, method = "auto", summary = "auto",
                   digits = 1L, levels = "")
  for (nm in names(defaults)) if (!nm %in% names(config)) config[[nm]] <- defaults[[nm]]
  config$levels_entered <- config$levels
  if (anyNA(config$type) || any(!config$type %in% c("continuous", "categorical", "ordinal")))
    stop("type must be continuous, categorical or ordinal")
  if (anyNA(config$summary) || any(!config$summary %in% c("auto", "both", "mean_sd", "median_iqr")))
    stop("summary must be auto, both, mean_sd or median_iqr")
  config$summary_resolved <- config$summary_reason <- config$summary_status <- ""
  if (anyNA(config$digits) || any(config$digits < 0 | config$digits > 8 | config$digits != floor(config$digits)))
    stop("digits must be integers from 0 to 8")
  methods <- c("auto", "none", "anova", "welch", "student_t", "welch_t", "kruskal", "wilcox",
               "chisq", "chisq_nc", "fisher", "fisher_mc")
  if (anyNA(config$method) || any(!config$method %in% methods)) stop("Invalid config method")
  if (anyNA(config$label) || any(!nzchar(config$label))) stop("Labels cannot be empty")
  tables <- list(); summaries <- list(); logs <- list(); normals <- list(); pairs <- list()
  for (i in seq_len(nrow(config))) {
    m <- config[i, , drop = FALSE]; v <- m$variable; x <- data[[v]]
    allowed <- switch(m$type,
      continuous = c("auto", "none", "anova", "welch", "student_t", "welch_t", "kruskal", "wilcox"),
      categorical = c("auto", "none", "chisq", "chisq_nc", "fisher", "fisher_mc"),
      ordinal = c("auto", "none", "kruskal", "wilcox", "chisq", "chisq_nc", "fisher", "fisher_mc"))
    if (!m$method %in% allowed) stop("Method incompatible with type for ", v)
    if (m$method %in% c("student_t", "welch_t", "wilcox") && length(groups) != 2)
      stop("Two-group method requested for ", length(groups), " groups: ", v)
    if (m$type == "continuous") {
      converted <- suppressWarnings(as.numeric(as.character(x)))
      if (any(!is.na(x) & is.na(converted)) || any(!is.finite(converted[!is.na(converted)])))
        stop("Invalid numeric value in '", v, "'; no silent conversion to missing")
      x <- converted
    } else {
      observed <- unique(as.character(x[!is.na(x)]))
      lv <- if (m$type == "ordinal" || (!is.na(m$levels) && nzchar(m$levels)))
        bt_resolve_levels(m$levels, observed, v) else bt_levels(x)
      if (m$type == "ordinal" || (!is.na(m$levels) && nzchar(m$levels)))
        config$levels[i] <- paste(lv, collapse = "|")
      x <- factor(as.character(x), levels = lv, ordered = m$type == "ordinal")
    }
    n_by <- vapply(groups, function(gl) sum(!is.na(x[g == gl])), integer(1))
    missing_by <- as.integer(table(g)) - n_by
    diag <- NULL; variance_p <- NA_real_; reason <- ""; resolved <- "none"
    status <- if (mode == "descriptive" || m$method == "none") "not_requested" else "pending"
    if (m$type == "continuous") {
      diag <- bt_normality(x, g, v, alpha); normals[[length(normals) + 1]] <- diag
      variance_p <- bt_variance_p(x, g)
    }
    display <- if (m$type == "continuous") bt_resolve_summary(m$summary, diag, alpha) else
      list(resolved = "n_pct", status = "not_applicable", reason = "not_continuous")
    config$summary_resolved[i] <- display$resolved
    config$summary_reason[i] <- display$reason
    config$summary_status[i] <- display$status
    if (status == "pending") {
      if (any(n_by == 0)) {
        status <- "insufficient_coverage"; reason <- "At least one group has no observations; no full-group test"
      } else if (length(unique(x[!is.na(x)])) < 2) {
        status <- "constant"; reason <- "Constant observed variable; retained descriptively"
      } else {
        resolved <- m$method
        if (resolved == "auto") {
          if (m$type == "categorical") resolved <- bt_category_method(x, g)
          if (m$type == "ordinal") resolved <- if (length(groups) == 2) "wilcox" else "kruskal"
          if (m$type == "continuous") {
            if (normality_policy == "review") {
              status <- "needs_review"; reason <- "Choose method after reviewing distribution, variance and study design"
              resolved <- "none"
            } else if (anyNA(diag$shapiro_p) || is.na(variance_p)) {
              status <- "needs_review"; reason <- "Incomplete normality/variance diagnostics; no automatic test"
              resolved <- "none"
            } else {
              near_normal <- all(diag$shapiro_p >= alpha)
              resolved <- if (!near_normal) {
                if (length(groups) == 2) "wilcox" else "kruskal"
              } else if (variance_p < alpha) {
                if (length(groups) == 2) "welch_t" else "welch"
              } else if (length(groups) == 2) "student_t" else "anova"
              reason <- "Exploratory screening heuristic; Shapiro non-rejection does not establish normality"
            }
          }
        } else reason <- "Explicit method override; assumptions require user review"
        # Validate type/method and group-count compatibility, even for overrides.
        allowed <- switch(m$type,
          continuous = c("none", "anova", "welch", "student_t", "welch_t", "kruskal", "wilcox"),
          categorical = c("none", "chisq", "chisq_nc", "fisher", "fisher_mc"),
          ordinal = c("none", "kruskal", "wilcox", "chisq", "chisq_nc", "fisher", "fisher_mc"))
        if (!resolved %in% allowed) stop("Method incompatible with type for ", v)
        if (resolved %in% c("student_t", "welch_t", "wilcox") && length(groups) != 2)
          stop("Two-group method requested for ", length(groups), " groups: ", v)
      }
    }
    # Ordinal scores preserve order without assuming equal grade spacing.
    tx <- if (m$type == "ordinal" && resolved %in% c("kruskal", "wilcox")) as.numeric(x) else x
    p <- statistic <- NA_real_; actual <- "Not requested"; warn <- ""; error <- ""
    if (status == "pending" && resolved != "none") {
      fit <- bt_capture(bt_test(tx, g, resolved, fisher_B, seed)); warn <- fit$warnings; error <- fit$error
      if (is.null(fit$value) || !is.finite(fit$value$p.value)) {
        status <- "failed"; actual <- "Not completed"; if (!nzchar(error)) error <- "Nonfinite p-value"
      } else {
        p <- unname(fit$value$p.value)
        if (length(fit$value$statistic)) statistic <- unname(fit$value$statistic[1])
        actual <- bt_method_name(resolved)
        if (resolved == "fisher_mc" && !grepl("simulated", fit$value$method, ignore.case = TRUE))
          actual <- "Fisher exact test (2x2; simulation not used)"
        status <- if (nzchar(warn)) "warning" else "ok"
      }
    }
    # Summaries are independent of the testing decision. No variables dropped.
    block <- list()
    addrow <- function(level, display, nn = NULL, miss = NULL) {
      row <- data.frame(variable = v, label = m$label, level = level, stringsAsFactors = FALSE)
      for (j in seq_along(display)) row[[paste0("G", j - 1L)]] <- display[j]
      row$p_value <- if (length(block) == 0) bt_pformat(p) else ""
      row$status <- if (length(block) == 0) status else ""
      block[[length(block) + 1L]] <<- row
    }
    scopes <- c("Overall", groups)
    scope_x <- c(list(x), lapply(groups, function(gl) x[g == gl]))
    fmt <- function(z) ifelse(is.na(z), "NA", formatC(z, format = "f", digits = m$digits))
    if (m$type == "continuous") {
      nums <- lapply(seq_along(scopes), function(j) {
        a <- scope_x[[j]]; b <- a[!is.na(a)]
        q <- if (length(b)) quantile(b, c(.25, .5, .75), names = FALSE) else rep(NA_real_, 3)
        data.frame(variable = v, scope = scopes[j], n = length(b), missing = sum(is.na(a)),
                   mean = if (length(b)) mean(b) else NA_real_,
                   sd = if (length(b) > 1) sd(b) else NA_real_,
                   p25 = q[1], median = q[2], p75 = q[3], stringsAsFactors = FALSE)
      })
      summaries[[length(summaries) + 1L]] <- do.call(rbind, nums)
      if (display$resolved %in% c("both", "mean_sd")) addrow("Mean (SD)", vapply(nums, function(a)
        if (!a$n) "No observations" else paste0(fmt(a$mean), " (", fmt(a$sd), ")"), character(1)), NULL, NULL)
      if (display$resolved %in% c("both", "median_iqr")) addrow("Median (Q1, Q3)", vapply(nums, function(a)
        if (!a$n) "No observations" else paste0(fmt(a$median), " (", fmt(a$p25), ", ", fmt(a$p75), ")"), character(1)), NULL, NULL)
    } else {
      lv <- levels(x)
      if (!length(lv)) addrow("No observed levels", rep("No observations", length(scopes)), NULL, NULL)
      for (lev in lv) addrow(lev, vapply(scope_x, function(a) {
        n <- sum(!is.na(a)); k <- sum(a == lev, na.rm = TRUE)
        if (!n) "No observations" else sprintf(paste0("%d (%.", m$digits, "f%%)"), k, 100 * k / n)
      }, character(1)), NULL, NULL)
    }
    addrow("Nonmissing n", vapply(scope_x, function(a) as.character(sum(!is.na(a))), character(1)), NULL, NULL)
    addrow("Missing n", vapply(scope_x, function(a) as.character(sum(is.na(a))), character(1)), NULL, NULL)
    tables[[length(tables) + 1L]] <- do.call(rbind, block)
    logs[[length(logs) + 1L]] <- data.frame(variable = v, type = m$type, requested_method = m$method,
      requested_summary = m$summary, resolved_summary = display$resolved,
      summary_status = display$status, summary_reason = display$reason,
      resolved_method = resolved, test = actual, status = status, n = sum(!is.na(x)),
      groups_with_data = sum(n_by > 0), n_by_group = paste(paste(groups, n_by, sep = ":"), collapse = ";"),
      missing_by_group = paste(paste(groups, missing_by, sep = ":"), collapse = ";"),
      variance_p = variance_p, statistic = statistic, p_value = p,
      reason = reason, warnings = warn, error = error, stringsAsFactors = FALSE)
    # Explicit opt-in: all within-variable pairs; raw p first, then one adjustment.
    # Pairwise Welch avoids the old pooled-SD error following Welch ANOVA.
    if (isTRUE(posthoc) && length(groups) > 2 && status %in% c("ok", "warning")) {
      pair_list <- lapply(combn(groups, 2, simplify = FALSE), function(gl) {
        keep <- g %in% gl; pg <- droplevels(g[keep]); px <- tx[keep]
        pm <- if (resolved %in% c("anova", "welch")) "welch_t" else
          if (resolved == "kruskal") "wilcox" else bt_category_method(px, pg)
        r <- bt_capture(bt_test(px, pg, pm, fisher_B, seed))
        pv <- if (is.null(r$value)) NA_real_ else unname(r$value$p.value)
        data.frame(variable = v, group1 = gl[1], group2 = gl[2], test = bt_method_name(pm),
                   p_raw = pv, warnings = r$warnings, error = r$error, stringsAsFactors = FALSE)
      })
      pr <- do.call(rbind, pair_list)
      pr$p_adjusted <- p.adjust(pr$p_raw, method = p_adjust)
      pr$adjustment <- p_adjust; pr$family_size <- nrow(pr)
      pr$status <- ifelse(!is.finite(pr$p_raw), "failed", ifelse(nzchar(pr$warnings), "warning", "ok"))
      pairs[[length(pairs) + 1L]] <- pr
    }
  }
  bind <- function(z) if (length(z)) do.call(rbind, z) else data.frame()
  methods_log <- bind(logs)
  posthoc_log <- bind(pairs)
  all_status <- c(methods_log$status, posthoc_log$status)
  overall_status <- if (any(all_status %in% c("failed", "needs_review", "insufficient_coverage")))
    "partial" else if (any(all_status == "warning")) "warning" else "complete"
  result <- list(table = bind(tables), methods = methods_log, normality = bind(normals),
    continuous_summary = bind(summaries), posthoc = posthoc_log, config = config,
    groups = data.frame(column = paste0("G", seq_along(groups)), group = groups,
                        n = as.integer(table(g)), stringsAsFactors = FALSE),
    metadata = list(engine_version = BASELINE_ENGINE_VERSION, R_version = R.version.string,
      mode = mode, normality_policy = normality_policy, independent_confirmed = independent,
      summary_rule = "Auto: any group Shapiro P<alpha -> median (Q1,Q3); all groups P>=alpha -> mean (SD); otherwise median provisionally, review required. Variance screening does not choose display.",
      status = overall_status, input_records = original_n, analyzed_records = nrow(data),
      excluded_missing_group = missing_group, exact_duplicate_rows_flagged = duplicate_rows,
      import_notes = import_notes,
      posthoc = posthoc, p_adjust = p_adjust, alpha = alpha,
      missing_codes = missing_codes, fisher_B = fisher_B, random_seed = seed,
      rng_kind = paste(RNGkind(), collapse = ";"),
      dependency_versions = paste(vapply(c("readxl", "writexl", "jsonlite", "flextable", "officer"),
        function(pkg) paste0(pkg, "=", if (requireNamespace(pkg, quietly = TRUE))
          as.character(utils::packageVersion(pkg)) else "not installed"), character(1)), collapse = ";"),
      categorical_denominator = "nonmissing within each scope",
      missing_handling = "Available observations per variable; no imputation; no automatic deduplication",
      omnibus_multiplicity = "Unadjusted exploratory per-variable p-values",
      created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)))
  result$session_info <- capture.output(sessionInfo())
  class(result) <- c("baseline_result", "list")
  result
}

baseline_export <- function(result, outdir, prefix = "Table1", word = FALSE) {
  stopifnot(inherits(result, "baseline_result"))
  if (length(prefix) != 1 || !grepl("^[A-Za-z0-9_-]+$", prefix)) stop("Unsafe output prefix")
  bt_require("writexl"); bt_require("jsonlite")
  if (isTRUE(word)) { bt_require("flextable"); bt_require("officer") }
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  tab <- result$table
  names(tab)[names(tab) == "G0"] <- paste0("Overall (N=", result$metadata$analyzed_records, ")")
  for (i in seq_len(nrow(result$groups))) {
    names(tab)[names(tab) == result$groups$column[i]] <-
      paste0("Group ", result$groups$group[i], " (N=", result$groups$n[i], ")")
  }
  path <- function(ext) file.path(outdir, paste0(prefix, ext))
  parts <- list(Table1 = tab, Methods_log = result$methods, Config = result$config,
                Groups = result$groups)
  if (nrow(result$normality)) parts$Diagnostics <- result$normality
  if (nrow(result$continuous_summary)) parts$Continuous_numeric <- result$continuous_summary
  if (nrow(result$posthoc)) parts$Posthoc <- result$posthoc
  metadata_text <- vapply(result$metadata, function(x) paste(x, collapse = ";"), character(1))
  parts$Run <- data.frame(key = names(metadata_text), value = unname(metadata_text))
  writexl::write_xlsx(parts, path(".xlsx"))
  jsonlite::write_json(unclass(result), path("_result.json"), pretty = TRUE,
                       auto_unbox = TRUE, na = "null", digits = NA)
  writeLines(result$session_info, path("_session.txt"))
  note <- c("Percentages use nonmissing observations within each group.",
    "Continuous rows explicitly state mean (SD) or median (Q1, Q3). No imputation.",
    "All variable-level P values are exploratory and unadjusted across variables.",
    "Inspect Methods_log for failed, incomplete or review-required analyses.",
    "Screening decisions are heuristic; inspect group distributions/residuals and study design.")
  auto_summary <- result$methods$requested_summary == "auto" & result$methods$type == "continuous"
  if (any(auto_summary)) note <- c(note,
    "Auto display: any group Shapiro P<alpha -> median (Q1,Q3); all groups P>=alpha -> mean (SD). Variance screening does not choose display.")
  review_summary <- result$methods$variable[result$methods$summary_status == "needs_review"]
  if (length(review_summary)) note <- c(note, paste0(
    "Incomplete distribution diagnostics; provisional median (Q1,Q3), review needed: ",
    paste(review_summary, collapse = "; "), ". See Methods_log / Config."))
  if (isTRUE(word)) {
    show <- tab[, setdiff(names(tab), "variable"), drop = FALSE]
    display_status <- c(ok = "OK", warning = "Warning", failed = "Failed",
      not_requested = "Not requested", needs_review = "Review needed",
      insufficient_coverage = "Missing groups", constant = "Constant")
    show$status <- ifelse(show$status == "", "", unname(display_status[show$status]))
    ft <- flextable::flextable(show)
    n <- ncol(show); total <- 10.59; first <- 2.1; second <- 1.3
    ft <- flextable::set_table_properties(ft, layout = "fixed")
    ft <- flextable::width(ft, j = 1, width = first)
    ft <- flextable::width(ft, j = 2, width = second)
    # Reserve adequate space for the explicit status; distribute data columns.
    ft <- flextable::width(ft, j = 3:(n-1), width = (total - first - second - 1.25)/(n - 3))
    ft <- flextable::width(ft, j = n, width = 1.25)
    ft <- flextable::fontsize(ft, size = 8, part = "all")
    ft <- flextable::padding(ft, padding = 2, part = "all")
    ft <- flextable::line_spacing(ft, space = 1, part = "all")
    ft <- flextable::add_footer_lines(ft, values = note)
    ft <- flextable::fontsize(ft, size = 8, part = "footer")
    ft <- flextable::padding(ft, padding = 1, part = "footer")
    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, "Baseline characteristics", style = "Normal")
    doc <- officer::body_add_par(doc, paste("Run status:", result$metadata$status), style = "Normal")
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_set_default_section(doc,
      officer::prop_section(page_size = officer::page_size(orient = "landscape"),
        page_margins = officer::page_mar(top = .6, bottom = .6, left = .55, right = .55)))
    print(doc, target = path(".docx"))
  }
  writeLines(note, path("_notes.txt"))
  invisible(normalizePath(outdir))
}
