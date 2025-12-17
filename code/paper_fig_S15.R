############################################################
## GLMa (count, truncated Poisson) diagnostics – high-res PNG
## A: importance via % change in deviance (LRT, drop1-style)
## B–E: main effects for top 4 numeric predictors
## Y-axis = Expected count per trap (given presence)
## With 95% confidence bands (link-scale SE from glmmTMB)
############################################################

## First run 02_abund_models.R !

## Open device
png(file.path(dir_plots, "abund_model_diagnostics_GLMa.png"),
    width  = 2000,
    height = 3000,
    res    = 300)

## VARIABLE IMPORTANCE VIA "DROP1"-STYLE LRT -------------------------
## For glmmTMB there is no drop1(), so we:
##  - take the full model logLik
##  - for each term, fit a reduced model without that term
##  - compute LRT = 2 * (logLik_full - logLik_reduced)
##  - relative importance = 100 * LRT / sum(LRT)

full_logLik <- logLik(GLMa)

# all fixed-effect terms in the model
terms_full <- attr(terms(GLMa), "term.labels")

get_drop1_row <- function(term) {
  # Build reduced formula: remove this term from the RHS
  form_full <- formula(GLMa)
  rhs_terms <- terms_full[terms_full != term]

  # if we removed everything, skip
  if (length(rhs_terms) == 0L) return(NULL)

  form_reduced <- as.formula(
    paste("count ~", paste(rhs_terms, collapse = " + "))
  )

  # fit reduced model
  mod_red <- update(GLMa, formula = form_reduced)

  ll_red  <- logLik(mod_red)

  LRT <- 2 * (as.numeric(full_logLik) - as.numeric(ll_red))
  df  <- attr(full_logLik, "df") - attr(ll_red, "df")
  pval <- pchisq(LRT, df = df, lower.tail = FALSE)

  data.frame(
    variable  = term,
    Df        = df,
    LRT       = LRT,
    Pr_Chi    = pval,
    row.names = NULL
  )
}

drop_list <- lapply(terms_full, get_drop1_row)
drop_df   <- do.call(rbind, drop_list)

# Remove any weird NA rows
drop_df <- drop_df[!is.na(drop_df$LRT), , drop = FALSE]

# Order and rescale to %Δdeviance
vi_glm <- drop_df[order(drop_df$LRT, decreasing = TRUE), ]
vi_glm$rel_inf <- 100 * vi_glm$LRT / sum(vi_glm$LRT)


## LAYOUT ------------------------------------------------------------
#   [1 1]
#   [2 3]
#   [4 5]
mat <- matrix(c(1, 1,
                2, 3,
                4, 5),
              nrow = 3, byrow = TRUE)
layout(mat, heights = c(1.6, 1, 1))
par(oma = c(0, 4, 0, 0))  # outer margins (for shared y label)


## PANEL A – IMPORTANCE BARPLOT -------------------------------------
par(mar = c(5, 10, 4, 2))

cols <- colorRampPalette(c("#0000A0", "#00BFFF"))(nrow(vi_glm))

barplot(
  height    = rev(vi_glm$rel_inf),
  names.arg = rev(vi_glm$variable),
  horiz     = TRUE,
  las       = 1,
  col       = rev(cols),
  xlab      = "Relative importance (%)",
  main      = "A.",
  xlim      = c(0, max(vi_glm$rel_inf) * 1.1),
  cex.names = 1.4,
  cex.axis  = 1.3,
  cex.lab   = 1.5,
  cex.main  = 1.6
)

## PANELS B–E – MAIN EFFECTS WITH 95% CI -----------------------------
par(mar = c(4.5, 3, 3.5, 1))

# numeric predictors in the positive-count data
numeric_vars <- names(ab_data_pos)[sapply(ab_data_pos, is.numeric)]

# don't treat response or coords/IDs as predictors
numeric_vars <- setdiff(numeric_vars, c("count", "ID", "lon", "lat"))

# only keep numeric vars that appear in the importance table
vi_glm_num <- vi_glm[vi_glm$variable %in% numeric_vars, , drop = FALSE]
vi_glm_num <- vi_glm_num[order(vi_glm_num$rel_inf, decreasing = TRUE), ]

n_panels <- min(4, nrow(vi_glm_num))
top_vars <- vi_glm_num$variable[1:n_panels]
top_inf  <- vi_glm_num$rel_inf[1:n_panels]
panel_letters <- c("B", "C", "D", "E")[seq_len(n_panels)]

## Build a "typical" covariate profile for other predictors -------------
# mean for numeric, most common level for factors/characters

base_prof <- ab_data_pos[1, , drop = FALSE]  # template row
for (nm in names(base_prof)) {
  if (nm == "count") next
  if (is.numeric(ab_data_pos[[nm]])) {
    base_prof[[nm]] <- mean(ab_data_pos[[nm]], na.rm = TRUE)
  } else if (is.factor(ab_data_pos[[nm]]) || is.character(ab_data_pos[[nm]])) {
    tab <- table(ab_data_pos[[nm]])
    base_prof[[nm]] <- names(tab)[which.max(tab)]
  }
}

## Helper: PD + 95% CI on expected count scale --------------------------

compute_pd_glma_ci <- function(var_name, grid.size = 50) {
  x_vals <- seq(
    min(ab_data_pos[[var_name]], na.rm = TRUE),
    max(ab_data_pos[[var_name]], na.rm = TRUE),
    length.out = grid.size
  )

  # newdata: only var_name varies, others fixed at base_prof
  newdat <- base_prof[rep(1, grid.size), , drop = FALSE]
  newdat[[var_name]] <- x_vals

  # predict on link scale with SE
  pr <- predict(GLMa, newdata = newdat, type = "link", se.fit = TRUE)
  eta  <- pr$fit
  se   <- pr$se.fit

  # 95% CI on link scale
  eta_lo <- eta - 1.96 * se
  eta_hi <- eta + 1.96 * se

  # transform to expected count scale (log link)
  mu_hat <- exp(eta)
  mu_lo  <- exp(eta_lo)
  mu_hi  <- exp(eta_hi)

  data.frame(
    x    = x_vals,
    mu   = mu_hat,
    mu_lo = mu_lo,
    mu_hi = mu_hi
  )
}

plot_pd_panel_glma_ci <- function(var_name, rel_inf, panel_letter) {
  pd <- compute_pd_glma_ci(var_name, grid.size = 60)
  x    <- pd$x
  mu   <- pd$mu
  mu_lo <- pd$mu_lo
  mu_hi <- pd$mu_hi

  ylim <- range(c(mu_lo, mu_hi), na.rm = TRUE)

  # empty plot first
  plot(
    x, mu,
    type = "n",
    xlab = paste0(var_name, " (", round(rel_inf, 1), "%)"),
    ylab = "",
    main = paste0(panel_letter, "."),
    ylim = ylim,
    cex.axis = 1.2,
    cex.lab  = 1.4,
    cex.main = 1.5
  )

  # confidence band (shaded)
  polygon(
    x = c(x, rev(x)),
    y = c(mu_lo, rev(mu_hi)),
    border = NA,
    col = adjustcolor("grey80", alpha.f = 0.5)
  )

  # main effect line
  lines(x, mu, lwd = 1.5)

  # rug of observed data
  rug(ab_data_pos[[var_name]])
}

for (i in seq_along(top_vars)) {
  plot_pd_panel_glma_ci(top_vars[i], top_inf[i], panel_letters[i])
}

# shared y-axis label
mtext("Predicted trap counts",
      side  = 2,
      outer = TRUE,
      line  = 1.2,
      cex   = 1,
      adj   = 0.25)

## Close device
dev.off()
