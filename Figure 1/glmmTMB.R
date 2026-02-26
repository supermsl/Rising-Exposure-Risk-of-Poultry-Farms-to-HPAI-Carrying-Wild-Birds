library(glmmTMB)
library(dplyr)
library(ggplot2)
library(patchwork)

# --------------------------------------------------
# 
# --------------------------------------------------
df <- read.csv(
  "Encounter rate and poultry outbreaks.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

response_var <- "OT2022-2023"
bird_vars <- setdiff(names(df), response_var)

# --------------------------------------------------
# 2. Tweedie
# --------------------------------------------------
models_tw <- list()

for (bird_var in bird_vars) {
  
  form_cond <- as.formula(
    paste0("`", response_var, "` ~ `", bird_var, "`")
  )
  
  model <- try(
    glmmTMB(
      formula = form_cond,
      family  = tweedie(link = "log"),
      data    = df
    ),
    silent = TRUE
  )
  
  if (!inherits(model, "try-error")) {
    models_tw[[bird_var]] <- model
  }
}

results_tw <- data.frame()

for (bird_var in names(models_tw)) {
  
  sum_m <- summary(models_tw[[bird_var]])
  coefs <- sum_m$coefficients$cond
  
  est <- coefs[, "Estimate"]
  se  <- coefs[, "Std. Error"]
  
  results_tw <- rbind(
    results_tw,
    data.frame(
      bird      = bird_var,
      term      = rownames(coefs),
      estimate  = est,
      p_value   = coefs[, "Pr(>|z|)"],
      ci_lower  = est - 1.96 * se,
      ci_upper  = est + 1.96 * se
    )
  )
}

results_tw <- results_tw %>%
  filter(term != "(Intercept)") %>%
  mutate(
    estimate_exp = exp(estimate),
    ci_lower_exp = exp(ci_lower),
    ci_upper_exp = exp(ci_upper),
    p_fdr        = p.adjust(p_value, method = "BH")
  )

top10_tw <- results_tw %>% slice(1:10)
top10_tw$bird <- factor(top10_tw$bird, levels = rev(top10_tw$bird))

p_tw <- ggplot(top10_tw, aes(x = estimate_exp, y = bird)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ci_lower_exp, xmax = ci_upper_exp), height = 0.2) +
  scale_x_log10() +
  labs(x = "Effect size (exp(β))", y = NULL, title = "A. Tweedie") +
  theme_classic(base_size = 13)

# --------------------------------------------------
# 3. ZINB
# --------------------------------------------------
models_zi <- list()

for (bird_var in bird_vars) {
  
  form_cond <- as.formula(
    paste0("`", response_var, "` ~ `", bird_var, "`")
  )
  
  form_zi <- as.formula(
    paste0("~ `", bird_var, "`")
  )
  
  model <- try(
    glmmTMB(
      formula   = form_cond,
      ziformula = form_zi,
      family    = nbinom2,
      data      = df
    ),
    silent = TRUE
  )
  
  if (!inherits(model, "try-error")) {
    models_zi[[bird_var]] <- model
  }
}

results_zi <- data.frame()

for (bird_var in names(models_zi)) {
  
  sum_m <- summary(models_zi[[bird_var]])
  
  for (comp in c("cond", "zi")) {
    
    coefs <- sum_m$coefficients[[comp]]
    est   <- coefs[, "Estimate"]
    se    <- coefs[, "Std. Error"]
    
    results_zi <- rbind(
      results_zi,
      data.frame(
        bird      = bird_var,
        component = ifelse(comp == "cond", "conditional", "zero_inflation"),
        term      = rownames(coefs),
        estimate  = est,
        p_value   = coefs[, "Pr(>|z|)"],
        ci_lower  = est - 1.96 * se,
        ci_upper  = est + 1.96 * se
      )
    )
  }
}

results_zi <- results_zi %>%
  filter(term != "(Intercept)") %>%
  mutate(
    estimate_exp = exp(estimate),
    ci_lower_exp = exp(ci_lower),
    ci_upper_exp = exp(ci_upper)
  ) %>%
  group_by(component) %>%
  mutate(p_fdr = p.adjust(p_value, method = "BH")) %>%
  ungroup()

top10_cond <- results_zi %>%
  filter(component == "conditional") %>%
  slice(1:10)

top10_zi <- results_zi %>%
  filter(component == "zero_inflation") %>%
  slice(1:10)

top10_cond$bird <- factor(top10_cond$bird, levels = rev(top10_cond$bird))
top10_zi$bird   <- factor(top10_zi$bird,   levels = rev(top10_zi$bird))

p_cond <- ggplot(top10_cond, aes(x = estimate_exp, y = bird)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ci_lower_exp, xmax = ci_upper_exp), height = 0.2) +
  scale_x_log10() +
  labs(x = "Effect size (exp(β))", y = NULL, title = "B. Conditional") +
  theme_classic(base_size = 13)

p_zi <- ggplot(top10_zi, aes(x = estimate_exp, y = bird)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ci_lower_exp, xmax = ci_upper_exp), height = 0.2) +
  scale_x_log10() +
  labs(x = "Odds ratio of structural zero (exp(β))",
       y = NULL,
       title = "C. Zero inflation") +
  theme_classic(base_size = 13)

# --------------------------------------------------
#
# --------------------------------------------------
final_figure <- p_tw / p_cond / p_zi +
  plot_layout(heights = c(1, 1, 1))

final_figure
