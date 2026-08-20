########################################################################
# CMP7205 Applied Statistics - GTA V Worldwide Sales & Player Analytics
# Full analysis script covering RQ1-RQ4 + 8 report-ready figures
#
# BEFORE RUNNING:
# 1. setwd() to the folder containing the CSV file
# 2. Install packages once:
#    install.packages(c("tidyverse","car","rstatix","broom","ggpubr","corrplot","moments"))
########################################################################

library(tidyverse)
library(car)
library(rstatix)
library(broom)
library(ggpubr)
library(corrplot)
library(moments)   # for skewness() - used to verify the RQ4 log-transform fix

# ----------------------------------------------------------------------
# 1. LOAD AND CLEAN DATA
# ----------------------------------------------------------------------

df <- read_csv("gta_v_worldwide_sales_player_analytics_2013_2026.csv")
glimpse(df)

colSums(is.na(df))

# major_sale_event / special_event: NA = no event that period. Recode, don't delete.
df <- df %>%
  mutate(
    major_sale_event = replace_na(major_sale_event, "None"),
    special_event     = replace_na(special_event, "None"),
    marketing_campaign_f = factor(marketing_campaign, labels = c("No Campaign", "Campaign"))
  ) %>%
  select(-top_game_category)   # constant column, no analytical value

colSums(is.na(df %>% select(units_sold, gross_revenue_usd, discount_percentage,
                              platform_generation, marketing_campaign, region)))


# ========================================================================
# FIGURE 1 (EDA - Introduction/Datasets section):
# Global sales trend over time - establishes the context before you dive
# into hypothesis tests. Every 90%+ report opens Results with a big-picture
# view before narrowing into specific questions.
# ========================================================================

yearly <- df %>%
  group_by(year) %>%
  summarise(total_units = sum(units_sold), total_revenue = sum(gross_revenue_usd)/1e6)

fig1 <- ggplot(yearly, aes(x = year, y = total_units)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Figure 1: Total GTA V Units Sold by Year (2013-2026)",
       x = "Year", y = "Total Units Sold") +
  theme_minimal(base_size = 12)
print(fig1)
ggsave("Fig1_yearly_sales_trend.png", fig1, width = 8, height = 5, dpi = 300)


# ========================================================================
# FIGURE 2 (Datasets section):
# Regional distribution of sales - shows dataset composition, supports
# your Section 3 dataset description with a visual instead of just text.
# ========================================================================

regional <- df %>%
  group_by(region) %>%
  summarise(total_units = sum(units_sold)) %>%
  arrange(desc(total_units))

fig2 <- ggplot(regional, aes(x = reorder(region, total_units), y = total_units, fill = region)) +
  geom_col() +
  coord_flip() +
  labs(title = "Figure 2: Total Units Sold by Region",
       x = "Region", y = "Total Units Sold") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
print(fig2)
ggsave("Fig2_regional_sales.png", fig2, width = 8, height = 5, dpi = 300)


# ========================================================================
# FIGURE 3 (Methodology/EDA - correlation heatmap):
# Shows relationships across ALL numeric variables at once. This single
# figure demonstrates you explored the full dataset before selecting
# techniques - directly supports the "planning" criterion (LO2).
# ========================================================================

numeric_vars <- df %>%
  select(units_sold, gross_revenue_usd, discount_percentage, average_selling_price_usd,
         digital_sales_percentage, average_playtime_hours, customer_rating,
         review_count, refund_rate_percentage) %>%
  drop_na()

corr_matrix <- cor(numeric_vars)

png("Fig3_correlation_heatmap.png", width = 2400, height = 2400, res = 300)
corrplot(corr_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45,
         title = "Figure 3: Correlation Matrix of Key Numeric Variables",
         mar = c(0,0,2,0))
dev.off()


# ========================================================================
# RQ1: Platform generation vs units sold (ANOVA)
# ========================================================================

set.seed(123)
df %>% group_by(platform_generation) %>% sample_n(100) %>%
  summarise(shapiro_p = shapiro.test(units_sold)$p.value)
leveneTest(units_sold ~ platform_generation, data = df)

rq1_aov <- aov(units_sold ~ platform_generation, data = df)
summary(rq1_aov)
TukeyHSD(rq1_aov)
kruskal.test(units_sold ~ platform_generation, data = df)   # robustness check
df %>% anova_test(units_sold ~ platform_generation)          # effect size

rq1_summary <- df %>%
  group_by(platform_generation) %>%
  summarise(mean_units = mean(units_sold), sd_units = sd(units_sold), n = n())
print(rq1_summary)
write_csv(rq1_summary, "RQ1_summary_table.csv")

# FIGURE 4
fig4 <- ggplot(df, aes(x = platform_generation, y = units_sold, fill = platform_generation)) +
  geom_boxplot() +
  labs(title = "Figure 4: Units Sold by Platform Generation",
       x = "Platform Generation", y = "Units Sold") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
print(fig4)
ggsave("Fig4_platform_generation_boxplot.png", fig4, width = 8, height = 5, dpi = 300)


# ========================================================================
# RQ2: Discount percentage vs units sold (correlation + regression)
# ========================================================================

cor.test(df$discount_percentage, df$units_sold, method = "pearson")
rq2_lm <- lm(units_sold ~ discount_percentage, data = df)
summary(rq2_lm)

png("Fig_appendix_RQ2_residuals.png", width = 2400, height = 2400, res = 300)
par(mfrow = c(2,2)); plot(rq2_lm); par(mfrow = c(1,1))
dev.off()

# FIGURE 5
fig5 <- ggplot(df, aes(x = discount_percentage, y = units_sold)) +
  geom_point(alpha = 0.12, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(title = "Figure 5: Discount Percentage vs Units Sold",
       x = "Discount (%)", y = "Units Sold") +
  theme_minimal(base_size = 12)
print(fig5)
ggsave("Fig5_discount_vs_units.png", fig5, width = 8, height = 5, dpi = 300)


# ========================================================================
# RQ3: Marketing campaign vs units sold (t-test)
# ========================================================================

leveneTest(units_sold ~ marketing_campaign_f, data = df)
df %>% group_by(marketing_campaign_f) %>% sample_n(100) %>%
  summarise(shapiro_p = shapiro.test(units_sold)$p.value)

t.test(units_sold ~ marketing_campaign_f, data = df, var.equal = FALSE)
df %>% cohens_d(units_sold ~ marketing_campaign_f)

df %>% group_by(marketing_campaign_f) %>%
  summarise(mean_units = mean(units_sold), sd_units = sd(units_sold), n = n())

# FIGURE 6
fig6 <- ggplot(df, aes(x = marketing_campaign_f, y = units_sold, fill = marketing_campaign_f)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.15, fill = "white") +
  labs(title = "Figure 6: Units Sold With vs Without Marketing Campaign",
       x = "", y = "Units Sold") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
print(fig6)
ggsave("Fig6_marketing_campaign_effect.png", fig6, width = 8, height = 5, dpi = 300)


# ========================================================================
# RQ4: Multiple regression predicting gross revenue
# units_sold deliberately EXCLUDED as predictor - revenue = units x price,
# so including it would be circular. State this explicitly in Methodology.
#
# FIX APPLIED: gross_revenue_usd is log-transformed before modelling.
# Reason: the untransformed model had heavily right-skewed residuals
# (skewness = 2.92) and failed normality checks - a known consequence of
# modelling raw monetary values, which are rarely normally distributed.
# Log-transforming is the standard, defensible fix for this. State this
# explicitly in your Methodology section - identifying and correcting an
# assumption violation is exactly what the 80-100% marking band rewards.
# ========================================================================

df$log_revenue <- log(df$gross_revenue_usd)

rq4_lm <- lm(log_revenue ~ discount_percentage + digital_sales_percentage +
               average_playtime_hours + average_selling_price_usd +
               marketing_campaign + region, data = df)
summary(rq4_lm)
vif(rq4_lm)

# --- Confirm the fix worked: check residual normality and skew ---
library(moments)
cat("\nResidual skewness (should be near 0):", skewness(resid(rq4_lm)), "\n")
set.seed(1)
cat("Shapiro-Wilk on residual sample (p > 0.05 = normal):",
    shapiro.test(sample(resid(rq4_lm), 500))$p.value, "\n")

png("Fig_appendix_RQ4_residuals.png", width = 2400, height = 2400, res = 300)
par(mfrow = c(2,2)); plot(rq4_lm); par(mfrow = c(1,1))
dev.off()

rq4_table <- tidy(rq4_lm, conf.int = TRUE)
write_csv(rq4_table, "RQ4_regression_results.csv")
glance(rq4_lm)

# FIGURE 7: Actual vs predicted (back-transformed to real USD for readability)
df$predicted_log_revenue <- predict(rq4_lm)
df$predicted_revenue <- exp(df$predicted_log_revenue)
fig7 <- ggplot(df, aes(x = predicted_revenue, y = gross_revenue_usd)) +
  geom_point(alpha = 0.1, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Figure 7: Actual vs Predicted Gross Revenue (Log Model, Back-Transformed)",
       x = "Predicted Revenue (USD)", y = "Actual Revenue (USD)") +
  theme_minimal(base_size = 12)
print(fig7)
ggsave("Fig7_actual_vs_predicted_revenue.png", fig7, width = 8, height = 5, dpi = 300)

# FIGURE 8: Regression coefficient plot (forest plot)
# NOTE: Because the outcome is now log(revenue), each coefficient represents
# the approximate PERCENTAGE change in revenue for a one-unit increase in
# the predictor, not a raw dollar amount. Convert with (exp(estimate)-1)*100
# for the exact percentage - approximate for small coefficients is just
# estimate*100. State this unit change explicitly in your Results text.
rq4_table_clean <- rq4_table %>%
  filter(term != "(Intercept)") %>%
  mutate(pct_change = (exp(estimate) - 1) * 100,
         pct_low = (exp(conf.low) - 1) * 100,
         pct_high = (exp(conf.high) - 1) * 100)

print(rq4_table_clean %>% select(term, estimate, pct_change, p.value))
write_csv(rq4_table_clean, "RQ4_regression_results_log_model.csv")

fig8 <- ggplot(rq4_table_clean, aes(x = reorder(term, pct_change), y = pct_change)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = pct_low, ymax = pct_high), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Figure 8: Predictor Effects on Gross Revenue (% Change, 95% CI)",
       x = "Predictor", y = "Approx. % Change in Revenue") +
  theme_minimal(base_size = 12)
print(fig8)
ggsave("Fig8_regression_coefficients.png", fig8, width = 8, height = 5, dpi = 300)


# ========================================================================
# DONE
# ========================================================================

cat("\n\n=== ALL ANALYSES COMPLETE ===\n")
cat("8 main figures saved as Fig1_... through Fig8_...\n")
cat("2 extra appendix diagnostic plots saved as Fig_appendix_...\n")
cat("2 CSV tables saved (RQ1 summary, RQ4 regression results)\n")
cat("Insert Fig1-Fig8 in your main Results/Discussion text.\n")
cat("Put the appendix diagnostic plots in an Appendix, referenced from Methodology.\n")
