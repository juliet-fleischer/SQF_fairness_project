# 1. tables ----
writexl::write_xlsx(fairness_audit_rf$fairness_metrics, path = "figures/sqf_case_study_table1.xlsx")
latex_tab1 <- xtable::xtable(res_all, caption = "Groupwise Fairness Metrics (2023)", label = "tab:groupwise_metrics_2023")
print(latex_tab1, file = "figures/groupwise_metrics_2023.tex", include.rownames = FALSE)
# \input{groupwise_metrics_2023.tex}

# 2. plots ----
ggsave("figures/sqf_case_study_plot1.png", p1_rf, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot2.png", p2_rf, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot3.png", p3, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot4.png", p4, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot5.png", p5, width = 10, height = 6, units = "in", dpi = 300, bg = "white")

# unconditional race distirbution in target vs train data
ggsave("figures/sqf_case_study_plot6.png", p8, width = 10, height = 6, units = "in", dpi = 300, bg = "white") # race distribution
ggsave("figures/sqf_case_study_plot7.png", p1_combined, width = 10, height = 6, units = "in", dpi = 300, bg = "white") # score densities
ggsave("figures/sqf_case_study_plot8.png", p2_combined, width = 10, height = 6, units = "in", dpi = 300, bg = "white") # metrics comparison
# arrestemtn rates
ggsave("figures/sqf_case_study_plot9.png", p13, width = 10, height = 6, units = "in", dpi = 300, bg = "white") # arrestment rates per race
ggsave("figures/sqf_case_study_plot10.png", p15, width = 10, height = 6, units = "in", dpi = 300, bg = "white") # frisk rates)
# training and target geopraphical distribution
ggsave("figures/sqf_case_study_plot11.png", p45, width = 10, height = 6, units = "in", dpi = 300, bg = "white")

# crime rates by borough
ggsave("figures/sqf_case_study_plot12.png", p16, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot13.png", p17, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot14.png", p19, width = 10, height = 6, units = "in", dpi = 300, bg = "white")

# 3. models and data ----
saveRDS(complete_cases, file = "data/data2023.rds")
saveRDS(complete_cases_2011, file = "data/data2011.rds")
saveRDS(lrn_rf_2011, file = "program/trained_rf_2011.rds")
saveRDS(lrn_rf_2023, file = "program/trained_rf_2023.rds")
saveRDS(bmr, "program/bmr_results.rds")



