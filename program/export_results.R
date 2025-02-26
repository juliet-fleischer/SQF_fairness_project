# 1. tables ----
writexl::write_xlsx(fairness_audit_rf$fairness_metrics, path = "figures/sqf_case_study_table1.xlsx")
latex_tab1 <- xtable::xtable(res_all, caption = "Groupwise Fairness Metrics (2023)", label = "tab:groupwise_metrics_2023")
print(latex_tab1, file = "figures/groupwise_metrics_2023.tex", include.rownames = TRUE)
# \input{groupwise_metrics_2023.tex}

# 2. plots ----
ggsave("figures/sqf_case_study_plot1.pdf", plot = p1_rf, width = 10, height = 8, units = "in", bg = "white")
ggsave("figures/sqf_case_study_plot2.pdf", p2_rf, width = 10, height = 8, units = "in", bg = "white")
ggsave("figures/sqf_case_study_plot3.pdf", p3, width = 10, height = 6, units = "in", bg = "white")
ggsave("figures/sqf_case_study_plot4.pdf", p4, width = 10, height = 8, units = "in", bg = "white")
ggsave("figures/sqf_case_study_plot5.pdf", p5, width = 10, height = 8, units = "in", bg = "white")

# unconditional race distirbution in target vs train data
ggsave("figures/sqf_case_study_plot6.pdf", p8, width = 10, height = 8, units = "in", bg = "white") # race distribution
ggsave("figures/sqf_case_study_plot7.pdf", p1_combined, width = 10, height = 8, units = "in", bg = "white") # score densities
ggsave("figures/sqf_case_study_plot8.pdf", p2_combined, width = 10, height = 8, units = "in", bg = "white") # metrics comparison
# arrestemtn rates
ggsave("figures/sqf_case_study_plot9.pdf", p13, width = 10, height = 8, units = "in", bg = "white") # arrestment rates per race
ggsave("figures/sqf_case_study_plot10.pdf", p15, width = 10, height = 8, units = "in", bg = "white") # frisk rates)
# training and target geopraphical distribution
ggsave("figures/sqf_case_study_plot11.pdf", p45, width = 10, height = 8, units = "in", bg = "white")

# crime rates by borough
ggsave("figures/sqf_case_study_plot12.pdf", p16, width = 10, height = 8, units = "in", bg = "white")
ggsave("figures/sqf_case_study_plot13.pdf", p17, width = 10, height = 8, units = "in", bg = "white")
ggsave("figures/sqf_case_study_plot14.pdf", p19, width = 10, height = 8, units = "in", bg = "white")

# 3. models and data ----
saveRDS(complete_cases, file = "data/data2023.rds")
saveRDS(complete_cases_2011, file = "data/data2011.rds")
saveRDS(lrn_rf_2011, file = "program/trained_rf_2011.rds")
saveRDS(lrn_rf_2023, file = "program/trained_rf_2023.rds")
saveRDS(bmr, "program/bmr_results.rds")



