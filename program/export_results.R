# 1. tables ----
writexl::write_xlsx(fairness_audit_rf$fairness_metrics, path = "figures/sqf_case_study_table1")

# 2. plots ----
ggsave("figures/sqf_case_study_plot1.png", p1_rf, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot2.png", p2_rf, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot3.png", p3, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot4.png", p4, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
ggsave("figures/sqf_case_study_plot5.png", p5, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
