# description for reduced data

ggplot(imputed_data_g, aes(x = race_group_a, y = SUSPECT_ARRESTED_FLAG)) +
  geom_col()

imputed_data_g |> 
  group_by(race_group_a) |> 
  summarise(n = n(), prop = mean(SUSPECT_ARRESTED_FLAG))