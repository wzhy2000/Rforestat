library(dplyr)
data("Loblolly")
df <- Loblolly %>% filter(Seed %in% c("301", "303"))
site_A <- df %>% filter(Seed == "301") %>% pull(height)
site_B <- df %>% filter(Seed == "303") %>% pull(height)
t_test_equal_var <- t.test(site_A, site_B, var.equal = TRUE, alternative = "two.sided")
t_test_unequal_var <- t.test(site_A, site_B, var.equal = FALSE, alternative = "two.sided")
t_test_one_sided <- t.test(site_A, site_B, var.equal = FALSE, alternative = "less")
var_test <- var.test(site_A, site_B)
df_pair <- df %>%
  select(age, height, Seed) %>%
  pivot_wider(names_from = Seed, values_from = height) %>%
  rename(site_A = `301`, site_B = `303`) %>%
  filter(!is.na(site_A) & !is.na(site_B)) 

t_test_paired <- t.test(df_pair$site_A, df_pair$site_B, paired = TRUE)
list(
  equal_var_two_sided = t_test_equal_var,
  unequal_var_two_sided = t_test_unequal_var,
  unequal_var_one_sided = t_test_one_sided,
  variance_test = var_test,
  paired_t_test = t_test_paired
)
