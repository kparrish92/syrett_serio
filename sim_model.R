

sim_df %>% 
  group_by(answer, condition) %>% 
  summarize(n = n()/288)


get_prior(sim_model)

.15*(sqrt(288))



prior_check = c(set_prior("normal(.15, 2)", class = "Intercept", dpar = "muis"),
                set_prior("normal(.22, 2)", class = "Intercept", dpar = "muisnot"),
                set_prior("normal(.76, 2)", class = "b", dpar = "muis", coef = "conditionnatural_kind"),
                set_prior("normal(1.26, 2)", class = "b", dpar = "muisnot", coef = "conditionnatural_kind"),
                set_prior("normal(1.4, 2)", class = "b", dpar = "muis", coef = "muis_conditionartifact"),
                set_prior("normal(2.59, 2)", class = "b", dpar = "muisnot", coef = "muis_conditionartifact"))



sim_model_priors = brm(answer ~ condition + (condition|ppt),
    prior = prior_check,                   
    data=sim_df,
    family="categorical")

summary(sim_model)

get_prior(sim_model)

conditional_effects(sim_model, categorical = TRUE)