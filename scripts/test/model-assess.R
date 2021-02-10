# Fit ICAR model using Stan and INLA and test that the scores from eval_****_model functions are similar

fit_inla <- m1_inla(mw)
fit_stan <- m1_stan(mw)

scores_inla <- eval_inla_model(mw, fit_inla, 1, S = 3600)
scores_stan <- eval_stan_model(mw, fit_stan, 1)

scores_inla
scores_stan
