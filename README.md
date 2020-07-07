# Mini-project 1: Small Area Estimation

| Model                   | Stan                    | INLA                    | TMB |
|-------------------------|-------------------------|-------------------------|-----|
| 1: Indep random effects | :ballot_box_with_check: | :ballot_box_with_check: |     |
| 2: Besg random effects  | :ballot_box_with_check: | :ballot_box_with_check: |     |
| 3: Weighted ICAR random effects |                 |                         |     |
| 4: BYM2 random effects  | :ballot_box_with_check: | :ballot_box_with_check: |     |
| 5: Centroid MVN         | :ballot_box_with_check: | :ballot_box_with_check: |     |
| 6: Indep + Centroid MVN | :ballot_box_with_check: |                         |     |


## To-do

- [x] Check hyper-parameter priors in INLA (and align thoughout) - often PC priors are used in INLA which are not easily available in Stan
- [ ] Code for producing model fit statistics in Stan (also need to be careful with the default statistics from INLA - inla.doc on priors shows that often factors are ommited)
- [ ] Find a way to implement more general BYM2 type models (such as Indep + Centroid MVN) in INLA. 
*Have spoken to Matt about this and confirmed it's not possible by default.
Might be able to achieve by giving the right adjacency / graph structure to INLA though*
- [ ] Learn TMB and add models
- [x] Produce Border ICAR sparse precision
- [ ] Write INLA/Stan code for fitting weighted ICAR models, such as the Border ICAR
- [ ] Spatial cross-validation in Stan working
- [ ] Spatial cross-validation in INLA
- [ ] Shiny app (most of the cloropleths look very visually similar, so Shiny may be better medium than paper)
- [x] Fix scaling factor discrepancy
*In part done but also need to figure out why the other code was broken.*
- [x] Move all parameters to the real line if possible (after discussion with AP) *Done for `sigma_phi`
- [ ] Move priors away from "uninformative" in line with Seth's suggestion
