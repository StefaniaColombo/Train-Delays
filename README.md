# Train-Delays
Code associated to the thesis "From Delays to Dynamics: Multi-state Modeling in Suburban Rail Transport" authored by Stefania Colombo, and supervised by Francesca Ieva, Simone Vantini and Alfredo Gimenez Zapiola.

## Outline
This thesis applies continuous-time multi-state models to analize delay evolution on the S5 suburban line in Lombardy, Italy. Using detailed operational, meteorological, and contextual data, the study models delay transitions while accounting for structural heterogeneity.

In particular:
- Scripts `preprocessing.Rmd` reports data preprocessing;
- Scripts `NP_3.Rmd` and `NP_4.Rmd` develops the code needed to model both the short- and long-term dynamics of train delays without considering any covariates, respectively for the 3-state and the 4-state models. It includes the estimation of the transition intensities between delay states, cumulative hazards, and transition probabilities of moving from an initial state to another over the time interval [0,30min];
- Script `semi.Rmd` follows the same steps of the prevoius one, but focusing on the effects of factors such as: boarding and alighting passengers, weather conditions, and train frequency at each station. It also contains the code to perform frailty models.

The code is developed in R (version 4.3.2).

## Data and Resources
Due to confidentiality agreements, the Trenord data and the weather records are not available. Two artificial datasets `train.csv` and `meteo.csv` are produced for demonstrative purposes, which, however, have no relationship with the actual data about railway movements and weather conditions in 2023.
