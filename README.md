# Rogers' model recast and resolved as an agent-based model

This is an agent-based model that was created as a final project for the graduate course, *Agent-based modeling and simulation* (ANTH 547), taught by Dr. Luke Premo in the Anthropology Department at Washington State University. The model was created in Netlogo 5.3.1, which was designed to replicate the original model from a classic paper (Rogers, 1988).

The original model "paradoxically" showed that despite the observation that culture is adaptive for humans, introducing *social learners* (individuals that copy information from others) to a population of *individual learners* (individuals that learn through a costly trial-and-error process) produces no benefit to the average fitness of that population.

This model extends this model and is largely a replication of Rendell et al. (2010), which demonstrates that (1) a spatially explicit scenario actually exaggerates Rogers' original result, and (2) a flexible strategy, where learners can critically use social learning until it fails, does improve fitness (and thus, resolves the paradox raised in Rogers' model).

# Running the model and more information

The `nlogo` file contains everything needed to run the model in Netlogo. (Update in 2022: The model was written in version 5.3.1, and at the time of this writing, the current Netlogo version is 6.2.2. Despite the warning you will get when opening the file, everything should run fine from the original model. Feel free to let me know if there are any issues running it.)

The `pdf` file contains a more thorough [Overview, Design concepts, and Details (ODD) description](https://www.jasss.org/23/2/7.html) of the ABM. This will also include things to try, and what the parameters refer to in the model interface.

# References

Rendell, L., Fogarty, L., & Laland, K. N. (2010). Rogers' Paradox Recast and Resolved: Population Structure and the Evolution of Social Learning Strategies. Evolution: International Journal of Organic Evolution, 64(2), 534-548. ([link](https://onlinelibrary.wiley.com/doi/pdfdirect/10.1111/j.1558-5646.2009.00817.x))

Rogers, A. R. (1988). Does biology constrain culture?. American Anthropologist, 90(4), 819-831. ([link](https://anthrosource.onlinelibrary.wiley.com/doi/abs/10.1525/aa.1988.90.4.02a00030?casa_token=uih8uf4wE34AAAAA:4h_rSZS5BGS9JkHoR31vBOWIr085Z8hYOuHz9jFxYq84cM3oJqG1r74-PvwfvDiCMpRfl09FYkiG_lM))
