## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
#### load required packages
require(resistanceGame)

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
plot_sim( run_sim(num_tsteps=672, emergence=emergence, survival=0.7,
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(168), t_stop=c(672),
                         control_id=c('irs_ops'))
plot_sim( run_sim(num_tsteps=672, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.5,
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(168), t_stop=c(672),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=672, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.1, resist_mech='metabolic',
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(168), t_stop=c(672),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=672, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.1, resist_mech='target',
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1")
plot_sim( run_sim(num_tsteps=672, emergence=emergence, survival=0.7,
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1")
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(168), t_stop=c(672),
                         control_id=c('irs_ops'))
plot_sim( run_sim(num_tsteps=672, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.5,
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1")
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(168), t_stop=c(672),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=672, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.1, resist_mech='metabolic',
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1")
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(168), t_stop=c(672),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=672, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.1, resist_mech='target',
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1")
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(1,169), t_stop=c(168,336),
                         control_id=c('irs_pyr','irs_ops'))
plot_sim( run_sim(num_tsteps=672, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.1, resist_mech='metabolic',
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

