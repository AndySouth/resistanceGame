## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
#### load required packages
require(resistanceGame)

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
plot_sim( run_sim(num_tsteps=96, emergence=emergence, survival=0.7,
                  resist_incr=0.2, resist_decr = 0.1), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(24), t_stop=c(96),
                         control_id=c('irs_ops'))
plot_sim( run_sim(num_tsteps=96, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.5,
                  resist_incr=0.2, resist_decr = 0.1), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(24), t_stop=c(96),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=96, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.01, resist_mech='metabolic',
                  resist_incr=0.2, resist_decr = 0.1), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(24), t_stop=c(144),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=144, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.01, resist_mech='metabolic',
                  #resist_incr=0.2, resist_decr = 0.1), 
                  resist_incr=0.05, resist_decr = 0.02),           
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(24), t_stop=c(144),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=144, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.01, resist_mech='metabolic',
                  #resist_incr=0.2, resist_decr = 0.1), 
                  resist_incr=0.05, resist_decr = 0.02),           
                  plot_emergence=TRUE, plot_type = '6month' )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- 0.3
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(24), t_stop=c(96),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=96, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.01, resist_mech='target',
                  resist_incr=0.2, resist_decr = 0.1), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

#emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1")
emergence <- expand_season(season_string="3:0.3;1:0.25;1:0.24;1:0.23;1:0.22;1:0.21;1:0.20;1:0.19;1:0.18;1:0.17;1:0.16;6:0.3;6:0.1",return_tstep='weeks')
plot_sim( run_sim(num_tsteps=96, emergence=emergence, survival=0.7,
                  resist_incr=0.2, resist_decr = 0.1), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1")
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(24), t_stop=c(96),
                         control_id=c('irs_ops'))
plot_sim( run_sim(num_tsteps=96, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.5,
                  resist_incr=0.2, resist_decr = 0.1), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1",return_tstep='weeks')
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(24), t_stop=c(96),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=96, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.01, resist_mech='metabolic',
                  resist_incr=0.2, resist_decr = 0.1), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1",return_tstep='weeks')
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(24), t_stop=c(96),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(num_tsteps=96, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.01, resist_mech='target',
                  resist_incr=0.2, resist_decr = 0.1), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----

emergence <- expand_season(season_string="6:0.3;6:0.1;6:0.3;6:0.1",return_tstep='weeks')
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(1,25), t_stop=c(24,96),
                         control_id=c('irs_pyr','irs_ops'))
plot_sim( run_sim(num_tsteps=96, emergence=emergence, survival=0.7,
                  l_config=l_config2,
                  insecticide_kill=0.6, resist_freq_start = 0.01, resist_mech='metabolic',
                  resist_incr=0.2, resist_decr = 0.1), 
                  plot_emergence=TRUE )

