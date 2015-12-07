## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
#### load required packages
require(resistanceGame)

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
emergence <- expand_season(season_string="6:0.3;6:0.1")
plot_sim( run_sim(num_tsteps=336, emergence=emergence, survival=0.7,
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
emergence <- expand_season(season_string="6:0.3;6:0.1")
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=c(1), t_stop=c(336),
                         control_id=c('irs_ops'))
plot_sim( run_sim(l_config=l_config2, 
                  num_tsteps=336, emergence=emergence, survival=0.7,
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
emergence <- expand_season(season_string="6:0.3;6:0.1")
l_config2 <- config_plan(l_config, t_strt=c(1), t_stop=c(336),
                         control_id=c('irs_pyr'))
plot_sim( run_sim(l_config=l_config2, 
                  num_tsteps=336, emergence=emergence, survival=0.7,
                  resist_freq_start = 0.01,
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
emergence <- expand_season(season_string="6:0.3;6:0.1")
l_config2 <- config_plan(l_config, t_strt=c(1,169), t_stop=c(168,336),
                         control_id=c('irs_ops','irs_pyr'))
plot_sim( run_sim(l_config=l_config2, 
                  num_tsteps=336, emergence=emergence, survival=0.7,
                  resist_incr=0.02, resist_decr = 0.01), 
                  plot_emergence=TRUE )

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
places <- read.csv( system.file('extdata','config1','places.csv', package='resistanceGame'))
print(places) 

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
vectors <- read.csv( system.file('extdata','config1','vectors.csv', package='resistanceGame'))
print(vectors)

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
controls <- read.csv( system.file('extdata','config1','controls.csv', package='resistanceGame'))
print(controls)

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
resistances <- read.csv( system.file('extdata','config1','resistances.csv', package='resistanceGame'))
print(resistances)

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
control_plan <- read.csv( system.file('extdata','config1','control_plan.csv', package='resistanceGame'))
print(control_plan)

