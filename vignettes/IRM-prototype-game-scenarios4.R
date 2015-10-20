## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
#### load required packages
require(resistanceGame)

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim( run_sim(num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config <- read_config()
l_config2 <- config_plan(l_config, t_strt=1, t_stop=360, control_id='irs_pyr')
plot_sim( run_sim(l_config=l_config2, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=1, t_stop=360, control_id='irs_ops')
plot_sim( run_sim(l_config=l_config2, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=1, t_stop=180,
                            control_id='irs_pyr')
plot_sim( run_sim(l_config=l_config2, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=1, t_stop=180, control_id=c('irs_ops'))
plot_sim( run_sim(l_config=l_config2, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=c(1,181), t_stop=c(180,360),
                            control_id=c('irs_pyr','irs_ops'))
plot_sim( run_sim(l_config=l_config2, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=c(1,181), t_stop=c(180,360),
                            control_id=c('irs_pyr','irs_ddt'))
plot_sim( run_sim(l_config=l_config2, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim( run_sim(randomness=0.5, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim( run_sim(randomness=0.5, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim( run_sim(randomness=0.1, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=1, t_stop=360, control_id='irs_pyr')
plot_sim( run_sim(l_config=l_config2, resistance_modifier=0.1, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim( run_sim(l_config=l_config2, resistance_modifier=10, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_controls(l_config, control_id='new_ai', vector_id='an_gamb',
                             control_kill_rate=0.4)
l_config2 <- config_resistances(l_config2, control_id='new_ai', resistance_id="target_site",
                                resistance_strength=0.8,
                                resistance_incr=0.2,
                                resistance_decr=0.1)
l_config2 <- config_plan(l_config2, t_strt=1, t_stop=360, control_id='new_ai')
plot_sim( run_sim(l_config=l_config2, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01) )


## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
emergence = c(rep(0.3,180),rep(0,180))
plot_sim( run_sim(emergence=emergence, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01), plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
#sin curve to create a gradual change from 0 to 0.3
emergence = 0.3*sin(seq(0,3.1,0.01))
plot_sim( run_sim(emergence=emergence, num_tsteps=360, resist_incr=0.02, resist_decr = 0.01), plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
emergence = c(rep(0.3,180),rep(0,180))
plot_sim( run_sim(num_tsteps=360,emergence=emergence, survival=0.7,
                  resist_incr=0.02, resist_decr = 0.01), 
          plot_emergence=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
emergence = c(rep(0.3,180),rep(0,180))
l_config2 <- config_plan(l_config, t_strt=c(1,181), t_stop=c(180,360),
                         control_id=c('irs_ops','irs_pyr'))
plot_sim( run_sim(l_config=l_config2, 
                  num_tsteps=360, emergence=emergence, survival=0.7,
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

