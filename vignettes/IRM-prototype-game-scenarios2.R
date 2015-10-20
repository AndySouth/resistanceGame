## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
#### load required packages
require(resistanceGame)

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim_oldcc( run_sim_oldcc() )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config <- read_config(in_folder=system.file("extdata","config_oldcc_no_control",
                                              package="resistanceGame"))
l_config2 <- config_plan(l_config, t_strt=1, t_stop=20, control_id='irs_pyr')
plot_sim_oldcc( run_sim_oldcc(l_config=l_config2) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=1, t_stop=20, control_id='irs_ops')
plot_sim_oldcc( run_sim_oldcc(l_config=l_config2) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=seq(1,20,2), t_stop=seq(1,20,2),
                            control_id='irs_pyr')
plot_sim_oldcc( run_sim_oldcc(l_config=l_config2) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=seq(1,20), t_stop=seq(1,20), control_id=c('irs_pyr','irs_ops'))
plot_sim_oldcc( run_sim_oldcc(l_config=l_config2) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=c(1,11), t_stop=c(10,20),
                            control_id=c('irs_pyr','irs_ops'))
plot_sim_oldcc( run_sim_oldcc(l_config=l_config2) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=c(1,11), t_stop=c(10,20),
                            control_id=c('irs_pyr','irs_ddt'))
plot_sim_oldcc( run_sim_oldcc(l_config=l_config2) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim_oldcc( run_sim_oldcc(randomness=0.5) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim_oldcc( run_sim_oldcc(randomness=0.5) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim_oldcc( run_sim_oldcc(randomness=0.1) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_plan(l_config, t_strt=1, t_stop=20, control_id='irs_pyr')
plot_sim_oldcc( run_sim_oldcc(l_config=l_config2, resistance_modifier=0.1) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
plot_sim_oldcc( run_sim_oldcc(l_config=l_config2, resistance_modifier=10) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
l_config2 <- config_controls(l_config, control_id='new_ai', vector_id='an_gamb',
                             control_kill_rate=0.4)
l_config2 <- config_resistances(l_config2, control_id='new_ai', resistance_id="target_site",
                                resistance_strength=0.8,
                                resistance_incr=0.2,
                                resistance_decr=0.1)
l_config2 <- config_plan(l_config2, t_strt=1, t_stop=20, control_id='new_ai')
plot_sim_oldcc( run_sim_oldcc(l_config=l_config2) )


## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
carry_cap = c(rep(1,6),rep(0.1,6))
plot_sim_oldcc( run_sim_oldcc(carry_cap=carry_cap, rate_growth=1.6), plot_cc=TRUE )

## ---- eval=TRUE, echo=TRUE, message=FALSE, results='hide', fig.width=7, fig.height=6----
carry_cap = seq(1,0.1,-0.1)
plot_sim_oldcc( run_sim_oldcc(carry_cap=carry_cap, rate_growth=1.6), plot_cc=TRUE )

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
places <- read.csv( system.file('extdata','config1_oldcc','places.csv', package='resistanceGame'))
print(places) 

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
vectors <- read.csv( system.file('extdata','config1_oldcc','vectors.csv', package='resistanceGame'))
print(vectors)

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
controls <- read.csv( system.file('extdata','config1_oldcc','controls.csv', package='resistanceGame'))
print(controls)

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
resistances <- read.csv( system.file('extdata','config1_oldcc','resistances.csv', package='resistanceGame'))
print(resistances)

## ---- eval=TRUE, echo=FALSE, message=FALSE, results='markup', fig.width=7, fig.height=6----
control_plan <- read.csv( system.file('extdata','config1_oldcc','control_plan.csv', package='resistanceGame'))
print(control_plan)

