## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
#### load required packages
require(resistanceGame)

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=NA) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=1) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=NA, use_ops=1) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=c(NA,1)) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=c(NA,1), use_ops=c(1,NA)) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=rep(c(1,NA),each=10), use_ops=rep(c(NA,1),each=10)) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=rep(c(1,NA),each=10), use_ddt=rep(c(NA,1),each=10)) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=NA, randomness=0.5) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=NA, randomness=0.5) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=NA, randomness=0.1) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=1,resistance_modifier=0.1) )

## ---- eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=6-----
plot_sim( run_sim(use_pyr=1,resistance_modifier=10) )

