#' ---
#' title: "Backwards Compatibility Demo"
#' author: "Ross Bennett"
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve optimization problems using what is
#' referred to as the v1 specification. The v1 specification was used in
#' before PortfolioAnalytics version 0.8.3 to define the optimization problem
#' with constraints and objectives.

library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)

data(edhec)
ret <- edhec[,1:13]
funds <- colnames(ret)

#' Set up constraint object using v1 specification
gen.constr <- constraint(assets=funds, min=0, max=0.25, min_sum=0.99, max_sum=1.01, 
                         weight_seq=generatesequence(min=0, max=0.25, by=0.002))
class(gen.constr)

#' Add an objective to the gen.constr object
gen.constr <- add.objective(constraints=gen.constr, type="return", name="mean", enabled=TRUE)

#' Here we run the optimization. Note that optimize.portfolio will detect 
#' that a v1_constraint object has been passed in and will update to the 
#' v2 specification using a portfolio object with constraints and objectives 
#' from the v1_constraint object.

#' Solve the problem using the random portfolios optimization engine
optrpv1 <- optimize.portfolio(R=ret, constraints=gen.constr, optimize_method="random", search_size=2000)
optrpv1

#' Solve the problem using the DEoption (Differential Evolution) optimization engine
optdev1 <- optimize.portfolio(R=ret, constraints=gen.constr, optimize_method="DEoptim", search_size=2000)
optdev1

#' Solve the problem using the ROI (R Optimization Infrastructure) optimization engine
optroiv1 <- optimize.portfolio(R=ret, constraints=gen.constr, optimize_method="ROI")
optroiv1




#' ---
#' title: "chart.Concentration Demo"
#' author: "Ross Bennett"
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to use chart.Concentration to visualize
#' the concentration of the portfolio.


library(PortfolioAnalytics)

data(edhec)
R <- edhec[, 1:13]
funds <- colnames(R)

#' Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, 
                             type="full_investment")

init.portf <- add.constraint(portfolio=init.portf, 
                             type="box", 
                             min=0, 
                             max=0.25)

init.portf <- add.objective(portfolio=init.portf, 
                            type="return", 
                            name="mean", 
                            multiplier=0)

init.portf <- add.objective(portfolio=init.portf, 
                            type="risk", 
                            name="ES")

#' Construct a risk budget portfolio.
rb.portf <- add.objective(portfolio=init.portf, 
                          type="risk_budget", 
                          name="ES",
                          max_prisk=0.4, 
                          arguments=list(p=0.92))

#' Use random portfolios for optimization.
system.time(opt <- optimize.portfolio(R=R, 
                          portfolio=init.portf, 
                          optimize_method="random", 
                          search_size=200000, 
                          trace=TRUE))

opt_rb <- optimize.portfolio(R=R, 
                             portfolio=rb.portf, 
                             optimize_method="random", 
                             search_size=2000, 
                             trace=TRUE)

#' This won't work because opt is not a risk budget optimization.
#' This should result in an error and not plot anything.
#chart.Concentration(opt, conc.type="pct_contrib")

#' `opt` is minimum ES optimization so we can still chart it using weights as
#' the measure of concentration.
chart.Concentration(opt, conc.type="weights", chart.assets=TRUE, col=heat.colors(10))
chart.Concentration(opt, conc.type="weights", chart.assets=TRUE, col=bluemono)

#' Here we plot the concentration based on the HHI of the percentage component 
#' contribution to risk.
chart.Concentration(opt_rb, conc.type="pct_contrib")

#' Here we plot the concentration is based on the HHI of the weights.
chart.Concentration(opt_rb, conc.type="weights")



#' ---
#' title: "Differential Evolution Optimization Demo"
#' date: "7/17/2014"
#' ---

#' This script demonstrates several optimization problems using Differential
#' Evolution as the optimization engine. This script is based heavily on
#' http://www.rinfinance.com/agenda/2012/workshop/Carl+Peterson.pdf.

#' The following optimization problems will be run
#' * mean-mETL: maximize mean-to-ETL (i.e. reward-to-risk)
#' * MinSD: minimize annualized standard deviation
#' * eqStdDev: equal risk (volatility)
#' * MeanRL: maximize mean with mETL risk limits

#' Include optimizer and multi-core packages
library(PortfolioAnalytics)
require(DEoptim)
require(foreach)

#' The multicore package, and therefore registerDoMC, should not be used in a
#' GUI environment, because multiple processes then share the same GUI. Only use
#' when running from the command line.
# require(doMC)
# registerDoMC(3)

#' Load the data
data(edhec)
edhec.R <- edhec[,c("Convertible Arbitrage", "Equity Market Neutral", 
                    "Fixed Income Arbitrage", "Event Driven", "CTA Global", 
                    "Global Macro", "Long/Short Equity")]

#' Define function to compute annualized standard deviation
pasd <- function(R, weights){
  as.numeric(StdDev(R=R, weights=weights)*sqrt(12)) # hardcoded for monthly data
  # as.numeric(StdDev(R=R, weights=weights)*sqrt(4)) # hardcoded for quarterly data
}

#' Set some parameters
rebalance_period = 'quarters' # uses endpoints identifiers from xts
clean = "none" #"boudt"
permutations = 4000

#' Create initial portfolio object used to initialize ALL the bouy portfolios
init.portf <- portfolio.spec(assets=colnames(edhec.R), 
                             weight_seq=generatesequence(by=0.005))
#' Add leverage constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="leverage", 
                             min_sum=0.99, 
                             max_sum=1.01)
#' Add box constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="box", 
                             min=0.05, 
                             max=0.3)

#' Add measure 1, mean return
init.portf <- add.objective(portfolio=init.portf,
                            type="return", # the kind of objective this is
                            name="mean", # name of the function
                            enabled=TRUE, # enable or disable the objective
                            multiplier=0 # calculate it but don't use it in the objective
)

#' Add measure 2, annualized standard deviation
init.portf <- add.objective(portfolio=init.portf,
                            type="risk", # the kind of objective this is
                            name="pasd", # to minimize from the sample
                            enabled=TRUE, # enable or disable the objective
                            multiplier=0 # calculate it but don't use it in the objective
)

#' Add measure 3, ES with confidence level p=(1-1/12)
p <- 1-1/12 # for monthly

init.portf <- add.objective(portfolio=init.portf,
                            type="risk", # the kind of objective this is
                            name="ES", # the function to minimize
                            enabled=FALSE, # enable or disable the objective
                            multiplier=0, # calculate it but don't use it in the objective
                            arguments=list(p=p)
)

#' Set up portfolio for Mean-mETL
MeanmETL.portf <- init.portf
MeanmETL.portf$objectives[[1]]$multiplier=-1 # mean
MeanmETL.portf$objectives[[3]]$enabled=TRUE # mETL
MeanmETL.portf$objectives[[3]]$multiplier=1 # mETL

#' Set up portfolio for min pasd
MinSD.portf <- init.portf
MinSD.portf$objectives[[2]]$multiplier=1

#' Set up portfolio for eqStdDev
EqSD.portf <- add.objective(portfolio=init.portf,
                            type="risk_budget",
                            name="StdDev",
                            min_concentration=TRUE,
                            arguments = list(p=(1-1/12)))
#' Without a sub-objective, we get a somewhat undefined result, since 
#' there are (potentially) many Equal SD contribution portfolios.
EqSD.portf$objectives[[2]]$multiplier=1 # min pasd

#' Set up portfolio to maximize mean with mETL risk limit
MeanRL.portf <- add.objective(portfolio=init.portf, 
                              type='risk_budget', 
                              name="ES", 
                              min_prisk=-Inf, 
                              max_prisk=0.4, 
                              arguments=list(method="modified", p=p))
MeanRL.portf$objectives[[1]]$multiplier=-1 # mean
#' Change box constraints max to vector of 1s
MeanRL.portf$constraints[[2]]$max=rep(1, 7)

#' Set the 'R' variable
R <- edhec.R

#' Start the optimizations
start_time<-Sys.time()
print(paste('Starting optimization at',Sys.time()))

#' Run the optimization
##### mean-mETL #####
MeanmETL.DE <- optimize.portfolio(R=R,
                                  portfolio=MeanmETL.portf,
                                  optimize_method="DEoptim",
                                  trace=TRUE,
                                  search_size=2000,
                                  traceDE=5)
print(MeanmETL.DE)
print(MeanmETL.DE$elapsed_time)
chart.Weights(object=MeanmETL.DE, main="Mean-mETL Weights")
chart.RiskReward(object=MeanmETL.DE, return.col="mean", risk.col="ES")
# save(MeanmETL.DE, file=paste('MeanmETL',Sys.Date(),'rda',sep='.'))

# Evaluate the objectives with DE through time
# MeanmETL.DE.t <- optimize.portfolio.rebalancing(R=R,
#                                                 portfolio=MeanSD.portf,
#                                                 optimize_method="random",
#                                                 trace=TRUE,
#                                                 search_size=2000,
#                                                 rebalance_on=rebalance_period,
#                                                 training_period=36)
# MeanmETL.w = extractWeights.rebal(MeanmETL.DE.t)
# MeanmETL=Return.rebalancing(edhec.R, MeanmETL)
# colnames(MeanmETL) = "MeanmETL"
# save(MeanmETL.DE, MeanmETL.DE.t, MeanmETL.w, MeanmETL, file=paste('MeanmETL',Sys.Date(),'rda',sep='.'))

print(paste('Completed MeanmETL optimization at',Sys.time(),'moving on to MinSD'))

#' Run the optimization
##### min pasd #####
MinSD.DE <- optimize.portfolio(R=R,
                               portfolio=MinSD.portf,
                               optimize_method="DEoptim",
                               trace=TRUE,
                               search_size=2000,
                               traceDE=5)
print(MinSD.DE)
print(MinSD.DE$elapsed_time)
chart.Weights(object=MinSD.DE, plot.type="barplot", legend.loc=NULL)
chart.RiskReward(object=MinSD.DE, return.col="mean", risk.col="pasd")
# save(MinSD.DE, file=paste('MinSD',Sys.Date(),'rda',sep='.'))

print(paste('Completed MinSD optimization at',Sys.time(),'moving on to EqSD'))

#' Run the optimization
##### EqSD #####
EqSD.DE <- optimize.portfolio(R=R,
                              portfolio=EqSD.portf,
                              optimize_method="DEoptim",
                              trace=TRUE,
                              search_size=2000,
                              traceDE=5)
print(EqSD.DE)
print(EqSD.DE$elapsed_time)
# save(EqSD.DE, file=paste('EqSD',Sys.Date(),'rda',sep='.'))

chart.Weights(object=EqSD.DE)
chart.RiskReward(object=EqSD.DE, return.col="mean", risk.col="StdDev")
chart.RiskBudget(object=EqSD.DE, risk.type="absolute")
chart.RiskBudget(object=EqSD.DE, risk.type="pct_contrib")

print(paste('Completed EqSD optimization at',Sys.time(),'moving on to MeanRL'))

#' Run the optimization
##### MeanRL.DE #####
MeanRL.DE <- optimize.portfolio(R=R,
                                portfolio=MeanRL.portf,
                                optimize_method="DEoptim",
                                trace=TRUE,
                                search_size=2000,
                                traceDE=5)
print(MeanRL.DE)
print(MeanRL.DE$elapsed_time)
# save(MeanRL.DE, file=paste('MeanRL',Sys.Date(),'rda',sep='.'))

chart.Weights(object=MeanRL.DE)
chart.RiskBudget(object=MeanRL.DE, risk.type="pct_contrib", neighbors=25)

end_time<-Sys.time()
print("Optimization Complete")
print(end_time-start_time)



#' ---
#' title: "Efficient Frontier Demo"
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to compute and plot the efficient frontier
#' given different constraints and objectives.

#' Efficient frontiers can be plotted two ways
#' 1. Run optimize.portfolio with trace=TRUE and then chart that object.
#' 2. create an efficient frontier and then chart that object.

#' Load required packages
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)

#' Load the data and change the column names for better legends in plotting.
data(edhec)
R <- edhec[, 1:5]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQM")
funds <- colnames(R)

#' Set up the initial portfolio object with some basic constraints.
init <- portfolio.spec(assets=funds)
init <- add.constraint(portfolio=init, type="full_investment")
init <- add.constraint(portfolio=init, type="box", min=0.15, max=0.45)
init <- add.constraint(portfolio=init, type="group",
                       groups=list(c(1, 3),
                                   c(2, 4, 5)),
                       group_min=0.05,
                       group_max=0.7)

#' Add objectives for mean-ES (Expected Shortfall) portfolio.
meanetl.portf <- add.objective(portfolio=init, type="risk", name="ES")
meanetl.portf <- add.objective(portfolio=meanetl.portf, type="return", name="mean")

#' Add objectives for mean-variance portfolio.
meanvar.portf <- add.objective(portfolio=init, type="risk", name="var", risk_aversion=10)
meanvar.portf <- add.objective(portfolio=meanvar.portf, type="return", name="mean")

#' Compute the mean-variance efficient frontier.
meanvar.ef <- create.EfficientFrontier(R=R, portfolio=init, type="mean-StdDev")
meanvar.ef
summary(meanvar.ef, digits=2)
meanvar.ef$frontier

#' The RAR.text argument can be used for the risk-adjusted-return name on the 
#' legend, by default it is 'Modified Sharpe Ratio'.
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", 
                        RAR.text="Sharpe Ratio", pch=4)

#' The tangency portfolio and line are plotted by default, these can be 
#' ommitted by setting rf=NULL.
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="b", rf=NULL)

#' The tangency line can be omitted with tangent.line=FALSE. The tangent 
#' portfolio, risk-free rate and Sharpe Ratio are still included in the plot.
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", tangent.line=FALSE)

#' The assets can be omitted with chart.assets=FALSE.
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", 
                        tangent.line=FALSE, chart.assets=FALSE)

#' Just the names of the assets can be omitted with labels.assets=FALSE and the 
#' plotting character can be changed with pch.assets.
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", 
                        tangent.line=FALSE, labels.assets=FALSE, pch.assets=1)

#' Chart the asset weights along the efficient frontier.
chart.EF.Weights(meanvar.ef, colorset=bluemono, match.col="StdDev")

#' Chart the group weights along the efficient frontier.
chart.EF.Weights(meanvar.ef, colorset=bluemono, by.groups=TRUE, match.col="StdDev")

#' The labels for Mean, Weight, and StdDev can be increased or decreased with
#' the cex.lab argument. The default is cex.lab=0.8.
chart.EF.Weights(meanvar.ef, colorset=bluemono, match.col="StdDev", main="", cex.lab=1)

#' If you have a lot of assets and they don't fit with the default legend, you
#' can set legend.loc=NULL and customize the plot.
par(mar=c(8, 4, 4, 2)+0.1, xpd=TRUE)
chart.EF.Weights(meanvar.ef, colorset=bluemono, match.col="StdDev", legend.loc=NULL)
legend("bottom", legend=colnames(R), inset=-1, fill=bluemono, bty="n", ncol=3, cex=0.8)
par(mar=c(5, 4, 4, 2)+0.1, xpd=FALSE)

#' Run optimize.portfolio and chart the efficient frontier of the optimal
#' portfolio object.
opt_meanvar <- optimize.portfolio(R=R, portfolio=meanvar.portf, 
                                  optimize_method="ROI", trace=TRUE)

#' The efficient frontier is created from the 'opt_meanvar' object by getting.
#' The portfolio and returns objects and then passing those to create.EfficientFrontier.
chart.EfficientFrontier(opt_meanvar, match.col="StdDev", n.portfolios=25, type="l")

#' Rerun the optimization with a new risk aversion parameter to change where 
#' the portfolio is along the efficient frontier. The 'optimal' portfolio 
#' plotted on the efficient frontier is the optimal portfolio returned by 
#' optimize.portfolio.
meanvar.portf$objectives[[2]]$risk_aversion=0.25
opt_meanvar <- optimize.portfolio(R=R, portfolio=meanvar.portf, optimize_method="ROI", trace=TRUE)
chart.EfficientFrontier(opt_meanvar, match.col="StdDev", n.portfolios=25, type="l")

#' The weights along the efficient frontier can be plotted by passing in the
#' optimize.portfolio output object.
chart.EF.Weights(opt_meanvar, match.col="StdDev")

chart.EF.Weights(opt_meanvar, match.col="StdDev", by.groups=TRUE)

#' Extract the efficient frontier and then plot it.
#' Note that if you want to do multiple charts of the efficient frontier from
#' the optimize.portfolio object, it is best to extractEfficientFrontier as 
#' shown below.
ef <- extractEfficientFrontier(object=opt_meanvar, match.col="StdDev", n.portfolios=15)
ef
summary(ef, digits=5)
chart.EF.Weights(ef, match.col="StdDev", colorset=bluemono)
chart.EF.Weights(ef, match.col="StdDev", colorset=bluemono, by.groups=TRUE)

#' Compute the mean-ES efficient frontier.
meanetl.ef <- create.EfficientFrontier(R=R, portfolio=init, type="mean-ES")
meanetl.ef
summary(meanetl.ef)
meanetl.ef$frontier

#' Chart the mean-ES efficient frontier.
chart.EfficientFrontier(meanetl.ef, match.col="ES", main="mean-ETL Efficient Frontier", type="l", col="blue", RAR.text="STARR")
chart.EF.Weights(meanetl.ef, colorset=bluemono, match.col="ES")
chart.EF.Weights(meanetl.ef, by.groups=TRUE, colorset=bluemono, match.col="ES")

#' Compute the mean-ES efficient frontier using random portfolios to solve
#' the optimization problem.
meanetl.rp.ef <- create.EfficientFrontier(R=R, portfolio=meanetl.portf, type="random", match.col="ES")
chart.EfficientFrontier(meanetl.rp.ef, match.col="ES", main="mean-ETL RP Efficient Frontier", type="l", col="blue", rf=0)
chart.EF.Weights(meanetl.rp.ef, colorset=bluemono, match.col="ES")

# mean-etl efficient frontier with optimize.portfolio output
opt_meanetl <- optimize.portfolio(R=R, portfolio=meanetl.portf, optimize_method="random", search_size=2000, trace=TRUE)
chart.EfficientFrontier(meanetl.rp.ef, match.col="ES", main="mean-ETL RP Efficient Frontier", type="l", col="blue", rf=0, RAR.text="STARR")

#' Create a mean-var efficient frontier for multiple portfolios and overlay 
#' the efficient frontier lines. Set up an initial portfolio with the full 
#' investment constraint and mean and var objectives.
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")

#' Portfolio with long only constraints.
lo.portf <- add.constraint(portfolio=init.portf, type="long_only")

#' Portfolio with box constraints.
box.portf <- add.constraint(portfolio=init.portf, type="box", min=0.05, max=0.65)

#' Portfolio with group constraints (also add long only constraints to the 
#' group portfolio).
group.portf <- add.constraint(portfolio=init.portf, type="group", 
                              groups=list(groupA=c(1, 3),
                                          groupB=c(2, 4, 5)),
                              group_min=c(0.25, 0.15), 
                              group_max=c(0.75, 0.55))
group.portf <- add.constraint(portfolio=group.portf, type="long_only")

#' Combine the portfolios into a list.
portf.list <- combine.portfolios(list(lo.portf, box.portf, group.portf))

#' Plot the efficient frontier overlay of the portfolios with varying constraints.
legend.labels <- c("Long Only", "Box", "Group + Long Only")
chart.EfficientFrontierOverlay(R=R, portfolio_list=portf.list, type="mean-StdDev", 
                               match.col="StdDev", legend.loc="topleft", 
                               legend.labels=legend.labels, cex.legend=0.6,
                               labels.assets=FALSE, pch.assets=18)

#' Efficient frontier in mean-ES space with varying confidence leves for
#' ES calculation.
ES90 <- add.objective(portfolio=lo.portf, type="risk", name="ES", 
                      arguments=list(p=0.9))

ES92 <- add.objective(portfolio=lo.portf, type="risk", name="ES", 
                      arguments=list(p=0.92))

ES95 <- add.objective(portfolio=lo.portf, type="risk", name="ES", 
                      arguments=list(p=0.95))

#' Combine the portfolios into a list.
portf.list <- combine.portfolios(list(ES.90=ES90, ES.92=ES92, ES.95=ES95))

#' Plot the efficient frontier overlay of the portfolios with varying 
#' confidence levels fot he ES calculation.
legend.labels <- c("ES (p=0.9)", "ES (p=0.92)", "ES (p=0.95)")
chart.EfficientFrontierOverlay(R=R, portfolio_list=portf.list, type="mean-ES", 
                               match.col="ES", legend.loc="topleft", 
                               legend.labels=legend.labels, cex.legend=0.6,
                               labels.assets=FALSE, pch.assets=18)





#' ---
#' title: "Factor Exposure Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a portfolio optimization problem with
#' factor exposure constraints.

#' Load the required packages
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)

#' Load the data
data(edhec)
ret <- edhec[, 1:4]

#' Create portfolio object
pspec <- portfolio.spec(assets=colnames(ret))

#' Here we define individual constraint objects.
#' Leverage constraint.
lev_constr <- weight_sum_constraint(min_sum=1, max_sum=1)

#' Box constraint
lo_constr <- box_constraint(assets=pspec$assets, min=c(0.01, 0.02, 0.03, 0.04), max=0.65)

#' Group constraint'
grp_constr <- group_constraint(assets=pspec$assets, groups=list(1:2, 3, 4), group_min=0.1, group_max=0.4)

#' Position limit constraint
pl_constr <- position_limit_constraint(assets=pspec$assets, max_pos=4)

#' Make up a B matrix for an industry factor model.
#' dummyA, dummyB, and dummyC could be industries, sectors, etc.
B <- cbind(c(1, 1, 0, 0),
           c(0, 0, 1, 0),
           c(0, 0, 0, 1))
rownames(B) <- colnames(ret)
colnames(B) <- c("dummyA", "dummyB", "dummyC")
lower <- c(0.1, 0.1, 0.1)
upper <- c(0.4, 0.4, 0.4)

#' Industry exposure constraint.
#' The exposure constraint and group constraint are equivalent to test that 
#' they result in the same solution.
exp_constr <- factor_exposure_constraint(assets=pspec$assets, B=B, lower=lower, upper=upper)

#' Here we define objectives.
#' 
#' Objective to minimize variance.
var_obj <- portfolio_risk_objective(name="var")

#' Objective to maximize return.
ret_obj <- return_objective(name="mean")

#' Objective to minimize ETL.
etl_obj <- portfolio_risk_objective(name="ETL")

#' Run optimization on minimum variance portfolio with leverage, long only,
#' and group constraints.
opta <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, grp_constr), 
                           objectives=list(var_obj), 
                           optimize_method="ROI")
opta

#' Run optimization on minimum variance portfolio with leverage, long only,
#' and factor exposure constraints.
optb <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, exp_constr), 
                           objectives=list(var_obj), 
                           optimize_method="ROI")
optb

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(opta$weights, optb$weights)

#' Run optimization on maximum return portfolio with leverage, long only,
#' and group constraints.
optc <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, grp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="ROI")
optc

#' Run optimization on maximum return portfolio with leverage, long only,
#' and factor exposure constraints.
optd <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, exp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="ROI")
optd

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(optc$weights, optd$weights)

#' Run optimization on minimum expected tail loss portfolio with leverage, 
#' long only, and group constraints.
opte <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, grp_constr), 
                           objectives=list(etl_obj), 
                           optimize_method="ROI")
opte

#' Run optimization on minimum expected tail loss portfolio with leverage, 
#' long only, and factor exposure constraints.
optf <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, exp_constr), 
                           objectives=list(etl_obj), 
                           optimize_method="ROI")
optf

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(opte$weights, optf$weights)

#' Run optimization on maximum return portfolio with leverage, long only,
#' and group constraints using DEoptim as the optimization engine.
set.seed(123)
optde1 <- optimize.portfolio(R=ret, portfolio=pspec, 
                             constraints=list(lev_constr, lo_constr, grp_constr), 
                             objectives=list(ret_obj), 
                             optimize_method="DEoptim", 
                             search_size=20000, 
                             trace=F)
optde1

#' Run optimization on maximum return portfolio with leverage, long only,
#' and factor exposure constraints using DEoptim as the optimization engine.
set.seed(123)
optde2 <- optimize.portfolio(R=ret, portfolio=pspec, 
                             constraints=list(lev_constr, lo_constr, exp_constr), 
                             objectives=list(ret_obj), 
                             optimize_method="DEoptim", 
                             search_size=20000, 
                             trace=F)
optde2

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(optde1$weights, optde2$weights)

#' Run optimization on maximum return portfolio with leverage, long only,
#' and group constraints using random portfolios as the optimization
#' engine.
optrp1 <- optimize.portfolio(R=ret, portfolio=pspec, 
                             constraints=list(lev_constr, lo_constr, grp_constr), 
                             objectives=list(ret_obj), 
                             optimize_method="random", 
                             search_size=2000, 
                             trace=TRUE)
optrp1

#' Run optimization on maximum return portfolio with leverage, long only,
#' and factor exposure constraints using random portfolios as the optimization
#' engine.
optrp2 <- optimize.portfolio(R=ret, portfolio=pspec, 
                             constraints=list(lev_constr, lo_constr, exp_constr), 
                             objectives=list(ret_obj), 
                             optimize_method="random", 
                             search_size=2000, 
                             trace=TRUE)
optrp2

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(optrp1$weights, optrp2$weights)



#' ---
#' title: "Leverage Exposure Constraint Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a portfolio optimization problem with
#' leverage exposure constraints.
#' 

#' Load the package and data
library(PortfolioAnalytics)
data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

#' Set up an initial portfolio object with basic constraints.
init.portf <- portfolio.spec(assets=funds)

#' Add an objective to maximize mean return per unit expected shortfall.
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES")

#' The leverage_exposure constraint type is supported for random, DEoptim, pso,
#' and GenSA solvers. The following examples use DEoptim for solving the
#' optimization problem.

#' Dollar neutral portfolio with max 2:1 leverage constraint.
dollar.neutral.portf <- init.portf
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="weight_sum", 
                                       min_sum=-0.01, max_sum=0.01)
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="box", min=-0.5, max=0.5)
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="leverage_exposure", leverage=2)

#' Run optimization using DEoptim as optimization backend.
dollar.neutral.opt <- optimize.portfolio(R=R, portfolio=dollar.neutral.portf, 
                                         optimize_method="DEoptim",
                                         search_size=20000)
dollar.neutral.opt

#' Set up leveraged portfolio with max 1.6:1 leverage exposure constraint.
leveraged.portf <- init.portf
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                  type="weight_sum", 
                                  min_sum=0.99, max_sum=1.01)
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                  type="box", min=-0.3, max=0.8)
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                  type="leverage_exposure", leverage=1.6)

#' Run optimization using DEoptim as optimization backend.
leveraged.opt <- optimize.portfolio(R=R, portfolio=leveraged.portf, 
                                    optimize_method="DEoptim",
                                    search_size=2000)
leveraged.opt




#' ---
#' title: "Maximizing Quadratic Utility Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a constrained maximimum quadratic
#' utility portfolio optimization problem.

#' Load the package and data
library(PortfolioAnalytics)
data(edhec)
R <- edhec[, 1:13]
funds <- colnames(R)

#' Construct initial portfolio with basic constraints
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.constraint(portfolio=init.portf, type="box",min=0.03,max=0.25)
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
#' Here we can set the risk_aversion parameter to control how much risk
#' is penalized.
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev",
                            risk_aversion=4)
init.portf

#' Maximizing quadratic utility can be formulated as a quardratic programming 
#' problem and solved very quickly using optimize_method="ROI". Although "StdDev"
#' was specified as an objective, the quadratic programming problem uses the 
#' variance-covariance matrix in the objective function.
maxQU.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                   optimize_method="ROI", trace=TRUE)
maxQU.lo.ROI

plot(maxQU.lo.ROI, risk.col="StdDev", 
     main=expression("Long Only Max Quadratic Utility" ~ lambda ~"=0.25"))

#' A risk aversion parameter that is very small, will effectively make the term
#' that penalizes risk zero and approximates the maximum return. Note that the
#' risk_aversion parameter must be non-zero.
init.portf$objectives[[2]]$risk_aversion <- 100

maxQU.maxret.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                       optimize_method="ROI", trace=TRUE)
maxQU.maxret.ROI

plot(maxQU.maxret.ROI, risk.col="StdDev", 
     main=expression("Long Only Max Quadratic Utility" ~ lambda ~"= 1e-6"))

#' A risk aversion parameter that is very large will heavily penalize the risk 
#' term in the objective function and approximates the minimum variance portfolio.
init.portf$objectives[[2]]$risk_aversion <- 1e6

maxQU.minvol.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                       optimize_method="ROI", trace=TRUE)
maxQU.minvol.ROI

plot(maxQU.minvol.ROI, risk.col="StdDev", 
     main=expression("Long Only Max Quadratic Utility" ~ lambda ~"= 1e6"))






