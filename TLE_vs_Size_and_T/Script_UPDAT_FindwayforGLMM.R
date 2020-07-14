packageVersion("lme4")
#reference:lme4 convergence warnings_troubleshooting.html


sp_TL.result2 <- sp_TL.result

fit.QART.lin2 <- glmer(b~std_T*std_L*quarter+(1+std_L|sp_ID),
                      data = sp_TL.result,
                      family = Gamma(link = log))
                      #,
                      #control = glmerControl(optimizer ='bobyqa' )
)




tt <- getME(fit.QART.lin2,"theta")
ll <- getME(fit.QART.lin2,"lower")
min(tt[ll==0])


derivs1 <- fit.tot.lin2@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))


library("RCurl")
afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/misc/issues/allFit.R"
eval(parse(text=getURL(afurl)))
aa <- allFit(fit.QART.lin2)
is.OK <- sapply(aa,is,"merMod")  ## nlopt NELDERMEAD failed, others succeeded
aa.OK <- aa[is.OK]
lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)

fit.tot.lin3 <- glmer(b~temp*std_L*quarter+(1+std_L|sp_ID),
                      data = sp_TL.result2,
                      family=Gamma(link="log"),
                      #nAGQ = 0,
                      #control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
                      control = glmerControl(optimizer ='bobyqa')
                      #                       tolPwrss=1e-3,
                      #                       optCtrl = list(maxfun = 2e+4))
)
(lliks <- sort(sapply(aa.OK,logLik)))
