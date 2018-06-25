%WHICH IS BETTER? WE CHECK WITH AIC
T=size(retsp1,1)

Mdl=garch(1,1); %specifying the model.
[EstMdl_GARCH,EstParamCov1, logL1] = estimate(Mdl,retsp1);
numParams(1)=sum(any(EstParamCov1))

[aic,bic]=aicbic(logL1, numParams(1),T)

Mdl=gjr(1,1);
[EstMdl_GJR, EstParamCov2, logL2] = estimate(Mdl,retsp1);
numParams(2)=sum(any(EstParamCov2))
[aic,bic]=aicbic(logL2, numParams(2),T)

Mdl=egarch(1,1); %specifying the model.
T=size(retsp1,1)
[EstMdl_EGARCH,EstParamCov3, logL3] = estimate(Mdl,retsp1);
numParams(3)=sum(any(EstParamCov3))
[aic,bic]=aicbic(logL3, numParams(3),T)
%The smallest AIC ==> IS THE BETTER MODEL ==> GJR in this case
% BIC = Schwarz criterion = BAYESIAN INFORMATION CRITERION