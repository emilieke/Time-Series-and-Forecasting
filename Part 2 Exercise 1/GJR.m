% ----------------------------------------------------------------------------------------------------
% Part 2, HOMEWORK 1
% Emilie Engen, 100356077

% ----------------------------------------------------------------------------------------------------
function gj=GJR()

% Create a financial time series object
hui_fts=ascii2fts('HUI_returns.txt');

% Convert time series data to matrix format
hui_return=fts2mat(hui_fts);

% ----------------------------------------------------------------------------------------------------
% Obtaining the estimated GARCH model
hui_return_1=hui_return-ones(size(hui_return,1),1)*mean(hui_return);
Model=gjr(1,1);
[EstMdl_GJR,EstParamCov3, logL3] = estimate(Model,hui_return_1);
numParams(3)=sum(any(EstParamCov3));
[cond_variance_gjr] = infer(EstMdl_GJR,hui_return_1);

% Lenght of time series
T=size(hui_return_1,1);

% Calculate AIC and BIC
[aic_gjr,bic_gjr]=aicbic(logL3, numParams(3),T);

% ----------------------------------------------------------------------------------------------------
% Compute the Innovations
Innovations=hui_return_1;

% Plot the conditional variance
subplot(1,2,1); plot(cond_variance_gjr);
title('Conditional Variance');
legend('hide');
xlim([1 T]);
grid('off');

% ----------------------------------------------------------------------------------------------------
% Compute the standardized residuals
sd_residuals=hui_return_1./cond_variance_gjr.^0.5;

% Plot the standardized residuals
subplot(1,2,2); plot(sd_residuals);
title('Standardized Residuals');
legend('hide');
xlim([1 T]);
grid('off');

% ----------------------------------------------------------------------------------------------------
% The Engle and NG (1993) test

% Compute the squared residuals
v2=sd_residuals.^2;
v2=v2(2:size(sd_residuals,1),1);

% Lag the Innovations
innovationsL = lagmatrix(Innovations,1);
innovationsL=innovationsL(2:size(Innovations,1),1);

% Create dummy variable for negative lags
d=zeros(size(innovationsL,1),1);
for i=1:size(innovationsL,1)
    if (innovationsL(i,1)<0)
        d(i,1)=1;
    end
end

% Regression of the test

% Create a matrix with the observations of the regressors
X=[ones(size(innovationsL,1),1) d d.*innovationsL innovationsL.*(1-d)];

[b_gjr,bint,r,rint,stats]=regress(v2,X);

test=size(v2,1).*stats(1,1);
pvalue_gjr = 1-chi2cdf(test,3);

C='We reject the null hypothesis';
if pvalue_gjr<0.05
    disp(C);
end





