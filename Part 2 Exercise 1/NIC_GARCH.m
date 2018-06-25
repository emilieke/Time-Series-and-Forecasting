function n=NIC_GARCH(filename)

% Create a financial time series object
hui_fts=ascii2fts(filename);

% Convert time series data to matrix format
hui_return=fts2mat(hui_fts);

% ----------------------------------------------------------------------------------------------------
% 1.First estimate your model with your data.

% Obtaining the estimated GARCH model
hui_return_1=hui_return-ones(size(hui_return,1),1)*mean(hui_return);
Model=garch(1,1);
[EstMdl_GARCH,EstParamCov1, logL1] = estimate(Model,hui_return_1);
numParams(1)=sum(any(EstParamCov1));
[cond_variance_garch] = infer(EstMdl_GARCH,hui_return_1);

% Lenght of time series
T=size(hui_return_1,1);

% ----------------------------------------------------------------------------------------------------
% 2. Save the innovations, lag them and squared them.
Innovations=hui_return_1;

% Lag the Innovations
innovationsL = lagmatrix(Innovations,1);
innovationsL=innovationsL(2:size(Innovations,1),1);

innovationsL_2 = innovationsL.^2;

alpha_0 = 0.0798082;
alpha_1 = 0.0557854;
beta_1 = 0.934376;

% 3.Simulate a similar model your the parameters estimated in step one and calculate the variance of the returns simulated 
Model_2 = garch('Constant',alpha_0,'GARCH',beta_1, 'ARCH',alpha_1);
rng default; % For reproducibility
[sim_conditional_variance] = simulate(Model_2,length(hui_return_1));
ucond_var = mean(sim_conditional_variance);

% Construct the NIC function and plot it.
cond_var_garch = alpha_0 + alpha_1*innovationsL_2 + beta_1.*ucond_var;
X_garch = [innovationsL cond_var_garch];
X_garch = sortrows(X_garch,1);


% ----------------------------------------------------------------------------------------------------
% Compute the conditional variance EGARCH

omega_ = 0.028864;
beta_ = 0.986741;
gamma_2 = 0.124405;
gamma_1 = -0.0212121;

% 3.Simulate a similar model your the parameters estimated in step one and 
% calculate the variance of the returns simulated 
Model_2 = egarch('Constant',omega_,'GARCH',beta_, 'ARCH',gamma_2,'Leverage',gamma_1);
[sim_conditional_variance] = simulate(Model_2,length(hui_return_1));
ucond_var = mean(sim_conditional_variance);

% Create dummy variable for negative lags
d=zeros(size(innovationsL,1),1);
for i=1:size(innovationsL,1)
    if (innovationsL(i,1)<0)
        d(i,1)=-1;
    end
    if (innovationsL(i,1)>=0)
        d(i,1)=1;
    end
end

cond_var_egarch = ucond_var^(2*beta_)*exp(omega_).*exp(innovationsL.*((gamma_1+gamma_2.*d)/ucond_var));

X_egarch = [innovationsL cond_var_egarch];
X_egarch = sortrows(X_egarch,1);

% ----------------------------------------------------------------------------------------------------
% Compute the conditional variance GJR

% Create dummy variable for negative lags
d=zeros(size(innovationsL,1),1);
for i=1:size(innovationsL,1)
    if (innovationsL(i,1)<0)
        d(i,1)=1;
    end
end

gamma_gjr = 0.078289;
alpha_gjr = 0.0480548;
beta_gjr = 0.934817;
lambda_gjr = 0.0155376;

% 3.Simulate a similar model your the parameters estimated in step one and 
% calculate the variance of the returns simulated 
Model_2 = gjr('Constant',gamma_gjr,'GARCH',beta_gjr, 'ARCH',alpha_gjr,'Leverage',lambda_gjr);
[sim_conditional_variance] = simulate(Model_2,length(hui_return_1));
ucond_var = mean(sim_conditional_variance);

cond_var_gjr = gamma_gjr + beta_gjr*ucond_var + (alpha_gjr + lambda_gjr.*d).*innovationsL.^2;

X_gjr = [innovationsL cond_var_gjr];
X_gjr = sortrows(X_gjr,1);
% ----------------------------------------------------------------------------------------------------
%plot(X_egarch(:,1),X_egarch(:,2));
plot(X_garch(:,1),X_garch(:,2),X_gjr(:,1),X_gjr(:,2));
title('Plot of the News Impact Curve: GARCH, GJR');
xlabel('Prediction error');
ylabel('Conditional variance');
ylim([5 25]);
legend('GARCH','GJR');
grid('off');