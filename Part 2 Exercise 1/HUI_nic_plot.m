% ----------------------------------------------------------------------------------------------------
% Part 2, HOMEWORK 1
% Emilie Engen, 100356077

% ----------------------------------------------------------------------------------------------------
function p=HUI_nic_plot(filename)

% Create a financial time series object
hui_fts=ascii2fts(filename);

% Convert time series data to matrix format
hui_return=fts2mat(hui_fts);

% ----------------------------------------------------------------------------------------------------
% Compute the prediction error
hui_return_1=hui_return-ones(size(hui_return,1),1)*mean(hui_return);

% Compute the unconditional variance
ucond_sd = mean(hui_return_1);
ucond_var = ucond_sd.^2;

% Lag the Innovations
innovationsL = lagmatrix(hui_return_1,1);
innovationsL=innovationsL(2:size(hui_return_1,1),1);

% ----------------------------------------------------------------------------------------------------
% Compute the conditional variance GARCH
alpha_0 = 2.41953;
alpha_1 = 2.80913e-08;
beta_1 = 0.75748;
cond_var_garch = alpha_0 + alpha_1*innovationsL.^2 + beta_1*ucond_var;
X_garch = [innovationsL cond_var_garch];
X_garch = sortrows(X_garch,1);

% ----------------------------------------------------------------------------------------------------
% Compute the conditional variance EGARCH

omega_ = 0.435225;
beta_ = 0.8102;
gamma_2 = 0.0531174;
gamma_1 = -0.0180886;

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

cond_var_egarch = ucond_var^(2*beta_)*exp(omega_).*exp(innovationsL.*((gamma_1+gamma_2.*d)/ucond_var)*10^(-34));

X_egarch = [innovationsL cond_var_egarch];
X_egarch = sortrows(X_egarch,1);

% ----------------------------------------------------------------------------------------------------
% Compute the conditional variance EGARCH
% Create dummy variable for negative lags
d=zeros(size(innovationsL,1),1);
for i=1:size(innovationsL,1)
    if (innovationsL(i,1)<0)
        d(i,1)=1;
    end
end

gamma_gjr = 2.06126e-06;
alpha_gjr = 2.97741e-09;
beta_gjr = 0.994259;
lambda_gjr = 0.00981375;
cond_var_gjr = gamma_gjr + beta_gjr*ucond_var + (alpha_gjr + lambda_gjr.*d).*innovationsL.^2;

X_gjr = [innovationsL cond_var_gjr];
X_gjr = sortrows(X_gjr,1);

% ----------------------------------------------------------------------------------------------------
% Plot NIC
%plot(X_gjr(:,1),X_garch(:,2),X_gjr(:,1),X_egarch(:,2),X_gjr(:,1),X_gjr(:,2));
plot(X_gjr(:,1),X_gjr(:,2))
title('Plot of the News Impact Curve: GARCH');
xlabel('Prediction error');
ylabel('Conditional variance');
%legend('GARCH','EGARCH','GJR');
grid('off');