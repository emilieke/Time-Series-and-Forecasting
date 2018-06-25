% ----------------------------------------------------------------------------------------------------
% Part 2, HOMEWORK 1
% Emilie Engen, 100356077

% ----------------------------------------------------------------------------------------------------
function [s_mean, s_sd, s_k, s_sk, autocorr_hui, autocorr_hui_2]=HUI_analysis()

% Create a financial time series object
hui_fts=ascii2fts('HUI_returns.txt');

% ----------------------------------------------------------------------------------------------------

% Plot the time series
subplot(3,2,1); plot(hui_fts)
title('Plot of HUI Returns');
ylabel('Returns in %');
legend('hide');
grid('off');

% ----------------------------------------------------------------------------------------------------
% Calculate simple statistics of the data

% Convert time series data to matrix format
hui_return=fts2mat(hui_fts);

% Lenght of time series
T=size(hui_return,1);

% Compute mean, SD, kurtosis and skewness
s_mean=mean(hui_return);
s_sd=sqrt(var(hui_return));
s_k=kurtosis(hui_return);
s_sk=skewness(hui_return);

% ----------------------------------------------------------------------------------------------------
% Plot the squared observations

hui_return2=(hui_return).^2;
subplot(3,2,2); plot(hui_return2)
title('Plot of Squared Returns');
legend('hide');
xlim([1 T]);
grid('off');

% ----------------------------------------------------------------------------------------------------
% Plot the autocorrelation function of returns
subplot(3,2,3); autocorr(hui_return,50)
title('ACF of Returns');

[autocorr_hui] = autocorr(hui_return,10);

% Plot the autocorrelation function of squared returns
subplot(3,2,4); autocorr(hui_return2,50)
title('ACF of Squared Returns');

[autocorr_hui_2] = autocorr(hui_return2,10);

% Plot the cross-correlation between squared returns and past returns
subplot(3,2,5); crosscorr(hui_return2,hui_return,50)
title('CCF Between Squared Returns and Past Returns');
