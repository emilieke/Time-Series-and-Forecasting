% ----------------------------------------------------------------------------------------------------
% Part 2, HOMEWORK 1
% Emilie Engen, 100356077

% ----------------------------------------------------------------------------------------------------
function p=HUI_plot()

% Create a financial time series object
hui_fts=ascii2fts('HUI_returns.txt');

% Convert time series data to matrix format
hui_return=fts2mat(hui_fts);

% Create a financial time series object
hui_prices=ascii2fts('HUI_prices.txt');

% Convert time series data to matrix format
hui_prices_2=fts2mat(hui_prices);

% ----------------------------------------------------------------------------------------------------

n=size(hui_return,1);
ax=1:1:n;

hui_return = hui_return+100;

% Plot the time series
plot(ax,hui_return,ax,hui_prices_2)
title('Plot of \^HUI Returns and Prices');
ylabel('Returns in % / Prices');
legend('hide');
xlim([1 n]);
grid('off');
