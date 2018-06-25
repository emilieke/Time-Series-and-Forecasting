function [s_mean, s_sd, s_k, s_sk]=statistics(namefile)

%This function calculates simple statistics of the data. The students
%should enter the name of the file between ''as 'retsp.txt'

%The function asciifts creates a financial time series object tsobj from
%the ASCII file named filename. 
%This form of the function can only read a data file without time-of-day 
%information and create a financial time series object without time information. 
%If time information is present in the ASCII file, an error message appears.
myfts=ascii2fts(namefile);
subplot(3,2,1); plot(myfts)
%The next three functions write the title of the graph, the name of the
%y-axes hide the legend and the gride
title('Plot of S&P 500 Returns');
ylabel('Returns in %');
legend('hide');
grid('off');
%The function fts2mat tsmat = fts2mat(tsobj) takes the data series in the 
%financial time series object tsobj and puts them into the matrix tsmat as columns. 
%The order of the columns is the same as the order of the data series in the object tsobj. 
retsp=fts2mat(myfts);
s_mean=mean(retsp);
s_sd=sqrt(var(retsp));
s_k=kurtosis(retsp);
s_sk=skewness(retsp);
%Next we obtain the squared observations
retsp2=(retsp).^2;
subplot(3,2,2); plot(retsp2)
title('Plot of Squared Returns');
xlim([1 6106]);
legend('hide');
grid('off');
%The next function computes the autocorrelation function of returns and
%squared returns
subplot(3,2,3); autocorr(retsp,50)
title('ACF of Returns');
subplot(3,2,4); autocorr(retsp2,50)
title('ACF of Squared Returns');
%The next function calculates the cross-correlation between squared returns and 
% past returns

subplot(3,2,5);crosscorr(retsp2,retsp,50)
title('CCF Between Squared Returns and Past Returns');

