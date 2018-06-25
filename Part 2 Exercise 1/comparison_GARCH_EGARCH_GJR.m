% ----------------------------------------------------------------------------------------------------
% Part 2, HOMEWORK 1
% Emilie Engen, 100356077

% ----------------------------------------------------------------------------------------------------
function q=comparison_GARCH_EGARCH_GJR(cond_variance_garch,cond_variance_egarch,cond_variance_gjr)

% ----------------------------------------------------------------------------------------------------
x=cond_variance_garch; %this is the conditional variance of the GARCH model
y=cond_variance_egarch; %this is the conditional variance of the EGARCH model
z=cond_variance_gjr; %this is the conditional variance of the GJR model

% ----------------------------------------------------------------------------------------------------
n=size(x,1);
ax=1:1:n;

% ----------------------------------------------------------------------------------------------------
% Plot the conditional variances for GARCH and EGARCH
subplot(2,2,1); plot(ax,x,ax,y,ax,z);
title('Plot of the Conditional Variances: GARCH, EGARCH and GJR'); 
hleg1 = legend('GARCH','EGARCH','GJR');
xlim([1 n]);

% ----------------------------------------------------------------------------------------------------
% Scatter plot of the conditional variances
%z1=8:12;
%z2=8:12;
subplot(2,2,2); scatter(x,y);%line(z1,z2);
xlabel('GARCH')
ylabel('EGARCH')
title('Scatter Plot of the Conditional Variances: GARCH and EGARCH');

subplot(2,2,3); scatter(z,y); %line(z1,z2);
xlabel('GJR')
ylabel('EGARCH')
title('Scatter Plot of the Conditional Variances: GJR and EGARCH');

subplot(2,2,4); scatter(x,z); %line(z1,z2);
xlabel('GARCH')
ylabel('GJR')
title('Scatter Plot of the Conditional Variances: GARCH and GJR');
q=1;
