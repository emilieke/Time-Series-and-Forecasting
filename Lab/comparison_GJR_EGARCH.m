function q=comparison_GJR_EGARCH(cond_variance_egarch,cond_variance_gjr)
x=cond_variance_egarch; %this is the conditional variance of the GARCH model
y=cond_variance_gjr; %this is the conditional variance of the GJR model
n=size(x,1);
ax=1:1:n;
subplot(1,2,1); plot(ax,x,ax,y);
title('Plot of the Conditional Variances-EGARCH--GJR'); 
hleg1 = legend('EGARCH','GJR');
xlim([1 6106]);
subplot(1,2,2); scatter(x,y);
xlabel('EGARCH')
ylabel('GJR')
title('Scatter Plot of the Conditional Variances-EGARCH--GJR');
q=1;
