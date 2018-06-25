function q=comparison_GARCH_EGARCH(cond_variance_garch,cond_variance_egarch,cond_variance_gjr)
x=cond_variance_garch; %this is the conditional variance of the GARCH model
y=cond_variance_egarch %this is the conditional variance of the EGARCH model
z=cond_variance_gjr; %this is the conditional variance of the GJR model
n=size(x,1);
ax=1:1:n;
subplot(2,2,1); plot(ax,x,ax,y,ax,z);
title('Plot of the Conditional Variances-GARCH--EGARCH--GJR'); 
hleg1 = legend('GARCH','EGARCH','GJR');
xlim([1 6613]);
%xlim([1 6106]);
%z1=1:30;
%z2=1:30;
subplot(2,2,2); scatter(x,y);%line(z1,z2);
xlabel('GARCH')
ylabel('EGARCH')
title('Scatter Plot of the Conditional Variances-GARCH--EGARCH');
subplot(2,2,3); scatter(z,y); %line(z1,z2);
xlabel('GJR')
ylabel('EGARCH')
title('Scatter Plot of the Conditional Variances-GJR--EGARCH');
q=1;
