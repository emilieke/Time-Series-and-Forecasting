function q=comparison_GARCH_GJR(cond_variance_garch,cond_variance_gjr)
x=cond_variance_garch; %this is the conditional variance of the GARCH model
y=cond_variance_gjr; %this is the conditional variance of the GJR model
n=size(x,1);
ax=1:1:n;
subplot(1,2,1); plot(ax,x,ax,y);
title('Plot of the Conditional Variances-GARCH--GJR'); 
hleg1 = legend('GARCH','GJR');
xlim([1 6613]);
%z1=1:40
%z2=1:40

subplot(1,2,2); scatter(x,y);%line(z1,z2);
xlabel('GARCH')
ylabel('GJR')
title('Scatter Plot of the Conditional Variances-GARCH--GJR');


q=1;
