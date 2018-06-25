function [cond_variance_garch]=estimation_GARCH_new(namefile)

myfts=ascii2fts(namefile);
retsp=fts2mat(myfts);


%This is another way of estimating the GARCH 
retsp1=retsp-ones(size(retsp,1),1)*mean(retsp);
Mdl=garch(1,1);
[EstMdl] = estimate(Mdl,retsp1); 
[cond_variance_garch] = infer(EstMdl,retsp1);
subplot(2,2,1); plot(myfts);

title('Plot of S&P 500 Returns');
ylabel('Returns in %');
legend('hide');
grid('off');

Innovations=retsp1;
subplot(2,2,2); plot(Innovations);
title('Innovations');
legend('hide');
grid('off');
xlim([1 6613]);



subplot(2,2,3); plot(cond_variance_garch);
title('Conditional Variance');
legend('hide');
grid('off');
xlim([1 6613]);

sd_residuals=retsp1./cond_variance_garch.^0.5;

subplot(2,2,4); plot(sd_residuals);
title('Standardized Residuals');
legend('hide');
grid('off');
xlim([1 6613]);


% Now we are going to implement the test of Engle and NG (1993)

v2=sd_residuals.^2;
v2=v2(2:size(sd_residuals,1),1);
%Here we lag the Innovations
innovationsL = lagmatrix(Innovations,1);
innovationsL=innovationsL(2:size(Innovations,1),1);

%Here we build the dummy
d=zeros(size(innovationsL,1),1);
for i=1:size(innovationsL,1)
if (innovationsL(i,1)<0)
    d(i,1)=1;
end
end

%Now we do the regression of the test
%We have to build a matrix with the observations of the regressors
X=[ones(size(innovationsL,1),1) d d.*innovationsL innovationsL.*(1-d)];

[b,bint,r,rint,stats]=regress(v2,X)

test=size(v2,1).*stats(1,1);
pvalue = 1-chi2cdf(test,3);

C='We reject the null hypothesis';
if pvalue<0.05
    disp(C);
end






