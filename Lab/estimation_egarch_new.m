function [cond_variance_egarch]=estimation_egarch_new(namefile)

myfts=ascii2fts(namefile);
retsp=fts2mat(myfts);

%First we set the structure of the model with the function garchset

retsp1=retsp-ones(size(retsp,1),1)*mean(retsp);
Mdl=egarch(1,1);
[EstMdl] = estimate(Mdl,retsp1); 
[cond_variance_egarch] = infer(EstMdl,retsp1);
subplot(2,2,1); plot(retsp);




title('Plot of S&P 500 Returns');
ylabel('Returns in %');
legend('hide');
grid('off');
xlim([1 6613]);

Innovations=retsp1;
subplot(2,2,2); plot(Innovations);
title('Innovations');
legend('hide');
grid('off');
xlim([1 6613]);


subplot(2,2,3); plot(cond_variance_egarch);
title('Conditional Variance');
legend('hide');
grid('off');
xlim([1 6613]);

%construct the series os standardized residuals
sd_residuals=retsp1./cond_variance_egarch.^0.5;

subplot(2,2,4); plot(sd_residuals);
title('Standardized Residuals');
legend('hide');
grid('off');
xlim([1 6613]);
hold off





