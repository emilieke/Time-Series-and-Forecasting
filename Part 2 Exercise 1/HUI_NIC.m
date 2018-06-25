% ----------------------------------------------------------------------------------------------------
% Part 2, HOMEWORK 1
% Emilie Engen, 100356077

% ----------------------------------------------------------------------------------------------------
function n=HUI_NIC(filename)

% Create a financial time series object
hui_fts=ascii2fts(filename);

% Convert time series data to matrix format
hui_return=fts2mat(hui_fts);

dax99   = hui_return;        % first line is date, second XetraDAX 1999
lndax99 = log(dax99);
ret     = diff(lndax99);


lagret = ret;				 % create the vector with lagged returns
ret    = ret(2:length(ret), :);
lagret = lagret(1:length(lagret) - 1, :);


[rx rf] = sker(lagret, ret, 0.01, 100, 'gau');


mh(:,1)      = rx;
mh(:,2)      = rf;
NewsImpCurve = mh(:, 2) .^ 2;
NewsImpCurve = [mh(:,1), NewsImpCurve];

l = 1;
for i = 1:length(NewsImpCurve)
    if NewsImpCurve(i,1) < 0.042
        NewsImpCurve1(l, :) = NewsImpCurve(i, :);
        l = l + 1;
    end
end

m = 1;
for i = 1:length(NewsImpCurve1)
    if NewsImpCurve1(i, 1) > -0.04
        NewsImpCurve2(m, :) = NewsImpCurve1(i, :);
        m = m + 1;
    end
end

NewsImpCurve = NewsImpCurve2;
plot(NewsImpCurve(:, 1), NewsImpCurve(:, 2), 'LineWidth', 2)
xlim([-0.04, 0.045])
title('DAX News Impact Curve', 'FontWeight', 'bold', 'FontSize', 16)
xlabel('Lagged Returns', 'FontWeight', 'bold', 'FontSize', 16)
ylabel('Conditional Variance', 'FontWeight', 'bold', 'FontSize', 16)
box on
set(gca,'FontSize', 16, 'LineWidth', 2, 'FontWeight', 'bold');