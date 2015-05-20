de_spot = csvread('C:/git/r/powerfor/inst/csv/full_data_de_spot.csv');
fr_spot = csvread('C:/git/r/powerfor/inst/csv/full_data_fr_spot.csv');
de_intraday = csvread('C:/git/r/powerfor/inst/csv/full_data_de_intraday.csv');
fr_intraday = csvread('C:/git/r/powerfor/inst/csv/full_data_fr_intraday.csv');

de_spot = de_spot(:, 1);
fr_spot = fr_spot(:, 1);
de_intraday = de_intraday(:, 1);
fr_intraday = fr_intraday(:, 1);

[h, pValue, stat, cValue, reg] = adftest(de_spot, 'model', 'ARD', 'lags', 14);
[h, pValue, stat, cValue, reg] = pptest(de_spot, 'model', 'ARD', 'lags', 14);
[h, pValue, stat, cValue, reg] = kpsstest(de_spot, 'trend', false, 'lags', 14);

[h, pValue, stat, cValue, reg] = adftest(fr_spot, 'model', 'ARD', 'lags', 14)
[h, pValue, stat, cValue, reg] = pptest(fr_spot, 'model', 'ARD', 'lags', 14)
[h, pValue, stat, cValue, reg] = kpsstest(fr_spot, 'trend', false, 'lags', 14)

[h, pValue, stat, cValue, reg] = adftest(de_intraday, 'model', 'ARD', 'lags', 14);
[h, pValue, stat, cValue, reg] = pptest(de_intraday, 'model', 'ARD', 'lags', 14);
[h, pValue, stat, cValue, reg] = kpsstest(de_intraday, 'trend', false, 'lags', 14);

[h, pValue, stat, cValue, reg] = adftest(fr_intraday, 'model', 'ARD', 'lags', 14)
[h, pValue, stat, cValue, reg] = pptest(fr_intraday, 'model', 'ARD', 'lags', 14)
[h, pValue, stat, cValue, reg] = kpsstest(fr_intraday, 'trend', false, 'lags', 14)