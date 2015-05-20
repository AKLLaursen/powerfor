% DE Spot
param_out_de_spot = zeros(730, 20);
forecast_out_de_spot = zeros(730, 1);
for ii = 1:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/hour_4/forecast_data_de_spot/data_%i.csv', ii));
    data_tmp = data_tmp(2:end, :);
    
    [n, ~] = size(data_tmp);
    
    theta = ones(20, 1) * 0.2;
    theta(1) = mean(data_tmp(1:(n - 1), 1));

    [param, ~] = optim_model_1(theta, data_tmp(1:(n - 1), :));
    
    param_out_de_spot(ii, :) = param';
    forecast_out_de_spot(ii) = forecast_model_1(param, data_tmp(:, 1), ...
        data_tmp(:, 2));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/param_de_spot.csv', ...
    param_out_de_spot);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/forecast_de_spot.csv', ...
    forecast_out_de_spot);

% DE Intraday
param_out_de_intraday = zeros(730, 20);
forecast_out_de_intraday = zeros(730, 1);
for ii = 1:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/hour_4/forecast_data_de_intraday/data_%i.csv', ii));
        
    [n, ~] = size(data_tmp);
    
    theta = ones(20, 1) * 0.2;
    theta(1) = mean(data_tmp(1:(n - 1), 1));
    theta(20) = 2;

    [param, ~] = optim_model_1(theta, data_tmp(1:(n - 1), :));
    
    param_out_de_intraday(ii, :) = param';
    forecast_out_de_intraday(ii) = forecast_model_1(param, data_tmp(:, 1), ...
        data_tmp(:, 2));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/param_de_intraday.csv', ...
    param_out_de_intraday);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/forecast_de_intraday.csv', ...
    forecast_out_de_intraday);

% FR Spot
param_out_fr_spot = zeros(730, 20);
forecast_out_fr_spot = zeros(730, 1);
for ii = 1:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/hour_4/forecast_data_fr_spot/data_%i.csv', ii));
    data_tmp = data_tmp(2:end, :);
    
    [n, ~] = size(data_tmp);
    
%     theta = ones(20, 1) * 0.2;
%     theta(1) = mean(data_tmp(1:(n - 1), 1));
%     theta(20) = 2;

    [param, ~] = optim_model_1(theta, data_tmp(1:(n - 1), :));
    
    param_out_fr_spot(ii, :) = param';
    forecast_out_fr_spot(ii) = forecast_model_1(param, data_tmp(:, 1), ...
        data_tmp(:, 2));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/param_fr_spot.csv', ...
    param_out_fr_spot);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/forecast_fr_spot.csv', ...
    forecast_out_fr_spot);

% FR Inraday
param_out_fr_intraday = zeros(730, 20);
forecast_out_fr_intraday = zeros(730, 1);
for ii = 1:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/hour_4/forecast_data_fr_intraday/data_%i.csv', ii));
        
    [n, ~] = size(data_tmp);
    
    theta = ones(20, 1) * 0.2;
    theta(1) = mean(data_tmp(1:(n - 1), 1));
    theta(20) = 2;

    [param, ~] = optim_model_1(theta, data_tmp(1:(n - 1), :));
    
    param_out_fr_intraday(ii, :) = param';
    forecast_out_fr_intraday(ii) = forecast_model_1(param, data_tmp(:, 1), ...
        data_tmp(:, 2));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/param_fr_intraday.csv', ...
    param_out_fr_intraday);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/forecast_fr_intraday.csv', ...
    forecast_out_fr_intraday);