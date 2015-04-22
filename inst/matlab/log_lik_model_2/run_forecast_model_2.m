% DE Spot
param_out_de_spot = zeros(730, 23);
forecast_out_de_spot = zeros(730, 1);
for ii = 1:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/forecast_data_de_spot/data_%i.csv', ii));
    data_tmp = data_tmp(2:end, :);
    
    [n, ~] = size(data_tmp);
    
    theta = ones(23, 1) * 0.2;
    theta(8) = mean(data_tmp(:, 1));
    theta(19:21) = -0.1;
    theta(23) = 2;

    [param, ~] = optim_model_2(theta, data_tmp(1:(n - 1), :));
    
    param_out_de_spot(ii, :) = param';
    forecast_out_de_spot(ii) = forecast_model_2(param, data_tmp(:, 1), ...
        data_tmp(:, 2), data_tmp(:, 3:5));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_2/forecast output/param_de_spot.csv', ...
    param_out_de_spot);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_2/forecast output/forecast_de_spot.csv', ...
    forecast_out_de_spot);

% DE Inraday
param_out_de_intraday = zeros(730, 23);
forecast_out_de_intraday = zeros(730, 1);
for ii = 63:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/forecast_data_de_intraday/data_%i.csv', ii));
    input_param = csvread('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output/param_de_intraday.csv');
    input_param = input_param(ii, :)';
    
    theta = ones(23, 1) * 0.2;
    theta(1:7) = input_param(8:14);
    theta(8:14) = input_param(1:7);
    theta(15:17) = input_param(15:17);
    theta(22:23) = input_param(19:20);
    
    [n, ~] = size(data_tmp);

    [param, ~] = optim_model_2(theta, data_tmp(1:(n - 1), :));
    
    param_out_de_intraday(ii, :) = param';
    forecast_out_de_intraday(ii) = forecast_model_2(param, data_tmp(:, 1), ...
        data_tmp(:, 2), data_tmp(:, 3:5));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_2/forecast output/param_de_intraday.csv', ...
    param_out_de_intraday);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_2/forecast output/forecast_de_intraday.csv', ...
    forecast_out_de_intraday);

% FR Spot
param_out_fr_spot = zeros(730, 23);
forecast_out_fr_spot = zeros(730, 1);
for ii = 1:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/forecast_data_fr_spot/data_%i.csv', ii));
    data_tmp = data_tmp(2:end, :);
    
    [n, ~] = size(data_tmp);
    
    theta = ones(23, 1) * 0.2;
    theta(8) = mean(data_tmp(1:(n - 1), 1));
    theta(23) = 2;

    [param, ~] = optim_model_2(theta, data_tmp(1:(n - 1), :));
    
    param_out_fr_spot(ii, :) = param';
    forecast_out_fr_spot(ii) = forecast_model_2(param, data_tmp(:, 1), ...
        data_tmp(:, 2), data_tmp(:, 3:5));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_2/forecast output/param_fr_spot.csv', ...
    param_out_fr_spot);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_2/forecast output/forecast_fr_spot.csv', ...
    forecast_out_fr_spot);

% FR Inraday
param_out_fr_intraday = zeros(730, 23);
forecast_out_fr_intraday = zeros(730, 1);
for ii = 1:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/forecast_data_fr_intraday/data_%i.csv', ii));
        
    [n, ~] = size(data_tmp);
    
    theta = ones(23, 1) * 0.2;
    theta(8) = mean(data_tmp(1:(n - 1), 1));
    theta(23) = 2;

    [param, ~] = optim_model_2(theta, data_tmp(1:(n - 1), :));
    
    param_out_fr_intraday(ii, :) = param';
    forecast_out_fr_intraday(ii) = forecast_model_2(param, data_tmp(:, 1), ...
        data_tmp(:, 2), data_tmp(:, 3:5));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_2/forecast output/param_fr_intraday.csv', ...
    param_out_fr_intraday);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_2/forecast output/forecast_fr_intraday.csv', ...
    forecast_out_fr_intraday);