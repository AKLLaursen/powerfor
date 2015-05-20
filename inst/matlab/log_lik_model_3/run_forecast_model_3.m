% DE Spot
param_out_de_spot = zeros(730, 21);
forecast_out_de_spot = zeros(730, 1);
for ii = 355:366
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/hour_4/forecast_data_de_spot/data_%i.csv', ii));
    data_tmp = data_tmp(2:end, :);
    input_param = csvread('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/param_de_spot.csv');
    input_param = input_param(ii, :)';
    
%     theta = -ones(21, 1);
%     theta(1:17) = input_param(1:17);
%     theta(20:21) = input_param(19:20);
    
    [n, ~] = size(data_tmp);

    [param, ~] = optim_model_3(theta, data_tmp(1:(n - 1), :), data_tmp(1:(n - 1), 7));
    
    param_out_de_spot(ii, :) = param';
    forecast_out_de_spot(ii) = forecast_model_3(param, data_tmp(:, 1), ...
        data_tmp(:, 2), data_tmp(:, 7));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_3/forecast output hour 4/param_de_spot.csv', ...
    param_out_de_spot);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_3/forecast output hour 4/forecast_de_spot.csv', ...
    forecast_out_de_spot);

% DE Inraday
param_out_de_intraday = zeros(730, 21);
forecast_out_de_intraday = zeros(730, 1);
for ii = 1:89
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/hour_10/forecast_data_de_intraday/data_%i.csv', ii));
    input_param = csvread('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 10/param_de_intraday.csv');
    input_param = input_param(ii, :)';
    
    theta = ones(21, 1) * 0.2;
%     theta(10:17) = input_param(10:17);
%     theta(15) = 0.2;
%     theta(20:21) = input_param(19:20);
    theta(21) = 4;
    
    [n, ~] = size(data_tmp);

    [param, ~] = optim_model_3(theta, data_tmp(1:(n - 1), :), data_tmp(1:(n - 1), 7));
    
    param_out_de_intraday(ii, :) = param';
    forecast_out_de_intraday(ii) = forecast_model_3(param, data_tmp(:, 1), ...
        data_tmp(:, 2), data_tmp(:, 7));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_3/forecast output hour 4/param_de_intraday.csv', ...
    param_out_de_intraday);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_3/forecast output hour 4/forecast_de_intraday.csv', ...
    forecast_out_de_intraday);

% FR Spot
param_out_fr_spot = zeros(730, 21);
forecast_out_fr_spot = zeros(730, 1);
for ii = 1:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/hour_4/forecast_data_fr_spot/data_%i.csv', ii));
    data_tmp = data_tmp(2:end, :);
    input_param = csvread('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/param_fr_spot.csv');
    input_param = input_param(ii, :)';
    
%     theta = -ones(21, 1);
%     theta(1:17) = input_param(1:17);
%     theta(20:21) = input_param(19:20);
    
    [n, ~] = size(data_tmp);

    [param, ~] = optim_model_3(theta, data_tmp(1:(n - 1), :), data_tmp(1:(n - 1), 7));
    
    param_out_fr_spot(ii, :) = param';
    forecast_out_fr_spot(ii) = forecast_model_3(param, data_tmp(:, 1), ...
        data_tmp(:, 2), data_tmp(:, 7));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_3/forecast output hour 4/param_fr_spot.csv', ...
    param_out_fr_spot);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_3/forecast output hour 4/forecast_fr_spot.csv', ...
    forecast_out_fr_spot);

% FR Inraday
param_out_fr_intraday = zeros(730, 21);
forecast_out_fr_intraday = zeros(730, 1);
for ii = 352:730
    
    ii
    
    data_tmp = csvread(sprintf('C:/git/r/powerfor/inst/csv/hour_4/forecast_data_fr_intraday/data_%i.csv', ii));
    input_param = csvread('C:/git/r/powerfor/inst/matlab/log_lik_model_1/forecast output hour 4/param_fr_intraday.csv');
    input_param = input_param(ii, :)';
    
    theta = ones(21, 1) * 0.2;
    theta(1:17) = input_param(1:17);
    theta(20:21) = input_param(19:20);
    
    [n, ~] = size(data_tmp);

    [param, ~] = optim_model_3(theta, data_tmp(1:(n - 1), :), data_tmp(1:(n - 1), 7));
    
    param_out_fr_intraday(ii, :) = param';
    forecast_out_fr_intraday(ii) = forecast_model_3(param, data_tmp(:, 1), ...
        data_tmp(:, 2), data_tmp(:, 7));
    
end
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_3/forecast output hour 4/param_fr_intraday.csv', ...
    param_out_fr_intraday);
csvwrite('C:/git/r/powerfor/inst/matlab/log_lik_model_3/forecast output hour 4/forecast_fr_intraday.csv', ...
    forecast_out_fr_intraday);