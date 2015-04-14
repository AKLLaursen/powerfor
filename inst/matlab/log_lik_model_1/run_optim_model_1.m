function run_optim_model_1(country)

data_de_spot = csvread(sprintf('C:/git/r/powerfor/inst/csv/data_%s_spot.csv', country));
data_de_intraday = csvread(sprintf('C:/git/r/powerfor/inst/csv/data_%s_intraday.csv', country));

theta_s = ones(20, 1) * 0.2;
theta_s(1) = mean(data_de_spot(:, 1));

if strcmp(country, 'fr') == 1
    theta_s(20) = 2;
end

[param_s, hessian_s] = optim_model_1(theta_s, data_de_spot);
std_s = sqrt(diag(inv(hessian_s)));

out_s = [param_s, std_s];

theta_i = ones(20, 1) * 0.2;
theta_i(1) = mean(data_de_intraday(:, 1));
theta_i(20) = 2;

[param_i, hessian_i] = optim_model_1(theta_i, data_de_intraday);
std_i = sqrt(diag(inv(hessian_i)));

out_i = [param_i, std_i];

csvwrite(sprintf('C:/git/r/powerfor/inst/matlab/log_lik_model_11/output/fit_model_11_%s_spot.csv', country), out_s);
csvwrite(sprintf('C:/git/r/powerfor/inst/matlab/log_lik_model_11/output/fit_model_11_%s_intraday.csv', country), out_i);

end