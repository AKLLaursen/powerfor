function [price_out] = forecast_model_2(theta, price, week_dum, seas_dum)

price_est = price(1:end - 1);
week_dum_est = week_dum(1:end - 1);
week_dum_for = week_dum(end);
seas_dum_est = seas_dum(1:end - 1, :);
seas_dum_for = seas_dum(end, :);

[n, ~] = size(price_est);

phi = theta(1:7); 

kappa_0 = theta(8);
kappa_1 = theta(9);
kappa_2 = theta(10);
kappa_3 = theta(11);
kappa_4 = theta(12);
kappa_5 = theta(13);
kappa_6 = theta(14);

omega = theta(15);
alpha = theta(16);
beta = theta(17);

eta_0 = theta(18);
eta_1 = theta(19);
eta_2 = theta(20);
eta_3 = theta(21);

mu = theta(22);
sigma = theta(23);

t = (1:n)';

s = kappa_0 + kappa_1 * t + ...
    kappa_2 * sin((t + kappa_3) * 2 * pi / 365) + ...
    kappa_4 * sin((t + kappa_5) * 4 * pi / 365) + ...
    kappa_6 * week_dum_est;

s_out = kappa_0 + kappa_1 * (t(end) + 1) + ...
    kappa_2 * sin((t(end) + 1 + kappa_3) * 2 * pi / 365) + ...
    kappa_4 * sin((t(end) + 1 + kappa_5) * 4 * pi / 365) + ...
    kappa_6 * week_dum_for;

x = price_est - s;

x_sum = zeros(n, 1);
h = zeros(n, 1);
e = zeros(n, 1);

h(7) = omega;

for ii = 8:n
    lambda = eta_0 + eta_1 * seas_dum_est(ii,1) + eta_2 * seas_dum_est(ii,2) + ...
    eta_3 * seas_dum_est(ii,3);
    for p = 1:7
        x_sum(ii) = x_sum(ii) + phi(p) * x(ii - p);
    end
    h(ii) = omega + alpha * e(ii - 1) ^ 2 + beta * h(ii - 1);
    e_1 = (x(ii) / (1 - lambda) - x_sum(ii)) / sqrt(h(ii));
    e_2 = (x(ii) / lambda - x_sum(ii) - sqrt(h(ii)) * e_1 - mu) / sigma;
    e(ii) = (1 - lambda) * e_1 + lambda * e_2;
end

lambda_out = eta_0 + eta_1 * seas_dum_for(1) + eta_2 * seas_dum_for(2) + ...
    eta_3 * seas_dum_for(3);
h_out = omega + alpha * e(end) ^ 2 + beta * h(end);

x_sum_out = 0;
for p = 0:6
    x_sum_out = x_sum_out + phi(p + 1) * x(n - p);
end
x_1_out = x_sum_out;
x_2_out = x_1_out + mu;

x_out = (1 - lambda_out) * x_1_out + lambda_out * x_2_out;

price_out = s_out + x_out;

end