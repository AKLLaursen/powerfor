function [price_out] = forecast_model_1(theta, price, week_dum)

price_est = price(1:end - 1);
week_dum_est = week_dum(1:end - 1);
week_dum_for = week_dum(end);

[n, ~] = size(price_est);

kappa_0 = theta(1);
kappa_1 = theta(2);
kappa_2 = theta(3);
kappa_3 = theta(4);
kappa_4 = theta(5);
kappa_5 = theta(6);
kappa_6 = theta(7);

phi = theta(8:14); 

omega = theta(15);
alpha = theta(16);
beta = theta(17);

lambda = theta(18);
mu = theta(19);
sigma = theta(20);

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
    for p = 1:7
        x_sum(ii) = x_sum(ii) + phi(p) * x(ii - p);
    end
    h(ii) = omega + alpha * e(ii - 1) ^ 2 + beta * h(ii - 1);
    e_1 = (x(ii) / (1 - lambda) - x_sum(ii)) / sqrt(h(ii));
    e_2 = (x(ii) / lambda - x_sum(ii) - sqrt(h(ii)) * e_1 - mu) / sigma;
    e(ii) = (1 - lambda) * e_1 + lambda * e_2;
end

h_out = omega + alpha * e(end) ^ 2 + beta * h(end);

x_sum_out = 0;
for p = 0:6
    x_sum_out = x_sum_out + phi(p + 1) * x(n - p);
end
x_1_out = x_sum_out;
x_2_out = x_1_out + mu;

x_out = (1 - lambda) * x_1_out + lambda * x_2_out;

price_out = s_out + x_out;

end