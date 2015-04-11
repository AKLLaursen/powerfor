function [log_lik] = log_lik_model_1(theta, price, week_dum)

[n, ~] = size(price);

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
    kappa_6 * week_dum;

x = price - s;

x_sum = zeros(n, 1);
h = zeros(n, 1);
e = zeros(n, 1);
log_l = zeros(n, 1);

h(7) = omega;

for ii = 8:n
    for p = 1:7
        x_sum(ii) = x_sum(ii) + phi(p) * x(ii - p);
    end
    h(ii) = omega + alpha * e(ii - 1) ^ 2 + beta * h(ii - 1);
    e_1 = (x(ii) / (1 - lambda) - x_sum(ii)) / sqrt(h(ii));
    e_2 = (x(ii) / lambda - x_sum(ii) - sqrt(h(ii)) * e_1 - mu) / sigma;
    e(ii) = (1 - lambda) * e_1 + lambda * e_2;
    
    part_1 = exp(-(price(ii) - s(ii) - x_sum(ii) - mu) ^ 2 / ...
        (2 * (h(ii) + sigma ^ 2))) * ...
        1 / sqrt(2 * pi * (h(ii) + sigma ^ 2));
    part_2 = exp(-(price(ii) - s(ii) - x_sum(ii)) ^ 2 / (2 * h(ii))) * ...
        1 / sqrt(2 * pi * h(ii));
    
    log_l(ii) = log(lambda * part_1 + (1 - lambda) * part_2);
end

log_lik = -sum(log_l(8:end));
end