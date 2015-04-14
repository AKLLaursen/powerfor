function [log_lik] = log_lik_model_2(theta, price, week_dum, seas_dum)

[n, ~] = size(price);

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
    kappa_6 * week_dum;

x = price - s;

x_ar = zeros(n, 1);
h = zeros(n, 1);
e = zeros(n, 1);
log_l = zeros(n, 1);

h(7) = omega;

for ii = 8:n
    lambda = eta_0 + eta_1 * seas_dum(ii,1) + eta_2 * seas_dum(ii,2) + ...
    eta_3 * seas_dum(ii,3);
    for p = 1:7
        x_ar(ii) = x_ar(ii) + phi(p) * x(ii - p);
    end
    h(ii) = omega + alpha * e(ii - 1) ^ 2 + beta * h(ii - 1);
    e_1 = (x(ii) / (1 - lambda) - x_ar(ii)) / sqrt(h(ii));
    e_2 = (x(ii) / lambda - x_ar(ii) - sqrt(h(ii)) * e_1 - mu) / sigma;
    e(ii) = (1 - lambda) * e_1 + lambda * e_2;
    
    part_1 = exp(-(price(ii) - s(ii) - x_ar(ii) - mu) ^ 2 / ...
        (2 * (h(ii) + sigma ^ 2))) * ...
        1 / sqrt(2 * pi * (h(ii) + sigma ^ 2));
    part_2 = exp(-(price(ii) - s(ii) - x_ar(ii)) ^ 2 / (2 * h(ii))) * ...
        1 / sqrt(2 * pi * h(ii));
    
    log_l(ii) = log(lambda * part_1 + (1 - lambda) * part_2);
end

log_lik = -sum(log_l(8:end));
end