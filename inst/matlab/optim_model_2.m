function [theta_hat, hessian] = optim_model_2(theta, input_matrix)

[p, ~] = size(theta);

options  =  optimset('fmincon');
options = optimset(options, 'Diagnostics', 'on', 'Display', 'iter', ...
    'LargeScale', 'on', 'MaxFunEvals',  100000, 'MaxIter', 10000);

 
b = zeros(3, 1);
A = zeros(3, p);
A(1, 15) = -1;
A(2, 16) = -1;
A(3, 17) = -1;

[theta_hat, ~, ~, ~, ~, ~, hessian] = fmincon(@(x) ... 
    log_lik_model_2(x, input_matrix(:, 1), input_matrix(:, 2), input_matrix(:, 3:5)), ...
    theta, A, b, [], [], [], [], [], options);
end

