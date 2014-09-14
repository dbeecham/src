function [a, b] = l2u4()
    % bisect: function, low_guess, high_guess, error_tolerance, iterations
    % -> root or error
    
    a = bisect(@(x) sumks(x) - 1.6, 0, 100, 0.00001, 1000);
    b = bisect(@(x) sumks(x) - 1.62, 0, 100, 0.00001, 1000);
end