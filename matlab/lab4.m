function [] = lab4
    f = @(x) x^3 + 5*x + 1
    fplot(f, [-2, 2])
    [xs, ~] = ginput(2);
    disp(bisect(f, xs(1), xs(2), 0.0000001, 10000));
end
