function [] = lab6()
    f = @(x) 2 - x - exp(x);
    g = @(x) -1 - exp(x);
    
    fplot(f, [-1, 1]);
    [guess, ~] = ginput(1);
    
    disp(newton_rapson(f, g, guess, 0.0000000001, 30));
end
