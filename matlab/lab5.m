function [] = lab5(f, g, e, max_iter)
    fplot(f, [-10, 10]);
    [guess, ~] = ginput(1);
    
    disp(fix_iter(g, guess, e, max_iter));
end

