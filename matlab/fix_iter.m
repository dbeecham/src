function c = fix_iter(f, guess, e, max_iter)

    a = f(guess);
    
    for n = 1:max_iter
        b = f(a);
        
        if abs(b - a) <= e
            c = b;
            return;
        end
        
        a = b;
    end
    
    error('Could not diverge.');
end

