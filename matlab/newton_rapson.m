function c = newton_rapson(f, g, guess, e, max_iter)
    a = f(guess);
    
    if abs(a) <= e
        c = a;
        return;
    end
    
    for n=1:max_iter
        b = a - (f(a) / g(a));
        
        if abs(b) <= e
            c = b;
            return;
        end
        
        a = b;
    end
    
    error('Could not diverge.');
end

