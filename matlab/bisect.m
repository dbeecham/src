function o = bisect(f, a, b, e, nmax)

    function b = sign(x)
        if (x > 0)
            b = 1;
        elseif (x < 0)
            b = -1;
        else
            b = 0;
        end
    end


    for n=1:nmax
        
        % Bisect
        c = (a + b)/2;
        
        % Is within range.
        if abs(f(c)) < e
        %if f(c) == 0 || (b - a)/2 < e
            o = c;
            return;
        end
        
        if sign(f(c)) == sign(f(a))
            a = c;
        else
            b = c;
        end
    end
    
    error('Could not find a value ');
    
end
