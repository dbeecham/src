function o = bisect(f, a, b, tol, nmax)

    % xnor truth table:
    %  a  |  b  |  o
    %-----+-----+-----
    %  0  |  0  |  1
    %  0  |  1  |  0
    %  1  |  0  |  0
    %  1  |  1  |  1
    %-----+-----+-----
    function o = xnor(a, b)
        o = not(xor(a, b));
    end


    for n=1:nmax
        
        % Bisect
        c = (a + b)/2;
        
        % Is within range.
        if f(c) == 0 || (b - a)/2 < tol
            o = c;
            return;
        end
        
        if xnor(f(c) < 0, f(a) < 0)
            a = c;
        else
            b = c;
        end
    end
    
    error('no');
    
end