function f = fib_m()
% Memoized version of fib.
    xs = [1 2];
    ys = [1 1];
    
    function o = fibm(x)
        i = find(xs == x);
        if isempty(i)
            % Calculate fib x
            o = fibm(x - 1) + fibm(x - 2);
            
            % Store fib x
            xs(end + 1) = x;
            ys(end + 1) = o;
        else
            o = ys(i);
        end
    end
    f = @fibm;
end

