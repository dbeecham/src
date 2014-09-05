function f = memoize( F )
    % Memoization of function F. Stores
    % already calculated values in xs and ys.
    
    % xs holds input values to F
    xs = [];
    
    % ys hold output values from F
    ys = [];
    
    function out = func(in)
        % Table lookup
        i = find(in == xs);
        
        % Value not found! Call F and store value.
        if isempty(i)
            out = F(in);
            xs(end + 1) = in;
            ys(end + 1) = out;
        else
            out = ys(i);
        end
    end

    f = @func;

end

