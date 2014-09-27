function o = nth(n, f)
    % Extracts the nth value of the discrete value-generating
    % side-effecting function f (a very strange notion indeed).
    
    i = 0;
    while i < n
        f();
        i = i + 1;
    end
    o = f();
end
