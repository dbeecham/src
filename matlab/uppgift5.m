function o = uppgift5(N)
    f = @(n) 1 / n^2;
    o = sum(arrayfun(f, 1:N));
end