function o = uppgift6(N)
    o = 0;
    for m = 1:N
        inner = 0;
        for n = 1:N
            inner = inner + sin((pi * (m + n))/(2 * N)) * sin((pi * m)/N);
        end
        
        o = o + inner;
    end
    o = o * (1/(N^2));
end