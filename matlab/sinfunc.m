function o = sinfunc(N)
    o = 0;
    for m = 1:N
        for n = 1:N
            o = o + sin((pi * (m + n))/(2 * n)) * sin((pi * m)/N);
        end
    end
    o = o * (1/(N^2));
end