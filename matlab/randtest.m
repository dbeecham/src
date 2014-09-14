min = 0.5;
max = 0.5;

for n=1:30000
    r = rand;
    if min > r
        min = r;
    end
    
    if max < r
        max = r;
    end
end

min
1-max