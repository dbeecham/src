function [] = l2u5()
    maxy = 0;
    maxx = 0;
    for x=1:100
        if cos(x) > max
            maxy = cos(x);
            maxx = x;
        end
    end
    
    fprinf('Max value at x = %i is %i.', maxx, maxy);
end