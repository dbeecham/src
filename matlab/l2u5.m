function [] = l2u5()
    maxy = cos(1);
    maxx = 1;
    for x=2:100
        if cos(x) > maxy
            maxy = cos(x);
            maxx = x;
        end
    end
    
    fprinf('Max value at x = %i is %i.', maxx, maxy);
end