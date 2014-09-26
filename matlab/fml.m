function [first, middle, last] = fml(xs)

    function o = evenn(x)
        o = rem(x, 2) == 0;
    end

    first = xs(1);
    last = xs(end);
    
    if evenn(length(xs))
        middle = (xs(end/2) + xs((end/2) + 1)) / 2;
    else
        middle = xs(ceil(end/2));
    end
    
end