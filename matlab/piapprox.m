function f = piapprox()
    num_inside = 0;
    num_all = 0;
    
    function o = func()
        % norm([rand*2-1 rand*2-1])
        % sqrt(rand^2 + rand^2)
        if norm([rand rand]) <= 1
            num_inside = num_inside + 1;
        end
        num_all = num_all + 1;
        
        o = 4 * (num_inside / num_all);
    end

    f = @func;
end