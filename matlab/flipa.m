function f = flipa(F)
    function o = func(a, b)
        o = F(b, a);
    end
    
    f = @func;
end