function f = partial2(F, a)
    function o = func(b)
        o = F(a, b);
    end

    f = @func;
end