function f = comb(F, G)
% comb :: (a -> b) -> (b -> c) -> (a -> c)
% Combines two functions into one, such that
% comb(F, G)(x) = F(G(x))

    f = @func;
    function out = func(in)
        out = G(in);
        out = F(out);
    end
end

