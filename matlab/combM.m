function f = combM(F, G)
% combM :: (a -> b) -> (b -> M ())
% Combines two functions into one, such that
% comb(F, G)(x) = F(G(x)). Ignores result.

    f = @func;
    function out = func(in)
        out = G(in);
        F(out);
    end
end
