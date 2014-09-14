function o = reverse(xs)
    for n=1:length(xs)
        o(n) = xs(end-n+1);
    end
end