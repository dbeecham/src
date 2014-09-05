function x = reduce(f, coll, xs) 
    for i = numel(xs)
        coll = f(coll, xs(i))
    end
    x = coll
end

