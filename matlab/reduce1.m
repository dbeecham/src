function o = reduce1(f, xs)
    o = reduce(f, f(xs(1), xs(2)), xs(3:end))
end