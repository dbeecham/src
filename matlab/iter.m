function o = iter(f, N)
    for n=1:N
        o = f();
    end
end