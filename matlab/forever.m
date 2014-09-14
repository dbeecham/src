function [] = forever(f)
    while 1
        f()
    end
end