function [ a ] = fib( x )
    if x == 1
        a = 1;
        return
    end
    
    if x == 2
        a = 1;
        return
    end
    
    a = fib(x - 1) + fib(x - 2);
end

