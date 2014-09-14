function f = randdist()
    results = zeros(10);
    
    function [] = func()
        r = floor(rand * 10) + 1;
        results(r) = results(r) + 1;
        plot(results)
    end

    f = @func;
end