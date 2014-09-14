function [] = uppgift11()
    for n = 1:9
        for m = [1, 10, 100, 1000]
            fprintf(1, '%i %i\n', n , m);
        end
    end
end