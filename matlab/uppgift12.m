function [] = uppgift12()
    for a = -99:0
        for b = 1:3
            for c = [10, 100, 1000]
                fprintf(1, '%i %i %i\n', a, b, c);
            end
        end
    end
end