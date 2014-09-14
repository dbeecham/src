function [] = uppgift7()
    for n = 1:7
        xs = linspace(1, 2*pi);
        ys = sin(n .* xs);
        figure(n);
        plot(xs, ys);
        pause(0.5);
    end
end