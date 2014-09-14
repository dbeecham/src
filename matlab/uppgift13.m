function [] = uppgift13()
    xs = linspace(-pi, pi);
    ys = xs;
    zs = sin(xs) .* cos(ys);
    plot3(xs, ys, zs);
end