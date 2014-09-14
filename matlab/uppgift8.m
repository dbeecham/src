function [] = uppgift8(v)
    wave = @(t, x) sin(x - (v * t));
    plotfunc = @(f) fplot(f, [0, 2*pi]);
    
    while 1
        for t = linspace(0, 2*pi)
          plotfunc(partial(wave, t));
          pause(1000);
        end
    end
end