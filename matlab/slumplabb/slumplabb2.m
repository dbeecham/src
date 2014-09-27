function [] = slumplabb2()
    % Andra uppgiften:
    % Visualisera slumpvandringarna.
    
    w = walker(0, randDiscreteStepper({-1, 1}));
    xs = 1:10000;
    ys = replicate(w, 10000);
    plot(xs, ys);
end
