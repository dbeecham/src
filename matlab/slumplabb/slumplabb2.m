function [] = slumplabb2()
    % Andra uppgiften:
    % Visualisera slumpvandringarna.
    
    w = w2scatter(amalgamate(walker(linstep()), walker(rndstepi({-1, 1}))));
end
