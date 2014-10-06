function ends = slumplabb3()
    % Tredje uppgiften:
    % Analysera hur långt en vandring med N steg typiskt tar sig från
    % startpunkten.
    
    ends = zeros(1, 100);
    i = 1;
    while i <= 100
        ends(i) = mean(sum(randi([0 1], 3000, 10*i)*2-1, 2));
        i = i + 1;
    end
    xs = 10:10:10*100;
    plot(xs, ends);
end
