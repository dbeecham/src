function [] = slumplabb3()
    % Tredje uppgiften:
    % Analysera hur långt en vandring med N steg typiskt tar sig från
    % startpunkten.
    
    N = 500; % Number of steps per walk
    step = 10; % Do math on n*step steps.
    hm = figure(1);
    
    visited = zeros(1, 2*N-N);
    hv = subplot(3, 3, [1]);
    hold(hv);
    
    hw = subplot(3, 3, [2]);
    hold(hw); 
    
    while 1
        cla(hw);
        w = amalgamate(2, walker(linstep()), walker(0, rndstepi({-1, 1})));
        w = wscatter(w, 1, hw, 1, [0, 0, 0], '.');
        d = replicate(w, 100);
        d = d(2:2:end);
        visited = [visited, d];
        hist(visited);
        pause(0.1);
    end
    % Move step with walker, plot step, for each 100
    % add position to endpoint-vector and plot endpointvector
    %endposs = replicate(partial(@nth, N, w), M);
    %disp(sum(abs(endposs)) / N)

end
