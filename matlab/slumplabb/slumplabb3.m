function [] = slumplabb3()
    % Tredje uppgiften:
    % Analysera hur långt en vandring med N steg typiskt tar sig från
    % startpunkten.
    
    N = 100; % Number of steps per walk
    step = 10; % Do math on n*step steps.
    hm = figure(1);
    
    visited = zeros(1, 2*N-N);
    hv = subplot(3, 3, [1]);
    hold(hv);
    
    hw = subplot(3, 3, [2]);
    hold(hw); 
    
    while 1
        cla(hw);
        w = walker(0, randDiscreteStepper({-1, 1}));
        w = wplot2d(w, hw, 1, [0, 0, 0], '.');
        w = 
        pause(0.1);
    end
    % Move step with walker, plot step, for each 100
    % add position to endpoint-vector and plot endpointvector
    %endposs = replicate(partial(@nth, N, w), M);
    %disp(sum(abs(endposs)) / N)

end
