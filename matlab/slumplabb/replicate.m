function o = replicate(f, N, varargin)
    % replicate takes a function (f), calls it N times, and gathers
    % the result in a list.
    
    % Function is called once here to extract vector size.
    O = f();
    
     % Filling the vector is done using linear index, and so 'step' is used
    % as an indexer where the interesting elements are n*step to
    % (n+1)*step.
    step = numel(O); 
    
    % Initialize output vector with zeroes.
    if step == 1
        o = zeros(1, N);
    else
        o = zeros([size(O), N]);
    end
    
    o(1:step) = O;
    
    n = 2;
    i = 1;
    while n <= N
        o((i*step)+1:(i*step)+step) = f();
        i = i + 1;
        n = n + 1;
    end
end
