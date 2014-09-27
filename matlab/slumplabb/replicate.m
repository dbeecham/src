function o = replicate(f, N, varargin)
    % replicate takes a function (f), calls it N times, and gathers
    % the result in a list. replicate also takes an optional stepping
    % argument where every 'step' element of f() is gathered.
    % Note that when 'step' does not divide N, f is still called N
    % times, but the latest value of f is not gathered. This will skew
    % any later uses of f. Be on the safe side and make sure 'step' |
    % N.
    
    if (nargin > 3)
        step = varargin{1};
    else
        step = 1;
    end
    
    % Create an empty 1xs vector, where s is the number of elements
    % needed to hold N elements when stepping by step. This part is a
    % bit ugly, due in part to a crude optimization techique (avoiding
    % the creation of vectors), and in part to bad programming.
    o = zeros(1, floor(N/step));
    n = 1;
    i = 1;
    while n <= N
        o(i) = f();
        i = i + 1;
        n = n + 1;
        
        skip = step - 1;
        while (skip > 0)
            f();
            n = n + 1;
            skip = skip - 1;
        end
    end
end
