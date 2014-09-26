function [] = slumplabb1()
% "Script" som man bara kör. Det är en funktion så att jag kan
% konkatenera multipla filer och funktionsdefinitioner.
% Texten är skriven på engelska helt enkelt för att jag resonerar
% i engelska i mitt huvud som jag programmerar (tack, engelsk litteratur).

    function F = walker(pos, f)
        % walker :: position -> (position -> position) -> IO position
        % A walker takes a random step and returns it's new position.
        % It takes an initial position and a step function and relies
        % on clojure and side-effects to update it's position on each
        % call, and thus this functions sort of like a class.
        % This walker is an n-dimensional walker where pos is an
        % n-dimensional position vector, f takes an n-dimensional vector
        % and returns an n-dimensional vector.
        function P = func()
            pos = f(pos);
            P = pos;
        end
        F = @func;
    end

    function F = stepper(f)
        % A stepper takes a 'step function' (f) which is a function that
        % returns a position step, and returns a function (F) that takes
        % a position and updates it according to the step function (f).
        function P = func(pos)
            P = pos + f();
        end
        F = @func;
    end

    function F = randDiscreteStepper(steps)
        % This is a stepper (position -> position) which randomly picks
        % on every step picks a random element from steps.
        % Notice that steps is a cell structure of array elements ({[]}).
        function P = func(pos)
            P = pos + steps{randi(end)};
        end
        F = @func;
    end

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

    function o = nth(n, f)
        % Extracts the nth value of the discrete value-generating
        % side-effecting function f (a very strange notion indeed).
        
        i = 0;
        while i < n
            f();
            i = i + 1;
        end
        o = f();
    end

    function f = partial(F, varargin)
        % Partial takes a function f and a variable number of arguments a,
        % and returns a function which takes a variable number of
        % arguments b, which calls f(a b). Note that |a| + |b| = |f|.
        
        outerargs = varargin;
        function o = func(varargin)
            o = F(outerargs{:}, varargin{:});
        end
        f = @func;
    end

    % Första uppgiften:
    % Skriv ett program som genererar 1-dimensionella slumpvandringar
    % med ett givet antal steg, N.
    
    %N = 100;
    %w = walker(0, randDiscreteStepper({-1, 1}));
    %xs = replicate(w, N)
    
    % Andra uppgiften:
    % Visualisera slumpvandringarna.
    
    %w = walker(0, randDiscreteStepper({-1, 1}));
    %xs = 1:10000;
    %ys = replicate(w, 10000);
    %plot(xs, ys);
    
    % Tredje uppgiften:
    % Analysera hur långt en vandring med N steg typiskt tar sig från
    % startpunkten.
    
    %w = walker(0, randDiscreteStepper({-1, 1}));
    % Move step with walker, plot step, for each 100
    % add position to endpoint-vector and plot endpointvector
    %endposs = replicate(partial(@nth, N, w), M);
    %disp(sum(abs(endposs)) / N)
    
    
end

