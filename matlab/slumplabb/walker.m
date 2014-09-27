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
