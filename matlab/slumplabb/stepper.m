function F = stepper(f)
    % A stepper takes a 'step function' (f) which is a function that
    % returns a position step, and returns a function (F) that takes
    % a position and updates it according to the step function (f).
    function P = func(pos)
        P = pos + f();
    end
    F = @func;
end
