function F = randDiscreteStepper(steps)
    % This is a stepper (position -> position) which randomly picks
    % on every step picks a random element from steps.
    % Notice that steps is a cell structure of array elements ({[]}).
    function P = func(pos)
        P = pos + steps{randi(end)};
    end
    F = @func;
end
