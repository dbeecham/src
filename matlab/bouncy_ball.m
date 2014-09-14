function f = bouncy_ball(initpos, initvel, bound)

    pos = initpos;
    vel = initvel;

    function [x, y] = func()
        % Collision detection
        mask = (pos + vel > bound(2)) | (pos + vel < bound(1));
        mask = arrayfun(@(x) iff(x, -1, 1), mask);
        vel = vel .* mask;
        
        % Update position
        pos = pos + vel;
        
        % Return values
        x = pos(1);
        y = pos(2);
    end

    f = @func;
end