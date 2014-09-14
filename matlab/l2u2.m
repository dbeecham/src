function [] = l2u2()
    n = 1;
    while n < 100
        n = n+1;
    end
    
    % inc n
    % cmp n, 100
    % jle loop
    
    fprintf('Slingan går %i varv.\n', n-1);
    fprintf('Värdet på n efter loopen är %i.\n', n);
end