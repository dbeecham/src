function [] = uppgift41()
    fib = fib_m();
    l1 = arrayfun(fib, 1:40);
    l2 = arrayfun(fib, 2:41);
    l3 = l2 ./ l1;
    plot(1:40, l3);
end