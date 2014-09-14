function [] = uppgift9()
    arrayfun(combM(@disp, @(x) x*(x+1)*(x+2)), 1:98);
end