(def total-rows (atom 0))

; (reset! atom new-value)
(reset! total-rows 10) ; total-rows is now 10!

; (swap! atom function & args)
(swap! total-rows + 100) ; 110

; (compare-and-set! atom old-value new-value)
(compare-and-set! total-rows 110 120) ; 120