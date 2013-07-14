generator <- function() {
    return(function(x) {
           return(x * x)
    })
}

gen <- generator()
print(gen(2))
