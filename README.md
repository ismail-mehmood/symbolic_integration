# symbolic_integration
My solution to the 2024 January Haskell Paper, Imperial College London.
This description is *unfinished*, apologies.

One of my main goals when completing this paper was to make use of FAM (Functors, Applicatives, Monads). The main use I found for this was in the intE function, where it is necessary to apply an Expr wrapper to a result of intE recursively, but intE returns a wrapped (Maybe Expr). This could be handled with fromJust, but I chose to use the <*> operator instead. This resulted in what I believe to be a more precise, concise and overall better line of code.
