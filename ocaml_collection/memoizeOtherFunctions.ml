
let rec fib n =  
    match n with 
    0 -> 0 |
    1-> 1 |
    _ -> fib (n-2) + fib (n-1);;


let makeFib l =
    let t = Array.make l (-1) 
    in let rec fibbing n = 
	if t.(n) >= 0 
        then t.(n)  
        else (t.(n) <- 
            (match n with  
            		0 -> 0 | 	
                	1 -> 1 |	
                	_ -> fibbing(n-2) + fibbing(n-1));
            			t.(n))
    in fibbing;; 




let fib1 = makeFib 50;; 














(*FACTORIAL STUFF *)



let rec fac n =
    if n > 1
    then n * fac(n-1)
    else 1;;


let makeFac l = 








