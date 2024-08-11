open List;;
let choose etc things =
    let rec choosing things =
        match things with
        [] -> ()|
        firstThing :: otherThings -> (etc firstThing) ; choosing otherThings
    in choosing things;;



let rec allbut things thing =
    if things = []
    then things
    else if hd things = thing
    then tl(things)
    else ((hd things) :: allbut (tl things) thing);;


    let permute etc things =
    let rec permuting permutedThings unpermutedThings =
        if permutedThings = []
        then etc permutedThings
        else choose (fun firstThing -> permuting (firstThing::permutedThings) (allbut unpermutedThings firstThing)) unpermutedThings
   in permuting [] things;;







