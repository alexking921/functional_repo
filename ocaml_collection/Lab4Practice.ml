let printThings format things =
  let rec printingThings things =
    match things
    with [] -> () |
         firstThing :: otherThings ->
           Printf.printf " ; " ;
           Printf.printf format firstThing ;
           printingThings otherThings
  in Printf.printf "[" ;
     (match things
      with [] -> () |
           firstThing :: otherThings -> 
             Printf.printf format firstThing ;
             printingThings otherThings) ;
     Printf.printf "]\n" ;;


Open list;;
open list;;
open List;;

let choose func things =
    let rec choosing things =
        match things with
        [] -> [] |
        firstThing :: otherThings -> (func firstThing) :: choosing otherThings
    in choosing things;;
