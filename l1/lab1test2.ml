open List;;

let rec howMany e l =
	if (l = [])
	then 0
	else if ((hd l) = e)
	then (1 + (howMany e (tl l)))
	else (howMany e (tl l))
	;;

let rec delete e l =
	if (l = [])
	then l
	else if (hd l = e)
	then delete e (tl l)
	else ((hd l) :: delete e (tl l));;

let rec length l =
	if l = []
	then 0.0
	else (1.0 +. (length (tl l)));;
let rec sum l =
	if l = []
	then 0.0
	else ((hd l) +. (sum (tl l)));;
let rec mean l =
	if l = []
	then 0.0
	else ((sum l) /. (length l));;