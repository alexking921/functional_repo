open List
# let rec howMany e l =
	if (l =[])
	then 0
	else if ((hd 1) = e)
	then (1+ (howMany e (tl l));;
# let rec delete e l =
	if (l = [])
	then l
	else if (hd l = e)
	then delete e (tl l)
	else ((hd l) :: delete e (tl l));;

# let rec delete e l =
	if (l = [])
	then l
	else if (hd l = e)
	then delete e (tl l)
	else ((hd l) :: delete e (tl l));;