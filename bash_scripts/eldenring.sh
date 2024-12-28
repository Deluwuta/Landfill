#!/bin/bash

echo "Welcome to Elden Ring. 
Now, tarnished, please choose your class:
1 - Samurai
2 - Prisioner
3 - Witch"
read class

case $class in
	1)
		type="Samurai"
		hp=20
		attack=10
		;;
	2)
		type="Prisioner"
		hp=30
		attack=5
		;;
	3)
		type="Witch"
		hp=15
		attack=5
		magic=12
		;;
esac

message="You have chosen a $type class. Your attack is $attack and you have $hp hp"

if [[ $type != "Witch" ]]; then
	echo "$message" 
else
	echo "$message. You also have $magic magic." 
fi

echo "You died"

# First battle
beast=$(( $RANDOM % 2))

echo "Your first beast approaches. Prepare for battle. Pick a number between 0-1. (0/1)"
read tarnished

if [[ $beast == $tarnished ]]; then
	echo "Beast VANQUISHED. Congrats fellow tarnished"
else
	echo "You died."
	exit 1
fi

# Boss battle. Margit, the Fell Omen
margit=$(( $RANDOM % 10 ))

echo "You next battle is vs Margit, the Fell Omen. Pick a number between 0-9. (0 to 9)"
read tarnished

if [[ $beast == $tarnished || $tarnished == "coffee" ]]; then
	echo "Margit VANQUISHED. You are one step closer to become the Elden Ring"
else
	echo "You died"
fi

