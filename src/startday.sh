dayNumber=$(printf "%02d" $1)
echo $dayNumber

cp src/DayX.hs src/Day$dayNumber.hs
sed -i "s/DayX/Day$dayNumber/" src/Day$dayNumber.hs
