day=$1
year=2025
newFile="src/main/scala/y$year/Day_$day.scala"
cp ./src/main/scala/2022/DayTemplate.scala $newFile
sed -ie "s/1/${day}/g" $newFile
sed -ie "s/DayTemplate/Day_${day}/g" $newFile
rm "${newFile}e"
touch ./src/main/resources/$year/day_$day.txt
