day=$1
newFile="src/main/scala/Day_$day.scala"
cp ./src/main/scala/DayTemplate.scala $newFile
sed -ie "s/1/${day}/g" $newFile
sed -ie "s/DayTemplate/Day_${day}/g" $newFile
rm "${newFile}e"
touch ./src/main/resources/day_$day.txt