#!/bin/sh
#rm -f Bobp.cache
#rm -f Charliew.cache
i=1
echo "Starting $i iterations"
while [ $i -ge 1 ]
do
#  echo "iteration $i"
  i=`expr $i - 1`
  ./conf-a.exe &
  ./conf-c.exe &
  ./conf-p.exe
done
