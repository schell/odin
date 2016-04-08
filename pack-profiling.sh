name=`date | tr ' ' -`
mv $1.prof $name.prof 
mv $1.hp $name.hp 
hp2ps -e8in -c $name.hp 
open -e $name.prof
open $name.ps
