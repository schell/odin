mkdir -p ./reports

name=`date | tr ' ' -`
mv $1.prof $name.prof
mv $1.hp $name.hp
hp2ps -e8in -c $name.hp
mv $name.* reports/
open -e reports/$name.prof
open reports/$name.ps
