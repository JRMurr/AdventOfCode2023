#!/usr/bin/env fish
argparse --name=setupDay 'd/day=' -- $argv
or exit

set -l day $_flag_day

if not string match --quiet --regex '^\d{2,}$' $day
    echo $day must be a 2 digit number
    exit 1
end

set -l dayStr D$day

# easy way to remove starting 0 if needed
set -l dayNum (math $day)

if test -d ./app/$dayStr
    echo "Day already exists"
    exit 1
end

cp -r ../_dayTemplate ./$dayStr

sed -i "s/DXX/$dayStr/" ./$dayStr/Day.roc
sed -i "s/<UPDATE>/$dayNum/" ./$dayStr/Day.roc


sed -i "s/# -- Add day import/$dayStr.Day,\n# -- Add day import/" ./App.roc

sed -i "s/# -- Add day List/$dayStr.Day.solution,\n    # -- Add day List/" ./App.roc

roc format App.roc

# aoc download -o -d $dayNum -I -i ./$dayStr/in