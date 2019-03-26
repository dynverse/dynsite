#!/bin/bash

from=$1
to=$2

if [ -d raw/$from ]; then
	mv raw/$from raw/$to
fi

if [ -d figures/$from ]; then
	mv figures/$from figures/$to
fi

if [ -d results/$from ]; then
	mv results/$from results/$to
fi

if [ -d scripts/$from ]; then
	mv scripts/$from scripts/$to
fi

if [ -d derived/$from ]; then
	mv derived/$from derived/$to
fi

find . -type f -regex ".*.Rmd" -print0 | xargs -0 sed -i "s#$from#$to#g"
find . -type f -regex ".*.R" -print0 | xargs -0 sed -i "s#$from#$to#g"
find . -type f -regex ".*.html" -print0 | xargs -0 sed -i "s#$from#$to#g"


