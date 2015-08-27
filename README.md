This repository creates bill cosponsorship networks for the [National Assembly of South Korea](http://korea.na.go.kr/), using data from [Politics in Korea](http://pokr.kr/) by [Team Popong](http://popong.com/). The code mimicks the code used for the [parlnet](https://github.com/briatte/parlnet) project, which builds legislative cosponsorship networks for European countries.

Static plots of the networks are viewable on [that page](http://f.briatte.org/parlviz/gukhoe/plots.html).

# HOWTO

Replicate by running `make.r` in R after checking the dependencies at the top of the script:

- `01-data.r` will download information on bills and sponsors
- `02-build.r` will build, weight and plot the networks

The data range back to 1950, and there are simply too many political parties in South Korea to map them to colours; instead, the code assigns a color from [ColorBrewer's Set3](http://colorbrewer2.org/?type=qualitative&scheme=Set3&n=8) to all parties with 5 or more members, and colours all other parties in light grey, or dark grey for independents.

The sponsors data that are downloaded are _much_ richer than the information reflected in the graphs (and include, for instance, electoral candidacy statuses in all competed elections).
