This repository contains code to build cosponsorship networks from bills (and motions) passed in the [Danish Parliament](http://www.ft.dk/).

- [interactive demo](http://f.briatte.org/parlviz/folketinget)
- [static plots](http://f.briatte.org/parlviz/folketinget/plots.html)
- [more countries](https://github.com/briatte/parlnet)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. The code is pretty straightforward and excludes only a handful of bills for which the scraper gets the columns wrong.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

The data contains three types of legislative items: "motions" (_Beslutningsforslag_, "B"), "bills" (_Lovforslag_, "L"), and resolutions (*Forslag_til_vedtagelse*, "V"). Only the first two are passed to the network building routine (resolutions are nonbinding). The networks are not radically different if resolutions are included.

- `title` -- bill title
- `uid` -- a unique identifier of the form "YYYYS-TNN"
	- "YYYY" is the year of introduction
	- "S" is the session number ("1" or "2")
	- "T" is the type of item ("B", "L" or "V")
	- "NN" is a number
- `ministry` -- the ministerial area (empty for resolutions)
- `year` -- the year of introduction, deduced from `uid`
- `status` -- the outcome of the bill (not clean)
- `authors` -- when the bill comes from the executive, the ministerial sponsor
- `links` -- semicolon-separated identifiers of bill sponsors
- `vote` -- detailed voting report on the item
- `summary` -- a summary of the item

## Sponsors

- `name` -- sponsor name
- `func` -- political function(s), e.g. "member,exsted,exmin"
- `party` -- political party, abbreviated
- `partyname` -- political party, full name
- `constituency` -- constituency, stored as the string to its Wikipedia Dansk entry
- `mandate` -- semicolon-separated mandate years, used to compute the `nyears` seniority variable
- `job` -- occupation
- `url` -- a unique identifier almost always composed of just uppercase letters
- `photo` -- photo URL, shortened to its unique identifier (identical to `url` or missing)
- `bio` -- short sponsor biography

Note -- due to how missing sponsors are handled, the version of `sponsors.csv` that is saved to the `data` folder contains only an intermediary version of the sponsors data. To retrieve the finalized dataset with the variables listed above, export the sponsors object `s` after running `data.r` in full.

# THANKS

Thanks to [Niels Erik Rasmussen](https://twitter.com/nilleren) and [Thomas Leeper](https://twitter.com/thosjleeper) for useful feedback on a preliminary version of the code.
