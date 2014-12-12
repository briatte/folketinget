This repository contains code to build cosponsorship networks from bills (and motions) passed in the [Danish Parliament](http://www.ft.dk/).

- [interactive demo](http://briatte.org/folketinget)
- [static plots](http://briatte.org/folketinget/plots.html)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. The code is pretty straightforward and excludes only a handful of bills for which the scraper gets the columns wrong. It should furthermore be easy to update the scraper past _samling_ 2014/1, which is currently the last considered.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

Note -- because modularity tends to increase with the number of parties, the scores are likely to be inflated by the presence of regionalist parties for the Faroe Islands and for Greenland. Excluding them from the computations, however, removes their non-trivial structural positions from the graphs.

# DATA

## Bills

The data contains three types of legislative items: "motions" (, "B"), "bills" (, "L"), and resolutions (, "V"). Only the first two are passed to the network building routine (resolutions are nonbinding). The networks are not radically different if resolutions are included.

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
- `mandate` -- semicolon-separated mandate years, used to compute the `nyears` seniority variable
- `job` -- occupation
- `url` -- a unique identifier almost always composed of just uppercase letters
- `photo` -- photo URL, shortened to its unique identifier (identical to `url` or missing)
- `bio` -- short sponsor biography

# CREDITS

Thanks to [Niels Erik Rasmussen](https://twitter.com/nilleren) and [Thomas Leeper](https://twitter.com/thosjleeper) for useful feedback on a preliminary version.
