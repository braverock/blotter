[![Travis build status](https://travis-ci.org/braverock/blotter.svg?branch=master)](https://travis-ci.org/braverock/blotter)

# blotter - Tools for Transaction-Oriented Trading Systems Development

Transaction infrastructure for defining instruments, transactions, portfolios and accounts for trading systems and simulation. Provides portfolio support for multi-asset class and multi-currency
portfolios. Actively maintained and developed.

## Installing

In order to install blotter from github, you will need to install devtools.

```
install.packages("devtools")
```

then

```
require(devtools)
install_github("braverock/blotter")
```

If you can run one of the demo files, you would have successfully installed blotter.

```
demo('longtrend', ask=FALSE)
```

### Prerequisites

There are a few dependencies for _blotter_, namely:

* R (>= 3.0.0)
* xts (>= 0.10-0)
* FinancialInstrument(>= 0.6.3)
* PerformanceAnalytics
* quantmod

Imports:

* zoo
* TTR
* graphics
* methods
* stats
* utils
* boot
* foreach

Suggests:

* Hmisc
* RUnit

## Authors, Creators and Contributors

* Peter Carl [aut]
* Brian G. Peterson [aut, cre]
* Joshua Ulrich [ctb]
* Jasen Mackie [ctb]
* Daniel Cegielka [ctb]
* Dirk Eddelbuettel [ctb]
* Jan Humme [ctb]
* Lance Levenson [ctb]
* Ben McCann [ctb]
* Jeff Ryan [ctb]
* Garrett See [ctb]
* Wolfgang Wu [ctb]

## License

This project is licensed under GPL-3. See https://www.gnu.org/licenses/gpl-3.0.en.html for details.
