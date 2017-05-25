[![Build Status](https://travis-ci.org/balhoff/owl-to-rules.svg?branch=master)](https://travis-ci.org/balhoff/owl-to-rules)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.geneontology/owl-to-rules/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.geneontology/owl-to-rules)

# owl-to-rules

Translation of OWL 2 RL axioms into Jena rules.

## Building

Install `sbt` (Scala Build Tool) on your system. For Mac OS X, it is easily done using [Homebrew](http://brew.sh):  `brew install sbt`. `sbt` requires a working Java installation, but you do not need to otherwise install Scala.

`sbt compile`

## License
owl-to-rules is published under the terms of a [BSD license](https://opensource.org/licenses/BSD-3-Clause). The FaCT++ reasoner is used to test reasoning results and can be obtained from [its site](https://bitbucket.org/dtsarkov/factplusplus) according to the [LGPL license](https://opensource.org/licenses/LGPL-2.1).
