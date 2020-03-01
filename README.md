# riverford-poc

- See the design brief [here](https://github.com/riverford/recipe-search-tech-test).

- See the PoC development history
[here](https://github.com/mattford63/riverford-poc/commits/master) for
a track of development decisions and optimisations.

## Approach

Given the time constraints to do this the focus has been on a solid
foundation from which to further develop out the search solution.

The essential requirement of speed (<10ms) and simple relevance has been
focused on. Sophisticated search techniques and recipe management are
out of scope for now.

The very first pass used
[clucie](https://github.com/federkasten/clucie) to get a feel for the
problem space (see
[here](https://github.com/mattford63/riverford-poc/blob/b7f7e4901a762d387f80943af4aab2c49d9601b7/src/riverford_poc/core.clj)).
However, after talking with RiverFord a from-scratch approach was
required.

Inverted indexes are excellent for building search engines
over. _"Introduction to Information Retrieval"_ by Manning, Raghavan
and Schutze provides a very good overview of the process and
efficiency algorithms.  The code implements the methods and algorithms
in the book outlined
[here.](https://nlp.stanford.edu/IR-book/html/htmledition/a-first-take-at-building-an-inverted-index-1.html)

The core abstraction is an `index-store` which contains `indexes`.The
recipes are of a consistent format and have the following sections

- title
- introduction
- ingredients
- method

The code is written such that each section of the recipe is an index
within the index-store.  Allowing searches to be constrained to a
particular section.  Then a merge operation over the four indexes
creates a 5th index `all` - over which the entire recipe can be
searched.

The speed of the search comes from using ordered vectors as the data
structure for the document ids in the reverse index. A class of
merge-algorithms that rely on the fact that vectors are ordered offer
more speed up when performing operations.  The computational expense
of the initial import operation ~50s is the price paid up front for
sub <1ms search later.

A DSL consisting of `intersect`, `union` and a limited `not` provide
search operations over the index-store.

## Usage

To test run `lein test`.  There's quite a lot of test coverage and the
page also highlights the shape of the data structures used.

- https://github.com/mattford63/riverford-poc/blob/master/test/riverford_poc/core_test.clj

The PoC is currently managed by the REPL. An example section at the
end of `core.clj` can be used to explore the functionality.  Just
remember to unzip and change the directory location appropriately.

- https://github.com/mattford63/riverford-poc/blob/master/src/riverford_poc/core.clj#L275

## Issues

- the `not` implementation is naive and very restricted at present to
  where it can be used.  The fix is to maintain in each index a full
  list of the document ids used in that index.


## Future work

- Migrate away from PoC code e.g. namespace fuctions appropriately,
  split tests etc.
- It should be relatively quick to add one of the these clojure
  [fuzzy stemmer](https://yomguithereal.github.io/clj-fuzzy/clojure.html) algorithms to
  [here](https://github.com/mattford63/riverford-poc/blob/master/src/riverford_poc/core.clj#L76)
  and
  [here](https://github.com/mattford63/riverford-poc/blob/master/src/riverford_poc/core.clj#L174).
  This would be a good way to make the search seem way more sophisticated.
- At present the individual document frequencies are lost when
  combined. A more sophisticated reverse index data structure would
  allow this data to be kept - this would open up the door to
  relevancy scoring based on term frequency.
- The tokenizer used records word position, by extending the reverse
  index data structure even further we could capture this also.
- Think about whether the index-store should be atomic.
- Think about a mechanism of updating the all index on other index updates.
- Parallelise the input (possibly transducers) but is it worth it?


## License

Copyright © 2020 Matt Ford

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
