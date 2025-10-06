cl-bridge
=========

cl-bridge is a Common Lisp library for running probability
simulations for game of bridge. It contains functions to
generate random deals for given constraints as well as
trick counting algorithm. 

Environment
===========

cl-bridge is developed on SBCL, so it should run on Linux
as well as Windows. It probably could be ran on Android
using cl-repl, but I haven't test it yet and have no idea
what performance would it provide.

Typical workflow
================

Initialization
--------------

Typical sessions starts by running SBCL in project directory
and loading the library:

```
~/code/bridge.cl$ sbcl
This is SBCL 2.1.11.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (load "bridge.cl")
*
```

Actually, loading a library may produce some warnings about syntax
and failed tests. However if there is `*` prompt in the end, the
library was loaded successfully and we can proceed.

Generating deals
----------------

Before any analysis could be ran, we have to generate deals
for given constraints. The constaints can refer to suit
distribution, as well as HCP distribuition (including HCP
per suit distribution). We can specify constraints for each
hand in deal as well as specify constant hands.

The most high-level interface for this task is a `deal` class.
It accepts two possible paramters: `:=` is used to pass list
of constant hands and `:~` is used to pass parameters for other
hands that should be generated for given constraints. If number
of given constant hands and constraint sets is less than 4, the
rest of hands would be generated with no constraints.

`:~` paramter accepts a list of parameter lists. Each of them
supports following named parameters: `:hcp`, a range or exact
number of HCP for that hand and `:c`, `:d`, `:h`, `:s` parameters
to spcify range or exact number of cards in each of suits: ♣, ♦,
♥, ♠. A range is simply a list of minimum and maximum (possible
nil if we don't want to specify). Additionaly, for suit parameters,
there can be provided exact or range for HCP in that suit as optional
third element of range list.

Example:

```
(make-instance 'deal := '(((K 8 4 3) (K 5 4) (K 10 5 4) (J 3)))
                     :~ '((:hcp (6 10) :c (7 nil (6 nil)))
                          (:hcp (16 nil) :s (6 nil))))
```

This code would create a deal generator object that would produce deals
that contain: `♣ K843 ♦ K54 ♥ K1054 ♠ J3` at all times first hand;
second hand that has 6-10HCP, 7+♣ and at least 6HCP located in ♣;
third that contain at least 16HCP and 6+♠. The fourth hand would be pure
random.

Having an instance of `deal`, we can use `build` method to generate a deal
for given constraints:

```
* (build (make-instance 'deal := '(((K 8 4 3) (K 5 4) (K 10 5 4) (J 3)))
                        :~ '((:hcp (6 10) :c (7 nil (6 nil)))
                             (:hcp (16 nil) :s (6 nil)))))
(HAND<♣ K843 ♦ K54 ♥ K1054 ♠ J3> HAND<♣ AQ109752 ♦ QJ ♥ 976 ♠ 5>
 HAND<♣ J ♦ A93 ♥ A3 ♠ AK109872> HAND<♣ 6 ♦ 108762 ♥ QJ82 ♠ Q64>)
```

`NOTE:` `deal` object computes probabilities for each possible HCP/suit
distribution and randomizes accordingly. This means that creating an
object (especially if no constant hand is given) can take some time
(On my Intel(R) Core(TM) i5-3320M CPU @ 2.60GHz, creating a deal
object with no constaints takes about a minute). That's why it's good
to instantiate the object once and then produce many deals.

Simulation object
-----------------

It's good to have simulation results stored so that we can perform various
queries without a need to rerun simulation each time. `mc-case` object is
for that purpose.

```
(defparameter sim
    (let ((dg (make-instance 'deal 
                             :~ '((:hcp (20 21) :c (2 4) :d (2 4) :h (2 4) :s (2 4)))
                             := '(((4 3 2) (10 4 3) (K 10 9 5 4) (8 7))))))
       (make-instance 'mc-case :! `(reorder (build ,dg) '(1 2 0 3))
                               := '(best-tricks trump-length)
                               :trump 'h)))
```

Having a `deal` instance we instantiate `mc-case`. `:!` parameter is a form return
a list of 4 hands. `:=` parameter is a list of measures to be computed for each
hand. `:trump` paramter specifies a trump suit for simulation. `nil` would
mean no trump. For suits values are `'c`, `'d`, `'h`, `'s`.

`reorder` function is used to put generated hands in desired order, so that first
hand is declarer. In this case I want to simulate an outcome of a non-trump contract,
when my parter is strong non-trump and I have just a king in 5 card hearts. Constant
hands are always first in `build` results and then come constrainted hands. So `1`
indicates partner's hand who is a declarer (opened `2NT`) and `0` is my hand. Thus:
`(reorder (build ,dg) '(1 2 0 3))`.

Measures passed to `:=` should be symbols bound to functions accepting 5 parameters:
trump suit (`nil`, `'c`, `'d`, '`'h` or `'s`) and 4 hands starting from declarer.
In the example there are 2 measures: `best-tricks`: estimated number of tricks to
be taken in specified suit, `trump-length`: number of trump cards held by playing
partnership.

Running simulation
------------------

Having a `mc-case` object, we have to run the simulation before we analize results.
Do it using `sim` function.

```
(sim sim 1000)
```

Means running 1000 cases for sim object. If there were already some results, they
will be appended.

Retrieving results
------------------

Results can be retrieved using `select` function. Usually it's combined with
`(print-hist (histogram (...)))` functions, which causes that instead a list
of result lists, a human readable probability breakdown would be printed:

```
* (print-hist (histogram (select sim)))
9: 40.46%
    8: 50.37%
    9: 24.94%
    7: 24.69%
10: 28.07%
    8: 49.82%
    9: 34.52%
    7: 15.66%
8: 20.48%
    8: 45.37%
    7: 37.07%
    9: 17.56%
11: 5.39%
    8: 48.15%
    9: 37.04%
    7: 14.81%
7: 4.80%
    7: 62.50%
    8: 33.33%
    9: 4.17%
(6 7): 0.70%
(12 9): 1/1001
```

The output is hierarchical. Least indented are values for first measure with
percent of cases in the simulation. Each next level of indetation means next
value and given percent is relative to parent value.

For example, in this case,
10 tricks are taken in 28,07% of case. In 49.82% of them there's an 8 card suit, 
in 34.52%, a 9 cards and only in 15,66% of cases 7-cards on line.

Retrieving specific fields and field reorder
--------------------------------------------

`select` function can be used with `:fields` parameter to select or reorder fields.

```
* (print-hist (histogram (select sim :fields '(trump-length best-tricks))))
8: 47.85%
    9: 42.59%
    10: 29.23%
    8: 19.42%
    11: 5.43%
    7: 3.34%
7: 26.47%
    9: 37.74%
    8: 28.68%
    10: 16.60%
    7: 11.32%
    11: 3.02%
    6: 2.64%
9: 25.67%
    9: 39.30%
    10: 37.74%
    8: 14.01%
    11: 7.78%
    7: 0.78%
    12: 0.39%
```

As you can see, order of parameters are reversed, `trump-length` is a top-level
measure and `best-tricks` are secondary.

Filtering results
-----------------

`select` function supports `:filter` parameter, to select only results, that match
a condition. Parameter is a form that is evaluated after substitution of measure
names to their values. There is also possibility to use `hands` symbol for whole
deal.

```
* (print-hist (histogram (select sim :filter '(find 12 (seektree '(0 2)  hands)))))
9: 42.61%
    8: 48.41%
    9: 28.03%
    7: 23.57%
10: 29.99%
    8: 50.23%
    9: 37.10%
    7: 12.67%
8: 17.10%
    8: 52.38%
    7: 25.40%
    9: 22.22%
11: 6.38%
    8: 53.19%
    9: 36.17%
    7: 10.64%
7: 3.26%
    7: 58.33%
    8: 37.50%
    9: 4.17%
(6 7): 0.54%
(12 9): 0.14%
```

In this example, we filter results with A♥ in declarer's hand. `12` stands for Ace
(ranks are values 0-12). In `'(0 2)`, `0` stands for first hand and `2` stands for
hearts (third suit).

Showing deal examples
---------------------

Special case of filtering is when we want to query for whole deals that meets some
condition. `show-deals` function does it.

```
* (show-deals sim '(>= best-tricks 12))
N: ♣ KJ96 ♦ A8 ♥ AJ83 ♠ AK9
E: ♣ Q5 ♦ J9762 ♥ Q ♠ J6543
S: ♣ 432 ♦ 1043 ♥ K10954 ♠ 87
W: ♣ A1087 ♦ KQ5 ♥ 762 ♠ Q102 
```

In this example, we show deals that make 12 or 13 tricks.

Showing deal play simulation
----------------------------

If we want to show optimal deal play found by `best-tricks`, we can use `simdeal`
function:

```
* (simdeal (str2deal "N: ♣ KJ96 ♦ A8 ♥ AJ83 ♠ AK9
E: ♣ Q5 ♦ J9762 ♥ Q ♠ J6543
S: ♣ 432 ♦ 1043 ♥ K10954 ♠ 87
W: ♣ A1087 ♦ KQ5 ♥ 762 ♠ Q102") :trump 'h)

N: ♣ KJ96 ♦ A8 ♥ AJ83 ♠ AK9
E: ♣ Q5 ♦ J9762 ♥ Q ♠ J6543
S: ♣ 432 ♦ 1043 ♥ K10954 ♠ 87
W: ♣ A1087 ♦ KQ5 ♥ 762 ♠ Q102
winners: 1
♣5 ♣2 ♣A ♣6 
♣7 ♣K ♣Q ♣3 
♥8 ♥Q ♥K ♥2 
♠8 ♠10 ♠K ♠3 
♠A ♠4 ♠7 ♠2 
♥J ♦2 ♥10 ♥6 
♥A ♠5 ♥9 ♥7 
♦A ♦6 ♦3 ♦5 
♠9 ♠J ♥4 ♠Q 
♣4 ♣8 ♣9 ♠6 
♥3 ♦7 ♦4 ♣10 
♣J ♦9 ♦10 ♦Q 
♦8 ♦J ♥5 ♦K
```

In this example there is a bug found. Somehow ♦4 is dropped on ♥3
which is not OK.

Saving & loading simulation results
-----------------------------------

To store a simulation results, use `save` function:

```
(save sim "sim.dat")
```

To load a sim object, we can use `load-mc-case` function:

```
(defparameter sim*
      (let ((dg (make-instance 'deal 
                     :~ '((:hcp (20 21) :c (2 4) :d (2 4) :h (2 4) :s (2 4)))
                     := '(((4 3 2) (10 4 3) (K 10 9 5 4) (8 7))))))
         (load-mc-case "sim.dat" :sim-count 1000
                                 :fallback-params `(:! (reorder (build ,dg) '(1 2 0 3))
                                                    := (best-tricks trump-length)
                                                    :trump h))))
```

It reads results from `sim.dat` if the file exists and runs the simulation for 1000 cases.
