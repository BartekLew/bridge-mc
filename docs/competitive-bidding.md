# Competitive Bidding – Discussion Summary and Implementation Plan

## Brief Summary
We are extending the bidding engine so that a player can make a first overcall when the opponents have opened. The new functionality lets the system recognise the opponent’s last bid, generate an appropriate set of overcall rules (new-suit, 1NT/2NT, jump, Michaels, takeout/strong double), evaluate those rules against the current hand, and choose a legal call. A helper that produces the logical complement of any set of bid rules is also provided; it will later be used to give meaning to a Pass.

## High-level Functionality

1. Stopper-aware hand evaluation  
   `assess-hand` now returns four values: suit lengths, HCP per suit, total losers and a new “stopper-points” list (HCP + length for each suit). Every downstream function that receives a measure (`good-opening?`, `choose-bid`, `test-bid`, `balanced?`) forwards this extra information, so rules can test stoppers with expressions such as `(>= Hstop 6)`.

2. First-overcall rule generator – `simple-overcall(last-bid)`  
   Given the opponents’ last bid (and optionally the whole list of their bids), the function builds an alist of legal overcalls together with the shape/strength conditions that justify them:
   - New-suit overcalls: 5+ cards, 8-17 HCP, plus the extra requirement that the suit contains at least 4 HCP unless the hand has 12+ total HCP.
   - 1NT/2NT overcalls: 15-18 HCP, balanced, stopper(s) in the opponent’s suit(s). Non-jump 2NT is treated as a strong balanced overcall; a jump 2NT shows 5-5 minors.
   - Jump overcalls: weak, same shape as the corresponding pre-emptive opening.
   - Michaels cue-bid: 5+ cards in the highest unbid suit plus 5+ in a minor (or both majors when the opponent opened a minor).
   - Takeout double (12-17 HCP) or any strong hand (18+ HCP) with at least three cards in every unbid suit.

   Only bids that are legal (higher than the last opponent bid) are emitted; the generator uses `bid-jump` to decide whether a candidate is a jump or a simple overcall.

3. Competitive bidding dispatch – `bidding-scheme-for`  
   When the history shows that an opponent has bid and our side has not yet acted, `bidding-scheme-for` calls `simple-overcall` with the last opposing bid (and the list of all opposing bids). The resulting table is then passed to `choose-bid`, which selects the first rule that matches the current hand.

4. Handling doubles in the bidding history  
   A double is merged with the preceding bid so that the internal representation stays uniform: `(1 S) X` becomes the single entry `(1 S X)`. Consequently every later function still sees a `(level suit)` form while the extra `X` token records that a double occurred. This also prevents a bare `X` from ever being treated as a “last bid”.

5. Logical complement of bid rules – `complement-bid-rules`  
   Given any list of shape/strength rules that appear in an opening, response or overcall table, the function returns a single rule that is true exactly when none of the original rules would be true. It uses De Morgan’s laws, comparison inversion and `merge-shapes` to combine multiple negated rules. The resulting expression is still a valid input for `choose-bid`/`find-in-bid`/`min-hcp`/`max-hcp`, so it can later be attached to a Pass to infer what a passed hand has shown.

6. Seat and history model (unchanged)  
   The first seat is always “us”. The only information needed for a first overcall is that an opponent has bid and our partner has not yet overcalled; the existing `bids` list (now containing `nil` entries for passes) already supplies this.

These additions give the engine a complete, self-contained mechanism for generating, evaluating and selecting first overcalls while preserving full compatibility with the existing one-sided bidding logic.
