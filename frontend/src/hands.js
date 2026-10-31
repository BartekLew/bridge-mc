// Hands as the user defines them, by cards or by ranges of HCP and suit
// lengths, shared by the simulator and the training page (HandEditor.vue)

import { suits, ranks } from './play.js'

const suitLetters = { '♠':'S', '♥':'H', '♦':'D', '♣':'C'};
const handIds = ['N', 'E', 'S', 'W', ''];

export const HandMode = { SELECT: "Select cards", RANDOM: "Random hand" };

function hasVal(field) {
    return field || field === 0;
}

function rangeFormula(range) {
    if(hasVal(range[0]) && hasVal(range[1])) {
        if(range[0] == range[1]) {
            return range[0];
        } else {
            return range[0] + "-" + range[1];
        }
    } else if (hasVal(range[0])) {
        return ">=" + range[0];
    } else if (hasVal(range[1])) {
        return "<=" + range[1];
    }
}

function setLispRange(dest, key, vals) {
    if(vals[0] || vals[1]) {
        if(Number.isInteger(key) && !dest[key]) {
            dest[key] = [ 'NIL', 'NIL' ];
        }

        dest[key] = [ (vals[0] || 'NIL'), (vals[1] || 'NIL') ];
    }
}

function lispStr (val) {
    if(val.constructor == Object) {
        return "( " + Object.keys(val)
                            .map((key) => ":" + key + " " + lispStr(val[key]))
                            .join(" ")
                    + ")";
    } else if (Array.isArray(val)) {
        return "( " + val.map((elem) => lispStr(elem))
                         .join(" ")
                    + " )";
    } else {
        return val;
    }
}

export class Selection {
    constructor(handid) {
        this.setMode(HandMode.SELECT);
        this.handid = handid;
    }

    total() {
        return Object.values(this.data)
                     .reduce((result,val) => {
                                 return result + val.size;
                     }, 0);
    }

    toggle(suit, rank) {
        const suitSet = this.data[suit];
        if(suitSet.has(rank)) {
            suitSet.delete(rank);
        } else {
            suitSet.add(rank);
        }
    }

    clear() {
        this.setMode(this.mode);
    }

    restDef() {
        if(this.mode == HandMode.SELECT) {
            return { hand: this.formula() };
        } else {
            var ans = {};
            setLispRange(ans, 'hcp', this.data['Overall']['HCP']);
            suits.forEach((suit) => {
                            const letter = suitLetters[suit];
                            setLispRange(ans, letter, this.data[suit]['Length']);
                            setLispRange(ans[letter], 2, this.data[suit]['HCP']);
                        });

            return { def: lispStr(ans) };
        }
    }

    formula() {
        if(this.mode == HandMode.SELECT) {
            return this.handid + ": "
                 + suits.map((suit) => suit + " " + ranks.filter((rank) => this.data[suit].has(rank))
                                                         .join(""))
                        .join(" ");
        } else {
            return "RANDOM("
                    + Object.keys(this.data)
                            .map((section) => {
                                   if(Object.keys(this.data[section]).length == 1) {
                                       return rangeFormula(this.data[section]["HCP"]);
                                   } else {
                                       return Object.keys(this.data[section])
                                                    .map((range) => {
                                                            const formula = rangeFormula(
                                                                                this.data[section][range]
                                                                            );
                                                            if(hasVal(formula)) {
                                                               return section + range + "=(" + formula + ")";
                                                            }
                                                    })
                                                    .filter((x) => x)
                                                    .join(", ");
                                   }
                            })
                            .filter((x)=>hasVal(x))
                            .join(", ")
                    + ")";
        }
    }

    has(suit, rank) {
        if(this.mode === HandMode.SELECT) {
            return this.data[suit].has(rank);
        }
    }

    setMode(mode) {
        this.mode = mode;
        if(mode === HandMode.SELECT) {
            this.data = Object.fromEntries(suits.map((suit) => [suit, new Set()]));
        } else {
            this.data = { Overall: { HCP: ["", ""] } };
            for (const suit of suits) {
                this.data[suit] = { Length: ["", ""], HCP: ["", ""] };
            }
        }
    }

    // Inverse of formula() for SELECT mode, e.g. "N: ♠ AK9 ♥ AJ83 ♦ A8 ♣ KJ96"
    static fromFormula(handid, str) {
        const sel = new Selection(handid);
        const body = str.includes(": ") ? str.slice(str.indexOf(": ") + 2) : str;
        for(const [, suit, cards] of body.matchAll(/([♠♥♦♣])\s*([AKQJ0-9]*)/g)) {
            for(const rank of cards.match(/10|[AKQJ2-9]/g) || []) {
                sel.data[suit].add(rank);
            }
        }
        return sel;
    }

    isValid() {
        if(this.mode === HandMode.SELECT) {
            return this.total() == 13
        } else {
            return true;
        }
    }

    // Plain object of the hand (sets as arrays) for JSON, and back
    snapshot() {
        return {
            handid: this.handid,
            mode: this.mode,
            data: this.mode === HandMode.SELECT
                ? Object.fromEntries(suits.map((suit) => [suit, [...this.data[suit]]]))
                : this.data
        };
    }

    static fromSnapshot(snap) {
        const sel = new Selection(snap.handid);
        sel.setMode(snap.mode);
        if(snap.mode === HandMode.SELECT) {
            for(const suit of suits) sel.data[suit] = new Set(snap.data[suit] || []);
        } else {
            sel.data = snap.data;
        }
        return sel;
    }
}

export class Hands {
    constructor() {
        this.data = [new Selection(handIds[0])];
    }

    edited() {
        return this.data[this.data.length - 1];
    }

    push() {
        this.data.push(new Selection(handIds[this.data.length]));
    }

    has(suit, rank) {
        return this.data.filter((sel) => sel.has(suit,rank))
                        .length > 0;
    }

    hasPushed() {
        return this.data.length > 1;
    }

    pushed() {
        return this.data.slice(0, this.data.length - 1);
    }

    canPush() {
        return this.data.length <= 4 && this.edited().isValid();
    }

    // Hands pushed as snapshots of Selection, a new one edited after them
    static fromSnapshots(snaps) {
        const ans = new Hands();
        ans.data = snaps.map((snap) => Selection.fromSnapshot(snap));
        ans.data.push(new Selection(handIds[ans.data.length]));
        return ans;
    }

    static fromFormulas(formulas) {
        const ans = new Hands();
        ans.data = formulas.map((str, i) => Selection.fromFormula(handIds[i], str));
        ans.data.push(new Selection(handIds[ans.data.length]));
        return ans;
    }
}
