<!--
   Copyright (C) 2025  Bartosz "Lew" Pastudzki <lew@wiedz.net.pl>
 
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
-->

<script setup>
import { ref, computed, onMounted } from 'vue'

// Suits and ranks
const suits = ['♠', '♥', '♦', '♣'];
const suitLetters = { '♠':'S', '♥':'H', '♦':'D', '♣':'C'};
const suitForVals = { "C":"♣", "D": "♦", "H": "♥", "S":"♠" };
const ranks = ['A','K','Q','J','10','9','8','7','6','5','4','3','2'];
const handIds = ['N', 'E', 'S', 'W', ''];

const HandMode = { SELECT: "Select cards", RANDOM: "Random hand" };

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

class Selection {
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
}

class Hands {
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

    static fromFormulas(formulas) {
        const ans = new Hands();
        ans.data = formulas.map((str, i) => Selection.fromFormula(handIds[i], str));
        ans.data.push(new Selection(handIds[ans.data.length]));
        return ans;
    }
}

const hands = ref(new Hands());
const edited = computed(() => hands.value.edited());
const measures = ref("");

const suitIndex = ref(0)
const currentSuit = computed(() => suits[suitIndex.value])

function pushHand() {
    hands.value.push();
    suitIndex.value = 0;
}

// Total selected cards
const totalSelected = computed(() => edited.value.total());

// Switch suits
function nextSuit() { suitIndex.value = (suitIndex.value + 1) % suits.length }
function prevSuit() { suitIndex.value = (suitIndex.value - 1 + suits.length) % suits.length }

// Computed formula string
const formula = computed(() => edited.value.formula()); 

const simulationReport = ref("");
async function playSimulation() {
    const response = await fetch("api/simdeal", {
                                    method: "POST",
                                    headers: {
                                        'Accept': 'application/json',
                                        'Content-Type': 'application/json',
                                    },
                                    body: JSON.stringify({
                                        hands: hands.value.pushed().map((hand)=>hand.formula())
                                    })
                                });
    if(!response.ok) {
        const text = await response.text(); // get response body (error message)
        console.error("Fetch failed:", response.status, response.statusText, text);
        alert(`Backend error ${response.status}: ${response.statusText}\n${text}`);
        return;
    }

    const body = await response.json();
    simulationReport.value = body;
    simId.value = body["ID"];
    choices.value = null;
    if(body["ID"]) {
        const url = new URL(window.location);
        url.searchParams.set("sim", body["ID"]);
        window.history.replaceState(null, "", url);
    }
    loadHints();
}

// Restore a saved deal simulation given as ?sim=<id>
async function restoreSimulation(id) {
    const response = await fetch("api/simdeal/" + encodeURIComponent(id));
    if(!response.ok) {
        const text = await response.text();
        console.error("Fetch failed:", response.status, response.statusText, text);
        alert(`Cannot restore simulation ${id}: ${response.status}\n${text}`);
        return;
    }

    const saved = await response.json();
    hands.value = Hands.fromFormulas(saved["HANDS"]);
    simulationReport.value = saved["RESULT"];
    simId.value = id;
    choices.value = null;
    loadHints();
}

// Id of the saved simulation shown, which play edits apply to
const simId = ref(null);

// Tricks offered when the card clicked doesn't take the next trick in a
// single way: { axis, card, takes, leader, tricks: [{ TRICK, WINNER }...] }; or
// the variants of a plan: { axis, plan, label, leader,
// tricks: [{ PLAN, LABEL, TRICKS: [{ TRICK, WINNER, LEADER }...] }...] }
const choices = ref(null);

function simEditRequest(axisNo, action, extra = {}) {
    return fetch("api/simedit", {
                    method: "POST",
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                    },
                    body: JSON.stringify({ id: simId.value, axis: axisNo, action: action, ...extra })
                });
}

// Plans the side on lead can apply, by contract (axis) not played out:
// [{ PLAN, LABEL, TRICKS, VARIANTS }...], TRICKS being how many the plan plays
// (in its longest variant), VARIANTS how many ways it can be played
const hints = ref({});

async function loadHints() {
    const power = simulationReport.value && simulationReport.value["POWER"];
    if(!simId.value || !power) {
        hints.value = {};
        return;
    }
    const all = {};
    await Promise.all(power.map(async (axis, axisNo) => {
        if(playDone(axis)) return;
        const response = await simEditRequest(axisNo, 'hints');
        if(response.ok) all[axisNo] = (await response.json())["HINTS"];
    }));
    hints.value = all;
}

// Plan as the backend sends it ({ TYPE, SUIT, SEAT }) to one for a request
function planRequest(plan) {
    return { type: plan["TYPE"], suit: plan["SUIT"], seat: plan["SEAT"] };
}

function applyHint(axisNo, hint) {
    editPlay(axisNo, 'plan', { plan: planRequest(hint["PLAN"]) });
}

// Applies an edit (pop, clear, split, play, trick or plan, see sim-edit in
// rest.cl) to play of contract axisNo of the saved simulation
async function editPlay(axisNo, action, extra = {}) {
    const response = await simEditRequest(axisNo, action, extra);
    if(!response.ok) {
        const text = await response.text();
        console.error("Fetch failed:", response.status, response.statusText, text);
        alert(`Cannot edit play: ${response.status}\n${text}`);
        return;
    }

    const body = await response.json();
    if(body["CHOICES"]) {
        choices.value = {
            axis: axisNo,
            card: extra.card,
            takes: body["TAKES"],
            leader: body["LEADER"],
            plan: extra.plan,
            label: body["LABEL"],
            tricks: body["CHOICES"]
        };
    } else {
        simulationReport.value = body["RESULT"];
        choices.value = null;
        loadHints();
    }
}

onMounted(() => {
    const id = new URLSearchParams(window.location.search).get("sim");
    if(id) {
        restoreSimulation(id);
    }
});

// Background colour of each suit (with white text), both in played and remaining cards
const suitColors = { '♠': '#1e3a8a', '♥': '#dc2626', '♦': '#ea580c', '♣': '#15803d' };

const seats = ['N', 'E', 'S', 'W'];

// Hand computed from remaining cards has no proper id; it's always the last one (W)
function declarerSeat(axis) {
    return seats.includes(axis['DECLARER']) ? axis['DECLARER'] : 'W';
}

function emptySeats() {
    return Object.fromEntries(seats.map((seat) => [seat, []]));
}

// Whole deal by seat: { N: { '♠': ['A', 'K'...], '♥': ...}, E: ... }. With
// three hands given, the last one (W) holds the remaining cards.
const dealHands = computed(() => {
    const pushed = hands.value.pushed();
    const deal = Object.fromEntries(seats.map((seat, i) => [
        seat,
        Object.fromEntries(suits.map((suit) => [
            suit,
            ranks.filter((rank) => pushed[i] ? pushed[i].has(suit, rank)
                                             : !pushed.some((sel) => sel.has(suit, rank)))
        ]))
    ]));
    return deal;
});

// Cards of deal not played in sequences
function cardsLeft(sequences) {
    const left = Object.fromEntries(seats.map((seat) => [
        seat,
        Object.fromEntries(suits.map((suit) => [suit, [...dealHands.value[seat][suit]]]))
    ]));
    for(const seq of sequences) {
        for(const trick of seq['TRICKS']) {
            for(const card of trick) {
                for(const seat of seats) {
                    left[seat][card[0]] = left[seat][card[0]].filter((rank) => rank !== card.slice(1));
                }
            }
        }
    }
    return left;
}

function playDone(axis) {
    const left = cardsLeft(axis['SEQUENCES']);
    return seats.every((seat) => suits.every((suit) => left[seat][suit].length == 0));
}

function tricksPlayed(axis) {
    return axis['SEQUENCES'].reduce((sum, seq) => sum + seq['TRICKS'].length, 0);
}

// Seat to lead after all tricks of axis
function nextLeader(axis) {
    var leader = (seats.indexOf(declarerSeat(axis)) + 1) % 4;
    for(const seq of axis['SEQUENCES']) {
        for(const winner of seq['WINNERS']) {
            leader = (leader + winner) % 4;
        }
    }
    return seats[leader];
}

// Distributes cards of a trick, listed from its leader, to seats:
// { N: card, E: ..., ...}
function trickBySeat(trick, leader) {
    const first = seats.indexOf(leader);
    return Object.fromEntries(trick.map((card, pos) => [seats[(first + pos) % 4], card]));
}

// Splits played cards of one contract into sequences of tricks and, within
// each, distributes them to the hands they come from. First trick is led by
// declarer's LHO; WINNERS holds position of each trick's winner relative to
// that trick's leader, who leads the next one. Empty sequences (one just
// started by the user) are skipped.
// Returns [{ played:    { N: [{card, trick, lead}...], E: ..., S: ..., W: ... },
//            remaining: { N: { '♠': ['A', 'K'...], '♥': ...}, E: ... },
//            comment:   ['line'...] }...]
// where remaining are cards held before the sequence is played.
function playSequences(axis) {
    var leader = (seats.indexOf(declarerSeat(axis)) + 1) % 4;
    var trickNo = 0;
    const sequences = axis['SEQUENCES'];

    const played = sequences.map((seq) => {
        const ans = emptySeats();
        seq['TRICKS'].forEach((trick, i) => {
            trick.forEach((card, pos) => {
                ans[seats[(leader + pos) % 4]].push({
                    card: card,
                    trick: trickNo,
                    lead: pos == 0
                });
            });
            leader = (leader + seq['WINNERS'][i]) % 4;
            trickNo++;
        });
        return ans;
    });

    return played.map((seq, k) => ({
                     played: seq,
                     remaining: cardsLeft(sequences.slice(0, k)),
                     comment: (sequences[k]['COMMENT'] || '').split('\n').filter(Boolean)
                 }))
                 .filter((seq, k) => sequences[k]['TRICKS'].length > 0);
}

function probabilityPercent(x) {
    return (x*100).toFixed(2) + "%";
}

function mcRepVal(x) {
    if(Array.isArray(x) && x.length == 0)
        return 'NT';

    const symbol = suitForVals[x];
    if(symbol)
        return symbol;
    else
        return x;
}

function mcTable (data) {
    var ans = [];

    function self(indent, cell) {
        var row = Array(indent).fill("»");

        var levels;
        if(Array.isArray(cell[0])) {
            row = row.concat(cell[0].map(mcRepVal));
            row.push(probabilityPercent(cell[1]));
            levels = cell[0].length;
        } else {
            row.push(mcRepVal(cell[0]))
            row.push(probabilityPercent(cell[1]));
            levels = 1;
        }

        ans.push(row);
        for(var subcase of cell.splice(2)) {
            self(indent+levels, subcase);
        }
    }

    for(var x of data) {
        self(0, x);
    }

    return ans;
}

async function mcSimulation(inMeasures) {
    var response = await fetch("api/mc", {
                                   method: "POST",
                                   headers: {
                                      'Accept': 'application/json',
                                      'Content-Type': 'application/json',
                                   },
                                   body: JSON.stringify({
                                       defs: hands.value.pushed().map((hand) => hand.restDef()),
                                       measures: inMeasures
                                   })
                               });

    if(!response.ok) {
        const text = await response.text(); // get response body (error message)
        console.error("Fetch failed:", response.status, response.statusText, text);
        alert(`Backend error ${response.status}: ${response.statusText}\n${text}`);
        return;
    }

    var body = await response.json();
    const jid = body["JOB-ID"];
    simulationReport.value = { DATA: mcTable(body['DATA']) };

    while(body["STATUS"] === "IN-PROGRESS") {
        await new Promise(resolve => setTimeout(resolve, 5000));
        response = await fetch("api/mc/" + jid, {
                                   method: "POST",
                                   body: JSON.stringify({
                                        measures: measures.value.split(" ").filter(Boolean)
                                   }),
                                   headers: {
                                      'Accept': 'application/json',
                                      'Content-Type': 'application/json',
                                   }
                               });

        if(!response.ok) {
            const text = await response.text(); // get response body (error message)
            console.error("Fetch failed:", response.status, response.statusText, text);
            alert(`Backend error ${response.status}: ${response.statusText}\n${text}`);
            return;
        }

        body = await response.json();
        simulationReport.value = { DATA: mcTable(body['DATA']) };
    }
}

async function runSimulation() {
    if(hands.value.pushed().filter((hand) => hand.mode === HandMode.RANDOM).length == 0
          && hands.value.pushed().length >= 3) {
       return playSimulation();
    } else {
       return mcSimulation(measures.value.split(" ").filter(Boolean));
    }
}

</script>

<template>
  <div class="min-h-screen flex flex-col items-center justify-center bg-gray-100 text-gray-800">

    <!-- Pushed hands view -->
    <div class="bg-white shadow-lg rounded-2xl p-6 mb-4"
         :class="simulationReport && simulationReport['SCORE'] ? 'w-fit max-w-full' : 'w-96'"
         v-if="hands.hasPushed()">
      <h3 class="text-center">Pushed hands:</h3>
      <ul>
        <li v-for="hand in hands.pushed()"
            class="bg-gray-200 rounded-2xl p-4 shadow-sm text-center mb-2 mt-2">
            {{hand.formula()}}
        </li>
      </ul>

      <input type="string"
             v-model="measures"
             style="width:100%"
             class="bg-gray-200 rounded-2xl p-2 shadow-sm text-center mb-0 mt-1 border" /> 

      <!-- Start simulation button -->
        
      <button 
        @click="runSimulation()" 
        :disabled="hands.pushed().length == 0"
        class="mt-4 w-full py-2 rounded-xl font-semibold"
        :class="{
            'bg-gray-500': hands.pushed().length == 0,
            'bg-red-200 hover:bg-red-300': hands.pushed().length > 0 
        }"
      >
        Start simulation
      </button>

      <!-- Simulation report -->
      <!-- 1. MC simulation -->
      <div v-if="simulationReport && simulationReport['DATA']" class="mt-8">
        <p>Probability profile for that kind of deal:</p>

        <table class="mt-2">
            <tr v-for="row of simulationReport['DATA']">
                <td v-for="cell of row" class="pl-2 pr-2 text-center">
                    {{cell}}
                </td>
            </tr>
        </table>
      </div>

      <!-- 2. Deal simulation -->
      <div v-if="simulationReport && simulationReport['SCORE']" class="mt-8">
        <p v-if="simulationReport['POWER'].every(playDone)">
          Probable outcome is {{simulationReport['SCORE'][0]}} vs defence {{simulationReport['SCORE'][1]}}
        </p>
        <div class="flex flex-col gap-8 mt-4">
          <div v-for="(axis, axisNo) in simulationReport['POWER']">
            <p class="text-center text-lg">
              <b>{{ axis['TRUMP'] }}</b> by <b>{{ declarerSeat(axis) }}</b>:
              <template v-if="playDone(axis)">{{ axis['SCORE'] }} tricks</template>
              <template v-else>
                {{ axis['SCORE'] }} tricks to declarer of {{ tricksPlayed(axis) }} played
              </template>
            </p>

            <!-- Play split into short sequences of tricks. Each one shows cards
                 held before it, then table view of the sequence: one row of
                 played cards per hand, N on top, W and E in the middle, S at the
                 bottom. Column tells the trick, colour the suit, underlined
                 card led the trick. Comment on the sequence goes below. -->
            <div v-for="(seq, k) in playSequences(axis)" :key="k" class="mt-4 border-t pt-4">
              <div class="flex flex-wrap justify-center items-center gap-x-10 gap-y-2">
                <!-- Cards held before the sequence -->
                <div class="grid grid-cols-3 gap-x-4 gap-y-1 items-center">
                  <template v-for="(seat, cell) in [null, 'N', null, 'W', null, 'E', null, 'S', null]"
                            :key="cell">
                    <div v-if="seat" class="font-mono text-sm leading-tight">
                      <div class="font-bold"
                           :class="{ 'text-blue-700': seat === declarerSeat(axis) }">{{ seat }}</div>
                      <div v-for="suit in suits" :key="suit"
                           class="whitespace-nowrap rounded px-1 mt-0.5 text-white"
                           :style="{ background: suitColors[suit] }">
                        {{ suit }} {{ seq.remaining[seat][suit].join('') || '—' }}
                      </div>
                    </div>
                    <div v-else></div>
                  </template>
                </div>

                <!-- Tricks of the sequence -->
                <div class="flex flex-col items-center gap-2">
                  <div v-for="row in [['N'], ['W', 'E'], ['S']]"
                       class="flex gap-10">
                    <div v-for="seat in row" class="flex items-center gap-1">
                      <span class="w-6 font-bold text-center"
                            :class="{ 'text-blue-700': seat === declarerSeat(axis) }">
                        {{ seat }}
                      </span>
                      <span v-for="played in seq.played[seat]"
                            :key="played.card"
                            :title="`Trick ${played.trick + 1}`"
                            class="w-9 rounded py-0.5 text-center font-mono text-sm text-white"
                            :class="{ 'font-bold underline': played.lead }"
                            :style="{ background: suitColors[played.card[0]] }">
                        {{ played.card }}
                      </span>
                    </div>
                  </div>
                </div>
              </div>

              <!-- w-0 min-w-full: text wraps to the width of cards above
                   instead of widening the panel -->
              <div class="w-0 min-w-full text-sm text-center mt-3">
                <p v-for="line in seq.comment">{{ line }}</p>
              </div>
            </div>

            <!-- Play editor: while cards are left, a click on one of them
                 adds the next trick, taken by that card. When it can't take
                 it, or can in several ways, the tricks to choose from are
                 listed instead. -->
            <div v-if="simId" class="mt-4 border-t pt-4 flex flex-col items-center gap-3">
              <template v-if="!playDone(axis)">
                <p class="text-sm text-gray-600">
                  Click a card to take the next trick; <b>{{ nextLeader(axis) }}</b> leads.
                  <template v-if="axis['SEQUENCES'].length > 0
                                  && axis['SEQUENCES'].at(-1)['TRICKS'].length == 0">
                    Tricks go to a new sequence.
                  </template>
                </p>

                <!-- Plans of the side on lead; a click plays all their tricks -->
                <div v-if="hints[axisNo] && hints[axisNo].length > 0"
                     class="flex flex-wrap justify-center gap-2">
                  <button v-for="hint in hints[axisNo]" :key="hint['LABEL']"
                          class="px-3 py-1 rounded bg-blue-100 hover:bg-blue-200 text-sm"
                          :title="hint['VARIANTS'] > 1
                                  ? `Up to ${hint['TRICKS']} trick(s), ${hint['VARIANTS']} variants to choose from`
                                  : `Plays ${hint['TRICKS']} trick(s)`"
                          @click="applyHint(axisNo, hint)">
                    {{ hint['LABEL'] }}
                    <span class="text-gray-500">({{ hint['TRICKS'] }}<template
                      v-if="hint['VARIANTS'] > 1">, {{ hint['VARIANTS'] }} variants</template>)</span>
                  </button>
                </div>
                <div class="grid grid-cols-3 gap-x-4 gap-y-1 items-center">
                  <template v-for="(seat, cell) in [null, 'N', null, 'W', null, 'E', null, 'S', null]"
                            :key="cell">
                    <div v-if="seat" class="font-mono text-sm leading-tight">
                      <div class="font-bold"
                           :class="{ 'text-blue-700': seat === declarerSeat(axis),
                                     'underline': seat === nextLeader(axis) }">{{ seat }}</div>
                      <div v-for="suit in suits" :key="suit"
                           class="whitespace-nowrap rounded px-1 mt-0.5 text-white"
                           :style="{ background: suitColors[suit] }">
                        {{ suit }}
                        <button v-for="rank in cardsLeft(axis['SEQUENCES'])[seat][suit]"
                                :key="rank"
                                class="rounded px-0.5 hover:bg-white hover:text-black"
                                @click="editPlay(axisNo, 'play', { card: suit + rank })">{{ rank }}</button>
                        <span v-if="cardsLeft(axis['SEQUENCES'])[seat][suit].length == 0">—</span>
                      </div>
                    </div>
                    <div v-else></div>
                  </template>
                </div>

                <!-- Tricks to choose from; cards by seat, winner ringed -->
                <div v-if="choices && choices.axis === axisNo"
                     class="flex flex-col items-center gap-2 border rounded p-3">
                  <p class="text-sm">
                    <template v-if="choices.label">
                      {{ choices.label }}: choose a variant:
                    </template>
                    <template v-else-if="choices.takes">
                      {{ choices.card }} can take the trick in several ways:
                    </template>
                    <template v-else>
                      {{ choices.card }} can't take the trick. Choose a trick taken by another card:
                    </template>
                  </p>
                  <!-- A plan variant is a line of tricks with its label, else a single trick -->
                  <button v-for="(choice, i) in choices.tricks" :key="i"
                          class="flex flex-col items-start gap-1 rounded px-2 py-1 hover:bg-gray-200"
                          @click="choice['TRICKS']
                                  ? editPlay(axisNo, 'trick', { tricks: choice['TRICKS'].map(t => t['TRICK']),
                                                                plan: planRequest(choice['PLAN']) })
                                  : editPlay(axisNo, 'trick', { trick: choice['TRICK'], plan: choices.plan })">
                    <span v-if="choice['LABEL'] && choice['LABEL'] !== choices.label"
                          class="text-sm font-semibold">{{ choice['LABEL'] }}</span>
                    <span v-for="(line, j) in (choice['TRICKS']
                                               || [{ ...choice, LEADER: choices.leader }])"
                          :key="j" class="flex gap-3">
                      <span v-for="(card, seat) in trickBySeat(line['TRICK'], line['LEADER'])"
                            :key="seat" class="flex items-center gap-1">
                        <span class="font-bold text-sm">{{ seat }}</span>
                        <span class="w-9 rounded py-0.5 text-center font-mono text-sm text-white"
                              :class="{ 'underline font-bold': seat === line['LEADER'],
                                        'ring-2 ring-offset-1 ring-black': card === line['TRICK'][line['WINNER']] }"
                              :style="{ background: suitColors[card[0]] }">{{ card }}</span>
                      </span>
                    </span>
                  </button>
                  <button class="text-sm text-gray-600 underline" @click="choices = null">Cancel</button>
                </div>
              </template>

              <div class="flex gap-2">
                <button class="px-3 py-1 rounded bg-gray-200 hover:bg-gray-300 disabled:opacity-50"
                        :disabled="tricksPlayed(axis) == 0"
                        @click="editPlay(axisNo, 'pop')">Remove last trick</button>
                <button class="px-3 py-1 rounded bg-gray-200 hover:bg-gray-300 disabled:opacity-50"
                        :disabled="tricksPlayed(axis) == 0"
                        @click="editPlay(axisNo, 'clear')">Remove all tricks</button>
                <button class="px-3 py-1 rounded bg-gray-200 hover:bg-gray-300 disabled:opacity-50"
                        :disabled="playDone(axis) || axis['SEQUENCES'].length == 0
                                   || axis['SEQUENCES'].at(-1)['TRICKS'].length == 0"
                        @click="editPlay(axisNo, 'split')">New sequence</button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

    <div class="bg-white shadow-lg rounded-2xl p-6 w-96">
      <!-- Mode switch -->
      <div class="flex justify-between mb-4 items-center space-x-2">
        <button 
            v-for="mode in Object.values(HandMode)"
            :key="mode"
            @click="edited.setMode(mode)" 
            :disabled="edited.mode === mode"
            class="mt-4 w-full py-2 rounded-xl font-semibold"
            :class="{
                'bg-gray-200': edited.mode != mode,
                'bg-green-300 hover:bg-green-400': edited.mode == mode
            }"
        >
            {{ mode }}
        </button>
      </div>

      <!-- Edited hand view -->
      <input 
        type="text"
        class="w-full mb-4 p-2 border rounded text-center font-mono text-lg"
        :value="formula"
        readonly
      />

      <!-- Hand settings -->
      <div v-if="edited.mode == HandMode.SELECT">  <!-- Manual hand settings -->
          <!-- Suit selector -->
          <div class="flex justify-between mb-4 items-center">
            <button @click="prevSuit" class="px-3 py-2 bg-gray-200 rounded-xl hover:bg-gray-300">◀</button>
            <div class="text-3xl">{{ currentSuit }}</div>
            <button @click="nextSuit" class="px-3 py-2 bg-gray-200 rounded-xl hover:bg-gray-300">▶</button>
          </div>

          <!-- Rank buttons -->
          <div class="grid grid-cols-3 gap-2">
            <button
                v-for="rank in ranks"
                :key="rank"
                @click="edited.toggle(currentSuit, rank)"
                :class="{
                    'bg-blue-300': hands.has(currentSuit, rank),
                    'bg-blue-100 hover:bg-blue-200': !hands.has(currentSuit, rank),
                    'cursor-not-allowed opacity-50': !hands.has(currentSuit, rank) && totalSelected >= 13
                }"
                class="rounded-xl py-2 text-lg font-semibold"
                :disabled="!hands.has(currentSuit, rank) && totalSelected >= 13"
            >
                {{ currentSuit }} {{ rank }}
            </button>
          </div>

          <!-- Info -->
          <div class="text-sm text-gray-500 mt-2">
            {{ totalSelected }}/13 cards selected
          </div>
      </div>
      <div v-else> <!-- Random hand generator parameters -->
          <div v-for="(ranges, section) in edited.data" :key="section"
               class="mb-2 p-4 bg-gray-50 rounded-2xl shadow-sm">
            <h3 class="font-bold mt-1 mb-2 text-center">{{section}} settings</h3>

            <div class="grid grid-cols-2 gap-4 ml-4 mr-4 text-center">
                <div v-for="(range, rangeName) in ranges" :key="rangeName"
                     class="flex flex-col items-center gap-1">
                   <label class="text-center">{{ rangeName }} range</label>
                   <div class="flex">
                     <input type="number"
                            v-model="ranges[rangeName][0]"
                            class="border rounded p-1 w-1/2 text-right mr-3" /> -
                     <input type="number"
                            v-model="ranges[rangeName][1]"
                            class="border rounded p-1 w-1/2 text-right ml-3" />
                  </div>
                </div>
            </div>
          </div>
      </div>
      <!-- Clear button -->
        <button 
        @click="edited.clear" 
        class="mt-4 w-full py-2 bg-red-200 hover:bg-red-300 rounded-xl font-semibold"
      >
        Clear
      </button>

      <!-- Push button -->
      <button 
        @click="pushHand" 
        :disabled="!hands.canPush()"
        class="mt-4 w-full py-2 rounded-xl font-semibold"
        :class="{
            'bg-gray-500': !hands.canPush(),
            'bg-red-200 hover:bg-red-300': hands.canPush() 
        }"
      >
        Push
      </button>


    </div>
  </div>
</template>

