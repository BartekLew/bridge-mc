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
    if(body["ID"]) {
        const url = new URL(window.location);
        url.searchParams.set("sim", body["ID"]);
        window.history.replaceState(null, "", url);
    }
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

// Splits played cards of one contract into sequences of tricks and, within
// each, distributes them to the hands they come from. First trick is led by
// declarer's LHO; WINNERS holds position of each trick's winner relative to
// that trick's leader, who leads the next one.
// Returns [{ played:    { N: [{card, trick, lead}...], E: ..., S: ..., W: ... },
//            remaining: { N: { '♠': ['A', 'K'...], '♥': ...}, E: ... },
//            comment:   ['line'...] }...]
// where remaining are cards held before the sequence is played.
function playSequences(axis) {
    var leader = (seats.indexOf(declarerSeat(axis)) + 1) % 4;
    var trickNo = 0;

    const played = axis['SEQUENCES'].map((seq) => {
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

    // Cards held before a sequence are those played in it and later
    return played.map((seq, k) => {
        const remaining = Object.fromEntries(seats.map((seat) => [
            seat,
            Object.fromEntries(suits.map((suit) => [suit, []]))
        ]));
        for(const later of played.slice(k)) {
            for(const seat of seats) {
                for(const { card } of later[seat]) {
                    remaining[seat][card[0]].push(card.slice(1));
                }
            }
        }
        for(const seat of seats) {
            for(const suit of suits) {
                remaining[seat][suit].sort((a, b) => ranks.indexOf(a) - ranks.indexOf(b));
            }
        }
        return {
            played: seq,
            remaining: remaining,
            comment: axis['SEQUENCES'][k]['COMMENT'].split('\n').filter(Boolean)
        };
    });
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
        <p>Probable outcome is {{simulationReport['SCORE'][0]}} vs defence {{simulationReport['SCORE'][1]}}</p>
        <div class="flex flex-col gap-8 mt-4">
          <div v-for="axis in simulationReport['POWER']">
            <p class="text-center text-lg">
              <b>{{ axis['TRUMP'] }}</b> by <b>{{ declarerSeat(axis) }}</b>:
              {{ axis['SCORE'] }} tricks
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

