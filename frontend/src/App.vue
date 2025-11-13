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
import { ref, computed } from 'vue'

// Suits and ranks
const suits = ['♠', '♥', '♦', '♣'];
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
}

const hands = ref(new Hands());
const edited = computed(() => hands.value.edited());

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
async function runSimulation() {
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
}

</script>

<template>
  <div class="min-h-screen flex flex-col items-center justify-center bg-gray-100 text-gray-800">

    <!-- Pushed hands view -->
    <div class="bg-white shadow-lg rounded-2xl p-6 w-96 mb-4"
         v-if="hands.hasPushed()">
      <h3 class="text-center">Pushed hands:</h3>
      <ul>
        <li v-for="hand in hands.pushed()"
            class="bg-gray-200 rounded-2xl p-4 shadow-sm text-center mb-2 mt-2">
            {{hand.formula()}}
        </li>
      </ul>

      <!-- Start simulation button -->
      <div v-if="hands.pushed().length == 4">
        
      <button 
        @click="runSimulation()" 
        :disabled="hands.pushed().length < 4"
        class="mt-4 w-full py-2 rounded-xl font-semibold"
        :class="{
            'bg-gray-500': hands.pushed().length < 4,
            'bg-red-200 hover:bg-red-300': hands.pushed().length == 4 
        }"
      >
        Start simulation
      </button>
      </div>

      <!-- Simulation report -->
      <div v-if="simulationReport" class="mt-8">
        <p>Probable outcome is {{simulationReport['SCORE'][0]}} vs defence {{simulationReport['SCORE'][1]}}</p>
        <div class="grid grid-cols-2 mt-4">
          <div v-for="axis in simulationReport['POWER']">
            <p v-for="(value, key) in axis"
               :key="key"
               class="text-center">
              <b>{{key}}</b>:
              <pre v-if="key === 'TRICKS'">{{
                 value.map((trick) => trick.join(" "))
                      .join("\n") }}
              </pre>
              <span v-else>{{value}}</span>
            </p>
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

