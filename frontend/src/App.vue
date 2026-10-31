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
import PlayAxis from './PlayAxis.vue'
import HandEditor from './HandEditor.vue'
import { suits, ranks, seats, playDone } from './play.js'
import { HandMode, Hands } from './hands.js'

const suitForVals = { "C":"♣", "D": "♦", "H": "♥", "S":"♠" };

const hands = ref(new Hands());
const measures = ref("");
// Play engine of a deal simulation: "deal" (play-deal) or "hints" (hint search)
const engine = ref("deal");
const simulating = ref(false);

const simulationReport = ref("");

async function playSimulation(formulas = hands.value.pushed().map((hand)=>hand.formula())) {
    newMcToken();
    const response = await fetch("api/simdeal", {
                                    method: "POST",
                                    headers: {
                                        'Accept': 'application/json',
                                        'Content-Type': 'application/json',
                                    },
                                    body: JSON.stringify({
                                        hands: formulas,
                                        engine: engine.value,
                                        // contracts to play, else the backend picks them
                                        contracts: measures.value.split(" ").filter(Boolean)
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
    if(body["ID"]) {
        const url = new URL(window.location);
        url.searchParams.delete("play");
        url.searchParams.delete("engine");
        url.searchParams.delete("measures");
        url.searchParams.set("sim", body["ID"]);
        window.history.replaceState(null, "", url);
    }
}

// URL of a page playing the deal of formulas by engine, in the contracts of
// measures when given (see onMounted)
function playUrl(formulas) {
    const url = new URL(window.location);
    url.search = "";
    url.searchParams.set("play", JSON.stringify(formulas));
    url.searchParams.set("engine", engine.value);
    if(measures.value.trim()) url.searchParams.set("measures", measures.value.trim());
    return url.toString();
}

// Plays the deal given as ?play=<formulas as JSON>&engine=<engine>
// (&measures=<contracts>), as if its hands were pushed and the simulation
// started
async function playFromUrl(param, eng, measureStr) {
    let formulas;
    try {
        formulas = JSON.parse(param);
    } catch(e) {
        alert(`Wrong deal in the URL: ${param}`);
        return;
    }
    hands.value = Hands.fromFormulas(formulas);
    if(eng) engine.value = eng;
    if(measureStr) measures.value = measureStr;
    simulating.value = true;
    try {
        await playSimulation(formulas);
    } finally {
        simulating.value = false;
    }
}

// Restore a saved deal simulation given as ?sim=<id>
async function restoreSimulation(id) {
    newMcToken();
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
}

// Id of the saved simulation shown, which play edits apply to
const simId = ref(null);

onMounted(() => {
    const params = new URLSearchParams(window.location.search);
    const id = params.get("sim");
    if(id) {
        restoreSimulation(id);
    } else if(params.get("play")) {
        playFromUrl(params.get("play"), params.get("engine"), params.get("measures"));
    } else if(params.get("mc")) {
        connectMc(params.get("mc"), params.get("measures"));
    }
});

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

// Rows of the histogram table: cells to show, and path, the values of the
// line from the top (null where the deals' values end), which names its group
function mcTable (data) {
    var ans = [];

    function self(indent, cell, prefix) {
        var row = Array(indent).fill("»");

        var levels, vals;
        if(Array.isArray(cell[0])) {
            row = row.concat(cell[0].map(mcRepVal));
            row.push(probabilityPercent(cell[1]));
            levels = cell[0].length;
            vals = cell[0].length ? cell[0] : [null];
        } else {
            row.push(mcRepVal(cell[0]))
            row.push(probabilityPercent(cell[1]));
            levels = 1;
            vals = [cell[0]];
        }

        const path = prefix.concat(vals);
        ans.push({ cells: row, path: path });
        for(var subcase of cell.splice(2)) {
            self(indent+levels, subcase, path);
        }
    }

    for(var x of data) {
        self(0, x, []);
    }

    return ans;
}

// Deals of a group of the MC simulation: { path, label, count, deals }
const mcGroup = ref(null);
async function loadGroup(row) {
    const response = await fetch("api/mc/" + simulationReport.value['JOB'] + "/deals", {
                                   method: "POST",
                                   headers: {
                                      'Accept': 'application/json',
                                      'Content-Type': 'application/json',
                                   },
                                   body: JSON.stringify({
                                        prefix: row.path,
                                        measures: measures.value.split(" ").filter(Boolean)
                                   })
                               });
    if(!response.ok) {
        const text = await response.text();
        console.error("Fetch failed:", response.status, response.statusText, text);
        alert(`Backend error ${response.status}: ${response.statusText}\n${text}`);
        return;
    }
    const body = await response.json();
    mcGroup.value = {
        path: row.path,
        label: row.path.map((v) => v === null ? "(end)" : mcRepVal(v)).join(" "),
        count: body['COUNT'],
        deals: body['DEALS']
    };
}


async function mcSimulation(inMeasures) {
    const token = newMcToken();
    var response = await fetch("api/mc", {
                                   method: "POST",
                                   headers: {
                                      'Accept': 'application/json',
                                      'Content-Type': 'application/json',
                                   },
                                   body: JSON.stringify({
                                       defs: hands.value.pushed().map((hand) => hand.restDef()),
                                       measures: inMeasures,
                                       setup: JSON.stringify({
                                           hands: hands.value.pushed().map((hand) => hand.snapshot()),
                                           measures: measures.value
                                       })
                                   })
                               });

    if(!response.ok) {
        const text = await response.text(); // get response body (error message)
        console.error("Fetch failed:", response.status, response.statusText, text);
        alert(`Backend error ${response.status}: ${response.statusText}\n${text}`);
        return;
    }

    const body = await response.json();
    if(token !== mcToken) return;
    const jid = body["JOB-ID"];
    mcGroup.value = null;
    simulationReport.value = { DATA: mcTable(body['DATA']), JOB: jid, STATUS: body["STATUS"] };
    mcUrl(jid);
    // Polled on without holding the simulate button: starting another
    // simulation retires this one (newMcToken)
    if(body["STATUS"] === "IN-PROGRESS") {
        setTimeout(() => pollMc(jid, token), 5000);
    }
}

// Token of the MC job shown: starting or opening another simulation takes a
// new one, so that polling of the job shown before stops and its answers
// still on the way are dropped (the job itself runs on in the backend)
let mcToken = 0;
function newMcToken() {
    return ++mcToken;
}

// Puts MC job jid and the measures in the URL (?mc=<jid>&measures=...), so
// that reloading the page or opening the link later connects to the job again
function mcUrl(jid) {
    const url = new URL(window.location);
    url.search = "";
    url.searchParams.set("mc", jid);
    if(measures.value.trim()) url.searchParams.set("measures", measures.value.trim());
    window.history.replaceState(null, "", url);
}

// Shows the histogram of MC job jid, every 5 seconds while it runs and token
// is the current one. With restore, the hands and measures the job was
// started with come back too. The job lives in the backend's memory: a
// restart of it loses the job.
async function pollMc(jid, token, restore = false) {
    while(token === mcToken) {
        const response = await fetch("api/mc/" + jid, {
                                   method: "POST",
                                   body: JSON.stringify({
                                        measures: measures.value.split(" ").filter(Boolean)
                                   }),
                                   headers: {
                                      'Accept': 'application/json',
                                      'Content-Type': 'application/json',
                                   }
                               });

        if(token !== mcToken) return;
        if(!response.ok) {
            const text = await response.text(); // get response body (error message)
            console.error("Fetch failed:", response.status, response.statusText, text);
            alert(`Cannot get MC job ${jid}: ${response.status} ${response.statusText}\n${text}`);
            return;
        }

        const body = await response.json();
        if(token !== mcToken) return;
        if(restore && typeof body["SETUP"] === "string") {
            const setup = JSON.parse(body["SETUP"]);
            hands.value = Hands.fromSnapshots(setup.hands);
            measures.value = setup.measures || "";
            restore = false;
        }
        simulationReport.value = { DATA: mcTable(body['DATA']), JOB: jid, STATUS: body["STATUS"] };
        if(body["STATUS"] !== "IN-PROGRESS") return;
        await new Promise(resolve => setTimeout(resolve, 5000));
    }
}

// Connects to MC job jid run before (its id from the URL or typed in)
const mcJobInput = ref("");
async function connectMc(jid, measureStr = measures.value) {
    jid = String(jid).trim();
    if(!/^\d+$/.test(jid)) {
        alert(`MC job id is a number, not "${jid}"`);
        return;
    }
    const token = newMcToken();
    measures.value = measureStr || "";
    mcGroup.value = null;
    simId.value = null;
    mcUrl(jid);
    await pollMc(jid, token, true);
}

async function runSimulation() {
    simulating.value = true;
    try {
        if(hands.value.pushed().filter((hand) => hand.mode === HandMode.RANDOM).length == 0
              && hands.value.pushed().length >= 3) {
           return await playSimulation();
        } else {
           return await mcSimulation(measures.value.split(" ").filter(Boolean));
        }
    } finally {
        simulating.value = false;
    }
}

</script>

<template>
  <div class="min-h-screen flex flex-col items-center justify-center bg-gray-100 text-gray-800">

    <div class="mb-4 flex items-center gap-4">
      <h1 class="text-2xl font-bold">Simulator</h1>
      <a href="training.html" class="text-sm text-blue-700 underline">Play training</a>
    </div>

    <!-- Connecting to an MC job run before, by its id -->
    <div class="mb-4 flex gap-2 items-center text-sm">
      <span>MC job</span>
      <input v-model="mcJobInput" @keyup.enter="connectMc(mcJobInput)"
             placeholder="id" size="5"
             class="bg-white rounded-xl px-2 py-1 border text-center" />
      <button @click="connectMc(mcJobInput)" :disabled="simulating || !mcJobInput.trim()"
              class="px-3 py-1 rounded-xl font-semibold"
              :class="simulating || !mcJobInput.trim() ? 'bg-gray-300' : 'bg-red-200 hover:bg-red-300'">
        Open
      </button>
    </div>

    <!-- Pushed hands view, also for an MC job connected to without hands -->
    <div class="bg-white shadow-lg rounded-2xl p-6 mb-4"
         :class="simulationReport && simulationReport['POWER'] ? 'w-fit max-w-full' : 'w-96'"
         v-if="hands.hasPushed() || (simulationReport && simulationReport['DATA'])">
      <h3 class="text-center" v-if="hands.hasPushed()">Pushed hands:</h3>
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
        
      <div class="mt-4 flex gap-2">
        <button
          @click="runSimulation()"
          :disabled="hands.pushed().length == 0 || simulating"
          class="flex-1 py-2 rounded-xl font-semibold"
          :class="{
              'bg-gray-500': hands.pushed().length == 0 || simulating,
              'bg-red-200 hover:bg-red-300': hands.pushed().length > 0 && !simulating
          }"
        >
          {{ simulating ? "Simulating…" : "Start simulation" }}
        </button>
        <!-- Play engine, for deals of known hands -->
        <select v-model="engine" :disabled="simulating"
                title="Play engine for a deal of known hands"
                class="bg-gray-200 rounded-xl px-2 border">
          <option value="deal">Play-deal</option>
          <option value="hints">Hints search</option>
        </select>
      </div>

      <!-- Simulation report -->
      <!-- 1. MC simulation -->
      <div v-if="simulationReport && simulationReport['DATA']" class="mt-8">
        <p>Probability profile for that kind of deal:</p>
        <p class="text-sm text-gray-600">
          MC job {{ simulationReport['JOB'] }},
          {{ simulationReport['STATUS'] === 'IN-PROGRESS' ? 'still running' : 'done' }}
        </p>

        <table class="mt-2">
            <tr v-for="row of simulationReport['DATA']"
                @click="loadGroup(row)"
                title="Show deals of this group"
                class="cursor-pointer hover:bg-red-100"
                :class="{ 'bg-red-200': mcGroup && JSON.stringify(mcGroup.path) === JSON.stringify(row.path) }">
                <td v-for="cell of row.cells" class="pl-2 pr-2 text-center">
                    {{cell}}
                </td>
            </tr>
        </table>

        <!-- Deals of the group clicked, each playable by the engine chosen -->
        <div v-if="mcGroup" class="mt-6">
          <p>
            Deals of <b>{{ mcGroup.label }}</b>: {{ mcGroup.count }}
            <template v-if="mcGroup.count > mcGroup.deals.length">
              (first {{ mcGroup.deals.length }} shown)
            </template>
          </p>
          <div v-for="deal in mcGroup.deals" :key="deal['INDEX']"
               class="mt-2 bg-gray-100 rounded-xl p-2 flex flex-wrap items-center gap-x-4 gap-y-1">
            <span v-for="hand in deal['HANDS']" class="font-mono text-sm">{{ hand }}</span>
            <span class="text-sm text-gray-600">{{ deal['VALUES'].map(mcRepVal).join(" ") }}</span>
            <!-- A plain link: the new tab plays the deal itself (?play=) -->
            <a :href="playUrl(deal['HANDS'])" target="_blank"
               :title="'Play this deal by ' + (engine === 'hints' ? 'hints search' : 'play-deal') + ' in a new tab'"
               class="ml-auto px-3 py-1 rounded-xl text-sm font-semibold bg-red-200 hover:bg-red-300">
              Play
            </a>
          </div>
        </div>
      </div>

      <!-- 2. Deal simulation -->
      <div v-if="simulationReport && simulationReport['POWER']" class="mt-8">
        <!-- SCORE only when one contract of each side was played -->
        <p v-if="simulationReport['SCORE']
                 && simulationReport['POWER'].every((axis) => playDone(dealHands, axis))">
          Probable outcome is {{simulationReport['SCORE'][0]}} vs defence {{simulationReport['SCORE'][1]}}
        </p>
        <div class="flex flex-col gap-2 mt-4">
          <PlayAxis v-for="(axis, axisNo) in simulationReport['POWER']" :key="`${simId}-${axisNo}`"
                    :axis="axis" :axis-no="axisNo" :sim-id="simId" :deal="dealHands"
                    @result="(result) => simulationReport = result" />
        </div>
      </div>
    </div>

    <HandEditor :hands="hands" />
  </div>
</template>

