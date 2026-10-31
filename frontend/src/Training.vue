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

<!-- Play training (training.cl): a random deal bid by the computer and
     played by the user from S (and dummy N when declaring) against the
     computer. The game lives in the backend, which only sends the cards the
     user may see. When it's over, its play is a saved simulation, editable,
     shown with the computer's play and the hint search's. -->
<script setup>
import { ref, computed, onMounted, onUnmounted } from 'vue'
import PlayAxis from './PlayAxis.vue'
import HandEditor from './HandEditor.vue'
import { suits, seats, suitColors, trickBySeat, formulasDeal, postJson } from './play.js'
import { Hands } from './hands.js'

const roles = [
    { value: 'declarer', label: 'Declarer' },
    { value: 'leader', label: 'Defender on lead' },
    { value: 'partner', label: "Defender, leader's partner" },
    { value: 'defender', label: 'Defender, either' }
];
const role = ref('declarer');

// Hands of the deals to come, N E S W as in the simulator (N deals; seats
// after the pushed ones are random): cards or ranges. The deal is turned
// so that the user sits S in his role.
const hands = ref(new Hands());
const showHands = ref(false);

// The game as training-view in rest.cl gives it
const game = ref(null);
const busy = ref(false);

// Simulation of the game once it's over ({ HANDS, RESULT }), polled for
// while the hint search runs
const sim = ref(null);
let pollTimer = null;

function setUrl(id) {
    const url = new URL(window.location);
    url.search = "";
    if(id) url.searchParams.set("game", id);
    window.history.replaceState(null, "", url);
}

async function showGame(body) {
    game.value = body;
    setUrl(body["ID"]);
    if(body["SIM"] && (!sim.value || sim.value.id !== body["SIM"])) {
        await loadSim(body["SIM"]);
    }
}

async function newGame() {
    busy.value = true;
    try {
        sim.value = null;
        const defs = hands.value.pushed().map((hand) => hand.restDef());
        const body = await postJson("api/train", { role: role.value, defs: defs }, "Cannot deal");
        if(body) await showGame(body);
    } finally {
        busy.value = false;
    }
}

async function playCard(card) {
    if(busy.value) return;
    busy.value = true;
    try {
        const body = await postJson(`api/train/${game.value["ID"]}/play`, { card: card }, "Cannot play");
        if(body) await showGame(body);
    } finally {
        busy.value = false;
    }
}

async function restoreGame(id) {
    const response = await fetch("api/train/" + encodeURIComponent(id));
    if(!response.ok) {
        alert(`Cannot restore game ${id}: ${response.status}`);
        setUrl(null);
        return;
    }
    await showGame(await response.json());
}

async function loadSim(id) {
    clearTimeout(pollTimer);
    const response = await fetch("api/simdeal/" + encodeURIComponent(id));
    if(!response.ok) return;
    const saved = await response.json();
    sim.value = { id: id, hands: saved["HANDS"], result: saved["RESULT"] };
    if(saved["RESULT"]["PENDING"]) {
        pollTimer = setTimeout(() => loadSim(id), 3000);
    }
}

onMounted(() => {
    const id = new URLSearchParams(window.location.search).get("game");
    if(id) restoreGame(id);
});
onUnmounted(() => clearTimeout(pollTimer));

const over = computed(() => game.value && !game.value["NEXT"]);
const contract = computed(() => game.value && game.value["CONTRACT"]);
const declarer = computed(() => contract.value && contract.value["DECLARER"]);
const dummy = computed(() => declarer.value && seats[(seats.indexOf(declarer.value) + 2) % 4]);

function contractStr(c) {
    return `${c["LEVEL"]}${c["TRUMP"]}${c["DOUBLED"] || ''} by ${c["DECLARER"]}`;
}

// Auction in rows N E S W, starting at the dealer's column
const auctionRows = computed(() => {
    if(!game.value) return [];
    const calls = Array(seats.indexOf(game.value["DEALER"])).fill('').concat(game.value["AUCTION"]);
    const rows = [];
    for(let i = 0; i < calls.length; i += 4) rows.push(calls.slice(i, i + 4));
    return rows;
});

function callColor(call) {
    const suit = suits.find((s) => call.includes(s));
    return suit ? suitColors[suit] : undefined;
}

// Cards of seat by suit: { '♠': ['♠A', '♠10'...], ... }, or null when hidden
function handBySuit(seat) {
    const cards = game.value["HANDS"][seat];
    if(!cards) return null;
    return Object.fromEntries(suits.map((suit) => [suit, cards.filter((card) => card[0] === suit)]));
}

const trick = computed(() => game.value ? game.value["TRICK"] : null);

// Can the user play card from seat now: his turn there, following suit
function playable(seat, card) {
    if(busy.value || over.value || game.value["NEXT"] !== seat || !game.value["MINE"].includes(seat)) {
        return false;
    }
    const led = trick.value["CARDS"].length > 0 ? trick.value["CARDS"][0][0] : null;
    if(!led || card[0] === led) return true;
    return !game.value["HANDS"][seat].some((c) => c[0] === led);
}

// The trick before the current one, shown till the user plays to the current one
const previousTrick = computed(() => {
    if(!game.value || game.value["TRICKS"].length === 0 || over.value) return null;
    const played = trickBySeat(trick.value["CARDS"], trick.value["LEADER"]);
    if(game.value["MINE"].some((seat) => played[seat])) return null;
    return game.value["TRICKS"].at(-1);
});

const resultStr = computed(() => {
    if(!over.value) return "";
    const tricks = game.value["TAKEN"][0];
    const need = contract.value["LEVEL"] + 6;
    return tricks >= need ? `made${tricks > need ? ` +${tricks - need}` : ''} (${tricks} tricks)`
                          : `down ${need - tricks} (${tricks} tricks)`;
});

const simDeal = computed(() => sim.value && formulasDeal(sim.value.hands));
</script>

<template>
  <div class="min-h-screen flex flex-col items-center bg-gray-100 text-gray-800 py-6 gap-4">
    <div class="flex items-center gap-4">
      <h1 class="text-2xl font-bold">Play training</h1>
      <a href="./" class="text-sm text-blue-700 underline">Simulator</a>
    </div>

    <!-- Role and a new deal -->
    <div class="bg-white shadow-lg rounded-2xl p-4 flex flex-wrap gap-4 items-center justify-center">
      <label v-for="r in roles" :key="r.value" class="flex items-center gap-1 text-sm">
        <input type="radio" v-model="role" :value="r.value" /> {{ r.label }}
      </label>
      <button @click="newGame" :disabled="busy"
              class="px-4 py-1 rounded-xl font-semibold"
              :class="busy ? 'bg-gray-300' : 'bg-blue-200 hover:bg-blue-300'">
        {{ busy && !game ? 'Dealing…' : 'New deal' }}
      </button>
      <button @click="showHands = !showHands" class="text-sm text-blue-700 underline">
        {{ showHands ? 'Hide hands' : 'Set hands' }}{{ hands.hasPushed() ? ` (${hands.pushed().length} set)` : '' }}
      </button>
    </div>

    <!-- Hands of the deals to come, as in the simulator -->
    <div v-if="showHands" class="flex flex-col items-center gap-2">
      <div class="bg-white shadow-lg rounded-2xl p-4 w-96 text-sm">
        <p class="text-gray-600">
          Hands N E S W (N deals), by cards or ranges; the seats after them are
          random. The deal is turned so that you sit S in your role.
        </p>
        <ul v-if="hands.hasPushed()">
          <li v-for="hand in hands.pushed()"
              class="bg-gray-200 rounded-2xl p-2 shadow-sm text-center mt-2 font-mono">
            {{ hand.formula() }}
          </li>
        </ul>
        <button v-if="hands.hasPushed()" @click="hands = new Hands()"
                class="mt-2 w-full py-1 rounded-xl font-semibold bg-red-200 hover:bg-red-300">
          Clear all
        </button>
      </div>
      <HandEditor :hands="hands" />
    </div>

    <div v-if="game" class="bg-white shadow-lg rounded-2xl p-6 flex flex-col items-center gap-4">
      <!-- Auction and contract -->
      <div class="flex flex-wrap gap-8 items-center justify-center">
        <table class="text-sm font-mono">
          <thead>
            <tr>
              <th v-for="seat in seats" :key="seat" class="px-3"
                  :class="{ 'text-blue-700': seat === 'S' }">{{ seat }}</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="(row, i) in auctionRows" :key="i">
              <td v-for="(call, j) in row" :key="j" class="px-3 text-center"
                  :style="{ color: callColor(call) }">{{ call }}</td>
            </tr>
          </tbody>
        </table>
        <div class="text-center">
          <div class="text-xl"><b>{{ contractStr(contract) }}</b></div>
          <div class="text-sm text-gray-600">
            You are {{ game['ROLE'] === 'declarer' ? 'declarer (S) with dummy N' : 'defending from S' }}
          </div>
          <div class="text-sm mt-1">
            Declarer {{ game['TAKEN'][0] }} : {{ game['TAKEN'][1] }} defence
          </div>
          <div v-if="over" class="text-lg mt-1 font-semibold">{{ resultStr }}</div>
        </div>
      </div>

      <!-- Table: N on top, W and E at the sides, S at the bottom, the trick
           in the middle. Hidden hands show only how many cards they hold. -->
      <div class="grid grid-cols-3 gap-4 items-center justify-items-center">
        <template v-for="(seat, cell) in [null, 'N', null, 'W', 'trick', 'E', null, 'S', null]" :key="cell">
          <div v-if="seat === 'trick'" class="flex flex-col items-center gap-2">
            <!-- Current trick, leader underlined -->
            <div class="grid grid-cols-3 gap-1 w-36 h-28 items-center justify-items-center">
              <template v-for="(s, c) in [null, 'N', null, 'W', null, 'E', null, 'S', null]" :key="c">
                <span v-if="s && trickBySeat(trick['CARDS'], trick['LEADER'])[s]"
                      class="w-9 rounded py-0.5 text-center font-mono text-sm text-white"
                      :class="{ 'underline font-bold': s === trick['LEADER'] }"
                      :style="{ background: suitColors[trickBySeat(trick['CARDS'], trick['LEADER'])[s][0]] }">
                  {{ trickBySeat(trick['CARDS'], trick['LEADER'])[s] }}
                </span>
                <span v-else></span>
              </template>
            </div>
            <!-- Previous trick, winner ringed -->
            <div v-if="previousTrick" class="flex flex-col items-center text-xs text-gray-600">
              Previous trick
              <div class="flex gap-1 mt-1">
                <span v-for="(card, s) in trickBySeat(previousTrick['CARDS'], previousTrick['LEADER'])"
                      :key="s" class="flex items-center gap-0.5">
                  <b>{{ s }}</b>
                  <span class="w-8 rounded py-0.5 text-center font-mono text-white"
                        :class="{ 'ring-2 ring-offset-1 ring-black': s === previousTrick['WINNER'] }"
                        :style="{ background: suitColors[card[0]] }">{{ card }}</span>
                </span>
              </div>
            </div>
          </div>

          <div v-else-if="seat" class="font-mono text-sm leading-tight min-w-[8rem]">
            <div class="font-bold"
                 :class="{ 'text-blue-700': seat === declarer, 'underline': seat === game['NEXT'] }">
              {{ seat }}
              <span class="font-normal text-gray-500">
                {{ seat === declarer ? 'declarer' : seat === dummy ? 'dummy' : '' }}
              </span>
            </div>
            <template v-if="handBySuit(seat)">
              <div v-for="suit in suits" :key="suit"
                   class="whitespace-nowrap rounded px-1 mt-0.5 text-white"
                   :style="{ background: suitColors[suit] }">
                {{ suit }}
                <template v-for="card in handBySuit(seat)[suit]" :key="card">
                  <button v-if="playable(seat, card)"
                          class="rounded px-0.5 font-bold hover:bg-white hover:text-black"
                          @click="playCard(card)">{{ card.slice(1) }}</button>
                  <span v-else class="px-0.5"
                        :class="{ 'opacity-60': game['NEXT'] === seat && game['MINE'].includes(seat) }">{{ card.slice(1) }}</span>
                </template>
                <span v-if="handBySuit(seat)[suit].length == 0">—</span>
              </div>
            </template>
            <div v-else class="text-gray-500 mt-1">{{ game['COUNTS'][seat] }} cards</div>
          </div>

          <div v-else></div>
        </template>
      </div>

      <p v-if="!over && game['MINE'].includes(game['NEXT'])" class="text-sm text-gray-600">
        Your turn: play a card from <b>{{ game['NEXT'] }}</b>.
      </p>
    </div>

    <!-- After the game: its play, the computer's and the hint search's, each
         editable like simulations -->
    <div v-if="over && sim" class="bg-white shadow-lg rounded-2xl p-6 w-full max-w-5xl">
      <div class="text-center text-sm mb-2">
        Saved as simulation <a :href="`./?sim=${sim.id}`" class="text-blue-700 underline">{{ sim.id }}</a>
      </div>
      <div class="flex flex-col gap-2">
        <PlayAxis v-for="(axis, axisNo) in sim.result['POWER']" :key="`${sim.id}-${axisNo}`"
                  :axis="axis" :axis-no="axisNo" :sim-id="sim.id" :deal="simDeal" :open="axisNo === 0"
                  @result="(result) => sim.result = result" />
        <p v-if="sim.result['PENDING']" class="text-center text-sm text-gray-600">
          Hint search is running…
        </p>
      </div>
    </div>
  </div>
</template>
