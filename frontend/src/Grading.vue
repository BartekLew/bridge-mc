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

<!-- Graded bidding debugger (grading.cl): a hand's opening by the legacy
     rules and the grade of each graded opening, with its rule. A hand given
     by ranges deals ten hands for them; a click on one shows it. -->
<script setup>
import { ref, computed } from 'vue'
import HandEditor from './HandEditor.vue'
import { suits, suitColors, formulaSuits, postJson } from './play.js'
import { Hands, Selection } from './hands.js'

const suitLetters = { '♠': 'S', '♥': 'H', '♦': 'D', '♣': 'C' };
const hands = ref(new Hands());
const busy = ref(false);

// The hands dealt for ranges, as grade-view in rest.cl gives them, and the
// one shown: a single hand given, or else the one selected in dealt
const dealt = ref([]);
const selected = ref(null);
const single = ref(null);
const view = computed(() => single.value ?? dealt.value[selected.value]);

function select(i) {
    selected.value = i;
    single.value = null;
}

// Rules live in backend/grade-bidding.cfg. A rule given, {bid, rule}, is
// saved there before grading.
async function post(request, rule) {
    return await postJson("api/grade", rule ? { ...request, rule } : request, "Cannot grade");
}

// Hands dealt for ranges: count of them, asked for in batches so that they
// appear as they come, added to the ones dealt before. progress is
// {done, total} while dealing; stopping ends it after the current batch.
const count = ref(100);
const progress = ref(null);
const stopping = ref(false);
const batch = 10;

async function deal(def) {
    const total = Math.max(1, Math.floor(count.value) || 0);
    progress.value = { done: 0, total };
    stopping.value = false;
    try {
        while(progress.value.done < total && !stopping.value) {
            const body = await post({ def, count: Math.min(batch, total - progress.value.done) });
            if(!body) break;
            dealt.value.push(...body["HANDS"]);
            progress.value.done += body["HANDS"].length;
        }
    } finally {
        progress.value = null;
    }
}

async function apply(selection) {
    const request = selection.restDef();
    busy.value = true;
    try {
        if(request.def) {
            single.value = null;
            await deal(request.def);
        } else {
            const body = await post(request);
            if(body) single.value = body;
        }
    } finally {
        busy.value = false;
    }
}

const differing = computed(() => dealt.value.filter((h) => h['LEGACY'] !== h['BEST']).length);

function clearDealt() {
    dealt.value = [];
    selected.value = null;
}

// The hands shown graded again, after saving a rule or when the file was
// edited by hand; false when the rule or a request failed
async function regrade(rule) {
    busy.value = true;
    try {
        if(rule) {
            const body = await post({}, rule);
            if(!body) return false;
        }
        if(dealt.value.length) {
            const body = await post({ hands: dealt.value.map((h) => h['HAND']) });
            if(!body) return false;
            dealt.value = body["HANDS"];
        }
        if(single.value) {
            const body = await post({ hand: single.value['HAND'] });
            if(!body) return false;
            single.value = body;
        }
        return true;
    } finally {
        busy.value = false;
    }
}

// Editing a rule: a click on its trace opens its text from the file;
// applying saves it and grades the hands again (a bad rule is reported, not
// saved, and stays open)
const editing = ref(null);
const draft = ref("");

function edit(g) {
    editing.value = g['BID'];
    draft.value = g['RULE'];
}

async function applyEdit() {
    if(await regrade({ bid: editing.value, rule: draft.value })) editing.value = null;
}

function editKey(event) {
    if(event.key === 'Escape') editing.value = null;
    else if(event.key === 'Enter' && (event.ctrlKey || event.metaKey)) applyEdit();
}

// A trace split into text and values (")=0.5"), so values stand out
function traceParts(trace) {
    return trace.split(/(\)=-?\d+(?:\.\d+)?)/).map((text, i) =>
        i % 2 ? { text: ')', value: text.slice(2) } : { text });
}

function valueClass(value) {
    const v = parseFloat(value);
    return v <= 0 ? 'text-red-600' : v >= 1 ? 'text-green-700' : 'text-amber-600';
}

function callColor(call) {
    const suit = suits.find((s) => call.includes(s));
    return suit ? suitColors[suit] : undefined;
}

// Grades, the highest first (on a tie, the bid listed first wins)
const grades = computed(() => view.value ? [...view.value["GRADES"]].sort((a, b) => b["GRADE"] - a["GRADE"]) : []);

const vFocus = { mounted: (el) => el.focus() };

// Measuring the bids: a job on the server grades count random hands; its
// statistics (gjob-stats in rest.cl) are asked for while it runs. A click on
// a bid lists its hands, those for which filter (a grade expression) grades 1.
const measureCount = ref(10000);
const job = ref(null);
const stats = ref(null);
const measuring = computed(() => stats.value?.['STATE'] === 'running');
const ALL = '*';
const bidSel = ref(null);
const filter = ref("");
const bidHands = ref(null);

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

async function startMeasure() {
    const body = await postJson("api/grade/measure", { count: measureCount.value }, "Cannot measure");
    if(!body) return;
    job.value = body['ID'];
    stats.value = { STATE: 'running', DONE: 0, TOTAL: measureCount.value, BIDS: [] };
    bidHands.value = null;
    poll(body['ID']);
}

async function poll(id) {
    while(job.value === id) {
        const body = await postJson(`api/grade/measure/${id}`, {}, "Cannot get statistics");
        if(!body || job.value !== id) return;
        stats.value = body;
        if(body['STATE'] !== 'running') {
            if(bidSel.value) loadHands();
            return;
        }
        await sleep(300);
    }
}

function stopMeasure() {
    postJson(`api/grade/measure/${job.value}/stop`, {}, "Cannot stop");
}

function selectBid(bid) {
    bidSel.value = bidSel.value === bid ? null : bid;
    bidHands.value = null;
    if(bidSel.value) loadHands();
}

async function loadHands() {
    const body = await postJson(`api/grade/measure/${job.value}/hands`,
                                { bid: bidSel.value === ALL ? null : bidSel.value, filter: filter.value },
                                "Cannot list hands");
    if(body) bidHands.value = body;
}

// A hand of the list opened in the debugger in a new tab, so the measuring
// stays: grading.html?hand=<formula> puts it in the editor and grades it
function openHand(h) {
    const url = new URL(window.location.href);
    url.search = "?hand=" + encodeURIComponent(h['HAND']);
    window.open(url, "_blank");
}

const handParam = new URLSearchParams(window.location.search).get("hand");
if(handParam) {
    const opened = new Hands();
    opened.data = [Selection.fromFormula("N", handParam)];
    hands.value = opened;
    apply(opened.edited());
}

const measureOf = (row, name) => row['MEASURES'].find((m) => m['NAME'] === name);
const tableMeasures = ['hcp', 'losers', 'S', 'H', 'D', 'C'];
const selectedStats = computed(() => stats.value?.['BIDS'].find((b) => b['BID'] === bidSel.value));

// A measure's distribution as bars, a value for each from its least to its
// greatest
function bars(m) {
    const probs = new Map(m['PROBS'].map(([v, p]) => [v, p]));
    const top = Math.max(...probs.values());
    const out = [];
    for(let v = m['MIN']; v <= m['MAX']; v++) {
        const p = probs.get(v) ?? 0;
        out.push({ v, p, height: Math.round(40 * p / top) });
    }
    return out;
}
</script>

<template>
  <div class="min-h-screen flex flex-col items-center bg-gray-100 text-gray-800 py-6 gap-4">
    <div class="flex items-center gap-4">
      <h1 class="text-2xl font-bold">Graded bidding</h1>
      <a href="./" class="text-sm text-blue-700 underline">Simulator</a>
      <button @click="regrade()" :disabled="busy" title="Grade again by backend/grade-bidding.cfg as it is now"
              class="text-sm text-blue-700 underline disabled:opacity-50">Reload rules</button>
    </div>

    <div class="flex flex-wrap gap-4 items-start justify-center">
      <div class="flex flex-col gap-2">
        <HandEditor :hands="hands" apply="Apply" :disabled="busy" @apply="apply" />
        <!-- Dealing for ranges: how many, and progress -->
        <div class="bg-white shadow-lg rounded-2xl p-4 text-sm flex flex-col gap-2">
          <label class="flex items-center gap-2">
            Hands to deal for ranges
            <input v-model.number="count" type="number" min="1" step="10" :disabled="busy"
                   class="w-24 border rounded px-1 disabled:bg-gray-100" />
          </label>
          <div v-if="progress" class="flex flex-col gap-1">
            <div class="flex items-center gap-2 text-blue-700 font-semibold">
              <span class="inline-block w-4 h-4 border-2 border-blue-600 border-t-transparent rounded-full animate-spin"></span>
              Dealing {{ progress.done }} / {{ progress.total }}…
              <button @click="stopping = true" :disabled="stopping"
                      class="ml-auto text-xs text-blue-700 underline disabled:opacity-50">
                {{ stopping ? 'stopping…' : 'stop' }}</button>
            </div>
            <div class="h-2 bg-gray-200 rounded">
              <div class="h-2 bg-blue-600 rounded transition-all"
                   :style="{ width: (100 * progress.done / progress.total) + '%' }"></div>
            </div>
          </div>
        </div>
      </div>

      <div class="flex flex-col gap-4">
        <!-- Hands dealt for ranges -->
        <div v-if="dealt.length" class="bg-white shadow-lg rounded-2xl p-4 text-sm">
          <div class="flex items-center gap-4 mb-2">
            <h3 class="font-bold">Hands dealt for the ranges: {{ dealt.length }}</h3>
            <span class="text-gray-500">{{ differing }} open otherwise than legacy</span>
            <button @click="clearDealt" :disabled="busy"
                    class="ml-auto text-xs text-blue-700 underline disabled:opacity-50">clear</button>
          </div>
          <ul class="max-h-96 overflow-y-auto">
            <li v-for="(hand, i) in dealt" :key="i">
              <button @click="select(i)"
                      class="w-full text-left font-mono rounded px-2 py-0.5 hover:bg-blue-100"
                      :class="{ 'bg-blue-200': !single && selected === i,
                                'border-l-4 border-amber-400': hand['LEGACY'] !== hand['BEST'] }">
                {{ hand['HAND'].slice(3) }}
                <span class="text-gray-500 ml-2">legacy</span>
                <span :style="{ color: callColor(hand['LEGACY']) }">{{ hand['LEGACY'] }}</span>
                <span class="text-gray-500 ml-2">graded</span>
                <span :style="{ color: callColor(hand['BEST']) }">{{ hand['BEST'] }}</span>
              </button>
            </li>
          </ul>
        </div>

        <!-- The hand: measures, legacy opening, grades -->
        <div v-if="view" class="bg-white shadow-lg rounded-2xl p-4 max-w-3xl">
          <div class="flex flex-wrap gap-6 items-start">
            <div class="font-mono text-sm leading-tight min-w-[8rem]">
              <div v-for="suit in suits" :key="suit"
                   class="whitespace-nowrap rounded px-1 mt-0.5 text-white"
                   :style="{ background: suitColors[suit] }">
                {{ suit }} {{ formulaSuits(view['HAND'])[suit].join('') || '—' }}
              </div>
            </div>
            <table class="text-sm">
              <tbody>
                <tr><td class="pr-2 text-gray-500">HCP</td><td>{{ view['MEASURES']['HCP'] }}</td>
                    <td class="pl-4 pr-2 text-gray-500">losers</td><td>{{ view['MEASURES']['LOSERS'] }}</td>
                    <td class="pl-4 pr-2 text-gray-500">balanced</td><td>{{ view['MEASURES']['BALANCED'] }}</td></tr>
                <tr><td class="pr-2 text-gray-500">long</td><td>{{ view['MEASURES']['LONG'] }}</td>
                    <td class="pl-4 pr-2 text-gray-500">second</td><td>{{ view['MEASURES']['SECOND'] }}</td>
                    <td class="pl-4 pr-2 text-gray-500">longpower</td><td>{{ view['MEASURES']['LONGPOWER'] }}</td></tr>
                <tr v-for="suit in suits" :key="suit">
                  <td class="pr-2" :style="{ color: suitColors[suit] }">{{ suit }}</td>
                  <td>{{ view['MEASURES'][suitLetters[suit]] }} cards</td>
                  <td class="pl-4 pr-2 text-gray-500">power</td>
                  <td>{{ view['MEASURES'][suitLetters[suit] + 'POWER'] }}</td>
                  <td class="pl-4 pr-2 text-gray-500">stop</td>
                  <td>{{ view['MEASURES'][suitLetters[suit] + 'STOP'] }}</td>
                </tr>
              </tbody>
            </table>
            <div class="text-lg">
              <div>Legacy: <b :style="{ color: callColor(view['LEGACY']) }">{{ view['LEGACY'] }}</b></div>
              <div>Graded: <b :style="{ color: callColor(view['BEST']) }">{{ view['BEST'] }}</b></div>
            </div>
          </div>

          <table class="mt-4 text-sm w-full">
            <thead>
              <tr class="text-left text-gray-500">
                <th class="pr-4">Bid</th><th class="pr-4">Grade</th><th>Rule</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="g in grades" :key="g['BID']" class="border-t align-top"
                  :class="{ 'bg-green-100': g['BID'] === view['BEST'] }">
                <td class="pr-4 py-1 font-bold" :style="{ color: callColor(g['BID']) }">{{ g['BID'] }}</td>
                <td class="pr-4 py-1 font-mono">{{ g['GRADE'].toFixed(2) }}</td>
                <td class="py-1">
                  <div v-if="editing === g['BID']" class="flex flex-col gap-1">
                    <textarea v-model="draft" @keydown="editKey" rows="8" spellcheck="false"
                              class="w-full font-mono text-xs border rounded p-1"
                              v-focus></textarea>
                    <div class="flex gap-2 text-xs">
                      <button @click="applyEdit" :disabled="busy"
                              class="px-2 py-0.5 rounded bg-blue-600 text-white disabled:opacity-50">Apply</button>
                      <button @click="editing = null" class="px-2 py-0.5 rounded bg-gray-200">Cancel</button>
                      <span class="text-gray-500 self-center">Ctrl+Enter applies, Esc cancels</span>
                    </div>
                  </div>
                  <pre v-else @click="edit(g)" title="Click to edit (saved to grade-bidding.cfg)"
                       class="text-xs whitespace-pre cursor-text rounded px-1 hover:bg-blue-50"
                  ><template v-for="(p, i) in traceParts(g['TRACE'])" :key="i">{{ p.text }}<b v-if="p.value" :class="valueClass(p.value)">={{ p.value }}</b></template></pre>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>

    <!-- Measuring the bids over random hands -->
    <div class="bg-white shadow-lg rounded-2xl p-4 w-full max-w-6xl text-sm flex flex-col gap-3">
      <div class="flex flex-wrap items-center gap-3">
        <h2 class="text-lg font-bold">Measure bids</h2>
        <label class="flex items-center gap-2">
          Random hands
          <input v-model.number="measureCount" type="number" min="100" step="1000" :disabled="measuring"
                 class="w-28 border rounded px-1 disabled:bg-gray-100" />
        </label>
        <button @click="startMeasure" :disabled="measuring"
                class="px-3 py-1 rounded bg-blue-600 text-white font-semibold disabled:opacity-50">Run</button>
        <template v-if="stats">
          <span v-if="measuring" class="flex items-center gap-2 text-blue-700 font-semibold">
            <span class="inline-block w-4 h-4 border-2 border-blue-600 border-t-transparent rounded-full animate-spin"></span>
            Grading {{ stats['DONE'] }} / {{ stats['TOTAL'] }}…
            <button @click="stopMeasure" class="text-xs underline">stop</button>
          </span>
          <span v-else class="text-gray-600">{{ stats['DONE'] }} hands, {{ stats['STATE'] }}</span>
          <span v-if="stats['ERROR']" class="text-red-600">Error: {{ stats['ERROR'] }}</span>
        </template>
        <span class="text-gray-500 text-xs">Rules as grade-bidding.cfg was when the run started</span>
      </div>
      <div v-if="measuring" class="h-2 bg-gray-200 rounded">
        <div class="h-2 bg-blue-600 rounded transition-all"
             :style="{ width: (100 * stats['DONE'] / stats['TOTAL']) + '%' }"></div>
      </div>

      <table v-if="stats && stats['BIDS'].length" class="w-full">
        <thead>
          <tr class="text-left text-gray-500">
            <th class="pr-3">Bid</th><th class="pr-3 text-right">Hands</th><th class="pr-3 text-right">Share</th>
            <th v-for="name in tableMeasures" :key="name" class="pr-3"
                :style="{ color: suitColors[suits.find((s) => suitLetters[s] === name)] }">
              {{ suits.find((s) => suitLetters[s] === name) || name }}</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="row in stats['BIDS']" :key="row['BID']" @click="selectBid(row['BID'])"
              class="border-t cursor-pointer hover:bg-blue-50"
              :class="{ 'bg-blue-100': bidSel === row['BID'] }">
            <td class="pr-3 py-0.5 font-bold" :style="{ color: callColor(row['BID']) }">{{ row['BID'] }}</td>
            <td class="pr-3 text-right font-mono">{{ row['COUNT'] }}</td>
            <td class="pr-3 text-right font-mono">{{ (100 * row['SHARE']).toFixed(1) }}%</td>
            <td v-for="name in tableMeasures" :key="name" class="pr-3 font-mono whitespace-nowrap">
              {{ measureOf(row, name)['MIN'] }}–{{ measureOf(row, name)['MAX'] }}
              <span class="text-gray-500">({{ measureOf(row, name)['MEAN'].toFixed(1) }})</span>
            </td>
          </tr>
          <tr @click="selectBid(ALL)" class="border-t cursor-pointer hover:bg-blue-50 text-gray-600"
              :class="{ 'bg-blue-100': bidSel === ALL }">
            <td class="pr-3 py-0.5 italic" colspan="3">All hands (for filtering)</td>
            <td :colspan="tableMeasures.length"></td>
          </tr>
        </tbody>
      </table>

      <!-- The bid selected: its distributions, and its hands by a filter -->
      <div v-if="bidSel" class="border-t pt-3 flex flex-col gap-3">
        <div v-if="selectedStats" class="flex flex-wrap gap-4">
          <div v-for="m in selectedStats['MEASURES']" :key="m['NAME']" class="flex flex-col items-start">
            <div class="text-xs text-gray-600">{{ m['NAME'] }} <span class="text-gray-400">mean {{ m['MEAN'].toFixed(1) }}</span></div>
            <div class="flex items-end gap-px h-10">
              <div v-for="b in bars(m)" :key="b.v" class="w-1.5 bg-blue-500"
                   :style="{ height: b.height + 'px' }" :title="`${b.v}: ${(100 * b.p).toFixed(1)}%`"></div>
            </div>
            <div class="flex justify-between w-full text-[10px] text-gray-500 font-mono">
              <span>{{ m['MIN'] }}</span><span>{{ m['MAX'] }}</span>
            </div>
          </div>
        </div>

        <div class="flex flex-wrap items-center gap-2">
          <span class="font-semibold">Filter</span>
          <input v-model="filter" @keydown.enter="loadHands" spellcheck="false"
                 placeholder="(* (< hcp 12) (>= S 5)) — kept where it grades 1 or more"
                 class="flex-1 min-w-[20rem] font-mono text-xs border rounded px-2 py-1" />
          <button @click="loadHands" class="px-2 py-1 rounded bg-blue-600 text-white text-xs">Apply</button>
          <button v-if="filter" @click="filter = ''; loadHands()" class="text-xs text-blue-700 underline">clear</button>
        </div>

        <div v-if="bidHands">
          <div class="text-gray-600 mb-1">
            {{ bidHands['COUNT'] }} hands<span v-if="bidHands['COUNT'] > bidHands['HANDS'].length">,
            the first {{ bidHands['HANDS'].length }} shown</span>; a click opens one in the debugger in a new tab
          </div>
          <ul class="max-h-96 overflow-y-auto">
            <li v-for="(h, i) in bidHands['HANDS']" :key="i">
              <button @click="openHand(h)" title="Open in the debugger in a new tab"
                      class="w-full text-left font-mono rounded px-2 py-0.5 hover:bg-blue-100">
                {{ h['HAND'].slice(3) }}
                <span class="text-gray-500 ml-2">hcp {{ h['HCP'] }} losers {{ h['LOSERS'] }}</span>
                <span class="ml-2" :style="{ color: callColor(h['BEST']) }">{{ h['BEST'] }}</span>
              </button>
            </li>
          </ul>
        </div>
      </div>
    </div>
  </div>
</template>
