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
import { Hands } from './hands.js'

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
  </div>
</template>
