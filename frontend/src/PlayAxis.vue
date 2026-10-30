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

<!-- One contract (axis) of a saved simulation: its result, a click on which
     shows or hides its play by sequences, and the play editor. Edits are
     sent as `result` with the simulation's new RESULT. -->
<script setup>
import { ref, computed, watch } from 'vue'
import { suits, suitColors, declarerSeat, cardsLeft, playDone, tricksPlayed, nextLeader,
         trickBySeat, playSequences, postJson } from './play.js'

const props = defineProps({
    axis: Object,
    axisNo: Number,
    // Id of the saved simulation, which edits apply to; none, no editor
    simId: String,
    // Whole deal by seat (see formulasDeal)
    deal: Object,
    open: { type: Boolean, default: false }
});
const emit = defineEmits(['result']);

const isOpen = ref(props.open);

const done = computed(() => playDone(props.deal, props.axis));
const left = computed(() => cardsLeft(props.deal, props.axis['SEQUENCES']));

// Tricks offered when the card clicked doesn't take the next trick in a
// single way: { card, takes, leader, tricks: [{ TRICK, WINNER }...] }; or
// the variants of a plan: { plan, label, leader,
// tricks: [{ PLAN, LABEL, TRICKS: [{ TRICK, WINNER, LEADER }...] }...] }
const choices = ref(null);

function simEditRequest(action, extra = {}) {
    return fetch("api/simedit", {
                    method: "POST",
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                    },
                    body: JSON.stringify({ id: props.simId, axis: props.axisNo, action: action, ...extra })
                });
}

// Plans the side on lead can apply while the play isn't over:
// [{ PLAN, LABEL, TRICKS, VARIANTS }...], TRICKS being how many the plan plays
// (in its longest variant), VARIANTS how many ways it can be played
const hints = ref([]);

async function loadHints() {
    hints.value = [];
    if(!props.simId || done.value) return;
    const response = await simEditRequest('hints');
    if(response.ok) hints.value = (await response.json())["HINTS"];
}

watch(() => [props.simId, props.axis], loadHints, { immediate: true });

// Plan as the backend sends it ({ TYPE, SUIT, SEAT }) to one for a request
function planRequest(plan) {
    return { type: plan["TYPE"], suit: plan["SUIT"], seat: plan["SEAT"] };
}

function applyHint(hint) {
    editPlay('plan', { plan: planRequest(hint["PLAN"]) });
}

// Applies an edit (pop, clear, split, play, trick or plan, see sim-edit in
// rest.cl) to play of the contract
async function editPlay(action, extra = {}) {
    const body = await postJson("api/simedit",
                                { id: props.simId, axis: props.axisNo, action: action, ...extra },
                                "Cannot edit play");
    if(!body) return;
    if(body["CHOICES"]) {
        choices.value = {
            card: extra.card,
            takes: body["TAKES"],
            leader: body["LEADER"],
            plan: extra.plan,
            label: body["LABEL"],
            tricks: body["CHOICES"]
        };
    } else {
        choices.value = null;
        emit('result', body["RESULT"]);
    }
}
</script>

<template>
  <div>
    <!-- Contract with its result; a click shows or hides its play -->
    <button class="w-full text-center text-lg rounded-xl px-3 py-1 bg-gray-100 hover:bg-gray-200"
            @click="isOpen = !isOpen">
      <span class="inline-block w-4 text-gray-500">{{ isOpen ? '▾' : '▸' }}</span>
      <template v-if="axis['LABEL']">{{ axis['LABEL'] }}: </template>
      <b>{{ axis['TRUMP'] }}</b> by <b>{{ declarerSeat(axis) }}</b>:
      <template v-if="done">{{ axis['SCORE'] }} tricks</template>
      <template v-else>
        {{ axis['SCORE'] }} tricks to declarer of {{ tricksPlayed(axis) }} played
      </template>
    </button>

    <template v-if="isOpen">

    <!-- Play split into short sequences of tricks. Each one shows cards
         held before it, then table view of the sequence: one row of
         played cards per hand, N on top, W and E in the middle, S at the
         bottom. Column tells the trick, colour the suit, underlined
         card led the trick. Comment on the sequence goes below. -->
    <div v-for="(seq, k) in playSequences(deal, axis)" :key="k" class="mt-4 border-t pt-4">
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
      <template v-if="!done">
        <p class="text-sm text-gray-600">
          Click a card to take the next trick; <b>{{ nextLeader(axis) }}</b> leads.
          <template v-if="axis['SEQUENCES'].length > 0
                          && axis['SEQUENCES'].at(-1)['TRICKS'].length == 0">
            Tricks go to a new sequence.
          </template>
        </p>

        <!-- Plans of the side on lead; a click plays all their tricks -->
        <div v-if="hints.length > 0"
             class="flex flex-wrap justify-center gap-2">
          <button v-for="hint in hints" :key="hint['LABEL']"
                  class="px-3 py-1 rounded bg-blue-100 hover:bg-blue-200 text-sm"
                  :title="hint['VARIANTS'] > 1
                          ? `Up to ${hint['TRICKS']} trick(s), ${hint['VARIANTS']} variants to choose from`
                          : `Plays ${hint['TRICKS']} trick(s)`"
                  @click="applyHint(hint)">
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
                <button v-for="rank in left[seat][suit]"
                        :key="rank"
                        class="rounded px-0.5 hover:bg-white hover:text-black"
                        @click="editPlay('play', { card: suit + rank })">{{ rank }}</button>
                <span v-if="left[seat][suit].length == 0">—</span>
              </div>
            </div>
            <div v-else></div>
          </template>
        </div>

        <!-- Tricks to choose from; cards by seat, winner ringed -->
        <div v-if="choices"
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
                          ? editPlay('trick', { tricks: choice['TRICKS'].map(t => t['TRICK']),
                                                plan: planRequest(choice['PLAN']) })
                          : editPlay('trick', { trick: choice['TRICK'], plan: choices.plan })">
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
                @click="editPlay('pop')">Remove last trick</button>
        <button class="px-3 py-1 rounded bg-gray-200 hover:bg-gray-300 disabled:opacity-50"
                :disabled="tricksPlayed(axis) == 0"
                @click="editPlay('clear')">Remove all tricks</button>
        <button class="px-3 py-1 rounded bg-gray-200 hover:bg-gray-300 disabled:opacity-50"
                :disabled="done || axis['SEQUENCES'].length == 0
                           || axis['SEQUENCES'].at(-1)['TRICKS'].length == 0"
                @click="editPlay('split')">New sequence</button>
      </div>
    </div>
    </template>
  </div>
</template>
