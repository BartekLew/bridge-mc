// Cards, seats and play of saved simulations, shared by the pages

export const suits = ['♠', '♥', '♦', '♣'];
export const ranks = ['A','K','Q','J','10','9','8','7','6','5','4','3','2'];
export const seats = ['N', 'E', 'S', 'W'];

// Background colour of each suit (with white text), both in played and remaining cards
export const suitColors = { '♠': '#1e3a8a', '♥': '#dc2626', '♦': '#ea580c', '♣': '#15803d' };

// Hand computed from remaining cards has no proper id; it's always the last one (W)
export function declarerSeat(axis) {
    return seats.includes(axis['DECLARER']) ? axis['DECLARER'] : 'W';
}

export function emptySeats() {
    return Object.fromEntries(seats.map((seat) => [seat, []]));
}

// Ranks by suit of a hand formula, e.g. "N: ♠ AK9 ♥ AJ83 ♦ A8 ♣ KJ96":
// { '♠': ['A', 'K', '9'], '♥': ... }
export function formulaSuits(str) {
    const ans = Object.fromEntries(suits.map((suit) => [suit, []]));
    const body = str.includes(": ") ? str.slice(str.indexOf(": ") + 2) : str;
    for(const [, suit, cards] of body.matchAll(/([♠♥♦♣])\s*([AKQJ0-9]*)/g)) {
        ans[suit] = cards.match(/10|[AKQJ2-9]/g) || [];
    }
    return ans;
}

// Whole deal by seat from four formulas N E S W: { N: { '♠': [...], ...}, E: ... }
export function formulasDeal(formulas) {
    return Object.fromEntries(seats.map((seat, i) => [seat, formulaSuits(formulas[i])]));
}

// Cards of deal not played in sequences
export function cardsLeft(deal, sequences) {
    const left = Object.fromEntries(seats.map((seat) => [
        seat,
        Object.fromEntries(suits.map((suit) => [suit, [...deal[seat][suit]]]))
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

export function playDone(deal, axis) {
    const left = cardsLeft(deal, axis['SEQUENCES']);
    return seats.every((seat) => suits.every((suit) => left[seat][suit].length == 0));
}

export function tricksPlayed(axis) {
    return axis['SEQUENCES'].reduce((sum, seq) => sum + seq['TRICKS'].length, 0);
}

// Seat to lead after all tricks of axis
export function nextLeader(axis) {
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
export function trickBySeat(trick, leader) {
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
export function playSequences(deal, axis) {
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
                     remaining: cardsLeft(deal, sequences.slice(0, k)),
                     comment: (sequences[k]['COMMENT'] || '').split('\n').filter(Boolean)
                 }))
                 .filter((seq, k) => sequences[k]['TRICKS'].length > 0);
}

// POST of body as JSON to url; the response's JSON, or null after telling
// what failed (what: "Cannot edit play" and such)
export async function postJson(url, body, what) {
    const response = await fetch(url, {
                                     method: "POST",
                                     headers: {
                                         'Accept': 'application/json',
                                         'Content-Type': 'application/json',
                                     },
                                     body: JSON.stringify(body)
                                 });
    if(!response.ok) {
        const text = await response.text();
        console.error("Fetch failed:", response.status, response.statusText, text);
        alert(`${what}: ${response.status}\n${text}`);
        return null;
    }
    return response.json();
}
