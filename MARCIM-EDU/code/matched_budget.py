"""Resource-matched replay of the optimistic game (MARCIM-EDU v1.3).

The recorded optimistic game was issued 47 tokens instead of the ordinary 40:
round 3 delivered 15 tokens instead of 8 (data/token-ledger.csv). This script
removes the seven surplus tokens in every admissible way -- any combination of
seven of the fifteen tokens issued in round 3, lever by lever -- carries the
lower lever values through rounds 3 to 5, and replays the game from the recorded
round-2 state. Card-granted tokens and every other decision are unchanged.

The result is the range of outcomes the optimistic profile would have produced
with the ordinary 40-token budget. It is a computation on the archived artifact,
not a new participant session, and says nothing about realism.
Run after reproduce.py.
"""
import argparse, csv, itertools, json
from pathlib import Path
import numpy as np
import engine as e

GAME, ROUND = 'optimista', 3  # the round that received the surplus
SURPLUS = 7


def issued_in_round():
    with (e.BASE / 'data/token-ledger.csv').open() as f:
        rows = [r for r in csv.DictReader(f) if r['escenario'] == GAME and int(r['ronda']) == ROUND]
    issued = {r['palanca']: int(r['fichas_entregadas']) for r in rows}
    assert sum(issued.values()) == 8 + SURPLUS, issued
    return issued


def removals(issued):
    levers = e.PALANCAS
    for combo in itertools.product(*[range(issued[k] + 1) for k in levers]):
        if sum(combo) == SURPLUS:
            yield dict(zip(levers, combo))


def run(out):
    out.mkdir(parents=True, exist_ok=True)
    issued = issued_in_round()
    recorded, _ = e.jugar(GAME)
    neutral, _ = e.jugar('neutral')
    rows = []
    for rem in removals(issued):
        override = {}
        for r in range(ROUND - 1, 5):  # rounds 3, 4 and 5 (zero-based index)
            p = e.CONFIG[GAME]['rounds'][r]
            override[r] = {k: round(p[k] - 0.1 * rem[k], 1) for k in e.PALANCAS}
            assert all(v >= 0.1 - 1e-9 for v in override[r].values()), (rem, r, override[r])
        states, _ = e.jugar(GAME, override)
        rows.append({**{'removed_' + k: rem[k] for k in e.PALANCAS},
                     **{f'active_r{r + 1}': e.activos(states[r]) for r in range(5)},
                     'destroyed_r5': states[4][5], 'nodes_r5': sum(states[4])})
    with (out / 'matched-budget.csv').open('w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    a5 = np.array([r['active_r5'] for r in rows])
    x5 = np.array([r['destroyed_r5'] for r in rows])
    res = {'version': '1.3', 'game': GAME, 'surplus_round': ROUND, 'surplus_tokens': SURPLUS,
           'issued_in_round': issued, 'matched_variants': len(rows),
           'recorded_replay_active_r5': e.activos(recorded[4]), 'recorded_replay_destroyed_r5': recorded[4][5],
           'neutral_replay_active_r5': e.activos(neutral[4]), 'neutral_replay_destroyed_r5': neutral[4][5],
           'matched_active_r5_min': float(a5.min()), 'matched_active_r5_median': float(np.median(a5)),
           'matched_active_r5_max': float(a5.max()),
           'matched_destroyed_r5_min': int(x5.min()), 'matched_destroyed_r5_median': float(np.median(x5)),
           'matched_destroyed_r5_max': int(x5.max()),
           'share_above_neutral_replay': float((a5 > e.activos(neutral[4])).mean()),
           'share_at_or_above_recorded_replay': float((a5 >= e.activos(recorded[4]) - 1e-9).mean()),
           'runs_not_conserving_350_nodes': int(sum(r['nodes_r5'] != 350 for r in rows)),
           'interpretation': 'Outcomes of the optimistic profile with the ordinary 40-token budget, over every way of removing the seven surplus round-3 tokens. Conditional model responses, not realism.'}
    (out / 'matched-budget.json').write_text(json.dumps(res, indent=2))
    ref = e.BASE / 'reference/matched-budget.json'
    if ref.exists() and ref.resolve() != (out / 'matched-budget.json').resolve():
        assert json.loads(ref.read_text()) == json.loads(json.dumps(res)), 'matched-budget results differ from reference'
        print('matched-budget results match reference/matched-budget.json')
    print(json.dumps(res, indent=2))
    return res


if __name__ == '__main__':
    a = argparse.ArgumentParser()
    a.add_argument('--out', type=Path, default=e.BASE / 'results')
    run(a.parse_args().out)
