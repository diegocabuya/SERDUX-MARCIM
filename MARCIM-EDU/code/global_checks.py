"""Additional archival-data checks for MARCIM-EDU v1.2.

1. Global screen of the eight player levers: in every reported game background
   and in rounds 2-5 (12 backgrounds), 20 base points are drawn by a discrete
   Latin hypercube over the eight levers (10 token levels); at each point, each
   lever is moved from 0.1 to 1.0 with the others held (radial one-at-a-time
   design). The effect on active services at round close is summarized per lever.
   Screening, not variance-based (Sobol) indices; it says nothing about realism
   or pedagogical validity. Runs in which the transcribed integer reconciliation
   does not conserve the 350 nodes are counted and retained, not discarded.
2. Descriptive contrast of the 12 participant totals: exact permutation
   distribution of the mean difference over all C(12,4)=495 allocations, and
   Hedges' g with its large-sample interval. Allocation was NOT random and the
   four players shared one session, so these quantify separation within this
   dataset, not the precision of a treatment effect.
Run after reproduce.py; the historical engine and scored responses are unchanged.
"""
import argparse, csv, itertools, json, math
from pathlib import Path
import numpy as np
from scipy.stats import norm
import engine as e

SEED, SAMPLES = 20261109, 20
GAMES = ['pesimista', 'neutral', 'optimista']
LEVELS = np.round(np.arange(1, 11) / 10, 1)


def save(path, rows):
    with path.open('w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)


def lhs(rng):
    # Each lever takes every token level SAMPLES/10 times, in an independent random order.
    return {k: rng.permutation(np.repeat(LEVELS, SAMPLES // len(LEVELS))) for k in e.PALANCAS}


def screen(out):
    """Full-range effect of each lever (1.0 minus 0.1, others held) at Latin-hypercube base points."""
    rng = np.random.default_rng(SEED)
    rows = []
    for game in GAMES:
        ys, _ = e.jugar(game)
        for r in [1, 2, 3, 4]:  # rounds 2-5, each from that game's replayed state
            design = lhs(rng)
            for i in range(SAMPLES):
                base = {k: float(design[k][i]) for k in e.PALANCAS}
                for k in e.PALANCAS:
                    res, nodes = [], []
                    for v in (0.1, 1.0):
                        p = dict(e.CONFIG[game]['rounds'][r]); p.update(base); p[k] = v
                        y = e.ronda(ys[r - 1], p)[-1]
                        assert min(y) >= 0
                        res.append(e.activos(y)); nodes.append(sum(y))
                    rows.append({'game': game, 'round': r + 1, 'point': i, 'lever': k,
                                 **{'base_' + q: base[q] for q in e.PALANCAS},
                                 'active_at_0.1': res[0], 'active_at_1.0': res[1], 'effect_pp': res[1] - res[0],
                                 'nodes_at_0.1': nodes[0], 'nodes_at_1.0': nodes[1]})
    save(out / 'global-screen-effects.csv', rows)
    return rows, summarize(rows)


def summarize(rows):
    """Per lever: overall magnitude, and sign by regime (rounds 2-3 no destruction; 4-5 destruction)."""
    summary = []
    for k in e.PALANCAS:
        d = np.array([float(x['effect_pp']) for x in rows if x['lever'] == k])
        rnd = np.array([int(x['round']) for x in rows if x['lever'] == k])
        early, late = d[rnd <= 3], d[rnd >= 4]
        summary.append({'lever': k, 'points': len(d), 'median_effect_pp': float(np.median(d)),
                        'max_abs_effect_pp': float(np.abs(d).max()), 'share_abs_below_1pp': float((np.abs(d) < 1).mean()),
                        'share_negative_rounds_2_3': float((early < -1e-9).mean()),
                        'share_negative_rounds_4_5': float((late < -1e-9).mean())})
    return summary


def contrast():
    with (e.BASE / 'data/cycle2-scored-matrix.csv').open() as f:
        data = list(csv.DictReader(f))
    tot = np.array([int(r['total']) for r in data])
    grp = np.array([r['grupo'] for r in data])
    c, i = tot[grp == 'Ctrl'], tot[grp == 'Intv']
    obs = i.mean() - c.mean()
    diffs = []
    for idx in itertools.combinations(range(12), 4):
        mask = np.zeros(12, bool)
        mask[list(idx)] = True
        diffs.append(tot[mask].mean() - tot[~mask].mean())
    diffs = np.array(diffs)
    p_one = float((diffs >= obs - 1e-12).mean())
    n1, n2 = len(i), len(c)
    sp = math.sqrt(((n1 - 1) * i.var(ddof=1) + (n2 - 1) * c.var(ddof=1)) / (n1 + n2 - 2))
    d = obs / sp
    g = d * (1 - 3 / (4 * (n1 + n2) - 9))
    se = math.sqrt((n1 + n2) / (n1 * n2) + g ** 2 / (2 * (n1 + n2)))
    z = norm.ppf(0.975)
    return {'mean_difference_points': float(obs), 'allocations': len(diffs), 'allocations_at_least_observed': int((diffs >= obs - 1e-12).sum()),
            'exact_permutation_p_one_sided': p_one, 'pooled_sd_points': sp, 'cohen_d': d, 'hedges_g': g,
            'hedges_g_ci95_low': g - z * se, 'hedges_g_ci95_high': g + z * se,
            'interpretation': 'Descriptive separation within these 12 records. Allocation was not random and the four players shared one session; not a treatment-effect estimate or its precision.'}


def run(out):
    out.mkdir(parents=True, exist_ok=True)
    rows, summary = screen(out)
    save(out / 'global-screen-summary.csv', summary)
    lost = sum((x['nodes_at_0.1'] != 350) + (x['nodes_at_1.0'] != 350) for x in rows)
    res = {'version': '1.2', 'global_screen_runs': 2 * len(rows), 'base_points': len(rows) // 8,
           'runs_not_conserving_350_nodes': int(lost),
           'levers': summary, 'contrast': contrast(),
           'interpretation': 'Radial full-range effects at Latin-hypercube base points in 12 backgrounds (3 games x rounds 2-5); screening, not Sobol indices, not realism.'}
    (out / 'global-checks.json').write_text(json.dumps(res, indent=2))
    ref = e.BASE / 'reference/global-checks.json'
    if ref.exists() and ref.resolve() != (out / 'global-checks.json').resolve():
        assert json.loads(ref.read_text()) == json.loads(json.dumps(res)), 'global checks differ from reference'
        print('global checks match reference/global-checks.json')
    print(json.dumps(res, indent=2))
    return res


if __name__ == '__main__':
    a = argparse.ArgumentParser()
    a.add_argument('--out', type=Path, default=e.BASE / 'results')
    run(a.parse_args().out)
