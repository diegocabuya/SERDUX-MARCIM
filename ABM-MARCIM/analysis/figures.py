#!/usr/bin/env python3
"""Figuras del manuscrito de IEEE Access.

    python3 Herramientas/figuras.py

1. **Figura 1** (arquitectura de la implementación): se dibuja en vectorial, `fig/fig_architecture.pdf`,
   reproduciendo el contenido del PNG enviado a JASSS; es un esquema y todo lo que dice está a la vista.
2. **Figuras 2–5**: se regeneran en vectorial desde los datos de BehaviorSpace de `Datos/`
   (`esc1.csv`: escenario 1, 30 réplicas × 36 pasos, N = 71; `esc2_nodos10.csv`: escenario 2, 10 réplicas ×
   121 pasos, N = 6200), recuperados el 2026-10-19 del proyecto de claude.ai donde se hicieron las originales.
   Antes de escribir las figuras, `validar()` recalcula **cada cifra que el texto del artículo afirma** sobre
   esos datos (determinismo agregado, divergencia, cambios de estado, tiempo por estado, tasa de resolución del
   algoritmo de asignación) y aborta si alguna no coincide: la figura y el texto salen de los mismos números.
"""
import os

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import FancyBboxPatch, Rectangle
from PIL import Image

matplotlib.rcParams['pdf.fonttype'] = 42
_AQUI = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# En el vault: <manuscrito>/ABM-MARCIM-IEEE-Access/fig y <manuscrito>/Datos. En el repositorio SERDUX-MARCIM
# (ABM-MARCIM/analysis/figures.py): ../figures y ../data. Las variables de entorno ABM_FIG y ABM_DATOS mandan.
FIG = os.environ.get('ABM_FIG') or next((d for d in (os.path.join(_AQUI, 'ABM-MARCIM-IEEE-Access', 'fig'),
                                                      os.path.join(_AQUI, 'figures')) if os.path.isdir(d)), os.path.join(_AQUI, 'figures'))
NETLOGO, PYTHON, PANEL, GREY = '#2F6FD3', '#E5632D', '#F3F3F3', '#555555'

COLUMNAS = [
    ('GLOBAL SETUP', [
        ('python-setup', 'start the Python interpreter and\nimport libraries', PYTHON),
        ('global variables', 'create and initialize the globals\nof the model', NETLOGO)]),
    ('SETUP', [
        ('rate-and-parameter-setup', 'capture the values entered by the\nexperimenter', NETLOGO),
        ('create agents and links', 'place agents in S, E, D, U and\nbuild the network', NETLOGO),
        ('cyber-risk-approach-calculation', 'compute the maritime cyber risk\nmodel', NETLOGO),
        ('python-state-update', 'transfer initial conditions and\nparameters', PYTHON)]),
    ('GO — one simulation step', [
        ('cyber-risk-approach-calculation', 'recompute the risk model for the\nstep', NETLOGO),
        ('python-equation-solver', 'integrate the update function\n(RK45) and round', PYTHON),
        ('node-graph-distribution-solution', 'allocate the aggregate change\nover admissible transitions', PYTHON),
        ('node-graph-distribution-update', 'apply the resulting moves to\nindividual agents', NETLOGO),
        ('outputs and stopping conditions', 'update displays; stop or advance\none step', NETLOGO)]),
]


def figura_arquitectura():
    W, H = 790, 723                      # el lienzo del PNG original, en puntos: misma composición
    fig = plt.figure(figsize=(W / 100, H / 100))
    ax = fig.add_axes([0, 0, 1, 1]); ax.set_xlim(0, W); ax.set_ylim(H, 0); ax.axis('off')
    panel_x, panel_w, box_h, gap, top = [18, 266, 514], 230, 95, 12, 20
    tops = {}
    for (titulo, cajas), x in zip(COLUMNAS, panel_x):
        ph = 58 + len(cajas) * (box_h + gap) + 8
        ax.add_patch(FancyBboxPatch((x, top), panel_w, ph, boxstyle='round,pad=0,rounding_size=10',
                                    fc=PANEL, ec='#DDDDDD', lw=0.8))
        ax.text(x + 14, top + 32, titulo, fontsize=9.5, fontweight='bold', va='center', color='#222222')
        tops[titulo] = (top + ph, ph)
        for i, (nombre, desc, color) in enumerate(cajas):
            y = top + 58 + i * (box_h + gap)
            ax.add_patch(FancyBboxPatch((x + 12, y), panel_w - 24, box_h, boxstyle='round,pad=0,rounding_size=4',
                                        fc='white', ec=color, lw=1.2))
            ax.add_patch(Rectangle((x + 12, y + 3), 5, box_h - 6, fc=color, ec='none'))
            ax.text(x + 26, y + 20, nombre, fontsize=8 if len(nombre) <= 26 else 7.1, fontweight='bold',
                    va='center', color='#111111')   # los nombres largos, algo menores, como en el original
            ax.text(x + 26, y + 52, desc, fontsize=7, va='center', color=GREY, linespacing=1.6)
    # flechas entre columnas (a la altura del segundo bloque) y bucle del GO
    for x, y in ((256, 165), (504, 270)):
        ax.plot([x], [y], marker='>', color='#444444', markersize=5, ls='none')
    xg, yg = panel_x[2] + panel_w / 2, tops[COLUMNAS[2][0]][0]
    ax.plot([xg, xg, 766, 766], [yg, yg + 28, yg + 28, 138], color='#444444', lw=1.2, solid_joinstyle='miter')
    ax.annotate('', xy=(panel_x[2] + panel_w + 2, 138), xytext=(766, 138),
                arrowprops=dict(arrowstyle='-|>', color='#444444', lw=1.2))
    ax.text(xg + 14, yg + 42, 'next simulation step', fontsize=7.5, color=GREY, va='center')
    # leyenda
    for x, color, texto in ((18, NETLOGO, 'Executed in NetLogo'), (190, PYTHON, 'Executed in Python')):
        ax.add_patch(Rectangle((x, H - 26), 26, 10, fc='white', ec=color, lw=1.4))
        ax.text(x + 36, H - 21, texto, fontsize=8, va='center', color='#222222')
    out = os.path.join(FIG, 'fig_architecture.pdf')
    fig.savefig(out); plt.close(fig)
    print('escrita', out)


# ============================================================== Figuras 2-5: datos de BehaviorSpace
import csv
from itertools import combinations

DATOS = os.environ.get('ABM_DATOS') or next((d for d in (os.path.join(_AQUI, 'Datos'), os.path.join(_AQUI, 'data'))
                                             if os.path.isdir(d)), os.path.join(_AQUI, 'data'))
ESTADOS = ['S', 'E', 'R', 'D', 'U', 'X']
NOMBRES = ['Susceptible', 'Exposed', 'Resistant', 'Degraded', 'Unavailable', 'Destroyed']
COLOR = dict(zip(ESTADOS, ['#3A7BD5', '#E8703A', '#3CB08C', '#E7B32A', '#E88CC0', '#2E8B3A']))
ESTILO = dict(zip(ESTADOS, ['-', '--', '-.', ':', (0, (3, 1, 1, 1)), '--']))
MARCA = dict(zip(ESTADOS, ['o', 's', '^', 'D', 'v', 'P']))
AZUL, NARANJA = '#3A7BD5', '#E8703A'
plt.rcParams.update({'font.size': 8, 'axes.titlesize': 8.5, 'axes.labelsize': 8, 'legend.fontsize': 7.5,
                     'xtick.labelsize': 7.5, 'ytick.labelsize': 7.5, 'axes.spines.top': False,
                     'axes.spines.right': False, 'axes.grid': True, 'grid.alpha': 0.3, 'grid.linewidth': 0.5})


def cargar(nombre):
    """Devuelve estados[run, paso, agente] (códigos 0..5), conteos[run, paso, estado] y solcontrol[run, paso]."""
    rows = list(csv.reader(open(os.path.join(DATOS, nombre), encoding='utf-8', newline='')))
    hdr, data = rows[6], rows[7:]
    i_run, i_step, i_sol, i_vec = hdr.index('[run number]'), hdr.index('[step]'), hdr.index('solcontrol'), len(hdr) - 1
    i_cnt = [hdr.index(f'count turtles with [serdux-state = "{e}"]') for e in ESTADOS]
    runs = sorted({int(r[i_run]) for r in data}); steps = sorted({int(r[i_step]) for r in data})
    cod = {e: k for k, e in enumerate(ESTADOS)}
    n = len(data[0][i_vec].strip('[]').split())
    est = np.zeros((len(runs), len(steps), n), dtype=np.uint8)
    cnt = np.zeros((len(runs), len(steps), 6), dtype=int)
    sol = np.zeros((len(runs), len(steps)), dtype=int)
    for r in data:
        a, t = runs.index(int(r[i_run])), steps.index(int(r[i_step]))
        est[a, t] = [cod[x] for x in r[i_vec].strip('[]').split()]
        cnt[a, t] = [int(r[i]) for i in i_cnt]
        sol[a, t] = int(r[i_sol])
    # el vector de agentes debe reproducir los conteos agregados de la misma fila
    assert all((np.bincount(est[a, t], minlength=6) == cnt[a, t]).all() for a in range(len(runs)) for t in range(len(steps)))
    return est, cnt, sol


def metricas(est, cnt, sol):
    R, T, N = est.shape
    pares = list(combinations(range(R), 2))
    d = np.array([[np.mean(est[i, t] != est[j, t]) for (i, j) in pares] for t in range(T)])   # [paso, par]
    cambios = (np.diff(est.astype(int), axis=1) != 0).sum(axis=1).ravel()                      # por agente y réplica
    tiempo = np.array([(est == k).mean() for k in range(6)])                                    # fracción del run
    llego_x = (est == 5).any(axis=1).mean()
    return dict(determinista=all((cnt[a] == cnt[0]).all() for a in range(R)),
                d_media=d.mean(axis=1), d_min=d.min(axis=1), d_max=d.max(axis=1), pares=len(pares),
                cambios=cambios, tiempo=tiempo, llego_x=llego_x,
                busqueda=sol[:, 1:].mean(), pasos_reconc=sorted({t for a in range(R) for t in range(1, T) if sol[a, t] == 0}),
                reconc_iguales=all(set(np.where(sol[a, 1:] == 0)[0] + 1) == set(np.where(sol[0, 1:] == 0)[0] + 1) for a in range(R)))


def validar(m1, m2):
    """Las cifras que afirma el texto del artículo (§V y §VI), recalculadas desde los CSV."""
    c1, c2 = m1['cambios'], m2['cambios']
    comprobaciones = [
        ('agregado determinista, esc. 1', m1['determinista'], True),
        ('agregado determinista, esc. 2', m2['determinista'], True),
        ('pares de réplicas esc. 1 / 2', (m1['pares'], m2['pares']), (435, 45)),
        ('d(t) máximo esc. 1', round(m1['d_media'].max(), 3), 0.753),
        ('paso del máximo esc. 1', int(m1['d_media'].argmax()), 7),
        ('d(t) final esc. 1', round(m1['d_media'][-1], 3), 0.486),
        ('d(t) máximo esc. 2', round(m2['d_media'].max(), 3), 0.718),
        ('paso del máximo esc. 2', int(m2['d_media'].argmax()), 93),
        ('d(t) final esc. 2', round(m2['d_media'][-1], 3), 0.565),
        ('cambios de estado esc. 1: media, mediana, máximo', (round(c1.mean(), 2), float(np.median(c1)), int(c1.max())), (1.92, 2.0, 4)),
        ('esc. 1: % con 2, 1 y 0 cambios', tuple(round(100 * (c1 == k).mean(), 1) for k in (2, 1, 0)), (47.4, 30.4, 0.2)),
        ('cambios de estado esc. 2: media, máximo', (round(c2.mean(), 2), int(c2.max())), (1.69, 7)),
        ('esc. 2: % con 0 y 3 cambios', tuple(round(100 * (c2 == k).mean(), 1) for k in (0, 3)), (20.7, 29.5)),
        ('% que pasa por Destroyed esc. 1 / 2', (round(100 * m1['llego_x'], 1), round(100 * m2['llego_x'], 1)), (45.1, 58.9)),
        ('tiempo en R y X esc. 1 (%)', (round(100 * m1['tiempo'][2], 1), round(100 * m1['tiempo'][5], 1)), (29.8, 34.9)),
        ('tiempo en R, S y E esc. 2 (%)', tuple(round(100 * m2['tiempo'][k], 1) for k in (2, 0, 1)), (4.1, 45.5, 28.6)),
        ('pasos resueltos por búsqueda esc. 1 (%)', round(100 * m1['busqueda'], 1), 88.6),
        ('pasos de reconciliación esc. 1, iguales en todas las réplicas', (m1['pasos_reconc'], m1['reconc_iguales']), ([6, 24, 31, 33], True)),
        ('pasos resueltos por búsqueda esc. 2 (%)', round(100 * m2['busqueda'], 1), 100.0),
    ]
    fallos = [(n, v, e) for n, v, e in comprobaciones if v != e]
    for n, v, e in comprobaciones:
        print(('  ✓ ' if (n, v, e) not in fallos else '  ✗ ') + f'{n}: {v}' + ('' if (n, v, e) not in fallos else f'  (el texto dice {e})'))
    assert not fallos, f'{len(fallos)} cifras del texto no coinciden con los datos'


def fig_aggregate(cnt1, cnt2):
    fig, axs = plt.subplots(1, 2, figsize=(7.0, 2.7), sharey=True, gridspec_kw=dict(wspace=0.12, right=0.84))
    for ax, cnt, titulo in zip(axs, (cnt1, cnt2), ('(a)  $N$ = 71', '(b)  $N$ = 6200')):
        T = cnt.shape[1]; pct = 100 * cnt / cnt[0, 0].sum(); paso = max(1, T // 10)
        for k, e in enumerate(ESTADOS):
            ax.plot(range(T), pct[0, :, k], ls=ESTILO[e], color=COLOR[e], lw=1.3, label=NOMBRES[k])
            ax.plot(range(0, T, paso), pct[1, ::paso, k], ls='none', marker=MARCA[e], mfc='white', mec=COLOR[e], ms=4.5, mew=1.1)
        ax.set_title(titulo, loc='left'); ax.set_xlabel('Simulation step'); ax.set_xlim(0, T - 1); ax.set_ylim(-2, 102)
    axs[0].set_ylabel('Agents per state (%)')
    axs[1].legend(loc='upper left', bbox_to_anchor=(1.02, 1.0), frameon=False, handlelength=2.2)
    fig.savefig(os.path.join(FIG, 'fig_aggregate.pdf'), bbox_inches='tight'); plt.close(fig)


def fig_divergence(m1, m2):
    fig, ax = plt.subplots(figsize=(7.0, 2.7))
    for m, color, ls, marca, etiqueta in ((m1, AZUL, '-', 'o', '$N$ = 71  (30 replications, 435 pairs)'),
                                           (m2, NARANJA, '--', 's', '$N$ = 6200  (10 replications, 45 pairs)')):
        T = len(m['d_media']); x = np.arange(T) / (T - 1); paso = max(1, T // 12)
        ax.fill_between(x, m['d_min'], m['d_max'], color=color, alpha=0.12, lw=0)
        ax.plot(x, m['d_media'], ls=ls, color=color, lw=1.4, label=etiqueta)
        ax.plot(x[::paso], m['d_media'][::paso], ls='none', marker=marca, mfc='white', mec=color, ms=4.5, mew=1.1)
        k = int(m['d_media'].argmax())
        ax.annotate(f'max {m["d_media"][k]:.2f}', (x[k], m['d_media'][k]), xytext=(0, 7), textcoords='offset points',
                    ha='center', color=color, fontsize=7.5, fontweight='bold')
    ax.set_xlabel('Simulation progress (fraction of the run)'); ax.set_ylabel('Divergence $d(t)$')
    ax.set_xlim(0, 1); ax.set_ylim(0, 0.9); ax.legend(loc='lower right', frameon=False)
    fig.savefig(os.path.join(FIG, 'fig_divergence.pdf'), bbox_inches='tight'); plt.close(fig)


def fig_histories(est1):
    from matplotlib.colors import ListedColormap
    from matplotlib.patches import Patch
    R, T, N = est1.shape
    fig, axs = plt.subplots(1, 2, figsize=(7.0, 3.3), sharey=True, gridspec_kw=dict(wspace=0.08))
    cmap = ListedColormap([COLOR[e] for e in ESTADOS])
    for ax, a, titulo in zip(axs, (0, 1), ('(a)  Replication 1', '(b)  Replication 2')):
        # la malla va rasterizada a 600 dpi (celdas de ~50 px): evita las juntas blancas que algunos visores
        # dibujan entre rectángulos vectoriales adyacentes; ejes y texto siguen siendo vectoriales
        ax.pcolormesh(np.arange(T + 1) - 0.5, np.arange(N + 1) - 0.5, est1[a].T, cmap=cmap, vmin=-0.5, vmax=5.5,
                      edgecolors='none', antialiased=False, rasterized=True)
        ax.set_title(titulo, loc='left'); ax.set_xlabel('Simulation step'); ax.grid(False)
        ax.set_xlim(-0.5, T - 0.5); ax.set_ylim(N - 0.5, -0.5)
    axs[0].set_ylabel('Agent (ordered by identifier)')
    fig.legend(handles=[Patch(color=COLOR[e], label=n) for e, n in zip(ESTADOS, NOMBRES)], loc='lower center', ncol=6,
               frameon=False, bbox_to_anchor=(0.5, -0.12), handlelength=1.6, columnspacing=1.6)
    fig.savefig(os.path.join(FIG, 'fig_histories.pdf'), bbox_inches='tight', dpi=600); plt.close(fig)


def fig_churn(m1, m2):
    fig, axs = plt.subplots(1, 2, figsize=(7.0, 2.75), gridspec_kw=dict(wspace=0.3))
    ax = axs[0]; kmax = max(m1['cambios'].max(), m2['cambios'].max()); ks = np.arange(kmax + 1); w = 0.38
    for m, color, dx, etiqueta in ((m1, AZUL, -w / 2, '$N$ = 71'), (m2, NARANJA, w / 2, '$N$ = 6200')):
        pct = np.array([100 * (m['cambios'] == k).mean() for k in ks])
        ax.bar(ks + dx, pct, w, color=color, label=etiqueta)
        if m is m1:
            for k, v in zip(ks, pct):
                if v >= 5: ax.annotate(f'{v:.0f}', (k + dx, v), xytext=(0, 2), textcoords='offset points', ha='center', fontsize=7)
    ax.set_title('(a)  Node churn', loc='left'); ax.set_xlabel('State changes per agent'); ax.set_ylabel('Agents (%)')
    ax.set_xticks(ks); ax.legend(frameon=False); ax.grid(axis='x', visible=False)
    ax = axs[1]; xs = np.arange(6)
    for m, color, dx in ((m1, AZUL, -w / 2), (m2, NARANJA, w / 2)):
        ax.bar(xs + dx, 100 * m['tiempo'], w, color=color)
    ax.set_title('(b)  Time in state', loc='left'); ax.set_xlabel('State'); ax.set_ylabel('Time in state (% of run)')
    ax.set_xticks(xs); ax.set_xticklabels(ESTADOS); ax.grid(axis='x', visible=False)
    fig.savefig(os.path.join(FIG, 'fig_churn.pdf'), bbox_inches='tight'); plt.close(fig)


def figuras_resultados():
    est1, cnt1, sol1 = cargar('esc1.csv'); est2, cnt2, sol2 = cargar('esc2_nodos10.csv')
    print(f'esc1: {est1.shape[0]} réplicas × {est1.shape[1]} pasos × {est1.shape[2]} agentes · '
          f'esc2_nodos10: {est2.shape[0]} × {est2.shape[1]} × {est2.shape[2]}')
    m1, m2 = metricas(est1, cnt1, sol1), metricas(est2, cnt2, sol2)
    print('Validación de las cifras del texto contra los datos:'); validar(m1, m2)
    fig_aggregate(cnt1, cnt2); fig_divergence(m1, m2); fig_histories(est1); fig_churn(m1, m2)
    print('escritas fig_aggregate.pdf, fig_divergence.pdf, fig_histories.pdf, fig_churn.pdf')


if __name__ == '__main__':
    figura_arquitectura()
    figuras_resultados()
