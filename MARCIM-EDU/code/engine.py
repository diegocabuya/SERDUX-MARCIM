"""MARCIM-EDU system-level replay v1.0.
Equations and time-grid logic transcribed from the source snapshot bundled in
source/MARCIM-WG.nlogo. Does not implement node-identity reassignment.
"""
import json
from pathlib import Path
import numpy as np
from scipy.integrate import solve_ivp
BASE = Path(__file__).resolve().parents[1]
PALANCAS = ['TCD','TCI','TSS','CCM','CDE','CPR','CCR','CDV']
ESTADOS = ['S','E','R','D','U','X']
CONFIG = json.loads((BASE/'data/configurations.json').read_text())
def leer_escenario(nombre):
    c=CONFIG[nombre]
    return c['rounds'], c['initial'], c['recorded_states'], c['recorded_active_pct']

def sistema(t, y, PRO0, L, ADE, ADU, CCM, CDE, CCR, CPR, CDV, a, m, tf, UOC, NNO):
    """Copia literal de system() del .nlogo (python-equation-solver)."""
    S, E, R, D, U, X = y
    N = S + E + R + D + U + X
    LRC = 3 / ((1 / CCM) + (1 / CDE) + (1 / CDV))
    IRC = 3 / ((1 / CPR) + (1 / CCR) + (1 / CDV))
    xi = 2 / ((1 / L) + (1 / ADE))
    PRO = PRO0 / (1 + ((1 - xi) * (D + U + X)))
    Theta = ADU * (1 - IRC)
    Iphase = ((1 - IRC) * ADE) / (1 + np.exp(-LRC * (t - (tf * Theta) + (tf / 4))))
    Fphase = -((1 - IRC) * ADE) / (1 + np.exp(-IRC * (t - (tf * Theta) + (tf / 4) - (tf * Theta))))
    CAD = Iphase + Fphase
    CAU = 2 / ((1 / ADE) + (1 / ADU)) if (ADE >= 0.5 and ADU <= 0.5) else 0.0
    CAX = 1 / ((1 / ADE) + (1 / ADU)) if (ADE >= 0.5 and ADU > 0.5) else 0.0
    REC = (CCR / 2) * np.tanh(((D + U + X) / N) * 3)
    SAN = (CPR / 2) * (1 - np.tanh(((S + E) / N) * 3))
    LOR = np.abs(np.exp(-a * t) * np.cos((2 * np.pi * t) / m))
    dS = NNO + (LOR * R) - (PRO * S) - (UOC * S) - (SAN * S)
    dE = (PRO * S) - (CAD * E) - (UOC * E) - (SAN * E)
    dR = (REC * D) + (REC * U) + (SAN * S) + (SAN * E) - (LOR * R) - (UOC * R)
    dD = (CAD * E) - (REC * D) - (UOC * D) - (CAU * D) - (CAX * D)
    dU = (UOC * (S + E + D + R)) + (CAU * D) - (CAX * U) - (REC * U)
    dX = (CAX * D) + (CAX * U)
    return [dS, dE, dR, dD, dU, dX]


def probabilidad_L(p, N):
    """cyber-risk-approach-calculation: L = ALI - 0.1*TLI (constante dentro de la ronda)."""
    LRC = 3 / ((1 / p['CCM']) + (1 / p['CDE']) + (1 / p['CDV']))
    TNT = min(p['nl'] / (N - 1), 0.1)
    TLI = 0.25 * LRC + 0.25 * p['TCI'] + 0.25 * p['TCD'] + 0.25 * p['TSS'] - TNT
    return (p['ATF'] + p['VUF']) / 2 - TLI * 0.1


def tick(y0, p, t):
    """Un «go» sin cambio de parametros: integra desde el estado inicial de la ronda y lee el punto
    de la rejilla mas cercano a t; redondeo y correccion como el .nlogo."""
    N0 = sum(y0)
    simt = int(t + 20)
    args = (p['PRO0'], probabilidad_L(p, N0), p['ADE'], p['ADU'], p['CCM'], p['CDE'], p['CCR'], p['CPR'],
            p['CDV'], p['a'], p['m'], p['tf'], p['UOC'], 0)
    sol = solve_ivp(sistema, (0, simt), y0, args=args, method="RK45", rtol=1e-3, atol=1e-6, dense_output=True)
    tv = np.linspace(0, simt, simt)
    S, E, R, D, U, X = sol.sol(tv[np.abs(tv - t).argmin()])
    S, E, R, D = (max(0, int(np.round(v))) for v in (S, E, R, D))
    U, X = max(0, int(np.ceil(U))), max(0, int(np.floor(X)))
    dif = N0 - (S + E + R + D + U + X)
    if dif != 0:
        U += dif
        if U < 0:
            U = 0
            R = max(0, R + dif)
    return [S, E, R, D, U, X]


def ronda(y0, p):
    """EJECUTAR (POR PASOS): el primer go consume el cambio de parametros (t = 0), luego t = 1..pasos-1.
    Para en cuanto no quede ningun nodo en D/U/X o ninguno en S/E/R."""
    tray, y = [], list(y0)
    for t in range(int(p['pasos'])):
        y = tick(y0, p, t)
        tray.append(y)
        if y[3] + y[4] + y[5] == 0 or y[0] + y[1] + y[2] == 0:
            break
    return tray


def jugar(nombre, override=None):
    """Las cinco rondas de un escenario; override: {indice_ronda: {palanca: valor}}."""
    rondas, y, _, _ = leer_escenario(nombre)
    por_ronda, trayectoria = [], []
    for r, p in enumerate(rondas):
        p = dict(p)
        if override and r in override:
            p.update(override[r])
        tray = ronda(y, p)
        y = tray[-1]
        por_ronda.append(y)
        trayectoria.extend([(r + 1, t) + tuple(s) for t, s in enumerate(tray)])
    return por_ronda, trayectoria


def activos(y):
    return 100 * (y[0] + y[1] + y[2]) / sum(y)


