# MARCIM-EDU supplementary material

Version 1.3, 10 November 2026. Manuscript Access-2026-42686.

This package accompanies the revised manuscript for peer review. It is also
published in the project repository, folder `MARCIM-EDU`:
https://github.com/diegocabuya/SERDUX-MARCIM/tree/main/MARCIM-EDU It is self-contained:
no private workbook, platform login or repository write is required for replay.

## Reproduce

Tested numerical environment: Python 3.9.6, NumPy 2.0.2, SciPy 1.13.1,
Matplotlib 3.9.4. Use a compatible Python 3.9--3.12 environment.

```
python -m pip install -r requirements.txt
python code/reproduce.py
python code/extended_checks.py
python code/global_checks.py
python code/matched_budget.py
```

The first command verifies SHA-256 hashes, regenerates all four recorded games,
compares every trajectory state with the archived reference, performs the
162-run one-at-a-time sweep, reconstructs assessment totals and outcome metrics,
and writes CSV, PDF and JSON outputs under results/. An assertion fails if
the archived regression-check discrepancy exceeds 2 percentage points, a count mismatches,
or a trajectory differs from its supplied reference.

The observed worst deviation for the three reported games is 1.72 percentage
points after rounding. This is comparison with the recorded NetLogo games;
regeneration of the supplied Python reference is exact in the tested environment.
The 2-point assertion is a software regression guard based on these archived
comparisons, not a prespecified scientific acceptance threshold or an error bound
for unobserved configurations.
The Python engine reproduces aggregate equations, grids, integer rounding and
round chaining. It does not reproduce the graphical node-identity reassignment
heuristic. Sensitivity does not validate realism or demonstrate a training effect.

## Files

- data/configurations.json: all round inputs, constants, initial and recorded states,
  extracted from the archived analysis workbook without retaining private data.
- data/decision-map.csv: all 35 nonzero card rules (Spanish source labels).
  Positive units are tokens, +0.1 each; negative units are direct multiples of 0.1.
- data/token-ledger.csv: issued and card-derived tokens by recorded game and round.
- data/cycle2-scored-matrix.csv: arbitrary participant codes, group, 25 binary
  item scores and total; no export IDs or exact timestamps.
- data/assessment-instrument-es-en.csv: administered Spanish wording and keys,
  plus the authors' English translation, item formats and learning-outcome mapping.
- data/cycle1-matrix.csv: 36 coded observations and the variables of the prior TTX study.
- data/episode-ratings.csv: coded numeric ratings for the two TTX surveys and the
  separate board prototype; direct identifiers and original platform row IDs are omitted; anonymity against all outside information is not guaranteed.
- reference/: original numerical outputs against which replay is checked.
- source/: inspected NetLogo source snapshot, for code inspection, not a claim
  about the exact installation or undocumented facilitation used in 2025.

## Assessment administration and scoring

The administered form was in Spanish and used Microsoft Forms. Eight comparison
officers did not play; four self-selected officers played and then answered.
Neither group received a pre-test. One point is awarded for a correct item and
zero otherwise. Ordering items require the complete correct sequence.
Items 15, 21 and 23 were not mandatory; no blank response occurred.
Item 25's individual points column was empty; its binary score was inferred
from each platform total and checked against a single consistent observed key.
The English translation was not administered. The export does not establish
whether options were shuffled. Original instruction screens, verified local
timestamp offsets and per-person delays after session closure were not retained
in the inspected archive; no new instructions are represented as historical ones.

The first author wrote the items without an independent validation panel.
Items 8, 9 and 23 use under-specified control terminology. The content audit
reviews all 25 items and flags fourteen context-dependent items for a separate
post hoc sensitivity check. This is an editorial review, not an independent
expert-panel validation. Proposed clarifications have not been administered. Published scores retain
the administered keys. Removing or correcting items now is not presented as
rescoring the historical assessment. The pooled reliability coefficient mixes
groups with different means and must not be treated as instrument validation.

## Decision admission and historical limits

The inspected software permits cyberattack in rounds 4--5, maximum two uses,
and ransom payment in rounds 3--5, maximum one use. The written facilitator
sheet differs; this package follows inspected software for the reproducible
specification without claiming to reconstruct the undocumented 2025 choices.
Phase-card conditions, currency charges and dice outcomes are checked by the
facilitator. The capability threshold of the cyberattack card is ambiguous;
accepted actions are explicit encoded inputs, not inferred from that wording.
Appendix C and Section VI distinguish the historical evidence from proposed
future rules for inadmissible or disputed decisions. The optimistic ledger has
47 issued rather than the standard 40 tokens (15 instead of 8 in round 3, which
the thesis narrative attributes to an exchange of BitMarCoins for tokens);
recorded inputs are retained, and version 1.3 adds the replay at 40 tokens.

## Source and provenance

Source snapshot: https://github.com/diegocabuya/SERDUX-MARCIM/blob/main/MARCIM-WG/MARCIM-WG.nlogo
Git blob SHA: 8f1c3f7842f8624e880c42c2fcf7e41ae242d094.
Retrieved 9 November 2026. The snapshot identifies NetLogo 6.4.0.
The underlying model's published licence is LGPL-3.0; that publication and
third-party notices retain their existing terms. No third-party workbook or
identifiable raw survey export is redistributed here.

The exercises and the assessment were educational activities conducted by the
research team within academic programmes; under the practice of the organizing
institutions they did not require ethics-committee review. Participants were told
at the start that the activities were academic and that their anonymous responses
would be used for research; consent was given orally and participation was
voluntary. Only de-identified records are included.


## Additional audit in version 1.1

The historical equations, accepted inputs, answer keys and scores are unchanged.
`code/extended_checks.py` writes:

- `participant-deletion.csv`: all 32 deletions of one comparison officer and one
  player. The observed difference ranges from 26.29 to 38.29 percentage points.
  This is an influence range, not a confidence interval. The four players shared
  one session; independent session variation cannot be estimated.
- `score-sensitivity.csv`: the original 25-item score, exclusion of items 8/9/23,
  and exclusion of the fourteen context-dependent items documented in
  `data/assessment-content-audit.csv`. All exclusions are post hoc diagnostics,
  not corrected historical scores or validated replacement scales.
- `solver-refinement.csv`: twelve round-4 cases, with three controls at four
  values. Tightened tolerances and maximum step 0.25 leave all twelve final
  integer vectors unchanged. This is a local numerical check.
- `paired-control-grid.csv` and `paired-control-reversals.csv`: 96 configurations
  in three four-by-four paired-control grids, in two fixed backgrounds. Of 144
  adjacent edges, 33 decrease active services when a control increases. These
  selected grids are not a global uncertainty distribution.

The node-count proxy weights every node equally and treats degraded nodes as
inactive. It is not measured mission performance or real service throughput.
The optimistic historical profile has extra issued resources; see the
version 1.3 replay at an equal budget below. Numerical reproducibility
does not establish operational or pedagogical validity.

## Version 1.2 additions

`code/global_checks.py` adds two analyses of the existing artifact and records:

- A global screen of the eight player levers in the three reported games and in
  rounds 2-5: 20 base points per background drawn by a discrete Latin hypercube
  (seed 20261109), and at each point every lever moved from 0.1 to 1.0 with the
  others held (3,840 runs). It is a screening analysis, not a variance-based
  decomposition. Runs in which the transcribed integer reconciliation leaves 349
  rather than 350 nodes are counted and retained (3 runs).
- A descriptive contrast of the 12 participant totals: the exact permutation
  distribution over all 495 splits into groups of 4 and 8, and Hedges' g with
  its large-sample 95% interval. Allocation was not random and the four players
  shared one session; these summaries describe separation within the records,
  not a treatment effect or its precision.

The script checks its output against `reference/global-checks.json` and fails on
any difference. The fourth archived game (`prueba` in configurations.json) is a
test run labeled as such in the archived workbook; it uses different attack
presets in rounds 4 and 5 and is not analyzed in the manuscript. The round
presets of the three reported games are identical; they differ only in the
encoded lever values.

## Version 1.3 additions

- `code/matched_budget.py`: replay of the optimistic game with the ordinary
  40-token budget. The seven surplus tokens of round 3 are removed in each of
  the 422 admissible ways (any seven of the fifteen tokens issued in that round,
  lever by lever), the lower lever values are carried through rounds 3-5, and the
  game is replayed. Final active services range from 47.14% to 80.86% (median
  61.71%); the outcome is ordered almost entirely by how many of the removed
  tokens come from the three impact controls (CPR, CCR, CDV). Output:
  `matched-budget.csv` and `matched-budget.json`, checked against `reference/`.
- `data/assessment-content-audit.csv` gains two columns: the situational-awareness
  level each item is mapped to by design, and a retrospective classification by
  the authors of the cognitive process the item demands (14 items recognition or
  recall, 7 understanding, 4 applying or evaluating in a stated situation). The
  classification has not been independently reviewed.
- A data dictionary and a licence file.

## Data dictionary

Column names keep the labels of the archived Spanish-language workbooks.

- `data/cycle2-scored-matrix.csv`: `participante` arbitrary code (P01-P12);
  `grupo` Ctrl = comparison officers, Intv = players; `i1`...`i25` item scores
  (1 correct, 0 incorrect); `total` sum of the 25 items.
- `data/assessment-instrument-es-en.csv`: `item`; `LO` learning outcome; `tipo`
  format (MC-4 multiple choice, T/F, ORD-4/ORD-5 ordering); `enunciado_es`,
  `opciones_es`, `clave_es` administered Spanish statement, options and key;
  `enunciado_en`, `opciones_en`, `clave_en` the authors' English translation.
- `data/assessment-content-audit.csv`: `item`; `retrospective_review`;
  `proposed_future_clarification_not_administered`; `source`;
  `included_in_broad_context_sensitivity` (True for the fourteen items excluded
  in the broader post hoc check); `sa_level_by_design`;
  `cognitive_process_retrospective`.
- `data/cycle1-matrix.csv`: `id` arbitrary code; `sesion` exercise (Argentina or
  Washington); `Y` overall satisfaction (0-5); `X1`-`X4` prior experience and
  training (1 yes, 0 no: any tabletop or crisis simulation; cyber simulation;
  maritime cyber simulation; cyber training); `X5`-`X7` exercise ratings;
  `X8`-`X13` scenario ratings (0-5).
- `data/episode-ratings.csv`: long format; `episode` (TTX_Argentina,
  TTX_Washington, Board_Washington, Board_model); `respondent` arbitrary code
  within the episode; `variable` (`Y`, `X1`-`X18`; `X14`-`X18` are the model
  items); `value`.
- `data/decision-map.csv`: `parametro` model input affected; `carta` card number;
  `nombre_carta` card name; `fichas` tokens granted (+1 = +0.1) or direct
  multiples of 0.1 subtracted (negative values).
- `data/token-ledger.csv`: `escenario` game (pesimista = pessimistic, neutral,
  optimista = optimistic, prueba = test run); `ronda` round; `palanca` lever;
  `fichas_entregadas` tokens issued by the facilitator; `fichas_por_cartas`
  tokens granted by phase cards.
- `data/configurations.json`: per game, `initial` state (S, E, R, D, U, X),
  `rounds` (the twelve inputs, constants, `pasos` = simulation steps, `nl` =
  links per node), `recorded_states` and `recorded_active_pct`.
- Lever and input acronyms are defined in Table 5 and Appendix C of the article.

## Licence

Code in `code/` is released under the MIT License; data, instrument text and
reference outputs in `data/` and `reference/` under Creative Commons Attribution
4.0 International (CC BY 4.0). See `LICENSE.md`. `source/MARCIM-WG.nlogo` is a
snapshot of the authors' file from the same repository, included for inspection.
