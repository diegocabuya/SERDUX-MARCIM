"""Additional archival-data checks for MARCIM-EDU v1.1.
Not a participant experiment, a calibrated model, or an independent validation.
Run after reproduce.py; historical engine and scored responses are unchanged.
"""
import argparse,csv,json,itertools
from pathlib import Path
import numpy as np
from scipy.integrate import solve_ivp as scipy_solve
import engine as e

def save(path,fields,rows):
 with path.open('w',newline='') as f:
  w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(rows)

def run(out):
 out.mkdir(parents=True,exist_ok=True)
 with (e.BASE/'data/cycle2-scored-matrix.csv').open() as f:data=list(csv.DictReader(f))
 C=np.array([[int(r[f'i{j}']) for j in range(1,26)] for r in data if r['grupo']=='Ctrl'])
 I=np.array([[int(r[f'i{j}']) for j in range(1,26)] for r in data if r['grupo']=='Intv'])
 # Exhaustive deletion of one officer in each group: 8*4 deterministic cases.
 # This range is an influence diagnostic, never a confidence interval.
 deletion=[]
 for c,i in itertools.product(range(8),range(4)):
  pc=np.delete(C,c,axis=0).mean()*100;pi=np.delete(I,i,axis=0).mean()*100
  deletion.append({'removed_control':f'P{c+1:02}','removed_intervention':f'P{i+9:02}','control_pct':pc,'intervention_pct':pi,'difference_pp':pi-pc})
 save(out/'participant-deletion.csv',list(deletion[0]),deletion)
 scored=[]
 for name,excluded in [('administered',[]),('control_taxonomy',[8,9,23]),('broader_context_review',[2,7,8,9,10,11,12,13,14,17,19,22,23,24])]:
  keep=[j-1 for j in range(1,26) if j not in excluded]
  pc=C[:,keep].mean()*100;pi=I[:,keep].mean()*100
  scored.append({'analysis':name,'excluded_items':';'.join(map(str,excluded)),'items_retained':len(keep),'control_pct':pc,'intervention_pct':pi,'difference_pp':pi-pc})
 save(out/'score-sensitivity.csv',list(scored[0]),scored)
 # Refine only the documented non-monotonic cases. Keep the neutral game's
 # historical replay start fixed, so this isolates numerical settings.
 ys,_=e.jugar('neutral');start=ys[2];refinement=[]
 for lever,value in itertools.product(['CPR','CCR','CDV'],[.1,.2,.5,1.]):
  p=dict(e.CONFIG['neutral']['rounds'][3]);p[lever]=value
  base=e.ronda(start,p)[-1]
  def tight(*args,**kw):
   kw.update(rtol=1e-8,atol=1e-10,max_step=.25)
   sol=scipy_solve(*args,**kw)
   assert sol.success,sol.message
   return sol
  e.solve_ivp=tight
  try:ref=e.ronda(start,p)[-1]
  finally:e.solve_ivp=scipy_solve
  refinement.append({'lever':lever,'value':value,'baseline_active_pct':e.activos(base),'refined_active_pct':e.activos(ref),'same_integer_state':base==ref,'difference_pp':e.activos(ref)-e.activos(base)})
 save(out/'solver-refinement.csv',list(refinement[0]),refinement)
 # Paired grids explore three targeted interaction surfaces, not global
 # sensitivity indices. Other controls, starting state and presets stay fixed.
 grids=[]
 for r in [2,3]:
  start=ys[r-1]
  for a,b in itertools.combinations(['CPR','CCR','CDV'],2):
   for va,vb in itertools.product([.1,.2,.5,1.],repeat=2):
    p=dict(e.CONFIG['neutral']['rounds'][r]);p[a]=va;p[b]=vb
    y=e.ronda(start,p)[-1]
    assert sum(y)==350 and min(y)>=0,(r,a,b,va,vb,y)
    grids.append({'round':r+1,'lever_a':a,'lever_b':b,'value_a':va,'value_b':vb,'active_pct':e.activos(y),**dict(zip(e.ESTADOS,y))})
 save(out/'paired-control-grid.csv',list(grids[0]),grids)
 # Number of adjacent grid edges where increasing a control decreases output.
 reversals=[]
 for r in [3,4]:
  for a,b in itertools.combinations(['CPR','CCR','CDV'],2):
   pts={(x['value_a'],x['value_b']):x['active_pct'] for x in grids if x['round']==r and x['lever_a']==a and x['lever_b']==b}
   vals=[.1,.2,.5,1.]
   for axis in ['a','b']:
    for fixed in vals:
     for low,high in zip(vals[:-1],vals[1:]):
      lo=pts[(low,fixed) if axis=='a' else (fixed,low)];hi=pts[(high,fixed) if axis=='a' else (fixed,high)]
      if hi<lo-1e-10:reversals.append({'round':r,'pair':a+'+'+b,'increased':a if axis=='a' else b,'fixed_value':fixed,'from_value':low,'to_value':high,'change_pp':hi-lo})
 save(out/'paired-control-reversals.csv',list(reversals[0]),reversals)
 summary={'version':'1.1','participant_deletions':len(deletion),'deletion_difference_min_pp':min(r['difference_pp'] for r in deletion),'deletion_difference_max_pp':max(r['difference_pp'] for r in deletion),'score_sensitivity':scored,'solver_refinement_cases':len(refinement),'solver_refinement_max_abs_pp':max(abs(r['difference_pp']) for r in refinement),'same_integer_end_states':sum(r['same_integer_state'] for r in refinement),'paired_grid_cases':len(grids),'grid_negative_edges':len(reversals),'max_grid_drop_pp':min(r['change_pp'] for r in reversals),'interpretation':'Deterministic archival-data diagnostics. Deletion range is not a confidence interval. Post hoc item exclusions do not validate a scale. Targeted grids do not establish global sensitivity, realism or pedagogical validity.'}
 (out/'extended-verification.json').write_text(json.dumps(summary,indent=2))
 print(json.dumps(summary,indent=2))

if __name__=='__main__':
 p=argparse.ArgumentParser();p.add_argument('--out',type=Path,default=e.BASE/'results');a=p.parse_args();run(a.out)
