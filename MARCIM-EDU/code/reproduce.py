"""Run from any directory: python code/reproduce.py [--out DIRECTORY]."""
import argparse,csv,json,hashlib,platform,sys
from pathlib import Path
import numpy as np
import scipy
from engine import BASE, CONFIG, PALANCAS, ESTADOS, jugar, ronda, activos

def read(p):
    with p.open(newline='') as f:return list(csv.DictReader(f))
def save(p,fields,rows):
    with p.open('w',newline='') as f:
        w=csv.writer(f);w.writerow(fields);w.writerows(rows)
def run(out):
    out.mkdir(parents=True,exist_ok=True)
    for row in read(BASE/'manifest-sha256.csv'):
        assert hashlib.sha256((BASE/row['file']).read_bytes()).hexdigest()==row['sha256'],row['file']
    trajectories=[];ends=[];worst=0;max_nodes=0
    refs={(r['escenario'],int(r['ronda'])):r for r in read(BASE/'reference/recorded-replay-comparison.csv')}
    for scenario,c in CONFIG.items():
        ys,ts=jugar(scenario)
        trajectories += [(scenario,*x) for x in ts]
        for i,y in enumerate(ys):
            assert sum(y)==350 and min(y)>=0,(scenario,i,y)
            rec=c['recorded_states'][i];pp=activos(y)-c['recorded_active_pct'][i]
            nodes=sum(abs(a-b) for a,b in zip(y,rec))//2
            assert abs(pp)<=2.0,(scenario,i,pp)
            assert y==[int(refs[(scenario,i+1)][s]) for s in ESTADOS],(scenario,i,y)
            if scenario!='prueba':worst=max(worst,abs(pp));max_nodes=max(max_nodes,nodes)
            ends.append((scenario,i+1,*y,activos(y),c['recorded_active_pct'][i],pp,nodes))
    save(out/'trajectories.csv',['scenario','round','t',*ESTADOS],trajectories)
    save(out/'round-outputs.csv',['scenario','round',*ESTADOS,'active_pct','recorded_active_pct','difference_pp','reassigned_nodes'],ends)
    reference=read(BASE/'reference/trajectories-reference.csv')
    assert len(reference)==len(trajectories)
    for got,r in zip(trajectories,reference):
        assert tuple(got)==(r['escenario'],int(r['ronda']),int(r['t']),*[int(r[s]) for s in ESTADOS]),(got,r)
    sweep=[];rounds=CONFIG['neutral']['rounds'];ys,_=jugar('neutral')
    for r in (2,3):
        start=ys[r-1];p0=rounds[r];y=ronda(start,p0)[-1]
        sweep.append(('neutral',r+1,'base','','',*y,round(activos(y),2)))
        for lever in PALANCAS:
            for value in np.round(np.arange(.1,1.01,.1),1):
                p=dict(p0);p[lever]=float(value);y=ronda(start,p)[-1]
                assert sum(y)==350 and min(y)>=0
                sweep.append(('neutral',r+1,lever,float(value),p0[lever],*y,round(activos(y),2)))
    assert len(sweep)==162
    refs=read(BASE/'reference/sensitivity-reference.csv')
    assert len(refs)==162
    for a,b in zip(sweep,refs):
        assert a[0:3]==(b['fondo'],int(b['ronda']),b['palanca'])
        assert tuple(a[5:11])==tuple(int(b[s]) for s in ESTADOS)
        assert abs(a[-1]-float(b['activos_pct']))<.005
    save(out/'sensitivity.csv',['background','round','lever','value','baseline',*ESTADOS,'active_pct'],sweep)
    data=read(BASE/'data/cycle2-scored-matrix.csv')
    M=np.array([[int(r[f'i{i}']) for i in range(1,26)] for r in data])
    assert M.shape==(12,25) and np.isin(M,[0,1]).all()
    group=np.array([r['grupo'] for r in data]);C=M[group=='Ctrl'];I=M[group=='Intv']
    assert C.shape==(8,25) and I.shape==(4,25)
    assert C.sum()==114 and I.sum()==91
    for x,r in zip(M,data):assert x.sum()==int(r['total'])
    stats={}
    for label,x in [('control',C),('intervention',I),('pooled_reference_only',M)]:
        t=x.sum(axis=1);p=x.mean(axis=0);v=t.var()
        kr=25/24*(1-(p*(1-p)).sum()/v)
        stats[label]={'n':len(x),'mean':float(t.mean()),'sample_sd':float(t.std(ddof=1)),'totals':t.tolist(),'KR20_exploratory':float(kr)}
    assert abs(stats['control']['KR20_exploratory']-.773)<.001
    assert abs(stats['intervention']['KR20_exploratory']-.231)<.001
    lo={r['item']:r['LO'] for r in read(BASE/'data/assessment-instrument-es-en.csv')}
    groups={}
    for item,outcome in lo.items():groups.setdefault(outcome,[]).append(int(item)-1)
    results=[]
    for outcome,idx in sorted(groups.items()):
        pc=C[:,idx].mean();pi=I[:,idx].mean();g=(pi-pc)/(1-pc)
        results.append((outcome,int(C[:,idx].sum()),C[:,idx].size,int(I[:,idx].sum()),I[:,idx].size,100*pc,100*pi,100*(pi-pc),g))
    assert sum(abs(r[-1]-1)<1e-9 for r in results)==7
    save(out/'learning-outcomes.csv',['LO','control_correct','control_responses','intervention_correct','intervention_responses','control_pct','intervention_pct','difference_pp','normalized_between_group_difference'],results)
    itemstats=[]
    for j in range(25):
        x=M[:,j];rest=M.sum(axis=1)-x
        corr=float(np.corrcoef(x,rest)[0,1]) if x.std()>0 and rest.std()>0 else None
        itemstats.append((j+1,float(C[:,j].mean()),float(I[:,j].mean()),corr))
    save(out/'item-statistics.csv',['item','p_control','p_intervention','corrected_item_total'],itemstats)
    summary={'version':'1.1','python':sys.version,'numpy':np.__version__,'scipy':scipy.__version__,'platform':platform.platform(),'reproduction_max_abs_pp':worst,'max_reassigned_nodes':max_nodes,'sensitivity_runs':len(sweep),'cycle2':stats,'limitations':['No new participant session or independent expert panel.','No causal inference; posttest-only non-randomized comparison.','No independent psychometric validation.','NetLogo integer node-reassignment heuristic not reimplemented.','Pooled KR20 is not evidence of validity.']}
    (out/'verification.json').write_text(json.dumps(summary,indent=2))
    plot(out,trajectories,sweep)
    print(json.dumps({'status':'all checks passed','max_abs_pp':worst,'max_reassigned_nodes':max_nodes,'sweep_runs':162,'participant_matrix':'12x25'},indent=2))

def plot(out,trajectories,sweep):
    import matplotlib;matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    plt.rcParams.update({'pdf.fonttype':42,'font.family':'serif','font.size':8})
    f,ax=plt.subplots(figsize=(7.16,2.8))
    for scenario,label,ls in [('pesimista','Pessimistic','-'),('neutral','Neutral','--'),('optimista','Optimistic',':')]:
        pts=[r for r in trajectories if r[0]==scenario]
        ax.plot([(r[1]-1)*48+r[2] for r in pts],[100*sum(r[3:6])/sum(r[3:9]) for r in pts],ls,color='black',label=label)
    ax.set(xlabel='Simulation step',ylabel='Active services (%)',ylim=(0,100));ax.legend(frameon=False);ax.grid(alpha=.25);f.tight_layout();f.savefig(out/'trajectories.pdf');plt.close(f)
    f,axs=plt.subplots(2,1,figsize=(3.5,4.3),sharex=True)
    styles=['-','--',':','-.',(0,(5,1)),(0,(1,1)),(0,(3,1,1,1)),(0,(5,3))]
    for ax,r in zip(axs,[3,4]):
        for lever,ls in zip(PALANCAS,styles):
            pts=[a for a in sweep if a[1]==r and a[2]==lever]
            ax.plot([a[3] for a in pts],[a[-1] for a in pts],ls=ls,label=lever,color='black')
        ax.set_title(f'Round {r}');ax.set_ylabel('Active services (%)');ax.grid(alpha=.25)
    axs[-1].set_xlabel('Lever value');axs[0].legend(frameon=False,ncol=4,fontsize=6);f.tight_layout();f.savefig(out/'sensitivity.pdf');plt.close(f)

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--out',type=Path,default=BASE/'results');a=p.parse_args();run(a.out)
