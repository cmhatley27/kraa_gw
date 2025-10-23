#%% select model and load settings/data
import numpy as np
import pandas as pd
import pastas as ps

well = 'dg01'
name = '1'

model_dir = 'data/models/'+well+'/'+name+'/'
input_dir = model_dir+'in/'
output_dir = model_dir+'out/'

input_settings = pd.read_csv(model_dir+'input_settings.csv')
eval_settings = pd.read_csv(model_dir+'eval_settings.csv')

gwl = pd.read_csv(input_dir+'gwl.csv', parse_dates = True, index_col='date').squeeze()
precip = pd.read_csv(input_dir+'precip.csv', parse_dates = True, index_col='date').squeeze()
et = pd.read_csv(input_dir+'et.csv', parse_dates = True, index_col='date').squeeze()
pump = pd.read_csv(input_dir+'pump.csv', parse_dates = True, index_col='date').squeeze()

ps.plots.series(head = gwl, stresses = [precip, et, pump], hist = True)
#%% model and save outputs

rch_lin = ps.RechargeModel(prec = precip, evap = et, recharge=ps.rch.Linear(), rfunc=ps.Gamma(), name = 'recharge')
rch_flex = ps.RechargeModel(prec = precip, evap = et, recharge=ps.rch.FlexModel(interception=False), rfunc=ps.Gamma(), name = 'recharge')
rch_pet = ps.RechargeModel(prec = precip, evap = et, recharge=ps.rch.Peterson(), rfunc=ps.Gamma(), name = 'recharge')
pump_stress = ps.StressModel(stress = pump, rfunc = ps.Hantush(), name = 'irr', up = False, settings = 'well')

ml = ps.Model(gwl['gwl'], name = 'head')
ml.add_stressmodel(rch_flex)
ml.set_parameter('recharge_lp', initial=0.01, vary = False)
ml.set_parameter('recharge_kv', initial=1.0, vary = False)
ml.set_parameter('recharge_srmax', initial=250, vary = False)
# ml.add_stressmodel(rch_lin)
# ml.set_parameter('recharge_f', initial = -1, vary = False)
# ml.set_parameter('constant_d', initial = 243.83, vary = False)
# ml.set_parameter('recharge_A', pmax = 0.4)
ml.add_stressmodel(pump_stress)
ml.add_noisemodel(ps.noisemodels.ArNoiseModel())
ml.solve(tmin = cal[1], tmax = cal[-1], report='full', freq_obs='30D')
metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
print(metrics)
ml.plots.results()
ml.plots.diagnostics()
#%%
ml.get_output_series().to_csv('data/models/dg01/1/out/cal.csv')
ml.simulate(tmin = val[1], tmax = val[-1]).to_csv('data/models/dg01/1/out/val.csv')

#%% get plots
ml.plots.results()
ml.plots.diagnostics()
ml.stats.diagnostics()
ml.plots.block_response()
ml.plots.step_response()
ml.get_step_response('recharge').to_csv('data/models/dg01/1/out/rch_step.csv')
ml.get_block_response('recharge').to_csv('data/models/dg01/1/out/rch_block.csv')
ps.rfunc.step()
ml.plots.decomposition()

