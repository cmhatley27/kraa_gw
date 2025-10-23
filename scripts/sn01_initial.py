#%% load data
import pandas as pd
import pastas as ps
    
gwl = pd.read_csv('data/wells/clean/daily/sn01.csv', 
                  parse_dates = True, index_col='date').drop(columns = 'id').squeeze().asfreq('D')

precip_mn = pd.read_csv('data/mesonet/clean/rossville.csv', parse_dates = True, index_col = 'date')['precip'].asfreq('D').rename('precip_mn')
eto_mn = pd.read_csv('data/mesonet/clean/rossville.csv', parse_dates = True, index_col = 'date')['eto_grass'].asfreq('D').rename('eto_mn')

stage = pd.read_csv('data/river/clean/daily/Rossville.csv', parse_dates = True, index_col = 'date')['stage'].asfreq('D').rename('stage')
stage = (stage - 897.47)*0.3048

irr = pd.read_csv('data/WIMAS/daily_use/sn01_irr.csv', parse_dates = True, index_col = 'date')['irr'].asfreq('D').rename('irr')

ps.plots.series(head = gwl, stresses = [precip_mn, eto_mn, stage, irr], hist = False)

#%% add pumping

rch_lin = ps.RechargeModel(prec = precip_mn, evap = eto_mn, recharge=ps.rch.Linear(), rfunc=ps.Gamma(), name = 'recharge')
rch_flex = ps.RechargeModel(prec = precip_la, evap = eto_mn,
                                recharge = ps.rch.FlexModel(), rfunc = ps.Gamma(),
                                name = 'Flex')

stage_gam = ps.StressModel(stress = stage, rfunc = ps.Gamma(), name = 'stage', settings = 'level')

pump = ps.StressModel(stress = irr, rfunc = ps.Hantush(), name = 'irr', up = False, settings = 'well')

ml = ps.Model(gwl, name = 'head')
ml.add_stressmodel(rch_lin)
#ml.add_stressmodel(stage_gam)
ml.add_noisemodel(ps.noisemodels.ArNoiseModel())
#ml.set_parameter('constant_d', initial=276.6, vary = False)
ml.solve()
ml.plots.results()
metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
print(metrics)
ml.add_stressmodel(pump)
#ml.set_parameter('constant_d', initial=276.6, vary = False)
ml.solve()
ml.plots.results()
metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
print(metrics)

ml.plots.diagnostics()
ml.stats.diagnostics()
#%% Save outputs
outs = ml.get_output_series()
outs.to_csv('data/modeling_temp/sn01_outputs.csv')

results = ml.plots.results()
results.savefig('data/modeling_temp/sn01_results.png')
