#%% load data
import pandas as pd
import pastas as ps
    
gwl = pd.read_csv('data/wells/clean/daily/lv01.csv', 
                  parse_dates = True, index_col='date').drop(columns = 'id').squeeze().asfreq('D')

#%%

precip_mn = pd.read_csv('data/mesonet/clean/olathe.csv', parse_dates = True, index_col = 'date')['precip'].asfreq('D').rename('precip_mn')
eto_mn = pd.read_csv('data/mesonet/clean/olathe.csv', parse_dates = True, index_col = 'date')['eto_grass'].asfreq('D').rename('eto_mn')
precip_la = pd.read_csv('data/met_station/clean/Lawrence_Airport.csv', parse_dates = True, index_col = 'date')['precip'].asfreq('D').rename('precip_la')
eto_la = pd.read_csv('data/met_station/with_eto/Lawrence_Airport.csv', parse_dates = True, index_col = 'date')['eto'].asfreq('D').rename('eto_la')

stage = pd.read_csv('data/river/clean/daily/Desoto.csv', parse_dates = True, index_col = 'date')['stage'].asfreq('D').rename('stage')
stage = (stage - 754.12)*0.3048

irr = pd.read_csv('data/WIMAS/daily_use/lv01_irr.csv', parse_dates = True, index_col = 'date')['irr'].asfreq('D').rename('irr')
mun = pd.read_csv('data/WIMAS/daily_use/lv01_mun.csv', parse_dates = True, index_col = 'date')['mun'].asfreq('D').rename('mun')

ps.plots.series(head = gwl, stresses = [precip_mn, eto_mn, stage, irr, mun], hist = False)

#%% model

rch_lin = ps.RechargeModel(prec = precip_la, evap = eto_mn, recharge=ps.rch.Linear(), rfunc=ps.Gamma(), name = 'recharge')
rch_flex = ps.RechargeModel(prec = precip_mn, evap = eto_mn,
                                recharge = ps.rch.FlexModel(), rfunc = ps.Gamma(),
                                name = 'Flex')

stage_gam = ps.StressModel(stress = stage, rfunc = ps.Gamma(), name = 'stage', settings = 'level')

pump_irr = ps.StressModel(stress = irr, rfunc = ps.Hantush(), name = 'irr', up = False, settings = 'well')
pump_mun = ps.StressModel(stress = mun, rfunc = ps.Hantush(), name = 'mun', up = False, settings = 'well')

ml = ps.Model(gwl, name = 'head')
ml.add_stressmodel(rch_lin)
ml.add_stressmodel(stage_gam)
ml.add_noisemodel(ps.noisemodels.ArNoiseModel())
# ml.solve()
# ml.plots.results()
# metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
# print(metrics)
# ml.add_stressmodel(pump_mun)
ml.add_stressmodel(pump_irr)
ml.solve()
ml.plots.results()
metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
print(metrics)

