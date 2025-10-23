#%% load data
import pandas as pd
import pastas as ps
    
gwl = pd.read_csv('data/wells/clean/daily/lv01.csv', 
                  parse_dates = True, index_col='date').drop(columns = 'id').squeeze().asfreq('D')

precip_mn = pd.read_csv('data/mesonet/clean/olathe.csv', parse_dates = True, index_col = 'date')['precip'].asfreq('D').rename('precip_mn')
eto_mn = pd.read_csv('data/mesonet/clean/olathe.csv', parse_dates = True, index_col = 'date')['eto_grass'].asfreq('D').rename('eto_mn')
precip_la = pd.read_csv('data/met_station/clean/Lawrence_Airport.csv', parse_dates = True, index_col = 'date')['precip'].asfreq('D').rename('precip_la')
eto_la = pd.read_csv('data/met_station/with_eto/Lawrence_Airport.csv', parse_dates = True, index_col = 'date')['eto'].asfreq('D').rename('eto_la')

stage = pd.read_csv('data/river/clean/daily/Desoto.csv', parse_dates = True, index_col = 'date')['stage'].asfreq('D').rename('stage')
stage = (stage - 754.12)*0.3048

irr = pd.read_csv('data/WIMAS/daily_irrigation/lv01.csv', parse_dates = True, index_col = 'date')['irr'].asfreq('D').rename('irr')

ps.plots.series(head = gwl, stresses = [precip_la, eto_mn, stage, irr], hist = False)

def run_rch_model(rch):
    ml = ps.Model(gwl, name = 'head')
    ml.add_stressmodel(rch)
    ml.add_noisemodel(ps.noisemodels.ArNoiseModel())
    ml.solve()
    ml.plots.results()
    metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
    print(' ')
    print('*********** ' + rch.name + ' ***********')
    print(' ')
    print(metrics)

#%% linear recharge - comparing precip stations
rch_lin_gam_la_precip = ps.RechargeModel(prec = precip_la, evap = eto_mn, recharge=ps.rch.Linear(), rfunc=ps.Gamma(), name = 'Airport Precip')
rch_lin_gam_mn_precip = ps.RechargeModel(prec = precip_mn, evap = eto_mn, recharge=ps.rch.Linear(), rfunc=ps.Gamma(), name = 'Mesonet Precip')

run_rch_model(rch_lin_gam_la_precip)
run_rch_model(rch_lin_gam_mn_precip)

#not a huge difference so will go with airport precip data

#%% linear recharge - comparing transfer functions

rch_lin_gam = ps.RechargeModel(prec = precip_la, evap = eto_mn, recharge=ps.rch.Linear(), rfunc=ps.Gamma(), name = 'recharge')
rch_lin_4p = ps.RechargeModel(prec = precip_la, evap = eto_mn, recharge=ps.rch.Linear(), rfunc=ps.FourParam(), name = 'recharge')

run_rch_model(rch_lin_gam)
run_rch_model(rch_lin_4p)

#Four param is slightly better, but not by a lot

#%% nonlinear recharge - comparing methods with Gamma function

rch_peterson = ps.RechargeModel(prec = precip_la, evap = eto_mn,
                                recharge = ps.rch.Peterson(), rfunc = ps.Gamma(),
                                name = 'Peterson')
rch_flex = ps.RechargeModel(prec = precip_la, evap = eto_mn,
                                recharge = ps.rch.FlexModel(), rfunc = ps.Gamma(),
                                name = 'Flex')
rch_berend = ps.RechargeModel(prec = precip_la, evap = eto_mn,
                                recharge = ps.rch.Berendrecht(), rfunc = ps.Gamma(),
                                name = 'Berendrecht')

run_rch_model(rch_peterson)
run_rch_model(rch_flex)
run_rch_model(rch_berend)

ml = ps.Model(gwl, name = 'head')
ml.add_stressmodel(rch_flex)
ml.add_noisemodel(ps.noisemodels.ArmaNoiseModel())
ml.set_parameter('Flex_kv', initial = 1, vary = True)
ml.solve()
ml.plots.results()
metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
print(metrics)

noise = ml.noise()
resid = ml.residuals()

noise.to_csv('data/modeling_temp/noise.csv')
resid.to_csv('data/modeling_temp/resid.csv')

ml.plots.diagnostics()
ml.stats.diagnostics()

#%% linear rechare + stage

rch_lin = ps.RechargeModel(prec = precip_la, evap = eto_mn, recharge=ps.rch.Linear(), rfunc=ps.Gamma(), name = 'recharge')
rch_flex = ps.RechargeModel(prec = precip_la, evap = eto_mn,
                                recharge = ps.rch.FlexModel(), rfunc = ps.Gamma(),
                                name = 'Flex')

stage_gam = ps.StressModel(stress = stage, rfunc = ps.Gamma(), name = 'stage', settings = 'level')

ml = ps.Model(gwl, name = 'head')
ml.add_stressmodel(rch_lin)
ml.add_stressmodel(stage_gam)
ml.add_noisemodel(ps.noisemodels.ArNoiseModel())
#ml.set_parameter('Flex_kv', initial = 1, vary = True)
ml.solve()
ml.plots.results()
metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
print(metrics)

#%% add pumping

rch_lin = ps.RechargeModel(prec = precip_mn, evap = eto_mn, recharge=ps.rch.Linear(), rfunc=ps.Gamma(), name = 'recharge')
rch_flex = ps.RechargeModel(prec = precip_la, evap = eto_mn,
                                recharge = ps.rch.FlexModel(), rfunc = ps.Gamma(),
                                name = 'Flex')

stage_gam = ps.StressModel(stress = stage, rfunc = ps.Gamma(), name = 'stage', settings = 'level')

pump = ps.StressModel(stress = irr, rfunc = ps.Hantush(), name = 'irr', up = False, settings = 'well')

ml = ps.Model(gwl, name = 'head')
ml.add_stressmodel(rch_lin)
ml.add_stressmodel(stage_gam)
ml.add_noisemodel(ps.noisemodels.ArNoiseModel())
ml.solve()
ml.plots.results()
metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
print(metrics)
ml.add_stressmodel(pump)
ml.solve()
ml.plots.results()
metrics = {'R2': ml.stats.rsq(), 'RMSE': ml.stats.rmse(), 'KGE': ml.stats.kge(), 'AICc': ml.stats.aicc()}
print(metrics)

