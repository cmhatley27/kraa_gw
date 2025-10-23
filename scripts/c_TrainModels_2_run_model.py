#%% select model and load settings/data
import numpy as np
import pandas as pd
import pastas as ps
import os

well = 'dg01'
name = 'warmup_step_proj'

model_dir = 'data/models/'+well+'/'+name+'/'
input_dir = model_dir+'in/'
output_dir = model_dir+'out/'

input_settings = pd.read_csv(model_dir+'input_settings.csv')

eval_method = input_settings.loc[input_settings['setting'] == 'eval_method', 'value'].squeeze()
eval_settings = pd.read_csv(model_dir+'eval_settings.csv')

warmup = input_settings.loc[input_settings['setting'] == 'warmup', 'value'].squeeze()

rch_model = input_settings.loc[input_settings['setting'] == 'rch_model', 'value'].squeeze()
rch_function = input_settings.loc[input_settings['setting'] == 'rch_function', 'value'].squeeze()

gwl = pd.read_csv(input_dir+'gwl.csv', parse_dates = True, index_col='date').squeeze()
precip = pd.read_csv(input_dir+'precip.csv', parse_dates = True, index_col='date').squeeze()
et = pd.read_csv(input_dir+'et.csv', parse_dates = True, index_col='date').squeeze()
pump = pd.read_csv(input_dir+'pump.csv', parse_dates = True, index_col='date').squeeze()

river_gage = input_settings.loc[input_settings['setting'] == 'river_gage', 'value'].squeeze()
if not pd.isna(river_gage):
    stage = pd.read_csv(input_dir+'stage.csv', parse_dates = True, index_col='date').squeeze()
    ps.plots.series(head = gwl, stresses = [precip, et, pump, stage], hist = True)
else:
    ps.plots.series(head = gwl, stresses = [precip, et, pump], hist = True)
    
period_correction = input_settings.loc[input_settings['setting'] == 'period_correction', 'value'].squeeze()

projections = input_settings.loc[input_settings['setting'] == 'projections', 'value'].squeeze()
    
# train model and save outputs for each evaluation set

#loop through evaluation sets 
for i in range(1,len(eval_settings)+1):
    #get input data and dates for each train/test split
    if eval_method == 'sliding_window':
        train_start = eval_settings.loc[i-1, 'start_date']
        train_end = eval_settings.loc[i-1, 'split_date']
        test_start = eval_settings.loc[i-1, 'split_date']
        test_end = eval_settings.loc[i-1, 'end_date']
        
        ml = ps.Model(gwl, name = 'head')
        
    if eval_method == 'blocked_cv':
        train_start = gwl.index[0]
        train_end = gwl.index[-1]
        test_start = eval_settings.loc[i-1, 'holdout_start']
        test_end = eval_settings.loc[i-1, 'holdout_end']
        
        gwl_in = gwl[np.logical_not(np.logical_and(gwl.index >= test_start, gwl.index <= test_end))]
        ml = ps.Model(gwl_in, name = 'head')
        
    if eval_method == 'growing_window':
        train_start = gwl.index[0]
        train_end = eval_settings.loc[i-1, 'split_date']
        test_start = eval_settings.loc[i-1, 'split_date']
        test_end = eval_settings.loc[i-1, 'end_date']
        
        ml = ps.Model(gwl, name = 'head')

    #add other model elements
    #recharge
    
    #get transfer function shape
    if rch_function == 'exponential':
        rf = ps.Exponential()
    if rch_function == 'gamma':
        rf = ps.Gamma()
    if rch_function == 'fourparam':
        rf = ps.FourParam()
        
    #nonlinear recharge
    if rch_model == 'flex':
        rch_stress = ps.RechargeModel(prec = precip, evap = et, 
                                      recharge=ps.rch.FlexModel(interception=False), 
                                      rfunc=rf, 
                                      name = 'recharge')
        ml.add_stressmodel(rch_stress)
        ml.set_parameter('recharge_lp', initial=0.01, vary = False)
        ml.set_parameter('recharge_kv', initial=1.0, vary = False)
        ml.set_parameter('recharge_srmax', initial=250, vary = False)
    #linear recharge
    if rch_model == 'linear':
        rch_stress = ps.RechargeModel(prec = precip, evap = et, 
                                      recharge=ps.rch.Linear(), 
                                      rfunc=rf, 
                                      name = 'recharge')
        ml.add_stressmodel(rch_stress)
        ml.set_parameter('recharge_f', initial = -1, vary = False)
    
    #pumping
    pump_stress = ps.StressModel(stress = pump, rfunc = ps.Hantush(), name = 'irr', up = False, settings = 'well')
    ml.add_stressmodel(pump_stress)
    
    #river stage
    if not pd.isna(river_gage):
        stage_stress = ps.StressModel(stress = stage, rfunc = ps.Gamma(), name = 'stage', settings = 'waterlevel')
        ml.add_stressmodel(stage_stress)
    
    #step
    if period_correction == 'step':
        step = ps.StepModel("2016-10-01", rfunc=ps.One(), name="step")
        ml.add_stressmodel(step)
        
    #noise
    ml.add_noisemodel(ps.noisemodels.ArNoiseModel())
    ml.set_parameter('noise_alpha', initial = 2, vary = True) #sometimes the noise model doesn't calibrate when the initial value is 1, idk why
    
    #solve
    if(warmup == 'TRUE'):
        warmup_end = max(train_start, pd.to_datetime('1984-01-01'))
    else:
        warmup_end = train_start
    ml.solve(tmin = warmup_end, tmax = train_end, freq_obs='30D', report = False)
    
    #save fit metrics
    metrics = {'metric': ['R2', 'RMSE', 'KGE'],
               'value': [ml.stats.rsq(), ml.stats.rmse(), ml.stats.kge()],
               'model_name': name,
               'train_set': i}
    pd.DataFrame(metrics).to_csv(output_dir+'metrics/'+str(i)+'.csv', index = False)
    
    #save parameters
    params = ml.parameters
    params['model_name'] = name
    params['train_set'] = i
    params.to_csv(output_dir+'params/'+str(i)+'.csv')
    
    #save training series
    train_series = ml.get_output_series()
    #filter out the dates in the test period
    train_series = train_series[np.logical_not(np.logical_and(train_series.index >= test_start, train_series.index <= test_end))]
    train_series['model_name'] = name
    train_series['train_set'] = i
    train_series.to_csv(output_dir+'train_series/'+str(i)+'.csv')
    
    #simulate test period and save
    test_series =  pd.DataFrame(ml.simulate(tmin = test_start, tmax = test_end))
    test_series['model_name'] = name
    test_series['train_set'] = i
    test_series.to_csv(output_dir+'test_series/'+str(i)+'.csv')
    
    #save transfer functions
    rch_pulse = pd.DataFrame(ml.get_block_response('recharge'))
    rch_pulse['model_name'] = name
    rch_pulse['train_set'] = i
    rch_pulse.to_csv(output_dir+'transfer_functions/'+str(i)+'_rch_pulse.csv')
    
    rch_integral = pd.DataFrame(ml.get_step_response('recharge'))
    rch_integral['model_name'] = name
    rch_integral['train_set'] = i
    rch_integral.to_csv(output_dir+'transfer_functions/'+str(i)+'_rch_integral.csv')
    
    irr_pulse = pd.DataFrame(ml.get_block_response('irr'))
    irr_pulse['model_name'] = name
    irr_pulse['train_set'] = i
    irr_pulse.to_csv(output_dir+'transfer_functions/'+str(i)+'_irr_pulse.csv')
    
    irr_integral = pd.DataFrame(ml.get_step_response('irr'))
    irr_integral['model_name'] = name
    irr_integral['train_set'] = i
    irr_integral.to_csv(output_dir+'transfer_functions/'+str(i)+'_irr_integral.csv')
    
    if not pd.isna(river_gage):
        stage_pulse = pd.DataFrame(ml.get_block_response('stage'))
        stage_pulse['model_name'] = name
        stage_pulse['train_set'] = i
        stage_pulse.to_csv(output_dir+'transfer_functions/'+str(i)+'_stage_pulse.csv')
        
        stage_integral = pd.DataFrame(ml.get_step_response('stage'))
        stage_integral['model_name'] = name
        stage_integral['train_set'] = i
        stage_integral.to_csv(output_dir+'transfer_functions/'+str(i)+'_stage_integral.csv')
        
    print('set '+str(i)+'/'+str(len(eval_settings))+ ' done!!')
ml.plots.results()
ml.plots.diagnostics()

#run projections
if projections == 'TRUE':
    proj_input_dirs = os.listdir(input_dir+'projections/')
    proj_output_dir = model_dir+'out/projections/'
    for gcm in range(1, len(proj_input_dirs)+1):
        climate_scenario = proj_input_dirs[gcm-1]
        proj_input_dir = input_dir+'projections/'+climate_scenario+'/'
        
        proj_precip = pd.read_csv(proj_input_dir+'precip.csv', parse_dates = True, index_col='date').squeeze()
        proj_et = pd.read_csv(proj_input_dir+'et.csv', parse_dates = True, index_col='date').squeeze()
        proj_pump = pd.read_csv(proj_input_dir+'pump.csv', parse_dates = True, index_col='date').squeeze()
        
        pml = ps.Model(gwl, name = 'head')
        
        if rch_model == 'flex':
            rch_stress = ps.RechargeModel(prec = proj_precip, evap = proj_et, 
                                          recharge=ps.rch.FlexModel(interception=False), 
                                          rfunc=rf, 
                                          name = 'recharge')
            pml.add_stressmodel(rch_stress)
        if rch_model == 'linear':
            rch_stress = ps.RechargeModel(prec = proj_precip, evap = proj_et, 
                                          recharge=ps.rch.Linear(), 
                                          rfunc=rf, 
                                          name = 'recharge')
            pml.add_stressmodel(rch_stress)
            
        pump_stress = ps.StressModel(stress = proj_pump, rfunc = ps.Hantush(), name = 'irr', up = False, settings = 'well')
        pml.add_stressmodel(pump_stress)
        
        if period_correction == 'step':
            step = ps.StepModel("2016-10-01", rfunc=ps.One(), name="step")
            pml.add_stressmodel(step)
        
        pml.add_noisemodel(ps.noisemodels.ArNoiseModel())
        pml.set_parameter('noise_alpha', initial = 2, vary = True) #sometimes the noise model doesn't calibrate when the initial value is 1, idk why
        
        for i in range(1, len(ml.parameters)+1):
            param_sel = ml.parameters.index[i-1]
            param_val = ml.parameters['optimal'][i-1]
            pml.set_parameter(param_sel, initial = param_val, vary = False)
        
        proj_output = pml.get_output_series(tmin = train_start, tmax = '2099-12-31')
        proj_output['scenario'] = climate_scenario
        proj_output.to_csv(proj_output_dir+climate_scenario+'.csv', index_label = 'date')

#%% mess around with model configurations

rch_lin = ps.RechargeModel(prec = precip, evap = et, recharge=ps.rch.Linear(), rfunc=ps.Gamma(), name = 'recharge')
rch_flex = ps.RechargeModel(prec = precip, evap = et, recharge=ps.rch.FlexModel(interception=False), rfunc=ps.Exponential(), name = 'recharge')
rch_pet = ps.RechargeModel(prec = precip, evap = et, recharge=ps.rch.Peterson(), rfunc=ps.Gamma(), name = 'recharge')
pump_stress = ps.StressModel(stress = pump, rfunc = ps.Hantush(), name = 'irr', up = False, settings = 'well')
if not pd.isna(river_gage):
    stage_stress = ps.StressModel(stress = stage, rfunc = ps.FourParam(), name = 'stage', settings = 'waterlevel')

ml = ps.Model(gwl, name = 'head')
ml.add_stressmodel(rch_flex)
ml.set_parameter('recharge_lp', initial=0.01, vary = False)
ml.set_parameter('recharge_kv', initial=1.0, vary = False)
ml.set_parameter('recharge_srmax', initial=250, vary = False)
# ml.set_parameter('recharge_simax', initial=2, vary = False)
# ml.add_stressmodel(rch_lin)
# ml.set_parameter('recharge_f', initial = -1, vary = False)
# ml.set_parameter('constant_d', initial = 0, vary = False)
# ml.set_parameter('recharge_A', pmax = 0.4)
ml.add_stressmodel(pump_stress)
# ml.set_parameter('irr_a', pmax = 10000)
if not pd.isna(river_gage):
    ml.add_stressmodel(stage_stress)
step = ps.StepModel("2016-10-01", rfunc=ps.One(), name="step")
ml.add_stressmodel(step)
ml.add_noisemodel(ps.noisemodels.ArNoiseModel())
ml.set_parameter('noise_alpha', initial = 1.1, vary = True)
ml.solve(tmin = max(train_start, pd.to_datetime('1984-01-01')), tmax = '2024-12-31', report='full', freq_obs='30D')
metrics = {'metric': ['R2', 'RMSE', 'KGE'],
           'value': [ml.stats.rsq(), ml.stats.rmse(), ml.stats.kge()]}
print(metrics)
ml.plots.results()
# ml.plots.diagnostics()

# asdf = ml.solver.prediction_interval(n = 1000)
# ax = ml.plot(figsize=(10, 3))
# ax.fill_between(asdf.index, asdf.iloc[:, 0], asdf.iloc[:, 1], color="lightgray")
# ax.legend(["Observations", "Simulation", "95% Prediction interval"], ncol=3, loc=2)

#%% get plots
ml.plots.results()
ml.plots.diagnostics()
ml.stats.diagnostics()
ml.plots.block_response()
ml.plots.step_response()
ml.get_step_response('recharge').to_csv(output_dir+'rch_step.csv')
ml.get_block_response('recharge').to_csv(output_dir+'rch_block.csv')
ps.rfunc.step()
ml.plots.decomposition()
ml.get_stress('recharge')


