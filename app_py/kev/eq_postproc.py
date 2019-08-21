# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: GGamov                                             #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

# import libraries -------------------------------------------

import math
import numpy as np
import pandas as pd
from copy import deepcopy

def eq_postproc(st_coeff_matrix, con_matrix, idx, c_res_out, g_res_out, con_data, 
                st_coeff_data, prod_names, prod_names_con, component_name_for_yields, type_con, ign_indices): 

    c_yie_out = c_res_out * st_coeff_matrix[:, idx[0]] * 100 / con_matrix[:, idx[0]].reshape((len(con_matrix[:, idx[0]]), 1))
    
    # trim series columns if exists
    
    if 'series' in con_data.columns:
        con_data = deepcopy(con_data)
        con_data = con_data.drop('series', 1)

    c_inp_out = con_data.to_numpy()
    
    prod_names_3 = prod_names_con
    
    p_comp = (-np.log(c_res_out) / math.log(10))[:, idx[0]]
    
    if np.shape(ign_indices)[0] > 0:
        
        g_res_out_tmp = np.zeros((len(c_res_out), len(prod_names_con)))
        g_res_out_tmp[:, ~np.isin(np.arange(g_res_out_tmp.shape[1]), ign_indices)] = np.array(g_res_out)
        g_res_out = g_res_out_tmp
    
    y_prod_names = ['p(' + component_name_for_yields + ')'] + prod_names
    y_indexes = [str('S_' + str(i+1)) for i in range(np.shape(con_data)[0])]

    c_yie_out = np.hstack((p_comp.reshape((len(p_comp), 1)), c_yie_out))

    # preparing data for output
    c_res_out = pd.DataFrame(data=np.array(c_res_out), columns = prod_names)
    c_yie_out = pd.DataFrame(data=np.array(c_yie_out), columns = y_prod_names, index = y_indexes)
    c_yie_out = c_yie_out.loc[:, (c_yie_out != 0).any(axis=0)]
    c_inp_out = np.vstack((prod_names_con, c_inp_out))
    c_inp_out = np.vstack((type_con, c_inp_out))
    c_inp_out = pd.DataFrame(data=np.array(c_inp_out))
    
    g_res_out = pd.DataFrame(data=np.array(g_res_out), columns = prod_names_con)
    
    
    results_stoich_coeff = st_coeff_data.drop('name', axis = 1)
    comp_name_res = pd.DataFrame(data=np.array(component_name_for_yields).reshape((1, 1)))
    
    return c_inp_out, c_res_out, c_yie_out, g_res_out, comp_name_res, results_stoich_coeff

