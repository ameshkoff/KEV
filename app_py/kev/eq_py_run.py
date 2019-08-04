# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: GGamov                                             #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

# import libraries -------------------------------------------

import numpy as np

# import functions from scripts

import eq_data
import eq_preproc
import eq_evaluator
import eq_postproc
import eq_writer

# function ----------------------------------------------------

def eq_run(_subdir, _sep = ',', _file = '', _file_out = '', max_iter = 1000, eps = float('1e-07'), if_save = True):
    
    # load data

    st_coeff_data, lg_k_data, con_data, type_con, component_name_for_yields = eq_data.eq_scripts_load(_sep, _subdir, _file)

    # preprocess

    st_coeff_matrix, prod_names, lg_k, prod_names_con,\
    con_matrix, ign_indices, idx, type_con = eq_preproc.eq_preproc(
        st_coeff_data, con_data, type_con, lg_k_data, component_name_for_yields)

    # calculate

    c_res_out, g_res_out = eq_evaluator.eq_calc(
        max_iter, eps, st_coeff_matrix, type_con,\
        lg_k, con_matrix, ign_indices)

    # postprocess

    c_inp_out, c_res_out, c_yie_out, g_res_out, comp_name_res, results_stoich_coeff = eq_postproc.eq_postproc(st_coeff_matrix, con_matrix, idx,\
        c_res_out, g_res_out, con_data, st_coeff_data,\
        prod_names, prod_names_con, component_name_for_yields, type_con, ign_indices)

    # save data if needed

    if if_save is True:    
        eq_writer.eq_output(_sep, _subdir, _file_out, results_stoich_coeff, lg_k_data, c_inp_out,\
                  c_res_out, c_yie_out, component_name_for_yields, g_res_out, comp_name_res)
    
    return c_inp_out, c_res_out, c_yie_out, g_res_out, comp_name_res, results_stoich_coeff
    
    
    