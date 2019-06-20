# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: GGamov                                             #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

# import libraries -------------------------------------------

import pandas as pd

def eq_output(sep_out, subdir_out, file_out, results_stoich_coeff, lg_k_data, c_inp_out, c_res_out, 
              c_yie_out, component_name_for_yields, g_res_out, comp_name_res):
       
    if file_out != "":
        
        if subdir_out != '':
            subdir_out = '/' + subdir_out
        subdir_out = '../../output' + subdir_out + '/'
        
    file_out = subdir_out + file_out
    
    # output
    with pd.ExcelWriter(file_out, mode = "w") as output: # specify the path!
        results_stoich_coeff.to_excel(output, sheet_name = 'input_stoich_coefficients', index = False)
        lg_k_data.to_excel(output, sheet_name = 'input_k_constants_log10', index = False)
        c_inp_out.to_excel(output, sheet_name = 'input_concentrations', header = None, index = False)
        c_res_out.to_excel(output, sheet_name = 'equilibrium_concentrations', index = False)
        c_yie_out.to_excel(output, sheet_name = component_name_for_yields + '_fractions', index_label = ['rn'])
        g_res_out.to_excel(output, sheet_name = 'percent_error', index = False)
        comp_name_res.to_excel(output, sheet_name = 'component_names', header = None, index = False)