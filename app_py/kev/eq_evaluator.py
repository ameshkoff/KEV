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
from copy import copy, deepcopy

def eq_calc(max_iter, eps, component_name_for_yields, ser_num, st_coeff_matrix, type_con, lg_k,
            con_matrix, ign_indices, ser_counts, ser_info): # ser_num, ser_counts, ser_info not further used yet!
    
    c_res_out, c_yie_out, g_res_out = [0] * len(con_matrix), [0] * len(con_matrix), [0] * len(con_matrix)
    
    for k in range(np.shape(con_matrix)[0]):
                
        reag_eq_con_matrix = deepcopy(con_matrix[k]) # initial estimation of equilibrium concentrations of reagents
        init_conc = deepcopy(con_matrix[k]) # value for residual calculation in inner function
        
        prod_eq_con_matrix, g_res = inner_eq_calc(reag_eq_con_matrix, max_iter, lg_k, st_coeff_matrix, init_conc, ign_indices, eps)   
                    
        c_res_out[k] += prod_eq_con_matrix[0]
        g_res_out[k] += g_res[0]

    return c_res_out, g_res_out

def inner_eq_calc(reag_eq_con_matrix, max_iter, lg_k, st_coeff_matrix, init_conc, ign_indices, eps): 
    
    lg_k, st_coeff_matrix, con_matrix = deepcopy(lg_k), deepcopy(st_coeff_matrix), deepcopy(init_conc)
    
    # if some equilibrium concentrations are set
    if np.shape(ign_indices)[0] > 0:

        range_start = st_coeff_matrix.shape[1]
        range_end = lg_k.shape[0]
        
        lg_k_upd = np.matmul(st_coeff_matrix[range_start:range_end, ign_indices],
          np.log10(init_conc[ign_indices])) #+ lg_k[range_start:range_end]
        
        lg_k[range_start:range_end] = lg_k_upd.reshape(-1, 1) + lg_k[range_start:range_end]
        
        init_conc = np.delete(init_conc, ign_indices, axis = 0)
        reag_eq_con_matrix = np.delete(reag_eq_con_matrix, ign_indices, axis = 0)
        st_coeff_matrix = np.delete(st_coeff_matrix, ign_indices, axis = 1)

    # start of iterative procedure
    for it in range(max_iter):

        # caclulating the equilibrium concentrations of products
        prod_eq_con_matrix = np.exp(np.transpose(np.array(math.log(10) * np.array(lg_k))) +
                                    np.dot(st_coeff_matrix, np.log(reag_eq_con_matrix)))
            
        # calculating the total concentrations of reagents
        reag_tot_con_matrix_calc = np.transpose(np.dot(np.transpose(st_coeff_matrix), np.transpose(prod_eq_con_matrix)))
            
        # calculating the residuals
        g_res = np.array(reag_tot_con_matrix_calc) - np.array(init_conc)
                            
        # calculating the Jacobi matrices
        jac_matrix = np.dot(np.transpose(st_coeff_matrix), (np.array(st_coeff_matrix) * np.transpose(prod_eq_con_matrix)))   
        
        # new estimation of equilibrium concentrations of reagents
        prev = np.log(reag_eq_con_matrix)
        reag_eq_con_matrix = np.exp(prev - np.transpose(np.dot(np.linalg.inv(jac_matrix), np.transpose(g_res))))
        reag_eq_con_matrix = reag_eq_con_matrix[0]
        error = abs(np.log(reag_eq_con_matrix) - prev)
                
        # checking the convergence
        if np.max(error) < eps:
               
            # if some equilibrium concentrations are set
            if np.shape(ign_indices)[0] > 0:
                prod_eq_con_matrix[:, ign_indices] = con_matrix[ign_indices]
                        
            break

    return prod_eq_con_matrix, g_res   