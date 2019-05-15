# importing the libraries required
from copy import copy, deepcopy
import math
import numpy as np
import pandas as pd
from collections import Counter

# input section
# reading the file containing the input data
excel_file = r" " # specify the path!
st_coeff_data = pd.read_excel(excel_file, sheet_name = 'input_stoich_coefficients')
con_data = pd.read_excel(excel_file, sheet_name = 'input_concentrations', header = 1, skiprows = 0)
component_name_for_yields = np.array(pd.read_excel(excel_file, sheet_name = 'component_names', header = None, skiprows = 0))

#checking if there are several series
if 'ser_num' in con_data.columns:
    ser_info = con_data['ser_num'].to_numpy()
    ser_unique = np.unique(ser_info)
    ser_num = np.shape(np.unique(ser_info))[0]
    ser_dict = Counter(ser_info)
    ser_counts = [0] * ser_num
    for i in range(ser_num):
        ser_counts[i] = ser_dict[ser_unique[i]]
    ser_ind = [0] * ser_num
    ser_skip = [0] * ser_num
    for i in range(ser_num):
        ser_ind[i] = np.sum(ser_counts[:i + 1])
        ser_skip[i] = int(np.sum(ser_counts[:i]))
else:
    ser_num = 1
    ser_counts = [0] * ser_num
    ser_skip = [0] * ser_num
    ser_counts[0], ser_skip[0] = np.shape(con_data)[0], 0

# creating the stoichiometric matrix including the formal reactions
st_coeff_matrix = pd.read_excel(excel_file, sheet_name = 'input_stoich_coefficients', header = None, skiprows = 1).to_numpy()
formal_matrix = np.eye(np.shape(st_coeff_matrix)[1], dtype = int)
st_coeff_matrix = np.vstack((formal_matrix, st_coeff_matrix))

# creating the list of products and reagents names for further using in output data
prod_names = [str()] * np.shape(st_coeff_matrix)[0]

for i in range(np.shape(st_coeff_matrix)[0]):
    for j in range(np.shape(st_coeff_matrix)[1]):
        if st_coeff_matrix[i][j] != 0:
            prod_names[i] += '{:-d}'.format(st_coeff_matrix[i][j]) + st_coeff_data.columns[j] + '+'
for i in range(np.shape(prod_names)[0]):
    prod_names[i] = prod_names[i].replace('1', '')[:-1]
            
# creating the vector of equilibrium constants including the formal reactions
lg_k = (np.vstack((np.zeros((np.shape(st_coeff_matrix)[1], 1)),
                   pd.read_excel(excel_file, sheet_name = 'input_k_constants_log10', header = None, skiprows = 1).to_numpy())))

# checking the consistency of reagent names in different sheets
prod_names_2 = [str()] * np.shape(st_coeff_matrix)[1]
for i in range(np.shape(st_coeff_matrix)[1]):
    prod_names_2[i] = con_data.columns[i]

if prod_names_2 != prod_names[:np.shape(prod_names_2)[0]]:
    print('Check the consistency of reagent names!')

# creating the concentrations matrix
con_matrix = [0] * ser_num
for i in range(ser_num):
    con_matrix[i] = (np.array(pd.read_excel(excel_file, sheet_name = 'input_concentrations', 
                                            header = None, skiprows = 2 + ser_skip[i], nrows = ser_counts[i]).loc[:, con_data.columns != 'ser_num']))

# reading the type of concentrations
type_con = pd.read_excel(excel_file, sheet_name = 'input_concentrations', header = None, nrows = 1).iloc[0,:]
ign_indices = np.array(type_con.index[type_con == 'eq'])

check = component_name_for_yields[0] in prod_names
if check != True:
    print('The component name for partition should be among those of basis components')
idx, = np.where(prod_names == component_name_for_yields[0])

# End of input section for equilibrium concentration calculations.

# Nominate calc parameters. 
max_iter, eps = 1000, 0.0000001

prod_eq_con_matrix = [0] * np.shape(st_coeff_matrix)[0] # create equilibrium concentration matrix
reag_tot_con_matrix_calc = [0] * np.shape(st_coeff_matrix)[1] # create total concentrations of reagents calculated
jac_matrix = [[0] * np.shape(st_coeff_matrix)[1] for j in range(np.shape(st_coeff_matrix)[1])] # create Jacobi matrix
error = [0] * np.shape(st_coeff_matrix)[1] # create vector of errors
prev = [0] * np.shape(st_coeff_matrix)[1] # create vector of previous estimation of equilibrium reagents concentrations
yields = [0] * np.shape(st_coeff_matrix)[0] #create yields vector
results_conc = [0] * ser_num
results_yields = [0] * ser_num
g_res = [0] * ser_num
for i in range(ser_num):
    results_conc[i] = [[0] * np.shape(st_coeff_matrix)[0] for j in range(np.shape(con_matrix[i])[0])] # create resulting concentrations matrix 
    results_yields[i] = [[0] * np.shape(st_coeff_matrix)[0] for j in range(np.shape(con_matrix[i])[0])] # create resulting yields matrix
    g_res[i] = [[0] * np.shape(st_coeff_matrix)[1] for j in range(np.shape(con_matrix[i])[0])] # create matrix of residuals

lg_k_copy, st_coeff_matrix_copy, con_matrix_copy = deepcopy(lg_k), deepcopy(st_coeff_matrix), deepcopy(con_matrix)

# start of calculations
for s in range(ser_num):
    
    for k in range(np.shape(con_matrix[s])[0]):
        
        # if some equilibrium concentrations are set
        if np.shape(ign_indices)[0] > 0:
            
            lg_k_app, st_coeff_matrix_app, con_matrix_app = lg_k, st_coeff_matrix, con_matrix
            
            for i in range(np.shape(lg_k)[0]):
                for j in range(np.shape(ign_indices)[0]):
                    lg_k_app[i] = np.array(lg_k[i]) + np.array(st_coeff_matrix[i][ign_indices[j]]) * np.log(con_matrix[s][k][ign_indices[j]]) / math.log(10)
            
            lg_k_app = np.delete(lg_k_app, ign_indices, axis = 0)
            
            con_matrix_app[s] = np.delete(con_matrix_app[s], ign_indices, axis = 1)
            st_coeff_matrix_app = np.delete(st_coeff_matrix_app, ign_indices, axis = 0)
            st_coeff_matrix_app = np.delete(st_coeff_matrix_app, ign_indices, axis = 1)
        
            lg_k, st_coeff_matrix, con_matrix = lg_k_app, st_coeff_matrix_app, con_matrix_app
            
        reag_eq_con_matrix = deepcopy(con_matrix[s][k]) # initial estimation of equilibrium concentrations of reagents
        
        # start of iterative procedure
        for it in range(max_iter):
        
            # caclulating the equilibrium concentrations of products
            prod_eq_con_matrix = np.exp(np.transpose(np.array(math.log(10) * np.array(lg_k))) + np.dot(st_coeff_matrix, np.log(np.array(reag_eq_con_matrix))))
            
            # calculating the total concentrations of reagents
            reag_tot_con_matrix_calc = np.transpose(np.dot(np.transpose(st_coeff_matrix), np.transpose(prod_eq_con_matrix)))
            
            # calculating the residuals
            g_res[s][k] = np.array(reag_tot_con_matrix_calc) - np.array(con_matrix[s][k])
                        
            # calculating the Jacobi matrices
            jac_matrix = np.dot(np.transpose(st_coeff_matrix), (np.array(st_coeff_matrix) * np.transpose(prod_eq_con_matrix)))
            prod_eq_con_matrix = np.transpose(prod_eq_con_matrix)
            
            # new estimation of equilibrium concentrations of reagents
            prev = np.log(reag_eq_con_matrix)
            reag_eq_con_matrix = np.exp(prev - np.transpose(np.dot(np.linalg.inv(jac_matrix), np.transpose(g_res[s][k]))))
            reag_eq_con_matrix = reag_eq_con_matrix[0]
            error = abs(np.log(reag_eq_con_matrix) - prev)
        
            # checking the convergence
            if np.max(error) < eps:
            
                # if some equilibrium concentrations are set
                if np.shape(ign_indices)[0] > 0:
                    lg_k, st_coeff_matrix, con_matrix = deepcopy(lg_k_copy), deepcopy(st_coeff_matrix_copy), deepcopy(con_matrix_copy)
                    for j in range(np.shape(ign_indices)[0]):
                        prod_eq_con_matrix = np.insert(np.transpose(prod_eq_con_matrix)[0], ign_indices[j], con_matrix[s][k][ign_indices[j]])
                        prod_eq_con_matrix = prod_eq_con_matrix.reshape((len(prod_eq_con_matrix), 1))
                
           
                # calculating the yields                
                for i in range(np.shape(prod_eq_con_matrix)[0]):
                    yields[i] = np.transpose(prod_eq_con_matrix[i]) * st_coeff_matrix[i][idx[0]] * 100 / con_matrix[s][k][idx[0]]
            
                # it is just a crooked nail aimed to solve the problem with array dimensions. Got no idea why it happens.
                if len(np.shape(np.array(yields))) > 1:
                    for i in range(len(yields)):
                        yields[i] = float(yields[i])
                        
                results_conc[s][k] = np.transpose(prod_eq_con_matrix)[0]
                results_yields[s][k] = np.array(yields)    
                break

    # end of calculations

    g_res[s] = np.array(g_res[s]).reshape((np.shape(con_matrix[s])[0], np.shape(con_matrix[s])[1] - np.shape(ign_indices)[0]))
    
    # if some equilibrium concentrations are set
    if np.shape(ign_indices)[0] > 0:
        for j in range(np.shape(ign_indices)[0]):
            g_res[s] = np.insert(g_res[s], ign_indices[j], np.zeros((1, np.shape(con_matrix[s])[0]), dtype = int), axis = 1)

# making the last preparation to the following section
c_res_out = [[0] * np.shape(lg_k)[0] for j in range(np.shape(con_data)[0])]
c_yie_out = [[0] * np.shape(lg_k)[0] for j in range(np.shape(con_data)[0])]
g_res_out = [[0] * np.shape(st_coeff_matrix)[1] for j in range(np.shape(con_data)[0])]
c_inp_out = [[0] * np.shape(st_coeff_matrix)[1] for j in range(np.shape(con_data)[0])]
m = 0

for s in range(ser_num):
    for i in range(ser_counts[s]):
        for j in range(np.shape(lg_k)[0]):
            c_res_out[m][j] = results_conc[s][i][j]
            c_yie_out[m][j] = results_yields[s][i][j]
        m += 1
m = 0
for s in range(ser_num):
    for i in range(ser_counts[s]):
        for j in range(np.shape(st_coeff_matrix)[1]):
            g_res_out[m][j] = g_res[s][i][j]
            c_inp_out[m][j] = con_matrix[s][i][j]
        m += 1
        
prod_names_3 = prod_names_2

if 'ser_num' in con_data.columns:
    c_res_out = np.hstack((c_res_out, ser_info.reshape((len(ser_info), 1))))
    c_yie_out = np.hstack((c_yie_out, ser_info.reshape((len(ser_info), 1))))
    g_res_out = np.hstack((g_res_out, ser_info.reshape((len(ser_info), 1))))
    c_inp_out = np.hstack((c_inp_out, ser_info.reshape((len(ser_info), 1))))
    prod_names = prod_names + ['ser_num']
    prod_names_2 = prod_names_2 + ['ser_num']

y_prod_names = ['p(' + component_name_for_yields[0][0] + ')'] + prod_names
y_indexes = [str()] * np.shape(con_data)[0]
for i in range(np.shape(con_data)[0]):
    y_indexes[i] = 'S_' + str(i+1)
p_comp = [[0] * 1 for j in range(np.shape(con_data)[0])]
for i in range(np.shape(con_data)[0]):
    p_comp[i][0] = -math.log(c_res_out[i][idx[0]]) / math.log(10)
c_yie_out = np.hstack((p_comp, c_yie_out))

# preparing data for output
c_res_out = pd.DataFrame(data=np.array(c_res_out), columns = prod_names)
c_yie_out = pd.DataFrame(data=np.array(c_yie_out), columns = y_prod_names, index = y_indexes)
c_yie_out = c_yie_out.loc[:, (c_yie_out != 0).any(axis=0)]
results_stoich_coeff = pd.DataFrame(data=np.array(st_coeff_matrix[np.shape(st_coeff_matrix)[1]:]), columns = prod_names_3)
results_lg_k = pd.DataFrame(data=np.array(lg_k[np.shape(st_coeff_matrix)[1]:]), columns = ['log10.K.'])
c_inp_out = np.vstack((prod_names_2, c_inp_out))
c_inp_out = np.vstack((type_con, c_inp_out))
c_inp_out = pd.DataFrame(data=np.array(c_inp_out))
g_res_out = pd.DataFrame(data=np.array(g_res_out), columns = prod_names_2)
comp_name_res = pd.DataFrame(data=component_name_for_yields)

# output
with pd.ExcelWriter(r" ", mode = "w") as output: # specify the path!
    results_stoich_coeff.to_excel(output, sheet_name = 'input_stoich_coefficients', index = False)
    results_lg_k.to_excel(output, sheet_name = 'input_k_constants_log10', index = False)
    c_inp_out.to_excel(output, sheet_name = 'input_concentrations', header = None, index = False)
    c_res_out.to_excel(output, sheet_name = 'equilibrium_concentrations', index = False)
    c_yie_out.to_excel(output, sheet_name = component_name_for_yields[0][0] + '_fractions', index_label = ['rn'])
    g_res_out.to_excel(output, sheet_name = 'percent_error', index = False)
    comp_name_res.to_excel(output, sheet_name = 'component_names', header = None, index = False)