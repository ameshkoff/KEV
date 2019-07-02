# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: GGamov                                             #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

# import libraries -------------------------------------------

import numpy as np
import re

# basic preprocessing ----------------------------------------
    
def eq_preproc(st_coeff_data, con_data, type_con, lg_k_data, component_name_for_yields):

    # matrix of stoich coeff with formal reactions added
    st_coeff_matrix = st_coeff_data.to_numpy()
    formal_matrix = np.eye(np.shape(st_coeff_matrix)[1], dtype = int)
    st_coeff_matrix = np.vstack((formal_matrix, st_coeff_matrix))
    
    # list of products and reagents names for further using in output data
    
    st_coeff_data['prod_names'] = ''
        
    for cl in st_coeff_data.drop('prod_names', axis = 1):
        
        st_coeff_data['prod_names'] = np.where(st_coeff_data[cl] > 0,
                                               st_coeff_data['prod_names'] + '+' + st_coeff_data[cl].apply(str) + cl,
                                               st_coeff_data['prod_names'])
        
        st_coeff_data['prod_names'] = st_coeff_data['prod_names'].replace({r'(\+)1([a-zA-Z])' : r'\1\2'}, regex = True)
        st_coeff_data['prod_names'] = st_coeff_data['prod_names'].replace(to_replace = r'^\+', value = '', regex = True)
        
    # product names lists : full and base components only
    
    prod_names_con = list(con_data.columns)
    prod_names = prod_names_con + st_coeff_data['prod_names'].tolist()
    
    # creating the vector of equilibrium constants including the formal reactions
    lg_k = (np.vstack((np.zeros((np.shape(st_coeff_matrix)[1], 1)), lg_k_data.to_numpy().astype(float))))
    
    # checking the consistency of reagent names in different sheets    
    if prod_names_con != list(st_coeff_data.drop('prod_names', axis = 1)):
        print('Check the consistency of reagent names!')
    
    # concentrations matrix
    
    con_matrix = con_data.to_numpy()    
    print(con_matrix)    
    # creating vector of indices of components with predetermined concentrations
    ign_indices = np.array(type_con.index[type_con == 'eq'])
    
    if component_name_for_yields not in prod_names:
        print('The component name for partition should be among those of basis components')
        
    idx, = np.where(component_name_for_yields == np.array(prod_names_con))
    
    return st_coeff_matrix, prod_names, lg_k, prod_names_con, con_matrix, ign_indices, idx, type_con  