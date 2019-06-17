# run --------------------------------------------------------

# in order to read from scv files, uncomment the following 3 code lines and comment 3 subsequent lines 
# in order to read from xlsx file, comment the following 3 code lines and uncomment 3 subsequent lines

# define variables -----------

# input for bunch of csv files
_subdir = "concentrations/ds.3p.2eq"
_sep = ","
_file = "" #"big_ser_test.xlsx" 

# input for xlsx file
#_subdir = "concentrations/ds.5p/xlsx"
#_sep = ";"
#_file = "big_ser_test.xlsx" #"data.xlsx" 

# input for ountput file
subdir_out = "concentrations/ds.3p.2eq"
sep_out = ";"
file_out = "data_res.xlsx" #"big_ser_test_res.xlsx" 
max_iter, eps = 1000, 0.0000001
    
# run loading function ------

st_coeff_data, lg_k_data, con_data, type_con, component_name_for_yields = eq_scripts_load(_sep, _subdir, _file)

# run preprocessing function ------

ser_num, st_coeff_matrix, prod_names, lg_k, prod_names_con,\
con_matrix, ign_indices, idx, ser_counts, ser_info, type_con = eq_preproc(
    st_coeff_data, con_data, type_con, lg_k_data, component_name_for_yields)

con_matrix = np.concatenate(con_matrix, axis=0)

# run calculations ------

c_res_out, g_res_out = eq_calc(
    max_iter, eps, component_name_for_yields, ser_num, st_coeff_matrix, type_con,\
    lg_k, con_matrix, ign_indices, ser_counts, ser_info)

# run postprocessing function ------

c_inp_out, c_res_out, c_yie_out, g_res_out, comp_name_res, results_stoich_coeff = eq_postproc(
    c_res_out, g_res_out, ser_num, ser_info, ser_counts, con_data, st_coeff_data,\
    prod_names, prod_names_con, component_name_for_yields, type_con, ign_indices)

# run writing to excel function ------

eq_output(sep_out, subdir_out, file_out, results_stoich_coeff, lg_k_data, c_inp_out,\
          c_res_out, c_yie_out, component_name_for_yields, g_res_out, comp_name_res)