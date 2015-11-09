# get most recent ids
last_id_log_file = "../log/last_id_log.txt"

# log run
sim_run_log_file = "../log/simulation_run_log.txt"

# Progenitor graph config: 
num_nodes = 30
prob_edges = 0.4
progenitor_graph_log_file = "../log/progenitor_graph_log.txt"
progenitor_graph_log_dir = "../log/progenitor_graphs/"

# Evolved graph config:
num_evolved = 3
num_mods = 3
evolved_graph_log_file = "../log/evolved_graph_log.txt"
evolved_graph_log_dir = "../log/evolved_graphs/"

# Data gen config:
parameterization = "linear_gaussian"
sample_size = 400
dataset_log_file = "../log/dataset_log.txt"
dataset_log_dir = "../log/datasets/"

# Data pooling config:
pooled_sample_sizes = c(400, 200, 200)
pooled_data_log_file = "../log/pooled_data_log.txt"
pooled_data_log_dir = "../log/pooled_datasets/"

# Graph search config:
path_to_tetrad_jar = "/Users/lizzie/Dissertation_code/Tetrad-jars/tetrad-5.2.1-3.jar"
search_log_file="../log/search_log.txt"

# recovered graph config:
recovered_graph_log_dir = "../log/recovered_graphs/"
eval_log_file = "../log/eval_log.txt"