################################################################################
get_new_run_ids <- function(last_id_log_file){
	if (file.exists(last_id_log_file)){
		last_run_ids = read.table(last_id_log_file, header=TRUE, sep=",")
	} else {
		last_run_ids = data.frame("run_id"=0, "progenitor_graph_id"=0, 
								  "evolved_graph_id"=0, "dataset_id"=0, 
	 							  "pooled_dataset_id"=0, "search_id"=0,
	 							  "recovered_graph_id"=0, "eval_id"=0)
	}
	new_run_ids = last_run_ids + 1
	return(new_run_ids)
}

################################################################################
log_progenitor <- function(progenitor, new_run_ids, num_nodes, prob_edges,
						   progenitor_graph_log_file, progenitor_graph_log_dir){
	pgid = new_run_ids$progenitor_graph_id
	progenitor_graph_log = data.frame("progenitor_graph_id"=pgid,
								"sim_run_id"=new_run_ids$run_id,
								"num_nodes"=num_nodes,
								"prob_edges"=prob_edges,
								"num_edges"=length(unlist(edges(progenitor))))
	write.table(progenitor_graph_log, progenitor_graph_log_file, sep=",", 
				row.names=FALSE, append=file.exists(progenitor_graph_log_file), 
				col.names = "!"(file.exists(progenitor_graph_log_file)))	
	# save and log an ASCII representation of the graph, and an R object:
	ascii_filename = paste(progenitor_graph_log_dir, "ASCII/progenitor_graph_",
						   pgid, ".txt", sep="")
	r_filename = paste(progenitor_graph_log_dir, "R_objects/progenitor_graph_",
					   pgid, ".R", sep="")						   
	save(progenitor, file=r_filename)
	dput(progenitor, file=ascii_filename)
	return(pgid)
}

################################################################################
log_evolved <- function(evolved_list, new_run_ids, pgid, num_evolved, 
						evolved_graph_log_file){
	# save and log evolved graphs here
	first_ev_id = new_run_ids$evolved_graph_id
	last_ev_id = new_run_ids$evolved_graph_id + num_evolved - 1
	evolved_graph_ids = seq(first_ev_id, last_ev_id)
	
	num_nodes=c()
	num_edges=c()
	mods = c()
	for (i in 1:num_evolved){
		g = evolved_list[[i]][[1]]
		num_nodes=c(num_nodes, numNodes(g))
		num_edges=c(num_edges, length(unlist(edges(g))))
		mods_i = paste(evolved_list[[i]][[2]], collapse="; ")
		mods = c(mods, mods_i)
		ascii_filename = paste(evolved_graph_log_dir, "ASCII/evolved_graph_",
							   evolved_graph_ids[i], ".txt", sep="")
		r_filename = paste(evolved_graph_log_dir, "R_objects/evolved_graph_",
						   evolved_graph_ids[i], ".R", sep="")
		save(g, file=r_filename)
		dput(g, file=ascii_filename)
	}
	
	# todo: add params of evolution process, once those are not hard-coded
	evolved_graph_log = data.frame("evolved_graph_ids" = evolved_graph_ids,
							"progenitor_id" = rep(pgid, num_evolved),
							"sim_run_id" = rep(new_run_ids$run_id, num_evolved),
							"num_nodes"= num_nodes,
							"num_edges"= num_edges,
							"num_mods" = num_mods,
							"mods"= mods)
	write.table(evolved_graph_log, evolved_graph_log_file, sep=",", 
				row.names=FALSE, 
				append=file.exists(evolved_graph_log_file), 
				col.names = "!"(file.exists(evolved_graph_log_file)))
	return(NULL)
}

################################################################################
################################################################################
################################################################################
