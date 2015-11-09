for filename in generated-data/solo/solo_*.csv;
    do
        newname=${filename#generated-data/solo/solo_};
        bob=${newname%.csv};
        if [[ $bob != *"2000"* ]]
    		then 
	        	java -Xmx16384m -jar code/tetrad-jars/tetradcmd-5.1.0-10.jar \
        		-data generated-data/solo/solo_${bob}.csv \
        		-datatype continuous -initialgraphtxt \
        		recovered-graphs/starting-point-for-phase2/false-priors/justgraph-allnodes_ges_phase1_pooled_${bob}.txt \
        		-algorithm ges > recovered-graphs/phase2/using-false-priors/ges_phase2_${bob}.txt;
        	fi
    done

# recovered-graphs/phase2/using-false-priors


#justgraph-allnodes_ges_phase1_pooled_
#2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-20_den-2.txt

#2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-200_den-2.csv
#solo_
