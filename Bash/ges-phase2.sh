for filename in solo_*.csv;
    do
        newname=${filename%.csv}.txt 
        java -Xmx16384m -jar tetradcmd-5.1.0-10.jar -data $filename \
        -datatype continuous -initialgraphtxt \
        justgraph-allnodes_ges_phase1_pooled_${newname:5} \
        -algorithm ges > ges_phase2_${newname:5};
    done

