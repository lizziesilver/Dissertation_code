for filename in ges_phase1_pooled*.txt;
    do
        python justgraph.py $filename ges_solo_${filename:18};
    done
