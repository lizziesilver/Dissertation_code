for filename in recovered-graphs/starting-point-for-phase2/justgraph*.txt;
    do
        python code/python/falsepriornetwork.py recovered-graphs/starting-point-for-phase2${filename:42};
    done
