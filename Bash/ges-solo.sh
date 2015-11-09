for filename in solo*.csv;
    do
        java -Xmx16384m -jar tetradcmd-5.1.0-10.jar -data $filename -datatype continuous \
        -algorithm ges > ges_${filename%.csv}.txt;
    done
