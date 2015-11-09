import sys

infile1 = sys.argv[1]

f = open(infile1,"r")
lines = f.readlines()
f.close()


nameend = infile1.split("/")[len(infile1.split("/"))-1]


vars = lines[2].split()

namestart = "recovered-graphs/starting-point-for-phase2/false-priors/"

name1 = namestart+nameend

h = open(name1,"w")
h.write("\n")
bob = False
for line in lines:
  if line=="Graph Nodes:"+"\n":
    bob = True
  if line=="Graph Edges: "+"\n":
    bob = False
  if bob==True:
    h.write(line)

h.write("Graph Edges:"+"\n")


for i in range(0, len(vars)-1):
  h.write(str(i+1)+". "+vars[i]+" --> "+vars[i+1]+"\n")




h.close()