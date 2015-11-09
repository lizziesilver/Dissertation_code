import sys

infile1 = sys.argv[1]

nodefile = sys.argv[2]

f = open(infile1,"r")
lines = f.readlines()
f.close()

g = open(nodefile,"r")
glines = g.readlines()
g.close()

name1 = "curr-" + infile1

h = open(name1,"w")
h.write("\n")
bob = False
for line in glines:
  if line=="Graph Nodes:"+"\n":
    bob = True
  if line=="Graph Edges:"+"\n":
    bob = False
  if bob==True:
    h.write(line)

bob = False
for line in lines:
  if line=="Graph Edges:"+"\n":
    bob = True
  if bob==True:
    h.write(line)



h.close()