#### independant clusters graph
#########

#library(DiagrammeR)

grViz("

digraph ind_clusters {

node [shape = circle, label = '']
A; B; C

node [shape = box]
1; 2; 3; 4; 5; 6; 7; 8; 9

# edge statements
A -> 1; A -> 2 [weight=1000]; A -> 3;
B -> 4; B -> 5; B -> 6;
C -> 7; C -> 8 [weight=1000]; C -> 9


{ rank = same; 1; 2; 3; 4; 5; 6; 7; 8; 9 }

{ rank = same; A; B; C}

}      
")



