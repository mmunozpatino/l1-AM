# Detailed description of the tree data structure and related functions
# ---------------------------------------------------------------------
#
# We will use structures to implement the tree datatype. The tree is formed by
# nodes. The code of a node
#
#  node <- new.node(name="Temperature", branches=c("Good", "Bad"))
#
# creates a node called 'Temperature' which has the branches: "Good" and
# "Bad". Before we will build the tree we create other node
#
#  node2 <- new.node("Rain", c("True", "False"))
#
# Note: You can omit the names of the parameters 'name' and 'branches'
#
# A tree is a list with the following components:
#	- nodesCount: numbers of nodes
#   - nodes: is a list of nodes, whose first element is the root node of tree
#	Each node are connected to his father by the 'parentId' value, except the
#	root node whose value parentId = NULL
#
# Now we can build a tree, for this we need choose which node will be the root
# of tree. The code
#
#  tree <- new.tree(node)
#
# Here we created a tree whose root is the node with attribute
# "Temperature". For definition branches of a tree we use the function
# add.subtree(). For example, we suppose there is a branch between node to node2
# for the value "Good" of node. The code to represent that is
#
# tree <- add.subtree(tree=tree, subtree=node2, parentId=node$branches[["Good"]])
#
# Note: You can also add a node a leaf or a tree.
#
# There are two nodes in a decision tree: decision node and leaf node. The
# first one are used to decide how classify a example. The last one are used
# to assign a class (or label) a example. A leaf node is different a decision
# node by two thing: (1) the leaf node does not have a 'name' and
# 'branches'. (2) The leaf node has a 'label'. The code to create a leaf node
# is:
#
# leaf <- new.leaf("Yes")
#
#
# Now we show as to use the tree data structure. For this, we are going to
# build the decision tree at figure 3.1 in the book "Machine Learning" by Tom
# Mitchell. But before, a bit of staff to use the data structure:

source("tree.R")

# The tree has three attributes. for each of them are representing by a node.
#
# The attribute Outlook
n0 <- new.node("Outlook", c("Sunny", "Overcast", "Rain"))

# The attribute Humidity
n1 <- new.node("Humidity", c("High", "Normal"))

# The attribute Wind
n2 <- new.node("Wind", c("Strong", "Weak"))

# Moreover, There also are two labels. These are 'leaf node'.
l.yes <- new.leaf("Yes")
l.no <- new.leaf("No")

# Now we build the tree. For that, we call the function new.tree() pass it as
# parameter the root of the tree


tree <- new.tree(n0)

# The following lines show how to build the branches of the tree
tree <- add.subtree(tree, n1, n0$branches[["Sunny"]])
tree <- add.subtree(tree, n2, n0$branches[["Rain"]])
tree <- add.subtree(tree, l.yes, n0$branches[["Overcast"]])
tree <- add.subtree(tree, l.no, n1$branches[["High"]])
tree <- add.subtree(tree, l.yes, n1$branches[["Normal"]])
tree <- add.subtree(tree, l.no, n2$branches[["Strong"]])
tree <- add.subtree(tree, l.yes, n2$branches[["Weak"]])


# Finally, we print the tree for screen

plot.tree(tree)
cat("_________________________________\n")



# There is other way to build a tree. This way, first is added nodes to a subtree
# and later is added this subtree to an incomplete tree. Follow the previous example


subtree <- new.tree(n2)
subtree <- add.subtree(subtree,l.no,n2$branches[["Strong"]])
subtree <- add.subtree(subtree,l.yes,n2$branches[["Weak"]])

plot.tree(subtree)
cat("_________________________________\n")

# Now create an incomplete tree adding nodes

incomplete.tree <- new.tree(n0)

# The following lines show how to build the branches of the tree
incomplete.tree <- add.subtree(incomplete.tree, n1, n0$branches[["Sunny"]])
incomplete.tree <- add.subtree(incomplete.tree, n2, n0$branches[["Rain"]])
incomplete.tree <- add.subtree(incomplete.tree, l.yes, n0$branches[["Overcast"]])
incomplete.tree <- add.subtree(incomplete.tree, l.no, n1$branches[["High"]])
incomplete.tree <- add.subtree(incomplete.tree, l.yes, n1$branches[["Normal"]])

plot.tree(incomplete.tree)
cat("_________________________________\n")

#Finally add the subtree to the incomplete tree

final.tree <- add.subtree(incomplete.tree,subtree,n0$branches[["Rain"]])

plot.tree(final.tree)
cat("_________________________________\n")