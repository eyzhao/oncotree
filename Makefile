oncotree_edge_list.txt: tumor_tree.txt
	python tumor_tree_to_edge_list.py -t $< > $@
