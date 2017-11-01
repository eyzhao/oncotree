''' tumor_tree_to_edge_list.py

Usage: tumor_tree_to_edge_list.py -t TREE

Options:
    -t --tree TREE      Path to oncotree TXT file
'''

from docopt import docopt
from functools import reduce
import itertools

def terms_to_edges(term_list):
    edge_list = set()
    a, b = itertools.tee(term_list)
    next(b, None)
    return(set(zip(a, b)))

def parse_tumor_tree(path):
    tumor_tree_handle = open(path)
    header = next(tumor_tree_handle).strip().split('\t')

    rows = [line.strip().split('\t')[:6] for line in tumor_tree_handle]
    entries = [
        {
            'type': row[5],
            'edges': terms_to_edges([item for item in row[1:4] if item.strip()]),
        } for row in rows
    ]

    edges = set.union(*[term['edges'] for term in entries])

    return(edges)

if __name__ == '__main__':
    args = docopt(__doc__)

    edge_list = parse_tumor_tree(args['--tree'])
    print('\t'.join(['parent', 'child']))
    for edge in edge_list:
        print('\t'.join(edge))
