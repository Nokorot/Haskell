def check_node(node):
    if node == None:
        return True
    if node.left.data > node.data or node.right.data < node.data:
        return False
    if not chack_node(node.left):
        return False
    return chack_node(node.right)

def check_binary_search_tree_(root):
    if root == None:
        return False
    if root.left.data > root.data:
        return False
    if root.right.data < root.data:
        return False
    if not check_node(root.left):
        return False
    return check_node(root.right)
