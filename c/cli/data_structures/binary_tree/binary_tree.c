#include <stdio.h>
#include <stdlib.h>

#define bool int
#define false 0
#define true 1

struct tree_tree { // {{{
    int value;
    struct tree_tree * left;
    struct tree_tree * right;
}; // }}}

typedef struct tree_tree * tree;

tree new_tree(int value, tree left, tree right) { // {{{
    tree l = malloc(sizeof(struct tree_tree));
    l->value = value;
    l->left = left;
    l->right = right;

    return l;
} // }}}

int tree_empty(tree a) { // {{{
    return (NULL == a);
} // }}}

bool tree_eq(tree a, tree b) { // {{{
    // both empty
    if (tree_empty(a) && tree_empty(b)) return true;

    // one empty
    if (tree_empty(a) || tree_empty(b)) return false;

    // same value
    else if (a->value == b->value) {
        // check children as well
        return (tree_eq(a->left, b->left) && tree_eq(a->right, b->right));
    }

    // different values
    return false;
} // }}}

int tree_sum(tree xs) { // {{{
    if (tree_empty(xs)) return 0;
    return xs->value + tree_sum(xs->left) + tree_sum(xs->right);
} // }}}

void tree_delete(tree xs) { // {{{
    if (!tree_empty(xs->left)) {
        tree_delete(xs->left);
    }
    if (!tree_empty(xs->right)) {
        tree_delete(xs->right);
    }

    free(xs);
} // }}}

void tree_clear(bool (*func)(tree), tree xs) { // {{{
    if (!tree_empty(xs->left)) {
        if (func(xs->left)) {
            tree_delete(xs->left);
            xs->left = NULL;
        } else {
            tree_clear(func, xs->left);
        }
    }

    if (!tree_empty(xs->right)) {
        if (func(xs->right)) {
            tree_delete(xs->right);
            xs->right = NULL;
        } else {
            tree_clear(func, xs->right);
        }
    }
} // }}}

bool tree_zero_sum(tree xs) { // {{{
    return tree_sum(xs) == 0;
} // }}}

void tree_clear_zero_sum(tree xs) { // {{{
    tree_clear(tree_zero_sum, xs);
} // }}}

void tree_print(tree xs) { // {{{
    if (tree_empty(xs)) {
        printf("null");
        return;
    }

    printf("%i {", xs->value);
    tree_print(xs->left);
    printf(", ");
    tree_print(xs->right);
    printf("}");
} // }}}

void print_int(int x) { // {{{
    printf("%i\n", x);
} // }}}

void tree_iterative_traverse(void (*func)(int), tree xs) { // {{{
    if (!tree_empty(xs->left)) {
        tree_iterative_traverse(func, xs->left);
    }
    func(xs->value);
    if (!tree_empty(xs->right)) {
        tree_iterative_traverse(func, xs->right);
    }
} // }}}

int main(int argc, const char *argv[])
{
    tree l = new_tree(10, 
                      new_tree(8,
                               new_tree(6, 
                                        new_tree(-4, NULL, NULL), 
                                        new_tree(-2, NULL, NULL)), 
                               NULL), 
                      new_tree(8,
                               new_tree(6, 
                                        NULL, 
                                        NULL),
                               NULL));
    tree_print(l);
    printf("\n\n");
    tree_iterative_traverse(print_int, l);

    tree_clear_zero_sum(l);
    tree_print(l);
    printf("\n\n");
    printf("%i\n", tree_sum(l->left->left));

    return 0;
}
