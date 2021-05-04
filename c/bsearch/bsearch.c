#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct item_s {
    char * key;
    int value;
};

int bsearch_cmp_counter = 0;

int bsearch_cmp (
    const void * a,
    const void * b
)
{

    const struct item_s * const item_a = a;
    const struct item_s * const item_b = b;

    bsearch_cmp_counter += 1;

    return strcmp(item_a->key, item_b->key);

    return 0;
}


int main (
    int argc,
    char const* argv[]
)
{
    // Set up a sorted list of key-value pairs
    struct item_s items[] = {
        {"aaaa", 301},
        {"aaab", 8},
        {"bar", 12},
        {"baz", 42},
        {"foo", 19},
        {"x", 1010}
    };
    uint_fast32_t items_len = sizeof(items) / sizeof(*items);

    // A needle to search for in the haystack; needs to be able to be compared
    // to items in the key-value list using the compare function (bsearch_cmp).
    struct item_s key = {"foo", 0};
    struct item_s * needle = bsearch(
        /* key = */ &key,
        /* haystack = */ items, 
        /* haystack_len = */ items_len,
        /* item size = */ sizeof(*items),
        /* cmp_cb = */ bsearch_cmp
    );
    if (NULL == needle) {
        printf("%s:%d:%s: item not found, cmp called %d times\n",
                __FILE__, __LINE__, __func__, bsearch_cmp_counter);
        return -1;
    }

    printf("%s:%d:%s: found item: %d, cmp called %d times\n",
            __FILE__, __LINE__, __func__, needle->value, bsearch_cmp_counter);
    

    return 0;
}
