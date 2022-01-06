#include <stdint.h>
#include <syslog.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#define EXAMPLE_ALPHABET_LEN 26

struct example_s {
    int sentinel;
};

struct example_trie_s {
    struct example_trie_s * children[EXAMPLE_ALPHABET_LEN];
    uint8_t character;
    bool leaf;
};


int example_trie_insert (
    struct example_trie_s * trie,
    const uint8_t * const string,
    const uint32_t string_len
)
{
    // end of the string; all done.
    if (0 == string_len) {
        return 0;
    }

    if (EXAMPLE_ALPHABET_LEN <= string[0] - 'a') {
        syslog(LOG_ERR, "%s:%d:%s: invalid character in string", __FILE__, __LINE__, __func__);
        return -1;
    }

    if (NULL == trie->children[string[0] - 'a']) {
        trie->children[string[0] - 'a'] = calloc(1, sizeof(struct example_trie_s));
        if (NULL == trie->children[string[0] - 'a']) {
            syslog(LOG_ERR, "%s:%d:%s: calloc returned NULL", __FILE__, __LINE__, __func__);
            return -1;
        }
    }

    return example_trie_insert(trie->children[string[0] - 'a'], string + 1, string_len - 1);
}


int example_trie_search (
    struct example_trie_s * root,
    const uint8_t * string,
    const uint32_t string_len,
    struct example_trie_s ** result
)
{
    
    // found a match
    if (0 == string_len) {
        *result = root;
        return 0;
    }

    // invalid character
    if (EXAMPLE_ALPHABET_LEN <= string[0] - 'a') {
        syslog(LOG_ERR, "%s:%d:%s: invalid character in string", __FILE__, __LINE__, __func__);
        return -1;
    }

    // this word is not in the trie
    if (NULL == root->children[string[0] - 'a']) {
        return -1;
    }

    return example_trie_search(root->children[string[0] - 'a'], string + 1, string_len - 1, result);
}


int example_trie_print_ (
    struct example_trie_s * trie,
    uint32_t level
)
{
    for (int i = 0; i < EXAMPLE_ALPHABET_LEN; i++) {
        if (NULL != trie->children[i]) {
            printf("%*c\n", level, i + 'a');
            example_trie_print_(trie->children[i], level + 1);
        }
    }

    return 0;
}


int example_trie_print (
    struct example_trie_s * trie
)
{
    return example_trie_print_(trie, 1);
}


int main (
    int argc,
    char const* argv[]
)
{
    int ret = 0;
    struct example_trie_s trie = {0};

    ret = example_trie_insert(&trie, (const uint8_t *)"abcdef", 6);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_trie_insert returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_trie_insert(&trie, (const uint8_t *)"abcddd", 6);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_trie_insert returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_trie_insert(&trie, (const uint8_t *)"abcefg", 6);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_trie_insert returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_trie_insert(&trie, (const uint8_t *)"abcefggg", 8);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_trie_insert returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_trie_insert(&trie, (const uint8_t *)"decdec", 6);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_trie_insert returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    struct example_trie_s * prefix = NULL;
    ret = example_trie_search(&trie, (const uint8_t *)"abc", 3, &prefix);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_trie_search returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    example_trie_print(&trie);
    example_trie_print(prefix);

    return 0;
}
