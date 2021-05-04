#include <stdint.h>
#include <stdio.h>

#define HASH_TABLE_ENTRY_KEY_LEN 128
#define HASH_TABLE_LEN 32


struct hash_table_entry_s {
    const uint8_t key[HASH_TABLE_ENTRY_KEY_LEN];
    void * value;
};

struct hash_table_s {
    struct hash_table_entry_s entries[HASH_TABLE_LEN];
    uint_fast32_t length;
};


int main (
    int argc,
    char const* argv[]
)
{

    struct hash_table_entry_s[
    
    return 0;
}
