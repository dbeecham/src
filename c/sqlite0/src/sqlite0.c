#include <stdio.h>
#include <string.h>
#include <sqlite3.h>

struct sqlite0_s {
    sqlite3 * db;
};

int sqlite0_init (
    struct sqlite0_s * sqlite0
)
{

    int ret = 0;

    ret = sqlite3_open_v2(
        /* path = */ "db.sqlite",
        /* db = */ &sqlite0->db,
        /* flags = */ SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,
        /* vfs = */ NULL
    );
    if (SQLITE_OK != ret) {
        printf("%s:%d:%s: sqlite3_open_v2 returned %d\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }

//    ret = sqlite3_close_v2(&sqlite0->db);
//    if (SQLITE_OK != ret) {
//        printf("%s:%d:%s: sqlite3_close_v2 returened %d", __FILE__, __LINE__, __func__, ret);
//        return -1;
//    }

    return 0;
}


int sqlite0_test_exec_cb (
    void * user_data,
    int columns,
    char ** values,
    char ** keys
)
{

    int ret = 0;

    printf("%s:%d:%s: hi\n", __FILE__, __LINE__, __func__);

    printf("%s:%d:%s: columns: %d, key[0]=%s, value[0]=%s\n",
            __FILE__, __LINE__, __func__, columns, keys[0], values[0]);

    return 0;
}


int sqlite0_test_exec (
    struct sqlite0_s * sqlite0
)
{

    int ret = 0;
    char * err = NULL;

    ret = sqlite3_exec(
        /* db = */ sqlite0->db,
        /* sql = */ "select CURRENT_TIMESTAMP;",
        /* callback = */ sqlite0_test_exec_cb,
        /* user_data = */ sqlite0,
        /* err = */ &err
    );
    if (SQLITE_OK != ret) {
        printf("%s:%d:%s: sqlite3_exec: %s\n", __FILE__, __LINE__, __func__, err);
        return -1;
    }

    return 0;
}

int sqlite0_test_stmt (
    struct sqlite0_s * sqlite0
)
{

    int ret = 0;
    sqlite3_stmt *stmt;

    ret = sqlite3_prepare_v3(
        /* db = */ sqlite0->db,
        /* sql = */ "select CURRENT_TIMESTAMP;",
        /* sql_len = */ -1,
        /* flags = */ 0,
        /* stmt = */ &stmt,
        /* sql_tail = */ NULL
    );
    if (SQLITE_OK != ret) {
        printf("%s:%d:%s: sqlite3_prepare_v3 returned %d\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    ret = sqlite3_step(stmt);
    if (SQLITE_ROW == ret) {
        
        int num_cols = sqlite3_column_count(stmt);
        for (int i = 0; i < num_cols; i++) {
            switch (sqlite3_column_type(stmt, i)) {
                case SQLITE3_TEXT:
                    printf("%s:%d:%s: %s=%s\n",
                            __FILE__, __LINE__, __func__,
                            sqlite3_column_name(stmt, i), sqlite3_column_text(stmt, i));
                    break;
                default:
                    printf("%s:%d:%s: switch-case defaulted!\n", __FILE__, __LINE__, __func__);
                    return -1;
            }
        }

    }

    return 0;
}


int main (
    int argc,
    char const* argv[]
)
{
    int ret = 0;
    struct sqlite0_s sqlite0 = {0};

    ret = sqlite0_init(&sqlite0);
    if (-1 == ret) {
        printf("%s:%d:%s: sqlite0_init returned %d\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    ret = sqlite0_test_exec(&sqlite0);
    if (-1 == ret) {
        printf("%s:%d:%s: sqlite0_test_exec returned %d\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    ret = sqlite0_test_stmt(&sqlite0);
    if (-1 == ret) {
        printf("%s:%d:%s: sqlite0_test_exec returned %d\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    ret = sqlite3_close_v2(sqlite0.db);
    if (SQLITE_OK != ret) {
        printf("%s:%d:%s: sqlite3_close returned %d\n", __FILE__, __LINE__, __func__, ret);
    }

    return 0;
}
