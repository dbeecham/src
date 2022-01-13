#include <stdio.h>
#include <string.h>
#include <sqlite3.h>

#define syslog(severity, format, ...) printf(format "\n", __VA_ARGS__)

struct sqlite0_s {
    sqlite3 * db;
};


void sqlite0_sqlite_update_cb (
    void * user_data,
    int action, // SQLITE_INSERT, SQLITE_DELETE, or SQLITE_UPDATE
    const char * const database,
    const char * const table,
    sqlite3_int64 rowid
)
{

    int ret = 0;

    syslog(LOG_DEBUG, "%s:%d:%s: hi!", __FILE__, __LINE__, __func__);
    

    return;
}


int sqlite0_sqlite_commit_cb (
    void * user_data
)
{
    // may not modify the database in any way. is not reentrant. if this
    // returns non-zero, the commit is rollbacked.
    syslog(LOG_DEBUG, "%s:%d:%s: hi!", __FILE__, __LINE__, __func__);
    return 0;
}


int sqlite0_init_schema_migrate_full (
    struct sqlite0_s * sqlite0
)
{
    char * err = NULL;
    int ret = 0;
    const char schema[] = 
        "begin;"
        "create table devices ("
            "deviceid text not null check (length(deviceid)==12),"
            "firmware text check(length(firmware) < 128),"
            "primary key (deviceid)"
        ") without rowid;"
        "pragma user_version=1;"
        "commit;"
        ;


    ret = sqlite3_exec(
        /* db = */ sqlite0->db,
        /* sql = */ schema,
        /* cb = */ NULL,
        /* user_data = */ NULL,
        /* err = */ &err
    );
    if (SQLITE_OK != ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite3_exec returned %d: %s",
            __FILE__, __LINE__, __func__, ret, err);
        return -1;
    }

    return 0;
}


int sqlite0_init_sqlite_migrate (
    struct sqlite0_s * sqlite0
)
{
    int ret = 0;
    sqlite3_stmt * stmt;


    ret = sqlite3_prepare_v3(
        /* db = */ sqlite0->db,
        /* sql = */ "pragma user_version;",
        /* sql_len = */ strlen("pragma user_version;"),
        /* flags = */ SQLITE_PREPARE_NORMALIZE,
        /* &stmt = */ &stmt,
        /* &sql_end = */ NULL
    );
    if (SQLITE_OK != ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite3_prepare_v2 returned %d: %s",
            __FILE__, __LINE__, __func__, ret, sqlite3_errmsg(sqlite0->db));
        return -1;
    }

    ret = sqlite3_step(stmt);
    if (SQLITE_ROW != ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite3_step returned %d: %s",
                __FILE__, __LINE__, __func__, ret, sqlite3_errmsg(sqlite0->db));
        sqlite3_finalize(stmt);
        return -1;
    }

    const int sqlite3_user_version = sqlite3_column_int(stmt, 0);

    ret = sqlite3_finalize(stmt);
    if (SQLITE_OK != ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite3_finalize returned %d: %s",
            __FILE__, __LINE__, __func__, ret, sqlite3_errmsg(sqlite0->db));
        return -1;
    }

    if (0 == sqlite3_user_version) {
        syslog(LOG_INFO, "%s:%d:%s: doing full schema migration", __FILE__, __LINE__, __func__);

        ret = sqlite0_init_schema_migrate_full(sqlite0);
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: sqlite0_init_schema_migrate_full returned -1", __FILE__, __LINE__, __func__);
            return -1;
        }

        return 0;
    }

    // current schema version
    if (1 == sqlite3_user_version) {
        return 0;
    }

    // if we reach this point, the sqlite3 schema version is too new for us to
    // handle; don't touch it!
    syslog(LOG_ERR, "%s:%d:%s: sqlite3 schema version is too new; giving up", __FILE__, __LINE__, __func__);
    return -1;
}


int sqlite0_init_sqlite (
    struct sqlite0_s * sqlite0
)
{

    int ret = 0;
    char * err = NULL;
    void * prev_user_data = NULL;

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

    // enable foreign keys
    ret = sqlite3_exec(
        /* db = */ sqlite0->db,
        /* sql = */ "pragma foreign_keys=1;",
        /* cb = */ NULL,
        /* user_data = */ NULL,
        /* err = */ &err
    );
    if (SQLITE_OK != ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite3_exec returned %d: %s",
            __FILE__, __LINE__, __func__, ret, err);
        return -1;
    }
    
    ret = sqlite0_init_sqlite_migrate(sqlite0);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite0_init_sqlite_migrate returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    // add an update hook. not called on "without rowid;" tables.
    prev_user_data = sqlite3_update_hook(
        /* db = */ sqlite0->db,
        /* cb = */ sqlite0_sqlite_update_cb,
        /* user_data = */ sqlite0
    );
    if (NULL != prev_user_data) {
        syslog(LOG_ERR, "%s:%d:%s: overwrote previous sqlite3 update hook", __FILE__, __LINE__, __func__);
        return -1;
    }

    // and a commit hook
    prev_user_data = sqlite3_commit_hook(
        /* db = */ sqlite0->db,
        /* cb = */ sqlite0_sqlite_commit_cb,
        /* user_data = */ sqlite0
    );
    if (NULL != prev_user_data) {
        syslog(LOG_ERR, "%s:%d:%s: overwrote previous sqlite3 update hook", __FILE__, __LINE__, __func__);
        return -1;
    }

    return 0;
}


int sqlite0_init (
    struct sqlite0_s * sqlite0
)
{

    int ret = 0;

    ret = sqlite0_init_sqlite(sqlite0);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite0_init_sqlite returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

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


int sqlite0_test_select_stmt (
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


int sqlite0_test_insert_stmt (
    struct sqlite0_s * sqlite0
)
{

    int ret = 0;
    sqlite3_stmt *stmt;
    
    syslog(LOG_DEBUG, "%s:%d:%s: hi!", __FILE__, __LINE__, __func__);

    const char sql[] = 
        "insert into devices(deviceid, firmware) values (?, ?);";

    ret = sqlite3_prepare_v3(
        /* db = */ sqlite0->db,
        /* sql = */ sql,
        /* sql_len = */ sizeof(sql) - 1,
        /* flags = */ SQLITE_PREPARE_NORMALIZE,
        /* stmt = */ &stmt,
        /* sql_tail = */ NULL
    );
    if (SQLITE_OK != ret) {
        printf("%s:%d:%s: sqlite3_prepare_v3 returned %d\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    // bind deviceid
    ret = sqlite3_bind_text(
        /* stmt = */ stmt,
        /* index = */ 1,
        /* text = */ "012345678901",
        /* text_len = */ 12,
        /* destructor = */ SQLITE_STATIC
    );
    if (SQLITE_OK != ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite3_bind_text returned %d: %s",
            __FILE__, __LINE__, __func__, ret, sqlite3_errmsg(sqlite0->db));
        return -1;
    }

    // bind firmware
    ret = sqlite3_bind_text(
        /* stmt = */ stmt,
        /* index = */ 2,
        /* text = */ "hello",
        /* text_len = */ 5,
        /* destructor = */ SQLITE_STATIC
    );
    if (SQLITE_OK != ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite3_bind_text returned %d: %s",
            __FILE__, __LINE__, __func__, ret, sqlite3_errmsg(sqlite0->db));
        return -1;
    }
    
    // step sqlite
    ret = sqlite3_step(stmt);
    if (SQLITE_DONE != ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite3_step returned %d: %s", __FILE__, __LINE__, __func__, ret, sqlite3_errmsg(sqlite0->db));
        return -1;
    }


    ret = sqlite3_finalize(stmt);
    if (SQLITE_OK != ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite3_finalize returned %d: %s", __FILE__, __LINE__, __func__, ret, sqlite3_errmsg(sqlite0->db));
        return -1;
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

    ret = sqlite0_test_insert_stmt(&sqlite0);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: sqlite0_test_insert_stmt returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = sqlite0_test_select_stmt(&sqlite0);
    if (-1 == ret) {
        printf("%s:%d:%s: sqlite0_test_exec returned %d\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    ret = sqlite3_close_v2(sqlite0.db);
    if (SQLITE_OK != ret) {
        printf("%s:%d:%s: sqlite3_close returned %d\n", __FILE__, __LINE__, __func__, ret);
    }

    return 0;
    (void)argv;
    (void)argc;
}
