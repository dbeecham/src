#define _POSIX_C_SOURCE 201805L
#include <stdlib.h>
#include <libpq-fe.h>

int main() {
    PGconn *conn;
    PGresult *res;
    int nFields;

    conn = PQconnectdb("host=127.11.0.1 user=postgres dbname=plejd_dev");
    if (PQstatus(conn) != CONNECTION_OK) {
        fprintf(stderr, "Connection to database failed: %s", PQerrorMessage(conn));
        exit(1);
    }

    res = PQexec(conn, "BEGIN");
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "BEGIN failed: %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(1);
    }


    res = PQexec(conn, "DECLARE c CURSOR FOR select device_id from gateways");
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        fprintf(stderr, "DECLARE CURSOR failed: %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(1);
    }

    res = PQexec(conn, "FETCH ALL in c");
    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
        fprintf(stderr, "FETCH ALL failed: %s\n", PQerrorMessage(conn));
        PQclear(res);
        PQfinish(conn);
        exit(1);
    }

    nFields = PQnfields(res);
    for (int i = 0; i < PQntuples(res); i++) {
        for (int j = 0; j < nFields; j++) {
            printf("%-15s", PQgetvalue(res, i, j));
            printf("\n");
        }
    }

    PQclear(res);
    res = PQexec(conn, "CLOSE c");
    PQclear(res);
    res = PQexec(conn, "END");
    PQclear(res);

    PQfinish(conn);
    exit(0);

}
