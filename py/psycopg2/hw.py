import psycopg2

def batch_insert(cur):
    rows = (
        ('ebbe402c-227f-11eb-b76f-0242ac110002', 1, True, 10),
        ('abbe402c-227f-11eb-b76f-0242ac110002', 1, False, 10),
        ('bbbe402c-227f-11eb-b76f-0242ac110002', 1, True, 10),
        ('cbbe402c-227f-11eb-b76f-0242ac110002', 1, False, 10),
        ('dbbe402c-227f-11eb-b76f-0242ac110002', 1, True, 10),
    )
    args_str = ','.join(cur.mogrify("(%s,%s,%s,%s)", x) for x in rows)
    print("insert into table values " + args_str)

try:
    conn = psycopg2.connect(host='localhost', port='5432', user='postgres', password='password', dbname='test')
except:
    printf("unable to connect")



cur = conn.cursor()
batch_insert(cur)
cur.close()
conn.close()
