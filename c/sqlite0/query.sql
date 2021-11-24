PRAGMA foreign_keys = ON;

insert into kv(key,value) values 
    ('srp_salt', 'abcd'),
    ('srp_verifier', 'abcde')
  on conflict(key) do update set value=excluded.value;


select * from kv;
