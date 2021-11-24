# must remember to ALWAYS do this; even in client code!!!!
pragma foreign_keys = ON;

# set the db schema version as user_version
pragma user_version;

drop table if exists kv;
drop table if exists sitedata;
drop table if exists hk;
drop table if exists devices;
drop table if exists sites;
drop table if exists gwy01;
drop table if exists hkkv;
drop view if exists hkwp;
drop table if exists hkprovision;
drop trigger if exists hkwpinsert;

create table gwy01 (
    linuxid text not null check (length(linuxid) == 16) unique,
    deviceid text not null check (length(deviceid) == 12) unique,
    primary key (linuxid)
) without rowid;

create table sites (
    linuxid text not null check (length(linuxid) == 16),
    siteid text not null check (length(siteid) == 36),
    primary key (linuxid,siteid),
    foreign key (linuxid) references gwy01(linuxid) on delete cascade
) without rowid;

create table hk (
    linuxid text not null check (length(linuxid) == 16),
    uuid text not null check (length(uuid) == 36),
    srp_salt blob not null check (length(srp_salt) == 16),
    srp_verifier blob not null check (length(srp_verifier) <= 384),
    token blob not null check (length(token) <= 2048),
    setupid text not null check (length(setupid) == 4),
    primary key (linuxid),
    foreign key (linuxid) references gwy01(linuxid) on delete cascade
) without rowid;

create table hkprovision (
    linuxid text not null check (length(linuxid) == 16),
    provisionid text not null check (length(provisionid) <= 64),
    primary key (linuxid),
    foreign key (linuxid) references gwy01(linuxid) on delete cascade
) without rowid;

create view hkwp as select * from hk natural left join hkprovision natural left join gwy01;

create trigger hkwpinsert instead of insert on hkwp for each row
begin
    insert or ignore into gwy01 values (NEW.linuxid, NEW.deviceid);
    insert or rollback into hk values (NEW.linuxid, NEW.uuid, NEW.srp_salt, NEW.srp_verifier, NEW.token, NEW.setupid);
    insert or rollback into hkprovision values (NEW.linuxid, NEW.provisionid);
end;

create table hkkv (
    linuxid text not null check (length(linuxid) == 16),
    key integer not null check (key >= 0 and key <= 255),
    value blob not null check (length(value) <= 2048),
    primary key (linuxid,key),
    foreign key (linuxid) references gwy01(linuxid) on delete cascade
);

create table devices (
    siteid text not null check (length(siteid) == 36),
    linuxid text not null check (length(linuxid) == 16),
    device_group_index int not null,
    device_type int not null,
    deviceid text not null check (length(deviceid) == 12),
    firmware text not null check (length(firmware) <= 64),
    name text not null check (length(name) <= 128),
    primary key (siteid,device_group_index),
    foreign key (siteid,linuxid) references sites(siteid,linuxid) on delete cascade,
    foreign key (linuxid) references gwy01(linuxid) on delete cascade
) without rowid;

# set the user version to 1
pragma user_version = 1;
pragma user_version;

# pretty cli output
.mode line


# example transactions...
begin;

#insert or rollback into gwy01 (linuxid, deviceid) values ('0f3414435759304b', 'E874ABB97117');
#insert or rollback into gwy01 (linuxid, deviceid) values ('0f3414435759304a', 'E874ABB97118');

# add some homekit values...
insert or rollback into hkwp (linuxid, deviceid, uuid, srp_salt, srp_verifier, token, setupid, provisionid) values 
    ('0f3414435759304b', 'E874ABB97117', '9d7d36b6-a484-44b9-84a8-c9ab83c49fda', 'abcdabcdabcdabcd', 'abcdfgij', 'abcd', 'abcd', 'abcdef');

# add the gateway to a site...
insert or rollback into sites (linuxid, siteid) values 
    ('0f3414435759304b', '9d7d36b6-a484-44b9-84a8-aaaaaaaaaaaa');

# add some devices
insert or rollback into devices (siteid, linuxid, device_group_index, device_type, deviceid, firmware, name) values
    ('9d7d36b6-a484-44b9-84a8-aaaaaaaaaaaa', '0f3414435759304b', 12, 1, 'E874ABB97111', '20200101', 'hi!');

# joins are ok. its a good idea to key correctly.
select * from hk natural left join gwy01 natural left join sites where linuxid = '0f3414435759304b';
select CURRENT_TIMESTAMP;
select * from hkwp;

delete from hkprovision;

select CURRENT_TIMESTAMP;

select * from hkwp;

select name,deviceid,firmware,device_type,device_group_index from devices;


# this should fail
#insert into hkkv (linuxid, key, value) values ('0f3414435759304b', 256, 'a');
#insert into hkkv (linuxid, key, value) values ('0f3414435759304b', -1, 'a');
#insert into hkkv (linuxid, key, value) values ('aaaaaaaaaaaaaaaa', 1, 'a');

# factory reset:
delete from gwy01;

# should be empty
select count(*) as hk_rows from hk;
select count(*) as devices_rows from devices;

commit;
