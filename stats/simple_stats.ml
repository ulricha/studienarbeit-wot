open Batteries
open Printf

let today = Unix.time ()

let extract_first l = 
  match l with 
    | hd :: tl -> Option.get hd
    | [] -> 0L

let all_keys_stats dbh =
  let overall = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today)" in
  let rsa_keys = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1" in
  let dsa_keys = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17" in
  let otheralg_keys = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg <> 1 and alg <> 17" in
  let rsa_512 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 512" in
  let rsa_768 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 768" in
  let rsa_1024 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 1024" in
  let rsa_2048 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 2048" in
  let rsa_3072 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 3072" in
  let rsa_4096 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 4096" in
  let usual_keylens = [512l; 768l; 1024l; 2048l; 3072l; 4096l] in
  let rsa_unusual = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen not in $@usual_keylens" in
  let dsa_512 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 512" in
  let dsa_768 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 768" in
  let dsa_1024 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 1024" in
  let dsa_2048 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 2048" in
  let dsa_3072 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 3072" in
  let dsa_4096 = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 4096" in
  let dsa_unusual = PGSQL(dbh) "select count(*) from keys where revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen not in $@usual_keylens" in
  let keys_with_expire_date = PGSQL(dbh) "select count(*) from keys where revoktime is null and exptime is not null and exptime > $today" in
  let avg_uids = PGSQL(dbh) "select avg(total) from (select keyid, count(*) as total from uids group by keyid) as uids_per_key" in
  let revoked = PGSQL(dbh) "select count(*) from keys where revoktime is not null" in
    printf "total number of keys %Ld (not expired and not revoked)\n" (extract_first overall);
    printf "number of revoked keys %Ld\n" (extract_first revoked);
    printf "number of rsa keys %Ld\n" (extract_first rsa_keys);
    printf "number of dsa keys %Ld\n" (extract_first dsa_keys);
    printf "number of keys with other pk algorithms %Ld\n" (extract_first otheralg_keys);
    printf "number of 512-bit rsa keys %Ld\n" (extract_first rsa_512);
    printf "number of 768-bit rsa keys %Ld\n" (extract_first rsa_768);
    printf "number of 1024-bit rsa keys %Ld\n" (extract_first rsa_1024);
    printf "number of 2048-bit rsa keys %Ld\n" (extract_first rsa_2048);
    printf "number of 3072-bit rsa keys %Ld\n" (extract_first rsa_3072);
    printf "number of 4096-bit rsa keys %Ld\n" (extract_first rsa_4096);
    printf "number of rsa keys with unusual key lengths %Ld\n" (extract_first rsa_unusual);
    printf "number of 512-bit dsa keys %Ld\n" (extract_first dsa_512);
    printf "number of 768-bit dsa keys %Ld\n" (extract_first dsa_768);
    printf "number of 1024-bit dsa keys %Ld\n" (extract_first dsa_1024);
    printf "number of 2048-bit dsa keys %Ld\n" (extract_first dsa_2048);
    printf "number of 3072-bit dsa keys %Ld\n" (extract_first dsa_3072);
    printf "number of 4096-bit dsa keys %Ld\n" (extract_first dsa_4096);
    printf "number of dsa keys with unusual key lengths %Ld\n" (extract_first dsa_unusual);
    printf "keys with expire dates %Ld\n" (extract_first keys_with_expire_date);
    printf "average number of uids per key %f (unreliable)\n" (Option.get (List.hd avg_uids))

let mscc_keys_stats dbh =
  let overall = PGSQL(dbh) "select count(*) from mscc_keys where revoktime is null and  (exptime is null or exptime > $today)" in
    print_endline "some_key_stats overall";
  let rsa_keys = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 1 " in
  let dsa_keys = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 17 " in
  let otheralg_keys = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg <> 1 and alg <> 17 " in
  let rsa_512 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 512 " in
  let rsa_768 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 768 " in
  let rsa_1024 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 1024 " in
  let rsa_2048 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 2048 " in
  let rsa_3072 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 3072 " in
  let rsa_4096 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 1 and keylen = 4096 " in
  let usual_keylens = [512l; 768l; 1024l; 2048l; 3072l; 4096l] in
  let rsa_unusual = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime < $today) and alg = 17 and keylen not in $@usual_keylens " in
  let dsa_512 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 512 " in
  let dsa_768 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 768 " in
  let dsa_1024 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 1024 " in
  let dsa_2048 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 2048 " in
  let dsa_3072 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 3072 " in
  let dsa_4096 = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime > $today) and alg = 17 and keylen = 4096 " in
  let dsa_unusual = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and (exptime is null or exptime < $today) and alg = 17 and keylen not in $@usual_keylens " in
  let keys_with_expire_date = PGSQL(dbh) "select count(*) from mscc_keys where  revoktime is null and exptime is not null and exptime < $today " in
  let avg_uids = PGSQL(dbh) "select avg(total) from (select uids.keyid, count(*) as total from uids inner join component_ids on uids.keyid = component_ids.keyid where component_id = 0 group by uids.keyid) as uids_per_key" in
  let reachable = PGSQL(dbh) "select count(*) from (select sigs.signer as signer, sigs.signee as signee  from sigs, component_ids where sigs.signer = component_ids.keyid and component_ids.component_id = 0) as signers, component_ids where signers.signee = component_ids.keyid and component_ids.component_id <> 0" in
  let reaching = PGSQL(dbh) "select count(*) from (select sigs.signer as signer, sigs.signee as signee  from sigs, component_ids where sigs.signer = component_ids.keyid and component_ids.component_id <> 0) as signers, component_ids where signers.signee = component_ids.keyid and component_ids.component_id = 0" in
    printf "total number of keys %Ld\n" (extract_first overall);
    printf "number of rsa keys %Ld\n" (extract_first rsa_keys);
    printf "number of dsa keys %Ld\n" (extract_first dsa_keys);
    printf "number of keys with other pk algorithms %Ld\n" (extract_first otheralg_keys);
    printf "number of 512-bit rsa keys %Ld\n" (extract_first rsa_512);
    printf "number of 768-bit rsa keys %Ld\n" (extract_first rsa_768);
    printf "number of 1024-bit rsa keys %Ld\n" (extract_first rsa_1024);
    printf "number of 2048-bit rsa keys %Ld\n" (extract_first rsa_2048);
    printf "number of 3072-bit rsa keys %Ld\n" (extract_first rsa_3072);
    printf "number of 4096-bit rsa keys %Ld\n" (extract_first rsa_4096);
    printf "number of rsa keys with unusual key lengths %Ld\n" (extract_first rsa_unusual);
    printf "number of 512-bit dsa keys %Ld\n" (extract_first dsa_512);
    printf "number of 768-bit dsa keys %Ld\n" (extract_first dsa_768);
    printf "number of 1024-bit dsa keys %Ld\n" (extract_first dsa_1024);
    printf "number of 2048-bit dsa keys %Ld\n" (extract_first dsa_2048);
    printf "number of 3072-bit dsa keys %Ld\n" (extract_first dsa_3072);
    printf "number of 4096-bit dsa keys %Ld\n" (extract_first dsa_4096);
    printf "number of dsa keys with unusual key lengths %Ld\n" (extract_first dsa_unusual);

    printf "keys with expire dates %Ld\n" (extract_first keys_with_expire_date);
    printf "average number of uids per key %f (unreliable)\n" (Option.get (List.hd avg_uids));
    printf "reachable set %Ld\n" (extract_first reachable);
    printf "reaching set %Ld\n" (extract_first reaching)

let sig_stats_overall dbh =
  let overall_sigs = PGSQL(dbh) "SELECT COUNT(*) FROM sigs" in
  let overall_valid_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let level_0x10_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE level = 16 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE level = 16 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let level_0x11_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE level = 17 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE level = 17 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let level_0x12_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE level = 18 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE level = 18 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let level_0x13_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE level = 19 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE level = 19 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let revoked_sigs = PGSQL(dbh) "SELECT count(*) FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE sigs.revoktime IS NOT NULL" in
  let valid_expiring_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND sigs.exptime < $today) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND sigs.exptime > $today)" in
  let md5_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE sigs.hash_alg = 1 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE sigs.hash_alg = 1 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let sha1_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE sigs.hash_alg = 2 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE sigs.hash_alg = 2 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let ripemd160_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE sigs.hash_alg = 3 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE sigs.hash_alg = 3 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let sha256_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE sigs.hash_alg = 8 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE sigs.hash_alg = 8 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let sha384_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE sigs.hash_alg = 9 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE sigs.hash_alg = 9 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let sha512_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE sigs.hash_alg = 10 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE sigs.hash_alg = 10 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
  let sha224_sigs = PGSQL(dbh) "(SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN keys ON sigs.signer = keys.keyid WHERE sigs.hash_alg = 11 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today)) INTERSECT (SELECT sigs.signer, sigs.signee FROM sigs INNER JOIN KEYS ON sigs.signee = keys.keyid WHERE sigs.hash_alg = 11 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND sigs.revoktime IS NULL AND (sigs.exptime IS NULL OR sigs.exptime > $today))" in
    printf "total number of sigs %Ld\n" (extract_first overall_sigs);
    printf "total number of revoked sigs %Ld\n" (extract_first revoked_sigs);
    printf "total number of valid sigs with expire dates %d\n" (List.length valid_expiring_sigs);
    printf "total number of valid sigs %d (valid sigs = not expired and not revoked and signer and signee are valid)\n" (List.length overall_valid_sigs);
    printf "total number of valid sigs with cert level 0x10 (generic) %d\n" (List.length level_0x10_sigs);
    printf "total number of valid sigs with cert level 0x11 (persona) %d\n" (List.length level_0x11_sigs);
    printf "total number of valid sigs with cert level 0x12 (casual) %d\n" (List.length level_0x12_sigs);
    printf "total number of valid sigs with cert level 0x13 (positive) %d\n" (List.length level_0x13_sigs);
    printf "total number of valid sigs with hash algorithm MD5 (be cautious) %d\n" (List.length md5_sigs);
    printf "total number of valid sigs with hash algorithm SHA1 (be cautious) %d\n" (List.length sha1_sigs);
    printf "total number of valid sigs with hash algorithm RIPE-MD/160 (be cautious) %d\n" (List.length ripemd160_sigs);
    printf "total number of valid sigs with hash algorithm SHA256 (be cautious) %d\n" (List.length sha256_sigs);
    printf "total number of valid sigs with hash algorithm SHA256 (be cautious) %d\n" (List.length sha384_sigs);
    printf "total number of valid sigs with hash algorithm SHA512 (be cautious) %d\n" (List.length sha512_sigs);
    printf "total number of valid sigs with hash algorithm SHA224 (be cautious) %d\n" (List.length sha224_sigs)

let sig_stats_mscc dbh =
  let overall_sigs = PGSQL(dbh) "SELECT COUNT(*) FROM mscc_sigs" in
  let overall_valid_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let level_0x10_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE level = 16 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE level = 16 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let level_0x11_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE level = 17 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE level = 17 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let level_0x12_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE level = 18 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE level = 18 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let level_0x13_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE level = 19 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE level = 19 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let revoked_sigs = PGSQL(dbh) "SELECT count(*) FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE mscc_sigs.revoktime IS NOT NULL" in
  let valid_expiring_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND mscc_sigs.exptime < $today) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND mscc_sigs.exptime > $today)" in
  let md5_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE mscc_sigs.hash_alg = 1 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE mscc_sigs.hash_alg = 1 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let sha1_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE mscc_sigs.hash_alg = 2 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE mscc_sigs.hash_alg = 2 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let ripemd160_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE mscc_sigs.hash_alg = 3 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE mscc_sigs.hash_alg = 3 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let sha256_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE mscc_sigs.hash_alg = 8 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE mscc_sigs.hash_alg = 8 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let sha384_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE mscc_sigs.hash_alg = 9 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE mscc_sigs.hash_alg = 9 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let sha512_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE mscc_sigs.hash_alg = 10 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE mscc_sigs.hash_alg = 10 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
  let sha224_sigs = PGSQL(dbh) "(SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN keys ON mscc_sigs.signer = keys.keyid WHERE mscc_sigs.hash_alg = 11 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today)) INTERSECT (SELECT mscc_sigs.signer, mscc_sigs.signee FROM mscc_sigs INNER JOIN KEYS ON mscc_sigs.signee = keys.keyid WHERE mscc_sigs.hash_alg = 11 AND keys.revoktime IS NULL AND (keys.exptime IS NULL OR keys.exptime > $today) AND mscc_sigs.revoktime IS NULL AND (mscc_sigs.exptime IS NULL OR mscc_sigs.exptime > $today))" in
    printf "total number of sigs in mscc %Ld\n" (extract_first overall_sigs);
    printf "total number of revoked sigs in mscc %Ld\n" (extract_first revoked_sigs);
    printf "total number of valid sigs in mscc with expire dates %d\n" (List.length valid_expiring_sigs);
    printf "total number of valid sigs in mscc %d (valid sigs = not expired and not revoked and signer and signee are valid)\n" (List.length overall_valid_sigs);
    printf "total number of valid sigs with cert level 0x10 (generic) %d\n" (List.length level_0x10_sigs);
    printf "total number of valid sigs in mscc with cert level 0x11 (persona) %d\n" (List.length level_0x11_sigs);
    printf "total number of valid sigs in mscc with cert level 0x12 (casual) %d\n" (List.length level_0x12_sigs);
    printf "total number of valid sigs in mscc with cert level 0x13 (positive) %d\n" (List.length level_0x13_sigs);
    printf "total number of valid sigs in mscc with hash algorithm MD5 (be cautious) %d\n" (List.length md5_sigs);
    printf "total number of valid sigs in mscc with hash algorithm SHA1 (be cautious) %d\n" (List.length sha1_sigs);
    printf "total number of valid sigs in mscc with hash algorithm RIPE-MD/160 (be cautious) %d\n" (List.length ripemd160_sigs);
    printf "total number of valid sigs in mscc with hash algorithm SHA256 (be cautious) %d\n" (List.length sha256_sigs);
    printf "total number of valid sigs in mscc with hash algorithm SHA256 (be cautious) %d\n" (List.length sha384_sigs);
    printf "total number of valid sigs in mscc with hash algorithm SHA512 (be cautious) %d\n" (List.length sha512_sigs);
    printf "total number of valid sigs in mscc with hash algorithm SHA224 (be cautious) %d\n" (List.length sha224_sigs)



let _ =
  if Array.length Sys.argv <> 2 then (
    print_endline "usage: simple_stats db";
    exit 1)

let main () =
  let dbh = PGOCaml.connect ~database:Sys.argv.(1) () in
    print_endline "\nall keys:";
    all_keys_stats dbh;
    print_endline "\n mscc keys:";
    mscc_keys_stats dbh;
    print_endline "\n all sigs:";
    sig_stats_overall dbh

let _ = 
  try main () with
    | e -> prerr_endline (Printexc.to_string e)
