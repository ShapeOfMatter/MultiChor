
-- Multiple servers
-- Multiple clients
$(mkLoc "server1")
$(mkLoc "server2")
$(mkLoc "server3")
$(mkLoc "server4")

$(mkLoc "client1")
$(mkLoc "client2")
$(mkLoc "client3")
$(mkLoc "client4")
$(mkLoc "client5")
$(mkLoc "client6")

type Fp -- field elements

type Servers = ["server1", "server2", "server3", "server4"]
type Clients = ["client1", "client2", "client3", "client4", "client5", "client6"]
type Participants = Servers ++ Clients

-- sendShares :: Located Fp c -> Member s Servers -> Choreo Participants (CLI m) (Located Fp s)
-- sendShares share prf = do
--   fps <- (c `introAnd` c, share) ~> s @@ nobody
--   return fps

-- createShares :: Choreo Clients (CLI m) (Faceted Clients [Fp])
-- createShares = do
--   facetedM Clients (do
--                        secret <- getstr
--                        let shares = secretShare secret (length Clients)
--                        facetedM sendShares (zip shares Servers))
--   return $ faceted shares

serverSum :: Choreo Participants (CLI m) (Located server1 Fp, Located server2 Fp)
serverSum = do
  s11, s12 <- client1 `_locally` getstr "enter secret" >>= secretShare
  s21, s22 <- client2 `_locally` getstr "enter secret" >>= secretShare
  s11s <- (client1 `introAnd` client1, s11) ~> server1 @@ nobody
  s21s <- (client2 `introAnd` client2, s21) ~> server1 @@ nobody
  s12s <- (client1 `introAnd` client1, s12) ~> server2 @@ nobody
  s22s <- (client2 `introAnd` client2, s22) ~> server2 @@ nobody
  sum1 <- (server1, \un -> return $ (un s11s) + (un s21s)) ~~> server2 @@ nobody
  sum2 <- (server2, \un -> return $ (un s12s) + (un s22s)) ~~> server1 @@ nobody
  total1 <- server1 `locally` \un -> return $ (un sum1) + (un sum2)
  total2 <- server2 `locally` \un -> return $ (un sum1) + (un sum2)
  return $ (total1, total2)

p2pSum :: Choreo Participants (CLI m) (Located client1 Fp, Located client2 Fp)
p2pSum = do
  s11, s12 <- client1 `_locally` getstr "enter secret" >>= secretShare
  s21, s22 <- client2 `_locally` getstr "enter secret" >>= secretShare
  s12s <- (client1 `introAnd` client1, s12) ~> client2 @@ nobody
  s21s <- (client2 `introAnd` client2, s21) ~> client1 @@ nobody
  sum1 <- (client1, \un -> return $ (un s11) + (un s21s)) ~~> client2 @@ nobody
  sum2 <- (client2, \un -> return $ (un s12s) + (un s22)) ~~> client1 @@ nobody
  total1 <- client1 `locally` \un -> return $ (un sum1) + (un sum2)
  total2 <- client2 `locally` \un -> return $ (un sum1) + (un sum2)
  return $ (total1, total2)
