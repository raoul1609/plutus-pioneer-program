{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime,
                                       PubKeyHash, ScriptContext, POSIXTimeRange, 
                                       mkMintingPolicyScript)
import          qualified  PlutusTx
import           PlutusTx.Builtins.Internal
import           PlutusTx.Prelude     (Bool (False), ($), traceIfFalse, (&&))
import           Utilities           
import           Plutus.V2.Ledger.Contexts 
import           Plutus.V1.Ledger.Interval 
import           Prelude                   (IO, Show (show))
import           Text.Printf               (printf)

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy pkh deadline () ctx =  -- ownerSigned &&  isDeadlinePassed
    traceIfFalse "the ownwer have not signed the tx"  ownerSigned &&
        traceIfFalse "the deadline has not passed" isDeadlinePassed 
    
    where txInfos :: TxInfo 
          txInfos = scriptContextTxInfo ctx 

          valideRangeForTheTx :: POSIXTimeRange
          valideRangeForTheTx = txInfoValidRange txInfos

          ownerSigned :: Bool
          ownerSigned = txSignedBy txInfos pkh
-- il faut comprendre la notion de deadline : la tx doit etre effectue avant le deadline 
          isDeadlinePassed :: Bool 
          isDeadlinePassed = after deadline valideRangeForTheTx
          

--{-# INLINABLE mkWrappedDeadlinePolicy #-}
-- mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
-- mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy  pkh deadline

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = let x = PlutusTx.unsafeFromBuiltinData pkh
                                           y = PlutusTx.unsafeFromBuiltinData deadline 
                                       in  wrapPolicy $ mkDeadlinePolicy  x y
    

signedHomework1Code :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
signedHomework1Code = $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])

-- deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
-- deadlinePolicy pkh deadline = mkMintingPolicyScript $
--     $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
--         `PlutusTx.applyCode` PlutusTx.liftCode pkh
--         `PlutusTx.applyCode` PlutusTx.liftCode deadline


deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)
        `PlutusTx.applyCode` PlutusTx.liftCode ( PlutusTx.toBuiltinData deadline)


saveHomework1ToFile :: IO ()
saveHomework1ToFile = writeCodeToFile "./assets/raoulHomework1WithoutParams.plutus" signedHomework1Code

saveHomework1Policy :: PubKeyHash -> POSIXTime -> IO ()
--saveHomework1Policy pkh deadline = writePolicyToFile (printf ("assets/signed-%s.plutus" (show pkh) (show deadline))) $ deadlinePolicy pkh deadline 
saveHomework1Policy pkh deadline = writePolicyToFile "./assets/raoulHomework1.plutus" $ deadlinePolicy pkh deadline
