{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Homework
    ( stakeValidator'
    , saveStakeValidator'
    ) where


import           PlutusTx.Builtins.Internal (BuiltinByteString (..))
import           Plutus.V1.Ledger.Value (valueOf)
import           Plutus.V1.Ledger.Api   (toBuiltinData, BuiltinData, Data (..), BuiltinData (..))
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Plutus.V2.Ledger.Api   (Address, BuiltinData,
                                         ScriptContext (scriptContextPurpose, scriptContextTxInfo),
                                         ScriptPurpose (Certifying, Rewarding),
                                         StakeValidator, StakingCredential,
                                         TxInfo (txInfoOutputs, txInfoWdrl),
                                         TxOut (txOutAddress, txOutValue),
                                         adaSymbol, adaToken,
                                         mkStakeValidatorScript, PubKeyHash (..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap      as PlutusTx
import           PlutusTx.Prelude       (AdditiveSemigroup ((+)), Bool (..),
                                         Eq ((==)), Integer,
                                         Maybe (Just, Nothing),
                                         MultiplicativeSemigroup ((*)),
                                         Ord ((>=)), Semigroup ((<>)), foldl,
                                         otherwise, traceError, traceIfFalse,
                                         ($), (.), (&&))
import           Prelude              (IO, String, undefined, read, ioError, fail)
import           System.IO.Error      (userError)
import           Utilities            (tryReadAddress, wrapStakeValidator, writeStakeValidatorToFile)

-- | A staking validator with two parameters, a pubkey hash and an address. The validator
--   should work as follows:
--   1.) The given pubkey hash needs to sign all transactions involving this validator.
--   2.) The given address needs to receive at least half of all withdrawn rewards.
{-# INLINABLE mkStakeValidator' #-}
mkStakeValidator' :: PubKeyHash -> Address -> () -> ScriptContext -> Bool
mkStakeValidator' pkh addr () ctx = case scriptContextPurpose ctx of
    Certifying _   -> True
    Rewarding credential -> traceIfFalse "doit signer toutes les transactions et avoir au moins la moitie des rewards" 
        (txSignedBy info pkh) && ((2 * paidToAddress) >= amount credential)
    _              -> False

    where 
            info :: TxInfo
            info = scriptContextTxInfo ctx

            amount ::   StakingCredential ->  Integer
            amount cred = case PlutusTx.lookup cred $ txInfoWdrl info of
                Just amt -> amt
                Nothing  -> traceError "withdrawal not found"
            
            paidToAddress :: Integer
            paidToAddress = foldl f 0 $ txInfoOutputs info
                where
                     f :: Integer -> TxOut -> Integer
                     f n o
                        | txOutAddress o == addr = n + valueOf (txOutValue o) adaSymbol adaToken
                        | otherwise              = n

{-# INLINABLE mkWrappedStakeValidator' #-}
mkWrappedStakeValidator' :: PubKeyHash -> Address -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator' pkh = wrapStakeValidator . mkStakeValidator' pkh

stakeValidator' :: PubKeyHash -> Address -> StakeValidator
stakeValidator' pkh addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedStakeValidator' ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode addr

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- saveStakeValidator' :: String -> String -> IO ()
-- saveStakeValidator' pkh bech32 = do
--     case tryReadAddress bech32 of
--         Nothing   -> ioError $ userError $ "Invalid address: " <> bech32
--         Just addr -> do 
--             let BuiltinData someData = toBuiltinData pkh 
--                 convertedPubKeyHash = BuiltinByteString something
--             case someData of 
--                 B val -> do 
--                     let altro = BuiltinByteString val
--                     writeStakeValidatorToFile "./assets/staking.plutus" $ stakeValidator' (PubKeyHash altro) addr
--                 _     -> fail "erreur de conversion"

saveStakeValidator' :: PubKeyHash -> String -> IO ()
saveStakeValidator' pkh bech32 = do
    case tryReadAddress bech32 of
         Nothing   -> ioError $ userError $ "Invalid address: " <> bech32
         Just addr -> writeStakeValidatorToFile "./assets/HomeworkStaking.plutus" $ stakeValidator' pkh addr

