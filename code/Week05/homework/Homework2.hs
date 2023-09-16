{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V2.Ledger.Api 
import           Plutus.V1.Ledger.Value 
import qualified PlutusTx
import           PlutusTx.Builtins.Internal
import           PlutusTx.Prelude     (Bool (False), ($), (.), null, (==), any, traceIfFalse, (&&))
import           Utilities            
import           Plutus.V2.Ledger.Contexts 
import           Plutus.V1.Ledger.Interval 
import           Prelude (IO)


{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy oref () ctx = traceIfFalse "impossible de consommer cet utxo" consumedTheRightUxo && 
                                 traceIfFalse "le tokenName doit etre vide" emptyTokenName

    where txInfos :: TxInfo
          txInfos = scriptContextTxInfo ctx

          txPurpose :: ScriptPurpose
          txPurpose = scriptContextPurpose ctx

          emptyTokenName :: Bool
          emptyTokenName = case flattenValue (txInfoMint txInfos) of 
                [(_, someTokenName , v)] -> someTokenName == TokenName "" &&  v == 1
                _                    -> False

          consumedTheRightUxo :: Bool
          consumedTheRightUxo = any (\someTxInInfos -> txInInfoOutRef someTxInInfos == oref) $ txInfoInputs txInfos 


-- {-# INLINABLE mkWrappedEmptyNFTPolicy #-}
-- mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
-- mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy someId someIndex = wrapPolicy $ mkEmptyNFTPolicy oref 
  where
    oref :: TxOutRef
    oref = TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData someId)
        (PlutusTx.unsafeFromBuiltinData someIndex)



signedHomework2Code :: PlutusTx.CompiledCode ( BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData  -> ())
signedHomework2Code = $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||])

nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $ $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref

-- nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
-- nftPolicy oref tn = mkMintingPolicyScript $ 
--       signedHomework2Code 
--             `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
--             `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)
--             `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)



-- ceci est la fonction de serialisation avec les parametres 
saveHomework2Policy :: TxOutRef -> TokenName -> IO ()
saveHomework2Policy utxoOutRef tokenName'  = writePolicyToFile "./assets/raoulHomework2.plutus" $ nftPolicy utxoOutRef tokenName'

-- fonction de serialisation sans les parametres, qui seront passes dans le offchain code. 
saveHomework2Code :: IO ()
saveHomework2Code = writeCodeToFile "assets/raoulHomework2.plutus" signedHomework2Code
