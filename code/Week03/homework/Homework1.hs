{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,
                                       mkValidatorScript)
import Plutus.V2.Ledger.Api 
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx = beneficiary1HasSigned && beforeOrAtDeadline || beneficiary2HasSigned && deadlinePassed 
    where 
        infosAboutTx = scriptContextTxInfo ctx
        ctxTimeRange = txInfoValidRange infosAboutTx
        beneficiary1HasSigned = txSignedBy infosAboutTx $ beneficiary1 dat
        beneficiary2HasSigned = txSignedBy infosAboutTx $ beneficiary2 dat 
        -- il faut bien comprendre la notion de temps ici, la datum sont les donnees incluses dans la tx precedente
        -- on prend les infos dans le context de la tx pour faire les comparaison avec le datum
        beforeOrAtDeadline = after (deadline dat) ctxTimeRange               
        deadlinePassed = before (deadline dat) ctxTimeRange
            
{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
