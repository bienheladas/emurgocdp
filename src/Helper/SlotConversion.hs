module SlotConversion where 

import Ledger.TimeSlot
import Ledger

testnetSlotConfig :: SlotConfig 
testnetSlotConfig = SlotConfig 1000 $ POSIXTime 1660003200000

getSlot :: POSIXTime -> Slot
getSlot n = posixTimeToEnclosingSlot testnetSlotConfig n 

testnetSlotConfig :: SlotConfig
--testnetSlotConfig = SlotConfig 1000 $ POSIXTime 1654059600000 --01/06/2022
--testnetSlotConfig = SlotConfig 1000 $ POSIXTime 1660003200000 --09/08/2022 
testnetSlotConfig = SlotConfig 1000 $ POSIXTime 1666656000000 --25/10/2022 preview
