data Customer = Customer {
      custID :: Int
    , custName :: String
    , custAddress :: Address
    }

newtype Zip = Zip Int
    deriving (Eq, Show)

data Address = Address {
      addrStreet :: String
    , addrCity :: String
    , addrState :: String
    , addrZip :: Zip
    }

setAddrZip :: Zip -> Address -> Address
setAddrZip zip addr = addr { addrZip = zip }

setCustAddress :: Address -> Customer -> Customer
setCustAddress addr cust = cust { custAddress = addr }

setCustZip :: Zip -> Customer -> Customer
setCustZip zip cust =
    setCustAddress (setAddrZip zip (custAddress cust)) cust
