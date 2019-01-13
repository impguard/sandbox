type Address = [String]
type CustomerID = Int

data Customer  = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)
