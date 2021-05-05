data Switch = SwitchOn | SwitchOff
  deriving (Show, Eq)

toggle SwitchOn  = SwitchOff
toggle SwitchOff = SwitchOn

data Dimmer = DimmerOff |
              DimmerOn Float
  deriving (Show, Eq)

dim1 = DimmerOn 50

isBright DimmerOff = False
isBright (DimmerOn percent) = percent >= 75

