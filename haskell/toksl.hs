
type Node = Begin Position
          | Pick Position Float [Decorator] [Decorator]
          | Place Position Float [Decorator] [Decorator]
          | Via Position 
          | End
          | Repeat
;

type Decorator = SensorWait String Bool
               | TimeWait Float
;




