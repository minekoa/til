
-record(query, { key :: unicode:chardata()
               , from :: {value, calender:date()} | none
               , to   :: {value, calender:date()} | none
               , direction = desc :: desc | asc
               }
       ).
