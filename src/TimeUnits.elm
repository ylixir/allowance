module TimeUnits exposing (day, hour, minute, second, week)


second : Int
second =
    1000


minute : Int
minute =
    60 * second


hour : Int
hour =
    60 * minute


day : Int
day =
    24 * hour


week : Int
week =
    7 * day
