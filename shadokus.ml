type date = { year : int; month : int; day : int; hour : int; minute : int }

let the_origin_of_time = { year = 1; month = 1; day = 1; hour = 0; minute = 0 }

let wellformed { year; month; day; hour; minute } = 
  year >= 1 
  && month >= 1 && month <= 5 
  && day >= 1 && day <= 4 
  && hour >= 0 && hour <= 2 
  && minute >= 0 && minute <= 1

let date_sub { year; month; day; hour; minute } =
  let { year = origin_year; month = origin_month; 
        day = origin_day; hour = origin_hour; minute = origin_minute } = the_origin_of_time in
  { year = year - origin_year; month = month - origin_month; 
    day = day - origin_day; hour = hour - origin_hour; minute = minute - origin_minute }

let date_add { year; month; day; hour; minute } =
  let { year = origin_year; month = origin_month; 
        day = origin_day; hour = origin_hour; minute = origin_minute } = the_origin_of_time in
  { year = year + origin_year; month = month + origin_month; 
    day = day + origin_day; hour = hour + origin_hour; minute = minute + origin_minute }

let minute_of_date date =
  let { year; month; day; hour; minute } = date_sub date in 
  minute
  + hour * 2
  + day * 2 * 3
  + month * 2 * 3 * 4
  + year * 2 * 3 * 4 * 5

let div_mod a b = (a / b, a mod b)
                       
let date_of_minute minutes =
  let (year, year_rem) = div_mod minutes (2 * 3 * 4 * 5) in
  let (month, month_rem) = div_mod year_rem (2 * 3 * 4) in
  let (day, day_rem) = div_mod month_rem (2 * 3) in
  let (hour, minute) = div_mod day_rem 2 in
  date_add ({ year = year; month = month; day = day; hour = hour; minute = minute })

let next date = date_of_minute ((minute_of_date date) + 1)

let of_int minutes = date_of_minute ((minute_of_date the_origin_of_time) + minutes)
