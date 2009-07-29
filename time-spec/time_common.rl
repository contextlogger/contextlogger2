%%{

  machine time_common;

  monday = ("Mon" "day"?) %{ seenWday = 1; } ;
  tuesday = ("Tue" "sday"?) %{ seenWday = 2; } ;
  wednesday = ("Wed" "nesday"?) %{ seenWday = 3; } ;
  thursday = ("Thu" "rsday"?) %{ seenWday = 4; } ;
  friday = ("Fri" "day"?) %{ seenWday = 5; } ;
  saturday = ("Sat" "urday"?) %{ seenWday = 6; } ;
  sunday = ("Sun" "day"?) %{ seenWday = 0; } ;

  day_of_week = monday | tuesday | wednesday | thursday | friday | saturday | sunday ;

  january = ("Jan" "uary"?) %{ seenMonth = 0; } ;
  february = ("Feb" "ruary"?) %{ seenMonth = 1; } ;
  march = ("Mar" "ch"?) %{ seenMonth = 2; } ;
  april = ("Apr" "il"?) %{ seenMonth = 3; } ;
  may = ("May") %{ seenMonth = 4; } ;
  june = ("Jun" "e"?) %{ seenMonth = 5; } ;
  july = ("Jul" "y"?) %{ seenMonth = 6; } ;
  august = ("Aug" "ust"?) %{ seenMonth = 7; } ;
  september = ("Sep" "tember"?) %{ seenMonth = 8; } ;
  october = ("Oct" "ober"?) %{ seenMonth = 9; } ;
  november = ("Nov" "ember"?) %{ seenMonth = 10; } ;
  december = ("Dec" "ember"?) %{ seenMonth = 11; } ;

  month_name = january | february | march | april | may | june | july | august | september | october | november | december ;

  action Mark { mark = fpc; }

  action HourSeen { seenHour = ATOI_RANGE(mark,0,23); }
  action MinSeen { seenMin = ATOI_RANGE(mark,0,59); }
  action SecSeen { seenSec = ATOI_RANGE(mark,0,59); }

  action ClearMinSec { seenMin = seenSec = 0; }

  just_time = 
    (digit{0,2} >Mark %HourSeen
     (":" digit{0,2} >Mark %MinSeen
      (":" digit{0,2} >Mark %SecSeen )?)?)
    >ClearMinSec
    ;

  action TodaySeen {
    seenYear = ctx_tm.tm_year;
    seenMonth = ctx_tm.tm_mon;
    seenDay = ctx_tm.tm_mday;
  }

  action TomorrowSeen {
    SET_SEEN_DATE_PLUS_DAYS(ctx, 1);
  }

  action TdatSeen {
    SET_SEEN_DATE_PLUS_DAYS(ctx, 2);
  }

  action NextDowSeen {
    // want next seenWday, but not today
    int plusDays = num_days_till_next_wday(ctx_tm.tm_wday, seenWday);
    SET_SEEN_DATE_PLUS_DAYS(ctx, plusDays);
  }

  relative_date = 
    "today" %TodaySeen |
    "tomorrow" %TomorrowSeen |
    (("the" space+)? "day" space+ "after" space+ "tomorrow") %TdatSeen |
    ("next" space+ day_of_week) %NextDowSeen ;

  action YearSeen { seenYear = ATOI_RANGE(mark,1900,9999) - 1900; }
  action MonthSeen { seenMonth = ATOI_RANGE(mark,1,12) - 1; }
  action DaySeen { seenDay = ATOI_RANGE(mark,1,31); }

  just_date = 
    (digit{0,4} >Mark %YearSeen
     "-" (digit{0,2} >Mark %MonthSeen | month_name)
     "-" digit{0,2} >Mark %DaySeen
     | relative_date)
    ;

  datetime = 
    "at" space+ just_time space+ "on" space+ just_date |
    ("on" space+)? just_date space+ ("at" space+)? just_time ;

}%%
