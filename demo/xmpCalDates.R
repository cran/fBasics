
#
# Example: 
#   Show how to manage calendar dates and holidays formatted as ISO8601
#   integers.
#
# Description: 
#   Part I: Formatted Gregorian Dates to Julian Date Numbers:
#     These examples convert formatted Gregorian Dates to Julian 
#     Day Numbers.
#   Part II: Dates in Standard Date Format:
#     These examples handle sdates
#     sjulian      convert ISO-8601 dates to Julian Day Numbers
#     sdate        convert Julian Day Numbers to ISO-8601 dates
#     sday.of.week return the day of week for ISO-8601 dates
#     sleep.year   test for leap years from ISO-8601 dates
#   Part III: Date/Time in Extended Date/Time Format:
#     These examples show how to handle xdates
#     xjulian      convert ISO-8601 date/times to Julian Minute Numbers
#     xdate        convert Julian Minute Numbers to ISO-8601 date/times
#     xday.of.week return the day of week for ISO-8601 x-dates
#     xleep.year   test for leap years from ISO-8601 x-dates
#   Part IV: Manage Holiday Calendars: 
#	  These examples demonstrate
#	  1 how to use the functions on.or.after(), on.or.before(), 
#	    nth.of.nday(), and last.of.nday(),
#	  2 how to calculate the date of Easter and Pentecost
#	  3 how to create a holiday calendar for the New York stock 
#	    exchange.
#
# Author:
#	(C) 2004, Diethelm Wuertz, GPL
#


################################################################################
# PART I: Formatted Gregorian Dates to Julian Date Numbers

    
   # Convert Formatted Gregorian Dates
   fdates = c("8/11/73", "08-11-73", "August 11 1973", "Aug11/73")
   fdates
   fjulian(fdates)
   
   # Convert with dmy order:
   fdates = c("11/8/73", "11-08-73", "11 August 1973", "11Aug73")
   fjulian(fdates, order='dmy')
   
   
################################################################################
# PART II: Dates in Standard Date Format


   # Handling a Single Date:
   # Manage Dates in Standard Date Formats
   # Date Standard: ISO-8601
   # Handling a Single Date:
   sjulian (19990101) 
   sdate(sjulian(19990101))
   sday.of.week(19990101)
   day.of.week(1, 1, 1999)
   sleap.year(19990101)

   # Handling Date Vectors:
   date = c(19730101, 19950131, 20000101)
   sjulian(date)
   sdate(sjulian(date))
   sday.of.week(date)
   sleap.year(date)
   
   
################################################################################
# Part III: Date/Time in Extended Date/Time Format


   # ISO-8601 ISO DATE/TIME FORMAT:
   # Manage Date/Time in Extended Date/Time Format
   # Date Standard: ISO-8601
   # Date: 1973-01-01 15:30
   xjulian(197301011530)
   print(xdate(xjulian(197301011530)), digits=9)
   xday.of.week(197301011530)
   xleap.year(197301011530)


################################################################################
# PART IV: Manage Holiday Calendars


   # What date has Monday (nday=1) on.or.after March 15, 1986? 
   on.or.after(year=1986, month=3, day=15, nday=1)	
   # What date has the Friday (nday=5) on.or.before April 22, 1977? 
   on.or.before(year=1977, month=4, day=22, nday=5)	
   # What date is the second Sunday in October 1980?
   nth.of.nday(year=1980, month=10, nday=0, nth=2)	
   # What date has the last Monday in May, 1996?
   last.of.nday(year=1996, month=5, lastday=31, nday=1)

   # Calculate the date for Easter and Pentecost from 2000 until 2010:
   Easter(2000:2010)
   Pentecost(2000:2010)

   # Create a holiday Calendar for the New York Stock Exchange for 2002:
   NYSECalendar = sort(c(NewYearsDay(2002), USMLKingsBirthday(2002), 
     USWashingtonsBirthday(2002), GoodFriday(2002), USMemorialDay(2002), 
     USIndependenceDay(2002), USLaborDay(2002), USThanksgivingDay(2002), 
     ChristmasDay(2002)))
   NYSECalendar
   sday.of.week(NYSECalendar)


################################################################################

