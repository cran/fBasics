
#
# Example:
#   Test functions and methods for holiday calendar management
#
# Author:
#	(C) 2004, Diethelm Wuertz, GPL
#


################################################################################
# Time Zone:

    # Note we need "GMT" as time zone
    # Test:
    Sys.timezone()
    
    # How to change Time Zone to GMT ?
    # Start -> Control Panel -> System
    #    Advanced -> Environment Variablees
    #      User Variables -> New
    # Add:
    #      Variable Name   TZ
    #      Variable Value  GMT
    # Ok -> Ok -> Ok
   

################################################################################
# Easter:

   currentYear         # prints current year as integer
   easter()            # date of easter this year
   easter(2000:2009)   # easter for the 2k decade  
    
   class(easter())     # what class?
   timeDate(easter())  # Convert to timeDate
    
    
################################################################################
# Holidays:

   holiday()
   holiday(2000:2009, "USLaborDay")
   class(holiday())
   print(USLaborDay(2000:2009))
    
    
################################################################################
# Time Zones and Day Light Saving Times:
   
   Sys.getenv("TZ")
   Sys.timezone()
   if (Sys.timezone() != "GMT") warning("Set timezone to GMT!")
    
   
################################################################################
# Convert to/from Local Time:

   # Start with character string:
   charvec = "2004-08-01 00:00:00"
   # Greenwich mean Time:
   GMT = timeDate(charvec, zone = "GMT", FinCenter = "GMT")
   GMT
   # From GMT to local Zurich time:
   ZUR = timeDate(GMT, zone = "GMT", FinCenter = "Europe/Zurich")
   ZUR
   # From Zurich local time to New-York local time:
   NYC = timeDate(ZUR, zone = "Europe/Zurich", FinCenter = "America/NewYork")
   NYC
   # Or, directly from GMT to New-York local time:
   NYC = timeDate(GMT, zone = "GMT", FinCenter = "America/NewYork")
   NYC

 
################################################################################
# Convert Within the Same Time Zone:

   # What time was it in Berlin at April 5th and 6th, 1980, 
   # at 4:00 PM Zurich time?
   ZURICH = c("1980-04-05 16:00:00", "1980-04-06 16:00:00")
   zone = "Europe/Zurich"
   FinCenter = "Europe/Berlin"
   BERLIN = timeDate(ZURICH, zone = zone, FinCenter = FinCenter)
   ZURICH; BERLIN
   # [1] "1980-04-05 16:00:00" "1980-04-06 16:00:00"
   # [1] "Europe/Berlin"
   # [1] [1980-04-05 16:00:00] [1980-04-06 17:00:00]
   # Note, in 1980 Switzerland had no Daylight Saving Time in 
   # contrast, to GermaNYC!


################################################################################
# Investigate changeover to DST:

   TIME = c("27 23:00:00", "27 23:15:00", "27 23:30:00", "27 23:45:00",
            "28 00:00:00", "28 00:15:00", "28 00:30:00", "28 00:45:00",
            "28 01:00:00", "28 01:15:00", "28 01:30:00", "28 01:45:00",
            "28 02:00:00", "28 02:15:00", "28 02:30:00", "28 02:45:00",
            "28 03:00:00", "28 03:15:00", "28 03:30:00", "28 03:45:00",
            "28 04:00:00", "28 04:15:00", "28 04:30:00", "28 04:45:00"  )
   GMT = paste("2004-03-", TIME, sep="")
   LONDON = timeDate(GMT, zone = "GMT", FinCenter = "Europe/London")
   GMT2 = timeDate(LONDON, zone = "Europe/London", FinCenter = "GMT")
   # Print:
   cbind(GMT, LONDON=as.character(LONDON@Data), GMT2=as.character(GMT2@Data))


################################################################################
# Some Additional Time Zone Abbreviations:

   # -------------------------------------------------------------------------
   # Time Zone Name     Other abbreviations  Time Zone    Use
   # -------------------------------------------------------------------------
   # GMT   Greenwich Mean Time          UTC  UTC          "GMT"
   # BST   British Summer  Time              UTC+1 hour   "Europe/London"
   # WET   Western European Time             UTC      
   # WEST  Western European Summer Time      UTC+1 hour   "Europe/Zurich"
   # CET   Central European Time        MEZ  UTC+1 hour 
   # CEST  Central European Summer Time MESZ UTC+2 hours 
   # EET   Eastern European Time             UTC+2 hours 
   # EEST  Eastern European Summer Time      UTC+3 hours  
   # ------------------------------------------------------------------------- 
   # AST   Atlantic Standard Time       HNA  UTC-4 hours  
   # ADT   Atlantic Daylight Time       HAA  UTC-3 hours 
   # EST   Eastern Standard Time        HNE  UTC-5 hours  "Europe/NewYork"
   # EDT   Eastern Daylight Time        HAE  UTC-4 hours 
   # CST   Central Standard Time        HNC  UTC-6 hours 
   # CDT   Central Daylight Time        HAC  UTC-5 hours   
   # MST   Mountain Standard Time       HNR  UTC-7 hours 
   # MDT   Mountain Daylight Time       HAR  UTC-6 hours 
   # PST   Pacific Standard Time        HNP  UTC-8 hours 
   # PDT   Pacific Daylight Time        HAP  UTC-7 hours 
   # -------------------------------------------------------------------------
    
  
################################################################################

  