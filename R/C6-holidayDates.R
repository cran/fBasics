
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for the code accessed (or partly included) from other R-ports:
#   R: see R's copyright and license file
#   date: Terry Therneau <therneau@mayo.edu>
#     R port by Th. Lumley <thomas@biostat.washington.edu>  K. Halvorsen 
#       <khal@alumni.uv.es>, and Kurt Hornik <Kurt.Hornik@R-project.org>
#   ts: Collected by Brian Ripley. See SOURCES
#   tseries: Compiled by Adrian Trapletti <a.trapletti@bluewin.ch>
# for ical:
#   libical: Libical is an Open Source implementation of the IETF's 
#	  iCalendar Calendaring and Scheduling protocols. (RFC 2445, 2446, 
#     and 2447). It parses iCal components and provides a C API for 
#     manipulating the component properties, parameters, and subcomponents.
#   Olsen's VTIMEZONE: These data files are released under the GNU 
#	  General Public License, in keeping with the license options of 
#     libical. 
# for the holiday database:
#   holiday information collected from the internet and governmental 
#	sources obtained from a few dozens of websites


################################################################################
# Holiday Database:
# Copyright 1997, Diethelm Wuertz
# 	www.rmetrics.org
# Required "Holiday" Functions:
#	"easter", "on.or.after", "nth.of.nday", "last.of.nday", 
# The functions return an object of class "sdate"
#	ISO-8601 formatted integers, i.e. CCYYMMDD
################################################################################


Septuagesima = function(year) {
	ans = easter(year, -63)}
Quinquagesima = function(year) {
	ans = easter(year, -49)}
AshWednesday = function(year) {
	ans = easter(year, -46)}
PalmSunday = function(year) {
	ans = easter(year, -7)}
GoodFriday = function(year) {
	ans = easter(year, -2)
	class(ans) = "sdate" 
	ans}
Easter = function(year) {
	ans = easter(year)
	class(ans) = "sdate" 
	ans}
EasterSunday = function(year) {
	ans = easter(year)
	class(ans) = "sdate" 
	ans}
EasterMonday = function(year) {
	ans = easter(year, 1)
	class(ans) = "sdate" 
	ans}
RogationSunday = function(year) {
	ans = easter(year, 35)
	class(ans) = "sdate" 
	ans}
Ascension = function(year) {
	ans = easter(year, 39)
	class(ans) = "sdate" 
	ans}
Pentecost = function(year) {
	ans = easter(year, 49)
	class(ans) = "sdate" 
	ans}
PentecostMonday	<- function(year) {
	ans = easter(year, 50)
	class(ans) = "sdate" 
	ans}
TrinitySunday = function(year) {
	ans = easter(year, 56)
	class(ans) = "sdate" 
	ans}
CorpusChristi = function(year) {
	ans = easter(year, 60)
	class(ans) = "sdate" 
	ans}

	
# ------------------------------------------------------------------------------


ChristTheKing = function(year) {
	ans = on.or.after(year, 11, 20, 0)
	class(ans) = "sdate" 
	ans}
Advent1st = function(year) {
	ans = on.or.after(year, 11, 27, 0)
	class(ans) = "sdate" 
	ans}
Advent2nd = function(year) {
	ans = on.or.after(year, 12,  4, 0)
	class(ans) = "sdate" 
	ans}
Advent3rd = function(year) {
	ans = on.or.after(year, 12, 11, 0)
	class(ans) = "sdate" 
	ans}
Advent4th = function(year) {
	ans = on.or.after(year, 12, 18, 0)
	class(ans) = "sdate" 
	ans}
ChristmasEve = function(year) {
	ans = year*10000 + 1224
	class(ans) = "sdate" 
	ans}
ChristmasDay = function(year) {
	ans = year*10000 + 1225
	class(ans) = "sdate"
	ans }
BoxingDay = function(year) {
	ans = year*10000 + 1226
	class(ans) = "sdate"
	ans }

	
# ------------------------------------------------------------------------------

	
SolemnityOfMary	<- function(year) {
	ans = year*10000 + 0101
	class(ans) = "sdate"
	ans }
Epiphany = function(year) {
	ans = year*10000 + 0106
	class(ans) = "sdate"
	ans }
PresentationOfLord = function(year) {
	ans = year*10000 + 0202
	class(ans) = "sdate"
	ans }
Annunciation = function(year) {
	ans = year*10000 + 0325
	class(ans) = "sdate"
	ans }
TransfigurationOfLord = function(year) {
	ans = year*10000 + 0806
	class(ans) = "sdate"
	ans }
AssumptionOfMary = function(year) {
	ans = year*10000 + 0815
	class(ans) = "sdate"
	ans }
BirthOfVirginMary = function(year) {
	ans = year*10000 + 0908
	class(ans) = "sdate"
	ans }
CelebrationOfHolyCross	<- function(year) {
	ans = year*10000 + 0914
	class(ans) = "sdate"
	ans }
MassOfArchangels = function(year) {
	ans = year*10000 + 0929
	class(ans) = "sdate"
	ans }
AllSaints = function(year) {
	ans = year*10000 + 1101
	class(ans) = "sdate"
	ans }
AllSouls = function(year) {
	ans = year*10000 + 1102
	class(ans) = "sdate"
	ans }

	
# ------------------------------------------------------------------------------


NewYearsDay = function(year) {
	ans = year*10000 + 0101
	class(ans) = "sdate"
	ans }
LaborDay = function(year) {
	ans = year*10000 + 0501
	class(ans) = "sdate"
	ans }
	
	
# ------------------------------------------------------------------------------
	

CHBerchtoldsDay	= function(year) {
	ans = year*10000 + 0102
	class(ans) = "sdate"
	ans }
CHSechselaeuten	= function(year) {
	ans = NULL
	for (y in year) {
		theDate = nth.of.nday(y, 4, 1, 3)
		if (theDate == easter(y, +1)) {
			theDate = nth.of.nday(y, 4, 1, 4) }
		ans = c(ans, theDate) }
	class(ans) = "sdate"
	ans
	}
CHAscension = function(year) {
	ans = easter(year, 39)
	class(ans) = "sdate" 
	ans}
CHConfederationDay = function(year) {
	ans = year*10000 + 0801
	class(ans) = "sdate"
	ans }
CHKnabenschiessen = function(year) {
	ans = nth.of.nday(year, 9, 1, 2)
	class(ans) = "sdate" 
	ans}

		
# ------------------------------------------------------------------------------


GBMayDay = function(year) {
	ans = nth.of.nday(year, 5, 1, 1)
	class(ans) = "sdate" 
	ans}
GBBankHoliday = function(year) {
	ans = last.of.nday(year, 5, 31, 1)
	class(ans) = "sdate" 
	ans}
GBSummerBankHoliday = function(year) {
	ans = last.of.nday(year, 8, 31, 1)
	class(ans) = "sdate" 
	ans}
GBMilleniumDay = function(year) {
	ans = 19991231
	class(ans) = "sdate"
	ans }

		
# ------------------------------------------------------------------------------


DEAscension = function(year) {
	ans = easter(year, 39)
	class(ans) = "sdate" 
	ans}
DECorpusChristi	<- function(year) {
	ans = easter(year, 60)
	class(ans) = "sdate" 
	ans}
DEGermanUnity = function(year) {
	ans = year*10000 + 1003
	class(ans) = "sdate"
	ans }
DEChristmasEve = function(year) {
	ans = year*10000 + 1224
	class(ans) = "sdate"
	ans }
DENewYearsEve = function(year) {
	ans = year*10000 + 1231
	class(ans) = "sdate"
	ans }

		
# ------------------------------------------------------------------------------


FRFetDeLaVictoire1945 = function(year) {
	ans = year*10000 + 0508
	class(ans) = "sdate"
	ans }
FRAscension = function(year) {
	easter(year, 39)}
FRBastilleDay = function(year) {
	ans = year*10000 + 0714
	class(ans) = "sdate"
	ans }
FRAssumptionVirginMary = function(year) {
	ans = year*10000 + 0815
	class(ans) = "sdate"
	ans }
FRAllSaints = function(year) {
	ans = year*10000 + 1101
	class(ans) = "sdate"
	ans }
FRArmisticeDay = function(year) {
	ans = year*10000 + 1111
	class(ans) = "sdate"
	ans }

	
# ------------------------------------------------------------------------------


ITEpiphany = function(year) {
	ans = year*10000 + 0106
	class(ans) = "sdate"
	ans }
ITLiberationDay	<- function(year) {
	ans = year*10000 + 0425
	class(ans) = "sdate"
	ans }
ITAssumptionOfVirginMary = function(year) {
	ans = year*10000 + 0815
	class(ans) = "sdate"
	ans }
ITAllSaints = function(year) {
	ans = year*10000 + 1101
	class(ans) = "sdate"
	ans }
ITStAmrose = function(year) {
	ans = year*10000 + 1207
	class(ans) = "sdate"
	ans }
ITImmaculateConception = function(year) {
	ans = year*10000 + 1208
	class(ans) = "sdate"
	ans }

	
# ------------------------------------------------------------------------------


USNewYearsDay = function(year) {
	ans = year*10000 + 0101
	class(ans) = "sdate"
	ans }
USInaugurationDay = function(year) {
	ans = year*10000 + 0120
	class(ans) = "sdate"
	ans }
USMLKingsBirthday = function(year) {
	ans = nth.of.nday(year, 1, 1, 3)
	class(ans) = "sdate" 
	ans}
USLincolnsBirthday = function(year) {
	ans = year*10000 + 0212
	class(ans) = "sdate"
	ans }
USWashingtonsBirthday = function(year) {
	ans = nth.of.nday(year, 2, 1, 3)
	class(ans) = "sdate" 
	ans}
USMemorialDay = function(year) {
	ans = last.of.nday(year, 5, 31, 1)
	class(ans) = "sdate" 
	ans}
USIndependenceDay = function(year) {
	ans = year*10000 + 0704
	class(ans) = "sdate"
	ans }
USLaborDay = function(year) {
	ans = nth.of.nday(year, 9, 1, 1)
	class(ans) = "sdate" 
	ans}
USColumbusDay = function(year) {
	ans = nth.of.nday(year, 10, 1, 2)
	class(ans) = "sdate" 
	ans}
USElectionDay = function(year) {
	ans = on.or.after(year, 11, 2, 2)
	class(ans) = "sdate" 
	ans}
USVeteransDay = function(year) {
	ans = year*10000 + 1111
	class(ans) = "sdate"
	ans }
USThanksgivingDay  = function(year) {
	ans = nth.of.nday(year, 11, 4, 4)}
USChristmasDay = function(year) {
	ans = year*10000 + 1225
	class(ans) = "sdate"
	ans }
USCPulaskisBirthday = function(year) {
	ans = nth.of.nday(year, 3, 1, 1)
	class(ans) = "sdate" 
	ans }
USGoodFriday = function(year) {
	ans = easter(year, -2)
	class(ans) = "sdate" 
	ans}
USPresidentsDay = function(year) {
	ans = nth.of.nday(year, 2, 1, 3) 
	class(ans) = "sdate" 
	ans }
USDecorationMemorialDay = function(year) {
	ans = year*10000 + 0530
	class(ans) = "sdate"
	ans }

	
# ------------------------------------------------------------------------------


CAVictoriaDay = function(year) {
	ans = on.or.before(year, 5, 24, 1)
	class(ans) = "sdate" 
	ans}
CACanadaDay = function(year) {
	ans = year*10000 + 0701
	class(ans) = "sdate"
	ans }
CACivicProvincialHoliday = function(year) {
	ans = nth.of.nday(year, 8, 1, 1)
	class(ans) = "sdate" 
	ans }
CALabourDay = function(year) {
	nth.of.nday(year, 9, 1, 1)}
CAThanksgivingDay = function(year) {
	ans = nth.of.nday(year, 10, 1, 2)
	class(ans) = "sdate" 
	ans }
CaRemembranceDay = function(year) {
	ans = year*10000 + 1111
	class(ans) = "sdate"
	ans }

	
# ------------------------------------------------------------------------------


JPNewYearsDay = function(year) {
	ans = year*10000 + 0101
	class(ans) = "sdate"
	ans }
JPGantan = function(year) {
	ans = year*10000 + 0101
	class(ans) = "sdate"
	ans }
JPBankHolidayJan2 = function(year) {
	ans = year*10000 + 0102
	class(ans) = "sdate"
	ans }
JPBankHolidayJan3 = function(year) {
	ans = year*10000 + 0103
	class(ans) = "sdate"
	ans }
JPComingOfAgeDay = function(year) {
	ans = year*10000 + 0115
	class(ans) = "sdate"
	ans }
JPSeijinNoHi = function(year) {
	ans = year*10000 + 0115
	class(ans) = "sdate"
	ans }
JPNatFoundationDay = function(year) {
	ans =year*10000 + 0211
	class(ans) = "sdate"
	ans }
JPKenkokuKinenNoHi = function(year) {
	ans = year*10000 + 0211
	class(ans) = "sdate"
	ans }
JPGreeneryDay = function(year) {
	ans = year*10000 + 0429
	class(ans) = "sdate"
	ans }
JPMidoriNoHi = function(year) {
	ans = year*10000 + 0429
	class(ans) = "sdate"
	ans }
JPConstitutionDay = function(year) {
	ans = year*10000 + 0503
	class(ans) = "sdate"
	ans }
JPKenpouKinenBi = function(year) {
	ans = year*10000 + 0503
	class(ans) = "sdate"
	ans }
JPNationHoliday = function(year) {
	ans = year*10000 + 0504
	class(ans) = "sdate"
	ans }
JPKokuminNoKyujitu = function(year) {
	ans = year*10000 + 0504
	class(ans) = "sdate"
	ans }
JPChildrensDay = function(year) {
	ans = year*10000 + 0505
	class(ans) = "sdate"
	ans }
JPKodomoNoHi = function(year) {
	ans = year*10000 + 0505
	class(ans) = "sdate"
	ans }
JPMarineDay = function(year) {
	ans = year*10000 + 0720
	class(ans) = "sdate"
	ans }
JPUmiNoHi = function(year) {
	ans = year*10000 + 0720
	class(ans) = "sdate"
	ans }
JPRespectForTheAgedDay = function(year) {
	ans = year*10000 + 0915
	class(ans) = "sdate"
	ans }
JPKeirouNOhi = function(year) {
	ans = year*10000 + 0915
	class(ans) = "sdate"
	ans }
JPAutumnalEquinox = function(year) {
	ans = year*10000 + 0924
	class(ans) = "sdate"
	ans }
JPShuubunNoHi = function(year) {
	ans =year*10000 + 0924
	class(ans) = "sdate"
	ans }
JPHealthandSportsDay = function(year) {
	ans = year*10000 + 1010
	class(ans) = "sdate"
	ans }
JPTaiikuNoHi = function(year) {
	ans = year*10000 + 1010
	class(ans) = "sdate"
	ans }
JPNationalCultureDay = function(year) {
	ans = year*10000 + 1103
	class(ans) = "sdate"
	ans }
JPBunkaNoHi = function(year) {
	ans = year*10000 + 1103
	class(ans) = "sdate"
	ans }
JPThanksgivingDay = function(year) {
	ans = year*10000 + 1123
	class(ans) = "sdate"
	ans }
JPKinrouKanshaNoHi = function(year) {
	ans = year*10000 + 1123
	class(ans) = "sdate"
	ans }
JPEmperorsBirthday = function(year) {
	ans = year*10000 + 1123
	class(ans) = "sdate"
	ans }
JPTennouTanjyouBi = function(year) {
	year*10000 + 1123
	class(ans) = "sdate"
	ans }
JPBankHolidayDec31 = function(year) {
	ans =year*10000 + 1231
	class(ans) = "sdate"
	ans }

	
# ------------------------------------------------------------------------------

