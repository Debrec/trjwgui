!-----------------------------------------------------------------------
! Subrutinas para transformar de tiempo gregoriano a tiempo serial y viceversa,
!-----------------------------------------------------------------------
MODULE date_conv
  PRIVATE
  ! time is the time in hours
  INTEGER, PARAMETER, PUBLIC :: dp = SELECTED_REAL_KIND(12,307)
  REAL(dp), PARAMETER :: defyear=1900.,defmonth=1.,defday=1.,deftime=0.
  REAL(dp) :: refyear,refmonth,refday,reftime
  REAL(dp) :: year, month, day, time
  REAL(dp) :: serial_date !serial date in hours

  PUBLIC :: greg2jul, jul2greg, julday, gregday!Public function

  !---------------------------------------------------------------------
CONTAINS
  !---------------------------------------------------------------------

  ! Return day of the year
  REAL(dp) FUNCTION julday(iyear,imonth,iday,itime)
    IMPLICIT none
    REAL(dp), INTENT(IN)::iyear,imonth,iday,itime

    refyear=iyear; refmonth=1.; refday=1.; reftime=0.;
    ! itime is the time in hours
    year=iyear
    month=imonth
    day=iday
    time=itime
    julday=ddate()/24.

  END FUNCTION JULDAY

  !---------------------------------------------------------------------

  !Return the gregorian date given the julian date
  SUBROUTINE gregday(jday,year,omonth,oday)
    IMPLICIT none
    REAL(dp), INTENT(IN) :: jday,year
    REAL(dp), INTENT(OUT) :: omonth ,oday

    INTEGER inty, j, iyear,iday, imonth
    REAL(dp) ::  var, dayp
    INTEGER :: NDAY(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    INTEGER :: DAYI,I,INDAY

    iyear=INT(year)

    inty=jday

    iday=inty

    I=1
    imonth=1
    INDAY=NDAY(I)
    DO WHILE(FLOOR(REAL(iday,dp)/REAL(INDAY,dp)).GE.1)
       IF((I.EQ.2).AND.((iday/(NDAY(I)+leap(iyear))).GE.1)) THEN
          imonth=imonth+1
          iday=iday-NDAY(I)-leap(iyear)
          I=I+1
       ELSE IF(I.NE.2) THEN
          imonth=imonth+1
          iday=iday-NDAY(I)
          I=I+1
       END IF
       IF(I.EQ.2) THEN
          INDAY=NDAY(I)+leap(iyear)
       ELSE
          INDAY=NDAY(I)
       END IF
    END DO

    oday=REAL(iday,dp)+1._dp ! iday is the number of day, but we begin for 1 not 0
    omonth=REAL(imonth,dp)

  END SUBROUTINE gregday


  !Return julian date given the gregorian date
  REAL(dp) FUNCTION greg2jul(iyear,imonth,iday,itime)
    IMPLICIT none
    REAL(dp), INTENT(IN)::iyear,imonth,iday,itime

    refyear=defyear; refmonth=defmonth; refday=defday; reftime=deftime;

    ! itime is the time in hours
    year=iyear
    month=imonth
    day=iday
    time=itime
    serial_date=ddate() ! Function ddate
    greg2jul=serial_date
  END FUNCTION greg2jul

  !---------------------------------------------------------------------
  !---------------------------------------------------------------------

  !Return the gregorian date given the julian date
  SUBROUTINE jul2greg(ryear,oyear,omonth,oday,ohour)
    IMPLICIT none
    REAL(dp), INTENT(IN) :: ryear
    REAL(dp), INTENT(OUT) :: oyear ,omonth ,oday ,ohour

    INTEGER inty, j, iyear, imonth, iday, ihour
    REAL(dp) ::  var, dayp
    INTEGER :: NDAY(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    INTEGER :: DAYI,I,INDAY

    refyear=defyear; refmonth=defmonth; refday=defday; reftime=deftime;


    ohour=24.*(ryear/24.-FLOOR(ryear/24.))
    inty=FLOOR((ryear)/24.)! number of days

    iyear=NINT(refyear)

    iday=inty

    IF(iday.GE.365.) THEN ! 1900 is not a leap year.
       iyear=iyear+1
       iday=iday-365.
    END IF

    DO WHILE(iday.GE.(365.+leap(iyear)))
       iday=iday-365-leap(iyear)
       iyear=iyear+1
    END DO

    ! WRITE(*,*) iday,leap(iyear)

    I=1
    imonth=1
    INDAY=NDAY(I)
    DO WHILE(FLOOR(REAL(iday,dp)/REAL(INDAY,dp)).GE.1)
       IF((I.EQ.2).AND.((iday/(NDAY(I)+leap(iyear))).GE.1)) THEN
          imonth=imonth+1
          iday=iday-NDAY(I)-leap(iyear)
          I=I+1
       ELSE IF(I.NE.2) THEN
          imonth=imonth+1
          iday=iday-NDAY(I)
          I=I+1
       END IF
       IF(I.EQ.2) THEN
          INDAY=NDAY(I)+leap(iyear)
       ELSE
          INDAY=NDAY(I)
       END IF
       !WRITE(*,*)I, iday,imonth
    END DO

    oyear=REAL(iyear,dp)
    oday=REAL(iday,dp)+1._dp ! iday is the number of day, but we begin for 1 not 0
    omonth=REAL(imonth,dp)

  END SUBROUTINE jul2greg

  !---------------------------------------------------------------------
  !---------------------------------------------------------------------

  !Return the difference between two dates in hours
  REAL(dp) FUNCTION ddate()
    IMPLICIT none
    INTEGER iyear,imonth,idays,leap
    INTEGER :: NDAY(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)

    ddate = time-reftime
    ddate = ddate + (day-refday)*24.0

    DO imonth=Int(refmonth),INT(month)-1
       IF(imonth.EQ.2) THEN
          idays=NDAY(imonth) + REAL(leap(NINT(year)))
       ELSE
          idays=NDAY(imonth)
       END IF
       ddate=ddate+REAL(idays)*24.0
    END DO

    DO iyear = INT(refyear)+1,INT(year)-1
       idays=365 + leap(iyear)
       ddate=ddate+REAL(idays)*24.0
    END DO
    IF(refyear.NE.year) THEN
       ddate=ddate+365.*24.
    END IF

  END FUNCTION ddate

  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------

  INTEGER FUNCTION leap(year1)
    IMPLICIT none
    INTEGER, INTENT(IN) :: year1

    IF(year1.EQ.4*NINT(0.25*REAL(year1)))THEN
       IF(year1.EQ.100*NINT(0.01*REAL(year1)))THEN
          IF(year1.EQ.400*NINT(0.0025*REAL(year1)))THEN
             leap=1
          ELSE
             leap=0
          ENDIF
       ELSE
          leap=1
       ENDIF
    ELSE
       leap=0
    ENDIF

  END FUNCTION leap

  !---------------------------------------------------------------------
END MODULE date_conv
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

INTEGER FUNCTION leap(year1)
  IMPLICIT none
  INTEGER, INTENT(IN) :: year1

  IF(year1.EQ.4*NINT(0.25*REAL(year1)))THEN
     IF(year1.EQ.100*NINT(0.01*REAL(year1)))THEN
        IF(year1.EQ.400*NINT(0.0025*REAL(year1)))THEN
           leap=1
        ELSE
           leap=0
        ENDIF
     ELSE
        leap=1
     ENDIF
  ELSE
     leap=0
  ENDIF

END FUNCTION leap

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
