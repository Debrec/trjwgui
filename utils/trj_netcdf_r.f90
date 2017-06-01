
MODULE trj_netcdf_r
  USE netcdf
  IMPLICIT none
  PRIVATE
  PUBLIC :: read_nc_file, dim_nc_file,read_dim_nc_file

  INTEGER :: ncid, status, varid
  INTEGER :: nvar, ndim, natts

CONTAINS

  SUBROUTINE nf(status) ! turns nf90_* function into subroutine + checks status
    ! Part of mo_netcdf.f90, MPI
    INTEGER :: status
    IF (status /= nf90_noerr) THEN
       WRITE (*,*) 'netcdf error: ', nf90_strerror(status)
       STOP
    ENDIF
  END SUBROUTINE nf

  SUBROUTINE dim_nc_file(filename,namedim,nro,nrodim)
    IMPLICIT none
    CHARACTER(*), INTENT(IN) :: filename, namedim(:)
    INTEGER, INTENT(IN) :: nrodim
    INTEGER, INTENT(OUT) :: nro(:)

    INTEGER Jdim,len

    CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))

    DO Jdim=1,nrodim
       CALL nf(NF90_INQ_VARID(ncid,namedim(Jdim),varid))
       CALL nf(NF90_INQUIRE_DIMENSION(ncid,varid,len=len))
       WRITE(*,*) namedim(Jdim), len
       nro(Jdim)=len
    END DO

    CALL nf(NF90_CLOSE(ncid))

  END SUBROUTINE dim_nc_file

  SUBROUTINE read_dim_nc_file(filename,namedim,datadim)
    IMPLICIT none
    CHARACTER(*), INTENT(IN) :: filename, namedim
    REAL, INTENT(OUT) :: datadim(:)

    CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))

    CALL nf(NF90_INQ_VARID(ncid,namedim,varid))
    CALL nf(NF90_GET_VAR(ncid,varid,datadim))

    CALL nf(NF90_CLOSE(ncid))

  END SUBROUTINE read_dim_nc_file

  SUBROUTINE read_nc_file(filename,varname,var,nro)
    IMPLICIT none

    CHARACTER(*), INTENT(IN) :: filename, varname
    INTEGER :: nro(:)
    REAL, INTENT(OUT) :: var(nro(1),nro(2))
    !CHARACTER(*), INTENT(OUT) :: units


    CALL nf(NF90_OPEN(PATH=filename,mode=nf90_nowrite,NCID=ncid))

    CALL nf(NF90_INQ_VARID(ncid,varname, varid))
    CALL nf(NF90_GET_VAR(ncid,varid,var(:,:)))
!    CALL nf(NF90_GET_ATT(ncid,varid,"units",units))

    CALL nf(NF90_CLOSE(ncid))

  END SUBROUTINE read_nc_file

END MODULE trj_netcdf_r


SUBROUTINE inttochar(I,wdat)

  INTEGER, INTENT(IN) :: I
  CHARACTER(*), INTENT(OUT) :: wdat

  CHARACTER*1 :: W(4)
  INTEGER :: J1,J2,J3,J4

  IF(I.LT.10) THEN
     J1=INT(REAL(MOD(I,10)))
     W(1)=CHAR(J1+48)
     wdat=W(1)
  ELSE IF(I.LT.100) THEN
     J1=INT(REAL(MOD(I,100))/10.0)
     J2=INT(REAL(MOD(I,10)))
     W(1)=CHAR(J1+48)
     W(2)=CHAR(J2+48)
     wdat=W(1)//W(2)
  ELSE IF(I.LT.1000) THEN
     J1 = INT(REAL(MOD(I,1000))/100.0)
     J2 = INT(REAL(MOD(I,100))/10.0)
     J3 = INT(REAL(MOD(I,10)))
     W(1)=CHAR(J1+48)
     W(2)=CHAR(J2+48)
     W(3)=CHAR(J3+48)
     wdat=W(1)//W(2)//W(3)
  ELSE IF(I.LT.10000) THEN
     J1 = INT(REAL(MOD(I,10000))/1000.0)
     J2 = INT(REAL(MOD(I,1000))/100.0)
     J3 = INT(REAL(MOD(I,100))/10.0)
     J4 = INT(REAL(MOD(I,10)))
     W(1)=CHAR(J1+48)
     W(2)=CHAR(J2+48)
     W(3)=CHAR(J3+48)
     W(4)=CHAR(J4+48)
     wdat=W(1)//W(2)//W(3)//W(4)
  END IF

  RETURN
END SUBROUTINE inttochar


SUBROUTINE read_data(filein,path)
  USE trj_netcdf_r
  USE date_conv
  IMPLICIT none
  CHARACTER(*) path,filein
  INTEGER nrop, nt,nro(2)
  REAL, ALLOCATABLE :: particle(:)
  REAL, ALLOCATABLE :: time(:)
  REAL, ALLOCATABLE :: Temp(:,:),Pres(:,:),Lat(:,:),Lon(:,:)
  REAl, ALLOCATABLE :: O3(:,:),PV(:,:)
  REAL(8) :: tyear,tmonth,tday,thour
  CHARACTER*1 chnro1
  CHARACTER*2 chnro2
  CHARACTER*3 chnro3
  CHARACTER*4 chnro4
  INTEGER ip,it

  CALL dim_nc_file(filein,(/'particle','time    '/),nro,2)
  nrop=nro(1); nt=nro(2);

  ALLOCATE(particle(nrop),time(nt),Temp(nrop,nt),&
       Pres(nrop,nt),Lat(nrop,nt),Lon(nrop,nt))
  ALLOCATE(O3(nrop,nt),PV(nrop,nt))

  CALL read_dim_nc_file(filein,'particle',particle)
  CALL read_dim_nc_file(filein,'time    ',time)


  CALL read_nc_file(filein,'Temp',Temp,nro)
  CALL read_nc_file(filein,'Press',Pres,nro)
  CALL read_nc_file(filein,'O3',O3,nro)
  CALL read_nc_file(filein,'Pv',PV,nro)
  CALL read_nc_file(filein,'lat',Lat,nro)
  CALL read_nc_file(filein,'Long',Lon,nro)

  !DO it=1,nt,96
  !    CALL jul2greg(DBLE(time(it)),tyear,tmonth,tday,thour)
  !    WRITE(*,FMT='(I4,X,I2.2,"/",I2.2,"/",I4,X,I2.2,":",I2.2,":",I2.2)')it,  &
  !         NINT(tday),NINT(tmonth),NINT(tyear),FLOOR(thour),FLOOR((thour-FLOOR(thour))*60.), &
  !         FLOOR(((thour-FLOOR(thour))*60.-FLOOR((thour-FLOOR(thour))*60.))*60.)
  !
  !   OPEN(15,FILE='trjmd'//CHAR(NINT((it-1)/96.)+48)//'.dat',STATUS='replace')
  !   DO ip=1,nrop
  !      WRITE(15,FMT='(4F13.5,2E15.7)')Lon(ip,it),Lat(ip,it),Pres(ip,it),&
  !           Temp(ip,it),O3(ip,it),PV(ip,it)
  !   END DO
  !   CLOSE(15)
  !END DO

  DO it=1,nt
     CALL jul2greg(REAL(time(it)),tyear,tmonth,tday,thour)
     WRITE(*,FMT='(I4,X,I2.2,"/",I2.2,"/",I4,X,I2.2,":",I2.2,":",I2.2)')it,  &
          NINT(tday),NINT(tmonth),NINT(tyear),FLOOR(thour),FLOOR((thour-FLOOR(thour))*60.), &
          FLOOR(((thour-FLOOR(thour))*60.-FLOOR((thour-FLOOR(thour))*60.))*60.)

     IF(it.LT.10) THEN
        CALL inttochar(it,chnro1)
        OPEN(15,FILE=path//'trjmd'//chnro1//'.dat',STATUS='replace')
     ELSE IF(it.LT.100) THEN
        CALL inttochar(it,chnro2)
        OPEN(15,FILE=path//'trjmd'//chnro2//'.dat',STATUS='replace')
     ELSE IF(it.LT.1000) THEN
        CALL inttochar(it,chnro3)
        OPEN(15,FILE=path//'trjmd'//chnro3//'.dat',STATUS='replace')
     ELSE IF(it.LT.10000) THEN
        CALL inttochar(it,chnro4)
        OPEN(15,FILE=path//'trjmd'//chnro4//'.dat',STATUS='replace')
     END IF
     DO ip=1,nrop
        WRITE(15,FMT='(4F13.5,2E15.7)')Lon(ip,it),Lat(ip,it),Pres(ip,it),&
             Temp(ip,it),O3(ip,it),PV(ip,it)
     END DO
     CLOSE(15)
  END DO

  DEALLOCATE(particle,time,Temp,Pres,Lat,Lon)
END SUBROUTINE read_data

PROGRAM test
  IMPLICIT none

  CALL read_data('../output/trj_output.nc','../output/')

END PROGRAM test
