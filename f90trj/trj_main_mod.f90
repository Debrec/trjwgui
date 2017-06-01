!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Modulo Principal
!----------------------------------------------------------------------
!----------------------------------------------------------------------
MODULE trj_main_mod
  USE date_conv, ONLY : greg2jul, jul2greg
  USE trj_netcdf_w,  ONLY : open_trj_nc_file, write_trj_nc_file , close_trj_nc_file
  IMPLICIT none
  PRIVATE
  INTEGER, PUBLIC :: Ntime, Npart, Nout
  INTEGER Npartx, Nparty
  REAL mLon,iLon, mLat, iLat
  REAL, ALLOCATABLE, PUBLIC :: Long(:,:),Lat(:,:), P(:,:)
  REAL, ALLOCATABLE :: T(:,:), &
       U(:,:),V(:,:),time(:),PV(:,:),O3(:,:)
  INTEGER isec,day,mon,year ! initial date
  REAL theta,rtime,irtime ! theta surface
  INTEGER :: its, ncid_trj  !its (time step, (min)), ncid_trj (netcdf id)

  PUBLIC :: trj_init, trj1, trj_finish

CONTAINS

  ! read data dimensions and reserve memory space for data storage
  !----------------------------------------------------------------------

  SUBROUTINE read_conf_file(windfile,tracfile,GF)

    IMPLICIT none
    CHARACTER*40, INTENT(OUT) :: windfile,tracfile
    INTEGER, INTENT(OUT) :: GF

    CHARACTER*20 CFGFILE
    CFGFILE='trj-2d.cfg'
    OPEN(17,FILE=CFGFILE)
    READ(17,*)Ntime, Nout
    READ(17,*)Npartx,Nparty
    READ(17,*)mLon,iLon,mLat,iLat
    READ(17,*)windfile,tracfile
    READ(17,*)isec,day,mon,year
    READ(17,*)theta
    READ(17,*)its
    READ(17,*)GF
    CLOSE(17)
    WRITE(*,*)'input wind data: ', windfile
    WRITE(*,*)'tracer data for: ', tracfile
    WRITE(*,FMT='(A8,X,I4,X,A8,X,I4)') "Ntime = ",Ntime,"Nout = ",Nout
    WRITE(*,FMT='(A8,X,I3,2X,A8,X,I4)')"Npartx= ",Npartx,"Nparty= ",Nparty
    WRITE(*,FMT='(4F12.2)') mLon,iLon,mLat,iLat
    WRITE(*,FMT='(A6,I6,I2,X,I2,X,I4)')'Date: ',isec,day,mon,year
    WRITE(*,FMT='(A6,F12.2)')'Potent. Temp.: ',theta
    Npart=Npartx*Nparty

    ALLOCATE(Long(Npart,2),lat(Npart,2),T(Npart,2), &
         P(Npart,2),U(Npart,2),V(Npart,2),time(2), &
         PV(Npart,2),O3(Npart,2))

  END SUBROUTINE read_conf_file

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

  SUBROUTINE trj_init
    USE winds, ONLY : readd,updatew,filein,filetrc
    IMPLICIT none
    INTEGER GF
    INTEGER ipartx,iparty,I
    INTEGER itime,ipart,jtraj

    CALL read_conf_file(filein,filetrc,GF)! read the configuration file and allocate
                                   ! room for the arrays
    jtraj=10
    WRITE(0,*) 'Inicializando'
    IF(GF.EQ.0) THEN  ! Define an uniform grid using the data
       WRITE(*,*)'Initial uniform grid'
       DO ipartx=1,Npartx  ! provided in the initialization file
          DO iparty=1,Nparty
             Lat((ipartx-1)*Nparty+iparty,1)=mLat+(iparty-1)*iLat
             Long((ipartx-1)*Nparty+iparty,1)=mLon+(ipartx-1)*iLon
          END DO
       END DO
    ELSE IF(GF.EQ.1) THEN ! Read de initial distribution
       OPEN(13,FILE='input/temp_trj.dat')   !from a previously writed
       DO i=1,Npart                   !file. This is used to
          READ(13,*)Long(i,1),Lat(i,1)!get an uniform grid at
       END DO                         !the end, using for that
       CLOSE(13)                      !calculated backwards
    END IF                            !trajectorys as initial
                                      !positions.

    WRITE(6,*) 'Cargando datos vientos'
    CALL readd

    WRITE(6,*) 'Iniciando bucle de tiempo'
    CALL open_trj_nc_file(ncid_trj, 'output/trj_output', &
          (/'Long ', 'lat  ','Temp ','Press','Pv   ','O3   '/), &
         (/'Deg','Deg','K  ','mb ','mks','mmr'/),Npart)
    time(1)=isec

    irtime=greg2jul(REAL(year),REAL(mon),REAL(day),REAL(isec)/3600.)
	CALL updatew(theta,irtime)

  END SUBROUTINE trj_init

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

  SUBROUTINE trj1(time1)
    !--------------------------------------------------------------------
    USE date_conv, ONLY : jul2greg
    USE winds, ONLY : updatew
    USE parametros
    USE thsurf, ONLY : update,T_surf,P_surf,dive_surf,lat_surf, &
         long_surf
    USE particle, ONLY : lat_part,long_part,trayect,T_part,P_part,&
         latcheck2,U_part,V_part,pv_part,o3_part


    !--------------------------------------------------------------------
    IMPLICIT none
    !--------------------------------------------------------------------
    INTEGER time1
    !        Variables locales
    INTEGER itime,ipart,jtraj,itout
    INTEGER ipartx,iparty,i,j
    REAL tyear,tmonth,tday,thour
    !----------------------------------------------------------------------

    itime=time1

    itout=NINT(REAL((itime-1)*(Nout-1)/(Ntime-1))+1.)

    rtime=irtime+REAL(itime-1)*its/60.
    CALL updatew(theta,rtime)

    time(2)=60*its+time(1)
    DO ipart=1,Npart
       lat_part=Lat(ipart,1)
       long_part=Long(ipart,1)
       CALL update()
       T(ipart,1)=T_part
       P(ipart,1)=P_part
       U(ipart,1)=U_part
       V(ipart,1)=V_part
       PV(ipart,1)=pv_part
       O3(ipart,1)=o3_part
			 WRITE(*,*) "Procesando particula: ",ipart
       CALL trayect(0.,time(1),time(2))
       CALL Latcheck2()

          Lat(ipart,2)=lat_part
          Long(ipart,2)=long_part
          T(ipart,2)=T_part
          P(ipart,2)=P_part
          U(ipart,2)=U_part
          V(ipart,2)=V_part
          PV(ipart,2)=pv_part
          O3(ipart,2)=o3_part

       END DO

    DO i=1,Npart
      CALL rangcheck(Long(i,1),Lat(i,1))
    END DO
		IF((FLOOR(REAL(Ntime*(itout)/Nout)).EQ.(itime)).OR.(itime.EQ.1)) THEN
		   CALL jul2greg(rtime,tyear,tmonth,tday,thour)
       CALL write_trj_nc_file(ncid_trj,rtime, &
            (/'Long ', 'lat  ','Temp ','Press','Pv   ','O3   '/) ,(/Long(:,1),&
            lat(:,1),T(:,1),P(:,1),PV(:,1),O3(:,1)/))
    END IF

    CALL jul2greg(rtime,tyear,tmonth,tday,thour)
    WRITE(0,FMT='(A,F12.2,I4,X,I2.2,"/",I2.2,"/",I4,X,I2.2,":",I2.2,":",I2.2,X,"%",F5.2)') 'Tiempo',time(1),itime, &
         NINT(tday),NINT(tmonth),NINT(tyear),FLOOR(thour),FLOOR((thour-FLOOR(thour))*60.), &
         FLOOR(((thour-FLOOR(thour))*60.-FLOOR((thour-FLOOR(thour))*60.))*60.), REAL(100.*REAL(itime)/REAL(Ntime))

    Lat(:,1)=Lat(:,2)
    Long(:,1)=Long(:,2)

    time(1)=time(2)
    IF(itime.EQ.(Ntime-1)) THEN
       DO i=1,Npart
          CALL rangcheck(Long(i,2),Lat(i,2))
       END DO
       rtime=irtime+REAL(Ntime-1)*its/60.
       CALL jul2greg(rtime,tyear,tmonth,tday,thour)
       itime = itime + 1
       WRITE(0,FMT='(A,F12.2,I4,X,I2.2,"/",I2.2,"/",I4,X,I2.2,":",I2.2,":",I2.2,X,"%",F6.2)') 'Tiempo',time(1),itime, &
       NINT(tday),NINT(tmonth),NINT(tyear),FLOOR(thour),FLOOR((thour-FLOOR(thour))*60.), &
       FLOOR(((thour-FLOOR(thour))*60.-FLOOR((thour-FLOOR(thour))*60.))*60.),REAL(100.*REAL(itime)/REAL(Ntime))

       CALL write_trj_nc_file(ncid_trj,rtime, &
            (/'Long ', 'lat  ','Temp ','Press','Pv   ','O3   '/) ,(/Long(:,2),&
            lat(:,2),T(:,2),P(:,2),PV(:,2),O3(:,2)/))
    END IF

    !--------------------------------------------------------------------
  END SUBROUTINE trj1

  SUBROUTINE trj_finish
    USE winds, ONLY : winds_finish
    IMPLICIT none
    CALL close_trj_nc_file(ncid_trj)
    DEALLOCATE(Long,lat,T, &
         P,U,V,time, &
         PV,O3)
    WRITE(0,*)
    WRITE(0,*) 'Terminado'

    CALL winds_finish
  END SUBROUTINE trj_finish

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------
  SUBROUTINE rangcheck(olong,olat)
    IMPLICIT none
    REAL olong, olat

    DO WHILE((olat.GT.90).OR.(olat.LT.(-90)))

       IF(olat.GT.90) THEN
          olat=180.-olat
          olong=180.+olong
       END IF
       IF(olat.LT.(-90)) THEN
          olat=-180.-olat
          olong=180.+olong
       END IF

    END DO

    DO WHILE((olong.GT.360).OR.(olong.LT.0))
       IF(olong.GT.360)olong=olong-360.
       IF(olong.LT.0)olong=olong+360.
    END DO

  END SUBROUTINE rangcheck

END MODULE trj_main_mod

!---------------------------------------------------------------------------------

!/////////////////////////////////////////////////////////////////////////////////

!---------------------------------------------------------------------------------

MODULE trj_main_mod_3d
  USE date_conv, ONLY : greg2jul, jul2greg
  USE trj_netcdf_w, ONLY : open_trj_nc_file, write_trj_nc_file , close_trj_nc_file
  IMPLICIT none
  PRIVATE
  INTEGER, PUBLIC :: Ntime, Npart, Nout
  INTEGER Npartx,Nparty
  REAL mLon,iLon,mLat,iLat
  REAL, ALLOCATABLE, PUBLIC :: Long(:,:),Lat(:,:), P(:,:)
  REAL, ALLOCATABLE :: T(:,:), &
       U(:,:),V(:,:),W(:,:),time(:),PV(:,:),O3(:,:)
  INTEGER isec,day,mon,year ! initial date
  REAL theta,rtime,irtime ! theta surface
  INTEGER :: its, ncid_trj  !its (time step, (min)), ncid_trj (netcdf id)

  PUBLIC :: trj_init_3d, trj_3d1, trj_finish_3d

CONTAINS

  ! read data dimensions and reserve memory space for data storage
  !----------------------------------------------------------------------

  SUBROUTINE read_conf_file_3d(windfile,tracfile,GF)

    IMPLICIT none
    CHARACTER*40, INTENT(OUT) :: windfile, tracfile
    INTEGER, INTENT(OUT) :: GF

    CHARACTER*20 CFGFILE
    CFGFILE='trj-3d.cfg'
    OPEN(17,FILE=CFGFILE)
    READ(17,*)Ntime, Nout
    READ(17,*)Npartx,Nparty
    READ(17,*)mLon,iLon,mLat,iLat
    READ(17,*)windfile, tracfile
    READ(17,*)isec,day,mon,year
    READ(17,*)theta
    READ(17,*)its
    READ(17,*)GF
    CLOSE(17)
    WRITE(*,*) 'input wind data: ', windfile
    WRITE(*,*)'tracer data for: ', tracfile
    WRITE(*,FMT='(A8,X,I4,X,A8,X,I4)') "Ntime = ",Ntime, "Nout = ", Nout
    WRITE(*,FMT='(A8,X,I3,2X,A8,X,I4)')"Npartx= ",Npartx,"Nparty= ",Nparty
    WRITE(*,FMT='(4F12.2)') mLon,iLon,mLat,iLat
    WRITE(*,FMT='(A6,I6,I2,I2,X,I4)')'Date: ',isec,day,mon,year
    WRITE(*,FMT='(A10,F12.2)')'Pressure: ',theta
    Npart=Npartx*Nparty


    ALLOCATE(Long(Npart,2),lat(Npart,2),T(Npart,2), &
         P(Npart,2),U(Npart,2),V(Npart,2),W(Npart,2),time(2), &
         PV(Npart,2),O3(Npart,2))

  END SUBROUTINE read_conf_file_3d

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------

  SUBROUTINE trj_init_3d
    USE winds_3d, ONLY : readd,updatew_3d,filein,filetrc
    IMPLICIT none
    INTEGER GF
    REAL rda
    INTEGER ipartx,iparty,I
    INTEGER itime,ipart,jtraj

    CALL read_conf_file_3d(filein,filetrc,GF)! read the configuration file and allocate

    jtraj=10
    WRITE(0,*) 'Inicializando'
    IF(GF.EQ.0) THEN  ! Define an uniform grid using the data
       DO ipartx=1,Npartx  ! provided in the initialization file
          DO iparty=1,Nparty
             Lat((ipartx-1)*Nparty+iparty,1)=mLat+(iparty-1)*iLat
             Long((ipartx-1)*Nparty+iparty,1)=mLon+(ipartx-1)*iLon
          END DO
       END DO
       DO i=1,Npart
          P(i,1)=theta
       END DO

    ELSE IF(GF.EQ.1) THEN ! Read de initial distribution
       OPEN(13,FILE='input/temp_trj_3d.dat')   !from a previously writed
       DO i=1,Npart                   !file. This is used to
          READ(13,*)Long(i,1),Lat(i,1),P(i,1)
                                      !get an uniform grid at
       END DO                         !the end, using for that
       CLOSE(13)                      !calculated backwards
    END IF                            !trajectorys as initial
                                      !positions.


    WRITE(6,*) 'Cargando datos vientos'
    CALL readd

    WRITE(6,*) 'Iniciando bucle de tiempo'
    WRITE(6,*)
    CALL open_trj_nc_file(ncid_trj, 'output/trj_output', &
         (/ 'Long ', 'lat  ','Temp ','Press','Pv   ','O3   '/), &
         (/'Deg', 'Deg','K  ','mb ','mks','mmr'/),Npart)
    time(1)=isec

    irtime=greg2jul(REAL(year),REAL(mon),REAL(day),REAL(isec)/3600.)
    CALL updatew_3d(irtime)

  END SUBROUTINE trj_init_3d

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------
  SUBROUTINE trj_3d1(time1)
    !--------------------------------------------------------------------
    USE date_conv, ONLY : jul2greg
    USE winds_3d, ONLY : updatew_3d
    USE parametros
    USE current_3d, ONLY : update,T_curr,P_curr,dive_curr,lat_curr, &
         long_curr
    USE particle_3d, ONLY : lat_part,long_part,trayect,T_part,P_part,&
         latcheck2,U_part,V_part,W_part,pv_part,o3_part


    !--------------------------------------------------------------------
    IMPLICIT none
    !--------------------------------------------------------------------
    INTEGER time1
    !        Variables locales
    INTEGER itime,ipart,jtraj,itout
    REAL Long2(Npart),Lat2(Npart),T2(Npart),P2(Npart),rda
    INTEGER ipartx,iparty,i,j
    REAL rtime
    REAL tyear,tmonth,tday,thour
    !----------------------------------------------------------------------

    itime=time1
    rtime=irtime+REAL(itime-1)*its/60.

    itout=NINT(REAL((itime-1)*(Nout-1)/(Ntime-1))+1.)

    CALL updatew_3d(rtime)

    time(2)=60*its+time(1)
    DO ipart=1,Npart
       WRITE(*,*) 'Procesando particula : ' , ipart
       lat_part=Lat(ipart,1)
       long_part=Long(ipart,1)
       P_part=P(ipart,1)

       CALL update()
       T(ipart,1)=T_part
       P(ipart,1)=P_part
       U(ipart,1)=U_part
       V(ipart,1)=V_part
       W(ipart,1)=W_part
       PV(ipart,1)=pv_part
       O3(ipart,1)=o3_part
       CALL trayect(0.,time(1),time(2))
       CALL Latcheck2()

       Lat(ipart,2)=lat_part
       Long(ipart,2)=long_part
       T(ipart,2)=T_part
       P(ipart,2)=P_part
       U(ipart,2)=U_part
       V(ipart,2)=V_part
       W(ipart,2)=W_part
       PV(ipart,2)=pv_part
       O3(ipart,2)=o3_part

    END DO

    DO i=1,Npart
      CALL rangcheck(Long(i,1),Lat(i,1))
    END DO

    IF((FLOOR(REAL(Ntime*(itout)/Nout)).EQ.(itime+1)).OR.(itime.EQ.1)) THEN
       CALL jul2greg(rtime,tyear,tmonth,tday,thour)
       WRITE(0,FMT='(A,F12.2,I4,X,I2.2,"/",I2.2,"/",I4,X,I2.2,":",I2.2,":",I2.2)') 'Tiempo',time(1)-60*its,itime, &
            NINT(tday),NINT(tmonth),NINT(tyear),FLOOR(thour),FLOOR((thour-FLOOR(thour))*60.), &
            FLOOR(((thour-FLOOR(thour))*60.-FLOOR((thour-FLOOR(thour))*60.))*60.)
       CALL write_trj_nc_file(ncid_trj,rtime, &
            (/ 'Long ', 'lat  ','Temp ','Press','Pv   ','O3   ' /),(/Long(:,1),&
            lat(:,1),T(:,1),P(:,1),PV(:,1),O3(:,1)/))
    END IF

    CALL jul2greg(rtime,tyear,tmonth,tday,thour)

    Lat(:,1)=Lat(:,2)
    Long(:,1)=Long(:,2)
    P(:,1)=P(:,2)
    time(1)=time(2)

    IF(itime.EQ.(Ntime-1)) THEN
       DO i=1,Npart
          CALL rangcheck(Long(i,2),Lat(i,2))
       END DO
       rtime=irtime+REAL(Ntime-1)*its/60.
       WRITE(0,FMT='(A,F12.2,I4,X,I2.2,"/",I2.2,"/",I4,X,I2.2,":",I2.2,":",I2.2)') 'Tiempo',time(1),itime, &
       NINT(tday),NINT(tmonth),NINT(tyear),FLOOR(thour),FLOOR((thour-FLOOR(thour))*60.), &
       FLOOR(((thour-FLOOR(thour))*60.-FLOOR((thour-FLOOR(thour))*60.))*60.)

       CALL write_trj_nc_file(ncid_trj,rtime, &
            (/ 'Long ', 'lat  ','Temp ','Press','Pv   ','O3   ' /),(/Long(:,2),&
            lat(:,2),T(:,2),P(:,2),PV(:,2),O3(:,2)/))
    END IF

    !--------------------------------------------------------------------
  END SUBROUTINE trj_3d1


  SUBROUTINE trj_finish_3d
    USE winds_3d, ONLY : winds_finish
    IMPLICIT none
    CALL close_trj_nc_file(ncid_trj)
    DEALLOCATE(Long,lat,T, &
         P,U,V,time, &
         PV,W,O3)
    WRITE(0,*)
    WRITE(0,*) 'Terminado'
    CALL winds_finish
  END SUBROUTINE trj_finish_3d

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------
  SUBROUTINE rangcheck(olong,olat)
    IMPLICIT none
    REAL olong, olat

    DO WHILE((olat.GT.90).OR.(olat.LT.(-90)))

       IF(olat.GT.90) THEN
          olat=180.-olat
          olong=180.+olong
       END IF
       IF(olat.LT.(-90)) THEN
          olat=-180.-olat
          olong=180.+olong
       END IF

    END DO

    DO WHILE((olong.GT.360).OR.(olong.LT.0))
       IF(olong.GT.360)olong=olong-360.
       IF(olong.LT.0)olong=olong+360.
    END DO

  END SUBROUTINE rangcheck

END MODULE trj_main_mod_3d

SUBROUTINE trj_main
  USE trj_main_mod
  IMPLICIT none
  INTEGER I

  CALL trj_init

  DO I=1,Ntime-1
     CALL trj1(I)
  END DO

  OPEN(13,FILE='input/temp_trj.dat')
  DO I=1,Npart
     WRITE(13,*) Long(I,2),  Lat(I,2)
  END DO
  CLOSE(13)
  OPEN(13,FILE='output/temp_trj_3d.dat')
  DO I=1,Npart
     WRITE(13,*) Long(I,2),  Lat(I,2), P(I,2)
  END DO
  CLOSE(13)

  CALL trj_finish

END SUBROUTINE trj_main


SUBROUTINE trj_main_3d
  USE trj_main_mod_3d
  IMPLICIT none
  INTEGER I
  CALL trj_init_3d

  DO I=1,Ntime-1
     CALL trj_3d1(I)
  END DO

  OPEN(13,FILE='input/temp_trj_out_3d.dat')
  DO I=1,Npart
     WRITE(13,*) Long(I,2),  Lat(I,2),P(i,2)
  END DO
  CLOSE(13)

  CALL trj_finish_3d
END SUBROUTINE trj_main_3d

PROGRAM trj
!FUNCTION trj()
  CHARACTER(2) :: flag3d

  OPEN(17,FILE='dimension.cfg')
  READ(17,*)flag3d
  CLOSE(17)

  IF(flag3d.EQ.'2d') THEN
     CALL trj_main
  ELSE IF(flag3d.EQ.'3d') THEN
     CALL trj_main_3d
  ELSE
    WRITE(*,*) 'Interface Incorrecta'
  ENDIF
!END FUNCTION trj
END PROGRAM trj
