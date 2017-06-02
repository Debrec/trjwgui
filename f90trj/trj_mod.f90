!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Modulos Para el calculo de trayectorias
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!               Modulos del programa trayectoria
!               Incluye:
!                    --> parametros
!                    --> particle
!                    --> thsurf
!
!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------
!           Rt = radio medio de la tierra en metros (m)
!           Pi = el numero Pi
!           DegToR= pasa de grados a radianes
!----------------------------------------------------------------------
MODULE parametros
  IMPLICIT none
  REAL Rt,DegToR,Pi

  parameter (Rt=6.371229E+06)
  parameter (Pi=3.14159265358979)
  parameter (DegToR=0.01745329251994)

END MODULE parametros
!------------------------------------------------------------------------

!////////////////////////////////////////////////////////////////////////

!------------------------------------------------------------------------
!      PROPIEDADES DE LAS PARTI!ULAS
!
!      U_part,V_part velocidades en (m/s)
!      lat_part, long_part latitud y longitud en grados
!      Alt_part Altura en (m)
!      P_part Presion (mb)
!      T_part Temperatura (k)
!      Theta_part Temperatura potencial en (k)
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
MODULE particle
  IMPLICIT none
  REAL, SAVE :: U_part,V_part
  REAL, SAVE :: lat_part , long_part, Alt_part
  REAL, SAVE :: xstr_part,ystr_part
  REAL, SAVE :: P_part, T_part, Theta_part,pv_part,o3_part

  !----------------------------------------------------------------------
CONTAINS
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !     lat = latitude en grados
  !     long = longitud en grados
  !     z = altura geometrica en metros
  !     U , V velocidades zonales y meridional
  !     ti,tf tiempos iniciales y finales
  !----------------------------------------------------------------------
  SUBROUTINE trayect(z,ti,tf)

    USE parametros
    !-----------------------------------------------------------------------
    IMPLICIT none
    REAL z,ti,tf

    !-----------------------------------------------------------------------
    EXTERNAL derivs,ode,derivstr,derivstrN
    REAL latf,longf,xstrf,ystrf,rlong,rlat,pf
    INTEGER Nok,Nbad,i

    IF((lat_part.GT.(-75)).AND.(lat_part.LT.75)) THEN
			 CALL ode(long_part,lat_part,longf,latf,ti,tf,derivs)
			 lat_part=latf
       long_part=longf
	     CALL ang_to_str(long_part*DegToR,lat_part*DegToR,&
            xstr_part,ystr_part)
    ELSE IF(lat_part.LE.(-75)) THEN
       CALL ang_to_str(long_part*DegToR,lat_part*DegToR,xstr_part,ystr_part)
       CALL ode(xstr_part,ystr_part,xstrf,ystrf,ti,tf,derivstr)
       xstr_part=xstrf
       ystr_part=ystrf
       CALL str_to_ang(rlong,rlat,xstr_part,ystr_part)
       long_part=rlong/DegToR
       lat_part=rlat/DegToR
    ELSE IF(lat_part.GE.75) THEN
       CALL ang_to_strN(long_part*DegToR,lat_part*DegToR,xstr_part,ystr_part)
       CALL ode(xstr_part,ystr_part,xstrf,ystrf,ti,tf,derivstrN)
       xstr_part=xstrf
       ystr_part=ystrf
       CALL str_to_angN(rlong,rlat,xstr_part,ystr_part)
       long_part=rlong/DegToR
       lat_part=rlat/DegToR
    END IF

  END SUBROUTINE trayect
  !----------------------------------------------------------------------
  !----------------------------------------------------------------------
  SUBROUTINE rangcheck(olong,olat)
    IMPLICIT none
    REAL olong, olat

    olat=lat_part
    olong=long_part
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

  SUBROUTINE Latcheck2()
    IMPLICIT none

  END SUBROUTINE Latcheck2

END MODULE particle
!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------
!      Propiedades sobre una superficie
!      de Theta constante en una grilla nx*ny  x --> long, y --> lat
!
!----------------------------------------------------------------------

MODULE thsurf
  USE Particle
  IMPLICIT none
  PRIVATE
  INTEGER :: nx,ny,indx,indy
  PARAMETER (nx=241,ny=121)

  INTEGER, SAVE :: intpm
  ! For the interpolation the fields must be periodic in longitud
  ! field(nx;:)=field(1,:)
  REAL, SAVE, PUBLIC :: U_surf(nx,ny),V_surf(nx,ny), Alt_surf(nx,ny)
  REAL, SAVE, PUBLIC :: lat_surf(nx,ny) , long_surf(nx,ny)
  REAL, SAVE, PUBLIC :: T_surf(nx,ny),P_surf(nx,ny), th_surf
  REAL, SAVE, PUBLIC :: dive_surf(nx,ny),pv_surf(nx,ny),o3_surf(nx,ny)
  REAL, SAVE :: U2a(nx,ny),V2a(nx,ny),Alt2a(nx,ny),P2a(nx,ny),&
       T2a(nx,ny)
  PUBLIC :: update

  !----------------------------------------------------------------------
CONTAINS
  !-----------------------------------------------------------------------

  ! Interpolate meteorological winds to requiered position
  ! for a particle
  SUBROUTINE update()
		use interpolacion
		IMPLICIT none
    REAL olat,olong
    !EXTERNAL interp2

    CALL rangcheck(olong,olat)

    Theta_part=th_surf

    intpm=2

    !IF(intpm.EQ.1) THEN
    IF (intpm.EQ.2) THEN
		   U_part = intp2d(olong,olat,long_surf(1:nx,1),&
            lat_surf(1,1:ny),U_surf,nx,ny)

       V_part = intp2d(olong,olat,long_surf(1:nx,1),&
            lat_surf(1,1:ny),V_surf,nx,ny)

       T_part = intp2d(olong,olat,long_surf(1:nx,1),&
            lat_surf(1,1:ny),T_surf,nx,ny)

       P_part = intp2d(olong,olat,long_surf(1:nx,1),&
            lat_surf(1,1:ny),P_surf,nx,ny)

       Alt_part = intp2d(olong,olat,long_surf(1:nx,1),&
            lat_surf(1,1:ny),Alt_surf,nx,ny)

       pv_part = intp2d(olong,olat,long_surf(1:nx,1),&
            lat_surf(1,1:ny),pv_surf,nx,ny)

       o3_part = intp2d(olong,olat,long_surf(1:nx,1),&
            lat_surf(1,1:ny),o3_surf,nx,ny)
    ELSE
      WRITE(*,*) 'Método de interpolación desconocido'
      STOP
    END IF

  END SUBROUTINE update

END MODULE thsurf

!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------

!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------
!      PROPIEDADES DE LAS PARTI!ULAS
!
!      U_part,V_part velocidades en (m/s)
!      W_part (Pa/s)
!      lat_part, long_part latitud y longitud en grados
!      Alt_part Altura en (m)
!      P_part Presion (mb)
!      T_part Temperatura (k)
!      Theta_part Temperatura potencial en (k)
!
!----------------------------------------------------------------------

MODULE particle_3d
  IMPLICIT none
  REAL, SAVE :: U_part,V_part, W_part
  REAL, SAVE :: lat_part , long_part, Alt_part
  REAL, SAVE :: xstr_part,ystr_part
  REAL, SAVE :: P_part, T_part, Theta_part,pv_part,o3_part

  !----------------------------------------------------------------------
CONTAINS
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !     lat = latitude en grados
  !     long = longitud en grados
  !     z = altura geometrica en metros
  !     U , V velocidades zonales y meridional
  !     ti,tf tiempos iniciales y finales
  !----------------------------------------------------------------------
  SUBROUTINE trayect(z,ti,tf)

    USE parametros
    !-----------------------------------------------------------------------
    IMPLICIT none
    REAL z,ti,tf

    !-----------------------------------------------------------------------
    EXTERNAL derivs_3d,ode_3d,derivstr_3d,derivstrN_3d
    REAL latf,longf,xstrf,ystrf,rlong,rlat,pf
    INTEGER Nok,Nbad,i

    IF((lat_part.GT.(-75)).AND.(lat_part.LT.75)) THEN
       CALL ode_3d(long_part,lat_part,P_part,longf,latf,pf,ti,tf,derivs_3d)
       lat_part=latf
       long_part=longf
       P_part=pf
       CALL ang_to_str(long_part*DegToR,lat_part*DegToR,&
            xstr_part,ystr_part)
    ELSE IF(lat_part.LE.(-75)) THEN
       CALL ang_to_str(long_part*DegToR,lat_part*DegToR,xstr_part,ystr_part)
       CALL ode_3d(xstr_part,ystr_part,P_part,xstrf,ystrf,pf,ti,tf,derivstr_3d)
       xstr_part=xstrf
       ystr_part=ystrf
       P_part=pf
       CALL str_to_ang(rlong,rlat,xstr_part,ystr_part)
       long_part=rlong/DegToR
       lat_part=rlat/DegToR
    ELSE IF(lat_part.GE.75) THEN
       CALL ang_to_strN(long_part*DegToR,lat_part*DegToR,xstr_part,ystr_part)
       CALL ode_3d(xstr_part,ystr_part,P_part,xstrf,ystrf,pf,ti,tf,derivstrN_3d)
       xstr_part=xstrf
       ystr_part=ystrf
       P_part=pf
       CALL str_to_angN(rlong,rlat,xstr_part,ystr_part)
       long_part=rlong/DegToR
       lat_part=rlat/DegToR
    END IF

  END SUBROUTINE trayect
  !----------------------------------------------------------------------
  !----------------------------------------------------------------------
  SUBROUTINE rangcheck(olong,olat)
    IMPLICIT none
    REAL olong, olat

    olat=lat_part
    olong=long_part
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

  SUBROUTINE Latcheck2()
    IMPLICIT none

  END SUBROUTINE Latcheck2

END MODULE particle_3d
!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------
!     current data.
!
!
!----------------------------------------------------------------------

MODULE current_3d
  USE Particle_3d
  IMPLICIT none
  PRIVATE
  INTEGER, PARAMETER :: nx=241
  INTEGER, PARAMETER :: ny=121
  INTEGER, PARAMETER :: nz=37

  INTEGER :: indx,indy
  INTEGER, SAVE :: intpm
  ! For the interpolation the fields must be periodic in longitud
  ! field(nx;:)=field(1,:)
  REAL, SAVE, PUBLIC :: U_curr(nx,ny,nz),V_curr(nx,ny,nz), Alt_curr(nx,ny,nz)
  REAL, SAVE, PUBLIC :: lat_curr(nx,ny) , long_curr(nx,ny)
  REAL, SAVE, PUBLIC :: T_curr(nx,ny,nz),P_curr(nx,ny,nz),W_curr(nx,ny,nz)
  REAL, SAVE, PUBLIC :: dive_curr(nx,ny,nz),pv_curr(nx,ny,nz),o3_curr(nx,ny,nz)
  REAL, SAVE :: U2a(nx,ny),V2a(nx,ny),Alt2a(nx,ny),P2a(nx,ny),&
       T2a(nx,ny),pv2a(nx,ny),W2a(nx,ny),o32a(nx,ny)
  PUBLIC :: update

  !----------------------------------------------------------------------
CONTAINS
  !-----------------------------------------------------------------------

  ! Interpolate meteorological winds to requiered position
  ! for a particle
  SUBROUTINE update()
		USE interpolacion
    IMPLICIT none
    REAL olat,olong,op
    LOGICAL :: inc
    INTEGER :: Jz,i_p
    !EXTERNAL interp2,interp2P

    CALL rangcheck(olong,olat)

     !And check the profiles order
    IF(P_curr(1,1,1).GT.P_curr(1,1,nz)) THEN
       inc=.TRUE.
    ELSE IF(P_curr(1,1,1).LT.P_curr(1,1,nz)) THEN
       inc=.FALSE.
    ELSE
       WRITE(0,*) 'Wrong profile data',P_curr(1,1,1),P_curr(1,1,nz)
       STOP
    END IF

    !Now look for corresponding pressure level
    IF(inc.EQV..TRUE.) THEN
       DO Jz=1,nz-1
          IF((P_part.LT.P_curr(1,1,Jz)).AND.(P_part.GE.P_curr(1,1,Jz+1))) THEN
             i_p=Jz
          END IF
       END DO
    ELSE
       DO Jz=1,nz-1
          IF((P_part.GE.P_curr(1,1,Jz)).AND.(P_part.LT.P_curr(1,1,Jz+1))) THEN
             i_p=Jz
          END IF
       END DO
    END IF

    DO Indx=1,nx
       DO indy=1,ny
          U2a(indx,indy)=intp1d(P_curr(indx,indy,i_p),P_curr(indx,indy,i_p+1), &
               P_part,U_curr(indx,indy,i_p:i_p+1))
          V2a(indx,indy)=intp1d(P_curr(indx,indy,i_p),P_curr(indx,indy,i_p+1), &
               P_part,V_curr(indx,indy,i_p:i_p+1))
          T2a(indx,indy)=intp1d(P_curr(indx,indy,i_p),P_curr(indx,indy,i_p+1), &
               P_part,T_curr(indx,indy,i_p:i_p+1))
          P2a(indx,indy)=intp1d(P_curr(indx,indy,i_p),P_curr(indx,indy,i_p+1), &
               P_part,P_curr(indx,indy,i_p:i_p+1))
          Alt2a(indx,indy)=intp1d(P_curr(indx,indy,i_p),P_curr(indx,indy,i_p+1), &
               P_part,Alt_curr(indx,indy,i_p:i_p+1))
          PV2a(indx,indy)=intp1d(P_curr(indx,indy,i_p),P_curr(indx,indy,i_p+1), &
               P_part,PV_curr(indx,indy,i_p:i_p+1))
          O32a(indx,indy)=intp1d(O3_curr(indx,indy,i_p),P_curr(indx,indy,i_p+1), &
               P_part,O3_curr(indx,indy,i_p:i_p+1))
          W2a(indx,indy)=intp1d(P_curr(indx,indy,i_p),P_curr(indx,indy,i_p+1), &
               P_part,W_curr(indx,indy,i_p:i_p+1))
       END DO
    END DO

    intpm=2

    !IF(intpm.EQ.1) THEN
    IF (intpm.EQ.2) THEN

	     U_part = intp2d(olong,olat,long_curr(1:nx,1),&
	          lat_curr(1,1:ny),U2a,nx,ny)

	     V_part = intp2d(olong,olat,long_curr(1:nx,1),&
	          lat_curr(1,1:ny),V2a,nx,ny)

	     T_part = intp2d(olong,olat,long_curr(1:nx,1),&
	          lat_curr(1,1:ny),T2a,nx,ny)

	     Alt_part = intp2d(olong,olat,long_curr(1:nx,1),&
	          lat_curr(1,1:ny),Alt2a,nx,ny)

	     pv_part = intp2d(olong,olat,long_curr(1:nx,1),&
	          lat_curr(1,1:ny),pv2a,nx,ny)

	     o3_part = intp2d(olong,olat,long_curr(1:nx,1),&
	          lat_curr(1,1:ny),O32a,nx,ny)

	     W_part = intp2d(olong,olat,long_curr(1:nx,1),&
	         lat_curr(1,1:ny),W2a,nx,ny)
    ELSE
      WRITE(*,*) 'Método de interpolación desconocido'
      STOP
		END IF
  END SUBROUTINE update

  REAL FUNCTION intp1d(var1,var2,rval,fun)
    REAL, INTENT(IN) ::  fun(2),rval,var1,var2

    intp1d=(fun(2)-fun(1))*((rval-var1)/(var2-var1))+ &
         fun(1)
  END FUNCTION intp1d


END MODULE current_3d

!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------
