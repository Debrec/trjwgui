!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Subrutinas auxiliares para obtener las derivadas temporales usadas en odeint
! dx/dt y dy/dt, Coordenadas locales curvilineas.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        y(1) = x
!        y(2) = y
!-----------------------------------------------------------------------
      SUBROUTINE derivs(time,y,Dydt)
!-----------------------------------------------------------------------
      USE particle, ONLY : U_part,V_part,lat_part,long_part,latcheck2
      USE thsurf, ONLY : update
      USE parametros, ONLY : DegToR, Rt

!-----------------------------------------------------------------------
!     Argumetos
      IMPLICIT none
      REAL Dydt(2) , time , y(2)
!-----------------------------------------------------------------------
      lat_part=y(2)
      long_part=y(1)
      CALL Latcheck2()
      CALL update()
      Dydt(1)=U_part/((Rt)*COS(lat_part*DegToR)*DegToR)
      Dydt(2)=V_part/((Rt)*DegToR)

      END SUBROUTINE derivs
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!        y(1) = x
!        y(2) = y
!        y(3) = z
!-----------------------------------------------------------------------
      SUBROUTINE derivs_3d(time,y,Dydt)
!-----------------------------------------------------------------------
      USE particle_3d, ONLY : U_part,V_part,W_part,lat_part,long_part, &
           P_part, latcheck2
      USE current_3d, ONLY : update
      USE parametros, ONLY : DegToR, Rt

!-----------------------------------------------------------------------
!     Argumetos
      IMPLICIT none
      REAL Dydt(3) , time , y(3)
!-----------------------------------------------------------------------
      P_part=y(3)
      lat_part=y(2)
      long_part=y(1)
      CALL Latcheck2()
      CALL update()
      Dydt(1)=U_part/((Rt)*COS(lat_part*DegToR)*DegToR)
      Dydt(2)=V_part/((Rt)*DegToR)
      Dydt(3)=W_part/100. ! Change velocitys to Hpa*s**-1

      END SUBROUTINE derivs_3d
!-----------------------------------------------------------------------
