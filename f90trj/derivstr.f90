!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Subrutinas auxiliares para obtener las derivadas temporales usadas en
! odeint
! dx/dt y dy/dt, Coordenadas locales curvilineas.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!        y(1) = x
!        y(2) = y
!----------------------------------------------------------------------
      SUBROUTINE derivstr(time,y,Dydt)
!----------------------------------------------------------------------
      USE particle, ONLY : U_part,V_part,lat_part,long_part,latcheck2,&
                           xstr_part,ystr_part
      USE thsurf, ONLY : update
      USE parametros, ONLY : DegToR, Rt

!----------------------------------------------------------------------
!     Argumetos
      IMPLICIT none
      REAL Dydt(2) , time , y(2)
      REAL Ustr,Vstr,rlong,rlat
!----------------------------------------------------------------------
      CALL str_to_ang(rlong,rlat,y(1),y(2))
      long_part=rlong/DegToR
      lat_part=rlat/DegToR

      CALL Latcheck2()
      CALL update()

      CALL strwinds(rlong,rlat,U_part,V_part,&
               Ustr,Vstr)
      Dydt(1)=Ustr
      Dydt(2)=Vstr

      END SUBROUTINE derivstr
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!        y(1) = x
!        y(2) = y
!----------------------------------------------------------------------
      SUBROUTINE derivstrN(time,y,Dydt)
!----------------------------------------------------------------------
      USE particle, ONLY : U_part,V_part,lat_part,long_part,latcheck2,&
                           xstr_part,ystr_part
      USE thsurf, ONLY : update
      USE parametros, ONLY : DegToR, Rt

!----------------------------------------------------------------------
!     Argumetos
      IMPLICIT none
      REAL Dydt(2) , time , y(2)
      REAL Ustr,Vstr,rlong,rlat
!----------------------------------------------------------------------
      CALL str_to_angN(rlong,rlat,y(1),y(2))
      long_part=rlong/DegToR
      lat_part=rlat/DegToR

      CALL Latcheck2()
      CALL update()

      CALL strwindsN(rlong,rlat,U_part,V_part,&
               Ustr,Vstr)
      Dydt(1)=Ustr
      Dydt(2)=Vstr

      END SUBROUTINE derivstrN
!----------------------------------------------------------------------


!----------------------------------------------------------------------
!        y(1) = x
!        y(2) = y
!        y(3) = z
!----------------------------------------------------------------------
      SUBROUTINE derivstr_3d(time,y,Dydt)
!----------------------------------------------------------------------
      USE particle_3d, ONLY : U_part,V_part,lat_part,long_part,latcheck2,&
                           xstr_part,ystr_part,P_part,W_part
      USE current_3d, ONLY : update
      USE parametros, ONLY : DegToR, Rt

!----------------------------------------------------------------------
!     Argumetos
      IMPLICIT none
      REAL Dydt(3) , time , y(3)
      REAL Ustr,Vstr,rlong,rlat
!----------------------------------------------------------------------
      CALL str_to_ang(rlong,rlat,y(1),y(2))
      long_part=rlong/DegToR
      lat_part=rlat/DegToR
      P_part=y(3)
      CALL Latcheck2()
      CALL update()

      CALL strwinds(rlong,rlat,U_part,V_part,&
               Ustr,Vstr)
      Dydt(1)=Ustr
      Dydt(2)=Vstr
      Dydt(3)=W_part/100.

      END SUBROUTINE derivstr_3d
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!        y(1) = x
!        y(2) = y
!        y(3) = z
!----------------------------------------------------------------------
      SUBROUTINE derivstrN_3d(time,y,Dydt)
!----------------------------------------------------------------------
      USE particle_3d, ONLY : U_part,V_part,lat_part,long_part,latcheck2,&
                           xstr_part,ystr_part,W_part,P_part
      USE current_3d, ONLY : update
      USE parametros, ONLY : DegToR, Rt
!
!----------------------------------------------------------------------
!     Argumetos
      IMPLICIT none
      REAL Dydt(3) , time , y(3)
      REAL Ustr,Vstr,rlong,rlat
!----------------------------------------------------------------------
      CALL str_to_angN(rlong,rlat,y(1),y(2))
      long_part=rlong/DegToR
      lat_part=rlat/DegToR
      P_part=y(3)
      CALL Latcheck2()
      CALL update()

      CALL strwindsN(rlong,rlat,U_part,V_part,&
               Ustr,Vstr)
      Dydt(1)=Ustr
      Dydt(2)=Vstr
      Dydt(3)=W_part/100. ! Change Velocitys to Hpa*s**-1

      END SUBROUTINE derivstrN_3d
!----------------------------------------------------------------------
