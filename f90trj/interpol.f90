!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Subrutinas auxiliares para para realizar las interpolaciones
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	MODULE interpolacion
	CONTAINS
	REAL FUNCTION intp1d(var1,var2,rval,fun)
		IMPLICIT none
		REAL, INTENT(IN) ::  fun(2),rval,var1,var2

		intp1d=(fun(2)-fun(1))*((rval-var1)/(var2-var1))+ &
			 fun(1)
	END FUNCTION intp1d

	REAL FUNCTION intp2d(x,y,XD,YD,ZD,Nx,Ny)
		INTEGER, INTENT(IN) :: Nx,NY
		REAL, INTENT(IN) :: x,y,XD(Nx),YD(Ny),ZD(NX,NY)
		REAL :: valor(2)
		CALL locateindex(XD,Nx,x,ix)
    CALL locateindex(YD,Ny,y,iy)
		valor(1) = intp1d(XD(ix),XD(ix+1),x,(/ZD(ix,iy),ZD(ix+1,iy)/))
    valor(2) = intp1d(XD(ix),XD(ix+1),x,(/ZD(ix,iy+1),ZD(ix+1,iy+1)/))
    intp2d = intp1d(YD(iy),YD(iy+1),y,(/valor(1), valor(2)/))
	END FUNCTION intp2d
!//////////////////////////////////////////////////////////////////////
!----------------------------------------------------------------------
       SUBROUTINE interp2(Xwant,Ywant,Value,X,Y,Array,Nmaxx,Nmaxy)
!----------------------------------------------------------------------
!     Argumentos
      IMPLICIT none
      INTEGER  Nmaxx , Nmaxy , Nx , Ny

      REAL, INTENT(IN) :: Array(Nmaxx,Nmaxy), X(Nmaxx) , Xwant ,&
        Y(Nmaxy) , Ywant
      REAL, INTENT(OUT):: Value


!----------------------------------------------------------------------
!     Variables locales

      INTEGER Jx , Jxp1 , Jy , Jyp1
      REAL s1 , s2 , s3 , s4 , zt , zta , ztb , zu , zua , zub

!----------------------------------------------------------------------
!     Procedimientos externos
      !EXTERNAL locateindex
!
!-----------------------------------------------------------------------
!
      CALL locateindex(X,Nmaxx,Xwant,Jx)
      CALL locateindex(Y,Nmaxy,Ywant,Jy)
			!IF (jx .EQ. Nmaxx) THEN
			!	Jxp1 = 1
			!ELSE
				Jxp1=Jx+1
			!END IF
			Jyp1=Jy+1

!     Look up relevant grid points.
      s1 = Array(Jx,Jy)
      s2 = Array(Jxp1,Jy)
      s3 = Array(Jxp1,Jyp1)
      s4 = Array(Jx,Jyp1)

!      Find slopes used in interpolation;
!     i) X.
      zta = Xwant - X(Jx)
			!IF (jx .EQ. Nmaxx) THEN
			!	ztb = X(Jxp1)+360 - X(Jx)
			!ELSE
				ztb = X(Jxp1) - X(Jx)
			!END IF

      zt = zta/ztb

!     ii) Y.
      zua = Ywant - Y(Jy)
      zub = Y(Jyp1) - Y(Jy)

      zu = zua/zub

!     Use bilinear interpolation.
      Value = (1.0-zt)*(1.0-zu)*s1 + zt*(1.0-zu)*s2 + zt*zu*s3 +&
             (1.0-zt)*zu*s4

!----------------------------------------------------------------------
      END SUBROUTINE interp2
!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------
      SUBROUTINE locateindex(ain,nn,var,index)
!----------------------------------------------------------------------
!          Argumentos
           IMPLICIT none
           INTEGER, INTENT(IN) :: nn
           REAL, INTENT(IN) :: ain(nn),var
           INTEGER, INTENT(OUT) :: index

!----------------------------------------------------------------------
!          Variables locales
           INTEGER i

           DO i=1,nn-1
             IF(ain(i).LE.ain(i+1)) THEN
               IF((ain(i).LE.var).AND.(ain(i+1).GE.var)) THEN
                 index=i

                 RETURN
               ELSE IF(i.EQ.(nn-1)) THEN
                 WRITE(0,*) "ain fuera de rango ver locateindex"
                 WRITE(0,*) "var es",var
                 WRITE(0,*) "ain(nn),ain(1) son",ain(nn),ain(1)
                 STOP
               END IF
             ELSE IF(ain(i).GT.ain(i+1)) THEN
               IF((ain(i).GE.var).AND.(ain(i+1).LE.var)) THEN
                 index=i

                 RETURN
               ELSE IF(i.EQ.(nn-1)) THEN
                 WRITE(0,*) "ain fuera de rango ver locateindex"
                 WRITE(0,*) "var es",var
                 WRITE(0,*) "ain(nn),ain(1) son",ain(nn),ain(1),nn
                 STOP
               END IF
             END IF
           END DO

      END SUBROUTINE locateindex

!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------
!        SUBROUTINE interp2P(Xwant,Ywant,Value,X,Y,Array,Nmaxx,Nmaxy)
! !----------------------------------------------------------------------
! !     Argumentos
!       IMPLICIT none
!       INTEGER  Nmaxx , Nmaxy , Nx , Ny
!       REAL, INTENT(IN) :: Array(Nmaxx,Nmaxy), X(Nmaxx) , Xwant ,&
!         Y(Nmaxy) , Ywant
!       REAL, INTENT(OUT):: Value
!
! !----------------------------------------------------------------------
! !     Variables locales
!
!       INTEGER Jx , Jxp1,Jxp2,Jxp3 , Jy , Jyp1, Jyp2, Jyp3,i,j,k,ij,ik
!       REAL Xzt(4),Yzt(4),Zzt(4,4),Zint(4),Q,C(4,4),dy
!
! !----------------------------------------------------------------------
! !     Procedimientos externos
!       !EXTERNAL locateindex
! !
! !-----------------------------------------------------------------------
! !
!
!       CALL locateindex(X,Nmaxx,Xwant,Jx)
!       CALL locateindex(Y,Nmaxy,Ywant,Jy)
!       !El ltimo y el primero son el mismo
!
!       IF((Jx.GE.2).AND.(Jx.LE.(Nmaxx-2))) THEN
!          Jxp1=Jx+1
!          Jxp2=Jx+2
! 	 Jxp3=Jx-1
!       ELSE IF(Jx.EQ.1) THEN
!          Jxp1=Jx+1
!          Jxp2=Jx+2
! 	 Jxp3=Nmaxx-1 ! la funci� es periodica en longitud
!       ELSE IF(Jx.EQ.(Nmaxx-1)) THEN
!          Jxp1=2
!          Jxp2=1
! 	 Jxp3=Jx-1 ! la funci� es periodica en longitud
!       END IF
!       IF((Jy.GE.2).AND.(Jy.LE.(Nmaxy-2)))THEN
!          Jyp1=Jy+1
!          Jyp2=Jy+2
! 	 Jyp3=Jy-1
!       ELSE IF(Jy.EQ.1) THEN
!          Jyp1=Jy+1
!          Jyp2=Jy+2
! 	 Jyp3=Jy+3
!       ELSE IF(Jy.EQ.(Nmaxy-1)) THEN
!          Jyp1=Jy+1
!          Jyp2=Jy-2
! 	 Jyp3=Jy-1
!       END IF
!
!
! !      Find slopes used in interpolation;
! !     i) X.
!       IF((Jx.GE.2).AND.(Jx.LE.(Nmaxx-2))) THEN
!           Xzt(1)=X(Jxp3)
!           Xzt(2)=X(Jx)
!           Xzt(3)=X(Jxp1)
!           Xzt(4)=X(Jxp2)
!       ELSE IF(Jx.EQ.1) THEN
!           Xzt(1)=X(Jxp3)-360
!           Xzt(2)=X(Jx)
!           Xzt(3)=X(Jxp1)
!           Xzt(4)=X(Jxp2)
!       ELSE IF(Jx.EQ.(Nmaxx-1)) THEN
!           Xzt(1)=X(Jxp3)
!           Xzt(2)=X(Jx)
!           Xzt(3)=X(Jxp1)+360
!           Xzt(4)=X(Jxp2)+360
!       END IF
!       DO i=1,4
!       IF((Jy.GE.2).AND.(Jy.LE.(Nmaxy-2))) THEN
! 	  Zzt(1,i)=Array(Jxp3,Jyp3+i-1)
!           Zzt(2,i)=Array(Jx,Jyp3+i-1)
!           Zzt(3,i)=Array(Jxp1,Jyp3+i-1)
!           Zzt(4,i)=Array(Jxp2,Jyp3+i-1)
! 	  Yzt(i)=Y(Jyp3+i-1)
!       ELSE IF(Jy.EQ.1) THEN
!           Zzt(1,i)=Array(Jxp3,Jy+i-1)
!           Zzt(2,i)=Array(Jx,Jy+i-1)
!           Zzt(3,i)=Array(Jxp1,Jy+i-1)
!           Zzt(4,i)=Array(Jxp2,Jy+i-1)
!           Yzt(i)=Y(Jy+i-1)
!       ELSE IF(Jy.EQ.(Nmaxy-1)) THEN
!           Zzt(1,i)=Array(Jxp3,Jyp2+i-1)
!           Zzt(2,i)=Array(Jx,Jyp2+i-1)
!           Zzt(3,i)=Array(Jxp1,Jyp2+i-1)
!           Zzt(4,i)=Array(Jxp2,Jyp2+i-1)
!           Yzt(i)=Y(Jyp2+i-1)
!       END IF
!       END DO
!
!       CALL polin2(Xzt,Yzt,Zzt,4,4,Xwant,Ywant,Value,dy)
! !----------------------------------------------------------------------
!       END SUBROUTINE interp2P
! !----------------------------------------------------------------------
!
! !//////////////////////////////////////////////////////////////////////
! !//////////////////////////////////////////////////////////////////////
!
! !----------------------------------------------------------------------
!       SUBROUTINE polin2(x1a,x2a,ya,m,n,x1,x2,y,dy)
!       INTEGER m,n,NMAX,MMAX
!       REAL dy,x1,x2,y,x1a(m),x2a(n),ya(m,n)
!       PARAMETER (NMAX=20,MMAX=20)
!       INTEGER j,k
!       REAL ymtmp(MMAX),yntmp(NMAX)
!       do j=1,m
!          do k=1,n
!           yntmp(k)=ya(j,k)
!          enddo
!          call polint(x2a,yntmp,n,x2,ymtmp(j),dy)
!       enddo
!       call polint(x1a,ymtmp,m,x1,y,dy)
!       return
!       END
!
!       SUBROUTINE polint(xa,ya,n,x,y,dy)
!       INTEGER n,NMAX
!       REAL dy,x,y,xa(n),ya(n)
!       PARAMETER (NMAX=10)
!       INTEGER i,m,ns
!       REAL den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
!       ns=1
!       dif=abs(x-xa(1))
!       do i=1,n
!           dift=abs(x-xa(i))
!           if (dift.lt.dif) then
!                ns=i
!                dif=dift
!           endif
!           c(i)=ya(i)
!           d(i)=ya(i)
!       enddo
!       y=ya(ns)
!       ns=ns-1
!       do m=1,n-1
!           do i=1,n-m
!              ho=xa(i)-x
!              hp=xa(i+m)-x
!              w=c(i+1)-d(i)
!              den=ho-hp
!       if(den.eq.0.) then
! 	     write(*,*) 'failure in polint'
! 		 STOP
! 	  endif
!
!       den=w/den
!       d(i)=hp*den
!       c(i)=ho*den
!       enddo
!       if (2*ns.lt.n-m)then
!               dy=c(ns+1)
!       else
!               dy=d(ns)
!               ns=ns-1
!       endif
!       y=y+dy
!       enddo
!       return
!       END
 END MODULE interpolacion
