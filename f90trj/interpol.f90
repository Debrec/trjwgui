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
 END MODULE interpolacion
