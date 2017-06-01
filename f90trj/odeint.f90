!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Subrutinas de integración
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
SUBROUTINE adaptive(yi,n,xi,xf,htol,mmax,derivs)
	IMPLICIT none
	REAL,INTENT(INOUT) :: yi(n)
	REAL,INTENT(IN) :: xi, xf
	INTEGER, INTENT(IN) :: n,mmax
	REAL, INTENT(IN) :: htol

	EXTERNAL derivs

	REAL x1,x2,y1(n),y2(n),ytemp(n)
	REAL m,h1
	LOGICAL var1, var2
	INTEGER test
	x1 = xi
	x2 = xf
	y1= yi
	ytemp = yi
	y2 = yi
	DO WHILE (x1 < x2)
		m=1
		h1=0
		DO
			IF (m .EQ. 1) THEN
				h1 = (x2-x1)/m
				CALL rk4(y1,x1,x1+h1,y2,2,derivs)
				m= m + 1
			ELSE
				var1 = (abs(y2(1)-y1(1)) .GE. (htol*(abs(y1(1)+y2(2))/2)))
				var2 = (abs(y2(2)-y1(2)) .GE. (htol*(abs(y1(2)+y2(2))/2)))
				IF ( var1 .AND. var2 .AND. (m .LT. mmax)) THEN
					h1 = (x2-x1)/m
					CALL rk4(y1,x1,x1+h1,y2,2,derivs)
					m= m + 1
        		ELSE IF (m.GE.mmax) THEN
         			 WRITE(*,*) 'ERROR - m mayor o igual que mmax en adaptive ode'
          			STOP
    			ELSE
					EXIT
				END IF
			END IF
		END DO

		ytemp = y1
		y1=y2
		y2=ytemp
		x1 = x1 + h1
	END DO
	yi = y2
END SUBROUTINE adaptive

SUBROUTINE rk4(yi,x1,x2,yf,n,derivs)
	IMPLICIT none
	REAL, INTENT(IN) :: x1,x2,yi(n)
	INTEGER,INTENT(IN) :: n
	REAL, INTENT(OUT) :: yf(n)

	EXTERNAL derivs

	REAL k1(n),k2(n),k3(n),k4(n), dydt(n),h

	h=x2-x1
	CALL derivs(x1,yi,dydt)
	k1=h*dydt
	CALL derivs(x1+h/2,yi+k1/2,dydt)
	k2=h*dydt
	CALL derivs(x1+h/2,yi+k2/2,dydt)
	k3=h*dydt
	CALL derivs(x1+h,yi+k3,dydt)
	k4=h*dydt
	yf=yi+k1/6+k2/3+k3/3+k4/6
END SUBROUTINE rk4

!----------------------------------------------------------------------
!----------------------------------------------------------------------
