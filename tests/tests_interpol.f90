MODULE tests_interpol
	USE tests
	USE interpolacion
	IMPLICIT none

CONTAINS
	SUBROUTINE test_interpol
		real, parameter :: pi=3.1415927
		INTEGER, PARAMETER :: Nx=30,Ny=40, Nout=6
		REAL :: xwant(4), ywant(3), val, mval
		REAL :: valor(Nx),x(Nx),y(Ny),array(Nx,Ny)
		INTEGER :: i,j
		CHARACTER(5) :: sval

		CALL mrun('test_interpol, interp2P')

		DO i=1,Nx
			x(i)=(i-1)*360./(Nx-1)
		END DO
		DO j=1,Ny
			y(j)=REAL(REAL(j)/REAL(Ny))
		END DO
		DO i = 1,Nx
			DO j = 1,Ny
				array(i,j) = y(j)*cos(2.*pi*x(i)/360.)
			END DO
		END DO

		xwant=(/100.,235.,320.,347.25/)
		ywant=(/0.3,0.5,0.8/)
		DO i = 1,4
			DO j = 1,3
		 		 mval = ywant(j)*cos(2.*pi*xwant(i)/360.)
		 		 CALL interp2P(xwant(i),ywant(j),val,x,y,array,Nx,Ny)
		 		 CALL checkTests(assertSimilar(val,mval,'Error al comparar el valor calculado con el interpolado: interp2P'))
			END DO
		END DO

		CALL mrun('test_interpol, intp2d ')

		xwant=(/100.,235.,320.,347.25/)
		ywant=(/0.3,0.5,0.8/)
		DO i = 1,4
			DO j = 1,3
		 		 mval = ywant(j)*cos(2.*pi*xwant(i)/360.)
		 		 val = intp2d(xwant(i),ywant(j),x,y,array,Nx,Ny)
		 		 CALL checkTests(assertSimilar(val,mval,'Error al comparar el valor calculado con el interpolado: interp2P'))
			END DO
		END DO

		!CALL checkTests(assertSimilar(value,mvalue,'Error al comparar el valor calculado con el interpolado:'))


	END SUBROUTINE test_interpol

END MODULE tests_interpol

PROGRAM test
	USE tests_interpol
	call test_interpol

END PROGRAM test
