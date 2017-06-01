	MODULE tests_ptotheta
		USE ptotheta
		USE tests
		IMPLICIT none

		INTEGER :: I
		REAL :: press(8)
		REAl :: temp(8)
		REAL :: theta(8), theta2(8)
		REAL :: tempp,tempth

	CONTAINS
		SUBROUTINE test_ptotheta
			!press=(/1.,5.,10.,50.,100.,500.,800.,1000./)
			!temp=(/235.,230.,225.,220.,216.,210.,260.,300./)
			CALL mrun('test_ptotheta')
			press=(/1000.,800.,500.,100.,50.,10.,5.,1./)
			temp=(/250.,260.,245.,216.,220.,225.,230.,235./)
			theta=(/250.9,278.2,299.8,418.8,520.1,842.9,1050.6,1700.8/);

			DO I=1,8
				theta2(I)=ctheta(press(I),temp(I))
			END DO
			CALL checktests(assertArraySimilar(theta,theta2,"Error : arrays theta y theta2 no son semejantes"))

			tempth = p2theta(press,temp,418.,temp,8)
			CALL checktests(assertSimilar(tempth,216.2,"Temperaturas distintas"))

			tempp = theta2p(theta,temp,100.,temp)
			CALL checktests(assertSimilar(tempth,216.0,"Temperaturas distintas"))

		END SUBROUTINE test_ptotheta
	END MODULE tests_ptotheta

	PROGRAM test
		USE tests_ptotheta

		CALL test_ptotheta()

	END PROGRAM test
